unit htmlformutils;

{$include ../internettoolsconfig.inc}

interface

uses
  Classes, SysUtils, simplehtmltreeparser, xquery.internals.common, xquery, internetaccess;

type
THtmlFormEnctype = (hfetUrlEncoded, hfetMultipart, hfetTextPlain);
THttpRequestHTMLFormKind = (hrhfDefault, hrhfNoValue, hrhfSubmitButton, hrhfCharsetSpecial);
THttpRequestParam = record
  key, value: string;
  kind: THttpRequestHTMLFormKind;
  mimeHeaders: TStringArray;
end;
PHttpRequestParam = ^THttpRequestParam;
EHttpRequestParamsException = Exception;
THttpRequestParams = object //This could completely replace TMIMEMultipartData
protected
//  procedure addSubmitButtonName(const key: string);
//  procedure removeImplicitSubmitButtonNames();
  procedure removeParamHard(keyIdx: integer);
public
  urlencoded: boolean;
  charset: TSystemCodePage;

  size: sizeint;
  data: array of THttpRequestParam;
  firstKeyIndex: TXQHashmapStrSizeInt;
  keysToRemove: TXQHashsetStr;

  hasSubmitParams: boolean;
  implicitSubmitElement: TTreeNode;
  function addRawKey(const k: string): PHttpRequestParam;
  function addRawParam(const p: THttpRequestParam): PHttpRequestParam;
  function addRawKeyValue(const k, v: string): PHttpRequestParam;

  function  addKeyValue(const n, v: string): PHttpRequestParam; //not encoded
  procedure addUrlEncodedList(s: string); //encoded
  procedure addXQValue(const value: IXQValue; const staticContext: TXQStaticContext);
  procedure addMime(const mime: TMIMEMultipartData);
  procedure addTextPlainRequest(const textPlain: string);

  //returns true if a value has been overridden
  function mergeOverride(const requestOverride: THttpRequestParams): boolean;
  //returns true if a value has been overridden
  function addFormAndMerge(form: TTreeNode; cmp: TStringComparisonFunc; const requestOverride: THttpRequestParams): boolean;


  function toEncodedRequest(enctype: THtmlFormEnctype; out header: string): string;
  function toUrlEncodedRequest: string;
  function toMimeRequest(out boundary: string): string;
  function toMimeRequest(): TMIMEMultipartData;
  function toTextPlainRequest(): string;

  function getSubmitIndices(): IXQValue;

  procedure compress;
  procedure clearData; //does not clear meta properties like charset
  constructor init;
  destructor done;

end;

function getAssociatedForm(t: TTreeNode): TTreeNode;


function xqFunctionForm(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
function xqFunctionRequest_combine(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
function xqFunctionRequest_decode(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;

implementation
uses htmlInformation, bbutils;

function getAssociatedForm(t: TTreeNode): TTreeNode;
begin
  result := t;
  while (result <> nil) and not striEqual(result.value, 'form') do
    result := result.parent;
end;

function getOptionsSelectName(node: TTreeNode; cmp: TStringComparisonFunc): string;
//this is a separate function so the pattern matcher could use it with non-empty t:form-request on an option element.
//but there a non-empty t:form-request is not actually supported. The t:form-request should be put on the select element
var ancestor: TTreeNode;
begin
  result := '';
  for ancestor in node.getEnumeratorAncestors do
    if cmp(ancestor.value, 'select') then begin
      result := ancestor.getAttribute('name', cmp);
      exit;
    end;
end;

type
TFormElementData = record
  hasData: boolean;
  kind: THttpRequestHTMLFormKind;
  names, values: array of string;
end;


function nodeToFormData(node: TTreeNode; cmp: TStringComparisonFunc; includeAllInputs: boolean): TFormElementData;
//todo: handle formaction, formmethod, formenctype on submit buttons
  procedure pushEntry(const n, v: string);
  begin
    SetLength(result.names, length(result.names) + 1);
    SetLength(result.values, length(result.values) + 1);
    result.names[high(result.names)] := n;
    result.values[high(result.values)] := v;
  end;
  procedure checkForDirname();
  begin
    if node.hasAttribute('dirname', cmp) then
      pushEntry(node.getAttribute('dirname', cmp), 'LTR');
  end;

//submittable elements:   button input object select textarea
type TSubmittableElement = (seButton, seInput, seObject, seSelect, seOption, seTextarea);
type TInputElementType = (ieNotAnInputElement, ietOther, ietHidden, ietTextOrSearch, ietCheckboxOrRadiobutton, ietImageButton, ietButton, ietFile );
const IMAGE_BUTTON_DEFAULT_COORD = '0';
var name: string;
  procedure pushEntryNameValue(n: TTreeNode = nil);
  begin
    if n = nil then n := node;
    pushEntry(name, n.getAttribute('value', cmp));
  end;

var
  typ: String;
  kind: TSubmittableElement;
  inputKind: TInputElementType = ieNotAnInputElement;
  ancestor, legend, descendant, firstOption: TTreeNode;
begin
  result := default(TFormElementData);
  result.hasData := false;
  if node.typ <> tetOpen then exit;

  case node.hash of
    HTMLNodeNameHashs.input: if cmp(node.value, 'input') then begin
      kind := seInput;
      typ := node.getAttribute('type', cmp);

      if cmp(typ, 'hidden') then inputKind := ietHidden //common types first for faster comparison
      else if (typ =  '') or cmp(typ, 'text') or cmp(typ, 'search') then inputKind := ietTextOrSearch
      else if cmp(typ, 'checkbox') or cmp(typ, 'radio') then inputKind := ietCheckboxOrRadiobutton
      else if cmp(typ, 'image') then begin
        inputKind := ietImageButton;
        result.kind := hrhfSubmitButton;
      end else if cmp(typ, 'submit') then begin
        inputKind := ietButton;
        result.kind := hrhfSubmitButton;
      end else if cmp(typ, 'reset') or cmp(typ, 'button')  then inputKind := ietButton
      else if cmp(typ, 'file') then inputKind := ietFile
      else case lowercase(typ) of {todo: handle cmp}
        'tel', 'url', 'email', 'password',
        'date', 'month', 'week',
        'time', 'datetime-local',
        'number', 'range', 'color': inputKind := ietOther;
        else inputKind := ietTextOrSearch; //invalid value => text default
      end;
    end else exit;
    HTMLNodeNameHashs.select: if  cmp(node.value, 'select') then kind := seSelect else exit;
    HTMLNodeNameHashs.textarea: if cmp(node.value, 'textarea') then kind := seTextarea else exit;
    HTMLNodeNameHashs.button: if cmp(node.value, 'button') then begin
      kind := seButton;
      case node.getAttribute('type', cmp) of
        'reset', 'button': ; //no submit
        {'submit'} else result.kind := hrhfSubmitButton;
      end;
    end else exit;
    HTMLNodeNameHashs.&object:  if cmp(node.value, 'object') then kind := seObject else exit;
    HTMLNodeNameHashs.option: if  cmp(node.value, 'option') then kind := seOption else exit;
    else exit;
  end;

  if not includeAllInputs then begin
    if kind = seObject then exit; //we have no plugins?
    if kind = seOption then exit; //handled through select element
    if ((kind = seButton) or (inputKind in [ietImageButton,ietButton])) and (result.kind <> hrhfSubmitButton) then exit;
    if (kind <> seObject) and node.hasAttribute('disabled', cmp) then exit;
    if (inputKind = ietCheckboxOrRadiobutton) and not node.hasAttribute('checked', cmp) then exit;

    legend := nil;
    for ancestor in node.getEnumeratorAncestors do begin
      if cmp(ancestor.value, 'fieldset') and ancestor.hasAttribute('disabled', cmp) then begin
        if ancestor.findChild(tetOpen, 'legend') <> legend then //descendants of the first legend *child* are always enabled
          exit;
      end else if cmp(ancestor.value, 'legend') then legend := ancestor
      //else if cmp(ancestor.value, 'datalist') then exit; html5 says, ignore fields with a datalist ancestor, but FF/Chromium do not do that
    end;
  end;


  name := node.getAttribute('name', cmp);

  if (name = '') and (inputKind <> ietImageButton) and (kind <> seOption) then exit;

  case kind of
    seSelect: begin
      firstOption := nil;
      for descendant in node.getEnumeratorDescendants do begin
        if cmp(descendant.value, 'option') and (not assigned(firstOption) or descendant.hasAttribute('selected', cmp)) then begin
          if not assigned(firstOption) then firstOption := descendant;
          if descendant.hasAttribute('selected', cmp) then
            pushEntryNameValue(descendant);
        end;
      end;
      if (length(result.names) = 0) and (assigned(firstOption)) then
        pushEntryNameValue(firstOption);
    end;
    seOption: begin
      name := getOptionsSelectName(node, cmp);
      if name = '' then exit;
      pushEntry(name, node.getAttribute('value', cmp));
    end;
    seInput: begin
      case inputKind of
        ietCheckboxOrRadiobutton: pushEntry(name, node.getAttribute('value', 'on', cmp));
        ietFile: exit; //todo: file upload
        ietImageButton: begin
          if name <> '' then name += '.';
          pushEntry(name + 'x', IMAGE_BUTTON_DEFAULT_COORD);
          pushEntry(name + 'y', IMAGE_BUTTON_DEFAULT_COORD);
        end;
        else begin
          pushEntryNameValue();
          case inputKind of
            ietHidden: if striEqual(name, '_charset_') and not node.hasAttribute('value', cmp) then result.kind := hrhfCharsetSpecial;
            ietTextOrSearch: checkForDirname();
            ieNotAnInputElement, ietOther, ietCheckboxOrRadiobutton, ietImageButton, ietButton, ietFile: ;
          end;
        end;
      end;
    end;
    //seObject: ; aborted above
    seTextarea: begin
      pushEntry(name, node.deepNodeText());
      checkForDirname();
    end else pushEntryNameValue();
  end;

  //todo: line normalization
  result.hasData := length(result.names) > 0;
end;


function getFormEncoding(n: TTreeNode): TSystemCodePage;
var
  encodingLabels, l: string;
begin
  result := CP_NONE;
  if n.getAttributeTry('accept-charset', encodingLabels) then
    for l in strSplitOnAsciiWS(encodingLabels) do begin
      result := strEncodingFromName(l);
      if result <> CP_NONE then break;
    end;

  if result = CP_NONE then begin
    n := n.getRootHighest(); //html5 says this encoding should not be used when @accept-charset exists, even if it is empty, but FF/Chromium use it anyways :/
    if n is TTreeDocument then
      result := TTreeDocument(n).baseEncoding;
  end;

  case result of
    CP_NONE, CP_ACP, CP_OEMCP, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE, CP_ASCII:
      result := CP_UTF8;
  end;
end;


function formEncode(s: string; encoding: TSystemCodePage): string;
begin
  if (encoding <> CP_UTF8) and (encoding <> CP_NONE) and (encoding <> StringCodePage(s)) {todo: does that make sense?} then
    s := strConvertFromUtf8(s, encoding);
  result := TInternetAccess.urlEncodeData(s, ueHTMLForm);
end;
function formMultipartFieldEncode(s: string): string;
begin
  result := TInternetAccess.urlEncodeData(s, ueHTMLMultipartFieldName);
end;


{procedure THttpRequestParams.addSubmitButtonName(const key: string);
begin
  SetLength(submitKeys, length(submitKeys) + 1);
  submitKeys[high(submitKeys)] := key;
end;}

procedure THttpRequestParams.removeParamHard(keyIdx: integer);
var
  i: SizeInt;
  key: String;
  pair: TXQHashmapStrSizeInt.TKeyValuePairOption;
begin
  if (keyIdx < 0) or (keyIdx > size) then exit;
  key := data[keyIdx].key;
  for i := keyIdx + 1 to size - 1 do
    data[i - 1] := data[i];
  dec(size);
  if firstKeyIndex[key] = keyIdx then firstKeyIndex.exclude(key);
  for pair in firstKeyIndex do
    if pair.value >= keyIdx then
      firstKeyIndex.include(pair.key, pair.value - 1)
end;

function THttpRequestParams.addRawKey(const k: string): PHttpRequestParam;
begin
  if size = length(data) then
    if length(data) > 0 then SetLength(data, 2*length(data))
    else SetLength(data, 16);
  result := @data[size];
  result.key := k;
  result.kind := hrhfDefault;
  keysToRemove.exclude(k);
  firstKeyIndex.include(k, size, false);
  inc(size);
end;

function THttpRequestParams.addRawParam(const p: THttpRequestParam): PHttpRequestParam;
begin
  result := addRawKey(p.key);
  result.value := p.value;
  result.kind := p.kind;
  result.mimeHeaders := p.mimeHeaders;
end;

function THttpRequestParams.addRawKeyValue(const k, v: string): PHttpRequestParam;
begin
  result := addRawKey(k);
  result.value := v;
end;

procedure THttpRequestParams.compress;
begin
  if length(data) <> size then
    SetLength(data, size);
end;

procedure THttpRequestParams.clearData;
begin
  size := 0;
  data := nil;
  firstKeyIndex.clear;
  keysToRemove.Clear;
end;

constructor THttpRequestParams.init;
begin
  keysToRemove.init;
  firstKeyIndex.init;
  charset := CP_UTF8;
  urlencoded := false;
  size := 0;
  data := nil;
  hasSubmitParams := false;
  implicitSubmitElement:= nil;
end;

destructor THttpRequestParams.done;
begin
  keysToRemove.done;
  firstKeyIndex.done;
end;

function getFormEnctypeActual(isPost: boolean; enctype: string): THtmlFormEnctype;
begin
 result := hfetUrlEncoded;
 if not isPost then exit();
 if striEqual(enctype, ContentTypeMultipart) then result := hfetMultipart
 else if striEqual(enctype, ContentTypeTextPlain) then result := hfetTextPlain
end;

procedure formToRequest(request: TXQBoxedStringMap;
  form, submitElement: TTreeNode; cmp: TStringComparisonFunc; contextBaseURI: string;
  out action: string; out encTypeEnum: THtmlFormEnctype; out methodIsPost: boolean);
  procedure nodeToRequest(n: TTreeNode; prefix: string; var actionURI, enctype, method: string);
  begin
    method := uppercase(n.getAttribute(prefix + 'method', cmp));
    if not ((method = 'POST') or (method = 'DIALOG')) then
      method := 'GET';
    actionURI := strTrim(n.getAttribute(prefix + 'action', cmp), [#$9, #$A, #$C, #$D, ' ']);
    {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
    if (form.getDocument() <> nil) then actionURI := strResolveURI(actionURI, form.getDocument().baseURI);
    actionURI := strResolveURI(actionURI, contextBaseURI);
    {$ENDIF}
    enctype := n.getAttribute(prefix + 'enctype', cmp);
    if not (striEqual(enctype, ContentTypeMultipart) or striEqual(enctype, ContentTypeTextPlain)) then
      enctype := ContentTypeUrlEncoded;
  end;

var
  enctype, method: string;
  originAction, originEnctype, originMethod: string;
  temp: TXQBoxedStringMap;
begin
  action := ''; enctype := ''; method := '';
  nodeToRequest(form, '', action, enctype, method);
  if assigned(submitElement) and (submitElement.hasAttribute('formmethod', cmp) or submitElement.hasAttribute('formaction', cmp) or submitElement.hasAttribute('formenctype', cmp)) then begin
    originAction := action;
    originEnctype := enctype;
    originMethod := method;
    nodeToRequest(submitElement, 'form', action, enctype, method);
    if (originAction <> action) or (originEnctype <> enctype) or (originMethod <> method) then begin
      temp := TXQBoxedStringMap.create();
      temp.setMutable('method', originMethod);
      temp.setMutable('enctype', originEnctype);
      temp.setMutable('action', originAction);
      request.setMutable('form-request', temp.boxInIXQValue);
    end;
  end;
  methodIsPost := striEqual(method, 'POST');
  encTypeEnum := getFormEnctypeActual(methodIsPost, enctype);
  request.setMutable('method', method);
end;

function THttpRequestParams.addKeyValue(const n, v: string): PHttpRequestParam; //not encoded
begin
  if urlEncoded then result := addRawKeyValue(formEncode(n, charset), formEncode(v, charset))
  else result := addRawKeyValue(n, v);
end;

procedure THttpRequestParams.addUrlEncodedList(s: string);
  procedure addUrlEncodedPair(temp: string);
  var
    key: String;
  begin
    key := strSplitGet('=', temp);
    if urlEncoded then addRawKeyValue(key, temp)
    else addRawKeyValue(urlHexDecode(key), urlHexDecode(temp));
  end;

var
  split: TStringArray;
  i: SizeInt;
begin
  if s = '' then exit;
  split := strSplit(s, '&');
  for i:=0 to high(split) do addUrlEncodedPair(split[i]);
end;


procedure THttpRequestParams.addXQValue(const value: IXQValue; const staticContext: TXQStaticContext);
  procedure addSpecialPair(n: string; v: TXQBoxedMapLike);
  var
    param: PHttpRequestParam;
    value, filename, contenttype, headers, h: String;
    temp: IXQValue;
    i: SizeInt;
    kind: THttpRequestHTMLFormKind = hrhfNoValue;
  begin
    if urlEncoded then n := formEncode(n, charset);

    if v.hasProperty('kind', temp) then begin
      case temp.toString of
        'submit': kind := hrhfSubmitButton;
        'charset': kind := hrhfCharsetSpecial;
      end;
      if kind = hrhfSubmitButton then begin
        hasSubmitParams := true;
        if (n = '') and not v.hasProperty('value') then exit;
      end;
    end;

    if v.hasProperty('x', temp) and v.hasProperty('y') then begin
      addRawKey(n + '.x').value:=temp.toString;
      addRawKey(n + '.y').value:=v.getProperty('y').toString;
      exit;
    end;


    param := addRawKey(n);
    param.kind := kind;

    headers := '';
    value := '';
    filename := '';
    contenttype := '';
    if v.hasProperty('file', temp) then begin
      filename := temp.toString;
      value := staticContext.retrieveFromFile(filename, contenttype, 'FOUT1170');
      if kind = hrhfNoValue then
        param.kind := hrhfDefault;
    end;
    if v.hasProperty('filename', temp) then filename := temp.toString;
    if v.hasProperty('type', temp) then contenttype := temp.toString;
    if v.hasProperty('value', temp) then begin
      value := temp.toString;
      if kind = hrhfNoValue then
        param.kind := hrhfDefault;
    end;

    if v.hasProperty('headers', temp) then begin
      for i := 1 to temp.getSequenceCount do begin
        h := temp.get(i).toString;
        if i > 1 then h := TMIMEMultipartData.HeaderSeparator + h;
        headers += h;
      end;
    end;

    if urlEncoded then value := formEncode(value, charset)
    else begin
      n := formMultipartFieldEncode(n);
      filename := formMultipartFieldEncode(filename);
      contenttype := formMultipartFieldEncode(contenttype);
    end;


    param.value := value;
    param.mimeHeaders := TMIMEMultipartData.buildHeaders(n, filename, contenttype, headers);
  end;

  procedure addSingletonXQValue(name: string; const v: IXQValue);
  begin
    if v.kind <> pvkObject then
      addKeyValue(name, v.toString)
    else
      addSpecialPair(name, v.getDataObject);
  end;

  procedure markUnusedKeyForDeletion(name: string);
  begin
    if urlEncoded then name := formEncode(name, charset);
    if firstKeyIndex.contains(name) then
      exit; //not unused
    keysToRemove.include(name);
  end;

  procedure addObject(const v: IXQValue);
  var
    p: TXQProperty;
    w: PIXQValue;
  begin
    for p in v.getEnumeratorStringPropertiesUnsafe do begin
      case p.value.kind of
        pvkUndefined: markUnusedKeyForDeletion(p.key);
        pvkSequence: case p.value.getSequenceCount of
          0: markUnusedKeyForDeletion(p.key);
          1: addSingletonXQValue(p.key, p.value);
          else for w in p.value.GetEnumeratorPtrUnsafe do
            addSingletonXQValue(p.key, w^);
        end;
        else addSingletonXQValue(p.key, p.value);
      end;
    end;
  end;

  procedure addFormData(v: TTreeNode; const formData: TFormElementData);
  var
    i: Integer;
  begin
    for i := 0 to high(formData.names) do
      addKeyValue(formData.names[i], formData.values[i]).kind := formData.kind;
    if formData.kind = hrhfSubmitButton then begin
      hasSubmitParams := true;
      if implicitSubmitElement = nil then implicitSubmitElement := v;
    end;
  end;

var
  v: PIXQValue;
  nodeCompare: TStringComparisonFunc;
  formData: TFormElementData;
begin
  nodeCompare := @staticContext.NodeCollation.equal;
  for v in value.GetEnumeratorPtrUnsafe do
    case v^.kind of
      pvkObject: addObject(v^);
      pvkNode: begin
        formData := nodeToFormData(v^.toNode, nodeCompare, true);
        if formData.hasData then addFormData(v^.toNode, formData)
        else if formData.kind = hrhfSubmitButton then hasSubmitParams := true
        else addUrlEncodedList(v^.toString);
      end
     else addUrlEncodedList(v^.toString)
   end;
end;

procedure THttpRequestParams.addMime(const mime: TMIMEMultipartData);
var
  i: SizeInt;
  param: PHttpRequestParam;
begin
  //if urlencoded then raise EHttpRequestParamsException.Create('Cannot add mime data to urlencoded request');
  for i := 0 to high(mime.data) do begin
    param := addKeyValue(mime.data[i].getFormDataName, mime.data[i].data);
    param.mimeHeaders := mime.data[i].headers;
  end;
end;

procedure THttpRequestParams.addTextPlainRequest(const textPlain: string);
var
  temp: sysutils.TStringArray;
  i: Integer;
  key: String;
begin
  temp := strSplit(textPlain, #13#10, false);
  for i := 0 to high(temp) do begin
    key := strSplitGet('=', temp[i]);
    addKeyValue(key, temp[i]);
  end;
end;

function THttpRequestParams.mergeOverride(const requestOverride: THttpRequestParams): boolean;
var
  requestOverrideUsed: array of boolean = nil;
  requestOverrideNextKeyOccurrence: array of SizeInt = nil; //this is used to build a multimap
  requestOverrideKeyIndex: TXQhashmapStrSizeInt;

  procedure initRequestOverrideInfo;
  var lastKeyIndex: TXQhashmapStrSizeInt;
    i, last: SizeInt;
    k: string;
  begin
    SetLength(requestOverrideUsed, requestOverride.size);
    SetLength(requestOverrideNextKeyOccurrence, requestOverride.size);
    requestOverrideKeyIndex.init;
    lastKeyIndex.init;
    for i := 0 to requestOverride.size - 1 do begin
      requestOverrideNextKeyOccurrence[i] := -1;
      last := lastKeyIndex.get(requestOverride.data[i].key, -1);
      if last >= 0 then requestOverrideNextKeyOccurrence[last] := i;
      lastKeyIndex[requestOverride.data[i].key] := i;
    end;
    lastKeyIndex.done;

    requestOverrideKeyIndex.init;
    requestOverrideKeyIndex.assign(requestOverride.firstKeyIndex);
    for k in requestOverride.keysToRemove do
      requestOverrideKeyIndex.include(k, -1, false);
  end;

  procedure mergeHeaders(var p: THttpRequestParam; const over: THttpRequestParam);
  var
    i, j: sizeint;
    name: String;
  begin
    if p.mimeHeaders = nil then begin
      p.mimeHeaders := over.mimeHeaders;
      exit;
    end;
    for i := 0 to high(p.mimeHeaders) do begin
      name := TMIMEMultipartData.nameFromHeader(p.mimeHeaders[i]);
      j := TMIMEMultipartData.indexOfHeader(over.mimeHeaders, name);
      if j >= 0 then p.mimeHeaders[i] := over.mimeHeaders[j];
    end;
    for j := 0 to high(over.mimeHeaders) do begin
      name := TMIMEMultipartData.nameFromHeader(over.mimeHeaders[j]);
      i := TMIMEMultipartData.indexOfHeader(p.mimeHeaders, name);
      if i < 0 then begin
        SetLength(p.mimeHeaders, length(p.mimeHeaders) + 1);
        p.mimeHeaders[high(p.mimeHeaders)] := over.mimeHeaders[j];
      end;
    end;
  end;

var oldData: array of THttpRequestParam;
  oldSize, i, replaced: SizeInt;
begin
  result := false;
  if (requestOverride.urlencoded <> urlencoded) or
     (urlencoded and (requestOverride.charset <> charset)) then
    raise EHttpRequestParamsException.Create('Incompatible http params');
  initRequestOverrideInfo;

  oldData := data;
  oldSize := size;
  clearData;
  SetLength(data, oldSize + requestOverride.size);
  for i := 0 to oldSize - 1 do begin
    replaced := requestOverrideKeyIndex.get(oldData[i].key, -2);
    //replaced = -2: not overridden; replaced = -1: to remove; >= 0: override
    if replaced <> -1 then begin
      addRawParam(oldData[i]);
      if replaced > -1 then begin
        result := true;
        requestOverrideKeyIndex.include(oldData[i].key, requestOverrideNextKeyOccurrence[replaced]);
        requestOverrideUsed[replaced] := true;
        case requestOverride.data[replaced].kind of
          hrhfDefault: data[size-1].value := requestOverride.data[replaced].value;
          hrhfNoValue: ;
          hrhfSubmitButton: begin
            data[size-1].kind := hrhfSubmitButton;
            data[size-1].value := requestOverride.data[replaced].value;
          end;
          hrhfCharsetSpecial: begin
            data[size-1].kind := hrhfCharsetSpecial;
            data[size-1].value := strEncodingName(charset)
          end;
        end;
        if requestOverride.data[replaced].mimeHeaders <> nil then mergeHeaders(data[size-1], requestOverride.data[replaced]);
      end;
    end;
  end;
  for i := 0 to high(requestOverrideUsed) do
    if not requestOverrideUsed[i] then
      addRawParam(requestOverride.data[i]);
  requestOverrideKeyIndex.done;
  compress;
end;

function THttpRequestParams.addFormAndMerge(form: TTreeNode; cmp: TStringComparisonFunc; const requestOverride: THttpRequestParams
  ): boolean;
var temp, tempIterator, iterationEnd: TTreeNode;
  formData: TFormElementData;
  i: Integer;
  inForm: Boolean = false;
  formId: String;
  implicitSubmitIdx: integer = -1;
  implicitSubmitCount: integer = 0;
begin
  if requestOverride.hasSubmitParams then
    hasSubmitParams := true;
  formId := form.getAttribute('id', cmp);
  if formId = '' then   tempIterator := form
  else tempIterator := form.getRootHighest();
  iterationEnd := tempIterator.reverse;
  while (tempIterator <> nil) and (tempIterator <> iterationEnd) do begin
    temp := tempIterator;
    tempIterator := tempIterator.next;
    case inForm of
      true: if temp = form.reverse then inForm := false;
      false: begin
        if temp = form then begin
          inform := true;
          continue;
        end;
        if temp.getAttribute('form', cmp) <> formId then continue;
      end;
    end;
    case temp.hash of
      HTMLNodeNameHashs.input,
      HTMLNodeNameHashs.select,
      HTMLNodeNameHashs.textarea,
      HTMLNodeNameHashs.button,
      HTMLNodeNameHashs.&object
      : {ok};
      else continue;
    end;
    formData := nodeToFormData(temp, cmp, false);
    case formData.kind of
      hrhfSubmitButton:
        if ((high(formdata.names) < 0) or not requestOverride.firstKeyIndex.contains(formData.names[0]))
           and ((high(formdata.names) < 1) or not requestOverride.firstKeyIndex.contains(formData.names[1])) //for image buttons
          then begin
          //name is not used in requestOverride
          if not hasSubmitParams then begin //use it as implicit submit element if there was none before
            hasSubmitParams := true;
            implicitSubmitElement := temp;
            implicitSubmitIdx := size;
            implicitSubmitCount := length(formdata.names);
          end else
            continue //skip otherwise
        end else begin
          hasSubmitParams := true;
          if implicitSubmitIdx >= 0 then begin
            //remove previous implicit submit element because requestOverride named this as explicit one (even if it is not marked as submit element in requestOverride)
            for i := 1 to implicitSubmitCount do
              removeParamHard(implicitSubmitIdx);
            implicitSubmitIdx := -1;
            implicitSubmitElement := nil;
          end;
        end;
      hrhfCharsetSpecial: if length(formData.values) > 0 then formData.values[0] := strEncodingName(charset);
      hrhfDefault, hrhfNoValue: ;
    end;
    if not formData.hasData then
      continue;
    for i := 0 to high(formData.names) do
      addKeyValue(formData.names[i], formData.values[i]).kind := formData.kind;
  end;
  result := mergeOverride(requestOverride);
end;

function THttpRequestParams.toEncodedRequest(enctype: THtmlFormEnctype; out header: string): string;
begin
  case enctype of
    hfetUrlEncoded: begin
      header := '';
      result := toUrlEncodedRequest;
    end;
    hfetMultipart: begin
      result := toMimeRequest(header);
      header := TMIMEMultipartData.HeaderForBoundary(header);
    end;
    hfetTextPlain: begin
      header := 'Content-Type: ' + ContentTypeTextPlain;
      result := toTextPlainRequest();
    end;
    {$if FPC_FULLVERSION <= 30203}else begin result := ''; header := ''; end; {$endif}
  end;
end;


function THttpRequestParams.toUrlEncodedRequest: string;
var sb: TStrBuilder;
  i: sizeint;
begin
  sb.init(@result);
  for i := 0 to size - 1 do begin
    if i <> 0 then sb.append('&');
    if urlencoded then begin
      sb.append(data[i].key);
      sb.append('=');
      sb.append(data[i].value);
    end else begin
      sb.append(formEncode(data[i].key,  charset));
      sb.append('=');
      sb.append(formEncode(data[i].value, charset));
    end;
  end;
  sb.final;
end;




function THttpRequestParams.toMimeRequest(out boundary: string): string;
var mime: TMIMEMultipartData;
begin
  if urlencoded then raise EHttpRequestParamsException.Create('MIME Request needs unencoded params');
  mime := toMimeRequest();
  result := mime.compose(boundary);
end;

function THttpRequestParams.toMimeRequest(): TMIMEMultipartData;
var
  i: SizeInt;
begin
  result := default(TMIMEMultipartData);
  SetLength(result.data, size);
  for i := 0 to size - 1 do begin
    result.data[i].data := data[i].value;
    result.data[i].headers := TMIMEMultipartData.insertMissingNameToHeaders(formMultipartFieldEncode(data[i].key), data[i].mimeHeaders);
  end;
end;

function THttpRequestParams.toTextPlainRequest(): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to size - 1 do begin
    result += data[i].key + '=' + data[i].value + #13#10;
  end;
end;

function THttpRequestParams.getSubmitIndices(): IXQValue;
var indices: array of integer = nil;
  i: Integer;
  resseq: TXQValueList;
begin
  for i := 0 to size - 1 do
    if data[i].kind = hrhfSubmitButton then begin
      setlength(indices, length(indices) + 1);
      indices[high(indices)] := i + 1;
    end;
  if length(indices) = 0 then result := xqvalue
  else begin
    resseq := TXQValueList.create(length(indices));
    for i := 0 to high(indices) do resseq.add(xqvalue(indices[i]));
    result := xqvalueSeqSqueezed(resseq);
  end;
end;

//see https://html.spec.whatwg.org/multipage/forms.html and https://html.spec.whatwg.org/multipage/form-control-infrastructure.html#form-submission-2
{todo:
form owner attribute can add submittable elements to the form that are not descendants of the form (e.g. form=xyz adds it to <form id=xyz)
need to handle submitter element (e.g. button)
check for <form method=dialog>
}
function xqFunctionForm(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var onlyFormsWithOverriddenValue: boolean = false;
    procedure failedToFindForm();
    begin
      raise EXQEvaluationException.create('XPDY0002', 'Could not find a form element');
    end;

    function findForms(n: TTreeNode): IXQValue;
    var
      nodeseq: TXQValueList;
      nlast: TTreeNode;
    begin
      nodeseq := TXQValueList.create();
      if n.typ in [tetOpen, tetDocument] then nlast := n.reverse
      else nlast := n.next;
      while (n <> nlast) and (n <> nil) do begin
        if (n.typ = tetOpen) and (n.hash = HTMLNodeNameHashs.form) and striEqual(n.value, 'form') then nodeseq.add(xqvalue(n));
        n := n.next;
      end;
      if nodeseq.Count = 0 then failedToFindForm();
      result := nodeseq.toXQValueSequenceSqueezed;
    end;

var requestOverride: THttpRequestParams;
    cmp: TStringComparisonFunc;
    lastFormHadOverriddenValue: boolean = false;

    function encodeForm(const form: TTreeNode): IXQValue;
    var
      contenttypeheader: string;
      post: Boolean;
      actionURI: String;
      resultobj: TXQBoxedStringMap;
      request: THttpRequestParams;
      encodedRequest: string;
      enctype: THtmlFormEnctype;
      temp: IXQValue;
    begin
      if form = nil then exit(xqvalue());
      request.init;

      request.charset := getFormEncoding(form);

      lastFormHadOverriddenValue := request.addFormAndMerge(form, cmp, requestOverride);

      resultobj := TXQBoxedStringMap.create();
      result := resultobj.boxInIXQValue;

      formToRequest(resultobj, form, request.implicitSubmitElement, cmp, context.staticContext.baseURI, actionURI, enctype, post);

      encodedRequest := request.toEncodedRequest(enctype, contenttypeheader);
      if contenttypeheader <> '' then
        resultobj.setMutable('headers', contenttypeheader);

      if post then begin
        resultobj.setMutable('post', encodedRequest)
      end else if encodedRequest <> '' then
        if strContains(actionURI, '?') then actionURI += '&' + encodedRequest
        else actionURI += '?' + encodedRequest;


      resultobj.setMutable('url', actionURI);
      if request.charset <> CP_UTF8 then resultobj.setMutable('charset', 'CP' + IntToStr(request.charset));

      if request.hasSubmitParams then begin
        temp := request.getSubmitIndices();
        if not temp.isUndefined then
          resultobj.setMutable('submit-indices', temp);
      end;

      request.done;
    end;

var v: PIXQValue;
  resseq: TXQValueList;
  form, overrideOptions, nresult: IXQValue;
begin
  requiredArgCount(argc, 0, 2);
  case argc of
    0: begin
      form := findForms(context.contextNode(true));
      overrideOptions := xqvalue;
    end;
    1: begin
      //overrideOptions := xqvalue; //hide uninitialized warning
      form := args[0];
      if (form.kind = pvkSequence) and (form.count = 1) then form := args[0].get(1);
      case form.kind of
        pvkNode, pvkSequence, pvkUndefined: overrideOptions := xqvalue;
        pvkObject, pvkString: begin
          overrideOptions := form;
          form := findForms(context.contextNode(true));
          onlyFormsWithOverriddenValue := true;
        end;
        else raiseXPTY0004TypeError(form, 'form argument');
      end;
    end;
    2: begin
      form := args[0];
      overrideOptions := args[1];
    end;
    else exit(xqvalue);
  end;

  if form.getSequenceCount = 0 then
    exit(xqvalue);

  cmp := @context.staticContext.nodeCollation.equal;
  requestOverride.init;
  requestOverride.addXQValue(overrideOptions, context.staticContext);

  resseq := TXQValueList.create();
  for v in form.GetEnumeratorPtrUnsafe do begin
    nresult := encodeForm(v^.toNode);
    if lastFormHadOverriddenValue or not onlyFormsWithOverriddenValue then begin
      resseq.add(nresult);
    end;
  end;
  result := resseq.toXQValueSequenceSqueezed;
  requestOverride.done;
  if onlyFormsWithOverriddenValue and (result.Count = 0) then failedToFindForm();
end;

function getRequestContentTypeAndMIMEBoundary(const v: IXQValue; out mimeBoundary: string): THtmlFormEnctype;
var
  headers: IXQValue;
  h: IXQValue;
  tempstr: String;
begin
  result := hfetUrlEncoded;
  mimeBoundary := '';
  if v.kind <> pvkObject then exit;
  headers := v.getProperty('headers');
  for h in headers.GetEnumeratorArrayTransparentUnsafe do begin
    tempstr := h.toString;
    if not striBeginsWith(tempstr, 'Content-Type') then continue;
    if striContains(tempstr, ContentTypeMultipart) then begin
      mimeBoundary := trim(strCopyFrom(tempstr, pos('=', tempstr) + 1));
      if strBeginsWith(tempstr, '"') then mimeBoundary := copy(mimeBoundary, 2, length(mimeBoundary) - 2);
      exit(hfetMultipart);
    end else if striContains(tempstr, ContentTypeTextPlain) then exit(hfetTextPlain);
  end;
end;




function xqFunctionRequest_combine(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  obj: TXQBoxedMapLike;
  enctype: THtmlFormEnctype;
  requests: array[0..1] of THttpRequestParams;

  urlencodedIsPOST: boolean = false;
  urlencodedGETurlPrefix: string;

  mimeBoundary: string;

  procedure addBaseRequestUrlEncoded;
  var oldUrl: String;
      oldUrlView: TPCharView;
      queryStart: PChar;
      hadLinkTarget: Boolean;
      query: string;
  begin
    urlencodedIsPOST := striEqual(obj.getProperty('method').toString, 'POST');
    if urlencodedIsPOST then begin
      urlencodedGETurlPrefix := '';
      query := obj.getProperty('post').toString;
    end else begin
      oldUrl := obj.getProperty('url').toString;
      oldUrlView.init(oldUrl);
      hadLinkTarget := oldUrlView.leftOfFind('#');
      queryStart := oldUrlView.find('?');
      if queryStart = nil then begin
        if hadLinkTarget then urlencodedGETurlPrefix := oldUrlView.toString
        else urlencodedGETurlPrefix := oldUrl;
        oldUrlView.rightOfFirst(length('https://'));
        if strIsAbsoluteURI(oldUrl) and not oldUrlView.contains('/') then urlencodedGETurlPrefix += '/?'
        else urlencodedGETurlPrefix += '?';
        query := '';
      end else begin
        urlencodedGETurlPrefix := oldUrlView.viewLeftWith(queryStart).toString;
        query := oldUrlView.viewRightOf(queryStart).toString;
      end;
    end;

    requests[0].urlencoded := true;
    requests[1].urlencoded := true;

    requests[0].addUrlEncodedList(query);
  end;
  procedure addBaseRequestMultipart;
  var mime: TMIMEMultipartData;
  begin
    mime.parse(obj.getProperty('post').toString, mimeBoundary);
    requests[0].addMime(mime);
  end;
  procedure addBaseRequestTextPlain;
  begin
    requests[0].addTextPlainRequest(obj.getProperty('post').toString);
  end;

  procedure updateContentType(const header: string);
  var
    headers: IXQValue;
    tempSeq: TXQValueList;
    h: IXQValue;
    tempstr: String;
  begin
    headers := obj.getProperty('headers');
    tempSeq := TXQValueList.create(headers.getSequenceCount);
    if header <> '' then
      tempSeq.add(xqvalue(header));
    for h in headers.GetEnumeratorArrayTransparentUnsafe do begin
      tempstr := h.toString;
      if not striBeginsWith(tempstr, 'Content-Type') then
        tempSeq.add(h);
    end;
    result := result.setImmutable('headers', tempSeq.toXQValueSequenceSqueezed).boxInIXQValue;
  end;

  procedure serializeRequestUrlEncoded;
  var
    newQuery: String;
  begin
    newQuery := requests[0].toUrlEncodedRequest;
    if urlencodedIsPOST then result := result.setImmutable('post', newQuery).boxInIXQValue
    else result := result.setImmutable('url', urlencodedGETurlPrefix + newQuery).boxInIXQValue
  end;
  procedure serializeRequestMultipart;
  var
    mime: TMIMEMultipartData;
    tempstr: String;
  begin
    mime := requests[0].toMimeRequest();
    result := result.setImmutable('post', mime.compose(tempstr, mimeBoundary)).boxInIXQValue;

    if tempstr <> mimeBoundary then
      updateContentType(TMIMEMultipartData.HeaderForBoundary(tempstr));
  end;
  procedure serializeRequestTextPlain;
  begin
    result := result.setImmutable('post', requests[0].toTextPlainRequest()).boxInIXQValue;
  end;

var submitIndices: array of integer = nil;
  procedure markSubmitIndices;
  var temp: IXQValue;
      i: Integer;
  begin
    if not obj.hasProperty('submit-indices', temp) then exit;
    if temp.isUndefined then exit;
    setlength(submitIndices, temp.getSequenceCount);
    for i := 0 to high(submitIndices) do
      submitIndices[i] := temp.get(i + 1).toInt64;
    for i := 0 to high(submitIndices) do
      if (submitIndices[i] >= 1) and (submitIndices[i] <= requests[0].size) then begin
        requests[0].data[submitIndices[i] - 1].kind := hrhfSubmitButton;
        requests[0].hasSubmitParams := true;
      end;
  end;

  procedure removeOldSubmitParams;
  var
    i: Integer;
  begin
    for i := high(submitIndices) downto 0 do //assume sequence is sorted in reverse
      requests[0].removeParamHard(submitIndices[i] - 1);
  end;
  procedure serializeSubmitIndices;
  begin
    result := result.setImmutable('submit-indices', requests[0].getSubmitIndices()).boxInIXQValue;
  end;
  procedure updateFormRequestForSubmit;
  var
    originalFormRequest: IXQValue;
    action, method: String;
    methodIsPost: Boolean;
    oldEncType: THtmlFormEnctype;
  begin
    originalFormRequest := obj.getProperty('form-request');
    action := originalFormRequest.getProperty('action').toString;
    method := originalFormRequest.getProperty('method').toString;
    methodIsPost := method = 'POST';
    oldEncType := enctype;
    encType := getFormEnctypeActual(methodIsPost, originalFormRequest.getProperty('enctype').toString);
    urlencodedIsPOST := methodIsPost;
    if methodIsPost <> (obj.getProperty('method').toString = 'POST') then begin
      if (not methodIsPost) then begin
        if (enctype = hfetUrlEncoded) then
          if action.contains('?') then urlencodedGETurlPrefix:=action + '&'
          else urlencodedGETurlPrefix:=action + '?';
        result := result.setImmutable('post', '').boxInIXQValue;
      end;
    end;
    if (oldEncType <> enctype) then
      case enctype of
        hfetUrlEncoded: updateContentType('');
        hfetMultipart: mimeBoundary := ''; //update later
        hfetTextPlain: updateContentType('Content-Type: ' + ContentTypeTextPlain);
      end;
    result := result.setImmutable('url', action).boxInIXQValue;
    result := result.setImmutable('method', method).boxInIXQValue;
  end;

var
  i: SizeInt;
  encoding: TSystemCodePage;
begin
  requiredArgCount(argc, 2);
  enctype := getRequestContentTypeAndMIMEBoundary(args[0], mimeBoundary);
  if args[0].kind  = pvkObject then begin
    obj := args[0].getDataObject;
    result := obj.boxInIXQValue;
  end else begin
    obj := TXQBoxedStringMap.create();
    TXQBoxedStringMap(obj).setMutable('url', args[0].toString);
    result := obj.boxInIXQValue;
  end;


  encoding := strEncodingFromName(obj.getProperty('charset').toString);
  if encoding = CP_NONE then encoding := CP_UTF8;

  for i := 0 to 1 do begin
    requests[i] := default(THttpRequestParams);
    requests[i].init;
    requests[i].charset:=encoding;
  end;

  case enctype of
    hfetUrlEncoded: addBaseRequestUrlEncoded;
    hfetMultipart: addBaseRequestMultipart;
    hfetTextPlain: addBaseRequestTextPlain;
  end;
  markSubmitIndices();

  requests[1].addXQValue(args[1], context.staticContext);
  if requests[1].hasSubmitParams then begin
    removeOldSubmitParams;
    if obj.hasProperty('form-request') then
      updateFormRequestForSubmit();
  end;


  requests[0].mergeOverride(requests[1]);

  case enctype of
    hfetUrlEncoded: serializeRequestUrlEncoded;
    hfetMultipart: serializeRequestMultipart;
    hfetTextPlain: serializeRequestTextPlain;
  end;

  if requests[0].hasSubmitParams or requests[1].hasSubmitParams then
    serializeSubmitIndices;

  requests[0].done;
  requests[1].done;
end;



function xqFunctionRequest_decode(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var paramobj: TXQBoxedStringMap;
  procedure addParam(const name, value: string);
  var tempseq: TXQValueList;
      v: IXQValue;
  begin
    if paramobj.hasProperty(name, v) then begin
      tempseq := TXQValueList.create(v.getSequenceCount + 1);
      tempseq.add(v);
      tempseq.add(xqvalue(value));
      paramobj.setMutable(name, tempseq.toXQValueSequenceSqueezed);
    end else paramobj.setMutable(name, value);
  end;

  procedure parseParams(enctype: THtmlFormEnctype; const q: ixqvalue);
  var
    request: THttpRequestParams;
    i: SizeInt;
  begin
    if paramobj = nil then paramobj := TXQBoxedStringMap.create();
    request.init;
    if enctype <> hfetTextPlain then request.addXQValue(q, context.staticContext)
    else request.addTextPlainRequest(q.toString);
    for i := 0 to request.size - 1 do
      addParam(request.data[i].key, request.data[i].value);
    request.done;
  end;
  procedure parseParamsMime(const data, boundary: string);
  var mime: TMIMEMultipartData;
    i: SizeInt;
  begin
    if paramobj = nil then paramobj := TXQBoxedStringMap.create();
    mime.parse(data, boundary);
    for i := 0 to high(mime.data) do
      addParam(mime.data[i].getFormDataName, mime.data[i].data);
  end;

var
  url, boundary: String;
  decoded: TDecodedUrl;
  resobj: TXQBoxedStringMapPendingUpdate;
  enctype: THtmlFormEnctype;
begin
  requiredArgCount(argc, 1);
  resobj := TXQBoxedStringMapPendingUpdate.create();
  result := resobj.boxInIXQValue;
  paramobj := nil;
  if args[0].kind = pvkObject then begin
    url := args[0].getProperty('url').toString;
    if args[0].getDataObject is TXQBoxedStringMap then begin
      resobj.prototype := args[0].getDataObject as TXQBoxedStringMap;
      resobj.prototype._AddRef;
    end else resobj.setMutable('url', url);
    if striEqual(args[0].getProperty('method').toString, 'POST') then begin
      enctype := getRequestContentTypeAndMIMEBoundary(args[0], boundary);
      if enctype = hfetMultipart then parseParamsMime(args[0].getProperty('post').toString, boundary)
      else parseParams(enctype, args[0].getProperty('post'));
    end;
  end else begin
    url := args[0].toString;
    resobj.setMutable('url', url);
  end;
  if url <> '' then begin
    decoded := decodeURL(url);
    resobj.setMutable('protocol', decoded.protocol);
    if decoded.username <> '' then
      resobj.setMutable('username', decoded.username);
    if decoded.password <> '' then
      resobj.setMutable('password', decoded.password);
    if decoded.host <> '' then
      resobj.setMutable('host', decoded.host);
    if decoded.port <> '' then
      resobj.setMutable('port', decoded.port);
    if decoded.params <> '' then begin
      if strBeginsWith(decoded.path, '/') then delete(decoded.path, 1, 1);
      resobj.setMutable('path', decoded.path);
    end;
    if decoded.params <> '' then begin
      if strBeginsWith(decoded.params, '?') then delete(decoded.params, 1, 1);
      resobj.setMutable('query', decoded.params);
      parseParams(hfetUrlEncoded, xqvalue(decoded.params));
    end;
    if decoded.linktarget <> '' then begin
      if strBeginsWith(decoded.linktarget, '#') then delete(decoded.linktarget, 1, 1);
      resobj.setMutable('target', decoded.linktarget);
    end;
  end;
  if paramobj <> nil then resobj.setMutable('params', paramobj.boxInIXQValue);
end;

end.

