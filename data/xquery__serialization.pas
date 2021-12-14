unit xquery__serialization;

{$include ../internettoolsconfig.inc}
{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils, xquery, xquery__serialization_nodes, simplehtmltreeparser;

function hashsetQNameIncludeAll(var hs: TXQHashsetQName; const v: IXQValue): boolean;
function hashsetQNameIncludeAll(var hs: TXQHashsetQName;const context: TXQEvaluationContext; node: TTreeNode; const s: string): boolean;

type TXQSerializationParamsHelper = record helper for TXQSerializationParams
  procedure setFromNode(const context: TXQEvaluationContext;paramNode: TTreeNode; isStatic: boolean);
  procedure setFromMap(const context: TXQEvaluationContext; const v: IXQValue);
  procedure initFromXQValue(const context: TXQEvaluationContext;const v: IXQValue);
  procedure setMethod(const s: string);
  procedure setStandalone(s: string; fromMap: boolean);
  procedure setStandalone(s: boolean);
  procedure setNormalizationForm(const s: string);
  procedure setEncoding(const s: string);
  procedure setExtensionProperty(const key, value: string);
  function needQNameList(var list: PXQHashsetQName): PXQHashsetQName;
end;


procedure serializeJSON(var serializer: TXQSerializer; const v: IXQValue; const params: TXQSerializationParams);
procedure serializeAdaptive(var serializer: TXQSerializer; const v: IXQValue; const params: TXQSerializationParams);
procedure serializeXMLHTMLText(var serializer: TXQSerializer; const v: IXQValue; var params: TXQSerializationParams);
type PSerializationParams = ^TXQSerializationParams;
function serializeWithContextDefaults(const context: TXQEvaluationContext; const value: IXQValue): string;
function serialize(const context: TXQEvaluationContext; const value: IXQValue; const serializationParams: IXQValue = nil): string;
procedure serialize(var serializer: TXQSerializer; const value: IXQValue; var serializationParams: TXQSerializationParams);
function serialize(const value: IXQValue; var serializationParams: TXQSerializationParams): RawByteString;

implementation
uses
  strutils, bbutils,
  internetaccess,
  htmlInformation,
  xquery.namespaces,
  xquery.internals.common;


procedure splitEQName(context: TXQEvaluationContext; node: TTreeNode; const eqname: string; out namespaceURL, localpart: string; kind: TXQDefaultNamespaceKind = xqdnkUnknown);
var
  namespacePrefix: String {$if FPC_FULLVERSION < 030300}= ''{$endif};
begin
  case parseEQName(eqname, namespacePrefix, localpart) of
    xqeqnPrefix, xqeqnPrefixAbsent: begin
      if node <> nil then begin
        namespaceURL := node.getNamespaceURL(namespacePrefix);
        if namespaceURL <> '' then exit;
      end;
      namespaceURL := namespaceGetURL(context.findNamespace(namespacePrefix, kind));
    end;
    xqeqnNamespaceUrl: namespaceURL := namespacePrefix;
    xqeqnInvalid: namespaceURL:='<invalid>';
  end;
end;



function hashsetQNameIncludeAll(var hs: TXQHashsetQName; const v: IXQValue): boolean;
var
  qname: TXQValueQName;
  pw: PIXQValue;
begin
  for pw in v.GetEnumeratorPtrUnsafe do begin
    if pw^.kind <> pvkQName then exit(false);
    qname := pw^.toValue as TXQValueQName;
    hs.include(qname.url, qname.local);
  end;
  result := true;
end;

function hashsetQNameIncludeAll(var hs: TXQHashsetQName; const context: TXQEvaluationContext; node: TTreeNode; const s: string): boolean;
var
  t, namespaceUrl, name: String;
begin
  for t in strTrimAndNormalize(s, WHITE_SPACE).Split(' ') do begin
    splitEQName(context, node, t, namespaceUrl, name, xqdnkElementType);
    hs.include(namespaceUrl, name);
  end;
  result := true;
end;







function toSerializationBool(const s:string; fromMap: boolean): boolean;
begin
  case trim(s) of
    'true', 'yes', '1': result := true;
    'false', 'no', '0': result := false;
    else raiseXQEvaluationException(IfThen(fromMap, 'SEPM0016', 'SEPM0017'), 'Expected boolean, got '+s);
  end;
end;


procedure TXQSerializationParamsHelper.setFromNode(const context: TXQEvaluationContext;paramNode: TTreeNode; isStatic: boolean);
  procedure error;
  begin
    raise EXQEvaluationException.create(IfThen(isStatic, 'XQST0109', 'SEPM0017'), 'Invalid serialization parameter: '+paramNode.outerXML());
  end;
  procedure error(code: string);
  begin
    raise EXQEvaluationException.create(code, 'Invalid serialization parameter: '+paramNode.outerXML());
  end;

const XMLNamespace_Output = 'http://www.w3.org/2010/xslt-xquery-serialization';

  procedure checkNoAttributes(node: ttreenode; allowValue: boolean = false);
  var att: TTreeAttribute;
  begin
    for att in node.getEnumeratorAttributes do
      if ((att.value <> 'value') or not allowValue) and not att.isNamespaceNode then
        error;
  end;

  procedure setCharacterMaps();
  var mapNode: TTreeNode;
      att: TTreeAttribute;
      mapString, character: String;
  begin
    if characterMaps = nil then new(characterMaps,init);
    if paramNode.hasAttribute('value') then error;
    mapNode := paramNode.getFirstChild();
    for mapnode in paramNode.getEnumeratorChildren do begin
      case mapnode.typ of
        tetOpen: begin
          if mapnode.namespace = nil then error;
          if not equalNamespaces(namespaceGetURL(mapNode.namespace), XMLNamespace_Output) then continue;
          if mapNode.value <> 'character-map' then error;
          mapString := '';
          character := '';
          for att in mapnode.getEnumeratorAttributes do
            case att.value of
              'map-string': mapString := att.realvalue;
              'character': character := att.realvalue ;
              else if not att.isNamespaceNode then error;
            end;
          if character.lengthInUtf8CodePoints <> 1 then error;
          if characterMaps.contains(character) then error('SEPM0018');
          characterMaps.include(character, mapString);
        end;
        else;
      end;
    end;
  end;

  function toSerializationBool(const s:string): boolean; overload;
  begin
    result := toSerializationBool(s, false);
  end;

var duplicateValueCheck: TXQHashsetStr;
begin
  if paramNode = nil then exit;
  if isStatic and (paramNode.typ = tetDocument) then paramNode := paramnode.getFirstChild();
  if paramNode = nil then exit;
  if not equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output)
     or (paramNode.value <> 'serialization-parameters')
     or (paramNode.typ <> tetOpen) then error('XPTY0004');
   checkNoAttributes(paramNode);
   duplicateValueCheck.init;
   for paramNode in paramNode.getEnumeratorChildren do begin
     if paramnode.typ <> tetOpen then continue;
     if paramnode.namespace = nil then error;
     if duplicateValueCheck.contains(paramNode.getNodeName()) then
       error('SEPM0019');
     duplicateValueCheck.include(paramNode.getNodeName());
     if equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output) then begin
       checkNoAttributes(paramNode, true);
       case paramNode.value of
         'allow-duplicate-names': allowDuplicateNames := toSerializationBool(paramNode.getAttribute('value'));
         'byte-order-mark': byteOrderMark := toSerializationBool(paramNode.getAttribute('value'));
         'cdata-section-elements': hashsetQNameIncludeAll(needQNameList(cdataSectionElements)^, context, paramNode, paramNode.getAttribute('value'));
         'doctype-public': doctypePublic := paramNode.getAttribute('value');
         'doctype-system': doctypeSystem := paramNode.getAttribute('value');
         'encoding':       setEncoding(paramNode.getAttribute('value'));
         'escape-uri-attributes': escapeURIAttributes := toSerializationBool(paramNode.getAttribute('value'));
         'html-version':   htmlVersion := paramNode.getAttribute('value');
         'include-content-type': includeContentType := toSerializationBool(paramNode.getAttribute('value'));
         'indent': if toSerializationBool(paramNode.getAttribute('value')) then indent := xqsiwIndent
                   else indent := xqsiwNever;
         'item-separator': itemSeparator := paramNode.getAttribute('value');
         'json-node-output-method': jsonNodeOutputMethod := paramNode.getAttribute('value');
         'media-type': mediaType := paramNode.getAttribute('value');
         'method':         setMethod(paramNode.getAttribute('value'));
         'normalization-form': setNormalizationForm(paramNode.getAttribute('value'));
         'omit-xml-declaration': omitXmlDeclaration := toSerializationBool(paramNode.getAttribute('value'));
         'standalone': setStandalone(paramNode.getAttribute('value'), false);
         'suppress-indentation': hashsetQNameIncludeAll(needQNameList(suppressIndentation)^, context, paramNode, paramNode.getAttribute('value'));
         'undeclare-prefixes': undeclarePrefixes := toSerializationBool(paramNode.getAttribute('value'));
         'use-character-maps': setCharacterMaps;
         'version':        version := paramNode.getAttribute('value');
         else error();
       end;
     end else if equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespaceURL_MyExtensionsNew) then begin
       setExtensionProperty(paramNode.value, paramNode.getAttribute('value'));
     end;

   end;
   duplicateValueCheck.done;
end;

procedure TXQSerializationParamsHelper.setFromMap(const context: TXQEvaluationContext; const v: IXQValue);
var
  pp: TXQStandardProperty;
  staticOptions: boolean = false;
  tempDoc: IXQValue;
  procedure raiseInvalidParameter(typeError: boolean = true);
  begin
    raiseXQEvaluationError(ifthen(typeError, 'XPTY0004', 'SEPM0016'), 'Invalid parameter for '+ pp.key.toXQuery, pp.Value);
  end;

  function toSerializationBool(const s:string): boolean; overload;
  begin
    result := toSerializationBool(s, true);
  end;

  function valueBool: Boolean;
  begin
    case pp.Value.kind of
      pvkBoolean: exit(pp.value.toBoolean);
      pvkString:
        if staticOptions then exit(toSerializationBool(pp.value.toString))
        else if pp.Value.instanceOf(baseSchema.untypedAtomic) or (pp.Value.kind = pvkNode) then
          case trim(pp.Value.toString) of
            'false': exit(false);
            'true': exit(true);
          end;
    end;

    result := false;
    raiseInvalidParameter();
  end;
  function valueString(): string;
  begin
    if pp.Value.kind = pvkString then result := pp.value.toString
    else begin raiseInvalidParameter; result := ''; end
  end;
  procedure setQNameList(var list: PXQHashsetQName);
  var
    ok: Boolean;
  begin
    needQNameList(list);
    if not staticOptions then ok := hashsetQNameIncludeAll(list^, pp.Value)
    else ok := hashsetQNameIncludeAll(list^,  context, nil, pp.Value.toString);
    if not ok then raiseInvalidParameter();
  end;
  procedure setCharacterMaps;
    procedure error;
    begin
      raiseXPTY0004TypeError(pp.value, 'Map for serialization param use-character-maps.');
    end;
  var characterp: TXQStandardProperty;
  begin
    if characterMaps = nil then new(characterMaps,init);
    if pp.value.kind <> pvkObject then error;
    for characterp in pp.value.getEnumeratorPropertiesUnsafe do begin
      if (not  TXQValueOwnershipTracker.isKeyStringLike(characterp.key.toValue)) or (characterp.Value.kind <> pvkString) then error;
      characterMaps.include(characterp.key.toString, characterp.Value.toString);
    end;
  end;

begin
  for pp in v.getEnumeratorPropertiesUnsafe do begin
    if not TXQValueOwnershipTracker.isKeyStringLike(pp.key.toValue) then begin
      if (pp.key.kind = pvkQName) and ((pp.key as TXQValueQName).url = XMLNamespaceURL_MyExtensionsNew) then
        setExtensionProperty((pp.key as TXQValueQName).local, pp.value.toString);
      continue;
    end;
    case pp.Value.getSequenceCount of
      0: continue;
      1: ; //fine
      else case pp.key.toString of
        'cdata-section-elements', 'suppress-indentation': ; //fine
        else raiseXPTY0004TypeError(v, 'Invalid parameter');
      end;
    end;
    case pp.key.toString of
      'allow-duplicate-names': allowDuplicateNames := valueBool();
      'byte-order-mark': byteOrderMark := valueBool();
      'cdata-section-elements': setQNameList(cdataSectionElements);
      'doctype-public': begin doctypePublic := valueString(); if doctypePublic = '' then doctypePublic := isAbsentMarker; end;
      'doctype-system': begin doctypeSystem := valueString(); if doctypeSystem = '' then doctypeSystem := isAbsentMarker; end;
      'encoding': setEncoding(valueString());
      'escape-uri-attributes': escapeURIAttributes := valueBool();
      'html-version':
        if pp.value.kind in [pvkInt64, pvkBigDecimal] then htmlVersion := inttostr(pp.value.toInt64)
        else if staticOptions and (pp.Value.kind = pvkString) then htmlVersion := pp.Value.toString
        else raiseInvalidParameter;
      'include-content-type': includeContentType := valueBool();
      'indent': if valueBool() then indent := xqsiwIndent
                else indent := xqsiwNever;
      'item-separator': itemSeparator := valueString();
      'json-node-output-method': jsonNodeOutputMethod := valueString();
      'media-type': mediaType := valueString();
      'method': setMethod(valueString());
      'normalization-form': setNormalizationForm(valueString());
      'omit-xml-declaration': omitXmlDeclaration := valueBool();
      'standalone': if staticOptions and (pp.Value.toString = 'omit') then standalone := xdsOmit
                    else setStandalone(valueBool());
      'suppress-indentation': setQNameList(suppressIndentation);
      'undeclare-prefixes': undeclarePrefixes := valueBool();
      'use-character-maps': setCharacterMaps();
      'version': version := valueString();
      #0'static-options': begin
        staticOptions := true;
        allowDuplicateNames := false;
        omitXmlDeclaration := false;
        tempDoc := v.getProperty('parameter-document');
        if assigned(tempDoc) then setFromNode(context, tempDoc.toNode, true);
      end;
      'parameter-document': if not staticOptions then
        raiseXQEvaluationException('XQST0109', 'Unknown serialization option.');
      else if staticOptions then raiseXQEvaluationException('XQST0109', 'Unknown serialization option.')
      else if pp.key.toString.beginsWith('x:') then setExtensionProperty(pp.key.toString.substring(2), pp.value.toString);
    end;
  end;
end;

procedure TXQSerializationParamsHelper.initFromXQValue(const context: TXQEvaluationContext;const v: IXQValue);
begin
  if v = nil then initDefault(false)
  else case v.kind of
    pvkObject: begin
      initDefault(true);
      setFromMap(context, v);
    end;
    pvkNode: begin
      initDefault(false);
      setFromNode(context, v.toNode, false);
    end
    else begin
      initDefault(true);
      if v.getSequenceCount > 0 then raiseXPTY0004TypeError(v, 'serialize params must be map() or node')
    end;
  end;
end;

procedure TXQSerializationParamsHelper.setMethod(const s: string);
begin
  case s of
    'xml': method := xqsmXML;
    'html': method := xqsmHTML;
    'xhtml': method := xqsmXHTML;
    'text': method := xqsmText;
    'json': method := xqsmJSON;
    'adaptive': method := xqsmAdaptive;
  end;
end;

procedure TXQSerializationParamsHelper.setStandalone(s: string; fromMap: boolean);
begin
  s := trim(s);
  if s = 'omit' then standalone := xdsOmit
  else setStandalone(toSerializationBool(s, fromMap));
end;

procedure TXQSerializationParamsHelper.setStandalone(s: boolean);
begin
  if s then standalone := xdsYes
  else standalone := xdsNo;
end;

procedure TXQSerializationParamsHelper.setNormalizationForm(const s: string);
begin
  normalizationForm := unicodeNormalizationForm(s);
  case normalizationForm of
    unfEmpty, unfUnknown: raise EXQEvaluationException.Create('SESU0011', 'Unknown normalization method: '+s);
  end;
end;


procedure TXQSerializationParamsHelper.setEncoding(const s: string);
begin
  encoding := s;
  encodingCP := strEncodingFromName(s);
  if encodingCP = $FFFF then raiseXQEvaluationException('SESU0007', 'Unknown encoding: '+s);
end;

procedure TXQSerializationParamsHelper.setExtensionProperty(const key, value: string);
begin
  case key of
    'key-order': keyOrderExtension := XQKeyOrderFromString(value);
  end;
end;

function TXQSerializationParamsHelper.needQNameList(var list: PXQHashsetQName): PXQHashsetQName;
begin
  if list = nil then new(list,init);
  result := list;
end;


type TSpecialStringHandler = object
  serializer: ^TXQSerializer;
  params: ^TXQSerializationParams;
  isUnicodeEncoding: boolean;
  function normalizeString(p: pchar; len: sizeint): string; overload;
  function normalizeString(const s: string): string; inline; overload;
  procedure appendJSONStringWithoutQuotes(const s: string);
  procedure appendXMLHTMLAttributeText(const s:string; html: boolean);
  procedure appendXMLHTMLCharacterMappedText(const s: string; attrib, html: boolean);
  function appendXMLHTMLText(const n: TTreeNode; html: boolean): boolean;
  procedure appendXMLHTMLAttributeTextFromNode(const n: TTreeAttribute; html: boolean);
  procedure init;
end;
procedure TSpecialStringHandler.init;
begin
  //calling this procedure silences an object never used warning
end;

function TSpecialStringHandler.normalizeString(p: pchar; len: sizeint): string;
begin
  result := xquery__serialization_nodes.normalizeString(strFromPchar(p, len), params^.normalizationForm)
end;

function TSpecialStringHandler.normalizeString(const s: string): string;
begin
  result := normalizeString(pchar(s), length(s));
end;

procedure TSpecialStringHandler.appendJSONStringWithoutQuotes(const s: string);
  procedure appendToSerializer(var serializer: TXQSerializer);
  var needNormalization, needEscaping: Boolean;
  var enumerator: TUTF8StringCodePointBlockEnumerator;
    entity: TXQHashmapStrStr.PHashMapEntity;
    hadNext: Boolean;
  begin
    if params^.characterMaps = nil then begin
      serializer.appendJSONStringWithoutQuotes(normalizeString(s));
    end else begin
      needNormalization := params^.hasNormalizationForm;
      needEscaping := false;
      enumerator.init(s);
      repeat
        hadNext := enumerator.MoveNext;
        entity := params^.characterMaps.findEntity(enumerator.currentPos, enumerator.currentByteLength);
        if (not hadNext) or (entity <> nil) then begin
          if needNormalization or needEscaping then serializer.appendJSONStringWithoutQuotes(normalizeString(enumerator.markedPos, enumerator.markedByteLength))
          else serializer.append(enumerator.markedPos, enumerator.markedByteLength);
          needEscaping := false;
          enumerator.markNext;
          if entity <> nil then
            serializer.append(string(entity^.Value));
        end else case enumerator.currentPos^ of
          #0..#31, '"', '\', '/': needEscaping := true;
        end;
      until not hadNext;
    end;
  end;

  procedure appendReEncoded();
  var temp: string;
      subserializer: TXQSerializer;
      cp: Integer;
  begin
    subserializer.init(@temp);
    appendToSerializer(subserializer);
    subserializer.final;
    for cp in temp.enumerateUtf8CodePoints do begin
      if cp <= $7F then serializer.append(chr(cp))
      else serializer.appendJSONStringUnicodeEscape(cp);
    end;
  end;

begin
  if isUnicodeEncoding then appendToSerializer(serializer^)
  else appendReEncoded();
end;


procedure TSpecialStringHandler.appendXMLHTMLAttributeText(const s: string; html: boolean);
begin
  if html then serializer^.appendHTMLAttrib(s)
  else serializer^.appendXMLAttrib(s);
end;

procedure TSpecialStringHandler.appendXMLHTMLCharacterMappedText(const s: string; attrib, html: boolean);
var needNormalization, needEscaping: Boolean;
var entity: TXQHashmapStrStr.PHashMapEntity;
    enumerator: TUTF8StringCodePointBlockEnumerator;
    hadNext: Boolean;
    temp: String;
begin
  entity := nil;
  needNormalization := params.hasNormalizationForm;
  needEscaping := false;
  enumerator.init(s);
  repeat
    hadNext := enumerator.MoveNext;
    if assigned(params^.characterMaps) then
      entity := params^.characterMaps.findEntity(enumerator.currentPos, enumerator.currentByteLength);
    if (not hadNext) or (entity <> nil) then begin
      if needNormalization or needEscaping then begin
        temp := normalizeString(enumerator.markedPos, enumerator.markedByteLength);
        if attrib then begin
          appendXMLHTMLAttributeText(temp, html);
        end else
          if html then serializer^.appendHTMLText(temp)
          else serializer^.appendXMLText(temp)
      end else begin
        serializer^.append(enumerator.markedPos, enumerator.markedByteLength);
      end;
      needEscaping := false;

      enumerator.markNext;
      if entity <> nil then
        serializer^.append(string(entity^.Value));
    end else case enumerator.currentPos^ of
      '<', '>', '&': needEscaping := true;
      '''', '"': needEscaping := needEscaping or attrib;
      #0..#$1F: needEscaping := true;
      #$C2: needEscaping := needEscaping or (  (enumerator.currentByteLength = 2) and ((enumerator.currentPos + 1)^ in [#$80..#$9F]));
      #$E2: needEscaping := needEscaping or ( (enumerator.currentByteLength = 3) and ((enumerator.currentPos + 1)^ = #$80) and ((enumerator.currentPos + 2)^ = #$A8));
    end;
  until not hadNext;
end;

function TSpecialStringHandler.appendXMLHTMLText(const n: TTreeNode; html: boolean): boolean;
  procedure appendXMLCDATATextASCII(const s: string);
  var enumerator: TUTF8StringCodePointBlockEnumerator;
    hadNext: Boolean;
  begin
    enumerator.init(s);
    repeat
      hadNext := enumerator.MoveNext;
      if enumerator.currentByteLength <> 1 then begin
        serializer.appendXMLCDATAText(enumerator.markedPos, enumerator.markedByteLength);
        if enumerator.currentByteLength > 0 then
          serializer.appendHexEntity(enumerator.current);
        enumerator.markNext;
      end;
    until not hadNext;
  end;

  procedure appendCDATA(const s: string);
  begin
    if isUnicodeEncoding then serializer.appendXMLCDATAText(s)
    else appendXMLCDATATextASCII(s);
  end;
  procedure appendNormalizedCDATA(const s: string);
  begin
    appendCDATA(normalizeString(s));
  end;

var
  parent: TTreeNode;
begin
  if assigned(params.cdataSectionElements) then begin
    parent := n.getParent();
    if assigned(parent) and params.cdataSectionElements.contains(parent.namespace, parent.value) then begin
      if params^.hasNormalizationForm then appendNormalizedCDATA(n.value)
      else appendCDATA(n.value);
      exit(true);
    end;
  end;
  if assigned(params^.characterMaps) then begin
    appendXMLHTMLCharacterMappedText(n.value, false, html);
    exit(true);
  end;
  result := false;
end;

procedure TSpecialStringHandler.appendXMLHTMLAttributeTextFromNode(const n: TTreeAttribute; html: boolean);
  procedure appendURIEscapedAttribute();
  var
    temp: String;
  begin
    temp := TInternetAccess.urlEncodeData(xquery__serialization_nodes.normalizeString(n.realvalue, unfNFC), ueXPathHTML4);
    if params^.hasNormalizationForm or assigned(params^.characterMaps) then
      appendXMLHTMLCharacterMappedText(temp, true, html)
     else
      appendXMLHTMLAttributeText(temp, html);
  end;

begin
  if params^.escapeURIAttributes and html and htmlAttributeIsURI(n) then
    appendURIEscapedAttribute()
   else
    appendXMLHTMLCharacterMappedText(n.realvalue, true, html);
end;

function isUnicodeEncoding(e: TSystemCodePage): boolean;
begin
  e := strActualEncoding(e);
  case e of
    CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE: result := true;
    else result := false;
  end;
end;



procedure serializeJSON(var serializer: TXQSerializer; const v: IXQValue; const params: TXQSerializationParams);
var
  interceptor: TSpecialStringHandler;
begin
  serializer.allowDuplicateNames := params.allowDuplicateNames;
  serializer.keyOrderExtension := params.keyOrderExtension;
  serializer.insertWhitespace := params.indent;
  case params.jsonNodeOutputMethod of
    'xml': serializer.nodeFormat := tnsXML;
    'xhtml': serializer.nodeFormat := tnsXML;
    'html': serializer.nodeFormat := tnsHTML;
    'text': serializer.nodeFormat := tnsText;
    else serializer.error('SEPM0016', v.toValue);
  end;

  if params.hasNormalizationForm or (params.characterMaps <> nil) or not isUnicodeEncoding(params.encodingCP) then begin
    interceptor.init;
    interceptor.params := @params;
    interceptor.serializer := @serializer;
    interceptor.isUnicodeEncoding := isUnicodeEncoding(params.encodingCP);
    serializer.onInterceptAppendJSONString := @interceptor.appendJSONStringWithoutQuotes;
  end;

  v.jsonSerialize(serializer);

  serializer.onInterceptAppendJSONString := nil;
end;

procedure serializeAdaptive(var serializer: TXQSerializer; const v: IXQValue; const params: TXQSerializationParams);
var
  itemSeparator: string;
  w: PIXQValue;
  first: Boolean;
begin
  serializer.nodeFormat:=tnsXML;
  serializer.insertWhitespace := params.indent;
  serializer.insertWhitespace := xqsiwNever;
  itemSeparator := params.itemSeparator;
  if itemSeparator = params.isAbsentMarker then itemSeparator := #10;

  first := true;
  for w in v.GetEnumeratorPtrUnsafe do begin
    if not first then serializer.append(itemSeparator);
    w^.adaptiveSerialize(serializer);
    first := false;
  end;
end;


procedure serializeXMLHTMLText(var serializer: TXQSerializer; const v: IXQValue; var params: TXQSerializationParams);
var firstElement: TTreeNode = nil;

  function findRootNodeCount(const v: IXQValue): SizeInt;
  var
    w, m: PIXQValue;
    n: TTreeNode;
  begin
    result := 0;
    for w in v.GetEnumeratorPtrUnsafe do begin
      case w^.kind of
        pvkNode: n := w^.toNode;
        pvkArray: begin
          for m in w^.GetEnumeratorMembersPtrUnsafe do begin
            result := findRootNodeCount(m^);
            if result > 1 then exit;
          end;
          continue;
        end
        else continue;
      end;
      if n.typ = tetDocument then n := n.getFirstChild();
      if n.typ <> tetAttribute then begin
        while (n <> nil) do begin
          case n.typ of
            tetText: if n.value.Trim() <> '' then inc(result);
            tetOpen: begin
              inc(result);
              if firstElement = nil then firstElement := n;
              if result > 1 then exit();
            end;
            else;
          end;
          n := n.getNextSibling();
        end;
      end;
    end;
  end;

var
  hasItemSeparator: Boolean;
  wasNodeOrFirst: Boolean;

  procedure addItemStart;
  begin
    if hasItemSeparator then begin
      if not wasNodeOrFirst then serializer.append(params.itemSeparator);
      wasNodeOrFirst := false;
    end;
  end;

var interceptor: TSpecialStringHandler;

  procedure appendText(const value: string);
  begin
    if params.hasNormalizationForm or (params.characterMaps <> nil)  then
      interceptor.appendXMLHTMLCharacterMappedText(value,false,false)
     else
      case params.method of
        xqsmXML, xqsmXHTML: serializer.appendXMLText(value);
        xqsmHTML: serializer.appendHTMLText(value);
        xqsmText: serializer.append(value);
        else;
      end;
  end;

  procedure addAtomicString(const s: string);
  begin
    addItemStart;
    if not hasItemSeparator then begin
      if not wasNodeOrFirst then appendText(' ');
      wasNodeOrFirst := false;
    end;
    appendText(s);
  end;




  procedure add(const v: IXQValue);
  var
    w, m: PIXQValue;
    n: TTreeNode;
    method: TXQSerializationMethod;
  begin
    method := params.method;
    for w in v.GetEnumeratorPtrUnsafe do begin
      case w^.kind of
        pvkNode: begin
          addItemStart;
          //this might be incomplete
          n := w^.toNode;
          if n.typ in [tetAttribute] then raiseXQEvaluationException('SENR0001', 'Cannot serialize attribute');
          case method of
            xqsmXML, xqsmXHTML: serializeNodes(n, serializer, true, false, @params);
            xqsmHTML: serializeNodes(n, serializer, true, true, @params);
            xqsmText: serializer.append(w^.toString);
            else;
          end;
          if not hasItemSeparator then wasNodeOrFirst := true;
        end;
        pvkArray: for m in v.GetEnumeratorMembersPtrUnsafe do
          add(m^);
        pvkNull: addAtomicString('');
        pvkObject, pvkFunction: raiseXQEvaluationError('SENR0001', 'Cannot serialize with XML/HTML/Text method', w^);
        else addAtomicString(w^.toString);
      end;
    end;
  end;

var
  hasDoctypeSystem: Boolean;

begin
  with params do
    case method of
      xqsmHTML, xqsmXHTML: begin
        if (htmlVersion = isAbsentMarker) then htmlVersion := version;
        if (htmlVersion = isAbsentMarker) then htmlVersion := '5.0';
        if (method = xqsmHTML) and assigned(cdataSectionElements) then begin
          cdataSectionElements.exclude('');
          if isHTML5 then cdataSectionElements.exclude(XMLNamespaceUrl_XHTML);
        end;
        if assigned(suppressIndentation) then suppressIndentation.addHTMLLowercaseQNames(isHTML5);
      end;
      xqsmXML, xqsmText: params.escapeURIAttributes := false;
      else;
    end;

  if params.hasNormalizationForm or (params.characterMaps <> nil) or assigned(params.cdataSectionElements) or params.escapeURIAttributes then begin
    interceptor.init;
    interceptor.params := @params;
    interceptor.serializer := @serializer;
    interceptor.isUnicodeEncoding := isUnicodeEncoding(params.encodingCP);
    if params.hasNormalizationForm or (params.characterMaps <> nil) or params.escapeURIAttributes then
      serializer.onInterceptAppendXMLHTMLAttribute := @interceptor.appendXMLHTMLAttributeTextFromNode;
    if params.hasNormalizationForm or assigned(params.characterMaps) or assigned(params.cdataSectionElements) then
      serializer.onInterceptAppendXMLHTMLText := @interceptor.appendXMLHTMLText;
  end;

  with params do begin
    hasItemSeparator := params.itemSeparator <> params.isAbsentMarker;
    hasDoctypeSystem := params.doctypeSystem <> params.isAbsentMarker;
    standalone := params.standalone;
    if findRootNodeCount(v) > 1 then begin
      if hasDoctypeSystem then hasDoctypeSystem := false;
      if standalone <> xdsOmit then standalone := xdsOmit;
    end;


    case params.method of
      xqsmXML, xqsmXHTML, xqsmHTML: begin
        //initialize missing default parameters
        if (method <> xqsmHTML) then begin
          if omitXmlDeclaration then
            if (standalone <> xdsOmit) or ( (version <> '1.0') and (version <> isAbsentMarker) and hasDoctypeSystem ) then
              raiseXQEvaluationException('SEPM0009', 'Invalid serialization parameter');

          if version = isAbsentMarker then version := '1.1';

          if undeclarePrefixes and (version = '1.0') then raiseXQEvaluationException('SEPM0010', 'Invalid serialization parameter');
        end;

        serializer.insertWhitespace := indent;

        //headers
        if (method <> xqsmHTML) then begin
          if not omitXmlDeclaration then begin
            serializer.appendXMLHeader(version, encoding, standalone);
            if indent = xqsiwIndent then
              serializer.append(LineEnding);
          end;
        end;
        if isHTML5 and (not hasDoctypeSystem)
           and (firstElement <> nil) and striEqual(firstElement.value, 'html')  {todo and only whitespace before firstelement}
           and ( (method = xqsmXHTML) or ( (method = xqsmHTML) and (doctypePublic = isAbsentMarker) )) then begin
           if method = xqsmHTML then serializer.append('<!DOCTYPE html>')
           else serializer.append('<!DOCTYPE '+firstElement.value+'>')
        end else if hasDoctypeSystem then begin
          if method = xqsmHTML then serializer.append('<!DOCTYPE html ')
          else if firstElement <> nil then
            serializer.append('<!DOCTYPE '+firstElement.value + ' ');
          if doctypePublic <> isAbsentMarker then serializer.append('PUBLIC "' + doctypePublic + '" ')
          else serializer.append('SYSTEM ');
          serializer.append('"'+doctypeSystem+'">');
        end else if (method = xqsmHTML) and (doctypePublic <> isAbsentMarker) then
          serializer.append('<!DOCTYPE html PUBLIC "'+doctypePublic+'">');
      end;
      xqsmText: begin
        //encoding: string;
      end;
      xqsmJSON, xqsmAdaptive: ;
    end;
  end;

  wasNodeOrFirst := true;
  add(v);

  serializer.onInterceptAppendXMLHTMLAttribute := nil;
  serializer.onInterceptAppendXMLHTMLText := nil;
end;

function serializeWithContextDefaults(const context: TXQEvaluationContext; const value: IXQValue): string;
begin
  result := serialize(context, value, context.staticContext.serializationOptions);
end;

function serialize(const context: TXQEvaluationContext; const value: IXQValue; const serializationParams: IXQValue): string;
var
  params: TXQSerializationParams;
begin
  params.initFromXQValue(context, serializationParams);
  result := serialize(value, params);
  params.done
end;

procedure serialize(var serializer: TXQSerializer; const value: IXQValue; var serializationParams: TXQSerializationParams);
  procedure serializeWithEncodingChange;
  var temp: string;
      tempserializer: TXQSerializer;
      temp2: RawByteString;
  begin
    serializationParams.allowEncodingConversion := false;
    tempserializer.init(@temp);
    serialize(tempserializer, value, serializationParams);
    tempserializer.final;
    serializationParams.allowEncodingConversion := true;
    if serializationParams.byteOrderMark then serializer.appendBOM(serializationParams.encodingCP);
    temp2 := strConvert(temp, CP_UTF8, serializationParams.encodingCP);
    serializer.append(pointer(temp2), length(temp2)); //append pchar, so it is not converted back to utf-8
  end;

begin
  if serializationParams.allowEncodingConversion and (serializationParams.encodingCP <> CP_UTF8) then begin
    serializeWithEncodingChange;
    exit;
  end;
  serializer.standard := serializationParams.standardMode;
  case serializationParams.method of
    xqsmJSON: serializeJSON(serializer, value, serializationParams);
    xqsmAdaptive: serializeAdaptive(serializer, value, serializationParams);
    else serializeXMLHTMLText(serializer, value, serializationParams);
  end;
end;

function serialize(const value: IXQValue; var serializationParams: TXQSerializationParams): RawByteString;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  serialize(serializer, value, serializationParams);
  serializer.final;
end;

initialization
xquery.globalSerializationCallback := @serializeWithContextDefaults;
end.

