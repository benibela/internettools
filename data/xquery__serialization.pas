unit xquery__serialization;

{$include ../internettoolsconfig.inc}
{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils, xquery, xquery.internals.common, xquery.namespaces, simplehtmltreeparser;

type TUnicodeNormalizationForm = (unfNFC, unfNFD, unfNFKC, unfNFKD,
                                  //do nothing (three variants for error handling)
                                  unfNone, unfEmpty, unfUnknown);

function unicodeNormalizationForm(const s: string): TUnicodeNormalizationForm;
function normalizeString(str: string; method: TUnicodeNormalizationForm): UTF8String;

type
TTrackOwnedXQHashsetStr = record
  class procedure addRef(o: PXQHashsetStr); static; inline;
  class procedure release(o: PXQHashsetStr); static; inline;
  class procedure addRef(o: PXQHashsetStrCaseInsensitiveASCII); static; inline;
  class procedure release(o: PXQHashsetStrCaseInsensitiveASCII); static; inline;
end;
TXQHashsetQName = object(specialize TXQHashmapStrOwning<PXQHashsetStr, TTrackOwnedXQHashsetStr>)
  function getOrCreate(const namespace: string): PXQHashsetStr;
  function contains(namespace: TNamespace; const local: string): boolean;
  procedure include(namespace: TNamespace; const local: string);
  function contains(const namespace,local: string): boolean;
  procedure include(const namespace,local: string);
  function includeAll(const v: IXQValue): boolean;
  function includeAll(const context: TXQEvaluationContext; node: TTreeNode; const s: string): boolean;
  procedure addHTMLLowercaseQNames(html5: boolean);
end;
PXQHashsetQName = ^TXQHashsetQName;
TXQTwoLevelHashsetCaseInsensitiveASCII = object(specialize TXQHashmapStrCaseInsensitiveASCIIOwning<PXQHashsetStrCaseInsensitiveASCII, TTrackOwnedXQHashsetStr>)
  function getOrCreate(const a: string): PXQHashsetStr;
  function contains(const a, b: string): boolean;
  procedure include(const a, b: string);
  procedure exclude(const a, b: string);
end;


type
TXQSerializationMethod = (xqsmXML, xqsmXHTML, xqsmHTML, xqsmText, xqsmJSON, xqsmAdaptive);
TXQSerializationParams = record
  isAbsentMarker: string;
  method: TXQSerializationMethod;
  encoding: string;
  encodingCP: TSystemCodePage;
  indent: TXQSerializerInsertWhitespace;
  itemSeparator: string;
  normalizationForm: TUnicodeNormalizationForm;
  characterMaps: ^TXQHashmapStrStr;

  //xml/html only
  version: string;
  doctypePublic, doctypeSystem: string;
  omitXmlDeclaration, undeclarePrefixes: boolean;
  standalone: TXMLDeclarationStandalone;
  cdataSectionElements, suppressIndentation: PXQHashsetQName;

  htmlVersion, mediaType: string;
  includeContentType,escapeURIAttributes: boolean;

  //json only
  jsonNodeOutputMethod: string;
  allowDuplicateNames: boolean;

  procedure done;

  procedure setDefault(isFromMap: boolean);
  procedure setFromNode(const context: TXQEvaluationContext; paramNode: TTreeNode; isStatic: boolean);
  procedure setFromMap(const context: TXQEvaluationContext;const v: IXQValue);
  procedure setFromXQValue(const context: TXQEvaluationContext; const v: IXQValue);

  procedure setMethod(const s: string);
  procedure setStandalone(s: string; fromMap: boolean);
  procedure setStandalone(s: boolean);
  procedure setNormalizationForm(const s: string);
  function hasNormalizationForm: boolean;
  procedure setEncoding(const s: string);
  function needQNameList(var list: PXQHashsetQName): PXQHashsetQName;

  function isHTML5: boolean;
  function getContentType: string;
end;

const XMLNamespaceUrl_XHTML = 'http://www.w3.org/1999/xhtml';
      XMLNamespaceURL_MathML = 'http://www.w3.org/1998/Math/MathML';
      XMLNamespaceUrl_SVG = 'http://www.w3.org/2000/svg';

function serializeJSON(const params: TXQSerializationParams; const v: IXQValue): string;
function serializeAdaptive(const params: TXQSerializationParams; const v: IXQValue): string;
function serializeXMLHTMLText(var params: TXQSerializationParams; const v: IXQValue): string;
type PSerializationParams = ^TXQSerializationParams;
procedure serializeNodes(base: TTreeNode; var builder: TXQSerializer; nodeSelf: boolean; html: boolean; params: PSerializationParams);
function serializeWithContextDefaults(const context: TXQEvaluationContext; const value: IXQValue): string;
function serialize(const context: TXQEvaluationContext; const value: IXQValue; const serializationParams: IXQValue = nil): string;

implementation
uses
  strutils, bbutils,
  internetaccess, //for uri escaping
  htmlInformation,
  {$IFDEF USE_BBFLRE_UNICODE}PUCU,bbnormalizeunicode{$ENDIF} //get FLRE from https://github.com/BeRo1985/flre or https://github.com/benibela/flre/
  {$IFDEF USE_BBFULL_UNICODE}bbunicodeinfo{$ENDIF}
  {$IFDEF USE_THEO_UNICODE}unicodeinfo{$ENDIF} //from http://wiki.lazarus.freepascal.org/Theodp
;

function unicodeNormalizationForm(const s: string): TUnicodeNormalizationForm;
begin
  case s of
    'NFC':  result := unfNFC;
    'NFD':  result := unfNFD;
    'NFKC': result := unfNFKC;
    'NFKD': result := unfNFKD;
    //'FULLY-NORMALIZED': ??
    'none': result := unfNone;
    '': result := unfEmpty;
    else result := unfUnknown;
  end;
end;

function normalizeString(str: string; method: TUnicodeNormalizationForm): UTF8String;
var
  p: pchar;
begin
  p := pchar(str);
  case method of
    unfNFC:  p := utf8proc_NFC(p);
    unfNFD:  p := utf8proc_NFD(p);
    unfNFKC: p := utf8proc_NFKC(p);
    unfNFKD: p := utf8proc_NFKD(p);
    //'FULLY-NORMALIZED': ??
    else exit(str);
  end;

  result := UTF8String(p);
  Freemem(p);
end;


procedure splitEQName(context: TXQEvaluationContext; node: TTreeNode; const eqname: string; out namespaceURL, localpart: string; kind: TXQDefaultNamespaceKind = xqdnkUnknown);
var
  colon: SizeInt;
  namespacePrefix: String;
begin
  localpart := xmlStrWhitespaceCollapse(eqname);
  if strBeginsWith(localpart, 'Q{') then begin //EQName!
    namespaceURL := strSplitGet('}', localpart);
    delete(namespaceURL, 1, 2); //Q{ no more
  end else begin
    colon := pos(':', localpart);
    if colon = 0 then namespacePrefix := ''
    else begin
      namespacePrefix := copy(localpart, 1, colon - 1);
      delete(localpart, 1, colon);
    end;
    if node <> nil then begin
      namespaceURL := node.getNamespaceURL(namespacePrefix);
      if namespaceURL <> '' then exit;
    end;
    namespaceURL := namespaceGetURL(context.findNamespace(namespacePrefix, kind));
  end;
end;



class procedure TTrackOwnedXQHashsetStr.addRef(o: PXQHashsetStr);
begin
  //empty
  ignore(o);
end;
class procedure TTrackOwnedXQHashsetStr.release(o: PXQHashsetStr);
begin
  dispose(o, done);
end;

class procedure TTrackOwnedXQHashsetStr.addRef(o: PXQHashsetStrCaseInsensitiveASCII);
begin
  ignore(o);
end;

class procedure TTrackOwnedXQHashsetStr.release(o: PXQHashsetStrCaseInsensitiveASCII);
begin
  dispose(o, done);
end;

function TXQHashsetQName.getOrCreate(const namespace: string): PXQHashsetStr;
var
  ent: PHashMapEntity;
begin
  ent := findEntity(namespace, true);
  if ent.Value = nil then new(PXQHashsetStr(ent.Value), init);
  result := PXQHashsetStr(ent.Value);
end;

function TXQHashsetQName.contains(namespace: TNamespace; const local: string): boolean;
begin
  result := contains(namespaceGetURL(namespace), local);
end;
procedure TXQHashsetQName.include(namespace: TNamespace; const local: string);
begin
  include(namespaceGetURL(namespace), local);
end;
function TXQHashsetQName.contains(const namespace, local: string): boolean;
var
  s: PXQHashsetStr;
begin
  s := getOrDefault(namespace);
  result := assigned(s) and s.contains(local);
end;
procedure TXQHashsetQName.include(const namespace, local: string);
begin
  getOrCreate(namespace).include(local);
end;

function TXQHashsetQName.includeAll(const v: IXQValue): boolean;
var
  qname: TXQValueQName;
  pw: PIXQValue;
begin
  for pw in v.GetEnumeratorPtrUnsafe do begin
    if pw^.kind <> pvkQName then exit(false);
    qname := pw^.toValue as TXQValueQName;
    include(qname.url, qname.local);
  end;
  result := true;
end;

function TXQHashsetQName.includeAll(const context: TXQEvaluationContext; node: TTreeNode; const s: string): boolean;
var
  t, namespaceUrl, name: String;
begin
  for t in strTrimAndNormalize(s, WHITE_SPACE).Split(' ') do begin
    splitEQName(context, node, t, namespaceUrl, name, xqdnkElementType);
    include(namespaceUrl, name);
  end;
  result := true;
end;

procedure TXQHashsetQName.addHTMLLowercaseQNames(html5: boolean);
  procedure transformFromTo(from: PXQHashsetStr; tonamespace: string);
  var str: string;
  begin
    if from = nil then exit;
    for str in from^ do begin
      Include(tonamespace, lowercase(str));
    end;
  end;

var v: TXQHashsetQName.PKeyValuePair;
begin
  if html5 then begin
    transformFromTo(getOrDefault(''), XMLNamespaceUrl_XHTML);
    transformFromTo(getOrDefault(XMLNamespaceUrl_XHTML), '');
  end;
  for v in self do begin
    transformFromTo(v.value, v.key);
  end;
end;




function TXQTwoLevelHashsetCaseInsensitiveASCII.getOrCreate(const a: string): PXQHashsetStr;
var
  ent: PHashMapEntity;
begin
  ent := findEntity(a, true);
  if ent.Value = nil then new(PXQHashsetStr(ent.Value), init);
  result := PXQHashsetStr(ent.Value);
end;

function TXQTwoLevelHashsetCaseInsensitiveASCII.contains(const a, b: string): boolean;
var
  nestedSet: PXQHashsetStr;
begin
  nestedSet := getOrDefault(a);
  result := assigned(nestedSet) and nestedSet.contains(b);
end;

procedure TXQTwoLevelHashsetCaseInsensitiveASCII.include(const a, b: string);
begin
  getOrCreate(a).include(b);
end;

procedure TXQTwoLevelHashsetCaseInsensitiveASCII.exclude(const a, b: string);
var
  nestedSet: PXQHashsetStr;
begin
  nestedSet := getOrCreate(a);
  if assigned(nestedSet) then nestedSet.exclude(b);
end;





procedure TXQSerializationParams.done;
begin
  if assigned(characterMaps) then Dispose(characterMaps,done);
  if assigned(cdataSectionElements) then Dispose(cdataSectionElements,done);
  if assigned(suppressIndentation) then Dispose(suppressIndentation,done);
end;

procedure TXQSerializationParams.setDefault(isFromMap: boolean);
begin
  isAbsentMarker := #0;
  method := xqsmXML;
  if isFromMap then version := '1.0' else version := isAbsentMarker;
  encoding := 'UTF-8';
  encodingCP := CP_UTF8;
  htmlVersion := isAbsentMarker;
  doctypePublic := isAbsentMarker;
  doctypeSystem := isAbsentMarker;
  omitXmlDeclaration := true;
  standalone := xdsOmit;
  itemSeparator := isAbsentMarker;
  if isFromMap then indent := xqsiwNever
  else indent := xqsiwConservative;
  jsonNodeOutputMethod := 'xml';
  normalizationForm := unfUnknown;
  characterMaps := nil;
  allowDuplicateNames := false;
  cdataSectionElements := nil;
  suppressIndentation := nil;
  includeContentType := isFromMap;
  escapeURIAttributes := isFromMap;
  mediaType := 'text/html';
  undeclarePrefixes := false;
end;

function toSerializationBool(const s:string; fromMap: boolean): boolean;
begin
  case trim(s) of
    'true', 'yes', '1': result := true;
    'false', 'no', '0': result := false;
    else raiseXQEvaluationException(IfThen(fromMap, 'SEPM0016', 'SEPM0017'), 'Expected boolean, got '+s);
  end;
end;


procedure TXQSerializationParams.setFromNode(const context: TXQEvaluationContext;paramNode: TTreeNode; isStatic: boolean);
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
         'byte-order-mark': ; //todo
         'cdata-section-elements': needQNameList(cdataSectionElements).includeAll(context, paramNode, paramNode.getAttribute('value'));
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
         'suppress-indentation': needQNameList(suppressIndentation).includeAll(context, paramNode, paramNode.getAttribute('value'));
         'undeclare-prefixes': undeclarePrefixes := toSerializationBool(paramNode.getAttribute('value'));
         'use-character-maps': setCharacterMaps;
         'version':        version := paramNode.getAttribute('value');
         else error();
       end;
     end;
   end;
   duplicateValueCheck.done;
end;

procedure TXQSerializationParams.setFromMap(const context: TXQEvaluationContext; const v: IXQValue);
var
  pp, characterp: TXQProperty;
  staticOptions: boolean = false;
  tempDoc: IXQValue;
  procedure raiseInvalidParameter(typeError: boolean = true);
  begin
    raiseXQEvaluationError(ifthen(typeError, 'XPTY0004', 'SEPM0016'), 'Invalid parameter for '+ pp.Name, pp.Value);
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
        else if pp.Value.instanceOf(baseSchema.untypedAtomic) then
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
    if not staticOptions then ok := list.includeAll(pp.Value)
    else ok := list.includeAll(context, nil, pp.Value.toString);
    if not ok then raiseInvalidParameter();
  end;
  procedure setCharacterMaps;
    procedure error;
    begin
      raiseXPTY0004TypeError(pp.value, 'Map for serialization param use-character-maps.');
    end;

  begin
    if characterMaps = nil then new(characterMaps,init);
    if pp.value.kind <> pvkObject then error;
    for characterp in pp.value.getPropertyEnumerator do begin
      if characterp.Value.kind <> pvkString then error;
      characterMaps.include(characterp.Name, characterp.Value.toString);
    end;
  end;

begin
  for pp in v.getPropertyEnumerator do begin
    case pp.Value.getSequenceCount of
      0: continue;
      1: ; //fine
      else case pp.Name of
        'cdata-section-elements', 'suppress-indentation': ; //fine
        else raiseXPTY0004TypeError(v, 'Invalid parameter');
      end;
    end;
    case pp.Name of
      'allow-duplicate-names': allowDuplicateNames := valueBool();
      'byte-order-mark': valueBool(); //todo
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
      else if staticOptions then raiseXQEvaluationException('XQST0109', 'Unknown serialization option.');
    end;
  end;
end;

procedure TXQSerializationParams.setFromXQValue(const context: TXQEvaluationContext;const v: IXQValue);
begin
  case v.kind of
    pvkObject: begin
      setDefault(true);
      setFromMap(context, v);
    end;
    pvkNode: begin
      setDefault(false);
      setFromNode(context, v.toNode, false);
    end
    else if v.getSequenceCount > 0 then raiseXPTY0004TypeError(v, 'serialize params must be map() or node')
    else setDefault(true);
  end;
end;

procedure TXQSerializationParams.setMethod(const s: string);
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

procedure TXQSerializationParams.setStandalone(s: string; fromMap: boolean);
begin
  s := trim(s);
  if s = 'omit' then standalone := xdsOmit
  else setStandalone(toSerializationBool(s, fromMap));
end;

procedure TXQSerializationParams.setStandalone(s: boolean);
begin
  if s then standalone := xdsYes
  else standalone := xdsNo;
end;

procedure TXQSerializationParams.setNormalizationForm(const s: string);
begin
  normalizationForm := unicodeNormalizationForm(s);
  case normalizationForm of
    unfEmpty, unfUnknown: raise EXQEvaluationException.Create('SESU0011', 'Unknown normalization method: '+s);
  end;
end;

function TXQSerializationParams.hasNormalizationForm: boolean;
begin
  result := not (normalizationForm in [unfNone, unfUnknown, unfEmpty])
end;

procedure TXQSerializationParams.setEncoding(const s: string);
begin
  encoding := s;
  encodingCP := strEncodingFromName(s);
  if encodingCP = $FFFF then raiseXQEvaluationException('SESU0007', 'Unknown encoding: '+s);
end;

function TXQSerializationParams.needQNameList(var list: PXQHashsetQName): PXQHashsetQName;
begin
  if list = nil then new(list,init);
  result := list;
end;

function TXQSerializationParams.isHTML5: boolean;
begin
  result := (htmlVersion = '5.0') or (htmlVersion = '5')
end;

function TXQSerializationParams.getContentType: string;
begin
  result := mediaType + '; charset=' + encoding;
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
  function appendXMLHTMLAttribute(const n: TTreeAttribute; html: boolean): boolean;
  procedure init;
end;
procedure TSpecialStringHandler.init;
begin
  //calling this procedure silences an object never used warning
end;

function TSpecialStringHandler.normalizeString(p: pchar; len: sizeint): string;
begin
  result := xquery__serialization.normalizeString(strFromPchar(p, len), params^.normalizationForm)
end;

function TSpecialStringHandler.normalizeString(const s: string): string;
begin
  result := normalizeString(pchar(s), length(s));
end;

procedure TSpecialStringHandler.appendJSONStringWithoutQuotes(const s: string);
var needNormalization, needEscaping: Boolean;
var enumerator: TUTF8StringCodePointBlockEnumerator;
  entity: TXQHashmapStrStr.PHashMapEntity;
  hadNext: Boolean;
begin
  if params^.characterMaps = nil then begin
    serializer^.appendJSONStringWithoutQuotes(normalizeString(s));
  end else begin
    needNormalization := params^.hasNormalizationForm;
    needEscaping := false;
    enumerator.init(s);
    repeat
      hadNext := enumerator.MoveNext;
      entity := params^.characterMaps.findEntity(enumerator.currentPos, enumerator.currentByteLength);
      if (not hadNext) or (entity <> nil) then begin
        if needNormalization or needEscaping then serializer^.appendJSONStringWithoutQuotes(normalizeString(enumerator.markedPos, enumerator.markedByteLength))
        else serializer^.append(enumerator.markedPos, enumerator.markedByteLength);
        needEscaping := false;
        enumerator.markNext;
        if entity <> nil then
          serializer^.append(string(entity^.Value));
      end else case enumerator.currentPos^ of
        #0..#31, '"', '\', '/': needEscaping := true;
      end;
    until not hadNext;
  end;
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

function TSpecialStringHandler.appendXMLHTMLAttribute(const n: TTreeAttribute; html: boolean): boolean;
  procedure appendURIEscapedAttribute(n: TTreeAttribute);
  var
    temp: String;
  begin
    temp := TInternetAccess.urlEncodeData(xquery__serialization.normalizeString(n.realvalue, unfNFC), ueXPathHTML4);
    if params^.hasNormalizationForm or assigned(params^.characterMaps) then
      appendXMLHTMLCharacterMappedText(temp, true, html)
     else
      appendXMLHTMLAttributeText(temp, html);
  end;

begin
  serializer.append(' ');
  serializer.append(n.getNodeName());
  serializer.append('="');
  if params^.escapeURIAttributes and html and htmlAttributeIsURI(n) then
    appendURIEscapedAttribute(n)
   else
    appendXMLHTMLCharacterMappedText(n.realvalue, true, html);
  serializer.append('"');
  result := true;
end;

function isUnicodeEncoding(e: TSystemCodePage): boolean;
begin
  e := strActualEncoding(e);
  case e of
    CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE: result := true;
    else result := false;
  end;
end;

procedure serializeNodes(base: TTreeNode; var builder: TXQSerializer; nodeSelf: boolean; html: boolean; params: PSerializationParams);
type TIncludeContentType = (ictIgnore, ictSearchingForHead, ictRemoveOld);
var known: TNamespaceList;
    deadPrefixes: TNamespaceList;
    indentationAllowed, undeclarePrefixes: boolean;
    xhtml, representsHTML, isHTML5: boolean;

    includeContentType: TIncludeContentType;


  function elementIsHTML(n: TTreeNode): boolean;
  begin
    //since in HTML5 empty and xhtml namespace are the same; older versions treat them separately
    result := (   (isHTML5 or html) and ( (n.namespace = nil) or (n.namespace.getURL = '') ) )
             or ( (isHTML5 or xhtml) and ( n.namespace.getURL = XMLNamespaceUrl_XHTML) )
  end;

  function elementIsPhrasing(n: TTreeNode): boolean;
  begin
    if elementIsHTML(n) then result := htmlElementIsPhrasing(n)
    else if n.getNamespaceURL() = XMLNamespaceURL_MathML then result := true
    else result := false
  end;

  function elementDescendantsMightBeIndented(n: TTreeNode; isHTMLElement: boolean): boolean;
  var a: TTreeAttribute;
  begin
    if Assigned(params.suppressIndentation) then begin
      if params.suppressIndentation.contains(n.namespace, n.value) then
        exit(false);
      if representsHTML and params.suppressIndentation.contains(n.namespace, lowercase(n.value)) then
        exit(false);
    end;
    if isHTMLElement then begin
      if htmlElementIsFormattedWhitespace(n.value) then exit(false);
    end else for a in n.getEnumeratorAttributes do
      if (a.value = 'space') and equalNamespaces(a.namespace, XMLNamespace_XML) and (a.realvalue = 'preserve') then
        exit(false);
    result := true;
  end;


  function requireNamespace(n: TNamespace): string;
  begin //that function is useless the namespace should always be in known. But just for safety...
    if (n = nil) or (n.getURL = XMLNamespaceUrl_XML) or (n.getURL = XMLNamespaceUrl_XMLNS) or (known.hasNamespace(n)) then exit('');
    known.add(n);
    result := ' ' + n.serialize;
  end;

  procedure killPrefixIfNecessary(n: TTreeNode; ns: TNamespace);
  var a: TTreeAttribute;
  begin
    if ns.prefix = '' then exit;
    case ns.url of
      XMLNamespaceUrl_XHTML, XMLNamespaceUrl_SVG, XMLNamespaceURL_MathML: begin
        for a in n.getEnumeratorAttributes do if a.namespace = ns then exit;
        if n.isNamespaceUsed(nil) then exit;
        deadPrefixes.add(ns);
      end;
    end;
  end;

  procedure appendContentTypeNow;
  begin
    if indentationAllowed then begin
      builder.indent;
      builder.append(LineEnding);
      builder.appendIndent;
    end;
    builder.append('<meta http-equiv="Content-Type"');
    builder.appendXMLElementAttribute('content', params.getContentType);
    if xhtml then builder.append(' />')
    else builder.append('>');
    if indentationAllowed then
      builder.unindent;
    includeContentType := ictRemoveOld;
  end;

  procedure inner(n: TTreeNode; insideHTMLElement: boolean); forward;

  procedure outer(n: TTreeNode; parentIsHTMLElement: boolean);
    procedure appendNodeName(n: TTreeNode);
    begin
      with builder do
        if (n.namespace = nil) or (n.namespace.prefix = '') or deadPrefixes.hasNamespace(n.namespace) then append(n.value)
        else begin
          append(n.namespace.prefix);
          append(':');
          append(n.value);
        end;
    end;
    procedure appendXMLElementEndTag2(n: TTreeNode);
    begin
      with builder do begin
        append('</');
        appendNodeName(n);
        append('>');
      end;
    end;

  var attrib: TTreeAttribute;
      oldnamespacecount: integer;
      i: Integer;
      temp: TNamespace;
      includeContentTypeHere: Boolean;
      isHTMLElement: boolean;
      oldDeadPrefixCount: Int64;
  begin
    with n do with builder do
    case typ of
      tetText: begin
        if assigned(builder.onInterceptAppendXMLHTMLText) and builder.onInterceptAppendXMLHTMLText(n, parentIsHTMLElement) then begin
          //empty
        end else if not parentIsHTMLElement then append(xmlStrEscape(value)) //using appendXMLText fails (lacking automatic encoding conversion?)
        else if (getParent() <> nil) and htmlElementIsImplicitCDATA(getParent().value) then append(value)
        else appendHTMLText(value);
      end;
      tetClose: appendXMLElementEndTag2(n);
      tetComment: begin
        append('<!--');
        append(value);
        append('-->');
      end;
      tetProcessingInstruction:
        if parentIsHTMLElement and html then appendHTMLProcessingInstruction(value, getAttribute(''))
        else appendXMLProcessingInstruction(value, getAttribute(''));
      tetOpen: begin
        isHTMLElement := (parentIsHTMLElement and assigned(n.parent) and (n.parent.namespace = n.namespace))
                         or (representsHTML and elementIsHTML(n));
        if isHTMLElement and (includeContentType = ictRemoveOld) and (htmlElementIsMetaContentType(n)) then exit;

        oldDeadPrefixCount := deadPrefixes.count;
        if known <> nil then begin
          oldnamespacecount:=known.count;
          n.getOwnNamespaces(known);
        end else begin
          oldnamespacecount:=0;
          known := TNamespaceList.Create;
          n.getAllNamespaces(known)
        end;

        if isHTML5 then
          for i:=oldnamespacecount to known.Count - 1 do
            killPrefixIfNecessary(n, known.items[i]);

        append('<');
        appendNodeName(n);

        for i:=oldnamespacecount to known.Count - 1 do begin
          if deadPrefixes.hasNamespace(known.items[i]) then begin
            appendXMLElementAttribute('xmlns', known.items[i].url);
          end else if (known.items[i].getURL <> '') or
             undeclarePrefixes or
             (known.hasNamespacePrefixBefore(known.items[i].getPrefix, oldnamespacecount)
                and (isNamespaceUsed(known.items[i])
                     or ((known.items[i].getPrefix = '') and (isNamespaceUsed(nil))))) then begin
                       append(' ');
                       append(known.items[i].serialize);
                     end;

        end;

        if namespace <> nil then append(requireNamespace(namespace))
        else if known.hasNamespacePrefix('', temp) then
          if temp.getURL <> '' then begin
            known.add(TNamespace.Make('', ''));
            append(' xmlns=""');
          end;
        if attributes <> nil then
          for attrib in getEnumeratorAttributes do
            append(requireNamespace(attrib.namespace));


        if attributes <> nil then
          for attrib in getEnumeratorAttributes do
            if not attrib.isNamespaceNode then begin
              if assigned(builder.onInterceptAppendXMLHTMLAttribute) and builder.onInterceptAppendXMLHTMLAttribute(attrib, isHTMLElement) then begin
                //empty
              end else begin
                append(' ');
                appendNodeName(attrib);
                append('="');
                if isHTMLElement then appendHTMLAttrib(attrib.realvalue)
                else appendXMLAttrib(attrib.realvalue);
                append('"');
              end;
            end;

        includeContentTypeHere := isHTMLElement and (includeContentType = ictSearchingForHead) and htmlElementIsHead(n);
        if (n.next = reverse)
           and not includeContentTypeHere
           and (   (html and (not isHTMLElement or htmlElementIsChildless(value)))
                or (xhtml and isHTMLElement and htmlElementIsExpectedEmpty(n, ishtml5))
                or not representsHTML
           ) then begin
          if not isHTMLElement then append('/>')
          else if html then append('>')
          else append(' />');
        end else begin
          append('>');
          if includeContentTypeHere then appendContentTypeNow;
          inner(n, isHTMLElement);
          appendXMLElementEndTag2(n);
        end;

        while known.count > oldnamespacecount do
          known.Delete(known.count-1);
        while deadPrefixes.count > oldDeadPrefixCount do
          deadPrefixes.Delete(deadPrefixes.count-1);
      end;
      tetDocument: inner(n, false);
      else; //should not happen
    end;
  end;

  procedure inner(n: TTreeNode; insideHTMLElement: boolean);
  var sub: TTreeNode;
    oldIndentationAllowed: Boolean;

  begin
    if not (n.typ in TreeNodesWithChildren) then exit;
    oldIndentationAllowed := indentationAllowed;
    if indentationAllowed then begin
      if Assigned(params) then indentationAllowed := elementDescendantsMightBeIndented(n, insideHTMLElement);
      sub := n.getFirstChild();
      while (sub <> nil) and indentationAllowed do begin
        case sub.typ of
          tetText: indentationAllowed := sub.value.IsBlank();
          tetOpen: indentationAllowed := not (insideHTMLElement and elementIsPhrasing(sub));
        end;
        sub := sub.getNextSibling();
      end;
      if indentationAllowed then begin
        builder.indent;
      end;
    end;

    sub := n.getFirstChild();
    while sub <> nil do begin
      if indentationAllowed and (sub.typ <> tetText) then begin
        builder.append(LineEnding);
        builder.appendIndent;
      end;
      if (not indentationAllowed) or (sub.typ <> tetText) then
        outer(sub, insideHTMLElement);
      sub := sub.getNextSibling();
    end;

    if indentationAllowed then begin
      builder.unindent;
      builder.append(LineEnding);
      builder.appendIndent;
    end;
    indentationAllowed := oldIndentationAllowed;
  end;

begin
  if builder.insertWhitespace = xqsiwIndent then indentationAllowed := true and (base.typ <> tetText)
  else indentationAllowed := false;
  xhtml := assigned(params) and (params.method = xqsmXHTML);
  representsHTML := html or xhtml;
  isHTML5 := representsHTML and (not assigned(params) or params.isHTML5);
  if representsHTML and assigned(params) and params.includeContentType then includeContentType := ictSearchingForHead
  else includeContentType := ictIgnore;
  undeclarePrefixes := assigned(params) and params.undeclarePrefixes;

  known := nil;
  deadPrefixes := TNamespaceList.Create;
  if nodeSelf then outer(base, representsHTML)
  else inner(base, elementIsHTML(base));
  known.free;
  deadPrefixes.Free;
end;



function serializeJSON(const params: TXQSerializationParams; const v: IXQValue): string;
var serializer: TXQSerializer;
  temp, temp2: string;
  interceptor: TSpecialStringHandler;
  cp: Integer;
begin
  serializer.init(@temp);
  serializer.standard := true;

  serializer.allowDuplicateNames := params.allowDuplicateNames;
  serializer.insertWhitespace := params.indent;
  case params.jsonNodeOutputMethod of
    'xml': serializer.nodeFormat := tnsXML;
    'xhtml': serializer.nodeFormat := tnsXML;
    'html': serializer.nodeFormat := tnsHTML;
    'text': serializer.nodeFormat := tnsText;
    else serializer.error('SEPM0016', v.toValue);
  end;

  if params.hasNormalizationForm or (params.characterMaps <> nil) then begin
    interceptor.init;
    interceptor.params := @params;
    interceptor.serializer := @serializer;
    serializer.onInterceptAppendJSONString := @interceptor.appendJSONStringWithoutQuotes;
  end;

  v.jsonSerialize(serializer);
  serializer.final;

  if not isUnicodeEncoding(params.encodingCP) then begin
    serializer.init(@temp2);
    for cp in temp.enumerateUtf8CodePoints do begin
      if cp <= $7F then serializer.append(chr(cp))
      else serializer.appendJSONStringUnicodeEscape(cp);
    end;
    serializer.final;
    temp := temp2;
  end;

  result := temp;
end;

function serializeAdaptive(const params: TXQSerializationParams; const v: IXQValue): string;
var serializer: TXQSerializer;
  temp, itemSeparator: string;
  w: PIXQValue;
  first: Boolean;
begin
  serializer.init(@temp);
  serializer.standard := true;

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
  serializer.final;
  result := temp;
end;


function serializeXMLHTMLText(var params: TXQSerializationParams; const v: IXQValue): string;
var serializer: TXQSerializer;

var firstElement: TTreeNode = nil;

  function findRootNodeCount(const v: IXQValue): integer;
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

  procedure addAtomicString(const s: string);
  begin
    addItemStart;
    if not hasItemSeparator then begin
      if not wasNodeOrFirst then serializer.append(' ');
      wasNodeOrFirst := false;
    end;
    serializer.append(s);
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

var temp: string;
  hasDoctypeSystem: Boolean;
  interceptor: TSpecialStringHandler;
begin
  serializer.init(@temp);
  serializer.standard := true;

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
    end;

  if params.hasNormalizationForm or (params.characterMaps <> nil) or assigned(params.cdataSectionElements) or params.escapeURIAttributes then begin
    interceptor.init;
    interceptor.params := @params;
    interceptor.serializer := @serializer;
    interceptor.isUnicodeEncoding := isUnicodeEncoding(params.encodingCP);
    if params.hasNormalizationForm or (params.characterMaps <> nil) or params.escapeURIAttributes then
      serializer.onInterceptAppendXMLHTMLAttribute := @interceptor.appendXMLHTMLAttribute;
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
          if not omitXmlDeclaration then
            serializer.appendXMLHeader(version, encoding, standalone);
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
      //xqsmJSON, xqsmAdaptive: ;
    end;
  end;

  wasNodeOrFirst := true;
  add(v);

  serializer.final;
  result := temp;
end;

function serializeWithContextDefaults(const context: TXQEvaluationContext; const value: IXQValue): string;
begin
  result := serialize(context, value, context.staticContext.serializationOptions);
end;

function serialize(const context: TXQEvaluationContext; const value: IXQValue; const serializationParams: IXQValue): string;
var
  params: TXQSerializationParams;
begin
  params.characterMaps := nil;
  if serializationParams <> nil then params.setFromXQValue(context, serializationParams)
  else params.setDefault(false);

  case params.method of
    xqsmJSON: result := serializeJSON(params, value);
    xqsmAdaptive: result := serializeAdaptive(params, value);
    else result := serializeXMLHTMLText(params, value);
  end;

  params.done
end;

initialization
xquery.globalSerializationCallback := @serializeWithContextDefaults;
end.

