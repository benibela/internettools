unit xquery__serialization_nodes;

{$include ../internettoolsconfig.inc}
{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils, xquery.internals.common, xquery.namespaces, simplehtmltreeparser;

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
  procedure addHTMLLowercaseQNames(html5: boolean);
end;
PXQHashsetQName = ^TXQHashsetQName;
{$if false}
TXQTwoLevelHashsetCaseInsensitiveASCII = object(specialize TXQHashmapStrCaseInsensitiveASCIIOwning<PXQHashsetStrCaseInsensitiveASCII, TTrackOwnedXQHashsetStr>)
  function getOrCreate(const a: string): PXQHashsetStrCaseInsensitiveASCII;
  function contains(const a, b: string): boolean;
  procedure include(const a, b: string);
  procedure exclude(const a, b: string);
end;
{$endif}

type
TXQSerializerInsertWhitespace = (xqsiwNever, xqsiwConservative, xqsiwIndent);
TXQSerializerOnString = procedure (const s: string) of object;
TXQSerializerOnNode = function (const n: TTreeNode; html: boolean): boolean of object;
TXQSerializerOnAttribute = procedure (const n: TTreeAttribute; html: boolean) of object;
TIndentingJSONXHTMLStrBuilder = object(TJSONXHTMLStrBuilder)
  insertWhitespace: TXQSerializerInsertWhitespace;
  onInterceptAppendJSONString: TXQSerializerOnString;
  onInterceptAppendXMLHTMLText: TXQSerializerOnNode;
  onInterceptAppendXMLHTMLAttribute: TXQSerializerOnAttribute;
  procedure init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});
  procedure indent;
  procedure appendIndent;
  procedure unindent;
protected
  indentCache: string;
  indentLevel: SizeInt;

end;

TXQSerializationMethod = (xqsmXML, xqsmXHTML, xqsmHTML, xqsmText, xqsmJSON, xqsmAdaptive);
TXQSerializationParams = record
  isAbsentMarker: string;
  method: TXQSerializationMethod;
  encoding: string;
  encodingCP: TSystemCodePage;
  byteOrderMark: boolean;
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

  //custom
  standardMode, allowEncodingConversion: boolean;

  procedure done;

  procedure initDefault(isFromMap: boolean);

  function hasNormalizationForm: boolean;
  function needQNameList(var list: PXQHashsetQName): PXQHashsetQName;

  function isHTML5: boolean;
  function getContentType: string;
end;

const XMLNamespaceUrl_XHTML = 'http://www.w3.org/1999/xhtml';
      XMLNamespaceURL_MathML = 'http://www.w3.org/1998/Math/MathML';
      XMLNamespaceUrl_SVG = 'http://www.w3.org/2000/svg';

type PSerializationParams = ^TXQSerializationParams;
procedure serializeNodes(base: TTreeNode; var builder: TIndentingJSONXHTMLStrBuilder; nodeSelf: boolean; html: boolean; params: PSerializationParams);

implementation
uses
  strutils, bbutils,
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

procedure TXQHashsetQName.addHTMLLowercaseQNames(html5: boolean);
  procedure transformFromTo(from: PXQHashsetStr; tonamespace: string);
  var str: string;
  begin
    if from = nil then exit;
    for str in from^ do begin
      Include(tonamespace, lowercase(str));
    end;
  end;

var v: TXQHashsetQName.TKeyValuePairOption;
begin
  if html5 then begin
    transformFromTo(getOrDefault(''), XMLNamespaceUrl_XHTML);
    transformFromTo(getOrDefault(XMLNamespaceUrl_XHTML), '');
  end;
  for v in self do begin
    transformFromTo(v.value, v.key);
  end;
end;




{$if false}
function TXQTwoLevelHashsetCaseInsensitiveASCII.getOrCreate(const a: string): PXQHashsetStrCaseInsensitiveASCII;
var
  ent: PHashMapEntity;
begin
  ent := findEntity(a, true);
  if ent.Value = nil then new(PXQHashsetStrCaseInsensitiveASCII(ent.Value), init);
  result := PXQHashsetStrCaseInsensitiveASCII(ent.Value);
end;

function TXQTwoLevelHashsetCaseInsensitiveASCII.contains(const a, b: string): boolean;
var
  nestedSet: PXQHashsetStrCaseInsensitiveASCII;
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
  nestedSet: PXQHashsetStrCaseInsensitiveASCII;
begin
  nestedSet := getOrCreate(a);
  if assigned(nestedSet) then nestedSet.exclude(b);
end;
{$endif}







procedure TIndentingJSONXHTMLStrBuilder.init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});
begin
  inherited init(abuffer, basecapacity, aencoding);
  insertWhitespace := xqsiwConservative;
  indentCache := '  ';
  indentLevel := 0;

  onInterceptAppendXMLHTMLAttribute := nil;
  onInterceptAppendXMLHTMLText := nil;

  //this is basically a custom VMT on the stack
  onInterceptAppendJSONString := @appendJSONStringWithoutQuotes;
end;

procedure TIndentingJSONXHTMLStrBuilder.indent;
begin
  inc(indentLevel);
  while 2 * indentLevel > length(indentCache) do indentCache := indentCache + indentCache;
end;

procedure TIndentingJSONXHTMLStrBuilder.appendIndent;
begin
  append(pchar(indentCache), 2 * indentLevel);
end;

procedure TIndentingJSONXHTMLStrBuilder.unindent;
begin
  dec(indentLevel);
end;






procedure TXQSerializationParams.done;
begin
  if assigned(characterMaps) then Dispose(characterMaps,done);
  if assigned(cdataSectionElements) then Dispose(cdataSectionElements,done);
  if assigned(suppressIndentation) then Dispose(suppressIndentation,done);
end;

procedure TXQSerializationParams.initDefault(isFromMap: boolean);
begin
  isAbsentMarker := #0;
  method := xqsmXML;
  if isFromMap then version := '1.0' else version := isAbsentMarker;
  encoding := 'UTF-8';
  encodingCP := CP_UTF8;
  byteOrderMark := false;
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

  standardMode := true;
  allowEncodingConversion := false;
end;



function TXQSerializationParams.hasNormalizationForm: boolean;
begin
  result := not (normalizationForm in [unfNone, unfUnknown, unfEmpty])
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


function isUnicodeEncoding(e: TSystemCodePage): boolean;
begin
  e := strActualEncoding(e);
  case e of
    CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE: result := true;
    else result := false;
  end;
end;

procedure serializeNodes(base: TTreeNode; var builder: TIndentingJSONXHTMLStrBuilder; nodeSelf: boolean; html: boolean; params: PSerializationParams);
type TIncludeContentType = (ictIgnore, ictSearchingForHead, ictRemoveOld);
var known: TNamespaceList;
    deadPrefixes: TNamespaceList;
    indentationAllowed, inCDATAElement, undeclarePrefixes: boolean;
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
    else case n.getNamespaceURL() of
      XMLNamespaceURL_MathML, XMLNamespaceUrl_SVG: result := true
      else result := false
    end;
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
      if htmlElementIsFormattedWhitespace(n) then exit(false);
    end else for a in n.getEnumeratorAttributes do
      if (a.hash = XMLAttributeNameHashs.space) and (a.value = 'space') and equalNamespaces(a.namespace, XMLNamespace_XML) and (a.realvalue = 'preserve') then
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
  procedure innerDocument(n: TTreeNode); forward;

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
        else if inCDATAElement then append(value)
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
              append(' ');
              appendNodeName(attrib);
              if isHTMLElement
                 and (length(attrib.value) = length(attrib.realvalue)) and striEqual(attrib.value, attrib.realvalue)
                 and htmlAttributeIsBooleanAttribute(attrib) then begin
                //skip
              end else begin
                append('="');
                if inCDATAElement then append(attrib.realvalue)
                else if assigned(builder.onInterceptAppendXMLHTMLAttribute) then builder.onInterceptAppendXMLHTMLAttribute(attrib, isHTMLElement)
                else if isHTMLElement then appendHTMLAttrib(attrib.realvalue)
                else appendXMLAttrib(attrib.realvalue);
                append('"');
              end;
            end;

        includeContentTypeHere := isHTMLElement and (includeContentType = ictSearchingForHead) and htmlElementIsHead(n);
        if (n.next = reverse)
           and not includeContentTypeHere
           and (   (html and (not isHTMLElement or htmlElementIsChildless(hash, value)))
                or (xhtml and isHTMLElement and htmlElementIsExpectedEmpty(n, ishtml5))
                or not representsHTML
           ) then begin
          if not isHTMLElement then append('/>')
          else if html then append('>')
          else append(' />');
        end else begin
          append('>');
          if includeContentTypeHere then appendContentTypeNow;
          if not inCDATAElement then begin
            inCDATAElement := isHTMLElement and htmlElementIsImplicitCDATA(hash, value);
            inner(n, isHTMLElement);
            inCDATAElement := false;
          end else inner(n, isHTMLElement);
          appendXMLElementEndTag2(n);
        end;

        while known.count > oldnamespacecount do
          known.Delete(known.count-1);
        while deadPrefixes.count > oldDeadPrefixCount do
          deadPrefixes.Delete(deadPrefixes.count-1);
      end;
      tetDocument: innerDocument(n);
      else; //should not happen
    end;
  end;

  function elementAndChildrenMightBeIndented(n: TTreeNode; insideHTMLElement: boolean): boolean;
  var
    sub: TTreeNode;
  begin
    result := true;
    if Assigned(params) then result := elementDescendantsMightBeIndented(n, insideHTMLElement);
    sub := n.getFirstChild();
    while (sub <> nil) and result do begin
      case sub.typ of
        tetText: result := sub.value.IsBlank();
        tetOpen: result := not (insideHTMLElement and elementIsPhrasing(sub));
        tetProcessingInstruction: result := not insideHTMLElement;
        else;
      end;
      sub := sub.getNextSibling();
    end;
  end;

  procedure innerDocument(n: TTreeNode);
  const insideHTMLElement = false;
  var
    sub: TTreeNode;
    oldIndentationAllowed: Boolean;
    first: boolean = true;
  begin
    oldIndentationAllowed := indentationAllowed;
    indentationAllowed := indentationAllowed and elementAndChildrenMightBeIndented(n, insideHTMLElement);
    sub := n.getFirstChild();
    while sub <> nil do begin
      if (not indentationAllowed) or (sub.typ <> tetText) then begin
        if indentationAllowed then begin
          if not first then builder.append(LineEnding);
          first := false;
          builder.appendIndent;
        end;
        outer(sub, insideHTMLElement);
      end;
      sub := sub.getNextSibling();
    end;
    //if indentationAllowed then builder.append(LineEnding); without this there is no line break between multiple documents in a sequence, however with it there is a pointless break after the sequence
    indentationAllowed := oldIndentationAllowed;
  end;

  procedure inner(n: TTreeNode; insideHTMLElement: boolean);
  var sub: TTreeNode;
    oldIndentationAllowed: Boolean;
  begin
    if not (n.typ in TreeNodesWithChildren) then exit;
    oldIndentationAllowed := indentationAllowed;
    if indentationAllowed  then begin
      indentationAllowed := elementAndChildrenMightBeIndented(n, insideHTMLElement);
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
  inCDATAElement := false;
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




initialization

end.

