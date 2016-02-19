function xqFunctionHead(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := args[0].get(1);
end;

function xqFunctionTail(const args: TXQVArray): IXQValue;
var
  len: Integer;
  seq: TXQValueSequence;
  i: Integer;
begin
  requiredArgCount(args, 1);
  len := args[0].getSequenceCount;
  if len < 2 then exit(xqvalue);
  if len = 2 then exit(args[0].get(2));
  seq := TXQValueSequence.create(len-1);
  for i := 2 to len do
    seq.seq.add(args[0].get(i));
  result := seq;
end;

function xqFunctionHas_Children(const context: TXQEvaluationContext;  const arg: TXQVArray): IXQValue;
var n: TTreeNode;
begin
  if length(arg) = 0 then n := context.contextNode()
  else n := arg[0].toNode;
  if n = nil then exit(xqvalueFalse);
  result := xqvalue(n.getFirstChild() <> nil);
end;

function xqFunctionPath(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
  function path(n: TTreeNode): string;
    function getPosition(checkValue: boolean): integer;
    var cur: TTreeNode;
    begin
      result := 1;
      cur := n.getPreviousSibling();
      while cur <> nil do begin
        if (cur.typ = n.typ) and (equalNamespaces(cur.namespace, n.namespace)) and ((not checkValue) or (cur.value = n.value)) then inc(result);
        cur := cur.getPreviousSibling();
      end;
    end;
    function getQualifiedName: string;
    begin
      result := n.value;
    end;

  var
    step: String;
  begin
    result := '';
    while n.parent <> nil do begin
      case n.typ of
        tetOpen: begin
          if n.namespace <> nil then step := 'Q{' + n.namespace.getURL + '}'
          else step := 'Q{}';
          step += n.value + '['+IntToStr(getPosition(true))+']';
        end;
        tetAttribute: begin
          if n.namespace <> nil then step := '@Q{' + n.namespace.getURL + '}'
          else step := '@';
          step += n.value;
        end;
        tetText: step := 'text()['+IntToStr(getPosition(false))+']';
        tetComment: step := 'comment()['+IntToStr(getPosition(false))+']';
        tetProcessingInstruction: step := 'processing-instruction('+n.value+')['+IntToStr(getPosition(true))+']';
        else raise EXQEvaluationException.create('pxp:INTERNAL', 'Invalid node: '+n.toString());
      end;
      if result <> '' then result := step + '/' + result
      else result := step;
      n := n.parent;
    end;
    if n.typ = tetDocument then exit('/' + result);
    if result <> '' then result := '/' + result;
    result := 'Q{http://www.w3.org/2005/xpath-functions}root()' + result;
  end;

var
  node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then begin
    if args[0].isUndefined then exit(args[0]);
    node := args[0].toNode;
  end else node := context.contextNode();
  result := xqvalue(path(node));
end;


function xqFunctionFunction_lookup(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  qname: TXQValueQName;
  temp: TXQTermDefineFunction;
  arity: Int64;
  //funcbody: TXQTermNamedFunction;
begin
  requiredArgCount(args, 2);
  requiredArgType(args[0], baseSchema.QName);
  qname := args[0] as TXQValueQName;
  arity := args[1].toInt64;
  temp := TXQTermDefineFunction.createReference(TXQTermNamedFunction.create(qname.url, {qname.prefix, todo}qname.local, arity), arity);
  temp.name := TXQEQNameWithPrefix.create;
  temp.name.namespaceURL := qname.url;
  temp.name.namespacePrefix := qname.prefix;
  temp.name.localname := qname.local;
  try
    result := temp.evaluate(context);
  except
    on e: EXQEvaluationException do
      if e.errorCode = 'XPST0017' then result := xqvalue() //todo: do not use exceptions for control flow
      else raise;
  end;
  temp.free;
end;

function xqFunctionFunction_name(const args: TXQVArray): IXQValue;
var
  f: TXQValueFunction;
begin
  requiredArgCount(args, 1);
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].debugAsStringWithTypeAnnotation());
  f := args[0] as TXQValueFunction;
  if f.name = '' then exit(xqvalue);
  result := TXQValueQName.create(f.namespaceURL, f.namespacePrefix, f.name);
end;

function xqFunctionFunction_arity(const args: TXQVArray): IXQValue;
var
  f: TXQValueFunction;
begin
  requiredArgCount(args, 1);
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].debugAsStringWithTypeAnnotation());
  f := args[0] as TXQValueFunction;
  result := xqvalue(length(f.parameters));
end;



function xqFunctionFold(left: boolean; const args: TXQVArray): IXQValue;
var
  func: TXQValueFunction;
  newargs: TXQVArray;
  count: Integer;
  v: IXQValue;
  i: Integer;
begin
  requiredArgCount(args, 3);
  func := args[2] as TXQValueFunction;
  count := args[0].getSequenceCount;
  if count = 0 then exit(args[1]);
  SetLength(newargs, 2);
  if left then begin
    //fn:fold-left(fn:tail($seq), $f($zero, fn:head($seq)), $f)
    newargs[0] := args[1];
    for v in args[0] do begin
      newargs[1] := v;
      newargs[0] := func.evaluate(newargs, nil);
    end;
    result := newargs[0];
  end else begin
    // $f(fn:head($seq), fn:fold-right(fn:tail($seq), $zero, $f))
    newargs[1] := args[1];
    for i := count downto 1 do begin
      newargs[0] := args[0].get(i);
      newargs[1] := func.evaluate(newargs, nil);
    end;
    result := newargs[1];
  end;
end;


function xqFunctionFold_left(const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(true, args);
end;

function xqFunctionFold_right(const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(false, args);
end;


function xqFunctionFor_each_pair(const args: TXQVArray): IXQValue;
var
  seq1: TXQValue;
  seq2: TXQValue;
  func: TXQValueFunction;
  newargs: TXQVArray;
  count: Integer;
  resseq: TXQValueSequence;
  i: Integer;
begin
  requiredArgCount(args, 3);
  seq1 := args[0] as TXQValue;
  seq2 := args[1] as TXQValue;
  func := args[2] as TXQValueFunction;

  count := min(seq1.getSequenceCount, seq2.getSequenceCount);
  SetLength(newargs, 2);
  resseq := TXQValueSequence.create(count);
  for i := 1 to count do begin
    newargs[0] := seq1.get(i);
    newargs[1] := seq2.get(i);
    resseq.add(func.evaluate(newargs, nil));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;


function xqFunctionEnvironment_Variable(const args: TXQVArray): IXQValue;
var
  name: String;
  i: Integer;
begin
  requiredArgCount(args, 1);
  //result := xqvalue(GetEnvironmentVariable(args[0].toString));
  name := args[0].toString + '=';
  for i:=1 to GetEnvironmentVariableCount do
    if strBeginsWith(GetEnvironmentString(i), name) then
      exit(xqvalue(strCopyFrom(GetEnvironmentString(i), length(name) + 1)));
  result := xqvalue();
end;

function xqFunctionAvailable_Environment_Variables(const args: TXQVArray): IXQValue;
var
  i: Integer;
begin
  requiredArgCount(args, 0);
  result := xqvalue();
  for i:=1 to GetEnvironmentVariableCount do
    xqvalueSeqAddMove(result, xqvalue(strBefore(GetEnvironmentString(i), '=')));
end;

function xqFunctionParse_Common(const context: TXQEvaluationContext; const args: TXQVArray; typ: string): IXQValue;
var
  node: TTreeNode;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(args[0]);
  node := nil;
  try
    node := context.parseDoc(args[0].toString, context.staticContext.baseURI, 'text/'+typ+'; charset=utf-8');
  except
  end;
  if node = nil then raise EXQEvaluationException.create('FODC0006', 'Invalid document for parse-xml/-fragment/html: '+copy(args[0].toString, 1, 1000));
  (node as TTreeDocument).documentURI := '';
  result := xqvalue(node);
end;

function xqFunctionParse_XML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionParse_Common(context, args, 'xml');
end;

function xqFunctionParse_XML_Fragment(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionParse_Common(context, args, 'xml-external-parsed-entity');
end;

function xqFunctionParse_HTML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionParse_Common(context, args, 'html');
end;

type TSerializationParams = record
  isAbsentMarker: string;

  method, version, encoding: string;
  htmlVersion, doctypePublic, doctypeSystem: string;
  omitXmlDeclaration: boolean;
  standalone: string;
  itemSeparator: string;
  procedure setDefault;
  procedure setFromNode(paramNode: TTreeNode);
end;

procedure TSerializationParams.setDefault;
begin
  isAbsentMarker := #0;
  method := 'xml';
  version := '1.1';
  encoding := 'UTF-8';
  htmlVersion := '5.0';
  doctypePublic := isAbsentMarker;
  doctypeSystem := isAbsentMarker;
  omitXmlDeclaration := true;
  standalone := 'omit';
  itemSeparator := isAbsentMarker;
end;

procedure TSerializationParams.setFromNode(paramNode: TTreeNode);
  function tobool(const s:string): boolean;
  begin
    case s of
      'yes': result := true;
      'no': result := false;
      else raise EXQEvaluationException.create('SEPM0016', 'Expected boolean, got '+s);
    end;
  end;



const XMLNamespace_Output = 'http://www.w3.org/2010/xslt-xquery-serialization';
begin
  if paramNode = nil then exit;
  if not equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output)
     or (paramNode.value <> 'serialization-parameters')
     or (paramNode.typ <> tetOpen) then exit;
   paramNode := paramNode.getFirstChild();
   while paramNode <> nil do begin
     if (paramNode.typ = tetOpen) and equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output) then
       case paramNode.value of
         //'allow-duplicate-names': todo 3.1
         'byte-order-mark': ; //todo
         'cdata-section-elements': ;//todo
         'doctype-public': doctypePublic := paramNode.getAttribute('value');
         'doctype-system': doctypeSystem := paramNode.getAttribute('value');
         'encoding':       encoding := paramNode.getAttribute('value');
         'escape-uri-attributes': ;//todo
         'html-version':   htmlVersion := paramNode.getAttribute('value');
         'include-content-type': ;//todo
         'indent': ;//todo
         'item-separator': itemSeparator := paramNode.getAttribute('value');
         //'json-node-output-method': todo 3.1
         'media-type': ;//todo
         'method':         method := paramNode.getAttribute('value');
         'normalization-form': ;//todo
         'omit-xml-declaration': omitXmlDeclaration := tobool(paramNode.getAttribute('value'));
         'standalone':     standalone := paramNode.getAttribute('value');
         'suppress-indentation': ;//todo
         'undeclare-prefixes': ;//todo
         'use-character-maps': ;//todo
         'version':        version := paramNode.getAttribute('value');
         else raise EXQEvaluationException.create('SEPM0017', 'Invalid serialization parameter: '+paramNode.value);
       end;
     paramNode:= paramNode.getNextSibling();
   end;
end;


function xqFunctionSerialize(const args: TXQVArray): IXQValue;
var
  v, arg: IXQValue;
  params: TSerializationParams;
  strres: String;
  wasNodeOrFirst: Boolean;
  hasItemSeparator: Boolean;
  n: TTreeNode;
  firstElement: TTreeNode;

  procedure addAtomicString(const s: string);
  begin
    if not hasItemSeparator then begin
      if not wasNodeOrFirst then strres += ' ';
      wasNodeOrFirst := false;
    end;
    strres += s;
  end;

begin
  //this is incomplete, but the options that it handles should be handled completely (except for some invalid value checking)
  requiredArgCount(args, 1, 2);
  arg := args[0];
  params.setDefault;
  if length(args) = 2 then params.setFromNode(args[1].toNode);

  firstElement := nil;
  for v in arg do with params do begin
    n := v.toNode;
    if n = nil then continue;
    if n.typ = tetDocument then n := n.getFirstChild();
    if n.typ = tetAttribute then break; //fail later
    while (n <> nil) and (firstElement = nil) do begin
      if n.typ = tetOpen then firstElement := n
      else n := n.getNextSibling();
    end;
    if firstElement <> nil then break;
  end;

  strres := '';

  with params do begin
    case params.method of
      'xml', 'xhtml', 'html': begin
        //initialize missing default parameters
        if (method = 'html') then begin
          if (htmlVersion = isAbsentMarker) then htmlVersion := version;
          if (htmlVersion = isAbsentMarker) then htmlVersion := '5.0';
        end else if version = isAbsentMarker then version := '1.1';

        //headers
        if (method <> 'html') and not omitXmlDeclaration then begin
          strres += '<?xml version="'+version+'" encoding="'+encoding+'"';
          if standalone <> 'omit' then strres += ' standalone="'+standalone+'"';
          strres += '?>';
        end;
        if (htmlVersion = '5.0') and (doctypeSystem = isAbsentMarker)
           and (firstElement <> nil) and striEqual(firstElement.value, 'html')  {todo and only whitespace before firstelement}
           and ( (method = 'xhtml') or ( (method = 'html') and (doctypePublic = isAbsentMarker) )) then begin
           if method = 'html' then strres += '<!DOCTYPE html>'
           else strres += '<!DOCTYPE '+firstElement.value+'>'
        end else if doctypeSystem <> isAbsentMarker then begin
          if method = 'html' then strres += '<!DOCTYPE html '
          else begin
            if firstElement = nil then raise EXQEvaluationException.create('SEPM0016', 'No element given');
            strres += '<!DOCTYPE '+firstElement.value + ' ';
          end;
          if doctypePublic <> isAbsentMarker then strres += 'PUBLIC "' + doctypePublic + '" '
          else strres += 'SYSTEM ';
          strres += '"'+doctypeSystem+'">';
        end else if (method = 'html') and (doctypePublic <> isAbsentMarker) then
          strres += '<!DOCTYPE html PUBLIC "'+doctypePublic+'">';

        if method = 'xhtml' then method := 'xml';
      end;
      'text': begin
        //encoding: string;
      end;
    end;
  end;

  hasItemSeparator := params.itemSeparator <> params.isAbsentMarker;
  wasNodeOrFirst := true;
  for v in arg do with params do begin
    if hasItemSeparator then begin
      if not wasNodeOrFirst then strres += params.itemSeparator;
      wasNodeOrFirst := false;
    end;
    case v.kind of
      pvkNode: begin
        //this might be incomplete
        n := v.toNode;
        if n.typ in [tetAttribute] then raise EXQEvaluationException.create('SENR0001', 'Cannot serialize attribute');
        case method of
          'xml': strres += n.outerXML();
         // 'xhtml':;
          'html': strres += n.outerHTML();
          'text': strres += v.toString;
        end;
        if not hasItemSeparator then wasNodeOrFirst := true;
      end;
      pvkObject, pvkArray, pvkNull: raiseXPTY0004TypeError(v, 'serialization');
      pvkFunction: raise EXQEvaluationException.create('SENR0001', 'Cannot serialize function');
      else addAtomicString(v.toString);
    end;
  end;
  result := xqvalue(strres);
end;

function xqFunctionUnparsed_Text(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  url: String;
  data: String;
  encoding: String;
  contenttype: string;
  enc: TEncoding;
begin
  requiredArgCount(args, 1, 2);

  if args[0].isUndefined then exit(args[0]);

  url := context.staticContext.resolveDocURI(args[0].toString);
  if strContains(url, '#') then raise EXQEvaluationException.create('FOUT1170', 'Fragment identifiers are not allowed');

  data := context.staticContext.retrieveFromURI(url, contenttype, 'FOUT1170');
  //todo: guess encoding
  encoding := 'utf-8';
  if length(args) = 2 then encoding := args[1].toString;

  enc := strEncodingFromName(encoding);
  if enc = eUnknown then raise EXQEvaluationException.create(IfThen(length(args) = 2,  'FOUT1190', 'FOUT1200'), 'Unknown encoding: '+encoding);

  data := strConvertToUtf8(data, enc);

  result := xqvalue(data);
end;


function xqFunctionUnparsed_Text_Available(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqvalueTrue;
  try
    xqFunctionUnparsed_Text(context, args);
  except
    result := xqvalueFalse;
  end;
end;


function xqFunctionGenerateId(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then node := context.contextNode(true)
  else if args[0].isUndefined then exit(xqvalue(''))
  else node := args[0].toNode;
  result := xqvalue('autoid'+strFromPtr(node));
end;


function englishfy(const n: string): string;
  function englishfybelow1000(const n: string; lowest: integer): string;
  const digits: array['1'..'9'] of string = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');
        prefix: array['1'..'9'] of string = ('ten', 'twen', 'thir', 'for', 'fif', 'six', 'seven', 'eigh', 'nine');
  begin
    result := '';
    if (lowest >= 3) and (n[lowest-2] > '0') then result := digits[n[lowest-2]] + ' hundred ';
    if (lowest >= 2) then begin
      case n[lowest - 1] of
        '0': ;
        '1': begin
          case n[lowest] of
            '0': result += 'ten';
            '1': result += 'eleven';
            '2': result += 'twelve';
            '4': result += 'fourteen'
            else result += prefix[n[lowest]] + 'teen';
          end;
          exit;
        end;

        else begin
          result += prefix[n[lowest-1]] + 'ty';
          if n[lowest] > '0' then result += '-';
        end;
      end;
    end;
    if n[lowest] <> '0' then result += digits[n[lowest]];
  end;

function blockgen(i: integer): string;
const cache: array[0..18] of string = ('n', 'm', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non', 'dec', 'undec', 'duodec', 'tredec', 'quattuordec', 'quindec', 'sexdec', 'septendec', 'octodec');      //illion
var units: array[0..9] of string = ('', 'un', 'duo', 'tre', 'quattor', 'quinqua', 'se', 'septe', 'octo', 'nove');
    tens: array[0..9] of string = ('', 'deci', 'viginti', 'triginta', 'quadraginta', 'quinquaginta', 'sexaginta', 'septuaginta', 'octoginta', 'nonaginta');
    hundreds: array[0..9] of string = ('', 'centi', 'ducenti', 'trecenti', 'quadringenti', 'quingenti', 'sescenti', 'septigenti', 'octingenti', 'nongenti');
    ones: Integer;
begin
  if i <= high(cache) then begin
    result := cache[i] + 'illion'; //this returns nillion 10^0
  end else if i >= 1000 then begin
    //use the Conway-Wechsler system for larger illions
    result := blockgen(i div 1000);
    delete(result, length(result) - 1, 2); //illion => illi
    result += blockgen(i mod 1000);
  end else begin
    //tens and hundreds
    result := tens[(i div 10) mod 10] + hundreds[i div 100];
    //ones, with special cases for intermediate letter insertion
    if result = '' then result := units[i] // should not happen
    else begin
      ones := i mod 10;
      case ones of
        3, 6:
          case result[1] of
            'v','t','q': result := units[ones] + 's' + result;
            'o', 'x':
              if ones = 3 then result := units[ones] + 's' + result
              else result := units[ones] + 'x' + result;
            else result := units[ones] + result;
          end;
        7, 9: case result[1] of
          'o', 'v': result := units[ones] + 'm' + result;
          'n': result := units[ones] + result;
          else result := units[ones] + 'n' + result;
        end;
        else result := units[ones] + result;
      end;
    end;
    //remove vocal, add illion
    case result[length(result)] of
      'i': result += 'llion';
      'a','o','e','u': begin
        delete(result,length(result),1);
        result += 'illion';
      end;
      else result += 'illion';
    end;
  end;
end;

var
  temp: String;
  blockStart: Integer;
  i: Integer;
begin
  if (n = '0') or (n = '') then exit('zero');
  blockStart := (length(n)-1) div 3;
  result := '';
  for i := blockStart downto 0 do begin
    temp := englishfybelow1000(n, length(n)-3*i);
    if temp <> '' then begin
      if (temp <> '') and (temp[length(temp)] = ' ') then result += temp //hundred ends with space, so do not add another one
      else result += temp + ' ';
      if i > 1 then result += blockgen(i-1)
      else if i = 1 then result += 'thousand';
      result += ' ';
    end;
  end;
  while (result <> '') and (result[length(Result)] = ' ') do delete(result, length(result), 1);
end;

function alphabetify(number: BigDecimal; one: char): string;
var base, aaa, aaaa: BigDecimal;
    len: Integer;
    remainder: BigDecimal;
    quotient: BigDecimal;
    i: Integer;
begin
  base := 26;
  aaa := 1; aaaa := base;
  len := 1;
  number := number - 1;
  while number >= aaaa do begin
    aaa := aaaa;
    aaaa := aaa * 26;
    number -= aaa;
    inc(len);
  end;
  SetLength(result, len);
  for i := 1 to len do begin
    divideModNoAlias(quotient, remainder, number, base, 0, [bddfFillIntegerPart, bddfNoFractionalPart]);
    result[len-i+1] := chr(ord(one) + BigDecimalToLongint(remainder));
    number := quotient;
  end;
end;

function xqFunctionFormat_Integer(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;


var
  primaryFormat, modifierVariant: String;
  separator: LongInt;
  modifiers: TXQFormatIntegerModifiers;
  i: Integer;
  number: BigDecimal;
  formatted: String;
  j: LongInt;
  signed: Boolean;
  lang: String;
  shortLang: String;
  procedure raiseInvalidPicture();
  begin
    raise EXQEvaluationException.create('FODF1310', 'Invalid picture string: ', nil, args[1]);
  end;

begin
  requiredArgCount(args, 2, 3);
  //preprocessing picture string
  primaryFormat := args[1].toString;
  separator := strLastIndexOf(primaryFormat, ';');
  modifiers := [];
  modifierVariant := '';
  if separator > 0 then begin
    i := separator + 1;
    if (i <= length(primaryFormat)) and (primaryFormat[i] in ['c', 'o']) then begin
      if primaryFormat[i] = 'o' then include(modifiers, xqfimOrdinal);
      inc(i);
      if (i <= length(primaryFormat)) and (primaryFormat[i] = '(') then begin
        j := strLastIndexOf(primaryFormat, ')');
        if j <= 0 then raiseInvalidPicture;
        modifierVariant := copy(primaryFormat, i + 1, j - i - 1);
        i := j + 1;
      end;
    end;
    if (i <= length(primaryFormat)) then
      case primaryFormat[i] of
        'a': inc(i);
        't': begin
          include(modifiers, xqfimTraditional);
          inc(i);
        end;
      end;
    if (i <= length(primaryFormat)) then raiseInvalidPicture();
    delete(primaryFormat, separator, length(primaryFormat) - separator + 1);
  end;

  if high(args) < 2 then lang := ''
  else lang := args[2].toString;
  shortLang := lang;
  if strContains(shortLang, '-') then shortLang := LowerCase(strBefore(shortLang, '-'));


  //default conversions

  number := args[0].toDecimal;
  signed := number.signed;
  number.signed := false;
  formatted := '';
  if primaryFormat <> '' then
    case primaryFormat of
      'A', 'a': if not isZero(number) then formatted := alphabetify(number, primaryFormat[1]);
      'i', 'I': begin
        if isLongint(number) then formatted := IntToRoman(BigDecimalToLongint(number));
        if primaryFormat = 'i' then formatted := LowerCase(formatted);
      end;
      'w','W','Ww': begin
        case shortLang of
          'en','': formatted := englishfy(BigDecimalToStr(number));
        end;
        if formatted <> '' then
          case primaryFormat of
            'W': formatted := UpperCase(formatted);
            'Ww': begin
              formatted[1] := upCase(formatted[1]);
              for i := 1 to length(formatted) - 1 do
                if formatted[i] in [' ', '-'] then formatted[i+1]:=upCase(formatted[i+1]);
            end;
          end;
      end;
    end;
  if formatted = '' then formatted := BigDecimalToStr(number);
  if Signed and not isZero(number) then formatted := '-' + formatted ;
  result := xqvalue(formatted);
end;

