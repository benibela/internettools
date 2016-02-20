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

//returns 1000^(i+1) as English numeral using the Conway-Wechsler system
function strConwayWechsler(i: integer): string;
const cache: array[0..18] of string = ('n', 'm', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non', 'dec', 'undec', 'duodec', 'tredec', 'quattuordec', 'quindec', 'sedec', 'septendec', 'octodec');      //illion
var units: array[0..9] of string = ('', 'un', 'duo', 'tre', 'quattor', 'quinqua', 'se', 'septe', 'octo', 'nove');
    tens: array[0..9] of string = ('', 'deci', 'viginti', 'triginta', 'quadraginta', 'quinquaginta', 'sexaginta', 'septuaginta', 'octoginta', 'nonaginta');
    hundreds: array[0..9] of string = ('', 'centi', 'ducenti', 'trecenti', 'quadringenti', 'quingenti', 'sescenti', 'septigenti', 'octingenti', 'nongenti');
    ones: Integer;
begin
  if i <= high(cache) then begin
    result := cache[i] + 'illion'; //this returns nillion 1000^0
  end else if i >= 1000 then begin
    result := strConwayWechsler(i div 1000);
    delete(result, length(result) - 1, 2); //illion => illi
    result += strConwayWechsler(i mod 1000);
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
            'o', 'c':
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


function englishfy(const n: string; modifiers: TXQFormatIntegerModifiers): string;
  const digits: array['1'..'9'] of string = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');
        prefix: array['1'..'9'] of string = ('ten', 'twen', 'thir', 'for', 'fif', 'six', 'seven', 'eigh', 'nine');
        digitsOrdinals: array['1'..'9'] of string = ('first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth');
  function englishfybelow100(lowest: integer): string;
    function ordinalCase: boolean; inline;
    begin
      result := (length(n) = lowest) and (xqfimOrdinal in modifiers);
    end;

  begin
    result := '';
    if lowest <= 0 then exit;
    if (lowest >= 2) then
      case n[lowest - 1] of
        '0': ;
        '1': begin
          case n[lowest] of
            '0': result := 'ten';
            '1': result := 'eleven';
            '2': if not ordinalCase then result := 'twelve' else exit('twelfth');
            '4': result += 'fourteen'
            else result += prefix[n[lowest]] + 'teen';
          end;
          if ordinalCase then result += 'th';
          exit;
        end;
        else begin
          result := prefix[n[lowest-1]];
          if n[lowest] > '0' then result += 'ty-'
          else if not ordinalCase then result += 'ty'
          else result += 'tieth'
        end;
      end;
    if (n[lowest] <> '0') then
      if not ordinalCase then result += digits[n[lowest]]
      else result += digitsOrdinals[n[lowest]];
  end;

  function englishfybelow1000(lowest: integer): string;
  var
    hundreds: String;
  begin
    result := englishfybelow100(lowest);
    if (lowest >= 3) and (n[lowest-2] > '0') then begin
      hundreds := digits[n[lowest-2]] + ' hundred';
      if result = '' then exit(hundreds);
      if xqfimTraditional in modifiers then result := hundreds + ' and ' + result
      else result := hundreds + ' ' + result;
    end;
  end;


  procedure addWithScale(number, scale: string);
  begin
    if number = '' then exit;
    if scale <> '' then scale := ' ' + scale;
    if result <> '' then result := result + ' ' + number + scale
    else result := number + scale;
  end;

var
  temp: String;
  blockStart: Integer;
  i: Integer;
  temp2: String;
begin
  result := '';
  if xqfimTraditional in modifiers then begin
    //let's go crazy
    if length(n) <= 4 then begin
      i := StrToInt(n);
      case i of
        0: result := 'goose egg';
        12: result := 'dozen';
        100: if not (xqfimOrdinal in modifiers) then result := 'teenty';
        144: result := 'gross';
        1000: result := 'grand';
        1728: result := 'great gross';
      end;
    end;
    if strBeginsWith(n, '10') and (strIndexOf(n, ['1'..'9'], 2) = 0) then begin
      case length(n) of
        1+4: result := 'myriad';
        1+100: result := 'googol';
      end;
    end;
    if result <> '' then begin
      if xqfimOrdinal in modifiers then result += 'th';
      exit;
    end;
  end;
  if (n = '0') or (n = '') then
    if xqfimOrdinal in modifiers then exit('zeroth')
    else exit('zero');
  blockStart := (length(n)-1) div 3;
  for i := blockStart downto 2 do begin
    temp := englishfybelow1000(length(n)-3*i);
    if temp <> '' then addWithScale(temp, strConwayWechsler(i-1));
  end;
  if not (xqfimTraditional in modifiers)
     or ((length(n) >= 5) and (n[length(n)-4] <> '0'))
     or ((length(n) >= 6) and (n[length(n)-5] <> '0')) then begin
       addWithScale(englishfybelow1000(length(n)-3), 'thousand');
       addWithScale(englishfybelow1000(length(n)), '');
  end else begin
    if length(n) > 2 then  temp := englishfybelow100(length(n)-2) else temp := '';
    temp2 := englishfybelow100(length(n));
    if (temp <> '') and (temp2 <> '') then begin
      addWithScale(temp, 'hundred');
      result += ' and';
    end else addWithScale(temp, 'hundred');
    addWithScale(temp2, '');
  end;
  if (xqfimOrdinal in modifiers) then
    case copy(n, length(n)-1,2) of
      '00', '0': result += 'th';
    end;
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

function charUnicodeZero(const cp: integer): integer;
const UNICODE_ZEROS: array[1..55] of integer = ($0030,$0660,$06F0,$07C0,$0966,$09E6,$0A66,$0AE6,$0B66,$0BE6,$0C66,$0CE6,$0D66,$0DE6,$0E50,$0ED0,$0F20,$1040,$1090,$17E0,$1810,$1946,$19D0,$1A80,$1A90,$1B50,$1BB0,$1C40,$1C50,$A620,$A8D0,$A900,$A9D0,$A9F0,$AA50,$ABF0,$FF10,$104A0,$11066,$110F0,$11136,$111D0,$112F0,$114D0,$11650,$116C0,$11730,$118E0,$16A60,$16B50,$1D7CE,$1D7D8,$1D7E2,$1D7EC,$1D7F6);
var
  i: Integer;
begin
  for i := low(UNICODE_ZEROS) to high(UNICODE_ZEROS) do
    if (cp >= UNICODE_ZEROS[i]) and (cp < UNICODE_ZEROS[i] + 10) then
      exit(UNICODE_ZEROS[i]);
  result := -11;
end;
function strBeginsWithUnicodeNumber(const picture: string): boolean;
var
  temp: Integer;
begin
  temp := 1;
  result := charUnicodeZero(strDecodeUTF8Character(picture, temp)) > 0;
end;

type TStrIterator = record
  FCurrent: integer;

  s: RawByteString;
  pos: integer;
  property Current: integer read FCurrent;
  function MoveNext: Boolean;
  function GetEnumerator: TStrIterator;
end;
function TStrIterator.MoveNext: Boolean;
begin
  result := pos <= length(s);
  fcurrent := strDecodeUTF8Character(s, pos);
end;

function TStrIterator.GetEnumerator: TStrIterator;
begin
  result := self;
end;

function strIterator(const s: RawByteString): TStrIterator;
begin
  result.s := s;
  result.pos := 1;
end;


function xqFunctionFormat_Integer(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;


var shortLang: String;
  number: BigDecimal;
  modifiers: TXQFormatIntegerModifiers;
  primaryFormat, modifierVariant: String;
  needOrdinalization: Boolean;

  function ordinalize(n: string): string;
  var
    suffix: String;
  begin
    result := n;
    if not needOrdinalization then exit;
    case shortLang of
      'en': begin
        suffix := 'th';
        if getDigit(number, 1) <> 1 then
          case getDigit(number, 0) of
            1: suffix := 'st';
            2: suffix := 'nd';
            3: suffix := 'rd';
          end;
        result += suffix;
      end;
    end;
  end;

  function formatUnicodeInteger(family: integer): string;
  var
    cp: Integer;
    optional: Integer;
    mandatory: Integer;
    validGroupingSeparator: TWrappedRegExpr;
    separators: array of record
      pos: integer;
      sep: string;
    end;
    sepcount: Integer;
    arabic: String;
    totalDigits: Integer;
    delta: Integer;
    i: Integer;

  begin
    SetLength(separators, 10);
    sepcount := 0;
    optional := 0;
    mandatory := 0;
    validGroupingSeparator := wregexprParse('^[^\p{Nd}\p{Nl}\p{No}\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}]$',[]);
    for cp in strIterator(primaryFormat) do begin
      if cp = ord('#') then begin
        if mandatory > 0 then raise EXQEvaluationException.create('FODF1310', 'Invalid optional in ' + primaryFormat);
        inc(optional)
      end else if (cp >= family) and (cp < family + 10) then inc(mandatory)
      else if (optional + mandatory > 0) and wregexprMatches(validGroupingSeparator, strGetUnicodeCharacter(cp)) then begin
        if sepcount >= length(separators) then SetLength(separators, sepcount * 2);
        separators[sepcount].sep := strGetUnicodeCharacter(cp);
        separators[sepcount].pos := optional + mandatory;
        inc(sepcount)
      end else begin
        wregexprFree(validGroupingSeparator);
        raise EXQEvaluationException.create('FODF1310', 'Invalid grouping separator in ' + primaryFormat);
      end
    end;
    wregexprFree(validGroupingSeparator);
    if mandatory = 0 then raise EXQEvaluationException.create('FODF1310', 'No digit in ' + primaryFormat);

    delta := -1;
    if sepcount > 0 then begin
      for i := 0 to sepcount - 1 do separators[i].pos := optional + mandatory - separators[i].pos;
        if separators[sepcount - 1].pos = 0 then
          raise EXQEvaluationException.create('FODF1310', 'Trailing grouping separator in ' + primaryFormat);
      delta := separators[sepcount-1].pos;
      for i := sepcount - 2 downto 0 do begin
        if (separators[i].pos - separators[i+1].pos <> delta) or (separators[i+1].sep <> separators[i].sep) then delta := -1;
        if separators[i+1].pos - separators[i].pos = 0 then raise EXQEvaluationException.create('FODF1310', 'Adjacent grouping separator in ' + primaryFormat);
      end;
    end;

    arabic := BigDecimalToStr(number);
    if length(arabic) < mandatory then arabic := strDup('0', mandatory - length(arabic) ) + arabic;
    if (family = ord('0')) and (sepcount = 0) then exit(arabic);

    result := '';
    totalDigits := 0;
    for i := length(arabic) downto 1 do begin
      result := strGetUnicodeCharacter(family + ord(arabic[i]) - ord('0')) + result;
      inc(totalDigits);
      if (sepcount > 0) and (separators[sepcount-1].pos <= totalDigits) then begin
        result := separators[sepcount-1].sep + result;
        if Delta > 0 then separators[sepcount-1].pos += delta
        else dec(sepcount);
      end;
    end;
  end;

var
  separator: LongInt;
  i: Integer;
  formatted: String;
  j: LongInt;
  signed: Boolean;
  lang: String;
  procedure raiseInvalidPicture();
  begin
    raise EXQEvaluationException.create('FODF1310', 'Invalid picture string: ', nil, args[1]);
  end;

begin
  requiredArgCount(args, 2, 3);
  if args[0].isUndefined then exit(xqvalue(''));
  //preprocessing picture string
  primaryFormat := args[1].toString;
  separator := strLastIndexOf(primaryFormat, ';');
  modifiers := [];
  modifierVariant := '';
  needOrdinalization := false;
  if separator > 0 then begin
    i := separator + 1;
    if (i <= length(primaryFormat)) and (primaryFormat[i] in ['c', 'o']) then begin
      if primaryFormat[i] = 'o' then begin
        include(modifiers, xqfimOrdinal);
        needOrdinalization := true;
      end;
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
  shortLang := 'en';


  //default conversions

  number := args[0].toDecimal;
  signed := number.signed;
  number.signed := false;
  formatted := '';
  case primaryFormat of
    '': raise EXQEvaluationException.create('FODF1310', 'Invalid picture string');
    'A', 'a': if not isZero(number) then begin
      formatted := alphabetify(number, primaryFormat[1]);
      if xqfimOrdinal in modifiers then formatted += '-';
    end;
    'i', 'I': begin
      if isLongint(number) then formatted := IntToRoman(BigDecimalToLongint(number));
      if primaryFormat = 'i' then formatted := LowerCase(formatted);
    end;
    'w','W','Ww': begin
      case shortLang of
        'en': formatted := englishfy(BigDecimalToStr(number), modifiers);
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
      needOrdinalization := false;
    end;
  end;
  if (formatted = '') then begin
    for i in strIterator(primaryFormat) do begin
      j := charUnicodeZero(i);
      if j > 0 then begin
        formatted := formatUnicodeInteger(j);
        break;
      end;
    end;
  end;
  if formatted <> '' then formatted := ordinalize(formatted);
  if formatted = '' then formatted := ordinalize(BigDecimalToStr(number));
  if Signed and not isZero(number) then formatted := '-' + formatted ;
  result := xqvalue(formatted);
end;

