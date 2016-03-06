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

//returns 1000^(i+1) as English numeral using the Conway-Wechsler system. The result always ends with 'illion'
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


function germanfy(const n: string; modifiers: TXQFormatIntegerModifiers): string;
  const digits: array['1'..'9'] of string = ('ein', 'zwei', 'drei', 'vier', 'fünf', 'sechs', 'sieben', 'acht', 'neun');
        prefix: array['1'..'9'] of string = ('zehn', 'zwanz', 'dreiß', 'vierz', 'fünfz', 'sechz', 'siebz', 'achtz', 'neunz');
        digitsOrdinals: array['1'..'9'] of string = ('erste', 'zweite', 'dritte', 'vierte', 'fünfte', 'sechste', 'siebte', 'achte', 'neunte');
  function germanfybelow100(lowest: integer): string;
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
            '0': result := 'zehn';
            '1': result := 'elf';
            '2': result := 'zwölf';
            '6': result := 'sechzehn';
            '7': result := 'siebzehn';
            else result += digits[n[lowest]] + 'zehn';
          end;
          if ordinalCase then result += 'te';
          exit;
        end;
        else begin
          result := prefix[n[lowest-1]] + 'ig';
          if ordinalCase then result += 'ste';
        end;
      end;
    if (n[lowest] <> '0') then begin
      if result = '' then begin
        if not ordinalCase then result += digits[n[lowest]]
        else result += digitsOrdinals[n[lowest]]
      end else result := digits[n[lowest]] + 'und' + result;
    end;
  end;

  function germanfybelow1000(lowest: integer): string;
  var
    hundreds: String;
  begin
    result := germanfybelow100(lowest);
    if (lowest >= 3) and (n[lowest-2] > '0') then
      result := digits[n[lowest-2]] + 'hundert' + result;
  end;

  function finalEins(s: string): string;
  begin
    result := s;
    if strEndsWith(s, 'ein') then result += 's';
  end;

  procedure addArdeIllion(numberilliarde, numberillion, scale: string);
  var
    i: Integer;
    illiarde: String;
  begin
    if (numberilliarde = '') and (numberillion = '') then exit;
    //germanfy english scale
    for i := 1 to length(scale) do
      if scale[i] = 'c' then
        if (i = 1) or (scale[i-1] <> 'o') then scale[i] := 'z'
        else scale[i] := 'k';

    scale[1] := upcase(scale[1]);
    if numberilliarde <> '' then begin
      if result <> '' then result += ' ';
      illiarde := copy(scale, 1, length(scale) - 6 {length(illion)} ) + 'illiarde';
      if numberilliarde = 'ein' then result += 'eine ' + illiarde
      else result += finalEins(numberilliarde) + ' ' +illiarde + 'n';
    end;

    if numberillion <> '' then begin
      if result <> '' then result += ' ';
      if numberillion = 'ein' then result += 'eine ' + scale
      else result += finalEins(numberillion) + ' '+ scale + 'en';
    end;
  end;

var
  temp: String;
  blockStart: Integer;
  i: Integer;
  temp2: String;
  illion: String;
  illiarde: String;
begin
  result := '';
  //if xqfimTraditional in modifiers then begin    end;
  if (n = '0') or (n = '') then
    if xqfimOrdinal in modifiers then exit('nullte')
    else exit('null');
  blockStart := (length(n)-1) div 6;
  for i := blockStart downto 1 do begin
    illiarde := germanfybelow1000(length(n)-6*i - 3);
    illion   := germanfybelow1000(length(n)-6*i);
    if (illiarde <> '') or (illion <> '') then addArdeIllion(illiarde, illion, strConwayWechsler(i));
  end;
  //0te illion
  illiarde := germanfybelow1000(length(n) - 3);
  illion := germanfybelow1000(length(n));
  if (illiarde <> '') or (illion <> '') then begin
    if result <> '' then result += ' ';
    if illiarde <> '' then result += illiarde + 'tausend';
    result += finalEins(illion);
  end;
  if (xqfimOrdinal in modifiers) then
    case copy(n, length(n)-1,2) of
      '00', '0': begin
        if result[length(result)] = 'e' then delete(result, length(result), 1)
        else if strEndsWith(result, 'illionen') or strEndsWith(result, 'illiarden') then delete(result, length(result) - 1, 2);
        result += 'ste';
      end;
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

function formatUnicodeInteger(arabic, primaryFormat: string; family: integer): string;
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
    totalDigits: Integer;
    delta: Integer;
    i: Integer;

  begin
    SetLength(separators, 10);
    sepcount := 0;
    optional := 0;
    mandatory := 0;
    validGroupingSeparator := nil;
    for cp in strIterator(primaryFormat) do begin
      if cp = ord('#') then begin
        if mandatory > 0 then raise EXQEvaluationException.create('FODF1310', 'Invalid optional in ' + primaryFormat);
        inc(optional)
      end else if (cp >= family) and (cp < family + 10) then inc(mandatory)
      else begin
        if validGroupingSeparator = nil then validGroupingSeparator := wregexprParse('^[^\p{Nd}\p{Nl}\p{No}\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}]$',[]);
        if (optional + mandatory > 0) and wregexprMatches(validGroupingSeparator, strGetUnicodeCharacter(cp)) then begin
          if sepcount >= length(separators) then SetLength(separators, sepcount * 2);
          separators[sepcount].sep := strGetUnicodeCharacter(cp);
          separators[sepcount].pos := optional + mandatory;
          inc(sepcount)
        end else begin
          wregexprFree(validGroupingSeparator);
          raise EXQEvaluationException.create('FODF1310', 'Invalid grouping separator in "' + primaryFormat + '": x' + IntToStr(cp) );
        end
      end;
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

    if length(arabic) < mandatory then arabic := strDup('0', mandatory - length(arabic) ) + arabic;
    if (family = ord('0')) and (sepcount = 0) then exit(arabic);

    result := '';
    totalDigits := 0;
    for i := length(arabic) downto 1 do begin
      if (sepcount > 0) and (separators[sepcount-1].pos <= totalDigits) then begin
        result := separators[sepcount-1].sep + result;
        if Delta > 0 then separators[sepcount-1].pos += delta
        else dec(sepcount);
      end;
      result := strGetUnicodeCharacter(family + ord(arabic[i]) - ord('0')) + result;
      inc(totalDigits);
    end;
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
      'de': result += '.';
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
  case shortLang of
    'de': ;
    else shortLang := 'en';
  end;


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
      formatted := BigDecimalToStr(number);
      case shortLang of
        'en': formatted := englishfy(formatted, modifiers);
        'de': begin
          formatted := germanfy(formatted, modifiers);
          if needOrdinalization and (modifierVariant <> '') and (strBeginsWith(modifierVariant, '-e')) then formatted += strCopyFrom(modifierVariant, 3);
        end;
      end;
      if formatted <> '' then
        case primaryFormat of
          'W': begin
            formatted := UpperCase(formatted);
            case shortLang of
              'de': formatted := StringReplace(StringReplace(formatted, 'ö', 'Ö', [rfReplaceAll]), 'ü', 'Ü', [rfReplaceAll]);
            end;
          end;
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
        formatted := formatUnicodeInteger(BigDecimalToStr(number), primaryFormat, j);
        break;
      end;
    end;
  end;
  if formatted <> '' then formatted := ordinalize(formatted);
  if formatted = '' then formatted := ordinalize(BigDecimalToStr(number));
  if Signed and not isZero(number) then formatted := '-' + formatted ;
  result := xqvalue(formatted);
end;


function dateWeekOfMonth(y,m,d: integer): integer;
var
  week: Word;
  firstweek: Word;
begin
  week := dateWeekOfYear( y, m, d );
  firstweek :=dateWeekOfYear( y, m, 4 );
  if (week >= firstweek) and (week < firstweek + 10) then exit(week - firstweek + 1);
  dec(m);
  if m <=0 then begin exit(dateWeekOfMonth(y-1,12,31)); end;
  firstweek :=dateWeekOfYear( y, m, 4 );
  result := week - firstweek + 1;
end;

function splitEQName(staticContext: TXQStaticContext; const eqname: string; out namespaceURL, localpart: string): boolean;
begin
  namespaceURL := '';
  localpart := xmlStrWhitespaceCollapse(eqname);
  result := true;
  if strBeginsWith(localpart, 'Q{') then begin //EQName!
    namespaceURL := strSplitGet('}', localpart);
    delete(namespaceURL, 1, 2); //Q{ no more
  end else if pos(':', localpart) > 0 then begin
    staticContext.splitRawQName(namespaceURL, localpart, xqdnkUnknown);
    result := namespaceURL <> '';
  end;
end;


function xqFunctionFormat_DateTimeC(const context: TXQEvaluationContext; const args: TXQVArray; allowDate, allowTime: boolean): IXQValue;

const monthNamesEnglish: array[1..12] of string = ('January', 'February', 'March','April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
      monthNamesGerman: array[1..12] of string = ('Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember');
      weekDayNamesEnglish: array[1..7] of string = ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday');
      weekDayNamesGerman: array[1..7] of string = ('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag', 'Sonntag');


var picture: string;
  procedure raiseInvalidPictureFOFD1340;
  begin
    raise EXQEvaluationException.create('FOFD1340', 'Invalid picture string: ' + picture);
  end;

  function parseWidth(const width: string; const def: integer): integer;
  begin
    if width = '*' then exit(def);
    result := StrToIntDef(width, -1);
    if result <= 0 then raiseInvalidPictureFOFD1340;
  end;

  function countDigits(const s: string; zerodp: PInteger = nil; allDigits: PBoolean = nil): integer;
  var
    j: Integer;
    zerocp: Integer;
  begin
    j := 1;
    result := 0;
    if assigned(allDigits) then allDigits^ := true;
    while j <= length(s) do begin
      case s[j] of
        '0'..'9': begin
          inc(result);
          inc(j);
          if assigned(zerodp) then zerodp^ := ord('0');
        end;
        '!'..'/', ':'..'z': begin
          inc(j); //non-digit ascii
          if assigned(allDigits) then allDigits^ := false;
        end
        else begin
          zerocp := charUnicodeZero(strDecodeUTF8Character(s, j));
          if zerocp > 0 then begin
            inc(result);
            if assigned(zerodp) then zerodp^ := zerocp;
          end else if assigned(allDigits) then allDigits^ := false;
        end;
      end;
    end;
  end;

var
  lang: String;
  calendar: String;
  place: String;

  pictured: array of record
    component: char;
    modifier: char;
    format: string;
    minwidth, maxwidth: integer;
  end;
  picturedlength: Integer;
  commapos: SizeInt;
  tempstrmin: RawByteString;
  tempstr: RawByteString;
  tempstrmax: RawByteString;
  last: Integer;
  i: Integer;
  calendarNamespace: string;
  dateTime: PXQValueDateTimeData;
  number: Integer;
  component, format: String;
  sublang: String;
  formatted: String;
  missingCharacterCount: Integer;
  j: Integer;
  zerocp: Integer;
  allDigits: Boolean;
  tz: Integer;
  fallbackOccured: String;


begin
  calendarNamespace := '';
  fallbackOccured := '';
  if length(args) = 5 then begin
    lang := args[2].toString;
    calendar := args[3].toString;
    if not splitEQName(context.staticContext, args[3].toString, calendarNamespace, calendar) then raiseInvalidPictureFOFD1340;
    if calendarNamespace = '' then
      case calendar of
        '':;
        'AD', 'ISO': ;
        'AH', 'AME', 'AM', 'AP', 'AS', 'BE', 'CB', 'CE', 'CL', 'CS', 'EE', 'FE', 'JE', 'KE', 'KY', 'ME', 'MS', 'NS', 'OS', 'RS', 'SE', 'SH', 'SS', 'TE', 'VE', 'VS': begin
          fallbackOccured += '[Calendar: AD]';
        end
        else raise EXQEvaluationException.create('FOFD1340', 'Invalid calendar: '+calendar);
      end;

    place := args[4].toString;
  end else if length(args) = 2 then begin
    lang := '';
    calendar := '';
    place := '';
  end else raise EXQEvaluationException.Create('XPST0017','Need 2 or 5 arguments passed');

  sublang := lang;
  if strContains(lang, '-') then sublang := strBefore(sublang, '-');
  case sublang of
    'de', 'en':;
    '': sublang := 'en';
    else begin
      sublang := 'en';
      fallbackOccured += '[Language: en]';
    end;
  end;

  if args[0].isUndefined then exit(xqvalue);

  picture := args[1].toString;
  //writeln('>>',picture);
  SetLength(pictured, 32);
  picturedlength := 0;
  last := 1;
  i := 1;
  while i <= length(picture) do begin
    case picture[i] of
      '[', ']': begin
        if picturedlength = length(pictured) then SetLength(pictured, 2 * length(pictured));
        pictured[picturedlength].component:='"';
        pictured[picturedlength].format := copy(picture, last, i - last);
        inc(picturedlength);
        last := i;

        if i + 1 > length(picture) then raise EXQEvaluationException.create('FOFD1340', 'Invalid datetime picture: '+picture);
        if picture[i+1] = picture[i] then begin
          pictured[picturedlength-1].format += picture[i];
          inc(i, 2);
          last := i;
        end else begin
          inc(i);
          while (i <= length(picture)) and (picture[i] in WHITE_SPACE) do inc(i);
          pictured[picturedlength].component:=picture[i];
          case pictured[picturedlength].component of
            'Y','M','D','d','F','W','w', 'E': if not allowDate then
              raise EXQEvaluationException.create('FOFD1350', 'Invalid component in '+picture);
            'H','h','P','m','s','f': if not allowTime then
              raise EXQEvaluationException.create('FOFD1350', 'Invalid component in '+picture);
            'Z', 'z': ;
            else raiseInvalidPictureFOFD1340;
          end;
          last := i + 1;
          i := strIndexOf(picture, ']', i);
          pictured[picturedlength].format := copy(picture, last, i - last);
          pictured[picturedlength].format := StringsReplace(pictured[picturedlength].format, [#9,#$A,#$D,' '], ['','','',''],[rfReplaceAll]);
          pictured[picturedlength].minwidth := 0;
          pictured[picturedlength].maxwidth := 999;
          commapos := strLastIndexOf(pictured[picturedlength].format, ',');
          if commapos > 0 then begin
            tempstrmax := strCopyFrom(pictured[picturedlength].format, commapos + 1);
            delete(pictured[picturedlength].format, commapos, length(pictured[picturedlength].format));
            if not strContains(tempstrmax, '-') then begin
              tempstrmin := tempstrmax;
              tempstrmax := '*'
            end else tempstrmin := strSplitGet('-', tempstrmax );
            pictured[picturedlength].minwidth := parseWidth(tempstrmin,0);
            pictured[picturedlength].maxwidth := parseWidth(tempstrmax,pictured[picturedlength].minwidth + 99);
          end else if pictured[picturedlength].format <> '' then begin
            with pictured[picturedlength] do begin
              minwidth := countDigits(format);
              if minwidth > 0 then maxwidth:=minwidth;
            end;
          end;
          if length(pictured[picturedlength].format) > 1 then begin
            if pictured[picturedlength].format[length(pictured[picturedlength].format)] in ['c','o','a','t'] then begin
              pictured[picturedlength].modifier := pictured[picturedlength].format[length(pictured[picturedlength].format)];
              delete(pictured[picturedlength].format, length(pictured[picturedlength].format), 1);
            end;
          end;
          last := i + 1;
          i := last;
          inc(picturedlength);
        end;
      end;
      else inc(i);
    end;
  end;
  if last <> i then begin
    pictured[picturedlength].component:='"';
    pictured[picturedlength].format := copy(picture, last, i - last);
    inc(picturedlength);
  end;

  {for i := 0 to picturedlength - 1 do
    with pictured[i] do
      writeln(component,': ',format, ' ',minwidth,'-',maxwidth);}

  dateTime := @((args[0] as TXQValueDateTime).value);

  try
    formatted := '';
    for i := 0 to picturedlength - 1 do begin
      component := '';
      format := pictured[i].format;
      case pictured[i].component of
        '"': begin
          formatted += pictured[i].format;
          continue;
        end;
        'Y': begin
          number := dateTime^.year;
          if pictured[i].maxwidth = 2 then number := number mod 100;
          if number < 0 then for j := 0 to picturedlength - 1 do
            if pictured[j].component = 'E' then number := abs(number);
        end;
        'M': number := dateTime^.month;
        'D': number := dateTime^.day;
        'd': number := DateMonthDaysCumSum[IsLeapYear(dateTime^.year), dateTime^.month - 1] + dateTime^.day;
        'F': begin
          number := DayOfWeek( dateEncode(dateTime^.year, dateTime^.month, dateTime^.day ) - 1 ); //-1 there seems to be an offset between pascal and iso
          if format = '' then format := 'n';
        end;
        'W': number := dateWeekOfYear( dateTime^.year, dateTime^.month, dateTime^.day ) ;
        'w': number := dateWeekOfMonth( dateTime^.year, dateTime^.month, dateTime^.day ) ;
        'H': number := dateTime^.hour;
        'h': begin
          number := dateTime^.hour;
          if number > 12 then number -= 12
          else if number = 0 then number := 12;
        end;
        'P': begin
          if dateTime^.hour >= 12 then component := 'pm'
          else component := 'am';
          format := #0;
        end;
        'm': begin
          number := dateTime^.min;
          if format = '' then format := '01';
        end;
        's': begin
          number := dateTime^.seconds;
          if format = '' then format := '01';
        end;
        'f': begin
          if pictured[i].maxwidth < 6 then
             number := (dateTime^.microsecs + powersOf10[6 - pictured[i].maxwidth] div 2) div powersOf10[6 - pictured[i].maxwidth]
           else number := dateTime^.microsecs ;
          if pictured[i].minwidth < 7 then
            while ((number mod 10) = 0 ) and (number > powersOf10[pictured[i].minwidth]) do number := number div 10;
         end;
        'Z', 'z': begin
          if format = 'N' then format := '01:01';
          if format = '' then format := '01:01';
          //writeln(picture, '  z: ', format);
          tz := dateTime^.timezone;
          if dateTime^.timezone = high(Integer) then begin
            if (format = 'Z') then component := 'J'
            else component := '';
            format := #0;
          end else if (format = 'Z') and (tz mod 60 = 0) and (tz >= -12*60) and (tz <= 12*60) then begin
            if tz = 0 then component := 'Z'
            else begin
              tz := tz div 60;
              if tz < 0 then component := chr(ord('M') - tz)
              else if tz < 10 then component := chr(ord('A') + tz - 1)
              else component := chr(ord('A') + tz); //military time goes H I K L M  i.e. J is skipped
            end;
            format := #0;
          end else begin
            if format = 'Z' then format := '00:00';
            tz := abs( (tz div 60) * 100 + (tz mod 60) );
            j := countDigits(format, @zerocp, @allDigits);
            if j > 0 then
              if (tz = 0) and (pictured[i].modifier = 't') then begin
                component := 'Z';
                format := #0;
              end else if allDigits and (j <= 2) then begin
                if tz mod 100 = 0 then component := formatUnicodeInteger(inttostr(tz div 100), format, zerocp)
                else component := formatUnicodeInteger(inttostr(tz), format + ':'+strdup(strGetUnicodeCharacter(zerocp),2), zerocp);
              end else if not allDigits then  component := formatUnicodeInteger(inttostr(tz), format, zerocp)
              else if j <= 4 then component := formatUnicodeInteger(inttostr(tz), format, zerocp);
            if (component <> '') and (component <> 'Z') then begin
              if dateTime^.timezone < 0 then component := '-' + component
              else component := '+' + component;
              if pictured[i].component = 'z' then component := 'GMT'+component;
              format := #0;
            end else number := tz;
          end;
        end;
        //if format = '' then format := '01';;
        'C': if format = '' then format := 'n';
        'E': if format = '' then format := 'n';
      end;
      if format <> #0 then begin //not handled above
        component := '';
        case format of
          '', '1': if pictured[i].modifier = #0 then
            component := IntToStr(number);
          '01': if pictured[i].modifier = #0 then begin
            component := IntToStr(number);
            if (length(component) = 1) and (pictured[i].maxwidth > 1) then component := '0' + component;
          end;
          'N','n','Nn': begin
            case pictured[i].component of
              'M': case sublang of
                'en': component := monthNamesEnglish[number];
                'de': component := monthNamesGerman[number];
              end;
              'F': case sublang of
                'en': component := weekDayNamesEnglish[number];
                'de': component := weekDayNamesGerman[number];
              end;
              'C': component := calendar;
              'E': if dateTime^.year >= 0 then begin
                case sublang of
                  'en': component := 'CE';
                  'de': component := 'n. d. Z.';
                end;
              end else
                case sublang of
                  'en': component := 'BCE';
                  'de': component := 'v. d. Z.';
                end;
            end;
            case format of
              'N': component := UpperCase(StringReplace(component, 'ä', 'Ä',[]));
              'n': component[1] := lowerCase(component[1]);
            end;
            if (length(component) > pictured[i].maxwidth) and (pictured[i].component in ['M','F']) then begin
              j := strLengthUtf8(component) - pictured[i].maxwidth;
              if j > 0 then begin
                case sublang of
                  'en', 'de': if (pictured[i].maxwidth > 3) and (pictured[i].minwidth = 3) then
                    j := strLengthUtf8(component) - pictured[i].minwidth;
                end;
                delete(component, length(component) - j + 1, j);
              end;
            end;
          end;
        end;
        if component = '' then begin
          if format = '' then format := '0';
          if pictured[i].modifier <> #0 then format += ';' + pictured[i].modifier else format += ';';
          component := xqFunctionFormat_Integer(context, xqvalueArray([xqvalue(number), xqvalue(format), xqvalue(lang)])).toString;
        end;
      end;
      missingCharacterCount := pictured[i].minwidth - strLengthUtf8(component);
      if missingCharacterCount > 0 then begin
        j := 1;
        zerocp := charUnicodeZero(strDecodeUTF8Character(component, j));
        if (zerocp > 0) and (pictured[i].component <> 'f') then
          component := strDup(strGetUnicodeCharacter(zerocp), missingCharacterCount) + component
        else begin
          if zerocp <= 0 then zerocp := ord(' ');
          component := component + strDup(strGetUnicodeCharacter(zerocp), missingCharacterCount);
        end;
      end;
      formatted += component;
    end;
    formatted := fallbackOccured + formatted;
  except
    on e:EXQEvaluationException do
      if e.errorCode = 'FODF1310' then raiseInvalidPictureFOFD1340
      else raise;
  end;
  result := xqvalue(formatted);
end;

function xqFunctionFormat_DateTime(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, args, true, true);
end;
function xqFunctionFormat_Date(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, args, true, false);
end;
function xqFunctionFormat_Time(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, args, false, true);
end;

type TXQSubPosition = (spInPrefix, spInInteger, spInFraction, spExponentItself, spInExponent, spInSuffix);

function xqFunctionFormat_Number(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  data: ^TXQDecimalFormatPropertyData;
  picture: String;
  c: integer;
  numberf: xqfloat;
  number: BigDecimal;
  currentPictureParser: integer;
  pictureParser: array[0..1] of record
    foundChar: array[TXQDecimalFormatProperty] of boolean;
    subPosition: TXQSubPosition;
    integerMandatory: Integer;
    integerOptional: Integer;
    fractionMandatory: Integer;
    fractionOptional: Integer;
    integerGroups: TLongintArray; //integer and fraction groups are stored reversed compared to each other
    fractionGroups: TLongintArray;
    exponentMandatory: Integer;
    exponentOptional: Integer;
    activeChar: Boolean;
    prefix, suffix: string;
  end;
  arabic: String;
  dot: LongInt;
  integerActual: Integer;
  fractionActual: Integer;
  resstr: String;
  i: Integer;
  integerGroupDelta, groupPos: Integer;
  j: integer;
  formatNamespaceURI: String;
  formatName: String;
  procedure raiseInvalidPicture;
  begin
    raise EXQEvaluationException.create('FODF1310', 'Invalid picture string for format-number: ' + picture);
  end;
  procedure checkDuplicate(prop: TXQDecimalFormatProperty);
  begin
    if pictureParser[currentPictureParser].foundChar[prop] then raiseInvalidPicture;
    pictureParser[currentPictureParser].foundChar[prop] := true;
  end;
  procedure checkPictureFinal;
  begin
    with pictureParser[currentPictureParser] do begin
      if (not  foundChar[xqdfpDigit]) and (not foundChar[xqdfpZeroDigit]) then raiseInvalidPicture;
      if (foundChar[xqdfpExponentSeparator] and (exponentMandatory + exponentOptional > 0))
         and (foundChar[xqdfpPercent] or foundChar[xqdfpPerMille]) then raiseInvalidPicture;
    end;
  end;

begin
  requiredArgCount(args, 2, 3);
  data := @XQDefaultDecimalFormat;
  if context.staticContext.decimalNumberFormats<> nil then begin
    formatNamespaceURI := '';
    formatName := '';
    if length(args) = 3 then if not splitEQName(context.staticContext, args[2].toString, formatNamespaceURI, formatName) then
      raise EXQEvaluationException.create('FODF1280', 'Invalid date format name', nil, args[2]);
    for i := 0 to context.staticContext.decimalNumberFormats.count - 1 do
      with TXQDecimalFormat(context.staticContext.decimalNumberFormats[i]) do
        if (formatNamespaceURI = namespaceURL) and (formatName = localname) then begin
          data := @formats;
          break;
        end;
  end;
  //todo matching eqname in args[2] :FODF1280, if it is no syntactical eqname

  //analyze picture
  picture := args[1].toString;
  FillChar(pictureParser, sizeof(pictureParser), 0);
  currentPictureParser := 0;
  pictureParser[0].subPosition := spInPrefix;
  pictureParser[1].subPosition := spInPrefix;
  for c in strIterator(picture) do with pictureParser[currentPictureParser] do begin
    activeChar := true;
    if c = data^.chars[xqdfpDecimalSeparator] then begin
      checkDuplicate(xqdfpDecimalSeparator);
      case subPosition of
        spExponentItself: raiseInvalidPicture;
        spInPrefix, spInInteger: subPosition := spInFraction;
      end;
    end else if c = data^.chars[xqdfpGroupingSeparator] then begin
      if subPosition = spExponentItself then raiseInvalidPicture;
      case subPosition of
        spInPrefix, spInInteger: begin
          subPosition := spInInteger;
          arrayAdd(integerGroups, integerMandatory + integerOptional);
        end;
        spInFraction: begin
          arrayAdd(fractionGroups, fractionMandatory + fractionOptional);
        end;
        //exponent?
      end;
    end {else if c = data^.chars[xqdfpMinusSign] then begin it is just a passive character
    end }else if (c = data^.chars[xqdfpPercent]) or (c = data^.chars[xqdfpPerMille]) then begin
      checkDuplicate(xqdfpPercent);
      if c = data^.chars[xqdfpPerMille] then foundChar[xqdfpPerMille] := true;
      activeChar := false;
    end else if (c >= data^.chars[xqdfpZeroDigit]) and (c < data^.chars[xqdfpZeroDigit] + 10)  then begin //mandatory digit
      foundChar[xqdfpZeroDigit]:=true;
      case subPosition of
        spInPrefix, spInInteger: inc(integerMandatory);
        spInFraction: begin
          if fractionOptional > 0 then raiseInvalidPicture;
          inc(fractionMandatory);
        end;
        spInExponent,spExponentItself: begin
          inc(exponentMandatory);
          subPosition := spInExponent
        end;
      end;
    end else if (c = data^.chars[xqdfpDigit]) then begin//optional digit
      foundChar[xqdfpDigit]:=true;
      case subPosition of
        spInPrefix, spInInteger: begin
          if integerMandatory > 0 then raiseInvalidPicture;
          inc(integerOptional);
        end;
        spInFraction: inc(fractionOptional);
        spInExponent,spExponentItself: begin
          inc(exponentOptional);
          subPosition := spInExponent
        end;
      end;
    end else if c = data^.chars[xqdfpPatternSeparator] then begin
      checkPictureFinal;
      if (currentPictureParser = 1) or (subPosition = spExponentItself) then raiseInvalidPicture;
      currentPictureParser := 1;
      continue;
    end else if c = data^.chars[xqdfpExponentSeparator] then begin
      case subPosition of
        spInPrefix: prefix += strGetUnicodeCharacter(c);
        spInInteger, spInFraction, spInExponent, spExponentItself: begin
          subPosition := spInFraction;
          checkDuplicate(xqdfpExponentSeparator);
        end;
        spInSuffix: suffix += strGetUnicodeCharacter(c);
      end;
    end else activeChar := false;
    if not activeChar then begin
      if subPosition = spInPrefix then prefix += strGetUnicodeCharacter(c)
      else begin
        if subPosition = spExponentItself then suffix += strGetUnicodeCharacter(data^.chars[xqdfpExponentSeparator]);
        suffix += strGetUnicodeCharacter(c);
        subPosition := spInSuffix;
      end;
    end;
    case subPosition of
      spInPrefix: if activeChar then subPosition := spInInteger;
      spInInteger, spInFraction, spInExponent, spExponentItself: if not activeChar then subPosition := spInSuffix;
      spInSuffix: if activeChar then raiseInvalidPicture;
    end;
  end;
  checkPictureFinal;

  case args[0].kind of
    pvkUndefined: exit(xqvalue(data^.nan));
    pvkFloat: begin
      numberf := args[0].toFloat;
      if IsNan(numberf) then exit(xqvalue(data^.nan));
      if not isSignedXQFloat(numberf) then currentPictureParser := 0
      else if currentPictureParser = 0 then pictureParser[0].prefix := strGetUnicodeCharacter(data^.chars[xqdfpMinusSign]) + pictureParser[0].prefix;
      if IsInfinite(numberf) then
        with pictureParser[currentPictureParser] do
          exit(xqvalue(prefix + data^.infinity + suffix));
      if args[0].instanceOf(baseSchema.float) then number := FloatToBigDecimal(single(numberf))
      else number := FloatToBigDecimal(numberf);
    end
    else begin
      number := args[0].toDecimal;
      if not number.signed then currentPictureParser := 0
      else if currentPictureParser = 0 then pictureParser[0].prefix := strGetUnicodeCharacter(data^.chars[xqdfpMinusSign]) + pictureParser[0].prefix
    end;
  end;

  with pictureParser[currentPictureParser] do begin
    number.signed := false;
    if foundChar[xqdfpPercent] then begin
      if foundChar[xqdfpPerMille] then shift10(number, 3)
      else shift10(number, 2)
    end;

    //todo: exponent (5,6)
    number := round(number, -(fractionMandatory + fractionOptional) );

    arabic := BigDecimalToStr(number);
    if (integerMandatory = 0) and (fractionOptional + fractionMandatory > 0) and strBeginsWith(arabic, '0') then begin
      delete(arabic, 1, 1);
      if arabic = '' then arabic := '.0';
    end;
    dot := strIndexOf(arabic, '.');

    if dot = 0 then begin
      dot := length(arabic) + 1;
      fractionActual := 0;
    end else begin
      fractionActual := length(arabic) - dot;
    end;
    integerActual := dot - 1;

    if length(integerGroups) > 0 then begin
      for i := High(integerGroups) downto 0 do
        integerGroups[i] := integerOptional + integerMandatory - integerGroups[i];
      integerGroupDelta := integerGroups[high(integerGroups)];
      for i := high(integerGroups) - 1 downto 0 do
        if integerGroupDelta <> integerGroups[i] - integerGroups[i+1] then begin
          integerGroupDelta := -1;
          break;
        end;
    end;

    resstr := '';
    groupPos := high(integerGroups);
    j := 1 - max(0, integerMandatory - integerActual);
    for i := dot - 1 downto j do begin
      if i >= 1 then c := ord(arabic[i]) - ord('0')
      else c := 0;
      resstr := strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit] + c  ) + resstr;
      if groupPos >= 0 then begin
        if (dot - i >= integerGroups[groupPos]) and (i <> j) then begin
          resstr := strGetUnicodeCharacter(data^.chars[xqdfpGroupingSeparator]) + resstr;
          if integerGroupDelta < 0 then dec(groupPos)
          else integerGroups[groupPos] += integerGroupDelta;
        end;
      end;
    end;
    if (dot < length(arabic)) or (fractionMandatory > 0) then begin
      resstr += strGetUnicodeCharacter(data^.chars[xqdfpDecimalSeparator]);
      groupPos := 0;
      for i := 1 to max(fractionMandatory, fractionActual) do begin
        if (groupPos <= High(fractionGroups)) and (i > fractionGroups[groupPos]) then begin
          resstr += strGetUnicodeCharacter(data^.chars[xqdfpGroupingSeparator]);
          inc(groupPos);
        end;
        if i <= fractionActual then c := ord(arabic[i + dot])  - ord('0')
        else c := 0;
        resstr += strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit] + c );
      end;
    end;

    result := xqvalue(prefix + resstr + suffix);
  end;
end;
