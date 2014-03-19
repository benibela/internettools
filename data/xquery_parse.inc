{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

type

{ TXQParsingContext }

 TXQParsingContext = class
  engine: TXQueryEngine;

  options: TXQParsingOptions;
  parsingModel: TXQParsingModel;
  encoding: TEncoding;
  staticContext: TXQStaticContext;

  str: string;
  pos: pchar;
  resultquery: TXQuery;

  procedure raiseParsingError(errcode, s: string);
  procedure raiseInvalidModel(s: string);

  procedure requireXQuery(s: string = '');
  procedure require3(s: string = '');
  procedure requireXQuery3(s: string = '');

  procedure skipWhitespace();
  procedure skipComment();
  procedure skipWhitespaceAndComment();
  procedure expect(c: char);
  procedure expect(s: string);
  function nextToken(lookahead: boolean=false): string;
  function nextTokenNCName(): string; inline; //returns a NCName (todo: raise an error if not an NCName)
  function nextTokenMaybeQName(): string; inline; //returns a QName, if there is one, or the same as nextToken otherwise
  function nextTokenQName(allowWildcards: boolean=false): string; inline; //returns a QName (todo: raise an error if not a QName)


  function parseSequenceLike(target: TXQTerm; closingChar: char = ')'): TXQTerm;
  function parseSequenceType(): TXQTermSequenceType;
  function replaceEntities(s: string): string;
  function parseString(const w: string): TXQTerm;
  function parseStringLiteral: string;
  function parseXString(nullTerminatedString: boolean = false): TXQTerm; //**< parses an extended string like @code(x"foo""bar"), @code(x"foo{$varref}ba{1+2+3}r")
  function parseJSONLikeObjectConstructor(): TXQTerm; //**< parses an json object constructor { "name": value, .. } or {| ... |}
  function parseJSONLikeArray(): TXQTermJSONArray;


  function parseFlower(akind: string): TXQTermFlower;
  function parseSomeEvery(const akind: string): TXQTermSomeEvery;
  function parseTypeSwitch: TXQTermTypeSwitch;
  function parseSwitch: TXQTermSwitch;
  function parseDirectConstructor(name: string): TXQTermConstructor;
  function parseComputedConstructor(name: string): TXQTermConstructor;
  function parseExtension: TXQTerm;
  function parseVariable: TXQTerm;

  //**Parses the next complete value@br
  //**i.e. it will parse from the left, until it obtains a value that can be evaluated.
  //**e.g. from @code(1 + 2) it will parse @code(1) (since @code(1) is complete), from @code(function(1,2,3)) it will parse everything (since @code(function) can not be evaluated without its parameters))@br
  //**(observation: that kind of looks like it parses everything except binary operators)
  function parseValue: TXQTerm;        //left most token of an ExprSingle
  function parse: TXQTerm;             //ExprSingle
  function parsePrimaryLevel: TXQTerm; //Expr
  function parseModule(): TXQTerm;

  procedure postProcessJSONLiterals(var t: TXQTerm);
end;



procedure TXQParsingContext.raiseParsingError(errcode, s: string);
begin
  if (pos < @str[1]) then pos := @str[1]; //make sure pos points to str
  if (pos > @str[length(str)]) then pos := @str[length(str)];
  raise EXQParsingException.Create(errcode, s+#13#10'in: '+strslice(@str[1],pos-1)+' [<- error occurs before here] '+strslice(pos,@str[length(str)]));
end;

procedure TXQParsingContext.raiseInvalidModel(s: string);
begin
  raiseParsingError('XPST0003', 'This language feature is not available in the selected language. '+s);
end;

procedure TXQParsingContext.requireXQuery(s: string);
begin
  if (parsingModel <> xqpmXQuery1) and (parsingModel <> xqpmXQuery3) then raiseInvalidModel('XQuery is required '+s);
end;

procedure TXQParsingContext.require3(s: string);
begin
  if (parsingModel <> xqpmXPath3) and (parsingModel <> xqpmXQuery3) then raiseInvalidModel('At least XQuery/XPath version 3.0 is required '+s);
end;

procedure TXQParsingContext.requireXQuery3(s: string);
begin
  if (parsingModel <> xqpmXQuery3) then raiseInvalidModel('XQuery version 3.0 is required '+s);
end;

procedure TXQParsingContext.skipWhitespace;
begin
  while pos^ in WHITE_SPACE do pos += 1;
end;

procedure TXQParsingContext.skipComment;
var nestene: integer;
begin
  nestene:=0;
  while pos^ <> #0 do begin
    if (pos^ = ':') and ((pos+1)^ = ')') then begin
      pos+=2;
      nestene-=1;
      if nestene=0 then exit;
    end else if (pos^ = '(') and ((pos+1)^ = ':') then begin
      pos+=2;
      nestene+=1;
    end else pos+=1;
  end;
end;

procedure TXQParsingContext.skipWhitespaceAndComment;
begin
  while (pos^ in WHITE_SPACE) or ((pos^ = '(') and ((pos+1)^ = ':')) do begin
    while pos^ in WHITE_SPACE do pos+=1;
    if (pos^ = '(') and ((pos+1)^ = ':') then
      skipComment();
  end;
end;

procedure TXQParsingContext.expect(c: char);
begin
  skipWhitespaceAndComment;
    if pos^ <> c then
      raise EXQParsingException.Create('XPST0003', '"'+c+'"'+' expected, but "'+pos^+'" found'#13#10+strslice(@str[1],pos));
    pos+=1;
end;

procedure TXQParsingContext.expect(s: string);
var
   i: Integer;
begin
  skipWhitespaceAndComment;
  for i:=1 to length(s) do begin
    if pos^ <> s[i] then
      raise EXQParsingException.Create('XPST0003', '"'+ s+'" expected, but "'+pos^+'" found'#13#10+strslice(@str[1],pos));
    pos+=1;
  end;
end;

//read the next token ('string', number: (-?[0-9]+|[0-9]*.[0-9]+|[0-9]+.[0-9]*)([eE][+-]?[0-9]+)?, symbol, identifier)
const SYMBOLS = ['''','"', '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', ':', '@', '$'];
const START_SYMBOLS = ['-'];
function TXQParsingContext.nextToken(lookahead: boolean=false): string;
var start:pchar;
   numberE, numberPoint: boolean;
begin
  skipWhitespaceAndComment;
  if pos^ = #0 then exit('');
  start:=pos;
  if pos^='''' then begin
    repeat
      pos+=1;
      if (pos^ = '''') then
        if ((pos+1)^ <> '''') then break
        else pos+=1;
    until pos^ in [#0];
    pos+=1;
  end else if pos^='"' then begin
    repeat
      pos+=1;
      if (pos^ = '"') then
        if ((pos+1)^ <> '"') then break
        else pos+=1;
    until pos^ in [#0];
    pos+=1;
  end else if (pos^ = ':') and ((pos+1)^ in ['=', ':']) then begin
    pos+=2;
  end else if pos^ in (SYMBOLS + START_SYMBOLS) then begin
    if lookahead then exit(pos^)
    else begin
      pos+=1;
      exit((pos-1)^);
    end;
  end else if (pos^ in ['0'..'9','-']) or ((pos^ = '.') and ((pos+1)^ in ['0'..'9','e','E'])) then begin
    numberE:=false;
    numberPoint:=pos^ = '.';
    repeat
      pos+=1;
      if pos^ = '.' then begin
        if numberPoint then raiseParsingError('XPST0003', 'Double . in number');
        numberPoint:=true;
        pos+=1;
      end;
      if pos^ in ['e','E'] then begin
        if numberE then raiseParsingError('XPST0003', 'Double e in number');
        pos+=1;
        numberE:=true;
        if not (pos^ in ['0'..'9','+','-']) then raiseParsingError('XPST0003', 'Invalid character after e in number')
        else pos+=1;
      end;
    until not (pos^ in ['0'..'9']);
  end else begin
    repeat
      pos+=1;
    until (pos^ in SYMBOLS + WHITE_SPACE + [#0]) or ((pos^ = ':') and ((pos+1)^='='));
  end;
  assert(start<pos);
  result:=strslice(start,pos-1);
  if lookahead then pos:=start;
end;

function TXQParsingContext.nextTokenNCName(): string;
begin
  result := nextToken(false);
end;

function TXQParsingContext.nextTokenMaybeQName(): string;
begin
  result := nextToken();
  if (pos^ = ':') and not ((pos+1)^ in (SYMBOLS + START_SYMBOLS + WHITE_SPACE - ['*'])) then begin //same check in parseValue for matchers
    expect(':');
    result += ':' + nextToken();
  end;
end;

function TXQParsingContext.nextTokenQName(allowWildcards: boolean): string;
begin
  result := nextTokenMaybeQName();
  //todo check that it is really a QName
  if result = '' then raiseParsingError('XPST0003', 'Expected QName');
  if (not allowWildcards) and ((result[1] = '*') or (result[length(result)] = '*')) then raiseParsingError('XPST0003', 'Expected QName, got: *');
end;

function TXQParsingContext.parseSequenceLike(target: TXQTerm; closingChar: char ): TXQTerm;
var
  t: String;
begin
  result := target;
  skipWhitespaceAndComment();
  if pos^ = closingChar then begin expect(closingChar); exit(); end;
  result.push(parse());
  t := nextToken();
  while t = ',' do begin
    result.push(parse());
    t := nextToken();
  end;
  if t <> closingChar then raiseParsingError('XPST0003', 'Expected closing parenthesis: '+ closingChar);
end;

function isKindTestFunction(const word: string): boolean;  //Lookahead to recognize KindTest of the XPath-EBNF
begin
  result := (word = 'text') or (word = 'node') or (word = 'comment') or (word = 'processing-instruction')
            or (word = 'element') or (word = 'document-node') or (word = 'schema-element') or (word = 'attribute') or (word = 'schema-attribute');
end;

function TXQParsingContext.parseSequenceType: TXQTermSequenceType;
var word: string;
  namespace: INamespace;
begin
  word := nextTokenQName();

  result := TXQTermSequenceType.Create();
  try
    result.allowNone:=false;
    result.allowMultiple:=false;

    result.name:=word;
    if (isKindTestFunction(word)) or (word = 'empty-sequence') or (word = 'item') then begin
      expect('(');

      skipWhitespaceAndComment();
      if pos^ = ')' then expect(')')
      else begin
        result.push(parse());
        if nextToken() = ',' then begin
          result.push(parseSequenceType());
          expect(')');
        end;
      end;

      if (word = 'empty-sequence') then begin
        result.kind:=tikNone;
        if length(result.children) <> 0 then raiseParsingError('XPST0003', 'invalid sequence type');
        exit
      end else if word = 'item' then begin
        result.kind:=tikAny;
        if length(result.children) <> 0 then raiseParsingError('XPST0003', 'invalid sequence type');
      end else begin
         result.kind:=tikElementTest;
         result.nodeMatching := convertElementTestToPathMatchingStep(word, result.children);
      end;
    end else if options.AllowJSON and ((word = 'array') or (word = 'object') or (word = 'json-item') or (word = 'structured-item')) then begin
      expect('('); expect(')');
      case word of
        'json-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.jsonItem; end;
        'structured-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseSchema.structuredItem; end;
        'array': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.array_; end;
        'object': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.object_; end;
        else raiseParsingError('XPST0003', 'WTF??');
      end;
    end else begin;
      staticContext.splitRawQName(namespace, word, xqdnkType);
      if (namespace = nil) then raiseParsingError('XPST0008', 'No schemas loaded for type '+word);
      result.kind:=tikAtomic;
      result.atomicTypeInfo := engine.findType(namespace.getURL, word);
      if result.atomicTypeInfo = nil then raiseParsingError('XPST0008', 'Unknown type: {'+namespace.getURL+'}:'+word);
    end;


    word := nextToken(true);
    if (length(word) = 1) and (word[1] in ['?', '*', '+']) then begin
      case word[1] of
        '?': result.allowNone:=true;
        '+': result.allowMultiple:=true;
        '*': begin result.allowNone:=true; result.allowMultiple:=true; end;
      end;
      pos+=1;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseFlower(akind: string): TXQTermFlower;
  procedure parseVariableName(out name: string; out namespace: INamespace);
  begin
    if nextToken() <> '$' then raiseParsingError('XPST0003', 'Expected variable name');
    name := nextTokenQName();
    staticContext.splitRawQName(namespace, name, xqdnkUnknown);
  end;

  procedure parseVarBinding;
  var temp: string;
  begin
    setlength(result.vars, length(result.vars) + 1);
    with result.vars[high(result.vars)] do begin
      if akind = 'for' then kind := xqfkFor
      else if akind = 'let' then kind := xqfkLet
      else raiseParsingError('XPST0003', 'Invalid flower: '+akind);

      if (kind = xqfkLet) and (parsingModel = xqpmXPath2) then raiseInvalidModel('let is not supported in XPath 2.0');

      parseVariableName(varname, namespace);

      temp := nextToken;
      if temp = 'as' then begin
        requireXQuery;
        sequenceTyp := parseSequenceType;
        temp := nextToken;
      end else sequenceTyp := nil;

      if kind = xqfkFor then begin

        {if temp = 'allowing' then begin
          requireXQuery3;
          allowingEmpty := nextToken;
          temp := nextToken;
        end;}

        if temp = 'at' then begin
          requireXQuery;
          parseVariableName(positionVarname, positionVarNamespace);
          temp := nextToken;
        end;

        if temp <> 'in' then raiseParsingError('XPST0003', 'Expected "in".')
      end else
        if temp <> ':=' then raiseParsingError('XPST0003', 'Expected ":=".');

      expr := parse();
    end;
  end;

var
  token: String;

  procedure parseOrderSpecAndNextToken;
  begin
    setlength(result.orders, length(result.orders) + 1);
    with result.orders[high(result.orders)] do begin
      expr := parse();
      token := nextToken;

      if (token = 'ascending') or (token = 'descending') then begin
        descending := token = 'descending';
        token := nextToken;
      end;

      emptyOrder := xqeoStatic;
      if token = 'empty' then begin
        token := nextToken;
        if token = 'greatest' then emptyOrder := xqeoEmptyGreatest
        else if token = 'least' then emptyOrder := xqeoEmptyLeast
        else raiseParsingError('XPST0003', 'Expected "greatest" or "least"');
        token := nextToken;
      end;

      if token = 'collation' then begin
        collation := nextToken;
        if (collation = '') or not (collation[1] in ['''', '"']) or (collation[1] <> collation[length(collation)]) then raiseParsingError('XPST0003', 'Invalid collation');
        collation := copy(collation, 2, length(collation) - 2);
        token := nextToken;
      end
    end;
  end;
begin
  result := TXQTermFlower.Create;
  try
    parseVarBinding;

    token := nextToken();

    while (token = ',') or (token = 'for') or (token = 'let') do begin
      if token <> ',' then begin
        requireXQuery;
        akind := token;;
      end;
      parseVarBinding;
      token := nextToken();
    end;

    if token <> 'return' then begin
      requireXQuery;

      if token = 'where' then begin
        result.where := parse;
        token := nextToken();
      end;

      if (token = 'stable') or (token = 'order') then begin
        if token = 'stable' then expect('order'); //always be stable
        expect('by');
        repeat
          parseOrderSpecAndNextToken;
        until token <> ',';
        if token <> 'return' then raiseParsingError('XPST0003', 'Expected return');
      end;
    end;

    result.returned := parse;
  except
    on EXQParsingException do begin result.free; raise; end;
  end;
end;

function TXQParsingContext.parseSomeEvery(const akind: string): TXQTermSomeEvery;
var
  word: String;
begin
  result := TXQTermSomeEvery.Create(akind = 'every');
  try
    expect('$');
    result.push(TXQTermVariable.Create(nextTokenQName(), staticContext));
    skipWhitespaceAndComment();
    if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType()); end;
    expect('in'); result.push(parse());
    word := nextToken();
    while word = ',' do begin
      expect('$');
      result.push(TXQTermVariable.Create(nextTokenQName(), staticContext));
      skipWhitespaceAndComment();
      if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType()); end;
      expect('in'); result.push(parse());
      word := nextToken();
    end;
    Assert(word = 'satisfies');
    result.push(parse());
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseTypeSwitch: TXQTermTypeSwitch;
var
  word: String;
begin
  requireXQuery('for typeswitch statement');
  result := TXQTermTypeSwitch.Create;
  result.push(parsePrimaryLevel());
  expect(')');

  word := nextToken();
  while word = 'case' do begin
    skipWhitespaceAndComment();
    result.push(TXQTermSequence.Create);
    if pos^ = '$' then begin expect('$'); result.children[high(result.children)].push(TXQTermVariable.create(nextTokenQName(), staticContext)); expect('as'); end;
    result.children[high(result.children)].push(parseSequenceType());
    expect('return');
    result.children[high(result.children)].push(parse());
    word := nextToken();
  end;
  if word <> 'default' then raiseParsingError('XPST0003', 'expected "default" clause');
  skipWhitespaceAndComment();
  result.push(TXQTermSequence.Create);
  if pos^ = '$' then begin expect('$'); result.children[high(result.children)].push(TXQTermVariable.create(nextTokenQName(), staticContext)); end;
  expect('return');
  result.children[high(result.children)].push(parse());
end;

function TXQParsingContext.parseSwitch: TXQTermSwitch;
var
  word: String;
begin
  requireXQuery3('for switch statement');
  result := TXQTermSwitch.Create;
  result.push(parsePrimaryLevel());
  expect(')');

  word := nextToken();
  while word = 'case' do begin
    skipWhitespaceAndComment();
    result.push(TXQTermSequence.Create);
    while word = 'case' do begin
      result.children[high(result.children)].push(parse());
      word := nextToken();
    end;
    if word <> 'return' then raiseParsingError('XPST0003', 'expected return');
    result.children[high(result.children)].push(parse());
    word := nextToken();
  end;
  if word <> 'default' then raiseParsingError('XPST0003', 'expected "default" clause');
  expect('return');
  result.push(parse());
end;

function TXQParsingContext.parseDirectConstructor(name: string): TXQTermConstructor;
  procedure parseCommonContent(parent: TXQTermConstructor; border: char; atBoundary: boolean);
    procedure pushElementContent(s: string);
    begin
      if length(s) = 0 then exit;
      if (length(parent.children) > 0) and (parent.children[high(parent.children)] is TXQTermString) then
        (parent.children[high(parent.children)] as TXQTermString).value += s
      else
        parent.push(TXQTermString.create(s));
    end;
    function strIsWhitespace(p: pchar; l: longint): boolean;
    begin
      if l = 0 then exit(true);
      strlTrimRight(p, l, WHITE_SPACE);
      result := l = 0;
    end;

  var marker: PChar;
    temp: TXQTerm;
  begin
    atBoundary := atBoundary and staticContext.stripBoundarySpace;
    while pos^ <> #0 do begin
      marker := pos;
      while not (pos^ in [#0, '{', '}', '&']) and (pos^ <> border) and ((border = '<') or not (pos^ in [#9, #10, #13])) do pos+=1;

      if pos^ in [#9, #10, #13] then begin //whitespace normalization #9, #10, #13 => space
        pushElementContent(strFromPchar(marker, pos - marker));
        if ((pos-1)^ <> #13) or (pos^ <> #10) then
          pushElementContent(' ');
        pos+=1;
        continue;
      end;

      if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');

      if not atBoundary or (pos^ in ['}', '&']) or strBeginsWith(pos, '<![') or not strIsWhitespace(marker, pos - marker) then begin
        pushElementContent(strFromPchar(marker, pos - marker));
        atBoundary := false;
      end;

      if pos^ = border then begin
        if (pos^ in ['''', '"']) and ((pos+1)^ = border) then begin
          pushElementContent(border);
          pos+=2;
          continue;
        end else exit;
      end;

      case pos^ of
        '{': if (pos+1)^ <> '{' then begin
          pos += 1;
          temp := parsePrimaryLevel;
          if temp is TXQTermConstructor then parent.push(TXQTermSequence.Create().push([temp]))
          else parent.push(temp);
          expect('}');
          atBoundary := (border = '<') and staticContext.stripBoundarySpace;
        end else begin
          pushElementContent('{');
          pos+=2;
        end;
        '}': begin
          if (pos+1)^ <> '}' then raiseParsingError('XPST0003', 'Invalid closing parenthesis');
          pushElementContent('}');
          atBoundary:=false;
          pos += 2;
        end;
        '&': begin
          marker := pos;
          while not (pos^ in [#0, ';']) do pos += 1;
          if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
          pos += 1;
          pushElementContent(replaceEntities(strFromPchar(marker, pos - marker)));
          atBoundary:=false;
        end;
      end;
    end;
  end;

  function parsePIConstructor: TXQTermConstructor;
  var marker: PChar;
  begin
    result := TXQTermConstructor.create(tetProcessingInstruction, txqtermstring.create(nextTokenQName()));
    skipWhitespace();
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '?') or ((pos+1)^ <> '>')) do pos+=1;
    result.push(txqtermstring.create(strFromPchar(marker, pos - marker)));
    if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
    expect('?>');
  end;

  function parseCommentConstructor: TXQTermConstructor;
  var marker: PChar;
  begin
    expect('--');
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '-') or ((pos+1)^ <> '-')) do pos+=1;
    if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
    result := TXQTermConstructor.create(tetComment, txqtermstring.create(strFromPchar(marker, pos - marker)));
    expect('-->');
  end;

var
  token: String;
  marker: PChar;
  attribute: TXQTermConstructor;
  lastWasCData: Boolean;
  i: Integer;
  oldDefaultElementTypeNamespace: INamespace;
begin
  name := nextTokenMaybeQName();
  if name = '!' then exit(parseCommentConstructor)
  else if name = '?' then exit(parsePIConstructor);
  result := TXQTermConstructor.create(tetOpen, TXQTermString.create(name));
  token := nextTokenMaybeQName();
  while (token <> '>') and (token <> '/') and (token <> '') do begin
    attribute := TXQTermConstructor.create(tetAttribute, TXQTermString.create(token));
    expect('=');
    skipWhitespaceAndComment();
    if not (pos^ in ['''', '"']) then raiseParsingError('XPST0003', 'Expected attribute value');
    marker := pos;
    pos+=1;
    parseCommonContent(attribute, marker^, false);
    if strBeginsWith(TXQTermString(attribute.nameValue).value, 'xmlns:') or (TXQTermString(attribute.nameValue).value = 'xmlns') then begin
      token := '';
      if length(attribute.children) > 0 then begin
        if not (attribute.children[0] is TXQTermString) then raiseParsingError('XQST0022', 'Namespace url is not a literal string');
        token := TXQTermString(attribute.children[0]).value;
      end; //else namespace undeclaration
      if result.implicitNamespaces = nil then result.implicitNamespaces := TNamespaceList.Create;
      if TXQTermString(attribute.nameValue).value = 'xmlns' then result.implicitNamespaces.add(TNamespace.create(token, ''))
      else result.implicitNamespaces.add(TNamespace.create(token, strCopyFrom(TXQTermString(attribute.nameValue).value, length('xmlns:')+1)));
      attribute.Free;
    end else result.push(attribute);
    expect(marker^);
    token := nextTokenMaybeQName();
  end;
  if token = '' then begin
    result.Free;
    raiseParsingError('XPST0003', 'Attribute expected');
  end;
  if token = '/' then begin
    expect('>');
    exit;
  end;

  oldDefaultElementTypeNamespace := staticContext.defaultElementTypeNamespace;
  if Result.implicitNamespaces <> nil then begin
    if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
    for i := 0 to result.implicitNamespaces.Count - 1 do begin
      staticContext.namespaces.add(Result.implicitNamespaces.namespaces[i]);
      if Result.implicitNamespaces.namespaces[i].getPrefix = '' then
        staticContext.defaultElementTypeNamespace := Result.implicitNamespaces.namespaces[i];
    end;
  end;

  lastWasCData := false;
  while pos^ <> #0 do begin
    parseCommonContent(result, '<', not lastWasCData);
    lastWasCData := false;

    if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
    if pos^ = '<' then begin
      pos += 1;
      case pos^ of
        '/': begin
          pos += 1;
          if nextTokenQName() <> name then raiseParsingError('XPST0003', 'Expected matching closing tag');
          expect('>');
          staticContext.defaultElementTypeNamespace := oldDefaultElementTypeNamespace;
          if Result.implicitNamespaces <> nil then
            for i := 0 to result.implicitNamespaces.Count - 1 do
              staticContext.namespaces.Delete(staticContext.namespaces.Count-1);

          exit;
        end;
        '!': if strBeginsWith(pos, '![CDATA[') then begin
          lastWasCData := true;
          pos += length('![CDATA[');
          marker := pos;
          while (pos^ <> #0) and not strBeginsWith(pos, ']]>') do pos+=1;
          if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
          result.push(TXQTermString.create(strFromPchar(marker, pos - marker)));
          pos+=3;
        end else if strBeginsWith(pos, '!--') then begin
          pos += 1;
          result.push(parseCommentConstructor);
        end else raiseParsingError('XPST0003', 'Invalid character combination after <!');
        '?': begin
          pos += 1;
          result.push(parsePIConstructor);
        end;
        else result.push(parseDirectConstructor('<'));
      end;
    end;
  end;
  raiseParsingError('XPST0003', 'Unexpected end (probably missing closing tag for <'+name+'>')
end;

function TXQParsingContext.parseComputedConstructor(name: string): TXQTermConstructor;
var
  token: String;
  tempSeq: TXQTerm;
  onlyConstructors: Boolean;
  i: Integer;
  expectName: Boolean;
begin
  token := name;
  if (token = 'element') then result := TXQTermConstructor.create(tetOpen)
  else if (token = 'document') then result := TXQTermConstructor.create(tetDocument)
  else if (token = 'attribute') then result := TXQTermConstructor.create(tetAttribute)
  else if (token = 'text') then result := TXQTermConstructor.create(tetText)
  else if (token = 'processing-instruction') then result := TXQTermConstructor.create(tetProcessingInstruction)
  else if (token = 'comment') then result := TXQTermConstructor.create(tetComment)
  else raiseParsingError('XPST0003', 'Unknown constructor name');
  expectName := (result.typ in [tetOpen, tetProcessingInstruction, tetAttribute]) ;
  if expectName then begin
    token := nextTokenMaybeQName();
    if token = '{' then begin
      result.nameValue := parsePrimaryLevel;
      expect('}');
    end else begin
      result.nameValue := TXQTermString.create(token);
      if (token = '') or not (token[1] in ['a'..'z','A'..'Z']) then raiseParsingError('XPST0003', 'Expected qname, got '+token);
    end;
  end;
  expect('{');
  skipWhitespaceAndComment();
  if pos^ <> '}' then begin
    if result.typ in [tetDocument, tetOpen, tetProcessingInstruction, tetAttribute] then begin
      tempSeq := parsePrimaryLevel;
      if tempSeq is TXQTermSequence then begin
        onlyConstructors := true;
        for i:= 0 to high(tempseq.children) do
          if not (tempSeq.children[i] is TXQTermConstructor) then begin
            onlyConstructors:=false;
            break;
          end;
        if onlyConstructors then begin
          result.children := tempseq.children;
          tempseq.children := nil;
          tempSeq.free;
        end else result.push(tempSeq); //that's really slow for nodes because it makes a deep copy of them if they are taken from a subsequence. But if it's mixing atomic/nodes flattening the sequences makes the separator spaces wrong
      end else result.push(tempSeq);
    end else result.nameValue := parsePrimaryLevel;
  end else if not expectName then
    raiseParsingError('XPST0003', 'This type of node must not be empty ');
  expect('}');
end;

function TXQParsingContext.parseExtension: TXQTerm;
var
  pragma: String;
begin
  requireXQuery('for extensions');
  expect('#');
  pragma := nextTokenQName();
  if system.pos(':', pragma) = 0 then raiseParsingError('XPST0003', 'Extension name requires namespace');
  while (pos^ <> #0) and ((pos^ <> '#') or ((pos+1)^ <> ')')) do
    pos += 1;
  if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
  pos += 2;
  skipWhitespaceAndComment();
  if (pos^ = '(') and ((pos+1)^ = '#') then begin
    expect('(');
    exit(parseExtension());
  end;
  expect('{'); result := parsePrimaryLevel; expect('}');
end;

function TXQParsingContext.parseVariable: TXQTerm;
var
  operatorMode: Boolean;
  propertyAccess: Boolean;
begin
  result := TXQTermVariable.Create(nextTokenQName(), staticContext);
  operatorMode := false;
  if (options.AllowPropertyDotNotation = xqpdnAllowFullDotNotation) and strContains(TXQTermVariable(result).value, '.') then begin
    propertyAccess := true;
    operatorMode := strEndsWith(TXQTermVariable(result).value, '.');
    if operatorMode then begin
      delete(TXQTermVariable(result).value, length(TXQTermVariable(result).value), 1);
      propertyAccess := strContains(TXQTermVariable(result).value, '.');
    end;
    if propertyAccess then
      result := TXQTermVariable.splitForDotNotation(TXQTermVariable(result));
  end else if (options.AllowPropertyDotNotation = xqpdnAllowUnambiguousDotNotation) and strEndsWith(TXQTermVariable(result).value, '.') then begin
    skipWhitespaceAndComment();
    operatorMode := pos^ in ['"', '''', '$'];
    if operatorMode then
      delete(TXQTermVariable(result).value, length(TXQTermVariable(result).value), 1);
  end;
  if operatorMode then begin
    result := TXQTermDynamicFunctionCall.create(result, parseValue());
    if result.children[1] is TXQTermNodeMatcher then begin
      result.free;
      raiseParsingError('pxp:XPST0003', 'A node matching step is not allowed directly after a property dot operator');
    end;
  end;
end;

function TXQParsingContext.replaceEntities(s: string): string;
var
  n, p: Integer;
  temp: string;
begin
  result := '';
  p := 1;
  n := strIndexOf(s, '&');
  while n > 0 do begin
    result += copy(s, p, n - p);
    p := n + 1;
    n := strIndexOf(s, ';', p);
    temp := copy(s, p, n - p);
    case temp of
      'lt': result += '<';
      'gt': result += '>';
      'amp': result += '&';
      'quot': result += '"';
      'apos': result += '''';
      else begin
        if (temp = '') or (temp[1] <> '#') then raiseParsingError('XPST0003', 'Invalid entity');
        delete(temp,1,1);
        if temp[1] = 'x' then result += strGetUnicodeCharacter(StrToInt('$'+strcopyfrom(temp,2)), staticContext.stringEncoding)
        else result += strGetUnicodeCharacter(StrToInt(temp), staticContext.stringEncoding);
      end;
    end;
    p := n + 1;
    n := strIndexOf(s, '&', n);
  end;
  result += strcopyfrom(s, p);
end;

function TXQParsingContext.parseString(const w: string): TXQTerm;
var s: string;
begin
  s := StringReplace(copy(w,2,length(w)-2), w[1]+w[1], w[1], [rfReplaceAll]);
  if ((parsingModel in [xqpmXQuery1,xqpmXQuery3]) and (options.StringEntities = xqseDefault)) or (options.StringEntities = xqseResolveLikeXQuery) then
    s := replaceEntities(s);
  exit(TXQTermString.create(s));
end;

function TXQParsingContext.parseStringLiteral: string;
var
  temp: TXQTerm;
begin
  temp := parseValue;
  try
    if not (temp is TXQTermString) then raiseParsingError('XPST0003', 'Expected string');
    result := TXQTermString(temp).value;
  finally
    temp.free;
  end;
end;

function TXQParsingContext.parseXString(nullTerminatedString: boolean): TXQTerm;
  function functionIsConcat(nf: TXQTermNamedFunction): boolean;
  begin
    result := (nf.func <> nil) and (nf.func is TXQBasicFunctionInfo) and (TXQBasicFunctionInfo(nf.func).func = @xqFunctionConcat)
              and (equalNamespaces(nf.namespace, nil) or equalNamespaces(nf.namespace, XMLNamespace_XPathFunctions) or equalNamespaces(nf.namespace, XMLNamespace_MyExtensions));
  end;

  procedure pushTerm(t: TXQTerm);
  begin
    if not (t is TXQTermString) then
      t := TXQTermNamedFunction.create(XMLNamespace_MyExtensions, 'join', [t]);
    if (result = nil) and (t is TXQTermString) then
      result := t
    else if result = nil then
      result := TXQTermNamedFunction.create(XMLNamespace_XPathFunctions, 'concat', [t])
    else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result)) then
      result.push(t)
    else
     result := TXQTermNamedFunction.create(XMLNamespace_XPathFunctions, 'concat', [result, t]);
  end;

  procedure pushRaw(from, too: pchar);
  var
    v: String;
  begin
    if too < from then exit;
    v := strFromPchar(from, too - from + 1);
    if ((parsingModel in [xqpmXQuery1,xqpmXQuery3]) and (options.StringEntities = xqseDefault)) or (options.StringEntities = xqseResolveLikeXQuery) then
      v := replaceEntities(v);
    if result <> nil then begin
      if result is TXQTermString then TXQTermString(result).value += v
      else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result))
              and (TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)] is TXQTermString) then
        TXQTermString(TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)]).value += v
      else pushTerm(TXQTermString.create(v));
    end else pushTerm(TXQTermString.create(v));
  end;
var
  strsymb: Char;
  mark: PChar;
begin
  result := nil;
  if nullterminatedString then strsymb := #0
  else begin
    strsymb := pos^;
    pos+=1;
    if not (strsymb in ['''', '"']) then raiseParsingError('pxp:XPST0003', 'Expected string start');
  end;
  mark := pos;
  try
    while pos^ <> #0 do begin
      while (pos^ <> strsymb) and not (pos^ in [#0, '{', '}']) do pos+=1;
      pushRaw(mark, pos - 1);
      if pos^ = #0 then begin mark := pos; break; end;
      if pos^ = (pos+1)^ then begin //escaped like {{, '', "", }}
        pos+=1;
        mark := pos;
        pos+=1;
      end else case pos^ of
        '}': raiseParsingError('pxp:XPST0003',  'Single closing } not allowed in extended strings (use }})');
        '{': begin
          pos+=1;
          pushTerm(parsePrimaryLevel);
          expect('}');
          mark := pos;
        end;
        else begin //string closed
          expect(strsymb);
          break;
        end;
      end;
    end;
    if nullterminatedString then pushRaw(mark, pos - 1);
  except
    on EXQParsingException do begin result.free; raise; end;
  end;
  if result = nil then result := TXQTermString.create('');
end;

function TXQParsingContext.parseJSONLikeObjectConstructor: TXQTerm;
var
  token: String;
  jn: TXQNativeModule;
begin
  //expect('{'); parsed by caller
  if pos^ = '|' then begin
    expect('|');
    jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
    if jn = nil then raiseParsingError('pxp:JSONIQ', 'The {| .. |} syntax can only be used, if the json unit is loaded.');
    result := TXQTermNamedFunction.create(xqfkBasic, jn.findBasicFunction('object'));
    result := parseSequenceLike(result, '|');
    expect('}');
    exit;
  end;
  result := TXQTermJSONObjectConstructor.create();
  try
    skipWhitespaceAndComment();
    if pos^ = '}' then begin expect('}'); exit;end;
    repeat
      result.push(parse);
      expect(':');
      //if not (result.children[high(result.children)] is TXQTermString) then raiseParsingError('pxp:OBJ','Expected simple string, got: '+result.children[high(result.children)].ToString); //removed as json-iq allows variables there
      skipWhitespaceAndComment();
      result.push(parse);
      token := nextToken();
    until (token <> ',');
    if token <> '}' then raiseParsingError('pxp:OBJ', 'Expected "}" or ",", but got '+token);
  except
    FreeAndNil(result);
    raise
  end;
end;

function TXQParsingContext.parseJSONLikeArray: TXQTermJSONArray;
begin
  //expect('['); parsed by caller
  result := parseSequenceLike(TXQTermJSONArray.Create, ']') as TXQTermJSONArray;
end;

function staticallyCastQNameAndNotation(term: TXQTerm; typ: TXSType; staticContext: TXQStaticContext; castable: boolean = false): txqterm;
var
  name: String;
  namespace: INamespace;
  castAllowed: Boolean;
begin
  result := term;
  if typ.derivedFrom([baseSchema.QName, baseSchema.NOTATION]) then begin
    castAllowed := (typ <> baseSchema.NOTATION)
                   and (term.children[0] is TXQTermString)
                   and (baseSchema.QName.qnameRegex.Exec(trim(TXQTermString(term.children[0]).value)));
    if not castAllowed then exit;
    if not castable then begin
      name := trim(TXQTermString(term.children[0]).value);
      staticContext.splitRawQName(namespace, name, xqdnkElementType);
      result := TXQTermNumber.create(TXQValueQName.create(typ, namespace, name));
    end else
      result := TXQTermNumber.create(xqvalue(castAllowed));
    term.free;
  end;
end;



function TXQParsingContext.parseValue: TXQTerm;
var
  word: String;
  wordlookahead: String;
  temp: PChar;
  constr: Boolean;
begin
  result := nil;
  word:=nextToken();
  if word='' then exit();
  case word[1] of
    '''', '"':  exit(parseString(word));
    '$': begin
      if word <> '$' then raiseParsingError('XPST0003', 'Invalid characters after $');
      exit(parseVariable());
    end;
    '@': exit(TXQTermReadAttribute.create(nextTokenQName(true)));
    '+', '-': begin
      wordlookahead:=nextToken(true);
      if (word = '-') and
           ((wordlookahead[1] in ['0'..'9'])
             or ((length(wordlookahead) > 2) and (wordlookahead[1] = '.') and (wordlookahead[2] in ['0'..'9']))) then begin
        expect(wordlookahead);
        exit(TXQTermNumber.Create('-' + wordlookahead));
      end else
        exit(TXQTermBinaryOp.create(word, TXQTermNumber.create( '0'), parseValue()));
    end;

    '(': begin
      if pos^ = '#' then exit(parseExtension);
      exit(parseSequenceLike(TXQTermSequence.Create)); //only sequence or priority brackets
    end;

    '*': if pos^ <> ':' then exit(TXQTermNodeMatcher.Create(word))
         else begin
           expect(':');
           exit(TXQTermNodeMatcher.Create(word + ':' + nextTokenNCName()))
         end;
    '/': begin
      if pos^ = '/' then begin expect('/'); word+='/';end;
      skipWhitespaceAndComment();
      if (pos^ in [#0,',',')',']','}','=','|','+',';']) or ((pos^ = '<') and (parsingModel in [xqpmXPath2, xqpmXPath3])) then
        exit(TXQTermNodeMatcher.Create('/')) //leading lone slash (see standard#parse-note-leading-lone-slash)
      else exit(TXQTermBinaryOp.Create(word, TXQTermNodeMatcher.Create('/'), parseValue()));
    end;

    '0'..'9': exit(TXQTermNumber.Create(word));
    '.': if (word = '.') or (word = '..') then exit(TXQTermNodeMatcher.Create(word))
         else if word[2] in ['0'..'9', 'e', 'E'] then exit(TXQTermNumber.Create(word))
         else raiseParsingError('XPST0003', 'Unknown term: '+word);

    '<': begin
      requireXQuery('to use constructors (this error can also mean an invalid < )');
      result := parseDirectConstructor(word);
    end;

    '{': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected {. (Enable json extension, to create a json like object) ');
      exit(parseJSONLikeObjectConstructor);
    end;
    '[': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected [. (Enable json extension, to create a json like array) ');
      exit(parseJSONLikeArray());
    end


    else if (word = 'x') and (pos^ in ['"', '''']) then begin
      if not options.AllowExtendedStrings then raiseParsingError('err:XPST0003', 'Extended string syntax was disabled');
      exit(parseXString());
    end else try
      wordlookahead := nextToken(true);
      if wordlookahead='::' then begin
        word+=nextToken();
        word+=nextTokenQName(true);
        wordlookahead := nextToken(true);
      end else if (wordlookahead = ':') and (pos^ = ':') and not (((pos+1)^ in (SYMBOLS + START_SYMBOLS + WHITE_SPACE - ['*']))) then begin
        expect(':');
        if pos^ = '*' then word += ':' + nextToken() {will add :* }
        else word += ':' + nextTokenNCName();
        wordlookahead := nextToken(true);
      end;


      if wordlookahead = '(' then begin
        expect('(');
        if word = 'if' then begin
          result := TXQTermIf.Create();
          result.push(parsePrimaryLevel);
          expect(')'); expect('then');
          result.push(parse());
          expect('else');
          result.push(parse());
          exit;
        end;

        if word = 'typeswitch' then
          exit(parseTypeSwitch);
        if word = 'switch' then
          exit(parseSwitch);

        if (isKindTestFunction(word)
           or (strContains(word, '::') and (isKindTestFunction(strCopyFrom(word, strIndexOf(word, '::') + 2))))) then begin
          if strContains(word, 'attribute') then begin
            result := TXQTermReadAttribute.create(word, true);
            exit(parseSequenceLike(result));
          end else begin
            result := TXQTermNodeMatcher.Create(word, true);
            skipWhitespaceAndComment();
            if pos^ <> ')' then begin
              result.push(parseValue());   skipWhitespaceAndComment();
              if pos^ = ',' then begin
                if (word <> 'element') and (word <> 'attribute') then
                  raiseParsingError('XPST0003', 'Only one parameter is allowed to matching test '+word);
                expect(',');
                result.push(parseSequenceType());
              end;
            end;
            expect(')');
            exit;
          end;

        end else begin
          result := TXQTermNamedFunction.createIfExists(word, staticContext);
          if result = nil then raiseParsingError('XPST0003', 'unknown function: '+word);
          result := parseSequenceLike(result);

          if (TXQTermNamedFunction(result).kind = xqfkTypeConstructor) and (length(result.children) = 1) then
            result := staticallyCastQNameAndNotation(result, TXSType(TObject(TXQTermNamedFunction(result).func)), staticContext);
          exit;
        end;
      end;


      if (wordlookahead<>'') and (wordlookahead[1]='$') then begin
        if (word = 'for') or (word = 'let') then
          exit(parseFlower(word));

        if (word = 'some') or (word = 'every') then
          exit(parseSomeEvery(word));
      end;


      if strBeginsWith(word,'attribute::') then
        exit(TXQTermReadAttribute.Create(strcopyfrom(word,length('attribute::')+1)));



      if (word = 'element') or (word = 'attribute') or (word = 'document') or (word = 'text') or (word = 'processing-instruction') or (word = 'comment') then begin
        constr := wordlookahead = '{';
        if not constr then begin //look for name (this will allow something like text name {...} here, but that's going to raise an error later anyways)
          temp := pos;
          nextTokenMaybeQName();
          if nextToken() = '{' then constr := true;
          pos := temp;
        end;
        if constr then begin
          requireXQuery('to use constructors');
          exit(parseComputedConstructor(word));
        end;
      end;

      if wordlookahead = '{' then begin
        if (word = 'unordered') or (word = 'ordered') then begin //TODO: actually use that
          requireXQuery();
          expect('{'); result := parse(); expect('}');
          exit;
        end;
      end;

      if (word = 'validate') and ((wordlookahead = 'lax') or (wordlookahead = 'strict') or (wordlookahead = '{')) then begin
        requireXQuery('for schema validation');
        if wordlookahead <> '{' then
          expect(wordlookahead);
        expect('{');
        raiseParsingError('XPST0075', 'Schema validation is not supported');
      end;

      if (word = '') or (word[1] in [',', ';', ':', ')', ']', '}']) then //todo: check if valid xml node name
        raiseParsingError('XPST0003', 'Unexpected character: ' + word);
      result := TXQTermNodeMatcher.Create(word);
    except
      result.free;
      raise;
    end;
  end;
end;


type PXQTerm = ^TXQTerm;

//searchs the term to split
//e.g.
//          to                          to
//        1     +        * 4      =>  1     +
//            2    3                      2    3 <- * 4
//takes a pointer to a txqterm, since it sets the variable that should be changed
//(parentheses can be ignored since they are stored as sequence terms in the tree)
function ripBinOpApart(term: pxqterm; const prio: integer): PXQTerm;
begin
  if not (term^ is TXQTermBinaryOp) or (TXQTermBinaryOp(term^).op.priority >= prio) then result := term
  else result := ripBinOpApart(@term^.children[1], prio);
end;

function TXQParsingContext.parse: TXQTerm;
var astroot: TXQTerm;

  function parseSomething: TXQTerm;
  begin
    result := parse();
    if result = nil then raiseParsingError('XPST0003', 'Unexpected query end');
  end;


  procedure pushBinaryOp(const opinfo: TXQOperatorInfo);
  var res: TXQTermBinaryOp;
      replace: PXQTerm;
    procedure handleCastStrangeness;
    var
      st: TXQTermSequenceType;
    begin
      expect(res.op.followedBy);
      st := parseSequenceType();
      res.push(st); //assume we read instance of/cast/castable/treat as
      if ((res.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs)) then begin
        if not (st.isSingleType()) then
          raiseParsingError('XPST0003', 'Need single typ for cast');
        replace^ := staticallyCastQNameAndNotation(res, st.atomicTypeInfo, staticContext, res.op.func = @xqvalueCastableAs);
      end;
    end;

  begin
    expect(opinfo.name);
    if opinfo.require3 then require3();

    res := TXQTermBinaryOp.Create(opinfo);
    replace := ripBinOpApart(@astroot, res.op.priority);

    res.push(replace^);
    replace^ := res;

    if res.op.followedBy <> '' then handleCastStrangeness
    else begin
      res.push(parseValue());
      if res.children[high(res.children)] = nil then raiseParsingError('XPST0003', 'Unexpected query end');
    end;
  end;

  var word: string;
    replace: PXQTerm;

  procedure parseDotOperator;
  var prop: string;
  begin
    replace := ripBinOpApart(@astroot, 10000);
    if (replace^ is TXQTermFilterSequence) or (replace^ is TXQTermSequence) or (replace^ is TXQTermVariable)
       or (replace^ is TXQTermNamedFunction) or (replace^ is TXQTermJSONObjectConstructor) or (replace^ is TXQTermDynamicFunctionCall) then begin
         word := strCopyFrom(word, 2);
         if word <> '' then
           for prop in strSplit(word, '.') do begin
             if prop = '' then raiseParsingError('XPST0003', 'Unexpected ..');
             replace^ := TXQTermReadObjectProperty.Create(prop).push([replace^]);
           end;
         if (word = '') or (strEndsWith(word, '.')) then begin
           replace^ := TXQTermDynamicFunctionCall.Create(replace^, parseValue);
           if replace^.children[1] is TXQTermNodeMatcher then
             raiseParsingError('pxp:XPST0003', 'A node matching step is not allowed directly after a property dot operator');
         end;
     end else begin
       raiseParsingError('XPST0003', 'Unexpected .');
     end;
  end;
var
  op: TXQOperatorInfo;
begin
  astroot := parseValue();
  try
    while true do begin
      word := nextToken(true);
      case word of
        '', ',', ';', ':', ')', ']', '}', 'else', 'return', 'satisfies', 'for', 'let', 'order', 'where', 'stable', 'ascending', 'descending', 'empty', 'collation', 'case', 'default':
          exit(astroot);
        '[': begin
          expect('[');
          skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000);
          if pos^ <> ']' then replace^ := TXQTermFilterSequence.Create(replace^, parse())
          else                replace^ := TXQTermFilterSequence.Create(replace^); //stupid hack to allow $var [] :=
          expect(']');
        end;
        '(': begin
          expect('('); skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000); //TODO: check
          if pos^ <> ')' then
            replace^ := TXQTermDynamicFunctionCall.Create(replace^, parseSomething())
           else
            replace^ := TXQTermDynamicFunctionCall.Create(replace^);
          expect(')');
        end;
        ':=': begin
          expect(':=');
          result := astroot;
          if result is TXQTermNodeMatcher then begin
            result := TXQTermVariable.create(IfThen(TXQTermNodeMatcher(astroot).namespace = '', TXQTermNodeMatcher(astroot).select, TXQTermNodeMatcher(astroot).namespace + ':' + TXQTermNodeMatcher(astroot).select), staticContext);
            FreeAndNil(astroot);
            astroot := result; //only astroot should contain allocated objects that need to be freed in case of a subsequent parsing error
            if (options.AllowPropertyDotNotation = xqpdnAllowFullDotNotation) and (strContains(TXQTermVariable(astroot).value, '.')) then
              astroot := TXQTermVariable.splitForDotNotation(TXQTermVariable(astroot));
          end;
          result := TXQTermDefineVariable.Create(astroot, parseSomething());
          //staticContext.splitRawQName(TXQTermDefineVariable(result).namespace, TXQTermDefineVariable(result).variablename, xqdnkUnknown);
          exit;
        end;
        '|': if options.AllowJSON and ((pos+1)^ = '}') then exit(astroot) // {| .. |} object merging syntax
             else pushBinaryOp(TXQueryEngine.findOperator(pos)); //| operator
        else begin
          op := TXQueryEngine.findOperator(pos);
          if op <> nil then pushBinaryOp(op)
          else if (word[1] = '.') and (options.AllowPropertyDotNotation <> xqpdnDisallowDotNotation) then begin
            expect(word);
            parseDotOperator;
          end else
            raiseParsingError('XPST0003', 'Unknown or unexpected operator: '+word);
        end;
      end;
    end;
  except
    astroot.free;
    raise;
  end;
  result := astroot;
end;


{type


TParseFinalizer = class(TXQTerm_Visitor)
  sc: TXQStaticContext;
  function visit(var term: TXQTerm): TXQTerm_VisitAction; override;
end;
 function TParseFinalizer.visit(var term: TXQTerm): TXQTerm_VisitAction;
var
  bop: TXQTermBinaryOp;
  name: String;
  namespace: INamespace;
  nf: TXQTermNamedFunction;
begin
  if term is TXQTermBinaryOp then begin
    bop := TXQTermBinaryOp(term);
    if ((bop.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs))
       and (term.children[0] is TXQTermString)
       and ((term.children[1] as TXQTermSequenceType).atomicTypeInfo.derivedFrom([baseSchema.QName, baseSchema.NOTATION])) then begin
       name := TXQTermString(term.children[0]).value;
       sc.splitRawQName(namespace, name, xqdnkElementType);
       term := TXQTermNumber.create(TXQValueQName.create((term.children[1] as TXQTermSequenceType).atomicTypeInfo, ns, name));
       exit(xqtvaDeleteWithChildren);
    end;
  end else if term is TXQTermNamedFunction then begin
    nf := TXQTermNamedFunction(term);
    if (nf.kind = xqfkTypeConstructor)
       and (TXSType(nf.func).derivedFrom([baseSchema.QName, baseSchema.NOTATION]))
       and (length(nf.children) = 0) then begin

    end;
  end;
end;        }

function TXQParsingContext.parsePrimaryLevel: TXQTerm;
begin
  result := parse;
  try
    if nextToken(true) = ',' then begin
      result := TXQTermSequence.Create.push([result]);
      while nextToken(true) = ',' do begin
        expect(',');
        result.push(parse);
      end;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseModule(): TXQTerm;
  procedure requireModule;
  begin
    if result <> nil then exit;
    requireXQuery();
    result := TXQTermModule.Create;
  end;

  procedure importSchema; //has read import schema
  var
    prefix, url: String;
  begin
    prefix := '';
    url := nextToken();
    case url of
      'default': begin
        expect('element');
        expect('namespace');
        url := parseStringLiteral();
      end;
      'namespace': begin
        prefix := nextTokenNCName();
        expect('=');
        url := parseStringLiteral();
      end
      else begin
        if (url = '') or not (url[1] in ['''', '"']) then raiseParsingError('XPST0003', 'Invalid schema import');
        url := '';
        prefix:=':::'; //no prefix given
      end;
    end;
    if staticContext.importedSchemas = nil then staticContext.importedSchemas := TNamespaceList.Create;
    staticContext.importedSchemas.add(TNamespace.Create(XMLNamespaceURL_XMLSchema, prefix)); //treat all schemas as equivalent to the default schema
    if nextToken(true) = 'at' then begin
      //discard schema addresses
      parseStringLiteral;
      while nextToken(true) = ',' do begin expect(','); parseStringLiteral; end;
    end;
  end;
  procedure importModule; //has read import module
  var
    moduleName: String;
    moduleURL: String;
    at: array of string;
    module: TXQuery;
    nativeModule: TXQNativeModule;
  begin
    skipWhitespaceAndComment();
    moduleName := '';
    if pos^ = 'n' then begin
      expect('namespace'); moduleName:=nextTokenNCName(); expect('=');
    end;
    moduleURL := parseStringLiteral;
    at := nil;
    if nextToken(true) = 'at' then begin
      expect('at');
      arrayAdd(at, parseStringLiteral);
      while nextToken(true) = ',' do begin expect(','); arrayAdd(at, parseStringLiteral); end;
    end;

    module := engine.findModule(moduleURL);
    if module = nil then begin
      if assigned(engine.OnImportModule) then engine.onImportModule(engine, moduleURL, at);
      module := engine.findModule(moduleURL);
      if module = nil then begin
        nativeModule := engine.findNativeModule(moduleURL);
        if nativeModule = nil then raiseParsingError('XQST0059', 'Unknown module: '+moduleURL);
        if moduleName <> '' then begin
          if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
          staticContext.namespaces.add(TNamespace.create(nativeModule.namespace.getURL, moduleName));
        end;
        exit;
      end;
    end;
    if staticContext.importedModules = nil then
      staticContext.importedModules := TStringList.Create;
    if moduleName = '' then moduleName := module.staticContext.moduleNamespace.getPrefix;
    staticContext.importedModules.AddObject(moduleName, module);
  end;

  function parseQNameWithType(out typ: TXQTermSequenceType): string;
  begin
    result := nextTokenQName();
    if nextToken(true) = 'as' then begin
      expect('as');
      typ := parseSequenceType();
    end else typ := nil;
  end;

  procedure declareVariable;
  var vari: TXQTermDefineVariable;
    tempTyp: TXQTermSequenceType;
  begin
    requireModule;
    expect('$');
    vari := TXQTermDefineVariable.create(parseQNameWithType(tempTyp), nil);
    staticContext.splitRawQName((vari.variable as TXQTermVariable).namespace, (vari.variable as TXQTermVariable).value, xqdnkUnknown);
    result.push(vari);
    if tempTyp <> nil then vari.push(tempTyp);
    case nextToken() of
      ':=': vari.push(parse());
      'external': ;
      else raiseParsingError('XPST0003', 'Invalid variable declaration');
    end;

  end;

  procedure declareFunction;
  var
    funcTerm: TXQTermDefineFunction;
    tempType: TXQTermSequenceType;
    tempVar: TXQTermDefineVariable;
  begin
    funcTerm := TXQTermDefineFunction.create(nextTokenQName());
    result.push(funcTerm);
    staticContext.splitRawQName(funcTerm.namespace, funcTerm.funcname, xqdnkFunction);
    expect('(');
    skipWhitespaceAndComment();
    while nextToken(true) <> ')' do begin
      expect('$');
      tempVar := TXQTermDefineVariable.create(parseQNameWithType(tempType), nil);
      staticContext.splitRawQName((tempVar.variable as TXQTermVariable).namespace, (tempVar.variable as TXQTermVariable).value, xqdnkUnknown);
      if tempType <> nil then tempVar.push(tempType);
      funcTerm.push(tempVar);
      skipWhitespaceAndComment();
      if not (pos^ in [',', ')']) then raiseParsingError('XPST0003', 'Missing , or )');
      if pos^ = ',' then pos+=1;
    end;
    pos+=1;
    funcTerm.parameterCount:=length(funcTerm.children);
    if nextToken(true) = 'as' then begin
      expect('as');
      funcTerm.push(parseSequenceType());
    end;
    case nextToken() of
      '{': begin
        funcTerm.push(parsePrimaryLevel);
        expect('}');
      end;
      'external': ;
    end;
    if funcTerm.namespace = nil then raiseParsingError('XQST0060', 'No namespace for declared function: '+funcTerm.funcname);
    if (funcTerm.namespace.getURL = 'http://www.w3.org/XML/1998/namespace') or
       (funcTerm.namespace.getURL = 'http://www.w3.org/2001/XMLSchema') or
       (funcTerm.namespace.getURL = 'http://www.w3.org/2001/XMLSchema-instance') or
       (funcTerm.namespace.getURL = 'http://www.w3.org/2005/xpath-functions') then raiseParsingError('XQST0045', 'Invalid namespace for function declaration: '+funcTerm.namespace.getURL);
  end;



  procedure readBoolean(var b: boolean; const v: string);
  begin
    case v of
      'on':  b := true;
      'off': b := false;
      'toggle': b := not b;
      else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/toggle');
    end;
  end;

var
  token: String;
  nameSpaceName: String;
  nameSpaceURL: String;
  temp: String;
  namespace: INamespace;
begin
  result := nil;
  try
    token := nextToken(true);
    if token = 'xquery' then begin
      requireXQuery();
      expect(token); expect('version');
      if parseStringLiteral <> '1.0' then raiseParsingError('XQST0031', 'Invalid xquery version, need 1.0');
      token := nextToken(true);
      if token = 'encoding' then begin
        expect(token);
        encoding := strEncodingFromName(parseStringLiteral);
        expect(';');
        token := nextToken(true);
      end else if token = ';' then begin expect(token); token := nextToken(true); end
      else raiseParsingError('XPST0003', 'expected encoding or ;');
    end;

    if token = 'module' then begin
      requireModule;
      expect(token); expect('namespace');
      staticContext.moduleNamespace := TNamespace.create('', nextTokenNCName());
      expect('=');
      (staticContext.moduleNamespace as TNamespace).url := parseStringLiteral;
      expect(';');
      token := nextToken(true);
      if staticContext.importedModules = nil then staticContext.importedModules := TStringList.Create;
      staticContext.importedModules.AddObject(staticContext.moduleNamespace.getPrefix, resultquery); //every module import itself so it can lazy initialize its variables
      if staticContext.sender.AutomaticallyRegisterParsedModules and (resultquery <> nil) then
        staticContext.sender.FModules.Add(IXQuery(resultquery));
    end;

    while (token = 'declare') or (token = 'import') do begin
      requireModule;
      expect(token);
      if token = 'import' then begin
        case nextToken() of
          'schema': importSchema;
          'module': importModule;
        end;
      end else case nextToken() of //declare ...
        'boundary-space':
          case nextToken() of
            'preserve': staticContext.StripBoundarySpace:=false;
            'strip': staticContext.StripBoundarySpace:=true;
            else raiseParsingError('XPST0003', 'unknown boundary-space declaration');
          end;
        'default': begin
          token := nextToken();
          case token of
            'collation': staticContext.collation := staticContext.sender.getCollation(parseStringLiteral, staticContext.baseURI);
            'order': begin
              expect('empty');
              case nextToken() of
                'greatest': staticContext.emptyOrderSpec:=xqeoEmptyGreatest;
                'least': staticContext.emptyOrderSpec:=xqeoEmptyLeast;
              end;
            end;
            'element', 'function': begin
              expect('namespace');
              if token = 'element' then staticContext.defaultElementTypeNamespace:=TNamespace.Create(parseStringLiteral, '')
              else staticContext.defaultFunctionNamespace := TNamespace.Create(parseStringLiteral, '')
            end;
            else raiseParsingError('XPST0003', 'Unknown default value');
          end;
        end;
        'base-uri': staticContext.baseUri := parseStringLiteral;
        'construction':
          case nextToken() of
            'strip': staticContext.constructionPreserve := false;
            'preserve': staticContext.constructionPreserve := true
            else raiseParsingError('XPST0003', 'invalid construction declaration');
          end;
        'ordering':
          case nextToken() of
            'unordered': staticContext.ordering:=false;
            'ordered': staticContext.ordering:=true;
            else raiseParsingError('XPST0003', 'invalid ordering mode');
          end;
        'copy-namespaces': begin
           case nextToken() of
             'preserve': staticContext.copyNamespacePreserve:=true;
             'no-preserve': staticContext.copyNamespacePreserve:=false;
             else raiseParsingError('XPST0003', 'Invalid copy-namespace');
           end;
           expect(',');
           case nextToken() of
             'inherit': staticContext.copyNamespaceInherit:=true;
             'no-inherit': staticContext.copyNamespaceInherit:=false;
             else raiseParsingError('XPST0003', 'Invalid copy-namespace');
           end;
         end;
        'namespace': begin
           nameSpaceName := nextTokenNCName();
           expect('=');
           nameSpaceURL := parseStringLiteral;
           if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
           staticContext.namespaces.Add(TNamespace.create(nameSpaceURL, nameSpaceName));
        end;
        'variable': declareVariable();
        'function': declareFunction;
        'option': begin
          token := nextTokenQName();
          temp := parseStringLiteral;
          namespace := staticContext.findNamespace(strSplitGet(':', token), xqdnkUnknown);
          if namespace <> nil then begin
            if (namespace.getURL = XMLNamespaceURL_MyExtensions) then begin
              case token of
                'default-node-collation': staticContext.nodeCollation := staticContext.sender.getCollation(temp, staticContext.baseURI);
                'extended-strings': readBoolean(options.AllowExtendedStrings, temp);
                'json': readBoolean(options.AllowJSON, temp);
                'property-dot-notation': //readBoolean(AllowPropertyDotNotation, temp);
                  case temp of
                    'on':  options.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;
                    'off': options.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
                    'unambiguous': options.AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation;
                    'toggle': raiseParsingError('pxp:XPST0003', 'The "toggle" value has been removed for the property-dot-notation option.');
                    else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/unambiguous');
                  end;
                'strict-type-checking': readBoolean(staticContext.strictTypeChecking, temp);
                'use-local-namespaces': readBoolean(staticContext.useLocalNamespaces, temp);
                'pure-json-objects': readBoolean(staticContext.objectsRestrictedToJSONTypes, temp);
                'extended-json': readBoolean(staticContext.jsonPXPExtensions, temp);
                'string-entities':
                  case temp of
                    'off': options.StringEntities:=xqseIgnoreLikeXPath;
                    'on': options.StringEntities:=xqseResolveLikeXQuery;
                    'default': options.StringEntities:=xqseDefault;
                    else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/default');
                  end;
              end;
            end else if namespace.getURL = 'http://jsoniq.org/functions' then
              case token of
                'jsoniq-boolean-and-null-literals':
                  case temp of
                    'yes': options.AllowJSONLiterals:=true;
                    'no': options.AllowJSONLiterals:=false;
                    else raiseParsingError('XQST0013', 'Unknown option value: '+temp+' for '+token+' (allowed is yes/no)');
                  end;
              end;
          end;
        end;
        else raiseParsingError('XPST0003', 'Unknown declaration');
      end;
      token := nextToken(true);
      if token = ';' then expect(';');
      token := nextToken(true);
    end;
    if result <> nil then result.push(parsePrimaryLevel)
    else result := parsePrimaryLevel();
  except
    result.free;
    raise;
  end;
  if staticContext.sender.AutomaticallyRegisterParsedModules and (resultquery <> nil) and (result is TXQTermModule) and (staticContext.moduleNamespace <> nil)  then begin
    resultquery.fterm := result;
    resultquery.initializeStaticContext(staticContext.sender.getEvaluationContext(staticContext));
  end;

  if result <> nil then begin
    if options.AllowJSONLiterals then
      postprocessJSONLiterals(result);
  end;
end;

procedure TXQParsingContext.postProcessJSONLiterals(var t: TXQTerm);
var
  i: Integer;
begin
  if t = nil then exit;
  if (length(t.children) = 0) and (t is TXQTermNodeMatcher) and not (t as TXQTermNodeMatcher).hadNamespace then begin
    case (t as TXQTermNodeMatcher).select of
      'true': begin t.free; t := TXQTermNamedFunction.create(XMLNamespace_XPathFunctions, 'true'); end;
      'false': begin t.free; t := TXQTermNamedFunction.create(XMLNamespace_XPathFunctions, 'false'); end;
      'null': if GlobalStaticNamespaces.namespaces['jn'] <> nil then begin t.free; t := TXQTermNamedFunction.create(GlobalStaticNamespaces.namespaces['jn'], 'null'); end;
    end;
    exit;
  end;
  if t is TXQTermBinaryOp then begin
    if (TXQTermBinaryOp(t).op.name = '/') or (TXQTermBinaryOp(t).op.name = '//') then begin
      for i := 0 to high(t.children) do
        if not (t.children[i] is TXQTermNodeMatcher) then postProcessJSONLiterals(t.children[i]);
      exit;
    end;
  end;
  if t is TXQTermFlower then begin
    for i := 0 to high(TXQTermFlower(t).vars) do postProcessJSONLiterals(TXQTermFlower(t).vars[i].expr);
    for i := 0 to high(TXQTermFlower(t).orders) do postProcessJSONLiterals(TXQTermFlower(t).orders[i].expr);
    postProcessJSONLiterals(TXQTermFlower(t).returned);
    if TXQTermFlower(t).where <> nil then postProcessJSONLiterals(TXQTermFlower(t).where);
  end else if t is TXQTermDefineVariable then begin
    for i := 0 to high(t.children) do postProcessJSONLiterals(t.children[i]);
  end else if t <> nil then
    for i := 0 to high(t.children) do postProcessJSONLiterals(t.children[i]);
end;

