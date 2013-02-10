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

  AllowExtendedStrings: boolean;
  AllowObjects: boolean;
  parsingModel: TXQParsingModel;
  encoding: TEncoding;
  staticContext: TXQStaticContext;

  str: string;
  pos: pchar;
  resultquery: TXQuery;

  procedure raiseParsingError(errcode, s: string);
  procedure raiseInvalidModel(s: string);

  procedure requireXQuery(s: string = '');

  procedure skipWhitespace();
  procedure skipComment();
  procedure skipWhitespaceAndComment();
  procedure expect(c: char);
  procedure expect(s: string);
  function nextToken(lookahead: boolean=false): string;
  function nextTokenNCName(lookahead: boolean=false): string; inline;
  function nextTokenQName(lookahead: boolean=false): string; inline;


  function parseSequenceLike(target: TXQTerm; closingChar: char = ')'): TXQTerm;
  function parseQNameContinuation: string;
  function parseSequenceType(): TXQTermSequenceType;
  function replaceEntities(s: string): string;
  function parseString(const w: string): TXQTerm;
  function parseStringLiteral: string;
  function parseXString(nullTerminatedString: boolean = false): TXQTerm; //**< parses an extended string like @code(x"foo""bar"), @code(x"foo{$varref}ba{1+2+3}r")
  function parseJSONLikeObjectConstructor(): TXQTermJSONObjectConstructor; //**<

  function parseFlower(akind: string): TXQTermFlower;
  function parseSomeEvery(const akind: string): TXQTermSomeEvery;
  function parseDirectConstructor(name: string): TXQTermConstructor;
  function parseComputedConstructor(name: string): TXQTermConstructor;
  function parseExtension: TXQTerm;

  //**Parses the next complete value@br
  //**i.e. it will parse from the left, until it obtains a value that can be evaluated.
  //**e.g. from @code(1 + 2) it will parse @code(1) (since @code(1) is complete), from @code(function(1,2,3)) it will parse everything (since @code(function) can not be evaluated without its parameters))@br
  //**(observation: that kind of looks like it parses everything except binary operators)
  function parseValue: TXQTerm;        //left most token of an ExprSingle
  function parse: TXQTerm;             //ExprSingle
  function parsePrimaryLevel: TXQTerm; //Expr
  function parseModule(): TXQTerm;
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
  if parsingModel <> xqpmXQuery1 then raiseInvalidModel('XQuery is required '+s);
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
function TXQParsingContext.nextToken(lookahead: boolean=false): string;
const SYMBOLS = ['''','"', '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', '$'];
const START_SYMBOLS = ['-'];
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
  end else if (pos^ = '@') and ((pos+1)^='*') then begin
    pos+=2;
  end else if (pos^ = ':') and ((pos+1)^='=') then begin
    pos+=2;
  end else begin
    repeat
      pos+=1;
    until (pos^ in SYMBOLS + WHITE_SPACE + [#0]) or ((pos^ = ':') and ((pos+1)^='='));
  end;
  assert(start<pos);
  result:=strslice(start,pos-1);
  if lookahead then pos:=start;
end;

function TXQParsingContext.nextTokenNCName(lookahead: boolean): string;
begin
  result := nextToken(lookahead);
end;

function TXQParsingContext.nextTokenQName(lookahead: boolean): string;
begin
  result := nextToken(lookahead);
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

function TXQParsingContext.parseQNameContinuation: string;
begin
  if (pos^ = ':') and (((pos - 1)^ = '*') or ((pos + 1)^ = '*')) then result := nextToken()
  else if ((pos - 1)^ = ':') and (((pos)^ = '*') or ((pos - 1)^ = '*')) then result := nextToken()
  else result := '';
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
  word := nextToken();

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
    end else begin;
      staticContext.splitRawQName(namespace, word, xqdnkType);
      if (namespace = nil) then raiseParsingError('XPST0008', 'No schemas loaded for type '+word);
      if namespace.getURL <> XMLNamespaceURL_XMLSchema then raiseParsingError('XQST0009', 'Invalid schema loaded: '+namespace.getURL+' by '+word);
      result.kind:=tikAtomic;
      result.atomicTypeInfo := TXQueryEngine.findTypeClass(word);
      if result.atomicTypeInfo = nil then raiseParsingError('XPST0008', 'Unknown type: '+word);
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
    name := nextToken;
    if name <> '$' then raiseParsingError('XPST0003', 'Expected variable name');
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
      result.push(TXQTermVariable.Create(nextToken(), staticContext));
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
    result := TXQTermConstructor.create(tetProcessingInstruction, txqtermstring.create(nextToken()));
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
begin
  name := nextToken();
  if name = '!' then exit(parseCommentConstructor)
  else if name = '?' then exit(parsePIConstructor);
  result := TXQTermConstructor.create(tetOpen, TXQTermString.create(name));
  token := nextToken();
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
    token := nextToken();
  end;
  if token = '' then raiseParsingError('XPST0003', 'Attribute expected');
  if token = '/' then begin
    expect('>');
    exit;
  end;

  if Result.implicitNamespaces <> nil then begin
    if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
    for i := 0 to result.implicitNamespaces.Count - 1 do
      staticContext.namespaces.add(Result.implicitNamespaces.namespaces[i]);
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
          if nextToken <> name then raiseParsingError('XPST0003', 'Expected matching closing tag');
          expect('>');

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
    token := nextToken();
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
  pragma := nextToken();
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

function TXQParsingContext.replaceEntities(s: string): string;
var
  n, p: Integer;
  temp: string;
begin
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
        if temp[1] <> '#' then raiseParsingError('XPST0003', 'Invalid entity');
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
  if parsingModel in [xqpmXQuery1] then s := replaceEntities(s);
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

function TXQParsingContext.parseXString(nullterminatedString: boolean): TXQTerm;
  function functionIsConcat(nf: TXQTermNamedFunction): boolean;
  begin
    result := (nf.func <> nil) and (nf.func is TXQBasicFunctionInfo) and (TXQBasicFunctionInfo(nf.func).func = @xqFunctionConcat)
              and (equalNamespaces(nf.namespace, nil) or equalNamespaces(nf.namespace, XMLNamespace_XPathFunctions) or equalNamespaces(nf.namespace, XMLNamespace_MyExtensions));
  end;

  procedure pushTerm(t: TXQTerm);
  begin
    if result = nil then
      result := t
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
    if parsingModel in [xqpmXQuery1] then v := replaceEntities(v);
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
    if not (strsymb in ['''', '"']) then raiseParsingError('pxp:XPTS0003ext', 'Expected string start');
  end;
  mark := pos;
  while pos^ <> #0 do begin
    while (pos^ <> strsymb) and not (pos^ in [#0, '{', '}']) do pos+=1;
    pushRaw(mark, pos - 1);
    if pos^ = #0 then break;
    if pos^ = (pos+1)^ then begin //escaped like {{, '', "", }}
      pos+=1;
      mark := pos;
      pos+=1;
    end else case pos^ of
      '}': raiseParsingError('pxp:XPTS0003ext',  'Single closing } not allowed in extended strings (use }})');
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
  if result = nil then result := TXQTermString.create('');
end;

function TXQParsingContext.parseJSONLikeObjectConstructor: TXQTermJSONObjectConstructor;
  function parseJSONArray: TXQTerm;
  begin
    expect('[');
    result := parseSequenceLike(TXQTermSequence.Create, ']');
  end;

var
  token: String;
begin
  //expect('{'); parsed by caller
  result := TXQTermJSONObjectConstructor.create();
  try
    skipWhitespaceAndComment();
    if pos^ = '}' then begin expect('}'); exit;end;
    repeat
      result.push(parseValue);
      expect(':');
      if not (result.children[high(result.children)] is TXQTermString) then raiseParsingError('pxp:OBJ','Expected simple string, got: '+result.children[high(result.children)].ToString);
      skipWhitespaceAndComment();
      if pos^ = '[' then result.push(parseJSONArray)
      else result.push(parseValue);
      token := nextToken();
    until (token <> ',');
    if token <> '}' then raiseParsingError('pxp:OBJ', 'Expected "}" or ",", but got '+token);
  except
    FreeAndNil(result);
    raise
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
      exit(TXQTermVariable.Create(nextTokenQName(), staticContext));
    end;
    '@': exit(TXQTermReadAttribute.create(strcopyfrom(word,2) + parseQNameContinuation));
    '+', '-': begin
      wordlookahead:=nextToken(true);
      if (word = '-') and
           ((wordlookahead[1] in ['0'..'9'])
             or ((length(wordlookahead) > 2) and (wordlookahead[1] = '.') and (wordlookahead[2] in ['0'..'9']))) then begin
        expect(wordlookahead);
        exit(TXQTermNumber.Create('-' + wordlookahead));
      end else
        exit(TXQTermBinaryOp.create(word, TXQTermNumber.create('0'), parseValue()));
    end;

    '(': begin
      if pos^ = '#' then exit(parseExtension);
      exit(parseSequenceLike(TXQTermSequence.Create)); //only sequence or priority brackets
    end;

    '*': exit(TXQTermNodeMatcher.Create(word + parseQNameContinuation));
    '/': begin
      if pos^ = '/' then begin expect('/'); word+='/';end;
      skipWhitespaceAndComment();
      if pos^ in [#0,',',')',']','}','=','|','+',';'] then exit(TXQTermNodeMatcher.Create('/')) //leading lone slash (see standard#parse-note-leading-lone-slash)
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
      if not AllowObjects then raiseParsingError('XPST0003', 'Unexpected {. (Enable object extension, to create a json like object) ');
      exit(parseJSONLikeObjectConstructor);
    end


    else if (word = 'x') and (pos^ in ['"', '''']) then begin
      if not AllowExtendedStrings then raiseParsingError('pxp:XPTS0003ext', 'Extended string syntax was disabled');
      exit(parseXString());
    end else begin
      wordlookahead := nextToken(true);
      if wordlookahead='::' then word+=nextToken();
      if strEndsWith(word,'::') then begin
        word+=nextToken();
        wordlookahead := nextToken(true);
      end;
      if ((wordlookahead = '*') and strEndsWith(word, ':') and not strEndsWith(word, '::'))
         or (strEndsWith(word, '::*') and strBeginsWith(wordlookahead, ':')) then begin
        word += nextToken();
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

        if word = 'typeswitch' then begin
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

          exit;
        end;

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
              if pos^ = ',' then begin expect(','); result.push(parseSequenceType()); end;
            end;
            expect(')');
            exit;
          end;

        end else begin
          result := TXQTermNamedFunction.createIfExists(word, staticContext);
          if result = nil then raiseParsingError('XPST0003', 'unknown function: '+word);
          exit(parseSequenceLike(result));
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
          nextToken();
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

      result := TXQTermNodeMatcher.Create(word);
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


  procedure pushBinaryOp(const v: string);
  var res: TXQTermBinaryOp;
      replace: PXQTerm;
  begin
    expect(v);

    res := TXQTermBinaryOp.Create(v);
    replace := ripBinOpApart(@astroot, res.op.priority);

    res.push(replace^);
    replace^ := res;

    if res.op.followedBy <> '' then begin
      expect(res.op.followedBy);
      res.push(parseSequenceType()); //assume we read instance of/cast/castable/treat as
      if ((res.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs)) and (not (res.children[high(res.children)] as TXQTermSequenceType).isSingleType()) then
        raiseParsingError('XPST0003', 'Need single typ for cast');
    end else
      res.push(parseValue())
  end;

  var word: string;
    replace: PXQTerm;
begin
  astroot := parseValue();
  try
    while true do begin
      word := nextToken(true);
      case word of
        '', ',', ';', ')', ']', '}', 'else', 'return', 'satisfies', 'for', 'let', 'order', 'where', 'stable', 'ascending', 'descending', 'empty', 'collation', 'case', 'default':
          exit(astroot);
        //'=', '+', '-', '*': pushBinaryOp(word);
        '!': pushBinaryOp('!=');
        '<', '>': case (pos+1)^ of
          '<', '>': pushBinaryOp(word+word);
          '=': pushBinaryOp(word+'=');
          else pushBinaryOp(word);
        end;
        '/':
          if (pos+1)^ = '/' then
            pushBinaryOp('//')
          else pushBinaryOp('/');
        '[': begin
          expect('[');
          replace := ripBinOpApart(@astroot, 10000);
          replace^ := TXQTermFilterSequence.Create(replace^, parse());
          expect(']');
        end;
        ':=': begin
          expect(':=');
          result := TXQTermDefineVariable.Create(astroot, nil, parse());
          staticContext.splitRawQName(TXQTermDefineVariable(result).namespace, TXQTermDefineVariable(result).variablename, xqdnkUnknown);
          exit;
        end;
        else if TXQueryEngine.findOperator(word) <> nil then pushBinaryOp(word)
        else if (word[1] = '.') and AllowObjects then begin
          expect(word);
          replace := ripBinOpApart(@astroot, 10000);
          if (replace^ is TXQTermFilterSequence) or (replace^ is TXQTermSequence) or (replace^ is TXQTermVariable) or (replace^ is TXQTermNamedFunction) or (replace^ is TXQTermJSONObjectConstructor)  then
            replace^ := TXQTermReadObjectProperty.Create(strCopyFrom(word, 2)).push([replace^])
           else
            raiseParsingError('XPST0003', 'Unexpected .');
        end else
          raiseParsingError('XPST0003', 'Unknown or unexpected operator: '+word);
      end;
    end;
  except
    astroot.free;
    raise;
  end;
  result := astroot;
end;

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
  begin
    skipWhitespaceAndComment();
    moduleName := '';
    if pos^ = 'n' then begin
      expect('namespace'); moduleName:=nextTokenNCName(); expect('=');
    end;
    moduleURL := parseStringLiteral;
    at := nil;
    if nextToken(true) = 'at' then begin
      arrayAdd(at, parseStringLiteral);
      while nextToken(true) = ',' do begin expect(','); arrayAdd(at, parseStringLiteral); end;
    end;

    module := engine.findModule(moduleURL);
    if module = nil then begin
      engine.onImportModule(engine, moduleURL, at);
      module := engine.findModule(moduleURL);
      if module = nil then raiseParsingError('XQST0059', 'Unknown module: '+moduleURL);
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
    staticContext.splitRawQName(vari.namespace, vari.variablename, xqdnkUnknown);
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
      staticContext.splitRawQName(tempVar.namespace, tempVar.variablename, xqdnkUnknown);
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
      staticContext.moduleNamespace := TNamespace.create('', nextToken());
      if system.pos(':', staticContext.moduleNamespace.getPrefix) > 0 then raiseParsingError('XPST0003', 'Namespace id must not begin with a namespace prefix itself');
      expect('=');
      (staticContext.moduleNamespace as TNamespace).url := parseStringLiteral;
      expect(';');
      token := nextToken(true);
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
          if (namespace <> nil) and (namespace.getURL = XMLNamespaceURL_MyExtensions) then begin
            case token of
              'default-node-collation': staticContext.nodeCollation := staticContext.sender.getCollation(temp, staticContext.baseURI);
              'extended-strings': readBoolean(AllowExtendedStrings, temp);
              'objects': readBoolean(AllowObjects, temp);
              'strict-type-checking': readBoolean(staticContext.strictTypeChecking, temp);
              'use-local-namespaces': readBoolean(staticContext.useLocalNamespaces, temp);
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
end;

