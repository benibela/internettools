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

 TXQSequenceTypeFlag = (xqstAllowValidationTypes, xqstIsCast);
 TXQSequenceTypeFlags = set of TXQSequenceTypeFlag;
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
  procedure raiseSyntaxError(s: string);
  procedure raiseInvalidModel(s: string);

  procedure requireXQuery(s: string = '');
  procedure require3(s: string = '');
  procedure requireXQuery3(s: string = '');
  function isModel3: boolean;

  procedure skipWhitespace();
  procedure skipComment();
  procedure skipWhitespaceAndComment();
  procedure expect(c: char);
  procedure expect(s: string);
  function nextToken(lookahead: boolean=false): string;
  function nextTokenNCName(): string; inline; //returns a NCName
  // $foo       -> ('', '', 'foo',    xqnmPrefix)
  // $*:foo     -> ('', '*', 'foo',   xqnmNone)
  // $prf:foo   -> ('', 'prf', 'foo', xqnmPrefix)
  // $Q{}foo    -> ('', 'Q', 'foo',   xqnmURL)
  // $Q{ns}foo  -> ('ns', 'Q', 'foo', xqnmURL)
  function nextTokenEQName(out url, prefix, localpart: string; allowWildcards: boolean=false): TXQNamespaceMode; //returns a splitted EQName
  //like the previous function, but resolves the prefix.
  //Returns true iff there is a prefix or Q{..} url (i.e. in every case except $foo)
  function nextTokenEQName(out url, localpart: string; resolveKind: TXQDefaultNamespaceKind): boolean;


  function parseSequenceLike(target: TXQTermWithChildren; closingChar: char = ')'; allowPartialApplication: boolean = false): TXQTermWithChildren;
  function parseFunctionCall(target: TXQTermWithChildren): TXQTermWithChildren;
  function parseSequenceType(flags: TXQSequenceTypeFlags): TXQTermSequenceType;
  function parseSequenceTypeUnion(): TXQTermSequenceType;
  function parsePatternMatcher(): TXQTermPatternMatcher;
  function replaceEntitiesAlways(s: string): string;
  function replaceEntitiesIfNeeded(const s: string): string; inline;
  function parseString: string;
  function parseString(const w: string): string;
  function parseXString(nullTerminatedString: boolean = false): TXQTerm; //**< parses an extended string like @code(x"foo""bar"), @code(x"foo{$varref}ba{1+2+3}r")
  function parseJSONLikeObjectConstructor(): TXQTermWithChildren; //**< parses an json object constructor { "name": value, .. } or {| ... |}
  function parseJSONLikeArray(): TXQTermJSONArray;


  function parseFlower(akind: string): TXQTermFlower;
  function parseSomeEvery(const akind: string): TXQTermSomeEvery;
  function parseTypeSwitch: TXQTermTypeSwitch;
  function parseSwitch: TXQTermSwitch;
  function parseDirectConstructor(): TXQTermConstructor;
  function parseComputedConstructor(name: string): TXQTermConstructor;
  function parseExtension: TXQTerm;
  function parseVariable: TXQTermVariable;
  function parseDefineVariable: TXQTermDefineVariable;
  function parseAnnotations: TXQAnnotations;
  function parseFunctionDeclaration(annotations: TXQAnnotations; anonymous: boolean = false): TXQTermDefineFunction;
  function parseTryCatch: TXQTermTryCatch;

  //**Parses the next complete value@br
  //**i.e. it will parse from the left, until it obtains a value that can be evaluated.
  //**e.g. from @code(1 + 2) it will parse @code(1) (since @code(1) is complete), from @code(function(1,2,3)) it will parse everything (since @code(function) can not be evaluated without its parameters))@br
  //**(observation: that kind of looks like it parses everything except binary operators)
  function parseValue: TXQTerm;        //left most token of an ExprSingle
  function parse: TXQTerm;             //ExprSingle
  function parsePrimaryLevel: TXQTerm; //Expr
  function parseModule(): TXQTerm;
end;

 { TJSONLiteralReplaceVisitor }

 TJSONLiteralReplaceVisitor = class(TXQTerm_Visitor)
   function visit(t: PXQTerm): TXQTerm_VisitAction; override;
 end;


procedure TXQParsingContext.raiseParsingError(errcode, s: string);
begin
  if (pos < @str[1]) then pos := @str[1]; //make sure pos points to str
  if (pos > @str[length(str)]) then pos := @str[length(str)];
  raise EXQParsingException.Create(errcode, s+#13#10'in: '+strslice(@str[1],pos-1)+' [<- error occurs before here] '+strslice(pos,@str[length(str)]));
end;

procedure TXQParsingContext.raiseSyntaxError(s: string);
begin
  raiseParsingError('XPST0003', s);
end;

procedure TXQParsingContext.raiseInvalidModel(s: string);
begin
  raiseSyntaxError('This language feature is not available in the selected language. '+s);
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

function TXQParsingContext.isModel3: boolean;
begin
  result := (parsingModel = xqpmXPath3) or (parsingModel = xqpmXQuery3) ;
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
  raiseSyntaxError('Never ending comment')
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
      raiseSyntaxError('"'+c+'"'+' expected, but "'+nextToken()+'" found');
    pos+=1;
end;

procedure TXQParsingContext.expect(s: string);
var
   i: Integer;
begin
  skipWhitespaceAndComment;
  for i:=1 to length(s) do begin
    if pos^ <> s[i] then
      raiseSyntaxError('"'+ s+'" expected, but "'+nextToken()+'" found');
    pos+=1;
  end;
end;

//read the next token ('string', number: (-?[0-9]+|[0-9]*.[0-9]+|[0-9]+.[0-9]*)([eE][+-]?[0-9]+)?, symbol, identifier)
const SYMBOLS = ['''','"', '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', ':', '@', '$', '%'];
const START_SYMBOLS = ['-'];
function TXQParsingContext.nextToken(lookahead: boolean=false): string;
var start:pchar;
   numberE, numberPoint: boolean;
   tempOp: TXQOperatorInfo;
begin
  skipWhitespaceAndComment;
  if pos^ = #0 then exit('');
  start:=pos;
  case pos^ of
    '''', '"': begin
      repeat
        pos+=1;
        if (pos^ = start^) then
          if ((pos+1)^ <> start^) then break
          else pos+=1;
      until pos^ in [#0];
      if pos^ = #0 then raiseSyntaxError('Unclosed string');
      pos+=1;
    end;
    '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', '@', '$', '%', '-': begin//SYMBOLS+START_SYMBOLS - [:-]
      tempOp := TXQueryEngine.findOperator(pos);
      if tempOp <> nil then result := tempOp.name
      else result := pos^;

      if lookahead then exit(result)
      else begin
        pos+=length(result);
        exit(result);
      end;
    end;
    ':': begin
      inc(pos);
      if pos^ in [':','='] then inc(pos);
    end;
    '0'..'9','.': begin
      numberPoint:=pos^ = '.';
      if numberPoint and not ((pos+1)^ in ['0'..'9']) then begin
        pos += 1; //not a number
        if pos^ = '.' then pos += 1; //..
      end else begin
        numberE:=false;
        repeat
          pos+=1;
          if pos^ = '.' then begin
            if numberPoint then raiseSyntaxError('Double . in number');
            numberPoint:=true;
            pos+=1;
          end;
          if pos^ in ['e','E'] then begin
            if numberE then raiseSyntaxError('Double e in number');
            pos+=1;
            numberE:=true;
            if not (pos^ in ['0'..'9','+','-']) then raiseSyntaxError('Invalid character after e in number')
            else pos+=1;
          end;
        until not (pos^ in ['0'..'9']);
        if (pos^ in ['a'..'z','A'..'Z']) then raiseSyntaxError('Space needed between number and non-symbolic operator');
      end;
    end;
    else begin
      repeat
        pos+=1;
      until (pos^ in SYMBOLS + WHITE_SPACE + [#0]) {deprecated? or ((pos^ = ':') and ((pos+1)^='='))};
    end;
  end;
  assert(start<pos);
  result:=strslice(start,pos-1);
  if lookahead then pos:=start;
end;

function TXQParsingContext.nextTokenNCName(): string;
begin
  result := nextToken(false);
  if not baseSchema.isValidNCName(result) then
    raiseSyntaxError('Invalid NCName: '''+result+'''');
end;

function TXQParsingContext.nextTokenEQName(out url, prefix, localpart: string; allowWildcards: boolean): TXQNamespaceMode;
const NONCNAME = (SYMBOLS + START_SYMBOLS + WHITE_SPACE - ['*']);
var
  marker: PChar;
begin
  skipWhitespaceAndComment();
  if pos^ <> '*' then localpart:=nextTokenNCName()
  else localpart := nextToken();
  result := xqnmPrefix;
  if (localpart = 'Q') and (pos^ = '{') then begin
    if ((pos-1)^ <> 'Q') then raiseSyntaxError('Q{ must not be separated by whitespace');
    require3('Q{..} namespace urls');
    prefix := localpart;
    inc(pos);
    marker := pos;
    while not (pos^ in ['}', #0]) do inc(pos);
    url := strFromPchar(marker, pos - marker);
    url := xmlStrWhitespaceCollapse(replaceEntitiesIfNeeded(url));
    inc(pos);
    if pos^ = '*' then begin
      inc(pos);
      localpart := '*';
    end else begin
      if (pos^ in WHITE_SPACE + SYMBOLS - ['*']) then raiseParsingError('err:XPST0003', 'Q{..}localname must not be separated by whitespace');
      localpart := nextTokenNCName();
    end;
    result := xqnmURL;
  end else if (pos^ = ':') and not ((pos+1)^ in NONCNAME) then begin //same check in parseValue for matchers
    expect(':');
    prefix := localpart;
    if pos^ <> '*' then localpart := nextTokenNCName()
    else localpart := nextToken();
    if prefix = '*' then
      result := xqnmNone;
  end else begin
    url := '';
    if allowWildcards and (localpart = '*') then result := xqnmNone;
  end;
  if (not allowWildcards) and ((result = xqnmNone) or (localpart = '*')) then raiseParsingError('XPST0003', 'Expected QName, got wildcards: '+prefix+':'+localpart);
end;

function TXQParsingContext.nextTokenEQName(out url, localpart: string; resolveKind: TXQDefaultNamespaceKind): boolean;
var
  prefix: string;
begin
  if nextTokenEQName(url, prefix, localpart) = xqnmPrefix then begin
    url := staticContext.findNamespaceURL(prefix, resolveKind);
    result := prefix <> '';
  end else result := true;
end;

function TXQParsingContext.parseSequenceLike(target: TXQTermWithChildren; closingChar: char; allowPartialApplication: boolean
  ): TXQTermWithChildren;
var partialApplications: integer;
  procedure nextValue;
  begin
    if allowPartialApplication then begin;
      skipWhitespaceAndComment();
      if pos^ = '?' then begin
        inc(pos);
        inc(partialApplications);
        result.push(TXQTermVariable.create(inttostr(partialApplications)+'.', XMLNamespace_MyExtensions));
        exit;
      end;
    end;
    result.push(parse());
  end;

var
  t: String;
  df: TXQTermDefineFunction;
begin
  partialApplications := 0;
  result := target;
  skipWhitespaceAndComment();
  if pos^ = closingChar then begin expect(closingChar); exit(); end;
  nextValue;
  t := nextToken();
  while t = ',' do begin
    nextValue;
    t := nextToken();
  end;
  if t <> closingChar then raiseParsingError('XPST0003', 'Expected closing parenthesis: '+ closingChar);
  if partialApplications > 0 then begin
    df := TXQTermDefineFunction.create;
    if result is TXQTermNamedFunction then df.kind := xqtdfStaticPartialApplication
    else df.kind := xqtdfDynamicPartialApplication;
    df.parameterCount := partialApplications;
    {for i := 1 to partialApplications do
      df.push(TXQTermDefineVariable.create(inttostr(i)+'.', XMLNamespace_MyExtensions));}
    df.push(result);
    result := df;
  end;
end;

function TXQParsingContext.parseFunctionCall(target: TXQTermWithChildren): TXQTermWithChildren;
begin
  result := parseSequenceLike(target, ')', parsingModel in [xqpmXPath3, xqpmXQuery3]);
end;

function isKindTestFunction(const word: string): boolean;  //Lookahead to recognize KindTest of the XPath-EBNF
begin
  result := (word = 'text') or (word = 'node') or (word = 'comment') or (word = 'processing-instruction')
            or (word = 'element') or (word = 'document-node') or (word = 'schema-element') or (word = 'attribute') or (word = 'schema-attribute');
end;

function TXQParsingContext.parseSequenceType(flags: TXQSequenceTypeFlags): TXQTermSequenceType;
var word: string;
  parens: Integer;
  namespaceUrl: string;
  hadNamespace: Boolean;
  schema: TXSSchema;
begin
  skipWhitespaceAndComment();
  parens := 0;
  while pos^ = '(' do begin
    inc(pos);
    skipWhitespaceAndComment();
    parens += 1;
  end;
  if parens > 0 then require3('for parentheses around types');
  if pos^ <> '%' then hadNamespace := nextTokenEQName(namespaceUrl, word, xqdnkType)
  else begin
    word := '%';
    inc(pos)
  end;


  result := TXQTermSequenceType.Create();
  try
    result.allowNone:=false;
    result.allowMultiple:=false;

    result.name:=word;
    if (not hadNamespace) and ((isKindTestFunction(word)) or (word = 'empty-sequence') or (word = 'item')) then begin
      expect('(');

      skipWhitespaceAndComment();
      if pos^ = ')' then expect(')')
      else begin
        result.push(parse());
        if nextToken() = ',' then begin
          result.push(parseSequenceType([xqstAllowValidationTypes]));
          expect(')');
        end;
      end;

      if (word = 'empty-sequence') then begin
        result.kind:=tikNone;
        if (length(result.children) <> 0) or (parens > 0) then raiseParsingError('XPST0003', 'invalid sequence type');
        exit
      end else if word = 'item' then begin
        result.kind:=tikAny;
        if length(result.children) <> 0 then raiseParsingError('XPST0003', 'invalid sequence type');
      end else begin
         result.kind:=tikElementTest;
         result.nodeMatching := convertElementTestToPathMatchingStep(word, result.children);
      end;
    end else if options.AllowJSON and (not hadNamespace) and ((word = 'array') or (word = 'object') or (word = 'json-item') or (word = 'structured-item')) then begin
      expect('('); expect(')');
      case word of
        'json-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.jsonItem; end;
        'structured-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseSchema.structuredItem; end;
        'array': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.array_; end;
        'object': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.object_; end;
        else raiseParsingError('XPST0003', 'WTF??');
      end;
    end else if (not hadNamespace) and (word = 'function') or (word = '%') then begin
      require3('function test');
      if word = '%' then begin
        parseAnnotations; //we do not need this nonsense
        expect('function');
      end;
      result.kind:=tikFunctionTest;
      expect('(');
      skipWhitespaceAndComment();
      if pos^ = '*' then begin
        expect('*');
        expect(')');
      end else begin
        while pos^ <> ')' do begin
          SetLength(result.arguments, length(result.arguments) + 1);
          result.arguments[high(result.arguments)] := parseSequenceType([]);
          skipWhitespaceAndComment();
          if pos^ <> ')' then expect(',');
        end;
        expect(')');
        expect('as');
        SetLength(result.arguments, length(result.arguments) + 1);
        result.arguments[high(result.arguments)] := parseSequenceType([]);
      end;
    end else begin
      result.kind:=tikAtomic;
      schema := staticContext.findSchema(namespaceUrl);
      if schema <> nil then result.atomicTypeInfo := schema.findType(word)
      else if hadNamespace then raiseParsingError('XPST0081', 'Unknown schema: '+namespaceUrl)
      else result.atomicTypeInfo := nil;
      if (result.atomicTypeInfo = nil)
         or (not (xqstAllowValidationTypes in flags) and baseSchema.isValidationOnlyType(result.atomicTypeInfo)) then
        raiseParsingError(ifthen((xqstIsCast in flags) and isModel3 , 'XQST0052', 'XPST0051'), 'Unknown type: Q{'+namespaceUrl+'}'+word);
    end;

    while parens > 0 do begin expect(')'); parens -= 1; end;

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

function TXQParsingContext.parseSequenceTypeUnion: TXQTermSequenceType;
var
  temp: TXQTermSequenceType;
begin
  result := parseSequenceType([]);
  if isModel3 and (nextToken(true) = '|') then begin
    temp := result;
    result := TXQTermSequenceType.create();
    try
      result.kind := tikUnion;
      result.push(temp);
      while nextToken(true) = '|' do begin
        expect('|');
        result.push(parseSequenceType([]));
      end;
    except
      result.free;
      raise;
    end;
  end;
end;

function TXQParsingContext.parsePatternMatcher: TXQTermPatternMatcher;
var
  curpos: PChar;
  temp: TXQTermConstructor;
begin
  if patternMatcherParse = nil then raiseParsingError('pxp:PATT0', 'No pattern matcher loaded ()');
  curpos := pos;
  expect('<');
  temp := parseDirectConstructor;
  temp.free;
  case pos^ of
    '+', '*', '?': inc(pos);
    '{': begin
      while not (pos^ in ['}', #0]) do inc(pos);
      expect('}');
    end;
  end;
  //result := TXQTermTemporaryNode.create();
  //result.node := (patternMatcherParse(strFromPchar(curpos, pos-curpos)));
  Result := patternMatcherParse(staticContext, strFromPchar(curpos, pos-curpos));
end;

function TXQParsingContext.parseFlower(akind: string): TXQTermFlower;

var usedVariables: TfpList;
  function parseFlowerVariable: TXQTermVariable;
  begin
    result := parseVariable;
    usedVariables.add(result);
  end;

var token: String;
  procedure parseInitialClauseAndNextToken;
    procedure parseWindow;
      procedure parseWindowVars(out vars: TXQTermFlowerWindowVarsAndCondition);
      begin
        skipWhitespaceAndComment();
        if pos^ = '$' then vars.currentItem := parseFlowerVariable;
        if nextToken(true) = 'at' then begin
          expect('at');
          vars.positionVar := parseFlowerVariable;
        end;
        if nextToken(true) = 'previous' then begin
          expect('previous');
          vars.previousItem := parseFlowerVariable;
        end;
        if nextToken(true) = 'next' then begin
          expect('next');
          vars.nextItem := parseFlowerVariable;
        end;
        expect('when');
        vars.when := parse;
      end;

    var flags: TXQTermFlowerWindowFlags;
      window: TXQTermFlowerWindow;
      duplicate: TXQTermVariable;
    begin
      token := nextToken();
      case token of
        'tumbling': flags := [];
        'sliding': flags := [xqtfwSliding];
        else raiseSyntaxError('Expected variable, sliding/tumbling window or pattern in flowr expression, but got: '+token);
      end;
      requireXQuery3();
      expect('window');
      window := TXQTermFlowerWindow.Create;
      result.push(window);
      window.flags := flags;
      window.loopvar := parseFlowerVariable;
      if nextToken(true) = 'as' then begin
        expect('as');
        window.sequenceTyp := parseSequenceType([]);
      end;
      expect('in');
      window.expr := parse();
      expect('start');
      parseWindowVars(window.startCondition);
      if nextToken(true) = 'only' then begin
        expect('only');
        Include(window.flags, xqtfwEndOnlyWhen);
      end;
      if ( xqtfwSliding in window.flags ) or (nextToken(true) = 'end')  then begin
        expect('end');
        parseWindowVars(window.endCondition);
      end;
      duplicate := window.findDuplicatedVariable;
      if duplicate <> nil then raiseParsingError('XQST0103', 'Duplicated variable name: '+duplicate.ToString);
    end;

  var temp: string;
    clause: TXQTermFlowerLet;
    patternclause: TXQTermFlowerLetPattern;
    isfor: boolean;
  begin
    case token of
      'let': begin
         isfor:=false;
         if (parsingModel = xqpmXPath2) then raiseInvalidModel('let is not supported in XPath 2.0');
      end;
      'for': isfor:=true;
      else raiseParsingError('XPST0003', 'Invalid flower: '+token);
    end;
    skipWhitespaceAndComment();
    if pos^ in ['s', 't'] then begin
      parseWindow;
      token := nextToken();
      exit;
    end;
    repeat
      if pos^ <> '<' then begin
        if isfor then result.push(TXQTermFlowerFor.Create)
        else result.push(TXQTermFlowerLet.Create);
        clause := TXQTermFlowerLet(result.children[high(result.children)]);
        with clause do begin
          loopvar := parseFlowerVariable;

          temp := nextToken;
          if temp = 'as' then begin
            requireXQuery;
            sequenceTyp := parseSequenceType([]);
            temp := nextToken;
          end else sequenceTyp := nil;

          if kind = xqtfcFor then begin

            if temp = 'allowing' then begin
              requireXQuery3;
              expect('empty');
              TXQTermFlowerFor(clause).allowingEmpty := true;
              temp := nextToken;
            end;

            if temp = 'at' then begin
              requireXQuery;
              TXQTermFlowerFor(clause).positionvar := parseFlowerVariable;
              if loopvar.equalsVariable(TXQTermFlowerFor(clause).positionvar) then raiseEvaluationError('XQST0089', 'Duplicated variable name');
              temp := nextToken;
            end;

            if temp <> 'in' then raiseSyntaxError('Expected "in".')
          end else
            if temp <> ':=' then raiseSyntaxError('Expected ":=".');

          expr := parse();
        end;
      end else begin
        if isfor then patternclause := TXQTermFlowerForPattern.Create
        else patternclause := TXQTermFlowerLetPattern.Create;
        result.push(patternclause);
        with patternclause do begin
          patternclause.pattern := parsePatternMatcher;
          temp := nextToken;
          if temp = 'as' then begin
            requireXQuery;
            sequenceTyp := parseSequenceType([]);
            temp := nextToken;
          end else sequenceTyp := nil;
          case kind of
            xqtfcLetPattern: if temp <> ':=' then raiseSyntaxError('Expected ":=".');
            xqtfcForPattern: if temp <> 'in' then raiseSyntaxError('Expected "in".');
          end;
          expr := parse();
        end;
      end;
      token := nextToken();
      skipWhitespaceAndComment();
    until token <> ',';
  end;

  procedure parseGroupClauseAndNextToken;
  var
    group: TXQTermFlowerGroup;

    procedure parseSpec;
    var
      let: TXQTermFlowerLet;
      i: Integer;
    begin
      SetLength(group.vars, length(group.vars) + 1);
      SetLength(group.seqtypes, length(group.seqtypes) + 1);
      group.vars[high(group.vars)] := parseVariable;
      skipWhitespaceAndComment();
      if pos^ in ['a', ':'] then begin
        let := TXQTermFlowerLet.Create;
        let.loopvar := TXQTermVariable(group.vars[high(group.vars)].clone);
        if pos^ = 'a' then begin
          expect('as');
          //let.sequenceTyp := parseSequenceType([]);
          group.seqtypes[high(group.seqtypes)] := parseSequenceType([]); //need to test the atomized value, let would test the original
        end;
        expect(':=');
        let.expr := parse;
        SetLength(result.children, length(result.children) + 1);
        result.children[high(result.children) - 1] := let;
        result.children[high(result.children)] := group;
      end else with group.vars[high(group.vars)] do begin
        for i := 0 to usedVariables.Count - 1 do
          if equalsVariable(TXQTermVariable(usedVariables[i])) then exit;
        raiseParsingError('XQST0094', 'Variable unknown');
      end;
    end;

  begin
    expect('by');

    group := TXQTermFlowerGroup.Create;
    result.push(group);
    parseSpec;
    while pos^ = ',' do begin
      inc(pos);
      parseSpec;
    end;
    token := nextToken();
    if token = 'collation' then begin
      group.collation := parseString;
      token := nextToken();
    end;
  end;

var
  hadOrder: boolean;
  hadWhere: Boolean;
  procedure parseOrderClauseAndNextToken;
  var
    clause: TXQTermFlowerOrder;
    temp: TXQTerm;
    i, j: Integer;
  begin
    if token = 'stable' then expect('order'); //always be stable
    expect('by');
    i := length(Result.children);
    repeat
      clause := TXQTermFlowerOrder.Create;
      result.push(clause);
      with clause do begin
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
          else raiseSyntaxError('Expected "greatest" or "least"');
          token := nextToken;
        end;

        if token = 'collation' then begin
          collation := nextToken;
          if (collation = '') or not (collation[1] in ['''', '"']) or (collation[1] <> collation[length(collation)]) then raiseParsingError('XPST0003', 'Invalid collation');
          collation := copy(collation, 2, length(collation) - 2);
          token := nextToken;
        end
      end;
    until token <> ',';
    //reverse order sub clauses
    j := high(result.children);
    while j > i do begin
      temp := result.children[i];
      result.children[i] := result.children[j] ;
      result.children[j] := temp;
      inc(i); dec(j);
    end;
  end;
begin
  result := TXQTermFlower.Create;
  usedVariables := tfplist.Create;
  hadOrder := false;
  hadWhere := false;
  try
    token := akind;
    while token <> 'return' do begin
      case token of
        'let', 'for': begin
          if Length(Result.children) > 0 then requireXQuery();
          parseInitialClauseAndNextToken;
        end;
        'where': begin
          if hadOrder or hadWhere then requireXQuery3()
          else requireXQuery();
          hadWhere := true;
          result.push(TXQTermFlowerWhere.Create);
          TXQTermFlowerWhere(result.children[high(result.children)]).test := parse;
          token := nextToken();
        end;
        'stable', 'order': begin
          if hadOrder then requireXQuery3()
          else requireXQuery();
          hadOrder := true;
          parseOrderClauseAndNextToken;
        end;
        'count': begin
          requireXQuery3();
          result.push(TXQTermFlowerCount.Create);
          TXQTermFlowerCount(result.children[high(result.children)]).countvar := parseFlowerVariable;

          token := nextToken();
        end;
        'group': begin
          requireXQuery3();
          parseGroupClauseAndNextToken;
        end;
        'return': exit;
        else raiseSyntaxError('Expected return ');
      end;
    end;
    result.push(parse);
  except
    on EXQParsingException do begin result.free; usedVariables.free; raise; end;
  end;
  usedVariables.free;
end;

function TXQParsingContext.parseSomeEvery(const akind: string): TXQTermSomeEvery;
var
  word: String;
begin
  result := TXQTermSomeEvery.Create(akind = 'every');
  try
    result.push(parseVariable);
    skipWhitespaceAndComment();
    if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType([])); end;
    expect('in'); result.push(parse());
    word := nextToken();
    while word = ',' do begin
      result.push(parseVariable);
      skipWhitespaceAndComment();
      if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType([])); end;
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
  tempSeq: TXQTermSequence;
begin
  requireXQuery('for typeswitch statement');
  result := TXQTermTypeSwitch.Create;
  try
    result.push(parsePrimaryLevel());
    expect(')');

    word := nextToken();
    while word = 'case' do begin
      skipWhitespaceAndComment();
      tempSeq := TXQTermSequence.Create;
      result.push(tempSeq);
      if pos^ = '<' then begin
        tempSeq.push(parsePatternMatcher());
      end else begin
        if pos^ = '$' then begin tempSeq.push(parseVariable); expect('as'); end;
        tempSeq.push(parseSequenceTypeUnion());
      end;
      expect('return');
      tempSeq.push(parse());
      word := nextToken();
    end;
    if word <> 'default' then raiseParsingError('XPST0003', 'expected "default" clause');
    skipWhitespaceAndComment();
    tempSeq := TXQTermSequence.Create;
    result.push(tempSeq);
    if pos^ = '$' then tempSeq.push(parseVariable);
    expect('return');
    tempSeq.push(parse());
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseSwitch: TXQTermSwitch;
var
  word: String;
  tempSeq: TXQTermSequence;
begin
  requireXQuery3('for switch statement');
  result := TXQTermSwitch.Create;
  result.push(parsePrimaryLevel());
  expect(')');

  word := nextToken();
  if word <> 'case' then raiseSyntaxError('Need at least one case');
  while word = 'case' do begin
    skipWhitespaceAndComment();
    tempSeq := TXQTermSequence.Create;
    result.push(tempSeq);
    while word = 'case' do begin
      tempSeq.push(parse());
      word := nextToken();
    end;
    if word <> 'return' then raiseSyntaxError('expected return');
    tempSeq.push(parse());
    word := nextToken();
  end;
  if word <> 'default' then raiseSyntaxError('expected "default" clause');
  expect('return');
  result.push(parse());
end;

function TXQParsingContext.parseDirectConstructor(): TXQTermConstructor;
  procedure parseCommonContent(parent: TXQTermConstructor; border: char; atBoundary: boolean; mustBeLiteral: boolean = false);
    procedure pushElementContent(s: string);
    begin
      if length(s) = 0 then exit;
      if (length(parent.children) > 0)
         and (parent.children[high(parent.children)] is TXQTermConstant)
         and (TXQTermConstant(parent.children[high(parent.children)]).value is TXQValueString) then
        (TXQTermConstant(parent.children[high(parent.children)]).value as TXQValueString).str += s
      else
        parent.push(TXQTermConstant.create(xqvalue(s)));
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
      while not (pos^ in [#0, '{', '}', '&', '<']) and (pos^ <> border) and ((border = '<') or not (pos^ in [#9, #10, #13])) do pos+=1;

      if pos^ in [#9, #10, #13] then begin //whitespace normalization #9, #10, #13 => space
        pushElementContent(strFromPchar(marker, pos - marker));
        if ((pos-1)^ <> #13) or (pos^ <> #10) then
          pushElementContent(' ');
        pos+=1;
        continue;
      end;

      if pos^ = #0 then raiseSyntaxError('Unexpected end');

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
          if mustBeLiteral then raiseParsingError('XQST0022', 'Enclosed expression not allowed') ;
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
          if (pos+1)^ <> '}' then raiseSyntaxError('Invalid closing parenthesis');
          pushElementContent('}');
          atBoundary:=false;
          pos += 2;
        end;
        '&': begin
          marker := pos;
          while not (pos^ in [#0, ';']) do pos += 1;
          if pos^ = #0 then raiseSyntaxError('Unexpected end');
          pos += 1;
          pushElementContent(replaceEntitiesAlways(strFromPchar(marker, pos - marker)));
          atBoundary:=false;
        end;
        '<': raiseSyntaxError('Unexpected <');
      end;
    end;
  end;

  function parsePIConstructor: TXQTermConstructor;
  var marker: PChar;
    name: String;
  begin
    expect('?');
    if pos^ in WHITE_SPACE then raiseSyntaxError('Unexpected space');
    name := nextTokenNCName();
    if striEqual(name, 'XML') or not (pos^ in (WHITE_SPACE + ['?'])) then raiseSyntaxError('Invalid PI name');
    result := TXQTermConstructor.create(tetProcessingInstruction, TXQTermConstant.create(xqvalue(name)));
    skipWhitespace();
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '?') or ((pos+1)^ <> '>')) do pos+=1;
    result.push(TXQTermConstant.create(xqvalue(strFromPchar(marker, pos - marker))));
    if pos^ = #0 then raiseSyntaxError('Unexpected end');
    expect('?>');
  end;

  function parseCommentConstructor: TXQTermConstructor;
  var marker: PChar;
  begin
    expect('!');
    expect('--');
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '-') or ((pos+1)^ <> '-')) do pos+=1;
    if pos^ = #0 then raiseSyntaxError('Unexpected end');
    result := TXQTermConstructor.create(tetComment, TXQTermConstant.create(strFromPchar(marker, pos - marker)));
    expect('-->');
  end;

  function nextTokenQName: string;
  var  namespaceUrl: string;
       namespacePrefix: string;
       name: string;
  begin
    if nextTokenEQName(namespaceUrl, namespacePrefix, name) = xqnmURL then
      raiseSyntaxError('Cannot use Q{} notation for direct constructors');
    if namespacePrefix = '' then result := name
    else result := namespacePrefix + ':' + name;
  end;

  procedure qnameToEQname(var vqname: IXQValue);
  var
    qname: String;
    prefix: String;
    namespace: INamespace;
  begin
    qname := vqname.toString;
    prefix := '';
    if system.pos(':', qname) > 0 then prefix := strSplitGet(':', qname);
    if (result.implicitNamespaces <> nil) and (Result.implicitNamespaces.hasNamespacePrefix(prefix)) then begin
      namespace := Result.implicitNamespaces.namespaces[prefix];
      if namespace.getURL = '' then namespace := nil;
    end else
      namespace := staticContext.findNamespace(prefix, xqdnkUnknown);
    if namespace <> nil then vqname := TXQValueQName.create(namespace.getURL, prefix, qname)
    else if prefix <> '' then raiseParsingError('XPST0081', 'Unknown namespace: '+vqname.toString)
    else vqname := TXQValueQName.create('', prefix, qname)
  end;

  procedure checkForDuplicatedAttributes;
  var
    i, j: Integer;
  begin
    for i := 0 to high(Result.children) do qnameToEQname(TXQTermConstant(TXQTermConstructor(result.children[i]).nameValue).value);
    for i := 0 to high(Result.children) do
      for j := 0 to high(Result.children) do
        if (i <> j)
           and (staticContext.compareAtomic(TXQTermConstant(TXQTermConstructor(result.children[i]).nameValue).value, TXQTermConstant(TXQTermConstructor(result.children[j]).nameValue).value, nil) = 0) then
           raiseParsingError('XQST0040', 'Duplicated attribute: '+TXQTermConstant(TXQTermConstructor(result.children[i]).nameValue).value.toString);
  end;

var
  token: String;
  marker: PChar;
  attribute: TXQTermConstructor;
  lastWasCData: Boolean;
  i: Integer;
  oldDefaultElementTypeNamespace: INamespace;
  prefix: String;
  isNamespaceNode: Boolean;
  oldNamespaceCount: Integer;
begin
  case pos^ of
    '!': exit(parseCommentConstructor);
    '?': exit(parsePIConstructor);
    #9,#10,#13,' ': raiseSyntaxError('Invalid whitespace in constructor');
  end;
  result := TXQTermConstructor.create(tetOpen, TXQTermConstant.create(nextTokenQName));
  try
    skipWhitespace();
    while not (pos^ in ['>', '/', #0]) do begin
      token := nextTokenQName;
      attribute := TXQTermConstructor.create(tetAttribute, TXQTermConstant.create(token));
      expect('=');
      skipWhitespace();
      if not (pos^ in ['''', '"']) then raiseSyntaxError('Expected attribute value');
      marker := pos;
      pos+=1;
      isNamespaceNode := strBeginsWith(token, 'xmlns:') or (token = 'xmlns');
      parseCommonContent(attribute, marker^, false, isNamespaceNode);
      expect(marker^);
      if isNamespaceNode then begin
        if token = 'xmlns' then prefix := ''
        else prefix := strCopyFrom(token, length('xmlns:')+1);
        if result.implicitNamespaces = nil then result.implicitNamespaces := TNamespaceList.Create
        else if result.implicitNamespaces.hasNamespacePrefix(prefix) then raiseParsingError('XQST0071', 'Duplicate namespace declaration');
        token := '';
        if length(attribute.children) > 0 then token := (attribute.children[0] as TXQTermConstant).value.toString;
        if ((prefix = 'xml') <> (token = XMLNamespaceUrl_XML)) or (prefix = 'xmlns') or (token = XMLNamespaceUrl_XMLNS) then
          raiseParsingError('XQST0070', 'Invalid namespace declaration');
        result.implicitNamespaces.add(TNamespace.create(xmlStrWhitespaceCollapse(token), prefix));
        attribute.Free;
      end else result.push(attribute);
      skipWhitespace();
    end;
    checkForDuplicatedAttributes;
    if pos^ = #0 then raiseSyntaxError('Attribute expected');
    if pos^ = '/' then begin
      expect('/');
      if pos^ <> '>' then raiseSyntaxError('Need >');
      inc(pos);
      exit;
    end;
    expect('>');

    oldDefaultElementTypeNamespace := staticContext.defaultElementTypeNamespace;
    if Result.implicitNamespaces <> nil then begin
      if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
      oldNamespaceCount := staticContext.namespaces.Count;
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
            if nextTokenQName() <> (result.nameValue as TXQTermConstant).value.toString then
              raiseParsingError('XQST0118', 'Expected matching closing tag');
            expect('>');
            staticContext.defaultElementTypeNamespace := oldDefaultElementTypeNamespace;
            if result.implicitNamespaces <> nil then
              staticContext.namespaces.deleteFrom(oldNamespaceCount);

            exit;
          end;
          '!': if strBeginsWith(pos, '![CDATA[') then begin
            lastWasCData := true;
            pos += length('![CDATA[');
            marker := pos;
            while (pos^ <> #0) and not strBeginsWith(pos, ']]>') do pos+=1;
            if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
            result.push(TXQTermConstant.create(strFromPchar(marker, pos - marker)));
            pos+=3;
          end else if strBeginsWith(pos, '!--') then result.push(parseCommentConstructor)
          else raiseParsingError('XPST0003', 'Invalid character combination after <!');
          '?': result.push(parsePIConstructor);
          else result.push(parseDirectConstructor());
        end;
      end;
    end;
    raiseParsingError('XPST0003', 'Unexpected end (probably missing closing tag for <'+(result.nameValue as TXQTermConstant).value.toString +'>');

  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseComputedConstructor(name: string): TXQTermConstructor;
var
  token: String;
  tempSeq: TXQTerm;
  onlyConstructors: Boolean;
  i: Integer;
  expectName: Boolean;
  namespaceUrl: string;
  namespacePrefix: string;
begin
  token := name;
  if (token = 'element') then result := TXQTermConstructor.create(tetOpen)
  else if (token = 'document') then result := TXQTermConstructor.create(tetDocument)
  else if (token = 'attribute') then result := TXQTermConstructor.create(tetAttribute)
  else if (token = 'text') then result := TXQTermConstructor.create(tetText)
  else if (token = 'processing-instruction') then result := TXQTermConstructor.create(tetProcessingInstruction)
  else if (token = 'comment') then result := TXQTermConstructor.create(tetComment)
  else raiseParsingError('XPST0003', 'Unknown constructor name');
  try
    expectName := (result.typ in [tetOpen, tetProcessingInstruction, tetAttribute]) ;
    if expectName then begin
      skipWhitespaceAndComment();
      if pos^ = '{' then begin
        pos += 1;
        result.nameValue := parsePrimaryLevel;
        expect('}');
      end else begin
        if (nextTokenEQName(namespaceUrl, namespacePrefix, token) = xqnmPrefix) then begin
          if result.typ <> tetProcessingInstruction then begin
            if result.typ <> tetAttribute then namespaceUrl := staticContext.findNamespaceURL(namespacePrefix, xqdnkElementType)
            else namespaceUrl := staticContext.findNamespaceURL(namespacePrefix, xqdnkUnknown);
            if (namespacePrefix <> '') and (namespaceUrl = '') then raiseParsingError('XPST0081', 'Unknown prefix: '+namespacePrefix+':'+token);
          end;
        end;
        if result.typ <> tetProcessingInstruction then
           result.nameValue := TXQTermConstant.create(TXQValueQName.create(namespaceUrl, namespacePrefix, token))
        else if (namespaceUrl = '') and (namespacePrefix = '') then
          result.nameValue := TXQTermConstant.create(baseSchema.NCName.createValue(token))
        else
          raiseSyntaxError('Cannot use namespace for processing instructions');
      end;
    end;
    expect('{');
    skipWhitespaceAndComment();
    if pos^ <> '}' then begin
      if result.typ in [tetDocument, tetOpen, tetProcessingInstruction, tetAttribute] then begin
        tempSeq := parsePrimaryLevel;
        if tempSeq is TXQTermSequence then begin
          onlyConstructors := true;
          for i:= 0 to high(TXQTermSequence(tempseq).children) do
            if not (TXQTermSequence(tempSeq).children[i] is TXQTermConstructor) then begin
              onlyConstructors:=false;
              break;
            end;
          if onlyConstructors then begin
            result.children := TXQTermSequence(tempseq).children;
            TXQTermSequence(tempseq).children := nil;
            tempSeq.free;
          end else result.push(tempSeq); //that's really slow for nodes because it makes a deep copy of them if they are taken from a subsequence. But if it's mixing atomic/nodes flattening the sequences makes the separator spaces wrong
        end else result.push(tempSeq);
      end else result.nameValue := parsePrimaryLevel;
    end else if not expectName then
      raiseParsingError('XPST0003', 'This type of node must not be empty ');
    expect('}');
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseExtension: TXQTerm;
var
  pragma: String;
  url: string;
begin
  requireXQuery('for extensions');
  expect('#');
  if not nextTokenEQName(url, pragma, xqdnkUnknown) then
    raiseParsingError('XPST0003', 'Extension name requires namespace');
  //just ignore it
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

function TXQParsingContext.parseVariable: TXQTermVariable;
var
  url: string;
  name: string;
begin
  expect('$');
  nextTokenEQName(url, name, xqdnkUnknown);
  result := TXQTermVariable.create(name, url);
end;

function TXQParsingContext.parseDefineVariable: TXQTermDefineVariable;
begin
  result := TXQTermDefineVariable.create(parseVariable);
  try
    if nextToken(true) = 'as' then begin
      expect('as');
      result.push(parseSequenceType([]));
    end;
  except
    result.free;
    raise;
  end;
end;


function TXQParsingContext.parseAnnotations: TXQAnnotations;
var
  i: Integer;
  j: Integer;
  prefix: string;
begin
  requireXQuery3('Annotations need XQuery 3');
  try
    setlength(result, 1);
    while true do begin
      with result[high(result)] do begin
        if nextTokenEQName(namespace, prefix, name) = xqnmPrefix then
          if prefix = '' then namespace := XMLNamespaceUrl_XQuery
          else namespace := staticContext.findNamespaceURL(prefix, xqdnkUnknown);
        if namespace = XMLNamespaceUrl_XQuery then begin
          if not ((name = 'private') or (name = 'public')) then raiseParsingError('XQST0045', 'Only private/public annotations are allowed in namespace '+namespace);
          if (length(result) > 1) {and ((name = 'private') or (name = 'public')) }  then
            for i := 0 to high(result) - 1 do
              if ((result[i].name = 'private') or (result[i].name = 'public')) and (result[i].namespace = XMLNamespaceUrl_XQuery) then
                while true do
                  case nextToken() of
                    'function': raiseParsingError('XQST0106', '%private/%public has to be unique');
                    'variable': raiseParsingError('XQST0116', '%private/%public has to be unique');
                    '': raiseParsingError('XPST0003', 'expected variable/function');
                  end;
        end else case namespace of
          XMLNamespaceUrl_XML, XMLNamespaceURL_XMLSchema, XMLNamespaceURL_XMLSchemaInstance,
          XMLNamespaceURL_XPathFunctions, XMLNamespaceURL_XPathFunctionsMath: raiseParsingError('XQST0045', 'No annotations are allowed in namespace '+namespace);
        end;

        if nextToken(true) = '(' then begin
          expect('(');
          while true do begin
            SetLength(params, length(params) + 1);
            params[high(params)] := parseValue;
            if not (params[high(params)] is TXQTermConstant) then raiseParsingError('XPST0003', 'Only literals allowed as annotation arguments');
            if nextToken(true) <> ',' then break;
            expect(',');
          end;
          expect(')')
        end;
      end;
      if nextToken(true) <> '%' then break;
      expect('%');
      setlength(result, length(result)+1);
    end;
  except
    for i := 0 to high(result) do for j := 0 to high(result[i].params) do
      result[i].params[j].free;
    raise;
  end;
end;

function TXQParsingContext.parseFunctionDeclaration(annotations: TXQAnnotations; anonymous: boolean): TXQTermDefineFunction;
var
  tempVar: TXQTermDefineVariable;
  i: Integer;
  namespaceUrl: string;
  namespacePrefix: string;
begin
  if anonymous and (length(annotations) > 0) then
    for i := 0 to high(annotations) do
      with annotations[i] do
        if (namespace = XMLNamespaceUrl_XQuery) and ((name = 'private') or (name = 'public')) then
          raiseParsingError('XQST0125', 'anonymous functions cannot be public or private');
  result := TXQTermDefineFunction.create();
  result.annotations := annotations;
  try
    if not anonymous then begin
      if nextTokenEQName(namespaceUrl, namespacePrefix, result.funcname) = xqnmPrefix then
        result.namespace := staticContext.findNamespace(namespacePrefix, xqdnkFunction)
       else
        result.namespace := TNamespace.create(namespaceUrl, namespacePrefix);
      expect('(');
    end else require3('Anonymous functions need XPath/XQuery 3');
    skipWhitespaceAndComment();
    while nextToken(true) <> ')' do begin
      tempVar := parseDefineVariable;
      result.push(tempVar);
      for i := 0 to high(Result.children) - 1 do
        if TXQTermVariable(TXQTermDefineVariable(Result.children[i]).variable).equalsVariable(TXQTermVariable(tempVar.variable)) then
          raiseParsingError('XQST0039', 'Duplicate variable name: '+tempVar.ToString);
      skipWhitespaceAndComment();
      if not (pos^ in [',', ')']) then raiseParsingError('XPST0003', 'Missing , or )');
      if pos^ = ',' then pos+=1;
    end;
    pos+=1;
    result.parameterCount:=length(result.children);
    if nextToken(true) = 'as' then begin
      expect('as');
      result.push(parseSequenceType([]));
    end;
    case nextToken() of
      '{': begin
        result.push(parsePrimaryLevel);
        expect('}');
      end;
      'external': if anonymous then raiseSyntaxError('Anonymous function cannot be external');
      else raiseSyntaxError('Function body { } or external expected');
    end;
    if not anonymous then begin
      if result.namespace = nil then raiseParsingError('XQST0060', 'No namespace for declared function: '+result.funcname);
      case result.namespace.getURL of
        XMLNamespaceUrl_XML, XMLNamespaceURL_XMLSchema, XMLNamespaceURL_XMLSchemaInstance, XMLNamespaceURL_XPathFunctions:
          raiseParsingError('XQST0045', 'Invalid namespace for function declaration: '+result.namespace.getURL);
        XMLNamespaceURL_XPathFunctionsMath, XMLNamespaceURL_XQuery:
          if parsingModel = xqpmXQuery3 then
            raiseParsingError('XQST0045', 'Invalid namespace for function declaration: '+result.namespace.getURL);
      end;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseTryCatch: TXQTermTryCatch;
var
  url: string;
  prefix: string;
  localpart: string;
  kind: TXQNamespaceMode;
  token: String;
begin
  expect('{');
  result := TXQTermTryCatch.Create(parsePrimaryLevel);
  try
    expect('}');
    token := nextToken(true);
    while token = 'catch' do begin
      expect('catch');
      SetLength(result.catches, length(result.catches) + 1);
      repeat
        kind := nextTokenEQName(url, prefix, localpart, true);
        if kind = xqnmPrefix then begin
          url := staticContext.findNamespaceURLMandatory(prefix, xqdnkUnknown);
          kind := xqnmURL;
        end;
        SetLength(result.catches[high(result.catches)].tests, length(result.catches[high(result.catches)].tests) + 1);
        result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].local := localpart;
        result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].namespace := url;
        result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].kind := kind;
        token := nextToken();
      until token <> '|';
      if token <> '{' then raiseSyntaxError('{ expected');
      result.catches[high(result.catches)].expr := parsePrimaryLevel;
      expect('}');
      token := nextToken(true);
    end;
  except
    result.free;
    raise;
  end;
end;


function TXQParsingContext.replaceEntitiesAlways(s: string): string;
var
  n, p: Integer;
  temp: string;
  code: Integer;
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
        if (temp = '') or (temp[1] <> '#')  then raiseSyntaxError('Invalid entity');
        delete(temp,1,1);
        code := -1;
        if temp <> '' then
          case temp[1] of
            'x': code := StrToIntDef('$'+strcopyfrom(temp,2), -1);
            'X': ; //strToInt does hex with x
            else code := StrToIntDef(temp, -1);
          end;
        if code < 0 then raiseSyntaxError('Invalid entity');
        if code = 0 then raiseParsingError('XQST0090', '0 is not allowed in strings');
        result += strGetUnicodeCharacter(code, staticContext.stringEncoding)
      end;
    end;
    p := n + 1;
    n := strIndexOf(s, '&', n);
  end;
  result += strcopyfrom(s, p);
end;

function TXQParsingContext.replaceEntitiesIfNeeded(const s: string): string;
begin
  result := s;
  if ((parsingModel in [xqpmXQuery1,xqpmXQuery3]) and (options.StringEntities = xqseDefault)) or (options.StringEntities = xqseResolveLikeXQuery) then
    Result := replaceEntitiesAlways(Result);
end;


function TXQParsingContext.parseString(const w: string): string;
begin
  result := replaceEntitiesIfNeeded(StringReplace(copy(w,2,length(w)-2), w[1]+w[1], w[1], [rfReplaceAll]));
end;

function TXQParsingContext.parseString: string;
begin
  skipWhitespaceAndComment();
  if not (pos^ in ['''', '"']) then raiseParsingError('XPST0003', 'Expected string');
  result := parseString(nextToken());
end;

function TXQParsingContext.parseXString(nullTerminatedString: boolean): TXQTerm;
  function functionIsConcat(nf: TXQTermNamedFunction): boolean;
  begin
    result := (nf.func <> nil) and (nf.func is TXQBasicFunctionInfo) and (TXQBasicFunctionInfo(nf.func).func = @xqFunctionConcat);
  end;

  procedure pushTerm(t: TXQTerm);
  begin
    if not (t is TXQTermConstant) then
      t := TXQTermNamedFunction.create(XMLNamespaceUrl_MyExtensions, 'join', [t]);
    if (result = nil) and (t is TXQTermConstant) then
      result := t
    else if result = nil then
      result := t//TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'concat', [t])
    else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result)) then
      TXQTermNamedFunction(result).push(t)
    else
     result := TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'concat', [result, t]);
  end;

  procedure pushRaw(from, too: pchar);
  var
    v: String;
  begin
    if too < from then exit;
    v := replaceEntitiesIfNeeded(strFromPchar(from, too - from + 1));
    if result <> nil then begin
      if (result is TXQTermConstant) and (TXQTermConstant(result).value is TXQValueString) then
        (TXQTermConstant(result).value as TXQValueString).str += v
      else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result))
              and (TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)] is TXQTermConstant)
              and (TXQTermConstant(TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)]).value is TXQValueString )
              then
        (TXQTermConstant(TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)]).value as TXQValueString).str += v
      else pushTerm(TXQTermConstant.create(v));
    end else pushTerm(TXQTermConstant.create(v));
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
  if result = nil then result := TXQTermConstant.create('')
  else if (result is TXQTermConstant) and not (TXQTermConstant(result).value is TXQValueString) then
    result := TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'string', [result]);
end;

function TXQParsingContext.parseJSONLikeObjectConstructor: TXQTermWithChildren;
var
  token: String;
  jn: TXQNativeModule;
begin
  //expect('{'); parsed by caller
  if pos^ = '|' then begin
    expect('|');
    jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
    if jn = nil then raiseParsingError('pxp:JSONIQ', 'The {| .. |} syntax can only be used, if the json unit is loaded.');
    result := TXQTermNamedFunction.create();
    TXQTermNamedFunction(result).kind := xqfkBasic;
    TXQTermNamedFunction(result).func := jn.findBasicFunction('object', 1, xqpmXPath2);
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

function createDynamicErrorTerm(const code, msg: string): TXQTermNamedFunction;
begin
  result := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'error', [
                TXQTermConstant.create(TXQValueQName.create(XMLNamespaceURL_XQTErrors, 'err', code)),
                TXQTermConstant.create(msg)]);
end;

function staticallyCastQNameAndNotation(term: TXQTermWithChildren; typ: TXSType; staticContext: TXQStaticContext; castable: boolean = false): txqterm;
  function castFail(code: string): txqterm;
  begin
    if castable then result := TXQTermConstant.create(xqvalueFalse)
    else result := createDynamicErrorTerm(code, 'Invalid cast to QName/NOTATION');
  end;

var
  name: String;
  namespace: INamespace;
begin
  result := term;
  if typ.storage = TXQValueQName then begin
    if typ = baseSchema.NOTATION then result := castFail('XPST0080')
    else if (term.children[0] is TXQTermConstant) then begin
      case TXQTermConstant(term.children[0]).value.kind of
        pvkQName: exit; {begin
          if castable then result := TXQTermConstant.create(xqvalueTrue)
          else begin
            result := term.children[0];
            term.children[0] := nil;
          end;
        end;}
        pvkString: begin
          name := trim(TXQTermConstant(term.children[0]).value.toString);
          if not (baseSchema.isValidQName(name)) then result := castFail('FORG0001')
          else if castable then result := TXQTermConstant.create(xqvalueTrue)
          else if (staticContext.model in [xqpmXPath3, xqpmXQuery3]) then exit
          else begin
            //see  TXQTermSequenceType.staticQNameCast
            if pos(':', name) > 0 then begin
              namespace := staticContext.findNamespace(strSplitGet(':', name), xqdnkElementType);
              if namespace = nil then result := createDynamicErrorTerm('FONS0004', 'Failed to find namespace of: '+TXQTermConstant(term.children[0]).value.toString)
              else result := TXQTermConstant.Create(TXQValueQName.create(typ, namespace, name));
            end else result := TXQTermConstant.Create(TXQValueQName.create(typ, staticContext.findNamespace('', xqdnkElementType), name));
          end;
        end
        else result := castFail('XPTY0004')
      end;
    end;
    if result <> term then term.free;
  end;
end;

function StrToIntWithError(const s: string): integer;
var
  i: Integer;
begin
  result := StrToIntDef(s, -1);
  if result = -1 then begin
    for i := 1 to length(s) do if not (s[i] in ['0'..'9']) then raise EXQEvaluationException.create('XPST0003', 'Invalid number: '+s);
    raise EXQEvaluationException.create('FOAR0002', 'Invalid number: '+s);
  end;
end;

function TXQParsingContext.parseValue: TXQTerm;
var
  word: String;
  wordlookahead: String;
  temp: PChar;
  constr: Boolean;
  annotations: TXQAnnotations;
  namespaceURL: String;
  namespacePrefix: String;
  axis: String;
  namespaceMode: TXQNamespaceMode;
  marker: PChar;

  procedure lookupNamedFunction(f: TXQTermNamedFunction);
  var
    akind: TXQTermNamedFunctionKind;
    afunc: TXQAbstractFunctionInfo;
    tempUrl: String;
  begin
    if TXQTermNamedFunction.findKindIndex(namespaceURL, word, length(f.children), staticContext, akind, afunc) then begin
      f.kind := akind;
      f.func := afunc;
      exit();
    end;
    if namespacePrefix = '' then begin
      if staticContext.defaultElementTypeNamespace <> nil then tempUrl := staticContext.defaultElementTypeNamespace.getURL
      else if staticContext.defaultTypeNamespace <> nil then tempUrl := staticContext.defaultTypeNamespace.getURL;
      if TXQTermNamedFunction.findKindIndex(tempUrl, word, length(f.children), staticContext, akind, afunc) then begin
        f.kind := akind;
        f.func := afunc;
        exit();
      end;
    end;
    if namespaceURL = XMLNamespaceURL_XPathFunctions then
      raiseParsingError('XPST0017', 'unknown function: ' + ifthen(namespacePrefix = '', 'Q{'+namespaceURL+'}'+namespacePrefix+':'+word, namespacePrefix+':'+word));
    f.namespaceUrl := namespaceURL;
    f.funcname := word; //cannot know if the function is valid yet. XQuery can have mutually recursive functions, but this is called during the function declaration (i.e. before the 2nd function has been parsed)
    f.kind := xqfkUnknown;
  end;

  function parseVariableWithDotNotation: TXQTerm;
  var
    operatorMode: Boolean;
    propertyAccess: Boolean;
  begin
    result := parseVariable;
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
      if TXQTermDynamicFunctionCall(result).children[1] is TXQTermNodeMatcher then begin
        result.free;
        raiseParsingError('pxp:XPST0003', 'A node matching step is not allowed directly after a property dot operator');
      end;
    end;
  end;

begin
  result := nil;
  skipWhitespaceAndComment();
  if pos^ = #0 then exit();
  case pos^ of
    '''', '"':  exit(TXQTermConstant.create(parseString()));
    '$': exit(parseVariableWithDotNotation());
    '-', '+': begin
      word := nextToken() + 'u';
      exit(TXQTermBinaryOp.create(word, TXQTermConstant.create(xqvalue()), parseValue()));
    end;
    '(': begin
      inc(pos);
      if pos^ = '#' then exit(parseExtension);
      result := TXQTermSequence.Create;
      try
        exit(parseSequenceLike(TXQTermWithChildren(result))); //only sequence or priority brackets
      except
        result.free;
        raise;
      end;
    end;


    '/': begin
      word := nextToken();
      if pos^ = '/' then begin expect('/'); word+='/';end;
      skipWhitespaceAndComment();
      if (pos^ in [#0,',',')',']','}','=','!','>','[','|','+',';']) or ((pos^ = '<') and (parsingModel in [xqpmXPath2, xqpmXPath3])) then
        exit(TXQTermNodeMatcher.Create('/')) //leading lone slash (see standard#parse-note-leading-lone-slash)
      else exit(TXQTermBinaryOp.Create(word, TXQTermNodeMatcher.Create('/'), parseValue()));
    end;

    '0'..'9': exit(TXQTermConstant.createNumber(nextToken()));
    '.': begin
      word := nextToken();
      if (word = '.') or (word = '..') then exit(TXQTermNodeMatcher.Create(word))
      else if word[2] in ['0'..'9', 'e', 'E'] then exit(TXQTermConstant.createNumber(word))
      else raiseParsingError('XPST0003', 'Unknown term: '+word);
    end;

    '<': begin
      requireXQuery('to use constructors (this error can also mean an invalid < )');
      inc(pos);
      exit(parseDirectConstructor());
    end;

    '{': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected {. (Enable json extension, to create a json like object) ');
      inc(pos);
      exit(parseJSONLikeObjectConstructor);
    end;
    '[': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected [. (Enable json extension, to create a json like array) ');
      inc(pos);
      exit(parseJSONLikeArray());
    end;
    '%': begin
      inc(pos);
      annotations := parseAnnotations;
      expect('function'); expect('(');
      exit(parseFunctionDeclaration(annotations, true));
    end;
    'x': if ((pos+1)^ in ['"', '''']) then begin
      if not options.AllowExtendedStrings then raiseParsingError('err:XPST0003', 'Extended string syntax was disabled');
      inc(pos);
      exit(parseXString());
    end;
  end;

  try
    axis := '';
    namespaceURL := '';
    namespacePrefix := '';
    marker := pos;
    word := nextToken();
    if word = '@' then axis := 'attribute'
    else if nextToken(true) = '::' then begin
      axis := word;
      expect('::');
    end else pos := marker; //roll back
    namespaceMode := nextTokenEQName(namespaceURL, namespacePrefix, word, true);

    skipWhitespaceAndComment();
    case pos^ of
      '(': begin
        expect('(');
        if namespacePrefix = '' then begin
          case word of
            'if': begin
              result := TXQTermIf.Create();
              with TXQTermIf(result) do begin
                push(parsePrimaryLevel);
                expect(')'); expect('then');
                push(parse());
                expect('else');
                push(parse());
              end;
              exit;
            end;
            'typeswitch': exit(parseTypeSwitch);
            'switch': if parsingModel in [xqpmXPath3, xqpmXQuery3] then exit(parseSwitch); //switch is not a xpath 3 statement, but still not allowed as function name
            'function': if parsingModel in [xqpmXQuery3, xqpmXPath3] then exit(parseFunctionDeclaration(nil, true));
          end;

          if isKindTestFunction(word) then begin
            result := TXQTermNodeMatcher.Create(word, true);
            TXQTermNodeMatcher(result).namespaceCheck := xqnmNone;
            if strContains(word, 'attribute') and (axis = '') then axis := 'attribute';
            TXQTermNodeMatcher(result).axis:=axis;
            skipWhitespaceAndComment();
            if pos^ <> ')' then begin
              with TXQTermNodeMatcher(result) do begin
                case word of
                  'processing-instruction': begin
                    skipWhitespaceAndComment();
                    if pos^ in ['"', ''''] then push(TXQTermConstant.create(parseString()))
                    else push(TXQTermConstant.create(nextTokenNCName()))
                  end;
                  'element', 'schema-element', 'attribute', 'schema-attribute', 'document-node': begin
                    push(parseValue());
                    if (not (children[0] is TXQTermNodeMatcher)) or
                       ((word <> 'document-node') and (length(TXQTermNodeMatcher(children[0]).children) > 0)) then
                         raiseSyntaxError('Invalid test');
                  end;
                  else raiseSyntaxError('No option allowed for matching test: '+word);
                end;
              end;
              skipWhitespaceAndComment();
              if pos^ = ',' then begin
                if (word <> 'element') and (word <> 'attribute') then
                  raiseParsingError('XPST0003', 'Only one parameter is allowed for matching test '+word);
                expect(',');
                TXQTermNodeMatcher(result).push(parseSequenceType([xqstAllowValidationTypes]));
              end;
            end;
            expect(')');
            if (word <> 'node') and (axis <> 'self') and ( (axis = 'attribute') <> (strContains(word, 'attribute')) ) then begin
              result.free;
              result := TXQTermSequence.create();
            end;
            exit;
          end;
          if axis <> '' then raiseParsingError('XPST0003', 'Not an kind/node test');
        end;


        if namespaceURL = '' then
          namespaceURL := staticContext.findNamespaceURL(namespacePrefix, xqdnkFunction);
        if namespaceURL = '' then raiseParsingError('XPST0017', 'No namespace given for function '+word);
        //result := TXQTermNamedFunction.createIfExists(namespaceURL, namespa word, parsingModel);

        result := TXQTermNamedFunction.create();
        result := parseFunctionCall(TXQTermNamedFunction(result));
        if result is TXQTermNamedFunction then begin
          lookupNamedFunction(TXQTermNamedFunction(result));
          with TXQTermNamedFunction(result) do
            if (kind = xqfkTypeConstructor) and (length(children) = 1) then
              result := staticallyCastQNameAndNotation(TXQTermNamedFunction(result), TXSType(TObject(func)), staticContext);
        end else lookupNamedFunction(TXQTermWithChildren(result).children[0] as TXQTermNamedFunction);
        exit();
      end;
      '$', '<': case word of
        'for', 'let': exit(parseFlower(word));
        'some', 'every': exit(parseSomeEvery(word));
      end;
      '{': case word of
        'unordered', 'ordered': begin //TODO: actually use that
           requireXQuery();
           expect('{'); result := parse(); expect('}');
           exit;
         end;
        'try': begin
          requireXQuery3('try');
          exit(parseTryCatch);
        end;
      end;
      '#': begin
        require3('Named Function Reference');
        expect('#');
        if namespaceURL = '' then
          exit(TXQTermDefineFunction.CreateReference(staticContext.findNamespace(namespacePrefix, xqdnkFunction), word, StrToIntWithError(nextToken())))
         else
          exit(TXQTermDefineFunction.CreateReference(namespaceURL, word, StrToIntWithError(nextToken())));
      end;
      ':': if (pos+1)^ = '=' then
        if namespacePrefix = '' then
          namespacePrefix := 'Q';
    end;

    if (namespaceMode = xqnmPrefix) and (namespacePrefix = '') then
      case word of
        'element', 'attribute', 'document', 'text', 'processing-instruction', 'comment': begin
          skipWhitespaceAndComment();
          constr := nextToken(true) = '{';
          if (not constr) and (pos^ <> #0) and not (pos^ in SYMBOLS) then begin //look for name (this will allow something like text name {...} here, but that's going to raise an error later anyways)
            temp := pos;
            nextTokenEQName(namespaceURL, namespacePrefix, wordlookahead, true);
            if nextToken() = '{' then constr := true;
            pos := temp;
          end;
          if constr then begin
            requireXQuery('to use constructors');
            exit(parseComputedConstructor(word));
          end;
        end;
        'validate': case nextToken(true) of
          'lax', 'strict', '{': begin
            requireXQuery('for schema validation');
            if pos^ <> '{' then
              nextToken();
            expect('{');
            raiseParsingError('XQST0075', 'Schema validation is not supported');
          end;
        end;
        'for': case nextToken(true) of
           'tumbling', 'sliding': exit(parseFlower(word));
         end;
      end;

    if (word = '') or (word[1] in [',', ';', ':', ')', ']', '}']) then //todo: check if valid xml node name
      raiseParsingError('XPST0003', 'Unexpected character: ' + word);

    //if (not staticContext.useLocalNamespaces) and (namespacePrefix <> '*') and ((namespacePrefix <> '') or (word <> '*'))  then
    //  namespaceURL := staticContext.findNamespaceURL(namespacePrefix, xqdnkElementType);
    result := TXQTermNodeMatcher.Create();
    TXQTermNodeMatcher(result).select := word;
    if namespaceMode = xqnmPrefix then
      if not staticContext.useLocalNamespaces then begin
        namespaceMode := xqnmURL;
        namespaceURL := staticContext.findNamespaceURL(namespacePrefix, xqdnkElementType);
      end else namespaceURL := namespacePrefix;

    TXQTermNodeMatcher(result).namespaceCheck := namespaceMode;
    TXQTermNodeMatcher(result).namespaceURLOrPrefix := namespaceURL;
    TXQTermNodeMatcher(result).axis:=axis;
  except
    result.free;
    raise;
  end;
end;




function TXQParsingContext.parse: TXQTerm;
  //searchs the term to split
  //e.g.
  //          to                          to
  //        1     +        * 4      =>  1     +
  //            2    3                      2    3 <- * 4
  //takes a pointer to a txqterm, since it sets the variable that should be changed
  //(parentheses can be ignored since they are stored as sequence terms in the tree)
  function ripBinOpApart(term: pxqterm; const prio: integer): PXQTerm;
  var
    binOp: TXQTermBinaryOp;
  begin
    if not (term^ is TXQTermBinaryOp) then exit(term);
    binOp := TXQTermBinaryOp(term^);
    if binOp.op.priority > prio then exit(term);
    if (binOp.op.priority = prio) then begin
      if (xqofAssociativeSyntax in binOp.op.flags) then exit(term);
      raiseSyntaxError('Operator requires parenthesis '+ binOp.op.name);
    end;
    result := ripBinOpApart(@binOp.children[1], prio);
  end;
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
      isCast: Boolean;
    begin
      expect(res.op.followedBy); //assume we read instance of/cast/castable/treat as
      isCast := ((res.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs));
      if isCast then st := parseSequenceType([xqstIsCast])
      else st := parseSequenceType([]);
      res.push(st);
      if isCast then begin
        if not (st.isSingleType()) then
          raiseSyntaxError('Need single typ for cast')
        else if baseSchema.isAbstractType(st.atomicTypeInfo) then
          raiseParsingError(ifthen(isModel3 or (st.atomicTypeInfo <> baseSchema.anySimpleType), 'XPST0080', 'XPST0051'), 'Invalid type for cast')
        else if (st.atomicTypeInfo is TXSSimpleType) and not (TXSSimpleType(st.atomicTypeInfo).variety in [xsvAbsent, xsvAtomic]) then
          raiseParsingError('XQST0052', 'Expected simple type');
        replace^ := staticallyCastQNameAndNotation(res, st.atomicTypeInfo, staticContext, res.op.func = @xqvalueCastableAs);
      end;
    end;

  begin
    expect(opinfo.name);
    if (opinfo.name[1] in ['a'..'z','A'..'Z']) and (pos^ in ['a'..'z','A'..'Z','0'..'9','.','-'])  then
      raiseSyntaxError('Need whitespace after operator');

    if opinfo.require3 then require3();

    replace := ripBinOpApart(@astroot, opinfo.priority);

    res := TXQTermBinaryOp.Create(opinfo);
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
    needDynamicCall: Boolean;
  begin
    replace := ripBinOpApart(@astroot, 10000);
    if (replace^ is TXQTermFilterSequence) or (replace^ is TXQTermSequence) or (replace^ is TXQTermVariable)
       or (replace^ is TXQTermNamedFunction) or (replace^ is TXQTermJSONObjectConstructor) or (replace^ is TXQTermDynamicFunctionCall) then begin
         if pos^ in SYMBOLS + WHITE_SPACE then needDynamicCall:=true
         else begin
           word := nextToken();
           for prop in strSplit(word, '.') do begin
             if prop = '' then raiseParsingError('XPST0003', 'Unexpected ..');
             replace^ := TXQTermReadObjectProperty.Create(prop).push([replace^]);
           end;
           needDynamicCall:=strEndsWith(word, '.');
         end;
         if needDynamicCall then begin
           replace^ := TXQTermDynamicFunctionCall.Create(replace^, parseValue);
           if TXQTermDynamicFunctionCall(replace^).children[1] is TXQTermNodeMatcher then
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
        '', ',', ';', ':', ')', ']', '}', 'else', 'return', 'satisfies', 'for', 'let', 'order', 'where', 'stable', 'end', 'only', 'ascending', 'descending', 'start', 'empty', 'group', 'collation', 'case', 'default', 'count':
          exit(astroot);
        '[': begin
          expect('[');
          skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000);
          if pos^ <> ']' then replace^ := TXQTermFilterSequence.Create(replace^, parsePrimaryLevel())
          else                replace^ := TXQTermFilterSequence.Create(replace^); //stupid hack to allow $var [] :=
          expect(']');
        end;
        '(': begin
          expect('('); skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000); //TODO: check
          replace^ := TXQTermDynamicFunctionCall.Create(replace^);
          replace^ := parseFunctionCall(TXQTermDynamicFunctionCall(replace^))
        end;
        ':=': begin
          expect(':=');
          result := astroot;
          if result is TXQTermNodeMatcher then begin
            case TXQTermNodeMatcher(astroot).namespaceCheck of
              xqnmNone: result := TXQTermVariable.create(TXQTermNodeMatcher(astroot).select);
              xqnmPrefix: result := TXQTermVariable.create(TXQTermNodeMatcher(astroot).namespaceURLOrPrefix + ':' + TXQTermNodeMatcher(astroot).select, staticContext);
              xqnmURL: result := TXQTermVariable.create(TXQTermNodeMatcher(astroot).select, TXQTermNodeMatcher(astroot).namespaceURLOrPrefix);
            end;
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
          else if (word = '.') and (options.AllowPropertyDotNotation <> xqpdnDisallowDotNotation) then begin
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
        TXQTermSequence(result).push(parse);
      end;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseModule(): TXQTerm;
  procedure parseEncoding;
  var
    encname: String;
  begin
    encname := parseString;
    encoding := strEncodingFromName(encname);
    if encoding = eUnknown then raiseParsingError('XQST0087', 'Unknown encoding: ' + encname);
    expect(';');
  end;

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
    requireModule;
    prefix := '';
    url := nextToken();
    case url of
      'default': begin
        expect('element');
        expect('namespace');
        url := parseString();
      end;
      'namespace': begin
        prefix := nextTokenNCName();
        expect('=');
        url := parseString();
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
      parseString;
      while nextToken(true) = ',' do begin expect(','); parseString(); end;
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
    requireModule;
    skipWhitespaceAndComment();
    moduleName := '';
    if pos^ = 'n' then begin
      expect('namespace'); moduleName:=nextTokenNCName(); expect('=');
    end;
    moduleURL := parseString;
    at := nil;
    if nextToken(true) = 'at' then begin
      expect('at');
      arrayAdd(at, parseString);
      while nextToken(true) = ',' do begin expect(','); arrayAdd(at, parseString); end;
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

  procedure declareVariable(annotations: TXQAnnotations);
  var vari: TXQTermDefineVariable;
  begin
    requireModule;
    vari := parseDefineVariable;
    vari.annotations := annotations;
    TXQTermModule(result).push(vari);
    case nextToken() of
      ':=': vari.push(parse());
      'external': if nextToken(true) = ':=' then begin
        requireXQuery3('default value');
        expect(':=');
        vari.push(parse());
        SetLength(vari.annotations, length(vari.annotations)+1);
        vari.annotations[high(vari.annotations)].name := 'external';
        vari.annotations[high(vari.annotations)].namespace := XMLNamespaceURL_MyExtensions;
      end;
      else raiseParsingError('XPST0003', 'Invalid variable declaration');
    end;
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
  annotations: TXQAnnotations;
  marker: PChar;



  oldNamespaceCount: Integer;
begin
  result := nil;
  oldNamespaceCount := 0;
  if staticContext.namespaces <> nil then oldNamespaceCount := staticContext.namespaces.Count;
  try
    token := nextToken(true);
    marker := pos;
    if token = 'xquery' then begin
      expect(token);
      case nextToken() of
        'version': begin
          requireXQuery();
          temp := parseString();
          if temp = '1.0' then parsingModel := xqpmXQuery1
          else if (temp <> '3.0') or not isModel3 then
            raiseParsingError('XQST0031', 'Invalid xquery version, need 1.0 or 3.0');
          token := nextToken(true);
          if token = 'encoding' then begin
            expect(token);
            parseEncoding;
            token := nextToken(true);
          end else if token = ';' then begin expect(token); token := nextToken(true); end
          else raiseSyntaxError('expected encoding or ;');
        end;
        'encoding': begin
          requireXQuery3();
          parseEncoding;
        end
        else pos := marker;
      end;
    end;

    if token = 'module' then begin
      expect(token);
      case nextToken() of
        '': pos := marker;
        'namespace': begin
          requireModule;
          staticContext.moduleNamespace := TNamespace.create('', nextTokenNCName());
          expect('=');
          (staticContext.moduleNamespace as TNamespace).url := parseString;
          expect(';');
          token := nextToken(true);
          if staticContext.importedModules = nil then staticContext.importedModules := TStringList.Create;
          staticContext.importedModules.AddObject(staticContext.moduleNamespace.getPrefix, resultquery); //every module import itself so it can lazy initialize its variables
          if staticContext.sender.AutomaticallyRegisterParsedModules and (resultquery <> nil) then
            staticContext.sender.FModules.Add(IXQuery(resultquery));
        end;
        else expect('namespace');
      end;
    end;

    while ((token = 'declare') or (token = 'import')) do begin
      marker := pos;
      expect(token);
      if token = 'import' then begin
        case nextToken() of
          'schema': importSchema;
          'module': importModule;
          else begin
            pos := marker;
            break;
          end;
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
            'collation': staticContext.collation := staticContext.sender.getCollation(parseString, staticContext.baseURI);
            'order': begin
              expect('empty');
              case nextToken() of
                'greatest': staticContext.emptyOrderSpec:=xqeoEmptyGreatest;
                'least': staticContext.emptyOrderSpec:=xqeoEmptyLeast;
              end;
            end;
            'element', 'function': begin
              expect('namespace');
              if token = 'element' then staticContext.defaultElementTypeNamespace:=TNamespace.Create(xmlStrWhitespaceCollapse(parseString), '')
              else staticContext.defaultFunctionNamespace := TNamespace.Create(xmlStrWhitespaceCollapse(parseString), '')
            end;
            else raiseParsingError('XPST0003', 'Unknown default value');
          end;
        end;
        'base-uri': staticContext.baseUri := parseString;
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
           nameSpaceURL := xmlStrWhitespaceCollapse(parseString);
           if (nameSpaceName = 'xml') or (nameSpaceName = 'xmlns')
              or (nameSpaceURL = XMLNamespaceUrl_XML) or (nameSpaceURL = XMLNamespaceUrl_XMLNS) then
                raiseParsingError('XQST0070', 'Undeclarable namespace');
           if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create
           else if staticContext.namespaces.lastIndexOfNamespacePrefix(nameSpaceName) >= oldNamespaceCount then
             raiseParsingError('XQST0033', 'Duplicated namespace declaration');
           staticContext.namespaces.Add(TNamespace.create(nameSpaceURL, nameSpaceName));
        end;
        else begin
          pos := marker;
          token := 'declare';
          break;
        end;
      end;
      expect(';');
      token := nextToken(true);
    end;

    while (token = 'declare') do begin
      marker := pos;
      expect(token);
      case nextToken() of
        'variable': declareVariable(nil);
        'function': begin
          requireModule;
          TXQTermModule(result).push(parseFunctionDeclaration(nil));
        end;
        '%': begin
          annotations := parseAnnotations;
          case nextToken() of
            'variable': declareVariable(annotations);
            'function': begin
              requireModule;
              TXQTermModule(result).push(parseFunctionDeclaration(annotations));
            end;
            else raiseParsingError('XPST0003', 'Only variables and functions can have annotations');
          end;
        end;
        'option': begin
          if nextTokenEQName(nameSpaceURL, nameSpaceName, token) = xqnmPrefix then
            if nameSpaceName = '' then nameSpaceURL := XMLNamespaceUrl_XQuery
            else nameSpaceURL := staticContext.findNamespaceURL(nameSpaceName, xqdnkUnknown);
          temp := parseString;
          if nameSpaceURL = XMLNamespaceURL_MyExtensions then begin
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
          end else if nameSpaceURL = 'http://jsoniq.org/functions' then
            case token of
              'jsoniq-boolean-and-null-literals':
                case temp of
                  'yes': options.AllowJSONLiterals:=true;
                  'no': options.AllowJSONLiterals:=false;
                  else raiseParsingError('XQST0013', 'Unknown option value: '+temp+' for '+token+' (allowed is yes/no)');
                end;
            end;
        end;
        '': begin
          pos := marker;
          break;
        end;
      end;
      expect(';');
      token := nextToken(true);
    end;



    if result = nil then result := parsePrimaryLevel()
    else if staticContext.moduleNamespace = nil then begin //main module
        TXQTermModule(result).push(parsePrimaryLevel);
        if TXQTermModule(result).children[high(TXQTermModule(result).children)] = nil then //huh? module only, no main expression
          raiseParsingError('XPST0003', 'A main module must have a query body, it cannot only declare functions/variables (add ; ())');
    end else if nextToken() <> '' then raiseSyntaxError('Module should have ended, but input query did not');
  except
    result.free;
    if staticContext.sender.AutomaticallyRegisterParsedModules and (resultquery <> nil) then begin
      resultquery._AddRef; //increase ref, so we can remove it from FModules without freeing. Cannot free it here, since the caller still has a reference to the object (but not the interface)
      staticContext.sender.FModules.Remove(IXQuery(resultquery));
    end;
    raise;
  end;
  if staticContext.sender.AutomaticallyRegisterParsedModules and (resultquery <> nil) and (result is TXQTermModule) and (staticContext.moduleNamespace <> nil)  then begin
    resultquery.fterm := result;
    resultquery.initializeStaticContext(staticContext.sender.getEvaluationContext(staticContext));
  end;

  if result <> nil then begin
    if options.AllowJSONLiterals then
      TJSONLiteralReplaceVisitor.startVisiting(@result);
  end;
end;


{ TJSONLiteralReplaceVisitor }

function TJSONLiteralReplaceVisitor.visit(t: PXQTerm): TXQTerm_VisitAction;
var
  i: Integer;
begin
  result := xqtvaContinue;
  if (t^ is TXQTermNodeMatcher) and (length(TXQTermNodeMatcher(t^).children) = 0)
     and ((TXQTermNodeMatcher(t^).namespaceCheck <> xqnmNone {this would be *:true}) and (TXQTermNodeMatcher(t^).namespaceURLOrPrefix = '')) then begin
    case TXQTermNodeMatcher(t^).select of
      'true': begin t^.free; t^ := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'true', 0); end;
      'false': begin t^.free; t^ := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'false', 0); end;
      'null': if GlobalStaticNamespaces.namespaces['jn'] <> nil then begin t^.free; t^ := TXQTermNamedFunction.create(GlobalStaticNamespaces.namespaces['jn'].getURL, 'null', 0); end;
    end;
    exit(xqtvaNoRecursion);
  end;
  if t^ is TXQTermBinaryOp then begin
    if (TXQTermBinaryOp(t^).op.name = '/') or (TXQTermBinaryOp(t^).op.name = '//') then begin
      for i := 0 to high(TXQTermNodeMatcher(t^).children) do
        if not (TXQTermNodeMatcher(t^).children[i] is TXQTermNodeMatcher) then self.simpleTermVisit(@TXQTermNodeMatcher(t^).children[i], t^);
      exit(xqtvaNoRecursion);
    end;
  end;
end;

