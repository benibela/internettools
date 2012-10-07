

{ TXQTerm }

function TXQTerm.debugTermToString: string;
var
  i: Integer;
begin
  result := '(' + ClassName;
  if length(children) >= 1 then result += ': '+children[0].debugTermToString();
  for i:= 1 to high(children) do result += ', '+children[1].debugTermToString();
  result += ')';
end;

destructor TXQTerm.destroy;
var
  i: Integer;
begin
  for i := 0 to high(children) do children[i].free;
  inherited destroy;
end;

procedure TXQTerm.push(t: TXQTerm);
begin
  SetLength(children, length(children) + 1);
  children[high(children)] := t;
end;

function TXQTerm.push(t: array of TXQTerm): TXQTerm;
var
  i: Integer;
begin
  for i := 0 to high(t) do push(t[i]);
  result := self;
end;

procedure TXQTerm.raiseParsingError(const s: string);
begin
  raise Exception.Create(s);
end;

procedure TXQTerm.raiseEvaluationError(const s: string);
begin
  raise EXQEvaluationException.Create(s);
end;

procedure TXQTerm.evaluateChildren(const context: TEvaluationContext; out results: TXQVArray);
var
  i: Integer;
begin
  setlength(results, length(children));
  for i:=0 to high(children) do
    results[i] := children[i].evaluate(context);
end;

function TXQTerm.toQueryCommand: TXQPathMatchingStep;
begin
  result.typ:=qcFunctionSpecialCase;
  result.specialCase:=self;
end;

procedure TXQTerm.addToQueryList(var path: TXQPathMatching);
begin
  SetLength(path, length(path) + 1);
  path[high(path)] := toQueryCommand;
end;

type

{ TXQParsingContext }

 TXQParsingContext = class
  AllowVariableUseInStringLiterals: boolean;

  str: string;
  pos: pchar;

  procedure raiseParsingError(s: string);

  procedure skipComment();
  procedure skipWhitespaceAndComment();
  procedure expect(c: char);
  procedure expect(s: string);
  function nextToken(lookahead: boolean=false): string;

  function parseSequenceLike(target: TXQTerm): TXQTerm;
  function parseQNameContinuation: string;

  //**Parses the next complete value@br
  //**i.e. it will parse from the left, until it obtains a value that can be evaluated.
  //**from @code(1 + 2) it will parse @code(1) (since @code(1) is complete), from @code(function(1,2,3)) it will parse everything (since @code(function) can not be evaluated without its parameters))@br
  //**(observation: that kind of looks like it parses everything except binary operators)
  function parseValue: TXQTerm;
  function parse: TXQTerm;
end;



procedure TXQParsingContext.raiseParsingError(s: string);
begin
  if (pos < @str[1]) then pos := @str[1]; //make sure pos points to str
  if (pos > @str[length(str)]) then pos := @str[1];
  raise Exception.Create(s+#13#10'in: '+strslice(@str[1],pos-1)+' [<- error occurs before here] '+strslice(pos,@str[length(str)]));
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
      raise Exception.Create('"'+c+'"'+' expected, but "'+pos^+'" found'#13#10+strslice(@str[1],pos));
    pos+=1;
end;

procedure TXQParsingContext.expect(s: string);
var
   i: Integer;
begin
  skipWhitespaceAndComment;
  for i:=1 to length(s) do begin
    if pos^ <> s[i] then
      raise Exception.Create('"'+ s+'" expected, but "'+pos^+'" found'#13#10+strslice(@str[1],pos));
    pos+=1;
  end;
end;

//read the next token ('string', number: (-?[0-9]+|[0-9]*.[0-9]+|[0-9]+.[0-9]*)([eE][+-]?[0-9]+)?, symbol, identifier)
function TXQParsingContext.nextToken(lookahead: boolean=false): string;
const SYMBOLS = ['''','"', '(','=','!','<','>',')',',','[',']','/','|','+','*']; // $ and ; are no symbols
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
        if numberPoint then raiseParsingError('Double . in number');
        numberPoint:=true;
        pos+=1;
      end;
      if pos^ in ['e','E'] then begin
        if numberE then raiseParsingError('Double e in number');
        pos+=1;
        numberE:=true;
        if not (pos^ in ['0'..'9','+','-']) then raiseParsingError('Invalid character after e in number')
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

function TXQParsingContext.parseSequenceLike(target: TXQTerm): TXQTerm;
var
  t: String;
begin
  result := target;
  skipWhitespaceAndComment();
  if pos^ = ')' then begin expect(')'); exit(); end;
  result.push(parse());
  t := nextToken();
  while t = ',' do begin
    result.push(parse());
    t := nextToken();
  end;
  if t <> ')' then raiseParsingError('Expected closing parenthesis: )');
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

function TXQParsingContext.parseValue: TXQTerm;

  function parseString(const w: string): TXQTerm;
  var s: string;
      lastStrStart, i: integer;
  begin
    s := StringReplace(copy(w,2,length(w)-2), w[1]+w[1], w[1], [rfReplaceAll]);
    if (not AllowVariableUseInStringLiterals) or (w[1] = '''') or (system.Pos('$', s) <= 0)  then
      exit(TXQTermString.create(s));

    result := TXQTermNamedFunction.create('concat');
    i := 1;
    lastStrStart := 1;
    while  i <= length(s) do begin
      if s[i] = '$' then begin //Read variable
        if lastStrStart < i then result.push(TXQTermString.create(copy(s, lastStrStart, i-lastStrStart)));
        if i >= length(s) - 1 then raiseParsingError('Variable without name or ; at end of '+s);
        lastStrStart:=i;
        while (i <= length(s)) and not (s[i] in [';',' ',#9,#13,#10]) do i+=1;
        if i > length(s) then raiseParsingError('Variable without terminating ; in '+s);
        if lastStrStart  = i then raiseParsingError('Variable without name ($;) in '+s);
        result.push(TXQTermVariable.create(copy(s, lastStrStart, i-lastStrStart)));
        i+=1;
        lastStrStart:=i;
      end else i+=1;
    end;
    if lastStrStart < length(str) then result.push(TXQTermString.Create(copy(s, lastStrStart, i - lastStrStart)));
  end;

var
  word: String;
  wordlookahead: String;
begin
  result := nil;
  word:=nextToken();
  if word='' then exit();
  case word[1] of
    '''', '"':  exit(parseString(word));
    '$': begin
      if length(word) < 2 then raiseParsingError('Variable must have at least 2 characters: '+word);
      exit(TXQTermVariable.Create(word));
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
        exit(TXQTermUnaryOp.create(word, parseValue()));
    end;

    '(': exit(parseSequenceLike(TXQTermSequence.Create)); //only sequence or priority brackets

    '*': exit(TXQTermNodeMatcher.Create(word + parseQNameContinuation));
    '/': begin
      if pos^ = '/' then begin expect('/'); word+='/';end;
      skipWhitespaceAndComment();
      if pos^ in [#0,',',')',']'] then exit(TXQTermNodeMatcher.Create('/')) //leading lone slash (see standard#parse-note-leading-lone-slash)
      else exit(TXQTermBinaryOp.Create(word, TXQTermNodeMatcher.Create('/'), parseValue()));
    end;

    '0'..'9': exit(TXQTermNumber.Create(word));
    '.': if word = '.' then exit(TXQTermNamedFunction.Create('.'))
         else if word = '..' then exit(TXQTermNodeMatcher.Create(word))
         else if word[2] in ['0'..'9', 'e', 'E'] then exit(TXQTermNumber.Create(word))
         else raiseParsingError('Unknown term: '+word);

    else begin
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


      if strBeginsWith(word,'attribute::') then begin
        if strEndsWith(word, ':node') then begin
          expect('('); expect(')');
          exit(TXQTermReadAttribute.create('*'));
        end;
        assert(wordlookahead <> '(');
        exit(TXQTermReadAttribute.Create(strcopyfrom(word,length('attribute::')+1)));
      end;

      if wordlookahead = '(' then begin
        expect('(');
        if word = 'if' then begin
          result := TXQTermIf.Create();
          result.push(parse());
          expect(')'); expect('then');
          result.push(parse());
          expect('else');
          result.push(parse());
          exit;
        end;

        result := TXQTermNamedFunction.createIfExists(word, strBeginsWith(word, 'op:'));
        if (result = nil) and
           (isKindTestFunction(word)
           or (strContains(word, '::') and (not strBeginsWith(word,'attribute')) and isKindTestFunction(strCopyFrom(word, strIndexOf(word, '::') + 2)))) then
          result := TXQTermNodeMatcher.Create(word, true);
        if result = nil then raise Exception.Create('unknown function: '+word);
        exit(parseSequenceLike(result));
      end;

      if (wordlookahead<>'') and (wordlookahead[1]='$') then begin
        if word = 'for' then begin
          result := TXQTermFor.Create;
          result.push(TXQTermVariable.Create(nextToken())); expect('in'); result.push(parse());
          word := nextToken();
          while word = ',' do begin
            result.push(TXQTermVariable.Create(nextToken())); expect('in'); result.push(parse());
            word := nextToken();
          end;
          Assert(word = 'return');
          result.push(parse());
          exit;
        end;

        if (word = 'some') or (word = 'every') then begin
          result := TXQTermSomeEvery.Create(word = 'every');
          result.push(TXQTermVariable.Create(nextToken())); expect('in'); result.push(parse());
          word := nextToken();
          while word = ',' do begin
            result.push(TXQTermVariable.Create(nextToken())); expect('in'); result.push(parse());
            word := nextToken();
          end;
          Assert(word = 'satisfies');
          result.push(parse());
          exit;
        end;
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
  if not (term^ is TXQTermBinaryOp) or (TXQTermBinaryOp(term^).operatorInfo^.priority >= prio) then result := term
  else result := ripBinOpApart(@term^.children[1], prio);
end;

function TXQParsingContext.parse: TXQTerm;
var astroot: TXQTerm;


  procedure pushBinaryOp(const v: string);
        function readSequenceType(): TXQTermType;  //read SequenceType of the xpathebnf and pushes it as tType ()
        var word, sequenceType: string;
        begin
          word := nextToken();
          sequenceType := word;
          if (isKindTestFunction(word)) or (word = 'empty-sequence') then begin
            repeat
              word := nextToken();
              sequenceType += word;
            until (word = ')') or (word = '');
          end else begin
            if sequenceType = 'item' then begin
              expect('('); expect(')');
              sequenceType+='()';
            end else if strBeginsWith(sequenceType, 'xs:') then delete(sequenceType, 1, 3);
            word := nextToken(true);
            if (length(word) = 1) and (word[1] in ['?', '*', '+']) then begin
              sequenceType+=word;
              expect(word);
            end;
          end;
          result := TXQTermType.Create(sequenceType);
        end;
  var res: TXQTermBinaryOp;
      oldres: TXQTermBinaryOp;
      replace: PXQTerm;
  begin
    expect(v);

    res := TXQTermBinaryOp.Create(v);
    replace := ripBinOpApart(@astroot, res.operatorInfo^.priority);

    res.push(replace^);
    replace^ := res;

    if res.operatorInfo^.followedBy <> '' then begin
      expect(res.operatorInfo^.followedBy);
      res.push(readSequenceType()); //assume we read instance of/cast/castable/treat as
    end else
      res.push(parseValue())
  end;

  var word: string;
    replace: PXQTerm;
begin
  astroot := parseValue();
  while true do begin
    word := nextToken(true);
    case word of
      '', ',', ')', ']', 'else', 'return', 'satisfies': exit(astroot);
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
        expect(']');   //todo: object
      end;
      ':=': begin expect(':='); exit(TXQTermDefineVariable.Create(astroot, parse())); end;
      else if binaryOps.IndexOf(word) >= 0 then pushBinaryOp(word)
      else if word[1] = '.' then begin
        expect(word);
        replace := ripBinOpApart(@astroot, 10000);
        if (replace^ is TXQTermFilterSequence) or (replace^ is TXQTermSequence) or (replace^ is TXQTermVariable) or (replace^ is TXQTermNamedFunction)  then
          replace^ := TXQTermReadObjectProperty.Create(strCopyFrom(word, 2)).push([replace^])
         else
          raiseParsingError('Unexpected .');
      end else raise exception.Create('Unknown operator: '+word);
    end;
  end;
  result := astroot;
end;

