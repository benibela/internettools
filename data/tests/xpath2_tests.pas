unit xpath2_tests;

{$mode objfpc}{$H+}

{$ifndef cpuarm}{$define TEST_FLOAT}{$endif}
{$WARN 6018 off : Unreachable code}
interface

uses
  Classes, SysUtils, strutils;


procedure unittests(TestErrors:boolean);


implementation

uses xquery, internetaccess, simplehtmltreeparser, bbutils, xquery_json, xquery__regex, commontestutils, xquery.namespaces, xquery.internals.protectionbreakers, xquery.internals.common;



function collection({%H-}fakeself, {%H-}sender: TObject; const variable: string; var value: IXQValue): boolean;
begin
  result := variable = '';
  if result then
    value := xqvalue('foobar');
end;

const strictTypeChecking = true;

procedure unittests(TestErrors:boolean);
var
  testid,i: Integer;
  ps: TXQueryEngine;
  xml: TTreeParser;

  procedure performUnitTest(s1,s2,s3: string);
  var got: string;
    rooted: Boolean;
    context: TXQEvaluationContext;
  begin
    testid+=1;
    context := ps.getEvaluationContext();
    if s3 <> '' then begin
      rooted := s3[1] = '!';
      if rooted then s3[1] := ' ';
      xml.parseTree(s3);
      if rooted then context.RootElement := xml.getLastTree
      else context.RootElement:=nil;
      if s1 = '' then exit;
    end;
    ps.parseQuery(s1, xqpmXPath2);
    if ps.LastQuery.getTerm <> nil then ps.LastQuery.getTerm.getContextDependencies;
//    if strContains(s1, '/') then writeln(s1, ': ', ps.debugTermToString(ps.FCurTerm));
    context.ParentElement := xml.getLastTree;
//    writeln(s1);
//    writeln('??');
//    writeln(ps.debugtermToString(ps.FCurTerm));
    got := ps.evaluate(context).toString;
    if got<>s2 then
        raise Exception.Create('XPath Test failed: '+IntToStr(testid)+ ': '+s1+#13#10'got: "'+got+'" expected "'+s2+'". Parsed query: '+ps.LastQuery.Term.debugTermToString);
  end;

  procedure t(a,b: string; c: string = '');
  begin
    try
    performUnitTest(a,b,c);

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      //writeln(e.Message);
      raise;
    end end;
  end;

  procedure tdouble(a,b:string; c: string = '');
  begin
    {$ifdef FPC_HAS_TYPE_EXTENDED}
    t(a,b,c);
    {$else}
    //these tests need decimal->double casting, which is implemented naively in TXSNumericType.tryCreateValue
    //to get rid of rounding errors we cast decimal->extended->double. without extended type the rounding fails the tests
    {$endif}
  end;
  procedure tdoubleOrInvalid(a,b:string);
  begin
    try
      t(a, b);
    except on e: exception do begin
      if (e is EXQEvaluationException) and (EXQEvaluationException(e).errorCode = 'FOAR0002') then exit
      else raise;
    end; end;
  end;

  procedure f(a, code: string; c: string = '');
   var
     err: string;
   begin
     if not TestErrors then exit;
     err := '-';
     try
     performUnitTest(a,'<error>',c);

     except on e: EXQEvaluationException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end; on e: EXQParsingException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end end;
     if err = '' then raise Exception.Create('No error => Test failed ');
     if (err <> code) and (err <> 'err:'+code)  then raise Exception.Create('Wrong error, expected '+code+ ' got '+err+LineEnding+'Test: '+a);
   end;

//var  time: TDateTime;
var tempb: Boolean;
  tt: String = '';
  j, k: Integer;
  baseboundary: String;
  func: String;
  untypedAtomic: String;
  xqv,xqw: IXQValue;
  randomboundary, bce1, bce2: String;
  iterator, iterator2: TXQValueEnumeratorPtrUnsafe;
  nbsp: string;
begin
  testid := 0;
//  time := Now;
  //vars:= TXQVariableChangeLog.create();
  if strictTypeChecking then untypedAtomic := 'xs:untypedAtomic'
  else untypedAtomic := '';

  ps := TXQueryEngine.Create;
  ps.StaticContext.model := xqpmXPath2;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.StaticContext.useLocalNamespaces:=false;
  ps.ImplicitTimezoneInMinutes:=-5 * 60;

  ps.VariableChangelog.add('abc', 'alphabet');
  ps.VariableChangelog.add('test', 'tset');
  ps.VariableChangelog.add('eval', '''abc'' = ''abc''');

  ps.OnCollection := TXQEvaluateVariableEvent(procedureToMethod(TProcedure(@collection)));

  //ps.OnEvaluateVariable:=@vars.evaluateVariable;
  //ps.OnDefineVariable:=@vars.defineVariable;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;
  xml.repairMissingStartTags:=false;
  //xml.TargetEncoding:=CP_NONE;
  xml.trimText:=true;

  f('',                          'XPST0003');
  t('''''',                      '',                                 '');
  t('''Test''',                  'Test',                             '');
  t(#9'   ''xyz''     '#13#10,   'xyz',                              '');
  t(''''#9'xyz'#13'''',           #9'xyz'#10,                        '');
  ps.ParsingOptions.LineEndingNormalization := xqlenNone;
  t(''''#9'xyz'#13'''',           #9'xyz'#13,                        '');
  ps.ParsingOptions.LineEndingNormalization := xqlenXML1;
  t('"abc"',                     'abc',                              '');
  t('"''"',                      '''',                               '');
  t('"He said, ""I don''t like it."""', 'He said, "I don''t like it."', '');
  t('''He said, "I don''''t like it."''', 'He said, "I don''t like it."', '');


  f('12 div3', 'XPST0003');
  f('12div 3', 'XPST0003');
  f('12 div-3', 'XPST0003');

                //Variable tests
  t('"$$;"',                   '$$;',                            '');
  t('">$$;<"',                 '>$$;<',                          '');


  // t('$abc;',                     'alphabet',                     '');
  t('$abc',                     'alphabet',                     '');


  t('concat(">",$abc,''<'')',  '>alphabet<',                     '');
  t('''$abc;''',                   '$abc;',                        ''); //no variable matching in '
  t('"$abc;"',                   '$abc;',                        ''); //no variable matching in "
  t('"&quot;"',                   '&quot;',                        '');
  t('''&quot;''',                 '&quot;',                        '');
  t('"x&quot;y"',                   'x&quot;y',                        '');
  t('''x&quot;y''',                 'x&quot;y',                        '');

  t('get("abc")', 'alphabet');
  t('get("test")', 'tset');
  t('get("unknown")', '');
  t('get("unknown", 42)', '42');
  t('concat($test :=123, get("test"))', '123123');
  t('concat(test :=456, get("test"))', '456456');

  f('$ABC;', 'err:XPST0003');
  f('$ABC', 'err:XPST0008');
  f('x"{$ABC}"', 'err:XPST0008');

  t('x"{$abc}"',                   'alphabet',                        ''); //variable matching in x"
  t('x''{$abc}''',                   'alphabet',                        ''); //variable matching in x"

  t('x"$$;"',                   '$$;',                           '');
  t('x">$$;<"',                 '>$$;<',                         '');
  t('concat($amp, $line-ending)',     '&' + LineEnding); //default variables

  f('x">{$unknown}<"', 'err:XPST0008');
  f('x"{$test}>{$unknown}<"', 'err:XPST0008');
  f('x"{$test}{$unknown}{$abc}"', 'err:XPST0008');

  //more extended strings
  t('x"123"', '123');
  t('x"{{123}}"', '{123}');
  t('x"123{4+5+6}"', '12315');
  t('x"123{4+5+6}789"', '12315789');
  t('x"{1+2}{3+4}{5+6}"', '3711');
  t('x"{1+2}{3+4}{{5+6}}"', '37{5+6}');
  t('x"{{1+2}}{{3+4}}{{5+6}}"', '{1+2}{3+4}{5+6}');
  t('x"""{1+2}""{3+4}""{5+6}"""', '"3"7"11"');
  f('x"1234+5+6}"', 'pxp:XPST0003'); //single } not allowed
  f('x"123{4+5+6}}"', 'pxp:XPST0003');
  t('x"123{4+5+6}}}"', '12315}');
  t('x"123" + 100 + x"77000"', '77223');

  t('x''123''', '123');
  t('x''123{4+5+6}''', '12315');
  t('x''123{4+5+6}789''', '12315789');
  t('x''{1+2}{3+4}{5+6}''', '3711');
  t('x''{{1+2}}{{3+4}}{{5+6}}''', '{1+2}{3+4}{5+6}');
  t('x''''''{1+2}''''{3+4}''''{5+6}''''''', '''3''7''11''');
  f('x''1234+5+6}''', 'pxp:XPST0003'); //single } not allowed
  f('x''123{4+5+6}}''', 'pxp:XPST0003');
  t('x''123{4+5+6}}}''', '12315}');
  t('x''123'' + 100 + x''77000''', '77223');

  t('type-of(x"{1}")', 'string');
  t('x"{(1,2,3)}"', '1 2 3');
  t('x"{("1","2"," 3")}"', '1 2  3');
  t('x">{(1,2,3)}<"', '>1 2 3<');

  //change base xml used for tests
  t('','', '<html attrib1="FIRST ATTRIBUTE" attrib2="SECOND ATTRIBUTE" attrib3="THIRD ATTRIBUTE">test:last text<!--comment!--><deep>:BEEP</deep>'#13#10+
          '<adv><table id="t1"><tr><td><!-- cya -->first col</td><td bgcolor="red">2nd col</td></tr></table>'#13#10+
                '<table id="t2"><tr><td colspan=3><blink>OMG!!</blink></td></tr><tr><td>A</td><td>B</td><td empty="">C</td></tr></table></adv></html> ');

                //XPath like HTML Reading
  t('html/text()',               'test:last text',                   '');
  t('html/comment()',            'comment!',                         '');
  t('html/@attrib1',             'FIRST ATTRIBUTE',                  '');
  t('html/@attrib2',             'SECOND ATTRIBUTE',                 '');
  t('html/@attrib3',             'THIRD ATTRIBUTE',                  '');
  t('html/adv/text()',           '',                                 '');
  t('string-join(html/adv/table/deep-text('' ''), "|")','first col 2nd col|OMG!! A B C'); //additional spaces!
  t('string-join(html/adv/table/inner-text(), "|")','first col'#9'2nd col|OMG!!'#10#9'A'#9'B'#9'C');
  t('string-join(html/adv/table/inner-text(tr/td[2]), "|")','2nd col|B');
  t('string-join(html/adv/table/@id, "|")',        't1|t2');
  t('string-join(html/adv/table/tr/td/text(), "|")', 'first col|2nd col|A|B|C');
  t('html/adv/table/tr/td/comment()', 'cya');
  t('html/adv/table[@id=''t2'']/@id','t2');
  t('html/adv/table[@id=''t2'']/tr/td/@colspan','3');
  t('html/adv/table[@id=''t2'']/tr/td/text()','ABC'); //if this fails with OMG!! direct child also matches a direct grand child
  t('html/adv/table[@id=''t2'']/tr/td/deep-text('' '')','OMG!!ABC');
  t('html/adv/table[@id=''t2'']/tr/td[@colspan!=''3'']/text()','',''); //not existing property != 3

                //Comparison tests
                //('''a == b''',                'a == b',                           ''),
                //('''a'' == ''b''',            'false',                            ''),
                //('''abc'' == ''abc''',        'true',                             ''),
  t('''123'' != ''abc''',        'true',                             '');
  t('''a = b''',                'a = b',                           '');
  t('''a'' = ''b''',            'false',                            '');
  t('''abc'' = ''abc''',        'true',                             '');
  t('''123'' != ''abc''',        'true',                             '');
                //('''$test;''==''abc''',       'false',                            ''),
                //Concatenation tests
  t('concat(''a'',''b'',''c'')', 'abc',                              '');
  f('concat(''one'')',           'err:XPST0017');
  t('concat(''hallo'', '' '', ''welt'') = ''hallo welt''',  'true', '');
  t('concat  (  ''a'',  ''b'',  ''c''  )',                   'abc',  '');
  t('concat(''a'',''b'',concat(''c'',''d''))',               'abcd', '');
  t('concat(''a'',concat(''x'',''y'',''z''),''b'',''c'')',   'axyzbc','');
                //Concatenation + Comparison tests (double as stack test)
  t('concat(''cond is '',''abc''=''abc'')',                 'cond is true', '');
  t('concat(''>'',''123''!=''test'',''<'')',                 '>true<', '');
  t('concat(concat(''123'',''abc'')=''123abc'',''-#-'')',   'true-#-', '');
  t('concat(''('',''abc''=concat(''a'',''b'',''c''),'')'')','(true)', '');
                //Undefined/empty set
  t('html/adv/table[@id=''t2'']/tr/td[@not=@bot]/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[@not!=@bot]/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[@not='''']/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[@not!='''']/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[exists(@not)]/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[exists(@colspan)]/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[exists(@not)]/blink/text()','','');
  t('html/adv/table[@id=''t2'']/tr/td[exists(@colspan)]/blink/text()','OMG!!','');
  t('html/adv/table/tr/td[exists(@bgcolor)]/text()','2nd col','');
  t('html/adv/table[@id="t2"]/tr/td[exists(@empty)]/text()','C','');
  t('html/adv/table[@id="t2"]/tr/td[@empty='''']/text()','C','');
  t('html/adv/table[@id="t2"]/tr/td[@empty!='''']/text()','','');


                //Regex-Filter
  t('extract(''modern'', ''oder'')',                 'oder',            '');
  t('extract(''regex'', ''.g.'')',                   'ege',             '');
  //t('extract(''reg123ex'', ''[0-9]*'')',             '',                '');
  t('extract(''reg123ex'', ''[0-9]+'')',             '123',             '');
  t('extract(''regexREGEX'', ''.G.'')',              'EGE',             '');
  t('extract(''abcdxabcdefx'', ''b[^x]*'')',         'bcd',             '');
  t('extract(''hallo welt'', ''(.*) (.*)'')',        'hallo welt',      '');
  t('extract(''hallo welt'', ''(.*) (.*)'', ''0'')', 'hallo welt',      '');
  t('extract(''hallo welt'', ''(.*) (.*)'', ''1'')', 'hallo',           '');
  t('extract(''hallo welt'', ''(.*) (.*)'', ''2'')', 'welt',            '');
  t('exists(extract(''hallo welt'', ''(.*) (.*)'', ()))', 'false',      '');
  t('string-join( extract(''hallo welt'', ''(.*) (.*)'', (1,2) ), ":")', 'hallo:welt');
  t('string-join( extract(''hallo welt foo bar'', ''(.*) (.*) (.*) (.*)'', (1,3,4,1,2,1) ), ":")', 'hallo:foo:bar:hallo:welt:hallo');
  t('join(extract("foo bar", "[ao]", 0, "*"))', 'o o a');
  t('join(extract("foo bar", "[^ ](.(.))", (1, 2), "*"))', 'oo o ar r');
  t('join(extract("foo bar", "[^ ](.(.))", (1, 2)))', 'oo o');
  t('count(extract("foo bar", "NOMATCH", (1, 2), "*"))', '0');
  t('count(extract("foo bar", "NOMATCH", (1, 2), ""))', '2');
  t('count(extract("foo bar", "NOMATCH", (-2), "*"))', '0');
  t('count(extract("foo bar", "NOMATCH", (-2), ""))', '1');

  if xquery__regex.UsingFLRE then begin
    t('matches("foo|barXr", "foo \| bar\Dr", "x")', 'true');
    t('extract("abc_-123.·:+", "\c+")', 'abc_-123.·:');
    t('extract("foobarfoobarrrbaba1", "foo(ba)r+\1\11")', 'foobarrrbaba1');
    t('extract("a-aa-ba-c", "[abcde][^a-z][abc-[abc-[c]]]")', 'a-c');
    t('extract("^ay---", "[^^][a-k-z][\--x][x\--[x]]")', 'y---');
    //t('extract("^ay-- --y--", "[x--[x]]")', '-'); //not sure about this one
    t('fn:matches("helloworld", "hello world", "x") ' , 'true');
    t('fn:matches("helloworld", "hello[ ]world", "x") ' , 'false');
    t('fn:matches("hello world", "hello\ sworld", "x") ' , 'true');
    t('fn:matches("hello world", "hello world", "x") ' , 'false');
    t('fn:matches("foo", "\P{Sk }", "x")' , 'true');

    f('matches("", "[^]")', 'FORX0002');
    f('matches("", "[--x]")', 'FORX0002');
    f('matches("", "[x--]")', 'FORX0002');
    f('matches("", "[---]")', 'FORX0002');

    t('matches("axax", "(a\p{Lu})\1", "i")', 'false');
    t('matches("aXax", "(a\p{Lu})\1", "i")', 'true');

    t('matches("axbcde", "((a\p{Lu}bcde)\2)", "i")', 'false');
    t('matches("aXbcdeaxbcde", "((a\p{Lu}bcde)\2)", "i")', 'true');
    t('matches("aXbcdeabcde", "((a\p{Lu}bcde)\2)", "i")', 'false');

    t('matches("12aubcdfoobareaubcdfoobaredfoobar", "12((a\p{Lu}bc(dfoobar)e)\2)\3", "i")', 'false');
    t('matches("12aUbcdfoobareaubcdfooBAReDFOOBAR", "12((a\p{Lu}bc(dfoobar)e)\2)\3", "i")', 'true');
    t('matches("12aUbcdfoobareaubcdfooBAReDFOOBAR---aa", "12((a\p{Lu}bc(dfoobar)e)\2)\3---(((\p{Lu})))\5", "i")', 'false');
    t('matches("12aUbcdfoobareaubcdfooBAReDFOOBAR---Aa", "12((a\p{Lu}bc(dfoobar)e)\2)\3---(((\p{Lu})))\5", "i")', 'true');

  end;

                //Replace
  t('replace("abracadabra", "bra", "*")', 'a*cada*', '');
  t('replace("abracadabra", "a.*a", "*")', '*', '');
  t('replace("abracadabra", "a.*?a", "*")', '*c*bra', '');
  t('replace("abracadabra", "a", "")', 'brcdbr', '');
  t('replace("abracadabra", "a(.)", ''a$1$1'')', 'abbraccaddabbra', '');
  //t('replace("abracadabra", ".*?", ''$1'')', 'abracadabra', ''); //should cause error
  t('replace("AAAA", "A+", "b")', 'b', '');
  t('replace("AAAA", "A+?", "b")', 'bbbb', '');
  t('replace("darted", ''^(.*?)d(.*)$'', ''$1c$2'')', 'carted', '');
  t('replace("AAAA", "a+", "b")', 'AAAA', '');
  t('replace("AAAA", "a+", "b", ''i'')', 'b', '');

  t('translate("bar","abc","ABC")', 'BAr', '');
  t('translate("--aaa--","abc-","ABC")', 'AAA', '');
  t('translate("abcdabc", "abc", "AB")', 'ABdAB', '');
  t('translate("abcdabc", "abc", "bca")', 'bcadbca', '');

                //Eval,
  t('''html/text()''', 'html/text()', '');
  t('eval(''html/text()'')', 'test:last text', '');
  t('$eval', '''abc'' = ''abc''', '');
  t('eval($eval)', 'true', '');

                //All together
  t('extract(concat(''abc'', ''def''), concat(''[^a'',''d]+'' ))', 'bc','');
  t('concat(''-->'', extract(''miauim'', ''i.*i'') , ''<--'')',   '-->iaui<--','');
  t('extract(''hallo'', ''a'') = ''a''', 'true',                       '');
  t('extract(''hallo'', ''x'') != ''''', 'false',                       '');
  t('extract(html/@attrib1, ''[^ ]+'')', 'FIRST',                            '');
  t('extract(html/@attrib2, ''[^ ]+'')', 'SECOND',                           '');
  t('extract(html/text(), ''[^:]+'')', 'test',                               '');
  t('string-join(tokenize("a,b,c",","), ";")', 'a;b;c',                               '');
                //('extract(''$testvar_t;'', ''$testvar_f;'' == ''true'') == ''true''', 'false',''),
                //('extract(''$testvar_t;'', ''$testvar_t;'' == ''true'') == ''true''', 'true',''),
                //('extract(''$testvar_f;'', ''$testvar_f;'' == ''true'') == ''true''', 'false',''),
                //('extract(''$testvar_f;'', ''$testvar_t;'' == ''true'') == ''true''', 'false','')


  t('inner-xml(a/b)', 'x<t>y<u>++</u></t>z', '<a><aa>aaa</aa><b>x<t>y<u>++</u></t>z</b>233<c>23</c>434<d>434</d></a>');
  t('outer-xml(a/b)', '<b>x<t>y<u>++</u></t>z</b>', '');
  t('a/b/inner-xml()', 'x<t>y<u>++</u></t>z', '');
  t('a/b/outer-xml()', '<b>x<t>y<u>++</u></t>z</b>', '');


                //New tests
  t('', '', '<a><b>Hallo</b><c></c><d>xyz</d><e><f>FFF</f><g>GGG<h>HHH<br/>hhh</h></g></e><e.z>ez</e.z></a>');

                //XPath reading
  t('a/b/text()', 'Hallo', '');
  t('a/c/text()', '', '');
  t('a/d/text()', 'xyz', '');
  t('a//text()', 'HalloxyzFFFGGGHHHhhhez');
  t('string-join(a/*/text(),"|")', 'Hallo|xyz|ez');
  t('a/g/h/text()', '', '');
  t('a/e/g/h/text()', 'HHHhhh', '');
  t('a/e/./g/././h/text()', 'HHHhhh', '');
  t('a/e/g/h/../text()', 'GGG', '');
  t('a/e/../e/../e/../e/g/h/../text()', 'GGG', '');
  t('a//h/text()', 'HHHhhh', '');
  t('a/e.z/text()', 'ez', '');

  //case (in-)sensitivesnes
  t('', '', '<A att1="att1"><b bat="man" BED="SLEEP">Hallo</b><C at="lol" aTt="LOL"></C></a>');

  t('A/b/text()', 'Hallo', '');
  t('a/b/text()', 'Hallo', '');
  t('a/B/text()', 'Hallo', '');
  t('a/@att1', 'att1', '');
  t('a/@ATT1', 'att1', '');
  t('a/attribute::att1', 'att1', '');
  t('a/attribute::aTt1', 'att1', '');
  t('a//B/@BAT', 'man', '');
  t('a//B/@bed', 'SLEEP', '');
  t('a//B/attribute::bed', 'SLEEP', '');
  t('a/B[@bat=''man'']/text()', 'Hallo', '');
  t('a/B[@BAT=''man'']/text()', 'Hallo', '');
  t('a/B[@bat=''MAN'']/text()', 'Hallo', ''); //comparison is also case-insensitive!
  t('a/B[@bat=''MEN'']/text()', '', '');
  t('a/B[''TRUE'']/text()', 'Hallo', '');
  t('a/B[''true'']/text()', 'Hallo', '');
  t('a/B[''FALSE'']/text()', 'Hallo', '');
  t('a/B['''']/text()', '', '');
  t('''A''=''a''', 'true', '');
  t('  ''A''  =  ''a''  ', 'true', '');
  t('A/attribute::*', 'att1', '');
  t('A/b/attribute::*', 'manSLEEP', '');
  t('(A/b/attribute::*)[1]', 'man', '');
  t('(A/b/attribute::*)[2]', 'SLEEP', '');
  t('A/@*', 'att1', '');
  t('A/b/@*', 'manSLEEP', '');
  t('A/b/@*[1]', 'man', '');
  t('A/b/@*[2]', 'SLEEP', '');
  t('A/b/@*[3]', '', '');

  //attributes without value
  t('', '', '<r><x attrib1>hallo</x><x attrib2>mamu</x><x attrib3 test="test">three</x><x x=y attrib4>four</x><x v="five" attrib5/><x v="six" attrib6/></r>');

  t('r/x[exists(@attrib1)]/text()', 'hallo', '');
  t('r/x[exists(@attrib2)]/text()', 'mamu', '');
  t('r/x[exists(@attrib3)]/text()', 'three', '');
  t('r/x[exists(@attrib4)]/text()', 'four', '');
  t('r/x[@attrib1=""]/text()', 'hallo', '');
  t('r/x[@attrib1!=""]/text()', '', '');
  t('r/x[@attrib1="attrib1"]/text()', '', '');
  t('r/x[@x=''y'']/text()', 'four', '');
  t('r/x[exists(@attrib5)]/@v', 'five', '');
  t('r/x[exists(@attrib6)]/@v', 'six', '');
  t('r/x[exists(@attrib)]/@v', 'hallo', '<r><x   v="hallo"   attrib=   /></r>');
  t('r/x[exists(@attribx)]/@v', '', '<r><x   v="hallo"   attrib=   /></r>');
  t('r/x[exists(@attrib)]/text()', 'mimp', '<r><x   v="hallo"   attrib=   >mimp</x></r>');
  t('r/x[exists(@attribx)]/text()', '', '<r><x   v="hallo"   attrib=   >dimp</x></r>');
  t('r/x[exists(@attrib)]/text()', 'Ximp', '<r><x   v="hallo"   attrib=>Ximp</x></r>');
  t('r/x[exists(@attribx)]/text()', '', '<r><x   v="hallo"   attrib=>dimp</x></r>');
                //strange attributes
  t('r/a/@href', '/home/some/people/use/this!', '<r><a href=/home/some/people/use/this!>...</a></r>');
  t('r/a/@href', '/omg/some/people/use/this!', '<r><a href=/omg/some/people/use/this!/></r>');
  t('r/text()', 'later', '<r><a href=/omg/some/people/use/this!/>later</r>');
  t('r/a/@href', '/omg/some/people/use/this!', '<r><a href=/omg/some/people/use/this!/>later</r>');
  t('r/a[exists(@wtf)]/@href', '/some/people/use/this!', '<r><a href=/some/people/use/this! wtf/></r>');
                //path rules
  t('a/b/c/../../x/text()', '3', '<a><b><x>1</x><c><x>2</x></c></b><x>3</x></a>');
  t('join(a//(x/text()))', '1 2 3', '');
  t('join(a//string(x/text()))', '3 1   2    ', '');
  t('a//x/text()', '123', ''); //if this returns 3 it is probably evaluated as a//(x/text()) and not ordered
  t('a//x/string(text())', '123', ''); //if this returns 3 it is probably evaluated as a//(x/text()) and not ordered
  t('a/b/c/../..//x/text()', '123', '');
  t('a/b/./c/../../x/text()', '3', '');
  t('html/body/t[@id="right"]/text()', '123', '<html>A<body>B<t>xy</t>C<t id="right">123</t>D</body>E</html>');
  t('html//t[@id="right"]/text()', '123', '');
  t('html/*/t[@id="right"]/text()', '123', '');
  t('html/t[@id="right"]/text()', '', '');
  t('html/body/t[@id="right"]/text()', '', '<html>A<body>B<x>C<t>xy</t>D<t id="right">123</t>E</x>F</body>G</html>');
  t('html//t[@id="right"]/text()', '123', '');
  t('html/*/t[@id="right"]/text()', '', '');
  t('html/*/*/t[@id="right"]/text()', '123', '');
  t('html/t[@id="right"]/text()', '', '');
  t('html/body/x/t[@id="right"]/text()', '123', '');
  t('html/*/x/t[@id="right"]/text()', '123', '');
  t('html/body/*/t[@id="right"]/text()', '123', '');
  t('html/body/t[@id="right"]/text()', '123', '<html><body><t>xy</t><t id="right">123</t></body></html>');
  t('html//t[@id="right"]/text()', '123', '');
  t('html/*/t[@id="right"]/text()', '123', '');
  t('html/t[@id="right"]/text()', '', '');
  t('html/body/t[@id="right"]/text()', '', '<html><body><x><t>xy</t><t id="right">123</t></x></body></html>');
  t('html//t[@id="right"]/text()', '123', '');
  t('html/*/t[@id="right"]/text()', '', '');
  t('html/*/*/t[@id="right"]/text()', '123', '');
  t('html/t[@id="right"]/text()', '', '');
  t('html/body/x/t[@id="right"]/text()', '123', '');
  t('html/*/x/t[@id="right"]/text()', '123', '');
  t('html/body/*/t[@id="right"]/text()', '123', '');
  t('a//d/text()', '4', '<a>1<b>2<c>3<d>4</d>5</c>6</b>7</a>');
                //TODO: check these: http://www.w3.org/TR/xpath20/#abbrev

                //numbers
  t('1234', '1234', '');
  t('1234e-3', '1.234', '');
  tdouble('1234e-4', '0.1234', '');
  t('-1234', '-1234', '');
//  t('-12.34E1', '-123.4', '');
  t('.34E2', '34', '');
  t('-.34E2', '-34', '');
  t('0.34E+3', '340', '');
  t('-42E-1', '-4.2', '');

                //New Comparisons
  t('3<4', 'true', '');
  t('4<4', 'false', '');
  t('-3<3', 'true', '');
  t('3.7<4', 'true', '');
  t('4.00<4', 'false', '');
  t('4.=4', 'true', '');
  t('"maus" <  "maushaus"', 'true', '');
  t('"maus" <  "hausmaus"', 'false', '');
  t('"123" <  "1234"', 'true', '');
  t('"1234" <  "1234"', 'false', '');
  t('"1234" <  "1234.0"', 'true', '');
  t('2 <  "3"', 'true', '');
  t('4 <  "3"', 'false', '');
  t('"2" <  5', 'true', '');
  t('3<=4', 'true', '');
  t('4<=4', 'true', '');
  t('-3<=3', 'true', '');
  t('3<=-3', 'false', '');
  t('"maus"="MAUS"', 'true', '');
  t('"maus"="MAUS4"', 'false', '');
  t('"maus" eq "MAUS"', 'true', '');
  t('"maus" eq "MAUS4"', 'false', '');
  t('"maus"=("MAUS4","abc","mausi")', 'false', '');
  t('"maus"=("MAUS4","abc","maus")', 'true', '');
  t('"maus"<="MAUS"', 'true', '');
  t('"maus">="MAUS"', 'true', '');
  t('"maus"<"MAUS"', 'false', '');
  t('"maus">"MAUS"', 'false', '');
  t('4.00>4', 'false', '');
  t('4.00>=4', 'true', '');
  t('4 eq 7', 'false', '');
  t('4 ne 7', 'true', '');
  t('4 lt 7', 'true', '');
  t('4 lt 4', 'false', '');
  t('4 le 7', 'true', '');
  t('4 gt 7', 'false', '');
  t('4 gt 4', 'false', '');
  t('4 ge 7', 'false', '');
  t('4 ge 4', 'true', '');

                //IEEE special numbers
  t('0e0 div 0e0', 'NaN', '');
  t('1e0 div 0e0', 'INF', '');
  t('-1e0 div 0e0', '-INF', '');
  t('-42 div xs:float("NaN")', 'NaN');

                //comparison+type conversion
  t('5<5.0', 'false', '');
  t('5>5.0', 'false', '');
  t('5=5.0', 'true', '');
  t('5<4.9', 'false', '');
  t('5>=1e-20', 'true', '');
  t('5>=6e-20', 'true', '');
  ps.StaticContext.strictTypeChecking:=strictTypeChecking;
  if strictTypeChecking then begin
    f('5.00<true()', 'XPTY0004');
    f('5.00<false()', 'XPTY0004');
    f('5.00>true()', 'XPTY0004');
    f('5.00>false()', 'XPTY0004');
    f('xs:untypedAtomic("fn:a") eq xs:QName("fn:a")', 'XPTY0004');
    f('xs:untypedAtomic("fn:a") = xs:QName("fn:a")', 'XPTY0004');

  end else begin
    t('5.00<true()', 'false');
    t('5<false()', 'false');
    t('5>true()', 'true');
    t('5.00>false()', 'true');
    t('1.00>true()', 'false');
    t('1.00>false()', 'true');
    t('0.00<true()', 'true');
    t('0.00<false()', 'false');
  end;
  t('10000000000000000000000000000000000000 < xs:untypedAtomic("9999999999999999999999")', 'false');
  t('xs:untypedAtomic("9999999999999999999999") < 10000000000000000000000000000000000000', 'true');


  //Generic comparisons
  t('(1, 2) = (2, 3)', 'true', '');
  t('(1, 2) != (2, 3)', 'true', '');
  t('(2, 3) = (3, 4)', 'true', '');
  t('(1, 2) = (3, 4)', 'false', '');

                //Binary/unary ops
  t('3-3', '0','');
  t('3+3', '6','');
  t('3--3', '6','');
  t('3---4', '-1','');
  t('3 * 2.5', '7.5','');
  t('3 div 2.0', '1.5','');
  t('3.0 idiv 2.0', '1','');
  t('-3.0 idiv 2.0', '-1','');
  t('3 idiv 2', '1','');
  t('-3 idiv 2', '-1','');
  t('3 idiv -2', '-1','');
  t('-3 idiv -2', '1','');
  t('10 idiv 3', '3','');
  t('9.0 idiv 3', '3','');
  t('-3.5 idiv 3', '-1','');
  t('3.0 idiv 4', '0','');
  t('3.1E1 idiv 6', '5','');
  t('3.1E1 idiv 7', '4','');
  t('(2*0.5) div 3', '0.333333333333333333');

  t('3 mod 2', '1','');
  t('-3 mod 2', '-1','');
  t('3 mod -2', '1','');
  t('-3 mod -2', '-1','');
  t('10 mod 3', '1','');
  t('9.0 mod 3', '0','');
  t('-3.5 mod 3', '-0.5','');
  t('3.0 mod 4', '3','');
  t('3.1E1 mod 6', '1','');
  t('3.1E1 mod 7', '3','');
  t('6 mod -2', '0','');
  t('4.5 mod 1.2', '0.9','');
  t('1.23E2 mod 0.6e1', '3','');

  t('3 * 2.0', '6','');
  t('3 = 3.0', 'true','');
  t('3 = + 3.0', 'true','');
  t('3 = - 3.0', 'false','');
  t('3 = ---3.0', 'false','');
  t('3 = --3.0', 'true','');
  t('-3= ---3.0', 'true','');

  t('1 to 3', '123','');
  t('join(1 to 3,",")', '1,2,3','');
  t('join(3 to 1,",")', '','');
  t('join(5 to 5,",")', '5','');
  t('join((10, 1 to 4),",")', '10,1,2,3,4','');
  t('join((10 to 10),",")', '10','');
  t('join((15 to 10),",")', '','');
  t('join(reverse(10 to 15),",")', '15,14,13,12,11,10','');


  t('true() and true()', 'true', '');
  t('false() and true()', 'false', '');
  t('false() and false()', 'false', '');
  t('true() or true()', 'true', '');
  t('false() or true()', 'true', '');
  t('false() or false()', 'false', '');

                //Priorities
  t('2*3 + 1', '7', '');
  t('1 + 2*3', '7', '');
  t('(1 + 2)*3', '9', '');
  t('((1 + 2))*3', '9', '');
  t('((1 + 2))*(1+2)', '9', '');
  t('1+((1 + 2))*(1+2)*(2+1)+1', '29', '');
  t('1 + 2 + 3', '6', '');
  t('1 + 2 * 7 + 3', '18', '');
  t('1 + 2 * 7 * 2 + 3', '32', '');
  t('1 + 2 - 4', '-1', '');
  t('1 - 4 + 6', '3', '');
  t('1 - 2 + 3 - 4', '-2', '');
  t('2 - 1 = 1', 'true', '');
  t('1 - 2 = -1', 'true', '');
  t('2 - 1 = 3', 'false', '');
  t('1 - 2 = -3', 'false', '');
  t('3 + 4 = 2 + 5', 'true', '');
  t('3 + 4 = 2 + 4', 'false', '');
  t('3 + 4 = -2 + 9', 'true', '');
  t('3 + 4 = -3 + 9', 'false', '');
  t('3 + 4 = -3 + 9 or 3 = 3', 'true', '');
  t('3 + 4 = -3 + 9 or 3 = 4', 'false', '');
  t('3 + 4 = -3 + 9 or 1+1+1 = 4', 'false', '');
  t('3 + 4 = -3 + 9 or 1+1+1 = 3', 'true', '');
  t('9223372036854775807', '9223372036854775807', '');
  t('-9223372036854775807', '-9223372036854775807', '');
  t('-  9223372036854775808', '-9223372036854775808', '');

                //Constructors
  t('"6.5" castable as xs:decimal', 'true', '');
  t('xs:decimal("6.5")', '6.5', '');
  t('xs:string("6.5")', '6.5', '');
  t('xs:string("MEMLEAK5")', 'MEMLEAK5', '');
  t('xs:boolean("true")', 'true', '');
  t('xs:decimal(())', '');
  t('xs:integer(xs:decimal("6.5"))', '6');
  t('xs:int(xs:decimal("6.5"))', '6');

  //do not allow lower case
  f('xs:int("6.5")', 'err:FORG0001');
  t('"6" castable as xs:int', 'true', '');
  t('"6.5" castable as xs:int', 'false', '');
  t('xs:decimal("6.5") castable as xs:int', 'true', '');

  f('xs:boolean("6.5")', 'err:FORG0001');
  t('xs:boolean("1")', 'true');
  t('"1" castable as xs:boolean', 'true', '');
  t('"6" castable as xs:boolean', 'false', '');
  t('"6.5" castable as xs:boolean', 'false', '');
  t('xs:decimal("6.5") castable as xs:boolean', 'true');

  t('xs:date("1900-01-01")', '1900-01-01', '');
  f('xs:decimal(xs:date("1900-01-01"))', 'err:XPTY0004'); // todo dynamic error [err:FORG0001]: "1900-01-01": value of type xs:string is not castable to type xs:dateTime |dateTime not castable as decimal
  f('xs:decimal("")', 'err:FORG0001');
  f('xs:decimal()', 'err:XPST0017');
  f('xs:string()', 'err:XPST0017');


  //strange tests
  t('(xs:int(4) + xs:int(2)) instance of xs:int', 'false');
  t('abs(xs:byte(0)) instance of xs:byte', 'false');
  t('ceiling(xs:unsignedInt(0)) instance of xs:unsignedInt', 'false'); //zorba returns true, but spec says " If the type of $arg is a type derived from one of the numeric types, the result is an instance of the base numeric type."
  t('10 cast as xs:integer castable as xs:boolean treat as xs:boolean instance of xs:boolean', 'true');
  f('10 cast as xs:integer castable as xs:boolean instance of xs:boolean cast as xs:boolean', 'XPST0003');


  t('type-of(xs:decimal("6.5"))', 'decimal', '');
  t('type-of(xs:string("6.5"))', 'string', '');
  t('type-of(xs:string("MEMLEAK6"))', 'string', '');
  t('type-of(xs:int("6"))', 'int', '');
  t('type-of(xs:integer("6"))', 'integer', '');
  t('type-of(xs:boolean("1"))', 'boolean', '');
  t('type-of(xs:date("1800-01-01"))', 'date', '');
  f('xs:decimal("INF")', 'err:FORG0001');
  f('xs:decimal("-INF")', 'err:FORG0001');
  f('xs:decimal("NaN")', 'err:FORG0001');
  t('xs:double("INF")', 'INF', '');
  t('xs:double("-INF")', '-INF', '');
  t('xs:double("NaN")', 'NaN', '');
  t('xs:float("INF")', 'INF', '');
  t('xs:float("-INF")', '-INF', '');
  t('xs:float("NaN")', 'NaN', '');

                //Functions
                 //Numbers
  t('abs(10.5)', '10.5', '');
  t('abs(-10.5)', '10.5', '');
  t('fn:abs(-10.5)', '10.5', '');
  t('abs(10.5) = 10.5', 'true', '');
  t('ceiling(10)', '10', '');
  t('ceiling(-10)', '-10', '');
  t('ceiling(10.5)', '11', '');
  t('ceiling(-10.5)', '-10', '');
  t('ceiling(10.6)', '11', '');
  t('ceiling(-10.6)', '-10', '');
  t('ceiling(10.4)', '11', '');
  t('ceiling(-10.4)', '-10', '');
  t('ceiling(10.4E0)', '11', '');
  t('ceiling(-10.4E0)', '-10', '');
  t('ceiling(xs:double("NaN"))', 'NaN', '');
  t('ceiling(xs:double("INF"))', 'INF', '');
  t('ceiling(xs:double("-INF"))', '-INF', '');
  t('floor(10)', '10', '');
  t('floor(-10)', '-10', '');
  t('floor(0)', '0', '');
  t('floor(10.4)', '10', '');
  t('floor(-10.4)', '-11', '');
  t('floor(10.5)', '10', '');
  t('floor(-10.5)', '-11', '');
  t('floor(10.6)', '10', '');
  t('floor(-10.6)', '-11', '');
  t('floor(10.5E0)', '10', '');
  t('floor(-10.5E0)', '-11', '');
  t('floor(xs:double("NaN"))', 'NaN', '');
  t('floor(xs:double("INF"))', 'INF', '');
  t('round(2.5)', '3', '');
  t('round(2.4999)', '2', '');
  t('round(-2.5)', '-2', '');
  t('round(2.5E0)', '3', '');
  t('round(2.4999E0)', '2', '');
  t('round(-2.5E0)', '-2', '');
  t('round(xs:double("NaN"))', 'NaN', '');
  t('round(xs:double("INF"))', 'INF', '');
  t('round-half-to-even(0.5)', '0', '');
  t('round-half-to-even(1.5)', '2', '');
  t('round-half-to-even(2.5)', '2', '');
  t('round-half-to-even(0.5E0)', '0', '');
  t('round-half-to-even(1.5E0)', '2', '');
  t('round-half-to-even(2.5E0)', '2', '');
  {$ifdef TEST_FLOAT}t('round-half-to-even(3.567812E+3, 2)', '3567.81', '');{$endif}
  t('round-half-to-even(4.7564E-3, 2)', '0', '');
  t('round-half-to-even(35612.25, -2)', '35600', '');
  t('round-half-to-even(xs:double("NaN"))', 'NaN', '');
  t('round-half-to-even(xs:double("INF"))', 'INF', '');
  t('number("42")', '42', '');
  t('number("")', 'NaN', '');
  t('number(false())', '0', '');
  t('number(true())', '1', '');
  t('number()', '7800', '<a>78<b>0</b>0</a>');
  t('number(())', 'NaN', '');
  t('number(xs:anyURI("abc"))', 'NaN', '');
  t('number(xs:duration("PT1S"))', 'NaN', '');
  t('number(xs:date("1900-02-02"))', 'NaN', '');
  t('number("")', 'NaN', '');
  t('number(xs:untypedAtomic(""))', 'NaN', '');
                 //Types
  t('exists(false())', 'true', '');
  t('exists("")', 'true', '');
  t('exists(@xxxxunknown)', 'false', '');
  t('exists(())', 'false', '');
  t('exists(0)', 'true', '');
  t('type-of(())', 'untyped', '');
  t('type-of(0)', 'integer', '');
  t('type-of(0.0)', 'decimal', '');
  t('type-of("a")', 'string', '');
  t('type-of(parse-dateTime("2010-10-10","yyyy-mm-dd"))', 'dateTime', '');
  t('type-of(parse-date("2010-10-10","yyyy-mm-dd"))', 'date', '');
  t('type-of(parse-time("2010-10-10","yyyy-mm-dd"))', 'time', '');
  t('type-of(eval("0"))', 'integer', '');
  t('(eval("0 + 9.0")) instance of xs:decimal', 'true', '');
  //t('type-of(eval("0 + 9.0"))', 'decimal', ''); todo?

  //Stringfunctions
  t('string()', 'mausxyzx', '<a>maus<b>xyz</b>x</a>');
  t('string()', 'mausxyzx', '');
  t('string(())', '', '');
  t('string(123)', '123', '');
  t('string("123a")', '123a', '');
  t('string-length("Harp not on that string, madam; that is past.")', '45', '');
  t('string-length(())', '0', '');
  t('string-length()', '8', ''); //mausxyzx
  t('concat("A", "-2")', 'A-2', '');
  t('concat("A", 2)', 'A2', '');
  t('concat("A", 2+3)', 'A5', '');
  t('concat("A", 2+3, 7)', 'A57', '');
  t('concat("A", 2+3, -7)', 'A5-7', '');
  t('string-join((''Now'', ''is'', ''the'', ''time'', ''...''), '' '')', 'Now is the time ...', '');
  t('string-join((''Blow, '', ''blow, '', ''thou '', ''winter '', ''wind!''), '''')', 'Blow, blow, thou winter wind!', '');
  t('string-join((), ''separator'')', '', '');
  t('string-join(("a","b","c"), '':'')', 'a:b:c', '');
  t('string-join(("a","b","c",("d","e"), (("f"))), '':'')', 'a:b:c:d:e:f', '');
  t('codepoints-to-string(65)', 'A', '');
  t('codepoints-to-string((65,66,67,68))', 'ABCD', '');
  t('string-to-codepoints("ABCD")', '65666768', '');
  t('join(string-to-codepoints("ABCD"),",")', '65,66,67,68', '');


  DefaultSystemCodePage := CP_UTF8;
  t('codepoints-to-string((2309, 2358, 2378, 2325))', 'अशॊक', ''); //if these tests fail, but those above work, fpc probably compiled the file with the wrong encoding (must be utf8);
  t('string-to-codepoints("Thérèse")', '84104233114232115101', '');
  t('join(string-to-codepoints("Thérèse"),",")', '84,104,233,114,232,115,101', '');
  t('substring("motor car", 6)', ' car', '');
  t('substring("metadata", 4, 3)', 'ada', '');
  t('substring("12345", 1.5, 2.6)', '234', '');
  t('substring("12345", 0, 3)', '12', '');
  t('substring("12345", 0, xs:decimal(3))', '12', '');
  t('substring("12345", xs:decimal(0), xs:decimal(3))', '12', '');
  t('substring("12345", 5, -3)', '', '');
  t('substring("12345", -3, 5)', '1', '');
  t('substring("12345", 0 div 0e0, 3)', '', '');
  t('substring("12345", 1, 0 div 0e0)', '', '');
  t('substring((), 1, 3)', '', '');
  t('substring("12345", -42, 1 div 0e0)', '12345', '');
  t('substring("12345", -1 div 0e0, 1 div 0e0)', '', '');
  t('lower-case("ABc!D")', 'abc!d', '');
  t('upper-case("abCd0")', 'ABCD0', '');
  t('contains( "tattoo", "t")', 'true', '');
  t('contains( "tattoo", "ttt")', 'false', '');
  t('contains( "tattoo", "tT")', 'true', '');
  t('contains( "",  ())', 'true', '');
  t('starts-with("tattoo", "tattoo")', 'true', '');
  t('starts-with("tattoo", "tattoox")', 'false', '');
  t('starts-with("tattoo", "tat")', 'true', '');
  t('starts-with("tattoo", "att")', 'false', '');
  t('starts-with("tattoo", "TAT")', 'true', '');
  t('starts-with((), ())', 'true', '');
  t('ends-with("tattoo", "tattoo")', 'true', '');
  t('ends-with("tattoo", "tattoox")', 'false', '');
  t('ends-with("tattoo", "too")', 'true', '');
  t('ends-with("tattoo", "atto")', 'false', '');
  t('ends-with("tattoo", "TTOO")', 'true', '');
  t('ends-with((), ())', 'true', '');
  t('substring-before("tattoo", "attoo")', 't', '');
  t('substring-before("tattoo", "o")', 'tatt', '');
  t('substring-before("tattoo", "OO")', 'tatt', '');
  t('substring-before("tattoo", "tatto")', '', '');
  t('substring-before((), ())', '', '');
  t('substring-after("tattoo", "tat")', 'too', '');
  t('substring-after("tattoo", "tatto")', 'o', '');
  t('substring-after("tattoo", "tattoo")', '', '');
  t('substring-after("tattoo", "T")', 'attoo', '');
  t('substring-after((), ())', '', '');
  t('matches("abracadabra", "bra")', 'true','');
  t('matches("abracadabra", ''^a.*a$'')', 'true', '');
  t('matches("abracadabra", "^bra")', 'false', '');
  t('poem/text()', 'Kaum hat dies der Hahn gesehen,'#10'Fängt er auch schon an zu krähen:'#10'«Kikeriki! Kikikerikih!!»'#10'Tak, tak, tak! - da kommen sie.', '<poem author="Wilhelm Busch">'#13#10'Kaum hat dies der Hahn gesehen,'#13#10'Fängt er auch schon an zu krähen:'#13#10'«Kikeriki! Kikikerikih!!»'#13#10'Tak, tak, tak! - da kommen sie.'#13#10'</poem>');
  t('./text()', '', ''); //above /\, white space trimmed
  t('text()', '', '');
  t('.//text()', 'Kaum hat dies der Hahn gesehen,'#10'Fängt er auch schon an zu krähen:'#10'«Kikeriki! Kikikerikih!!»'#10'Tak, tak, tak! - da kommen sie.', '');
  t('matches(poem/text(), "Kaum.*krähen", "")', 'false', '');
  t('matches(poem/text(), "Kaum.*krähen")', 'false', '');
  t('matches(poem/text(), "Kaum.*krähen", "s")', 'true', '');
  t('matches(poem/text(), ''^Kaum.*gesehen,$'', "m")', 'true', '');
  t('matches(poem/text(), ''^Kaum.*gesehen,$'', "")', 'false', '');
  t('matches(poem/text(), ''^Kaum.*gesehen,$'')', 'false', '');
  t('matches(poem/text(), "kiki", "")', 'false', '');
  t('matches(poem/text(), "kiki", "i")', 'true', '');
  t('normalize-space("  hallo   ")', 'hallo', '');
  t('normalize-space("  ha'#9#13#10'llo   ")', 'ha llo', '');
  t('normalize-space("  ha'#9#13#10'l'#9' '#9'lo   ")', 'ha l lo', '');
                //Boolean
  t('boolean(0)', 'false', '');
  t('boolean("0")', 'true', '');
  t('boolean("1")', 'true', '');
  t('boolean("")', 'false', '');
  t('boolean(1)', 'true', '');
  t('boolean("false")', 'true', '');
  t('boolean("true")', 'true', '');
  t('boolean(false())', 'false', '');
  t('boolean(true())', 'true', '');
  t('false()', 'false', '');
  t('true()', 'true', '');
  t('not(false())', 'true', '');
  t('not(true())', 'false', '');
  t('not("")', 'true', '');
  t('not("false")', 'false', '');
  t('not("true")', 'false', '');
  t('not("falses")', 'false', '');
                //Dates
  t('parse-date("2010-10-9", "yyyy-mm-d")', '2010-10-09', '');
  t('parse-date("2010-10-08", "yyyy-mm-d")', '2010-10-08', '');
  t('parse-date("1899-Dec-31", "yyyy-mmm-d")', '1899-12-31', '');
  t('parse-date("1899-Dec-29", "yyyy-mmm-d")', '1899-12-29', '');
  t('parse-date("1234/08/06")', '1234-08-06');
  t('parse-date("10.03.1900")', '1900-03-10');
  t('parse-date("1/2/2000")', '2000-02-01');
  t('parse-date("1234-8-6")', '1234-08-06');
  t('parse-date("2123 Feb 17")', '2123-02-17');
  t('parse-date("10 Mär 1989")', '1989-03-10');
  t('parse-date("30 März 1989")', '1989-03-30');
  t('parse-date("/2345/Mär/19/")', '2345-03-19');
  t('parse-date("/2345:::May--19:")', '2345-05-19');
  if strictTypeChecking then func := 'year-from-date'
  else func := 'year-from-dateTime';
  t(func+'(parse-date("1800-09-07", "yyyy-mm-dd"))', '1800', '');
  t('year-from-date(parse-date("1800-09-07", "yyyy-mm-dd"))', '1800', '');
  t(func+'(parse-date(">>2012<<01:01", ">>yyyy<<mm:dd"))', '2012', '');
  t(func+'(parse-date(">>1700<<01:01", ">>yyyy<<mm:dd"))', '1700', '');
  t(func+'(parse-date(">>05<<01:01", ">>yy<<mm:dd"))', '2005', '');
  t(func+'(parse-date(">>90<<01:01", ">>yy<<mm:dd"))', '1990', '');
  t(func+'(parse-date(">>89<<01:01", ">>yy<<mm:dd"))', '2089', '');
  t('month-from-date(parse-date("1899-Dec-31", "yyyy-mmm-d")) ', '12', '');
  t('month-from-date(parse-date("1899-Jul-31", "yyyy-mmm-d")) ', '7', '');
  t('day-from-date(parse-date("1899-Jul-31", "yyyy-mmm-d")) ', '31', '');

  t('parse-date("1899-Dec-31", "yyyy-mmm-d") - parse-date("1899-Dec-29", "yyyy-mmm-d")', 'P2D', '');
                //Sequences
  t('index-of ((10, 20, 30, 40), 35)', '', '');
  t('index-of ((10, 20, 30, 30, 10), 20)', '2', '');
  t('index-of ((10, 20, 30, 30, 20, 10), 20)', '25', '');
  t('join(index-of ((10, 20, 30, 30, 10), 20), ",")', '2', '');
  t('join(index-of ((10, 20, 30, 30, 20, 10), 20), ",")', '2,5', '');
  t('join(index-of (("a", "sport", "and", "a", "pastime"), "a"), ",")', '1,4', '');
  t('("MEMLEAKTEST1", "MEMLEAKTEST2")', 'MEMLEAKTEST1MEMLEAKTEST2', '');
  t('string-join(("MEMLEAKTEST3", "MEMLEAKTEST4"), "-")', 'MEMLEAKTEST3-MEMLEAKTEST4', '');
  t('empty(())', 'true', '');
  t('empty((4))', 'false', '');
  t('empty((false()))', 'false', '');
  t('empty((true(),1,2,3))', 'false', '');
  t('distinct-values((1, 2.0, 3, 2))', '123', '');
  t('join(distinct-values((1, 2.0, 3, 2)),",")', '1,2,3', '');
  t('string-join(insert-before(("a", "b", "c"), 0, "z"), ",")', 'z,a,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 1, "z"), ",")', 'z,a,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 2, "z"), ",")', 'a,z,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 3, "z"), ",")', 'a,b,z,c', '');
  t('string-join(insert-before(("a", "b", "c"), 4, "z"), ",")', 'a,b,c,z', '');
  t('string-join(insert-before(("a", "b", "c"), 5, "z"), ",")', 'a,b,c,z', '');
  t('string-join(insert-before(("a", "b", "c"), 0, "z"), ",")', 'z,a,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 1, ("x","y","z")), ",")', 'x,y,z,a,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 2, ("x","y","z")), ",")', 'a,x,y,z,b,c', '');
  t('string-join(insert-before(("a", "b", "c"), 3, ("x","y","z")), ",")', 'a,b,x,y,z,c', '');
  t('string-join(insert-before(("a", "b", "c"), 4, ("x","y","z")), ",")', 'a,b,c,x,y,z', '');
  t('string-join(insert-before(("a", "b", "c"), 5, ("x","y","z")), ",")', 'a,b,c,x,y,z', '');
  t('string-join(remove(("a", "b", "c"), 0), ",")', 'a,b,c', '');
  t('string-join(remove(("a", "b", "c"), 1), ",")', 'b,c', '');
  t('string-join(remove(("a", "b", "c"), 2), ",")', 'a,c', '');
  t('string-join(remove(("a", "b", "c"), 3), ",")', 'a,b', '');
  t('string-join(remove(("a", "b", "c"), 6), ",")', 'a,b,c', '');
  t('string-join(remove((), 3), ",")', '', '');
  t('string-join(remove("a", 3), ",")', 'a', '');
  t('string-join(remove("a", 1), ",")', '', '');
  t('string-join(remove("a", 0), ",")', 'a', '');
  t('string-join(reverse(("c","b","a")), ",")', 'a,b,c', '');
  t('string-join(reverse(("hello")), ",")', 'hello', '');
  t('string-join(reverse(()), ",")', '', '');
  t('join(subsequence((1,2,3,4,5), 4), ",")', '4,5', '');
  t('subsequence((), 1)', '', '');
  t('subsequence((), 1, 2)', '', '');
  t('subsequence((6), 1, 2)', '6', '');
  t('subsequence(6, 1, 2)', '6', '');
  t('subsequence(6, 1, 1)', '6', '');
  t('subsequence(6, 0, 1)', '', '');
  t('subsequence((6,7), 2, 1)', '7', '');
  t('subsequence((6), 2, 1)', '', '');
  t('subsequence(6, 2, 1)', '', '');
  t('subsequence(6, xs:double("2"))', '');
  t('subsequence(6, xs:double("0"))', '6');
  t('subsequence(6, xs:double("-INF"))', '6');
  t('subsequence(6, xs:double("-2"), 3)', '');
  t('subsequence(6, xs:double("-2"), 4)', '6');
  t('subsequence(6, xs:double("-2"), 30)', '6');
  t('subsequence(1 to 3,-123456789123456789123, 1)', '');
{  t('subsequence(1 to 3,-123456789123456789123, 123456789123456789123)', '');
  t('subsequence(1 to 3,-123456789123456789123, 123456789123456789124)', '');
  t('subsequence(1 to 3,-123456789123456789123, 123456789123456789125)', '1');//this is supposed to fail, since the numbers should be converted to double  }
  t('join(subsequence(1 to 3,0))', '1 2 3');
  t('join(subsequence((1,2,3,4,5), 3, 2), ",")', '3,4', '');
  t('concat(join(subsequence((1,2,3,4,5), 3, 2147483646), ","), ":", join(subsequence((1,2,3,4,5), 3, 2147483648), ","))', '3,4,5:3,4,5', '');
  t('join(unordered((1,2,3,4,5)), ",")', '1,2,3,4,5', '');
  t('deep-equal(1, 2)', 'false', '');
  t('deep-equal(1, 1)', 'true', '');
  t('deep-equal((1), ())', 'false', '');
  t('deep-equal((1), (1))', 'true', '');
  t('deep-equal((1), (1,1))', 'false', '');
  t('deep-equal((1), ("1"))', 'false', '');
  t('deep-equal((1), (1.0))', 'true', '');
  t('deep-equal((xs:unsignedInt(1)), (xs:float(1)))', 'true', '');
  t('deep-equal((1), (xs:float(1)))', 'true', '');
  t('deep-equal((1,1), (1))', 'false', '');
  t('deep-equal((1,1), (1,1.0))', 'true', '');
  t('deep-equal((1,2,3,4), (1,2,3,4))', 'true', '');
  t('deep-equal((1,2,3,4), (1,2,3))', 'false', '');
  t('deep-equal(("A","B"), ("a","b"))', 'true', '');
  t('deep-equal(("A","B"), ("a","bc"))', 'false', '');
  t('deep-equal(("A","B", ("c")), ("a","b", "c"))', 'true', '');
  t('count(())', '0', '');
  t('count((10))', '1', '');
  t('count((10,2,"abc","def",17.0))', '5', '');
  t('avg((1,2,3))', '2', '');
  t('avg((1,2,3,4))', '2.5', '');
  t('avg((3,4,5))', '4', '');
  t('avg(())', '', '');
  t('avg((xs:float(''INF''), xs:double(''-INF'')))', 'NaN', '');
  t('avg((3,4,5, xs:float(''NaN'')))', 'NaN', '');
  t('max((1,2,3))', '3', '');
  t('max((3,4,5))', '5', '');
  t('max((1,xs:double("NaN"),3))', 'NaN', '');
  t('type-of(max((3,4,5)))', 'integer', '');
  t('max((5, 5.0e0))', '5', '');

  t('max(("3","4","Zero"))', 'Zero', '');
  t('max((current-date(), parse-date("2001-01-01","yyyy-mm-dd"))) = current-date()', 'true', '');
  t('max((current-date(), parse-date("9001-01-01","yyyy-mm-dd"))) = current-date()', 'false', '');
  t('max(("a", "b", "c"))', 'c', '');
  t('max((1,2,3,4))', '4', '');
  t('max((1,25.0,-3,4))', '25', '');
  t('max((1,xs:untypedAtomic("5"),3,4))', '5', '');
  t('max((1,xs:untypedAtomic("2"),3,4))', '4', '');
  t('max(("10haus", "100haus", "2haus", "099haus"))', '100haus', '');
  t('min((1,2,3))', '1', '');
  t('min((1,xs:double("NaN"),3))', 'NaN', '');
  t('min((3,4,5))', '3', '');
  t('type-of(min((3,4,5)))', 'integer', '');
  t('min((5,5.0))', '5', '');
  t('type-of(min((5,5.0)))', 'integer', '');
  t('min(("3","4","Zero"))', '3', '');
  t('min((current-date(), parse-date("3001-01-01","yyyy-mm-dd"))) = current-date()', 'true', '');
  t('min((current-date(), parse-date("1901-01-01","yyyy-mm-dd"))) = current-date()', 'false', '');
  t('min((-0.0,0.0))', '0', '');
  t('min((-0.0,0.0)) instance of xs:decimal', 'true', '');
  t('min((1,2,3,4))', '1', '');
  t('min((1,25.0,-3,4))', '-3', '');
  t('min((1,xs:untypedAtomic("5"),3,4))', '1', '');
  t('min((1,xs:untypedAtomic("2"),3,4))', '1', '');
  t('min(("10haus", "100haus", "2haus", "099haus"))', '2haus', '');
  t('min(("a", "b", "c"))', 'a', '');
  t('min((1,xs:double("NaN"),3))', 'NaN', '');
  t('max((1,xs:double("NaN"),3))', 'NaN', '');
  t('xs:double(xs:untypedAtomic("NaN"))', 'NaN');
  t('min((1,xs:untypedAtomic("NaN"),3))', 'NaN', '');
  t('max((1,xs:untypedAtomic("NaN"),3))', 'NaN', '');
  t('xs:untypedAtomic("foobar2") castable as xs:double', 'false');
  f('min((3,xs:untypedAtomic("foobar"),5))', 'err:FORG0001');
  f('max((3,xs:untypedAtomic("foobar"),5))', 'err:FORG0001');
  t('sum((3,4,5))', '12', '');
  t('sum(())', '0', '');
  t('sum((),())', '', '');
  t('sum((3,xs:double("NaN"),5))', 'NaN', '');
  t('sum((3,xs:untypedAtomic("NaN"),5))', 'NaN', '');
  f('sum((3,xs:untypedAtomic("foobar"),5))', 'err:FORG0001');
  t('sum((xs:unsignedByte(200), xs:unsignedByte(200)))', '400', '');
  t('(x:product(()), x:product(1 to 3))', '6');

  t('(1,2,3)[true()]', '123', '');
  t('(1,2,3)[false()]', '', '');
  t('join((1,2,3)[true()], ",")', '1,2,3', '');
  t('join((1,2,3)[false()], ",")', '', '');
  t('(4,5,6)[1]', '4', '');
  t('(4,5,6)[2]', '5', '');
  t('(4,5,6)[3]', '6', '');
  t('1[18446744073709551616*16+1]', '', '');
  t('("a","bc","de")[2]', 'bc', '');
  t('join((4,5,6)[1], ",")', '4', '');
  t('join((4,5,6)[2], ",")', '5', '');
  t('join((4,5,6)[3], ",")', '6', '');
  t('join((4,5,6)[true()][1], ",")', '4', '');
  t('join((4,5,6)[true()][true()][true()][true()][true()][1], ",")', '4', '');
  t('(4,5,6)[string() = ''5'']', '5', '');
  t('join((4,5,6)[string()="5"], ",")', '5', '');
  t('join((1 to 100)[number() eq 15 or string() = "23"], ",")', '15,23', '');
  t('join((1 to 100)[number() mod 5 eq 0], ",")', '5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100', '');
  t('join((21 to 29)[5],",")', '25', '');
  t('join((21 to 29)[5],",")', '25', '');
  t('join((21 to 29)[number() gt 24][2],",")', '26', '');
  t('string-join(("hallo","mast","welt","test","tast","ast")[contains(string(),"as")], ",")', 'mast,tast,ast', '');
  t('join( (4,5) [.=4] , ",")', '4', '');
  t('join( ( (4,5) [.=4] ) , ",")', '4', '');
  t('join( ( ((4,5)) [(.=4)] ) , ",")', '4', '');
  t('join( (((( ((4,5)) [(.=4)] )))) , ",")', '4', '');
  t('join((1 to 100)[. mod 5 eq 0],",")', '5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100', '');
  t('(join(((1 to 100)[. mod 5 eq 0]),","))', '5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100', '');
  t('join(((1 to 5)[3] to (3 to 7)[3]),",")', '3,4,5', '');
  t('join((4,5,6)[.=(5,6)], ",")', '5,6', '');
  t('count((98.5, 98.3, 98.9))', '3', '');
  t('count((98.5, 98.3, 98.9)[.>100])', '0', '');
  t('count((98.5, 98.3, 98.9)[.>98.5])', '1', '');
  t('sum((1 to 100)[.<0], 0)', '0', '');
  t('sum((1 to 100)[.<0], "aber")', 'aber', '');
  t('sum((1 to 100)[.<10], "abc")', '45', '');
  t('join((101 to 120)[position() = 4], ",")', '104', '');
  t('join((101 to 120)[position() = last()], ",")', '120', '');
  t('join((101 to 120)[position() = last() - 2], ",")', '118', '');
  t('join((101 to 120)[position() >= 4 and position() < 10], ",")', '104,105,106,107,108,109', '');
  t('join((101 to 120)[position() >= 4 and position() < 10][position()=last()], ",")', '109', '');
  t('join((101 to 120)[position() >= 4 and position() < 10][position()=3], ",")', '106', '');
  t('join((101 to 120)[position() >= 4 and position() < 10][4], ",")', '107', '');

                //Axis tests
  t('','','<a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2</b>al</a>');

                //Iterator
  t('a/b/text()', 'b1b2', '');
  t('a/b/c/text()', 'c1c2c3c4', '');
  t('a/b/c[text()="c2"]/text()', 'c2', '');
  t('a/b/c[.="c3"]/text()', 'c3', '');
  t('a/b/c[1]/text()', 'c1', '');
  t('a/b/c[2]/text()', 'c2', '');
  t('a/b/c[position() = 2]/text()', 'c2', '');
  t('a/b/c[.="c2" or .="c3"]/text()', 'c2c3', '');

                //Full sequence
  t('a/b/c[.="c2" or .="c3"][2]/text()', 'c3', '');
  t('a/b/c[last()]/text()', 'c4', '');
  t('a[true()]/b[true()][true()]/c[.="c2" or .="c3"][2]/text()', 'c3', '');
  t('string-join(a/b/c[.="c2" or .="c3"]/text(),",")', 'c2,c3', '');
  t('string-join(a[true()]/b[true()][true()]/c[.="c2" or .="c3"]/text(), ",")', 'c2,c3', '');
  t('string-join(a[true()]  [ 1 ]   /   b[true()] [true(      )]/c[.=("c2","c3")]/text(), ",")', 'c2,c3', '');
  t('string-join(a/b/c/text(), ",")', 'c1,c2,c3,c4', '');
  t('string-join(a/b/text(), ",")', 'b1,b2', '');


  t('','','<a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b>'+
             '<b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>'+ 'al' +
             '<d>d1</d>'+'<d>d2</d>'+'<d>d3<e>dxe1</e></d>'+'<f>f1</f>'+'<f>f2</f>'+
           '</a>');

  t('string-join(a/b/c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('string-join(a/b/c[2]/text(), ",")', 'c2,cx2', '');
  t('string-join(a/b/c[position()=(2,3)]/text(), ",")', 'c2,c3,cx2', '');
  t('string-join(a/b/c/c/text(), ",")', 'CC1', '');
  t('string-join(a/b//c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a//b/c[2]/text(), ",")', 'c2,cx2', '');
  t('string-join(a/b//c[2]/text(), ",")', 'c2,cx2', '');
  t('string-join(a/b//c[position()=(2,3)]/text(), ",")', 'c2,c3,cx2', '');
  t('string-join(a/b//c[7]/text(), ",")', '', '');
  t('string-join(a//c[1]/text(), ",")', 'c1,cx1,CC1', '');
  t('string-join(a/b//c[1]/text(), ",")', 'c1,cx1,CC1', '');
  t('string-join(a/b//c[2]/text(), ",")', 'c2,cx2', '');
  t('string-join(a/b//c[position()=last()]/text(), ",")', 'c4,cx2,CC1', '');
  t('string-join(a/*/text(), ",")', 'b1,b2,d1,d2,d3,f1,f2', '');
  t('string-join(a/node()/text(), ",")', 'b1,b2,d1,d2,d3,f1,f2', '');
  t('string-join(a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join(a/b/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('string-join(a/*/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('string-join(a/node()/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('string-join(a//c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('string-join(a/d, ",")', 'd1,d2,d3dxe1', '');
  t('string-join(a/d/text(), ",")', 'd1,d2,d3', '');
  t('string-join(a/f, ",")', 'f1,f2', '');
  t('string-join(a/f/node(), ",")', 'f1,f2', '');
  t('string-join(a/(d,f), ",")', 'd1,d2,d3dxe1,f1,f2', '');
  t('string-join(a/(d,f)/text(), ",")', 'd1,d2,d3,f1,f2', '');
  t('string-join((a/b) / (c/c), ",")', 'CC1', '');
  t('string-join(a/b[2]/c[1]/c[1], ",")', '', '');
  t('string-join(a/b[2]/c[2]/c[1], ",")', 'CC1', '');
                //concatenate,union,intersect,except
  t('string-join((a/b, a/f), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join((a/f, a/b), ",")', 'f1,f2,b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join((a/f, a/b, a/f), ",")', 'f1,f2,b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/b | a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/f | a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/b union a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/f union a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/b | a/f | a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/f | a/b | a/f |a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(a/b[1] | a/f, ",")', 'b1c1c2c3c4,f1,f2', '');
  t('string-join(a/b[1] | a/f[2], ",")', 'b1c1c2c3c4,f2', '');
  t('string-join(a/b | a/f[2], ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f2', '');
  t('string-join((a/b, a/d) / (e, c/c), ",")', 'CC1,dxe1', '');
  t('string-join((a/b|a/d) / (e, c/c), ",")', 'CC1,dxe1', '');
  t('string-join((a/b, a/d) / (e|c/c), ",")', 'CC1,dxe1', '');
  t('string-join((a/b|a/d) / (e|c/c), ",")', 'CC1,dxe1', '');
  t('string-join(a//c except a/b/c/c , ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('string-join(a//c except a/b/c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('string-join(a//c/text() except a/b/c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('string-join(a//c/text() except a/b/c/c , ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a//c except a//c/c , ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('string-join(a//c except a//c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('string-join(a//c/text() except a//c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('string-join(a//c/text() except a//c/c , ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a//c intersect a/b/c/c , ",")', 'CC1', '');
  t('string-join(a//c intersect a/b/c/c/text() , ",")', '', '');
  t('string-join(a//c/text() intersect a/b/c/c/text() , ",")', 'CC1', '');
  t('string-join(a//c/text() intersect a/b/c/c , ",")', '', '');
  t('string-join(a//c intersect a//c/c , ",")', 'CC1', '');
  t('string-join(a//c intersect a//c/c/text() , ",")', '', '');
  t('string-join(a//c/text() intersect a//c/c/text() , ",")', 'CC1', '');
  t('string-join(a//c/text() intersect a//c/c , ",")', '', '');
  t('string-join((a/f | a/b) intersect a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join((a/f | a/b) intersect a/f, ",")', 'f1,f2', '');
  t('string-join((a/f | a/b) intersect a/b[2], ",")', 'b2cx1cx2CC1', '');
  t('string-join((a/f | a/b) intersect a/f[2], ",")', 'f2', '');
  t('string-join((a/f | a/b) intersect (a/b | a/f), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join((a/f | a/b) intersect (a/f | a/d), ",")', 'f1,f2', '');
  t('string-join((a/f | a/b) intersect (a/d), ",")', '', '');
  t('string-join((a/f | a/b) intersect (), ",")', '', '');
  t('string-join(() intersect (a/f | a/d), ",")', '', '');
  t('string-join((a/f | a/b) except a/b, ",")', 'f1,f2', '');
  t('string-join((a/f | a/b) except a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join((a/f | a/b) except a/b[2], ",")', 'b1c1c2c3c4,f1,f2', '');
  t('string-join((a/f | a/b) except a/f[2], ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1', '');
  t('string-join((a/f | a/b) except (a/b | a/f), ",")', '', '');
  t('string-join((a/f | a/b) except (a/f | a/d), ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join((a/f | a/b) except (a/d), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join((a/f | a/b) except (), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('string-join(() except (a/f | a/d), ",")', '', '');
                //is,<<,>>
  t('a/b[1] is a/b[2]', 'false', '');
  t('a/b[2] is a/b[1]', 'false', '');
  t('a/b[1] is a/b[1]', 'true', '');
  t('a/b[1]/c[1] is a/b/c/c', 'false', '');
  t('a/b[2]/c[1] is a/b/c/c', 'false', '');
  t('a/b[2]/c[2]/c[1] is a/b/c/c', 'true', '');
  t('a/b[1] << a/b[2]', 'true', '');
  t('a/b[2] << a/b[1]', 'false', '');
  t('a/b[1] << a/b[1]', 'false', '');
  t('a/b[1]/c[1] << a/b/c/c', 'true', '');
  t('a/b[2]/c[1] << a/b/c/c', 'true', '');
  t('a/b[2]/c[2]/c[1] << a/b/c/c', 'false', '');
  t('a/b[1] >> a/b[2]', 'false', '');
  t('a/b[2] >> a/b[1]', 'true', '');
  t('a/b[1] >> a/b[1]', 'false', '');
  t('a/d[1] >> a/b[1]', 'true', '');
  t('a/b[1]/c[1] >> a/b/c/c', 'false', '');
  t('a/b[2]/c[1] >> a/b/c/c', 'false', '');
  t('a/b[2]/c[2]/c[1] >> a/b/c/c', 'false', '');
                //axes
  t('a/child::b', 'b1c1c2c3c4b2cx1cx2CC1', '');
  t('join(child::a/child::b)', 'b1c1c2c3c4 b2cx1cx2CC1', '');
  t('child::a/child::b/child::text()', 'b1b2', '');
  t('string-join(a/child::b,",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join(child::a/child::b,",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join(child::a/child::b/child::text(),",")', 'b1,b2', '');
  t('string-join((child::a/child::b)[2],",")', 'b2cx1cx2CC1', '');
  t('string-join((child::a/child::b/child::text())[1],",")', 'b1', '');
  t('node-name((child::a/child::b)[1])', 'b', '');
  t('node-name((child::a/child::b/child::text())[1])', '', '');
  t('node-name((child::a/child::b/child::text()/..)[1])', 'b', '');
  t('node-name((child::a/child::b/child::text()/../..)[1])', 'a', '');
  t('string-join(a/self::a/self::a/self::a/child::b,",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('string-join(a/self::a//text(),",")', 'b1,c1,c2,c3,c4,b2,cx1,cx2,CC1,al,d1,d2,d3,dxe1,f1,f2', '');
  t('string-join(a/self::b//text(),",")', '', '');
  t('string-join(a/child::b/parent::a/child::b/text(),",")', 'b1,b2', '');
  t('string-join(a/child::b/parent::x/child::b/text(),",")', '', '');
  t('string-join(a/child::b[1]/following::c,",")', 'cx1,cx2CC1,CC1', '');
  t('string-join(a/child::b/following::c,",")', 'cx1,cx2CC1,CC1', '');
  t('string-join(a/descendant::c/text(),",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a/descendant-or-self::c/text(),",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a/child::c/descendant::c/text(),",")', '', '');
  t('string-join(a/child::c/descendant-or-self::c/text(),",")', '', '');
  t('string-join(a/b/child::c/descendant::c/text(),",")', 'CC1', ''); //failing here might come from an an end node not being found
  t('string-join(a/b/child::c/descendant-or-self::c/text(),",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('string-join(a/b/following::d/text(),",")', 'd1,d2,d3', '');
  t('string-join(a/b/following-sibling::d/text(),",")', 'd1,d2,d3', '');
  t('string-join(a/b/c/following::d/text(),",")', 'd1,d2,d3', '');
  t('string-join(a/b/c/following-sibling::d/text(),",")', '', '');
  t('string-join(a/b/c/c/ancestor::c/text(),",")', 'cx2', '');
  t('string-join(a/b/c/c/ancestor::b/text(),",")', 'b2', '');
  t('string-join(a/b/c/c/ancestor::a/text(),",")', 'al', '');
  t('string-join(a/b/c/c/ancestor::*/text(),",")', 'b2,cx2,al', '');
  t('string-join(a/b/c/c/ancestor-or-self::*/text(),",")', 'b2,cx2,CC1,al', '');
  t('string-join(a/f/ancestor-or-self::*/text(),",")', 'al,f1,f2', '');
  t('string-join(a/f/ancestor::*/text(),",")', 'al', '');
  t('string-join(a/f/preceding-sibling::*/text(),",")', 'b1,b2,d1,d2,d3,f1', '');
  t('string-join(a/f/preceding::*/text(),",")', 'b1,c1,c2,c3,c4,b2,cx1,cx2,CC1,d1,d2,d3,dxe1,f1', '');
  t('string-join(a/b/c/c/preceding::*/text(),",")', 'b1,c1,c2,c3,c4,cx1', '');



                //todo ,('string-join(a/(d,f)/../text(), ",")', '', '')
               //examples taken from the xpath standard
  t('','','<para>p1</para>' + '<para type="warning">p2</para>' + '<rd>texti</rd>'+'<para type="warning">p3</para>'+'<x>XX</x>'+'<para type="warning">p4</para>'+'<npara>np<para>np1</para><para>np2</para></npara>'+
   '<chapter><ti></ti><div><para>cdp1</para><para>cdp2</para></div></chapter>'+'<b>BB</b>'+'<rd>ltext</rd>'+'<chapter><title>Introduction</title><div><para>CDP1</para><para>CDP2</para></div></chapter>');
  t('string-join(child::para,",")', 'p1,p2,p3,p4', '');
  t('string-join(child::*,",")', 'p1,p2,texti,p3,XX,p4,npnp1np2,cdp1cdp2,BB,ltext,IntroductionCDP1CDP2', '');
  t('string-join(child::text(),",")', '', '');
  t('string-join(rd/child::text(),",")', 'texti,ltext', '');
  t('string-join(child::node(),",")', 'p1,p2,texti,p3,XX,p4,npnp1np2,cdp1cdp2,BB,ltext,IntroductionCDP1CDP2', '');
  t('string-join(descendant::para,",")', 'p1,p2,p3,p4,np1,np2,cdp1,cdp2,CDP1,CDP2', '');
  t('string-join(chapter/div/para/ancestor::div,",")', 'cdp1cdp2,CDP1CDP2', '');
  t('string-join(chapter/div/ancestor::div,",")', '', '');
  t('chapter/div/para/ancestor-or-self::div', 'cdp1cdp2CDP1CDP2', '');
  t('string-join(chapter/div/para/ancestor-or-self::div,",")', 'cdp1cdp2,CDP1CDP2', '');
  t('string-join(chapter/div/ancestor-or-self::div,",")', 'cdp1cdp2,CDP1CDP2', '');
  t('string-join(chapter/ancestor-or-self::div,",")', '', '');
  t('string-join(descendant-or-self::para,",")', 'p1,p2,p3,p4,np1,np2,cdp1,cdp2,CDP1,CDP2', '');
  t('string-join(para/descendant-or-self::para,",")', 'p1,p2,p3,p4', '');
  t('string-join(para/descendant::para,",")', '', '');
  t('string-join(para/self::para,",")', 'p1,p2,p3,p4', '');
  t('string-join(self::para,",")', '', '');
  t('string-join(child::chapter/descendant::para ,",")', 'cdp1,cdp2,CDP1,CDP2', '');
  t('string-join(child::*/child::para ,",")', 'np1,np2', '');
  t('string-join(npara/child::*,",")', 'np1,np2', '');
  t('string-join(npara/child::*/child::para ,",")', '', '');
  t('string-join(child::para[position()=1],",")', 'p1', '');
  t('string-join(child::para[position()=last()] ,",")', 'p4', '');
  t('string-join(child::para[position()=last()-1] ,",")', 'p3', '');
  t('string-join(child::para[position()>1] ,",")', 'p2,p3,p4', '');
  t('string-join(following-sibling::chapter[position()=1] ,",")', '', '');
  t('string-join(chapter/following-sibling::chapter[position()=1] ,",")', 'IntroductionCDP1CDP2', '');
  t('string-join(chapter/preceding-sibling::chapter[position()=1] ,",")', 'cdp1cdp2', '');
  t('string-join(descendant::para[position()=3] ,",")', 'p3', '');
  t('string-join(descendant::para[position()=8] ,",")', 'cdp2', '');
  t('string-join(child::chapter[position()=2]/*/child::para[position()=1],",")', 'CDP1', '');
  t('string-join(child::chapter[position()=1]/*/child::para[position()=2],",")', 'cdp2', '');
  t('string-join(child::para[attribute::type="warning"],",")', 'p2,p3,p4', '');
  t('string-join(child::para[attribute::type="warning"][2],",")', 'p3', '');
  t('string-join(child::para[attribute::type="warning"][1],",")', 'p2', '');
  t('string-join(child::para[2][attribute::type="warning"],",")', 'p2', '');
  t('string-join(child::para[1][attribute::type="warning"],",")', '', '');
  t('string-join(child::chapter[child::title=''Introduction''],",")', 'IntroductionCDP1CDP2', '');
  t('string-join(child::chapter[child::title],",")', 'IntroductionCDP1CDP2', '');
  t('string-join(child::chapter[child::div],",")', 'cdp1cdp2,IntroductionCDP1CDP2', '');
  t('string-join(child::chapter[child::ti],",")', 'cdp1cdp2', '');
  t('string-join(child::chapter[child::ti or child::title],",")', 'cdp1cdp2,IntroductionCDP1CDP2', '');
  t('string-join(child::chapter[child::title or child::ti],",")', 'cdp1cdp2,IntroductionCDP1CDP2', '');
  t('string-join(child::chapter[child::title or child::ti][1],",")', 'cdp1cdp2', '');
  t('string-join(child::chapter[child::title][1],",")', 'IntroductionCDP1CDP2', '');
               //abbreviated examples
  t('string-join(para,",")', 'p1,p2,p3,p4', '');
  t('string-join(*,",")', 'p1,p2,texti,p3,XX,p4,npnp1np2,cdp1cdp2,BB,ltext,IntroductionCDP1CDP2', '');
  t('string-join(text(),",")', '','');
  t('string-join(npara/*,",")', 'np1,np2','');
  t('string-join(npara/text(),",")', 'np','');
  t('string-join(x/text(),",")', 'XX', '');
  t('string-join(para[1],",")', 'p1', '');
  t('string-join(para[last()],",")', 'p4', '');
  t('string-join(*/para,",")', 'np1,np2', '');
  t('string-join(chapter[2]/div[1]/para,",")', 'CDP1,CDP2', '');
  t('string-join(chapter[2]/div/para[1],",")', 'CDP1', '');
  t('string-join(chapter//para,",")', 'cdp1,cdp2,CDP1,CDP2', '');
  t('string-join(.//para,",")', 'p1,p2,p3,p4,np1,np2,cdp1,cdp2,CDP1,CDP2', '');
  t('string-join(x/../para,",")', 'p1,p2,p3,p4', '');
  t('string-join(para[@type="warning"],",")', 'p2,p3,p4', '');
  t('string-join(para[@type="warning"][2],",")', 'p3', '');
  t('string-join(para[4][@type="warning"],",")', 'p4', '');
  t('string-join(para[1][@type="warning"],",")', '', '');
  t('string-join(chapter[title="Introduction"],",")', 'IntroductionCDP1CDP2', '');
  t('string-join(chapter[title],",")', 'IntroductionCDP1CDP2', '');
  t('string-join(chapter[ti and title],",")', '', '');
  t('string-join(chapter[ti and div],",")', 'cdp1cdp2', '');
  t('string-join(chapter[ti and div],",")', 'cdp1cdp2', '');
  t('string-join(chapter[ti or div],",")', 'cdp1cdp2,IntroductionCDP1CDP2', '');
  t('string-join(.//para[2],",")', 'p2,np2,cdp2,CDP2', '');
  t('string-join(./descendant::para[2],",")', 'p2', '');

               {
               //examples taken from the xpath standard
               ,('','','<para>p1</para>' + '<para type="warning">p2</para>' + 'texti'+'<para type="warning">p3</para>'+'<x>XX</x>'+'<para type="warning">p4</para>'+'<npara>np<para>np1</para><para>np2</para></npara>'+
               '<chapter><ti></ti><div><para>cdp1</para><para>cdp2</para></div></chapter>'+'<b>BB</b>'+'ltext'+'<chapter><title>Introduction</title><div><para>CDP1</para><para>CDP2</para></div></chapter>')
  }


               //examples taken from http://msdn.microsoft.com/en-us/library/ms256086.aspx
  t('','','<x><y>a</y><y>b</y></x><x><y>c</y><y>d</y></x>');
  t('string-join( x/y[1], ",")', 'a,c', '');
  t('string-join( x/y[position() = 1]  , ",")', 'a,c', '');
  t('string-join(  (x/y)[1] , ",")', 'a', '');
  t('string-join(  x[1]/y[2]  , ",")', 'b', '');

               //comments
  t('0 (: ... ::: :)', '0', '');
  t('4 +(: ... ::: :)7', '11', '');
  t('4 - (: Houston, we have a problem :) 7', '-3', '');
  t('4 - (: Houston, (:we have (::)a problem:):) 7', '-3', '');
  t('(: commenting out a (: comment :) may be confusing, but often helpful :)0', '0', '');
  t('"abc(::)"', 'abc(::)', '');
  t('"abc(::)"(::)   (:(:(::):):)  (:*:)', 'abc(::)', '');
  t('string-join( (:..:) x[1](:x:)/(::)y[2]  , ",")', 'b', '');

               //block structures
  t('for $x in (1,2,3) return $x', '123', '');
  t('join(for $x in (1,2,3) return $x,",")', '1,2,3', '');
  t('join(for $x in (1,2,3,"4","5") return $x,",")', '1,2,3,4,5', '');
  t('join(for $x in (1,2,3,'+untypedAtomic+'("4"),'+untypedAtomic+'("5")) return ($x + 1),",")', '2,3,4,5,6', '');
  t('join(for $x in (1,2,3,'+untypedAtomic+'("4"),'+untypedAtomic+'("5")) return $x + 1,",")', '2,3,4,5,6', '');
  t('for $x in (1,2,3,'+untypedAtomic+'("4"),'+untypedAtomic+'("5")) return $x + 1', '23456', '');
  t('for $x in (1,2,3) return (for $y in (10,20,30) return $x)', '111222333', '');
  t('for $x in (1,2,3) return (for $y in (10,20,30) return $y)', '102030102030102030', '');
  t('for $x in (1,2,3) return (for $y in (10,20,30) return $x + $y)', '112131122232132333', '');
  t('join(for $x in (1,2,3) return (for $y in (10,20,30) return $x),",")', '1,1,1,2,2,2,3,3,3', '');
  t('join(for $x in (1,2,3) return (for $y in (10,20,30) return $y),",")', '10,20,30,10,20,30,10,20,30', '');
  t('join(for $x in (1,2,3) return (for $y in (10,20,30) return $x + $y),",")', '11,21,31,12,22,32,13,23,33', '');
  t('join(for $x in (1,2,3) return (for $y in (10,20,30) return $x),",")', '1,1,1,2,2,2,3,3,3', '');
  t('join(for $x in (1,2,3) return (for $y in (10,20,30) return $y),",")', '10,20,30,10,20,30,10,20,30', '');
  t('join(for $x in (1,2,3) return for $y in (10,20,30) return $x + $y,",")', '11,21,31,12,22,32,13,23,33', '');
  t('join(for $x in (1,2,3), $y in (10,20,30) return $x + $y,",")', '11,21,31,12,22,32,13,23,33', '');
  t('join(for $x in (1,2,3), $y in (10,20,30) return $x + $y + 1 * 5,",")', '16,26,36,17,27,37,18,28,38', '');
  t('for $i in (10, 20), $j in (1, 2)  return ($i + $j)', '11122122', '');
  t('for $i in (10, 20), $j in (1, 2)  return $i + $j', '11122122', '');
  t('join(for $i in (10, 20), $j in (1, 2)  return ($i + $j), ",")', '11,12,21,22', '');
               //For-example of the standard. Attention: The example is wrong in the standard
  t('(for $a in fn:distinct-values(bib/book/author) return (bib/book/author[. = $a][1], bib/book[author = $a]/title))[1]','Stevens','<bib>' + '  <book>' + '    <title>TCP/IP Illustrated</title>' + '    <author>Stevens</author>' + '    <publisher>Addison-Wesley</publisher>' + '  </book>' + '  <book>' + '    <title>Advanced Programming in the Unix Environment</title>' + '    <author>Stevens</author>' + '    <publisher>Addison-Wesley</publisher>' + '  </book>' + '  <book>' + '    <title>Data on the Web</title>' + '    <author>Abiteboul</author>' + '    <author>Buneman</author>' + '    <author>Suciu</author>' + '  </book>' + '</bib>' );
  t('string-join(for $a in fn:distinct-values(bib/book/author) return (bib/book/author[. = $a][1], bib/book[author = $a]/title), ",")','Stevens,Stevens,TCP/IP Illustrated,Advanced Programming in the Unix Environment,Abiteboul,Data on the Web,Buneman,Data on the Web,Suciu,Data on the Web','');
  t('(for $a in fn:distinct-values(bib/book/author) return ((bib/book/author[. = $a])[1], bib/book[author = $a]/title))[1]','Stevens','');
  t('string-join(for $a in fn:distinct-values(bib/book/author) return ((bib/book/author[. = $a])[1], bib/book[author = $a]/title), ",")','Stevens,TCP/IP Illustrated,Advanced Programming in the Unix Environment,Abiteboul,Data on the Web,Buneman,Data on the Web,Suciu,Data on the Web','');

  t('some $x in (1,2,3) satisfies $x', 'true', '');
  t('every $x in (1,2,3) satisfies $x', 'true', '');
  t('some $x in (1,2,3) satisfies ($x = 1)', 'true', '');
  t('every $x in (1,2,3) satisfies ($x = 1)', 'false', '');
  t('some $x in (1,2,3) satisfies ($x = 0)', 'false', '');
  t('every $x in (1,2,3) satisfies ($x = 0)', 'false', '');
  t('some $x in (1,2,3), $y in (1,2,3) satisfies ($x=$y)', 'true', '');
  t('every $x in (1,2,3), $y in (1,2,3) satisfies ($x=$y)', 'false', '');
  t('some $x in (1,2,3), $y in (4,5,6) satisfies ($x=$y)', 'false', '');
  t('every $x in (1,2,3), $y in (4,5,6) satisfies ($x=$y)', 'false', '');
  t('some $x in (1,1,1), $y in (1,1,1) satisfies ($x=$y)', 'true', '');
  t('every $x in (1,1,1), $y in (1,1,1) satisfies ($x=$y)', 'true', '');
  t('some $test in (2,4,6) satisfies ($test mod 2 = 0)', 'true', '');
  t('every $test in (2,4,6) satisfies ($test mod 2 = 0)', 'true', '');
  t('some $test in (2,4,7) satisfies ($test mod 2 = 0)', 'true', '');
  t('every $test in (2,4,7) satisfies ($test mod 2 = 0)', 'false', '');
  t('some $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4', 'true', '');
  t('every $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4', 'false', '');
  t('some $x in (1, 2, "cat") satisfies $x * 2 = 4', 'true', '');
  t('every $x in (1, 2, "cat") satisfies $x * 2 = 4', 'false', '');

  t('if ("true") then 1 else 2', '1', '');
  t('if ("") then 1 else 2', '2', '');
  t('if ("false") then 1 else 2', '1', '');
  t('if (true()) then 1 else 2', '1', '');
  t('if (false()) then 1 else 2', '2', '');
  t('if (1) then 1 else 2', '1', '');
  t('if (0) then 1 else 2', '2', '');
  t('if ("a"="A") then 1 else 2', '1', '');
  t('if ("a"="b") then 1 else 2', '2', '');
  t('if ("true") then (1) else 2', '1', '');
  t('if ("false") then (1) else 2', '1', '');
  t('if ("") then (1) else 2', '2', '');
  t('if (true()) then (1) else 2', '1', '');
  t('if (false()) then (1) else 2', '2', '');
  t('if (1) then (1) else 2', '1', '');
  t('if (0) then (1) else 2', '2', '');
  t('if ("a"="A") then (1) else 2', '1', '');
  t('if ("a"="b") then (1) else 2', '2', '');
  t('if ("true") then 1 else (2)', '1', '');
  t('if ("false") then 1 else (2)', '1', '');
  t('if ("") then 1 else (2)', '2', '');
  t('if (true()) then 1 else (2)', '1', '');
  t('if (false()) then 1 else (2)', '2', '');
  t('if (1) then 1 else (2)', '1', '');
  t('if (0) then 1 else (2)', '2', '');
  t('if ("a"="A") then 1 else (2)', '1', '');
  t('if ("a"="b") then 1 else (2)', '2', '');
  t('if ("true") then (1) else (2)', '1', '');
  t('if ("false") then (1) else (2)', '1', '');
  t('if ("") then (1) else (2)', '2', '');
  t('if (true()) then (1) else (2)', '1', '');
  t('if (false()) then (1) else (2)', '2', '');
  t('if (1) then (1) else (2)', '1', '');
  t('if (0) then (1) else (2)', '2', '');
  t('if ("a"="A") then (1) else (2)', '1', '');
  t('if ("a"="b") then (1) else (2)', '2', '');
  t('if (true()) then 3+4*7 else 2*2+5', '31', '');
  t('if (false()) then 3+4*7 else 2*2+5', '9', '');
  t('if (true()) then 4*7+3 else 5+2*2', '31', '');
  t('if (false()) then 4*7+3 else 5+2*2', '9', '');

  t('if (true()) then for $x in (1,2,3) return $x else for $x in (4,5,6) return $x', '123', '');
  t('join(if (true()) then (for $x in (1,2,3) return $x) else for $x in (4,5,6) return $x,",")', '1,2,3', '');
  t('join(if (false()) then (for $x in (1,2,3) return $x) else for $x in (4,5,6) return $x,",")', '4,5,6', '');
  t('join(if (true()) then for $x in (1,2,3) return $x else (for $x in (4,5,6) return $x),",")', '1,2,3', '');
  t('join(if (false()) then for $x in (1,2,3) return $x else (for $x in (4,5,6) return $x),",")', '4,5,6', '');
  t('join(if (true()) then (for $x in (1,2,3) return $x) else (for $x in (4,5,6) return $x),",")', '1,2,3', '');
  t('join(if (false()) then (for $x in (1,2,3) return $x) else (for $x in (4,5,6) return $x),",")', '4,5,6', '');
  t('join(if (true()) then for $x in (1,2,3) return $x else for $x in (4,5,6) return $x,",")', '1,2,3', '');
  t('join(if (false()) then for $x in (1,2,3) return $x else for $x in (4,5,6) return $x,",")', '4,5,6', '');
  t('join(for $x in (1,2,3,4,5) return if ($x mod 2 = 0) then $x else (),",")', '2,4', '');
  t('join(for $x in (1,2,3,4,5) return if ($x mod 2 = 1) then $x else (),",")', '1,3,5', '');
  t('join(for $x in (1,2,3,4,5) return if ($x mod 2 = 0) then $x else "",",")', ',2,,4,', '');
  t('deep-equal(for $x in (1,2,3,4,5) return if ($x mod 2 = 1) then $x else (),(1,3,5))', 'true', '');
  t('deep-equal(for $x in (1,2,3,4,5) return if ($x mod 2 = 1) then $x else (),(1,3,5,7))', 'false', '');
  t('for $x in 4 return $x + 1', '5', '');
  t('some $x in 4 satisfies $x + 1 = 5', 'true', '');
  t('every $x in 4 satisfies $x + 1 = 5', 'true', '');
  t('some $x in 4 satisfies $x + 1 = 4', 'false', '');
  t('every $x in 4 satisfies $x + 1 = 4', 'false', '');
  t('(some $x in 4 satisfies $x + 1 = 4) = (every $x in 4 satisfies $x + 1 = 4)', 'true', '');
  t('(some $x in 4 satisfies $x + 1 = 5) = (every $x in 4 satisfies $x + 1 = 5)', 'true', '');
  t('some $x in (1,2,3) satisfies ()', 'false', '');
  t('every $x in (1,2,3) satisfies ()', 'false', '');
  t('some $x in (1,2,3) satisfies ($x > 0)', 'true', '');
  t('every $x in (1,2,3) satisfies ($x > 0)', 'true', '');
  t('some $x in 1 to 3 satisfies ($x > 0)', 'true', '');
  t('every $x in 1 to 3 satisfies ($x > 0)', 'true', '');
  t('join(for $x in 1 to 10 return 2*$x,",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('(for $x in (1,2,3), $y in (1,2) return concat("x",$x,"y",$y))[1]', 'x1y1', '');
  t('join(for $x in (1,2,3), $y in (1,2) return concat("x",$x,"y",$y),",")', 'x1y1,x1y2,x2y1,x2y2,x3y1,x3y2', '');
  t('for $i in (1, 2), $j in (1, 2)  return $i + $j', '2334', '');
  t('for $i in (1,2  ), $j in (1,2) return $i + $j', '2334', '');
  t('for $i in (1,2,3), $j in (1,2) return $i+$j', '233445', '');
  t('for $x in (1,2,3), $y in (1,2) return $y+$x', '233445', '');
  t('for $x in (1,2,3), $y in (1,2) return ($y*$x)', '122436', '');
  t('for $x in (1,2,3), $y in (1,2) return $y*$x', '122436', '');
  t('for $x in 1 to 3, $y in (1,2) return $y*$x', '122436', '');
  t('join(for $x in 1 to 3, $y in (1,2) return $y*$x,",")', '1,2,2,4,3,6', '');
  t('join(for $x in 1 to 3, $y in (1,2) return $y*$x,",")', '1,2,2,4,3,6', '');
  t('some $x in 1 to 3, $y in (1,2) satisfies $x = $y', 'true', '');
  t('every $x in 1 to 3, $y in (1,2) satisfies $x = $y', 'false', '');
  t('some $x in 1 to 3, $y in (1,2) satisfies $x > $y', 'true', '');
  t('some $x in 1 to 3, $y in (-1,-2) satisfies $x > $y', 'true', '');
  t('every $x in 1 to 3, $y in (1,2) satisfies $x > $y', 'false', '');
  t('every $x in 1 to 3, $y in (-1,-2) satisfies $x > $y', 'true', '');
  t('every $x in 1 to 3, $y in (for $k in (1,2) return -1*$k) satisfies $x > $y', 'true', '');
  t('for $x in 1 to 3, $y in (for $k in (1,2) return -1*$k) return concat($x,">",$y)', '1>-11>-22>-12>-23>-13>-2', '');
               //Amazing overloaded meanings
  t('', '', '<for><return>RR</return><in>a<return>ar</return><return>ar2</return></in><in>b</in><in><return>cr</return>c<return>cr2</return></in><return>RX</return><if>F<then>THN</then><else>LS</else></if></for>');
  t('for $for in for/in return $for/return', 'arar2crcr2', '');
  t('string-join(for $for in for/in return $for/return,",")', 'ar,ar2,cr,cr2', '');
  t('string-join(for $for in for/in return $for,",")', 'aarar2,b,crccr2', '');
  t('string-join(for $for in for/in return for/return,",")', 'RR,RX,RR,RX,RR,RX', '');
  t('string-join(for $for in for/in return for,",")', 'RRaarar2bcrccr2RXFTHNLS,RRaarar2bcrccr2RXFTHNLS,RRaarar2bcrccr2RXFTHNLS', '');
  t('string-join(for $for in for/in return return,",")', '', '');
  t('string-join(for $for in for/in, $in in $for/return return $in,",")', 'ar,ar2,cr,cr2', '');
  t('string-join(for $for in for/* return $for/then,",")', 'THN', '');
  t('string-join(for $for in for/* return for/then,",")', '', '');
  t('string-join(for $for in for/* return for/if/then,",")', 'THN,THN,THN,THN,THN,THN', '');
  t('string-join(for $for in for/* return for/*/then,",")', 'THN,THN,THN,THN,THN,THN', '');
  t('string-join(for $for in for/if return $for/then,",")', 'THN', '');
  t('string-join(for,",")', 'RRaarar2bcrccr2RXFTHNLS', '');
  t('string-join(for/in,",")', 'aarar2,b,crccr2', '');
  t('string-join(for | for/in,",")', 'RRaarar2bcrccr2RXFTHNLS,aarar2,b,crccr2', '');
  t('some $x in for satisfies $x/return', 'true', '');
  t('some $x in for satisfies $x/in/return', 'true', '');
  t('some $x in for/in satisfies $x/return', 'true', '');
  t('some $x in for/in satisfies $x/return[2]', 'true', '');
  t('some $x in for satisfies for', 'true', '');
  t('some $x in for satisfies return', 'false', '');
  t('some $x in for satisfies for/return', 'true', '');
  t('some $x in for satisfies for/in/return', 'true', '');
  t('some $x in for satisfies satisfies', 'false', '');
  t('every $x in for satisfies $x/return', 'true', '');
  t('every $x in for satisfies $x/in/return', 'true', '');
  t('every $x in for/in satisfies $x/return', 'false', '');
  t('every $x in for/in satisfies $x/return[2]', 'false', '');
  t('every $x in for satisfies for', 'true', '');
  t('every $x in for satisfies return', 'false', '');
  t('every $x in for satisfies for/return', 'true', '');
  t('every $x in for satisfies for/in/return', 'true', '');
  t('every $x in for satisfies satisfies', 'false', '');
  t('some $satisfies in satisfies satisfies satisfies', 'true', '<satisfies></satisfies>');
  t('every $satisfies in satisfies satisfies satisfies', 'true', '');
  t('some $satisfies in satisfies satisfies satisfiesx', 'false', '');
  t('every $satisfies in satisfies satisfies satisfiesx', 'false', '');
  t('some $satisfies in satisfiesx satisfies satisfies', 'false', '');
  t('every $satisfies in satisfiesx satisfies satisfies', 'true', '');
  t('some $satisfies in satisfiesx satisfies satisfiesx', 'false', '');
  t('every $satisfies in satisfiesx satisfies satisfiesx', 'true', '');
  t('some $satisfies in satisfies satisfies satisfies/satisfies', 'false', '');
  t('every $satisfies in satisfies satisfies satisfies/satisfies', 'false', '');
  t('some $satisfies in satisfies satisfies satisfies/satisfies', 'true', '<satisfies><satisfies><satisfies></satisfies></satisfies></satisfies>');
  t('every $satisfies in satisfies satisfies satisfies/satisfies', 'true', '');
  t('some $satisfies in satisfies satisfies satisfies/satisfies/satisfies', 'true', '');
  t('every $satisfies in satisfies satisfies satisfies/satisfies/satisfies', 'true', '');
  t('some $satisfies in satisfies satisfies satisfies/satisfies/satisfies/satisfies', 'false', '');
  t('every $satisfies in satisfies satisfies satisfies/satisfies/satisfies/satisfies', 'false', '');
  t('some $satisfies in satisfies/satisfies satisfies satisfies/satisfies/satisfies', 'true', '');
  t('every $satisfies in satisfies/satisfies satisfies satisfies/satisfies/satisfies', 'true', '');
  t('some $satisfies in satisfies/satisfies satisfies satisfies/satisfies/satisfies/satisfies', 'false', '');
  t('every $satisfies in satisfies/satisfies satisfies satisfies/satisfies/satisfies/satisfies', 'false', '');
  t('some $satisfies in satisfies/satisfies satisfies $satisfies/satisfies', 'true', '');
  t('every $satisfies in satisfies/satisfies satisfies $satisfies/satisfies', 'true', '');
  t('some $satisfies in satisfies/satisfies satisfies $satisfies/satisfies/satisfies', 'false', '');
  t('every $satisfies in satisfies/satisfies satisfies $satisfies/satisfies/satisfies', 'false', '');
  t('some $satisfies in satisfies/satisfies/satisfies satisfies $satisfies', 'true', '');
  t('every $satisfies in satisfies/satisfies/satisfies satisfies $satisfies', 'true', '');
  t('some $satisfies in satisfies/satisfies/satisfies satisfies $satisfies/satisfies', 'false', '');
  t('every $satisfies in satisfies/satisfies/satisfies satisfies $satisfies/satisfies', 'false', '');
  t('some $satisfies in satisfies/satisfies/satisfies/satisfies satisfies $satisfies', 'false', '');
  t('every $satisfies in satisfies/satisfies/satisfies/satisfies satisfies $satisfies', 'true', '');
  t('if (true) then some $satisfies in satisfies satisfies satisfies else every $satisfies in satisfies satisfies satisfies', 'true', '');
  t('if (false) then some $satisfies in satisfies satisfies satisfies else every $satisfies in satisfies satisfies satisfies', 'true', '');
  t('for $in in in return in', 'ABAB', '<in>A</in><in>B</in>');
  t('for $in in return return return', 't', '<return>t</return>');
  t('for $in in if return if', 't', '<if>t</if>');
  t('for $in in some return some', 't', '<some>t</some>');
  t('for $in in div return idiv', 'y', '<div>x</div><idiv>y</idiv>');
  t('every $a in () satisfies $a = ">a<" ', 'true', '');
  t('some $a in for $x in div/x return concat(">",$x,"<") satisfies $a = ">a<" ', 'true', '<div><x>a</x><x>b</x><x>c</x></div>');
  t('every $a in for $x in div/x return concat(">",$x,"<") satisfies $a = ">a<" ', 'false', '');
  t('some $a in for $x in div/x return concat(">",$x,"<") satisfies $a = ">x<" ', 'false', '');
  t('every $a in for $x in div/x return concat(">",$x,"<") satisfies $a = ">x<" ', 'false', '');
  t('some $a in for $x in div/xy return concat(">",$x,"<") satisfies $a = ">x<" ', 'false', '');
  t('string-join(for $x in div/xy return concat(">",$x,"<"),";") ', '', '');
  t('every $a in for $x in div/xy return concat(">",$x,"<") satisfies $a = ">x<" ', 'true', '');  //empty sequence, so every is always satisfied
  t('some $a in for $x in div/x return concat(">",$x,"<") satisfies $a = (">x<",">y") ', 'false', '');
  t('every $a in for $x in div/x return concat(">",$x,"<") satisfies $a = (">x<",">y<") ', 'false', '');
  t('some $a in for $x in div/x return concat(">",$x,"<") satisfies $a = (">a<",">b<",">c<",">d<") ', 'true', '');
  t('every $a in for $x in div/x return concat(">",$x,"<") satisfies $a = (">a<",">b<",">c<",">d<") ', 'true', '');

               //Variable defining
  t('x := 123', '123', '');
  t('$x', '123', '');
  f('$X', 'err:XPST0008'); //fail test
  t('X := 456', '456', '');
  t('$x', '123', '');
  t('$X', '456', '');
  t('jh:=concat($X,"COE")', '456COE', '');
  t('$jh', '456COE', '');
  t('maus := "haus"', 'haus', '');
  t('$maus', 'haus', '');
  t('$maus := "haus2"', 'haus2', '');
  t('$maus', 'haus2', '');
  t('$maus:= "haus3"', 'haus3', '');
  t('$maus', 'haus3', '');
  t('$maus:= "haus4"', 'haus4', '');
  t('$maus', 'haus4', '');
  t('a := 1, b:= 2', '12', '');
  t('$a', '1', '');
  t('$b', '2', '');
  t('a := 10, A:= 20', '1020', '');
  t('$a', '10', '');
  t('$b', '2', '');
  t('$A', '20', '');
  t('concat($a,";",$b,";",$A)', '10;2;20', '');
  t('a := 1, b:= ($a * 3), c := $b + 7', '1310', '');
  t('concat($a,";",$b,";",$c)', '1;3;10', '');
  t('a := (1,2,3,42), b:= 4+5, c:=2*3 + 1, d:=a/text()', '1234297hallo', '<a>hallo</a>');
  t('join($a,";")', '1;2;3;42', '');
  t('concat($b,";",$c,";",$d)', '9;7;hallo', '');
  t('x := for $y in $a return $y+10 ', '11121352', '');
  t('join($x,",")', '11,12,13,52', '');
  t('join(m := (1,2,3),",")', '1,2,3', '');
  t('join($m,",")', '1,2,3', '');
  t('join((m := (1,2,3), b := (4,5,6), c:=(7,8,9)),",")', '1,2,3,4,5,6,7,8,9', '');
  t('join($m,",")', '1,2,3', '');
  t('join($b,",")', '4,5,6', '');
  t('join($c,",")', '7,8,9', '');
  t('join((m := (1,2,3), b := ($m, 10), c:=(100,$b, 1000)),",")', '1,2,3,1,2,3,10,100,1,2,3,10,1000', '');
  t('if (1 = 1) then a := 10 else a := 20', '10', '');
  t('$a', '10', '');
  t('if (() = 1) then a := 10 else a := 20', '20', '');
  t('$a', '20', '');
  t('concat(join(eval("a:=30, b := 12")), $a, $b)', '30 123012', '');


               //collations
  t('starts-with("tattoo", "tat", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'true', '');
  t('starts-with("tattoo", "att", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'true', '');
  t('starts-with("tattoo", "tat", "case-insensitive-clever")', 'true', '');
  t('starts-with("tattoo", "att", "case-insensitive-clever")', 'false', '');
  t('starts-with("tattoo", "TAT", "case-insensitive-clever")', 'true', '');
  t('starts-with("tattoo", "tat", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'true', '');
  t('starts-with("tattoo", "att", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('starts-with("tattoo", "tat", "case-sensitive-clever")', 'true', '');
  t('starts-with("tattoo", "att", "case-sensitive-clever")', 'false', '');
  t('starts-with("tattoo", "TAT", "case-sensitive-clever")', 'false', '');
  t('starts-with("tattoo", "tat", "http://www.w3.org/2005/xpath-functions/collation/codepoint/")', 'true', '');
  t('starts-with("tattoo", "att", "http://www.w3.org/2005/xpath-functions/collation/codepoint/")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.w3.org/2005/xpath-functions/collation/codepoint/")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.w3.org/2005/xpath-functions/collation/codepoint")', 'false', '');
  t('starts-with("tattoo", "tat", "fpc-localized-case-insensitive")', 'true', '');
  t('starts-with("tattoo", "att", "fpc-localized-case-insensitive")', 'false', '');
  t('starts-with("tattoo", "TAT", "fpc-localized-case-insensitive")', 'true', '');
  t('starts-with("tattoo", "tat", "fpc-localized-case-sensitive")', 'true', '');
  t('starts-with("tattoo", "att", "fpc-localized-case-sensitive")', 'false', '');
  t('starts-with("tattoo", "TAT", "fpc-localized-case-sensitive")', 'false', '');
  t('starts-with("tattoo", "tat", "http://www.benibela.de/2012/pxp/fpc-localized-case-insensitive")', 'true', '');
  t('starts-with("tattoo", "att", "http://www.benibela.de/2012/pxp/fpc-localized-case-insensitive")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.benibela.de/2012/pxp/fpc-localized-case-insensitive")', 'true', '');
  t('starts-with("tattoo", "tat", "http://www.benibela.de/2012/pxp/fpc-localized-case-sensitive")', 'true', '');
  t('starts-with("tattoo", "att", "http://www.benibela.de/2012/pxp/fpc-localized-case-sensitive")', 'false', '');
  t('starts-with("tattoo", "TAT", "http://www.benibela.de/2012/pxp/fpc-localized-case-sensitive")', 'false', '');
               //,('starts-with("äöüaou", "ÄÖÜA", "fpc-localized-case-insensitive")', 'true', '')
               //,('starts-with("äöüaou", "ÄÖÜA", "fpc-localized-case-sensitive")', 'false', '')
  t('ends-with("tattoo", "too", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'true', '');
  t('ends-with("tattoo", "atto", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'false', '');
  t('ends-with("tattoo", "TTOO", "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'true', '');
  t('ends-with("tattoo", "too", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'true', '');
  t('ends-with("tattoo", "atto", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('ends-with("tattoo", "TTOO", "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('string-join(distinct-values(("a", "A", "aA", "AA")),",")', 'a,aA', '');
  t('string-join(distinct-values(("a", "A", "aA", "AA"), "http://www.benibela.de/2012/pxp/case-insensitive-clever"),",")', 'a,aA', '');
  t('string-join(distinct-values(("a", "A", "aA", "AA"), "http://www.benibela.de/2012/pxp/case-sensitive-clever"),",")', 'a,A,aA,AA', '');
  t('deep-equal(("A","B"), ("a","b"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'true', '');
  t('deep-equal(("A","B"), ("a","bc"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', 'false', '');
  t('deep-equal(("A","B"), ("A","B"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'true', '');
  t('deep-equal(("A","B"), ("a","b"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('deep-equal(("A","B"), ("a","bc"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', 'false', '');
  t('max(("10haus", "100HAUS", "2haus", "099haus", "100haus"))', '100haus', '');
  t('max(("10haus", "100haus", "2haus", "099haus", "100HAUS"))', '100HAUS', '');
  t('max(("10haus", "100haus", "2haus", "099haus", "100HAUS"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', '100HAUS', '');
  t('max(("10haus", "100HAUS", "2haus", "099haus", "100haus"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', '100haus', '');
  t('max(("10haus", "100haus", "2haus", "099haus", "100HAUS"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', '100haus', '');
  t('max(("10haus", "100HAUS", "2haus", "099haus", "100haus"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', '100haus', '');
  t('max(("10haus", "100haus", "2haus", "099haus", "100HAUS"), "fpc-localized-case-insensitive")', '2haus', '');
  t('max(("10haus", "100HAUS", "2haus", "099haus", "100haus"), "fpc-localized-case-insensitive")', '2haus', '');
  t('max(("10haus", "100haus", "2haus", "099haus", "100HAUS"), "fpc-localized-case-sensitive")', '2haus', '');
  t('max(("10haus", "100HAUS", "2haus", "099haus", "100haus"), "fpc-localized-case-sensitive")', '2haus', '');
  t('min(("10haus", "100haus", "2haus", "099haus", "2HAUS"))', '2haus', '');
  t('min(("10haus", "100haus", "2HAUS", "099haus", "2haus"))', '2HAUS', '');
  t('min(("10haus", "100haus", "2haus", "099haus", "2HAUS"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', '2haus', '');
  t('min(("10haus", "100HAUS", "2HAUS", "099haus", "2haus"), "http://www.benibela.de/2012/pxp/case-insensitive-clever")', '2HAUS', '');
  t('min(("10haus", "100haus", "2haus", "099haus", "2HAUS"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', '2HAUS', '');
  t('min(("10haus", "100HAUS", "2HAUS", "099haus", "2haus"), "http://www.benibela.de/2012/pxp/case-sensitive-clever")', '2HAUS', '');
  t('min(("10haus", "100haus", "2haus", "099haus", "2HAUS"), "fpc-localized-case-sensitive")', '099haus', '');
  t('min(("10haus", "100haus", "2HAUS", "099haus", "2haus"), "fpc-localized-case-sensitive")', '099haus', '');
  t('min(("10haus", "100haus", "2haus", "099haus", "2HAUS"), "fpc-localized-case-insensitive")', '099haus', '');
  t('min(("10haus", "100haus", "2HAUS", "099haus", "2haus"), "fpc-localized-case-insensitive")', '099haus', '');
  t('default-collation()', 'http://www.benibela.de/2012/pxp/case-insensitive-clever', '');

               //IDs
  t('string-join(id("a"),",")', 'singleA', '<html>void<y id="c">singleC</y><raven id=" ">nevermore</raven><table><x id="a">singleA</x><z id="b">doubleB</z></table>void<end id="b">doubleB2</end></html>');
  t('string-join(id("b"),",")', 'doubleB,doubleB2', '');
  t('string-join(id("z"),",")', '', '');
  t('string-join(id("a b c"),",")', 'singleC,singleA,doubleB,doubleB2', '');
  t('string-join(id(("a b", "c")),",")', 'singleC,singleA,doubleB,doubleB2', '');
  t('string-join(id(("a       b", "c")),",")', 'singleC,singleA,doubleB,doubleB2', '');
  t('string-join(id(" "),",")', '', '');
  t('static-base-uri()', 'pseudo://test', '');
  t('string-join(id("b", html/y),",")', 'doubleB,doubleB2', '');
  t('string-join(id("z", html/y),",")', '', '');

               //http://www.dpawson.co.uk/xsl/rev2/exampler2.html
               //http://www.w3.org/TR/xslt20/#function-function-available?? that's xsl not xpath





   //type tests
   t('type-of(xs:int(-2147483648))', 'int', '');
   t('type-of(xs:int(2147483647))', 'int', '');
   t('type-of(xs:short(-32768))', 'short', '');
   t('type-of(xs:short(32767))', 'short', '');
   t('type-of(xs:byte(-128))', 'byte', '');
   t('type-of(xs:byte(127))', 'byte', '');
   t('type-of(xs:nonPositiveInteger(0))', 'nonPositiveInteger', '');
   t('type-of(xs:negativeInteger(-1))', 'negativeInteger', '');
   t('type-of(xs:nonNegativeInteger(0))', 'nonNegativeInteger', '');
   t('type-of(xs:positiveInteger(1))', 'positiveInteger', '');
   t('type-of(xs:unsignedLong(0))', 'unsignedLong', '');
   t('type-of(xs:unsignedInt(0))', 'unsignedInt', '');
   t('type-of(xs:unsignedInt(4294967295))', 'unsignedInt', '');
   t('type-of(xs:unsignedShort(0))', 'unsignedShort', '');
   t('type-of(xs:unsignedShort(65535))', 'unsignedShort', '');
   t('type-of(xs:unsignedByte(0))', 'unsignedByte', '');
   t('type-of(xs:unsignedByte(255))', 'unsignedByte', '');
   t('xs:untypedAtomic(''abc'')', 'abc', '');
   t('type-of(xs:untypedAtomic(''abc''))', 'untypedAtomic', '');
//   t('xs:untyped(''abc'')', 'abc', '');
//   t('type-of(xs:untyped(''abc''))', 'untyped', '');
   t('xs:normalizedString(''abc'')', 'abc', '');
   t('type-of(xs:normalizedString(''abc''))', 'normalizedString', '');
   t('xs:token(''abc'')', 'abc', '');
   t('type-of(xs:token(''abc''))', 'token', '');
   t('xs:language(''abc'')', 'abc', '');
   t('type-of(xs:language(''abc''))', 'language', '');
   t('xs:NMTOKEN(''abc'')', 'abc', '');
   t('type-of(xs:NMTOKEN(''abc''))', 'NMTOKEN', '');
   t('xs:Name(''abc'')', 'abc', '');
   t('type-of(xs:Name(''abc''))', 'Name', '');
   if UsingFLRE then begin
     t('xs:NCName(''abc'')', 'abc', '');
     t('type-of(xs:NCName(''abc''))', 'NCName', '');
     t('xs:ID(''abc'')', 'abc', '');
     t('type-of(xs:ID(''abc''))', 'ID', '');
     t('xs:IDREF(''abc'')', 'abc', '');
     t('type-of(xs:IDREF(''abc''))', 'IDREF', '');
     t('xs:ENTITY(''abc'')', 'abc', '');
     t('type-of(xs:ENTITY(''abc''))', 'ENTITY', '');
     t('xs:anyURI(''abc'')', 'abc', '');
     t('type-of(xs:anyURI(''abc''))', 'anyURI', '');
   end;
   t('xs:QName(''abc'')', 'abc', '');
   t('type-of(xs:QName(''abc''))', 'QName', '');
   //t('xs:NOTATION(''abc'')', 'abc', '');
   //t('type-of(xs:NOTATION(''abc''))', 'NOTATION', '');
   t('xs:unsignedByte(255)', '255', '');
   f('xs:unsignedByte(256)', 'err:FORG0001');
   f('xs:unsignedByte(257)', 'err:FORG0001');
   t('255 castable as xs:unsignedByte', 'true', '');
   t('256 castable as xs:unsignedByte', 'false', '');
   t('xs:byte(127)', '127');
   f('xs:byte(128)', 'err:FORG0001');
   t('xs:byte(-1)', '-1', '');
   t('xs:byte(-127)', '-127', '');
   t('xs:byte(-128)', '-128', '');
   f('xs:byte(-129)', 'err:FORG0001');
   f('xs:byte(-130)', 'err:FORG0001');
   f('xs:unsignedByte(-1)', 'err:FORG0001');
   f('xs:unsignedByte(-2)', 'err:FORG0001');
   t('type-of(xs:float(4.0))','float','');
   t('type-of(xs:double(4.0))','double','');
   t('type-of(xs:decimal(4.0))','decimal','');
   t('type-of(4.0)','decimal','');
   t('type-of(4.0e1)','double','');
   t('type-of(4)','integer','');
   t('xs:byte(1)+xs:byte(2)','3','');
   t('type-of(xs:byte(1)+xs:byte(2))','integer','');
   t('xs:short(1)-xs:short(2)','-1','');
   t('type-of(xs:short(1)-xs:short(2))','integer','');
   t('xs:short(4)*xs:short(2)','8','');
   t('type-of(xs:short(4)*xs:short(2))','integer','');
   t('xs:short(1) div xs:short(2)','0.5','');
   t('type-of(xs:short(1) div xs:short(2))','decimal','');
   t('type-of(xs:short(1) div xs:short(1))','decimal','');
   t('xs:short(1) idiv xs:short(2)','0','');
   t('type-of(xs:short(1) idiv xs:short(2))','integer','');
   t('xs:byte(1)+xs:short(2)','3','');
   t('type-of(xs:byte(1)+xs:short(2))','integer','');
   t('xs:byte(1)+xs:unsignedShort(2)','3','');
   t('type-of(xs:byte(1)+xs:unsignedShort(2))','integer','');
   t('xs:byte(1)*xs:int(2)','2','');
   t('type-of(xs:byte(1)*xs:int(2))','integer','');
   t('xs:byte(1)+2','3','');
   t('type-of(xs:byte(1)+2)','integer','');
   t('xs:float(1)+2','3','');
   t('type-of(xs:float(1)+2)','float','');
   t('xs:double(1)+2','3','');
   t('type-of(xs:double(1)+2)','double','');
   t('xs:decimal(1)+2','3','');
   t('type-of(xs:decimal(1)+2)','decimal','');
   t('xs:float(1)*xs:double(2)','2','');
   t('type-of(xs:float(1)*xs:double(2))','double','');
   t('xs:float(1)*2','2','');
   t('type-of(xs:float(1)*2)','float','');
   t('xs:float(1)*2.0','2','');
   t('type-of(xs:float(1)*2.0)','float','');
   t('xs:double(1)*2','2','');
   t('type-of(xs:double(1)*2)','double','');
   t('max((xs:float(1),xs:double(2),xs:decimal(3)))','3','');
   t('type-of(max((xs:float(1),xs:double(2),xs:decimal(3))))','double','');
   t('max((xs:float(1),xs:decimal(3)))','3','');
   t('type-of(max((xs:float(1),xs:decimal(3))))','float','');
   t('min((xs:float(1),xs:double(2),xs:decimal(3)))','1','');
   t('type-of(min((xs:float(1),xs:double(2),xs:decimal(3))))','double','');
   t('sum((xs:float(1),xs:decimal(3)))','4','');
   t('type-of(sum((xs:float(1),xs:decimal(3))))','float','');
   t('xs:byte(1) instance of byte','true','');
   t('xs:byte(1) instance of short','true','');
   t('xs:byte(1) instance of integer','true','');
   t('xs:byte(1) instance of decimal','true','');
   t('xs:byte(1) instance of float','false','');
   t('xs:byte(1) instance of double','false','');
   t('xs:byte(1) instance of unsignedByte','false','');
   t('xs:float(1) instance of float','true','');
   t('xs:float(1) instance of double','false','');
   t('xs:float(1) instance of decimal','false','');
   t('xs:double(1) instance of float','false','');
   t('xs:double(1) instance of double','true','');
   t('xs:double(1) instance of decimal','false','');
   t('xs:decimal(1) instance of float','false','');
   t('xs:decimal(1) instance of double','false','');
   t('xs:decimal(1) instance of decimal','true','');
   t('xs:decimal(1) instance of integer','false','');
   t('4.0 instance of integer','false','');
   t('xs:decimal(1) instance (: ... :) of decimal','true','');
   t('xs:decimal(1) instance of (: .. :) decimal','true','');
   t('xs:date("1800-02-03") instance of dateTime','false','');
   t('xs:time("23:59:59") instance of dateTime','false','');
   f('xs:time("23:59:59") instance of datetime','err:XPST0051');
   t('xs:untypedAtomic("abc") instance of string','false','');
   t('string("def") instance of xs:untypedAtomic','false','');
//   t('xs:untyped("abc") instance of string','false','');
   t('xs:byte(1) castable as byte','true','');
   t('xs:byte(1) castable as integer','true','');
   t('xs:byte(1) castable as float','true','');
   t('xs:byte(1) castable as double','true','');
   t('xs:byte(1) castable as unsignedByte','true','');
   t('xs:byte(-1) castable as byte','true','');
   t('xs:byte(-1) castable as integer','true','');
   t('xs:byte(-1) castable as float','true','');
   t('xs:byte(-1) castable as double','true','');
   t('xs:byte(-1) castable as unsignedByte','false','');
   t('xs:byte(-1) castable as unsignedByte','false','');
   t('1.0 castable as positiveInteger','true','');
   t('1.0e5 castable as positiveInteger','true','');
   t('1.5 castable as positiveInteger','true','');
   t('0.5 castable as positiveInteger','false','');
   t('-1.0 castable as positiveInteger','false','');
   t('-1.0 castable as integer','true','');
   t('-1.2 castable as integer','true','');
   t('-1.2 castable as boolean','true','');
   t('5 castable as boolean','true','');
   t('0 castable as boolean','true','');
   t('"false" castable as boolean','true','');
   t('xs:date("2050-01-01") castable as dateTime','true','');
   t('xs:time("00:00:00") castable as dateTime','false','');
   t('xs:dateTime("1899-12-31T04:04:04") castable as date','true','');
   t('xs:dateTime("2012-02-01T14:15:16") castable as time','true','');
   t('xs:dateTime("1234-12-12T01:01:01") castable as time','true','');
   t('xs:dateTime("6543-12-12T08:08:08") castable as date','true','');
   t('xs:dateTime("65436-12-12T08:08:08") castable as date','true','');
   t('xs:date("1650-04-12") castable as time','false','');
   t('xs:time("12:00:00") castable as date','false','');
   t('xs:time("12:12:59") castable as untypedAtomic','true','');
   t('xs:date("2000-01-01") castable as untypedAtomic','true','');
   t('0.5 castable as untypedAtomic','true','');
//   t('0.5 castable as untyped','true',''); todo
   t('xs:untypedAtomic("0.5") castable as float','true','');
   t('xs:untypedAtomic("abc") castable as float','false','');
   t('xs:untypedAtomic("abc") castable as string','true','');
   t('xs:untypedAtomic("---15") castable as gDay','true','');
   t('"---15" castable as gDay','true','');
   t('"---15Z" castable as gDay','true','');
   t('"---15+12:30" castable as gDay','true','');
   t('"---15foobar" castable as gDay','false','');
   t('"---5" castable as gDay','false',''); //true?
   t('"---05" castable as gDay','true','');
   t('"--12" castable as gMonth','true','');
   t('"--13" castable as gMonth','false','');
   t('"--12-10" castable as gMonth','false','');
   t('"--12-29" castable as gMonthDay','true','');
   t('"--13-29" castable as gMonthDay','false','');
   t('"2010" castable as gYear','true','');
   t('"2010X" castable as gYear','false','');
   t('"2010Z" castable as gYear','true',''); //2010 in UTC
   t('"2010-12" castable as gYearMonth','true','');
   t('"2010-13" castable as gYearMonth','false','');
   t('"2010-12-10" castable as gYearMonth','false','');
   t('"2010-12+10:00" castable as gYearMonth','true',''); //with timezone
   t('type-of(xs:gYear("2010Z"))','gYear','');
   t('type-of(xs:gMonth("--12-10:00"))','gMonth','');
   t('xs:gMonth("--12")','--12','');
   t('xs:gMonthDay("--12-03")','--12-03','');
   t('xs:gDay("---30")','---30','');
   t('xs:hexBinary("2020")','2020','');
   t('-1.2 cast as boolean','true','');
   t('5 cast as boolean','true','');
   t('0 cast  as boolean','false','');
   t('"false" cast as boolean','false','');
   t('xs:byte(-1) cast as double','-1','');
   t('xs:time("23:45:59") cast as untypedAtomic','23:45:59','');
   t('xs:date("1999-12-31") cast as untypedAtomic','1999-12-31','');
   t('0.5 cast as untypedAtomic','0.5','');
   t('xs:untypedAtomic("0.5") cast as float','0.5','');
   t('xs:untypedAtomic("abc") cast as string','abc','');
   t('type-of(xs:time("00:59:00") cast as untypedAtomic)','untypedAtomic','');
   t('type-of(xs:date("2012-12-21") cast as untypedAtomic)','untypedAtomic','');
   t('type-of(0.5 cast as untypedAtomic)','untypedAtomic','');
   t('type-of(xs:untypedAtomic("0.5") cast as float)','float','');
   t('type-of(xs:untypedAtomic("abc") cast as string)','string','');









  t('123 instance of anyAtomicType', 'true', '');
  t('(5,4,2) instance of anyAtomicType', 'false', '');
  t('(5,4,2) instance of anyAtomicType+', 'true', '');
  t('123.6 instance of anySimpleType', 'true', '');
  ///t('xs:Name("B4") instance of anyType', 'true', '');
  //             ,('(5,4,2) instance of anySimpleType', 'true', '') todo: check ??
  //             ,('(5,4,2) instance of anyType', 'true', '') todo: check??

                //durations
  t('yearMonthDuration("P1Y1M") * 0.5', 'P7M', '');
  t('dayTimeDuration("P4D") ', 'P4D', '');
  t('duration("P1Y2M3D") ', 'P1Y2M3D', '');
  t('duration("P1Y2M3DT5H6M70S") ', 'P1Y2M3DT5H7M10S', '');
  t('duration("P0Y0M0DT5H6M70.123S") ', 'PT5H7M10.123S', '');
  t('duration("P0M") ', 'PT0S', '');
  t('yearMonthDuration("P0Y0M") ', 'P0M', '');
  t('dayTimeDuration("PT0H0S") ', 'PT0S', '');
  t('dayTimeDuration("PT1H") ', 'PT1H', '');
  t('dayTimeDuration("-PT1H") ', '-PT1H', '');
  t('yearMonthDuration("-P3Y") ', '-P3Y', '');
  t('yearMonthDuration("P4Y0M") div 2 ', 'P2Y', '');
  t('dayTimeDuration("P3D") div 4', 'PT18H', '');
  t('xs:dayTimeDuration("PT2H2M") div xs:dayTimeDuration("PT1H1M")', '2', '');
  t('xs:yearMonthDuration("P1Y") div xs:yearMonthDuration("P3M")', '4', '');
  t('xs:yearMonthDuration("P7M") + xs:yearMonthDuration("P6M")', 'P1Y1M', '');
  t('xs:dayTimeDuration("P7D") + xs:dayTimeDuration("PT3H")', 'P7DT3H', '');
  t('xs:date("2012-12-20") + xs:dayTimeDuration("P4D")', '2012-12-24', '');
  t('xs:date("2012-12-20") + xs:dayTimeDuration("P4DT20H")', '2012-12-24', '');
  t('(xs:date("2012-12-20") + xs:dayTimeDuration("P4DT20H")) + xs:dayTimeDuration("P4DT20H")', '2012-12-28', '');
  t('xs:date("2012-12-20") - xs:dayTimeDuration("P4DT20H")', '2012-12-15', '');
  t('xs:date("2012-12-24") - xs:date("2012-12-20")', 'P4D', '');
  t('timezone-from-dateTime(xs:date("2012-12-30+05:30") '+ IfThen(strictTypeChecking, 'cast as xs:dateTime', '')+')', 'PT5H30M', '');
  t('timezone-from-date(xs:date("2012-12-30Z"))', 'PT0S', '');
  t('timezone-from-time(xs:time("02:18:20-12:03"))', '-PT12H3M', '');
     {
  t('', '', '');
  t('', '', '');
               ,('', '', '')}
               //from the xpath standard
  t('xs:dayTimeDuration("PT2H10M") * 2.1', 'PT4H33M', '');
  t('xs:yearMonthDuration("P2Y11M") * 2.3', 'P6Y9M', '');
  t('xs:dayTimeDuration("P1DT2H30M10.5S") div 1.5', 'PT17H40M7S', '');
  t('xs:yearMonthDuration("P2Y11M") div 1.5', 'P1Y11M', '');
  t('xs:dayTimeDuration("P2DT53M11S") div xs:dayTimeDuration("PT1S")', '175991', '');
  t('fn:round-half-to-even( xs:dayTimeDuration("P2DT53M11S") div xs:dayTimeDuration("P1DT10H"), 4)', '1.4378', '');
  t('xs:yearMonthDuration("P3Y4M") div xs:yearMonthDuration("P1M")', '40', '');
  t('xs:yearMonthDuration("P3Y4M") div xs:yearMonthDuration("-P1Y4M")', '-2.5', '');
  t('xs:dayTimeDuration("PT2H10M") * 2.1', 'PT4H33M', '');
  t('xs:yearMonthDuration("P2Y11M") * 2.3', 'P6Y9M', '');
  t('xs:dayTimeDuration("P2DT12H5M") + xs:dayTimeDuration("P5DT12H")', 'P8DT5M', '');
  t('xs:date("2004-10-30Z") + xs:dayTimeDuration("P2DT2H30M0S")', '2004-11-01Z', '');
  t('xs:dateTime("2000-10-30T11:12:00") + xs:dayTimeDuration("P3DT1H15M")', '2000-11-02T12:27:00', '');
  t('xs:time("11:12:00") + xs:dayTimeDuration("P3DT1H15M")', '12:27:00', '');
  t('xs:time("23:12:00+03:00") + xs:dayTimeDuration("P1DT3H15M")', '02:27:00+03:00', '');
  t('xs:yearMonthDuration("P2Y11M") + xs:yearMonthDuration("P3Y3M")', 'P6Y2M', '');
  t('xs:date("2000-10-30") + xs:yearMonthDuration("P1Y2M")', '2001-12-30', '');
  t('xs:dateTime("2000-10-30T11:12:00") + xs:yearMonthDuration("P1Y2M")', '2001-12-30T11:12:00', '');
  t('xs:date("2000-10-30") - xs:dayTimeDuration("P3DT1H15M")', '2000-10-26', '');
  t('xs:dateTime("2000-10-30T11:12:00") - xs:dayTimeDuration("P3DT1H15M")', '2000-10-27T09:57:00', '');
  t('xs:time("11:12:00") - xs:dayTimeDuration("P3DT1H15M")', '09:57:00', '');
  t('xs:time("08:20:00-05:00") - xs:dayTimeDuration("P23DT10H10M")', '22:10:00-05:00', '');
  t('xs:date("2000-10-30") - xs:yearMonthDuration("P1Y2M")', '1999-08-30', '');
  if ps.StaticContext.namespaces = nil then ps.StaticContext.namespaces := TNamespaceList.create;
  ps.StaticContext.namespaces.add(XMLNamespace_MyExtensionOperators);
  t('xs:date("2000-02-29Z") - xs:yearMonthDuration("P1Y")', '1999-02-28Z', '');
  t('xs:date("2000-10-31-05:00") - xs:yearMonthDuration("P1Y1M")', '1999-09-30-05:00', '');
  t('xs:dateTime("2000-10-30T11:12:00") - xs:yearMonthDuration("P1Y2M")', '1999-08-30T11:12:00', '');
  t('xs:dayTimeDuration("P2DT12H") - xs:dayTimeDuration("P1DT10H30M")', 'P1DT1H30M', '');
  t('xs:yearMonthDuration("P2Y11M") - xs:yearMonthDuration("P3Y3M")', '-P4M', '');
  t('xs:date("2000-10-30") - xs:date("1999-11-28")', 'P337D', '');
  t('xs:dateTime("2000-10-30T06:12:00-05:00") - xs:dateTime("1999-11-28T09:00:00Z")', 'P337DT2H12M', '');
  t('xs:time("11:12:00Z") - xs:time("04:00:00-05:00")', 'PT2H12M', '');
  t('xs:time("11:00:00-05:00") - xs:time("21:30:00+05:30")', 'PT0S', '');
  t('xs:time("17:00:00-06:00") - xs:time("08:00:00+09:00")', 'P1D', '');
  t('xs:time("24:00:00") - xs:time("23:59:59")', '-PT23H59M59S', '');
  t('xs:time("24:00:00") - xs:time("23:59:59")', '-PT23H59M59S', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime("2002-03-07T10:00:00"))', '2002-03-07T10:00:00-05:00', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime(''2002-03-07T10:00:00-07:00''))', '2002-03-07T12:00:00-05:00', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime("2002-03-07T10:00:00"), xs:dayTimeDuration("-PT10H"))', '2002-03-07T10:00:00-10:00', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime("2002-03-07T10:00:00-07:00"), xs:dayTimeDuration("-PT10H"))', '2002-03-07T07:00:00-10:00', '');
  t('adjust-dateTime-to-timezone(xs:dateTime("2002-03-07T10:00:00-07:00"), xs:dayTimeDuration("PT10H"))', '2002-03-08T03:00:00+10:00', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime(''2002-03-07T00:00:00+01:00''), xs:dayTimeDuration("-PT8H"))', '2002-03-06T15:00:00-08:00', '');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime(''2002-03-07T10:00:00''), ())','2002-03-07T10:00:00','');
  t('fn:adjust-dateTime-to-timezone(xs:dateTime(''2002-03-07T10:00:00-07:00''), ())','2002-03-07T10:00:00','');
  t('hours-from-dateTime(fn:adjust-dateTime-to-timezone(xs:dateTime("2002-03-07T10:00:00-07:00"), xs:dayTimeDuration("-PT10H")))', '7', '');
  t('fn:local-name-from-QName(fn:QName("http://www.example.com/example", "pn:person"))', 'person', '');
  t('fn:prefix-from-QName(fn:QName("http://www.example.com/example", "pn:person"))', 'pn', '');
  t('fn:namespace-uri-from-QName(fn:QName("http://www.example.com/example", "pn:person"))', 'http://www.example.com/example', '');
  t('fn:local-name-from-QName(fn:QName("http://www.example.com/example", "person"))', 'person', '');
  t('fn:prefix-from-QName(fn:QName("http://www.example.com/example", "person"))', '', '');
  t('fn:namespace-uri-from-QName(fn:QName("http://www.example.com/example", "person"))', 'http://www.example.com/example', '');
  t('fn:local-name-from-QName(xs:QName("person"))', 'person', '');
  t('fn:namespace-uri-from-QName(xs:QName("person"))', '', '');
  t('//x', '2', '!<abc id="a" xml:lang="en-US">1<x>2</x>3</abc>');
  t('fn:root(//x)', '123', '');
  t('fn:root()', '123', '');
  t('outer-xml(root(//x))', '<abc id="a" xml:lang="en-US">1<x>2</x>3</abc>', '');
  t('node-name(root(//x))', '', '');
  t('fn:root()/abc/@id', 'a', '');
  t('fn:root(//x)/abc/@id', 'a', '');
  f('(7)[@a]', 'err:XPTY0020');
  t('lang("en")', 'true', '');
  t('lang("en-")', 'false', '');
  t('lang("en", //x)', 'true', '');
  t('lang("En", //x)', 'true', '');
  t('nilled(/abc/x)', 'true', '!<abc id="a" xml:lang="en-US">1<x xml:nil="true"></x><y xml:nil="true">as</y><z xml:nil="false"></z></abc>');
  t('nilled(/abc/y)', 'false', '');
  t('nilled(/abc/z)', 'false', '');
  t('data(/abc/y)', 'as', '');
  t('string-join(data(/abc/y),",")', 'as', '');
  t('type-of(data(/abc/y))', 'untypedAtomic', '');
  t('namespace-uri-from-QName(resolve-QName("t:abc", *:r/*:sub))', 'test', '<r xmlns="default" xmlns:t="test"><sub></sub><override xmlns:t="test2"><sub/></override></r>');
  t('namespace-uri-from-QName(resolve-QName("t:abc", *:r/*:override/*:sub))', 'test2', '');
  t('namespace-uri-from-QName(resolve-QName("abc", *:r/*:override/*:sub))', 'default', '');
  t('namespace-uri-from-QName(resolve-QName((), *:r/*:override/*:sub))', '', '');
  t('namespace-uri-for-prefix("t", *:r/*:override/*:sub)', 'test2', '');
  t('namespace-uri-for-prefix((), *:r/*:override/*:sub)', 'default', '');
  t('string-join(in-scope-prefixes(*:r/*:override/*:sub),",")', 'xml,,t', ''); //order does not matter
  t('trace(152, "a")', '152', '');
  t('resolve-uri("/def", "http://www.example.com/a/b/c")', 'http://www.example.com/def', '');
  t('resolve-uri("#frag", "http://www.example.com/a/b/c")', 'http://www.example.com/a/b/c#frag', '');
  t('resolve-uri("?param#frag", "http://www.example.com/a/b/c")', 'http://www.example.com/a/b/c?param#frag', '');
  t('resolve-uri("d?param#frag", "http://www.example.com/a/b/c")', 'http://www.example.com/a/b/d?param#frag', '');
  t('resolve-uri("./.././../", "http://www.example.com/a/b/c")', 'http://www.example.com/', '');
  t('resolve-uri("./.././../", "http://www.example.com/a/b/c/")', 'http://www.example.com/a/', '');
  t('resolve-uri("./.././../ghi", "http://www.example.com/a/b/c")', 'http://www.example.com/ghi', '');
  t('resolve-uri("./.././../ghi", "http://www.example.com/a/b/c/")', 'http://www.example.com/a/ghi', '');
  t('resolve-uri("ghi", "file:///home/example/.config/foobar")', 'file:///home/example/.config/ghi', '');
  t('resolve-uri("ghi", "file:///home/example/.config/foobar/")', 'file:///home/example/.config/foobar/ghi', '');
  t('resolve-uri("../ghi", "file:///home/example/.config/foobar")', 'file:///home/example/ghi', '');
  t('resolve-uri("../ghi", "file:///home/example/.config/foobar/")', 'file:///home/example/.config/ghi', '');
  t('resolve-uri("/tmp/abc", "file:///home/example/.config/foobar")', 'file:///tmp/abc', '');
  t('fn:encode-for-uri("http://www.example.com/00/Weather/CA/Los%20Angeles#ocean")', 'http%3A%2F%2Fwww.example.com%2F00%2FWeather%2FCA%2FLos%2520Angeles%23ocean', '');
  t('concat("http://www.example.com/", encode-for-uri("~bébé"))', 'http://www.example.com/~b%C3%A9b%C3%A9', '');
  t('concat("http://www.example.com/", encode-for-uri("100% organic"))', 'http://www.example.com/100%25%20organic', '');
  t('fn:iri-to-uri ("http://www.example.com/00/Weather/CA/Los%20Angeles#ocean")', 'http://www.example.com/00/Weather/CA/Los%20Angeles#ocean', '');
  t('fn:iri-to-uri ("http://www.example.com/~bébé")', 'http://www.example.com/~b%C3%A9b%C3%A9', '');
  t('fn:escape-html-uri ("http://www.example.com/00/Weather/CA/Los Angeles#ocean")', 'http://www.example.com/00/Weather/CA/Los Angeles#ocean', '');
  t('fn:escape-html-uri ("javascript:if (navigator.browserLanguage == ''fr'') window.open(''http://www.example.com/~bébé'');")', 'javascript:if (navigator.browserLanguage == ''fr'') window.open(''http://www.example.com/~b%C3%A9b%C3%A9'');', '');
  t('base-uri(doc/paragraph/link)', 'http://example.org/today/xy', '<doc xmlns:xlink="http://www.w3.org/1999/xlink" xml:base="http://example.org/today/"><paragraph xml:base="xy"><link xlink:type="simple" xlink:href="new.xml"></link>!</paragraph></doc>');
  t('name(./tmp/*)', 'pr:abc', '<tmp xmlns:pr="http://www.example.com"><pr:abc></pr:abc></tmp>');
  t('name(./tmp/element())', 'pr:abc', '');
  t('local-name(./tmp/element())', 'abc', '');
  t('namespace-uri(./tmp/element())', 'http://www.example.com', '');
  t('type-of(temp := xs:byte(12))', 'byte', '');
  t('$temp', '12', '');
  t('type-of($temp)', 'byte', '');

  t('uri-decode(uri-encode("! # $ % & '' ( ) * + , / : ; = ? @ [ ]"))', '! # $ % & '' ( ) * + , / : ; = ? @ [ ]');
  t('uri-decode("%20%41+")', ' A ');
  t('uri-decode(">%4d<")', '>M<');
  t('uri-decode("%4D")', 'M');
  f('uri-decode("%")', 'pxp:uri');
  f('uri-decode("%A")', 'pxp:uri'); //invalid input, ignore it
  f('uri-decode("%XY")', 'pxp:uri');
  t('uri-encode(" A ")', '%20A%20');

  t('uri-combine("a=1&b=2", "a=17&c=13")', 'a=17&b=2&c=13');
  t('uri-combine("a=1&b=2", "")', 'a=1&b=2');
  t('uri-combine("a=1&b=2", ())', 'a=1&b=2');
  t('uri-combine("a=1&b=2", {"a": "-"})', 'a=-&b=2');
  t('uri-combine("", "a=17&c=13")', 'a=17&c=13');
  t('uri-combine((), "a=17&c=13")', 'a=17&c=13');
  t('uri-combine(({"x": "++"}, "a=100"), "c=13")', 'x=%2B%2B&a=100&c=13');
  t('uri-combine({"x": "++", "y&": 123}, {"x": 0, "y": 456})', 'x=0&y%26=123&y=456');
  t('uri-combine({"x": "++", "y&": 123}, {"y": "456", "x": 0})', 'x=0&y%26=123&y=456');

  t('uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", "a=1")', 'a=1&b=v&b=x&b=z');
  t('uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", "a=1&a=2")', 'a=1&b=v&a=2&b=x&b=z');
  t('uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", "a=1&a=2&a=3")', 'a=1&b=v&a=2&b=x&a=3&b=z');
  t('uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", "a=1&a=2&a=3&a=4")', 'a=1&b=v&a=2&b=x&a=3&b=z&a=4');
  t('join(for $i in 0 to 5 return uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", {"a": 1 to $i}))', 'b=v&b=x&b=z a=1&b=v&b=x&b=z a=1&b=v&a=2&b=x&b=z a=1&b=v&a=2&b=x&a=3&b=z a=1&b=v&a=2&b=x&a=3&b=z&a=4 a=1&b=v&a=2&b=x&a=3&b=z&a=4&a=5');
  t('join(for $i in 0 to 5 return uri-combine("a=u&b=v&a=w&b=x&a=y&b=z", {"a": 1 to 2, "b": 1 to $i}))', 'a=1&a=2 a=1&b=1&a=2 a=1&b=1&a=2&b=2 a=1&b=1&a=2&b=2&b=3 a=1&b=1&a=2&b=2&b=3&b=4 a=1&b=1&a=2&b=2&b=3&b=4&b=5');

  t('binary-to-string(xs:hexBinary("1234567890ABCDEF"))',#$12#$34#$56#$78#$90#$AB#$CD#$EF);
  t('binary-to-string(xs:hexBinary("C3A4"))','ä');
  t('binary-to-string(xs:hexBinary("C3A4"), "latin1")','Ã¤');
  t('binary-to-string(xs:base64Binary("aGFsbG8gd2VsdA=="))','hallo welt');
  t('string-to-hexBinary("abc")', '616263');
  t('string-to-hexBinary("abcä")', '616263C3A4');
  t('string-to-hexBinary("abcä", "latin1")', '616263E4');
  t('string-to-base64Binary("abc")', 'YWJj');
  t('string-to-base64Binary("abcä")', 'YWJjw6Q=');
  t('string-to-base64Binary("abcä", "latin1")', 'YWJj5A==');
  t('string-to-codepoints(binary-to-string(xs:hexBinary("4904"), "UTF-16LE"))', '1097');
  t('string-to-codepoints(binary-to-string(xs:hexBinary("49040000"), "UTF-32LE"))', '1097');

  ps.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;

               //Objects extension
  t('(obj := object())[0]', '', '');
  t('type-of($obj)', 'object()');
  t('obj.foo := "bar"', 'bar', '');
  t('$obj.foo', 'bar', '');
  t('($obj).foo', 'bar', '');
  t('$obj("foo")', 'bar', '');
  t('$obj("foo") := "xyz"', 'xyz', '');
  t('$obj.foo', 'xyz', '');
//  t('($obj) . foo', 'bar', '');
  t('obj.foo := 123', '123', '');
  t('obj.foo := $obj.foo * 2', '246', '');
  t('(obj.o := $obj)[0]', '', '');
  t('$obj.o.foo', '246', '');
  t('$obj.bar := 17', '17', '');
  t('$obj.o.bar', '', '');
  t('obj.o.foo := "new"', 'new', '');
  t('$obj.o.foo', 'new', '');
  t('$obj.foo', '246', '');
  t('$obj.bar', '17', '');
  t('(obj.o.p := $obj)[0]', '', '');
  t('$obj.o.p.foo', '246', '');
  t('$obj.o.p.o.foo', 'new', '');
  t('(obj.o.p.o := object())[4]', '', '');
  t('$obj.o.p.o.foo', '', '');
  t('obj.o.p.o.foo := 99', '99', '');
  t('$obj.o.p.o.foo', '99', '');
  t('(test := object())[112]', '', '');
  t('test.t1 := 1', '1', '');
  t('test.t2 := 2', '2', '');
  t('test.t3 := 3', '3', '');
  t('test.t4 := 4', '4', '');
  t('$test.t1', '1', '');
  t('$test.t2', '2', '');
  t('$test.t3', '3', '');
  t('$test.t4', '4', '');
  {t('test.foo:bar := 123', '123', '');
  t('$test.foo:bar', '123', '');
  t('$test.foo:bar := 456', '456', ''); //good idea to allow both??
  t('$test.foo:bar', '456', '');}
  t('(obj := object(("a", "b", "c", 123)))[-1]', '', '');
  t('$obj.a', 'b', '');
  t('$obj.b', '', '');
  t('$obj.c', '123', '');
  t('"a$obj.c;b"', 'a$obj.c;b', '');
  t('''a$obj.c;b''', 'a$obj.c;b', '');
  t('x"a{$obj.c}b"', 'a123b', '');
  t('type-of($obj.c)', 'integer', '');
  t('serialize-json((object(("x", "y")), object(("u", "v"))))', '[{"x": "y"}, {"u": "v"}]');
  t('(object(("x", "y")), object(("u", "v")))[1].x', 'y', '');
  t('(object(("x", "y")), object(("u", "v")))[1].u', '', '');
  t('(object(("x", "y")), object(("u", "v")))[2].u', 'v', '');

  t('(obj := {"b": {"c": {"d": 1}}})[false()]', '');
  t('serialize-json($obj)', '{"b": {"c": {"d": 1}}}');
  t('$obj.b.c.d', '1');
  t('(x := $obj)[0]', '');
  t('serialize-json($x)', '{"b": {"c": {"d": 1}}}');
  t('$x.b.c.d := 2', '2');
  t('$x.b.c.d', '2');
  t('$obj.b.c.d', '1');
  t('serialize-json($obj)', '{"b": {"c": {"d": 1}}}');
  t('serialize-json($x)', '{"b": {"c": {"d": 2}}}');
  t('x.b.c.d := 3', '3');
  t('serialize-json($x)', '{"b": {"c": {"d": 3}}}');
  t('$x.b.c.d', '3');
  t('$obj.b.c.d', '1');
  t('$x.b("c").d := 4', '4');
  t('$x.b.c.d', '4');
  t('serialize-json($x)', '{"b": {"c": {"d": 4}}}');
  t('$x("b")("c")("d") := 5', '5');
  t('$x.b.c.d', '5');
  t('serialize-json($x)', '{"b": {"c": {"d": 5}}}');
  t('$x("b")("c").d := 6', '6');
  t('$x.b.c.d', '6');
  t('serialize-json($x)', '{"b": {"c": {"d": 6}}}');
  t('($x("b").c := {"y": 123})[0]', '');
  t('$x.b.c.d', '');
  t('$x.b.c.y', '123');
  t('serialize-json($x)', '{"b": {"c": {"y": 123}}}');




  t('string-join(for $i in object(("a", "x", "b", "Y", "c", "Z")).a return $i, "|")', 'x', '');
  t('string-join(for $i in object(("a", "x", "b", "Y", "c", "Z")) return $i.a, "|")', 'x', '');
  t('string-join(for $i in object(("a", "x", "b", "Y", "c", "Z")) return ($i.b,$i.c), "|")', 'Y|Z', '');
  t('string-join(for $i in (object(("abc", "123")), object(("abc", "456")), object(("abc", "789"))) return $i.abc, "|")', '123|456|789', '');
  t('string-join(for $i in (object(("abc", "123")), object(("abc", "456")), object(("abc", "789"))) return x"{$i.abc}", "|")', '123|456|789', '');
  t('join((i := object(("a", 1))[0], for $i in object(("a", "2")) return $i.a), "|")', '2', '');
  t('join((i := object(("a", 1))[0], for $i in object(("b", "2")) return $i.a), "|")', '', '');
  t('join((i := object(("a", 1))[0], for $i in (object(("b", "2")),object(("a", "3"))) return $i.a), "|")', '3', '');

  //New object syntax
  t('object(("hallo", 123)).hallo', '123'); //old
  t('({"hallo": 123}).hallo', '123');
  t('({"hallo": 123, "foobar": 456, "xyz": 789}).foobar', '456');
  t('({"nest": {"ing": "birdy"}}).nest.ing', 'birdy');
  t('join(({"array": (1, 2, 3)}).array, " ")', '1 2 3');
  t('{"hallo": 123}.hallo', '123');
  t('{}.hallo', ''); //no exception on undefined properties
  t('join({"array": (1, 2, 3)}.array, " ")', '1 2 3');
  t('join(jn:members({"array": [1, 2, 3]}.array), " ")', '1 2 3');
  t('join(({"array": [1, 2, 3]}.array)(), " ")', '1 2 3');
  t('join({"array": [1, 2, 3]}.array(), " ")', '1 2 3');
  t('join({"array": (1)}.array, " ")', '1');
  t('join(jn:members({"array": [1]}.array), " ")', '1');
  t('join(({"array": [1]}.array)(), " ")', '1');
  t('join({"array": ()}.array, " ")', '');
  t('join(jn:members({"array": []}.array), " ")', '');
  t('join(({"array": []}.array)(), " ")', '');
  t('{"array": ({"a": 10}, {"a": 20}, {"a": 30})}.array[1].a', '10');
  t('{"array": ({"a": 10}, {"a": 20}, {"a": 30})}.array[2].a', '20');
  t('{"array": ({"a": 10}, {"a": 20}, {"a": 30})}.array[3].a', '30');
  t('{} instance of object()', 'true');
  t('{"a": 123, "b": 16} instance of object()', 'true');
  t('{} instance of array()', 'false');
  t('[] instance of object()', 'false');
  t('[] instance of object()', 'false');
  t('[] instance of array()', 'true');
  t('[123] instance of array()', 'true');

  t('{} instance of json-item()', 'true');
  t('[123] instance of json-item()', 'true');
  t('123 instance of json-item()', 'false');
  t('(/) instance of json-item()', 'false');

  t('{} instance of structured-item()', 'true');
  t('[123] instance of structured-item()', 'true');
  t('123 instance of structured-item()', 'false');
  t('(/) instance of structured-item()', 'true');

  t('indirect := "foo"', 'foo');
  t('$x($indirect) := "bar"', 'bar');
  t('$x($indirect)', 'bar');
  t('$x("foo")', 'bar');
  t('$x.foo', 'bar');
  t('$x(1+1+1) := 6', '6');
  t('$x.3', '6');

  //. is an operator
  t('$x .foo', 'bar');
  t('$x .$indirect', 'bar');
  t('$x . "foo"', 'bar');
  t('$x . (concat("fo", "o"))', 'bar');
  t('$x . concat("fo", "o")', 'bar'); //is this really allowed?
  t('$x . $indirect', 'bar');

  t('$x."foo"', 'bar');
  t('$x.(concat("fo", "o"))', 'bar');
  t('$x.$indirect', 'bar');

  t('$x. "foo"', 'bar');
  t('$x. (concat("fo", "o"))', 'bar');
  t('$x. concat("fo", "o")', 'bar'); //is this really allowed?
  t('$x. $indirect', 'bar');

  t('$x.omg := "strange"', 'strange', '<foo>omg</foo>');
  t('$x.foo', 'bar');
  t('$x. (foo)', 'strange'); //beware! the strangeness of dot space
  t('$x . (foo)', 'strange');
  f('$x. foo', 'pxp:XPST0003');
  f('$x . foo', 'pxp:XPST0003');
  t('$x .foo', 'bar');

  t('serialize-json({$indirect: $x.$indirect})', '{"foo": "bar"}');

  t('($x).foo := "mouse"', 'mouse');
  t('($x).foo', 'mouse');
  t('($x)("foo") := "mice"', 'mice');
  t('($x)("foo")', 'mice');
  t('($x)(("foo")) := "more mice"', 'more mice');
  t('($x)(("foo"))', 'more mice');

  // []:= operator
  t('($x).foo []:= "tap"', 'tap');
  t('join(($x).foo)', 'more mice tap');
  t('($x).foo [] := "top"', 'top');
  t('join(($x).foo)', 'more mice tap top');
  t('$optest [] := 1', '1');
  t('join($optest)', '1');
  t('$optest [] := 2', '2');
  t('join($optest)', '1 2');
  t('$optest [] := 3', '3');
  t('join($optest)', '1 2 3');
  f('optest [] := 3', 'pxp:VAR'); //do not allow relaxed var name
  t('$optest[1] := 17', '17');
  t('join($optest)', '17 2 3');
  t('$optest[2] := 18', '18');
  t('join($optest)', '17 18 3');
  t('$optest[6] := 222', '222');
  t('join($optest)', '17 18 3 222');
  t('$optest[3] := ()', '');
  t('join($optest)', '17 18 222');
  t('$optest[2] := (18, 16)', '1816');
  t('join($optest)', '17 18 16 222');
  t('$optest[1] := (19, 17)', '1917');
  t('join($optest)', '19 17 18 16 222');
  t('$optest[1][] := 18.5', '18.5');
  t('join($optest)', '19 18.5 17 18 16 222');
  t('$optest[0] := "prepend"', 'prepend');
  t('join($optest)', 'prepend 19 18.5 17 18 16 222');
  f('$optest[-1] := "prepend"', 'pxp:VAR');

  t('$optest2[1] := 0', '0');
  t('join($optest2)', '0');
  t('$optest2[1] := 3', '3');
  t('join($optest2)', '3');
  t('$optest2[0][] := 2', '2');
  t('join($optest2)', '2 3');

  //sequences in properties
  t('serialize-json($objwithseq := {"a": (1,2)})', '{"a": [1, 2]}');
  t('join(( $objwithseq, {"a": 3} ).a)', '1 2 3');
  t('join(( $objwithseq, {"a": 3} )("a"))', '1 2 3');
  t('join(( $objwithseq, {"a": 3} )/a)', '1 2 3');
  t('serialize-json($objwithseq)', '{"a": [1, 2]}');
  t('join($props := $objwithseq("a"))', '1 2');
  t('join($props[10] := 17)', '17');
  t('join($props)', '1 2 17');
  t('join($props := $objwithseq("a"))', '1 2');
  t('join($props[] := 17)', '17');
  t('join($props)', '1 2 17');
  t('serialize-json($objwithseq)', '{"a": [1, 2]}');

  //():= operator for json arrays + objects
  t('serialize-json($artest := [])', '[]');
  t('serialize-json($artest(1) := 100)', '100');
  t('serialize-json($artest)', '[100]');
  t('$artest(1) := 111', '111');
  t('serialize-json($artest)', '[111]');
  t('$artest(10) := 22', '22');
  t('serialize-json($artest)', '[111, 22]');
  t('$artest(10) := 3', '3');
  t('serialize-json($artest)', '[111, 22, 3]');
  t('serialize-json(($artest(2) := 8, $artest))', '[8, [111, 8, 3]]');
  t('serialize-json(($artest(2) := (), $artest))', '[111, 3]');
  t('serialize-json(($artest(1)[] := 123, $artest))', '[123, [111, 123, 3]]');
  t('serialize-json(($artest(1) := (), $artest))', '[123, 3]');
  t('serialize-json(($artest(0) := -1, $artest))', '[-1, [-1, 123, 3]]');

  t('serialize-json($artest := {"a": []})', '{"a": []}');
  t('serialize-json(($artest("a")(2) := 1, $artest))', '[1, {"a": [1]}]');
  t('serialize-json(($artest("a")(2) := 22, $artest))', '[22, {"a": [1, 22]}]');
  t('serialize-json(($artest("a")(3) := 333, $artest))', '[333, {"a": [1, 22, 333]}]');
  t('serialize-json(($artest("a")(1) := -1, $artest))', '[-1, {"a": [-1, 22, 333]}]');
  t('serialize-json(($artest("a")(2)[] := 100, $artest))', '[100, {"a": [-1, 22, 100, 333]}]');
  t('serialize-json((($artest).a(1) := (), $artest))', '{"a": [22, 100, 333]}');
  t('serialize-json(($artest("a")(2) := (), $artest))', '{"a": [22, 333]}');
  t('serialize-json(($artest("a")(0) := -2, $artest))', '[-2, {"a": [-2, 22, 333]}]');
  t('serialize-json(($artest("a")(1)[0] := -10, $artest))', '[-10, {"a": [-10, -2, 22, 333]}]');
  t('serialize-json(($artest("a")(3)[0] := 11, $artest))', '[11, {"a": [-10, -2, 11, 22, 333]}]');

  t('serialize-json($artest := {"a": [{"b": "1"}, {"c": "2"}]})', '{"a": [{"b": "1"}, {"c": "2"}]}');
  t('serialize-json((($artest).a(2).c := 222, $artest))', '[222, {"a": [{"b": "1"}, {"c": 222}]}]');
  t('serialize-json((($artest).a(2).c[] := 3, $artest))', '[3, {"a": [{"b": "1"}, {"c": [222, 3]}]}]');
  f('($artest).a(2).c(1) := (1, 2)', 'pxp:OBJECT'); //assignment does not work, because it is a sequence, not an array
  t('serialize-json((($artest).a(2).c[1] := (1, 2), $artest))', '[1, 2, {"a": [{"b": "1"}, {"c": [1, 2, 3]}]}]');
  t('serialize-json((($artest).a(2).c[1][] := 1.5, $artest))', '[1.5, {"a": [{"b": "1"}, {"c": [1, 1.5, 2, 3]}]}]');

  t('{"foo": 123}.foo', '123');
  t('{"foo": 123}.$indirect', '123');
  t('{"foo": 123}. "foo"', '123');
  t('{"foo": 123}. (concat("foo", ""))', '123');
  t('{"foo": 123}. $indirect', '123');

  t('{"foo": 123} .foo', '123');
  t('{"foo": 123} . "foo"', '123');
  t('{"foo": 123} . (concat("foo", ""))', '123');
  t('{"foo": 123} .$indirect', '123');
  t('{"foo": 123} . $indirect', '123');

  t('{"foo": {"bar": 456}}.foo.bar', '456');
  t('{"foo": {"bar": 456}}."foo"."bar"', '456');
  t('{"foo": {"bar": 456}}."foo". "bar"', '456');
  t('{"foo": {"bar": 456}}."foo" ."bar"', '456');
  t('{"foo": {"bar": 456}}."foo" . "bar"', '456');
  t('{"foo": {"bar": 456}}. "foo"."bar"', '456');
  t('{"foo": {"bar": 456}}. "foo". "bar"', '456');
  t('{"foo": {"bar": 456}}. "foo" ."bar"', '456');
  t('{"foo": {"bar": 456}}. "foo" . "bar"', '456');
  t('{"foo": {"bar": 456}} ."foo"."bar"', '456');
  t('{"foo": {"bar": 456}} ."foo". "bar"', '456');
  t('{"foo": {"bar": 456}} ."foo" ."bar"', '456');
  t('{"foo": {"bar": 456}} ."foo" . "bar"', '456');
  t('{"foo": {"bar": 456}} . "foo"."bar"', '456');
  t('{"foo": {"bar": 456}} . "foo". "bar"', '456');
  t('{"foo": {"bar": 456}} . "foo" ."bar"', '456');
  t('{"foo": {"bar": 456}} . "foo" . "bar"', '456');

  t('{"foo": {"bar": 456}}.foo.bar + 1', '457');
  t('{"foo": {"bar": 456}}."foo"."bar" + 1', '457');
  t('{"foo": {"bar": 456}}."foo". "bar" + 1', '457');
  t('{"foo": {"bar": 456}}."foo" ."bar" + 1', '457');
  t('{"foo": {"bar": 456}}."foo" . "bar" + 1', '457');
  t('{"foo": {"bar": 456}}. "foo"."bar" + 1', '457');
  t('{"foo": {"bar": 456}}. "foo". "bar" + 1', '457');
  t('{"foo": {"bar": 456}}. "foo" ."bar" + 1', '457');
  t('{"foo": {"bar": 456}}. "foo" . "bar" + 1', '457');
  t('{"foo": {"bar": 456}} ."foo"."bar" + 1', '457');
  t('{"foo": {"bar": 456}} ."foo". "bar" + 1', '457');
  t('{"foo": {"bar": 456}} ."foo" ."bar" + 1', '457');
  t('{"foo": {"bar": 456}} ."foo" . "bar" + 1', '457');
  t('{"foo": {"bar": 456}} . "foo"."bar" + 1', '457');
  t('{"foo": {"bar": 456}} . "foo". "bar" + 1', '457');
  t('{"foo": {"bar": 456}} . "foo" ."bar" + 1', '457');
  t('{"foo": {"bar": 456}} . "foo" . "bar" + 1', '457');

  t('1000 + {"foo": {"bar": 456}}.foo.bar + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}."foo"."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}."foo". "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}."foo" ."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}."foo" . "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}. "foo"."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}. "foo". "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}. "foo" ."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}}. "foo" . "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} ."foo"."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} ."foo". "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} ."foo" ."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} ."foo" . "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} . "foo"."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} . "foo". "bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} . "foo" ."bar" + 1', '1457');
  t('1000 + {"foo": {"bar": 456}} . "foo" . "bar" + 1', '1457');

  //f('{} <= 123e0', 'XPTY0004');
  //f('{} lt 123e0', 'XPTY0004');

  //multiple properties
  t('join(({ "foo" : "bar" }, { "foo" : "bar2" }, { "bar" : "foo" })("foo"))', 'bar bar2'); //test based on jsoniq standard
  t('join(({ "foo" : "bar" }, { "foo" : "bar2" }, { "bar" : "foo" })."foo")', 'bar bar2'); //test based on jsoniq standard
  t('for $f in "foo" return join(({ "foo" : "bar" }, { "foo" : "bar2" }, { "bar" : "foo" }).$f)', 'bar bar2'); //test based on jsoniq standard
  t('join(({ "foo" : "bar" }, { "foo" : "bar2" }, { "bar" : "foo" }).foo)', 'bar bar2'); //test from jsoniq standard

//  t('join(({ "foo" : "bar" }, [ "foo" , "bar2" ], { "bar" : "foo" })("foo"))', 'bar'); //test based on jsoniq standard
//  t('join(({ "foo" : "bar" }, [ "foo" , "bar2" ], { "bar" : "foo" })."foo")', 'bar'); //test based on jsoniq standard
//  t('for $f in "foo" return join(({ "foo" : "bar" }, [ "foo" , "bar2" ], { "bar" : "foo" }).$f)', 'bar'); //test based on jsoniq standard
//  t('join(({ "foo" : "bar" }, [ "foo" , "bar2" ], { "bar" : "foo" }).foo)', 'bar'); //test from jsoniq standard

  //t('(17).abc', '');

  //  t('$x. bar', 'bar');
//  t('$x.$indirect', 'bar');

  t('join(jn:keys({"a": 1, "b": 2, "c": 3}))', 'a b c');
  t('join(({"a": 1, "b": 2, "c": 3})())', 'a b c');
  t('join({"a": 1, "b": 2, "c": 3}())', 'a b c');
  t('join({"a": 1}())', 'a');
  t('join({}())', '');

  t('join(([1,2,3], {"a": 17}, {}, [], {"c": 7, "d": 1})())', '1 2 3 a c d');
  t('join(({}, [], {}, [1,2,3], {"a": 17}, {}, [], {"c": 7, "d": 1})())', '1 2 3 a c d');
  t('(xyz := {"mu": "mi"})[0]', '');
  t('$xyz.abc := 17', '17');
  t('join($xyz())', 'mu abc');

  //t('join(jn:keys(({"a": 1}, {"b": 2}, {"c": 3})))', 'a b c');

  ps.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation;
  t('$a := 123', '123');
  t('$a.b.c := 456', '456');
  t('$a', '123');
  t('$a.b.c', '456');
  f('$a.b', 'err:XPST0008');
  t('($a := {"b": 17})[false()]', '');
  f('$a.b', 'err:XPST0008');
  f('$a. b', 'err:XPST0003');
  t('$a .b', '17');
  t('$a("b")', '17');
  t('$a."b"', '17');
  t('$a.''b''', '17');
  f('$a.("b")', 'err:XPST0008');
  t('$te.mp := "b"', 'b');
  t('$a.$te.mp', '17');


  ps.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;
  t('$a.b', '17');
  t('$a.b.c', '');

  t('{"a.b.c": "miza"}("a.b.c")', 'miza');
  t('{"a.b.c": "miza"}.a.b.c', '');

  t('($obj := {})[0]', '');
  t('$obj("x.y.z") := 17', '17');
  t('$obj("x.y.z")', '17');
  t('serialize-json($obj)', '{"x.y.z": 17}');

  t('$a / b', '17');
  t('$a // b', '17');
  t('[$a] / b', '17');
  t('[$a] // b', '17');
  t('join(({}/(a:=123),":", $a))', '123 : 123');
  t('(($seq := ({"a": 1, "b": 2, "c": 3}, {"b": 4, "c": 5, "d": 6, "e": [{"a": 10, "b": 11}], "f": {"a": 20, "b": 21}}))[2])[0]', '');
  t('join($seq / a)', '1');
  t('join($seq / b)', '2 4');
  t('join([$seq] / b)', '2 4');
  t('join($seq / (b, c, d))', '2 3 4 5 6');
  t('join($seq // b)', '2 4 11 21');
  f('join($seq // (a, b) )','pxp:JSON'); //todo: , '1 2 4 10 11 20 21'); ?
  t('join($seq / (.//a, .//b) )', '1 2 10 20 4 11 21');
  t('join( ( $seq // a, $seq // b) )', '1 10 20 2 4 11 21');
  t('join($seq / . / a )', '1');
  t('join($seq / . / b )', '2 4');
  t('join($seq / . / f / a )', '20');
  t('join($seq / . / f / . / b )', '21');
  f('join($seq / . / e () / a )', 'err:XPST0017'); //it thinks "e()" is a function
  t('join($seq / . / e / .() / a )', '10');
  t('join($seq / . / e / a )', '10');
  t('join($seq[1] / *)', '1 2 3');
  f('join($seq[1] / @*)', 'err:XPTY0019');
  f('join($seq[1] / attribute::*)', 'err:XPTY0019');
  t('serialize-json($seq[2] / *)', '[4, 5, 6, [{"a": 10, "b": 11}], {"a": 20, "b": 21}]');
  t('serialize-json($seq / *)', '[1, 2, 3, 4, 5, 6, [{"a": 10, "b": 11}], {"a": 20, "b": 21}]');

  t('join($seq[1] // *)', '1 2 3');
  t('serialize-json($seq // *)', '[1, 2, 3, 4, 5, 6, [{"a": 10, "b": 11}], {"a": 20, "b": 21}, 10, 11, 20, 21]');

  t('join([{"a": 1}, {"a": 2}, {"a": 3}] / a)', '1 2 3');
  f('join([{"a": 1}, {"a": 2}, {"a": 3}, 80, 90] / a)', 'pxp:JSON');
  f('join([{"a": 1}, {"a": 2}, {"a": 3}, [{"a": 4}] ] / a)', 'pxp:JSON');
  t('join([{"a": 1}, {"a": 2}, {"a": 3}, 80, 90] // a)', '1 2 3');

  t('(obj := {"a": 123}).a', '123');
  t('join($obj / a)', '123');
  t('$obj.a := 456', '456');
  t('join($obj / a)', '456');
  t('($obj.a := {"b": 7})[0]', '');
  t('serialize-json($obj / a)', '{"b": 7}');
  t('serialize-json($obj // b)', '7');
  t('$obj.a := 8', '8');
  t('serialize-json($obj / a)', '8');
  t('serialize-json($obj // b)', 'null');

  //Json tests
  t('json(''{"a": 123}'').a', '123');
  t('json(''"123"'')', '123');
  t('json(''12.7'')', '12.7');
  t('json(''2000000000'')', '2000000000');
  t('json(''2000000000000000'')', '2000000000000000');
  t('json(''true'')', 'true');
  t('jn:members(json(''[1, 2, 3]''))', '123');
  t('join(jn:members(json(''[1, 2, 3]'')), " ")', '1 2 3');
  t('join(jn:members(json(''[1, 2, 3, [4, 5, 6], [7] ]'')), " ")', '1 2 3 4 5 6 7'); //this should raise an error in JSONiq, but correct in 3.1
  t('join(jn:members(([1, 2, 3], {"a": 17}, [4,5,6], 7889, "hallo")))', '1 2 3 4 5 6');
  t('join(jn:keys(([1, 2, 3], {"a": 17}, [4,5,6], 7889, "hallo")))', 'a');
  t('join(jn:keys(([1, 2, 3], {"a": 17}, [4,5,6], {"a": 7, "b": 8, "c": 89}, "hallo", {"b": []})))', 'a b c');
  t('join(jn:keys(([1, 2, 3], {"a": 17}, [4,5,6], {"a": 7, "b": 8, "c": 89}, "hallo", {"b": []})))', 'a b c');

  for tempb := false to true do begin
    ps.StaticContext.strictTypeChecking:=tempb;
    t('count(json(''[1, 2, 3, [4, 5, 6], [7] ]''))', '1'); //todo: fix?
    t('json(''[{"hallo": "world"}]'')(1).hallo', 'world');
    t('json(''[{"hallo": "world"}, {hallo: 1000}]'')(2).hallo', '1000');
    t('json(''{"hallo": "world"} {hallo: 1000}'')[1].hallo', 'world');
    t('json(''{"hallo": "world"} {hallo: 1000}'')[2].hallo', '1000');
    f('jn:parse-json(''{"hallo": "world"} {hallo: 1000}'')[1].hallo', 'jerr:JNDY0021');
    t('jn:parse-json(''{"hallo": "world"} {hallo: 1000}'', {"liberal": true()})[1].hallo', 'world');
    t('jn:parse-json(''{"hallo": "world"} {hallo: 1000}'', {"liberal": true(), "jsoniq-multiple-top-level-items": ()})[1].hallo', 'world');
    t('jn:parse-json(''{"hallo": "world"} {hallo: 1000}'', {"liberal": true(), "jsoniq-multiple-top-level-items": true()})[1].hallo', 'world');
    f('jn:parse-json(''{"hallo": "world"} {hallo: 1000}'', {"liberal": true(), "jsoniq-multiple-top-level-items": false()})[1].hallo', 'jerr:JNDY0021');
    t('jn:parse-json(())', '');

    t('[4,5,6](0)', '');
    t('[4,5,6](1)', '4');
    t('[4,5,6](2)', '5');
    t('[4,5,6](3)', '6');
    t('[4,5,6](4)', '');
    t('[4](0)', '');
    t('[4](1)', '4');
    t('[4](2)', '');
    t('[](0)', '');
    t('[](1)', '');

    t('{"foobar": 123, "maus": 456}("foobar")', '123');
    t('{"foobar": 123, "maus": 456}("maus")', '456');
    t('{"foobar": 123, "maus": 456}("unkn")', '');
    t('{"foobar": 123, "argh": [9,8,7]}("argh")(1)', '9');
    t('{"foobar": 123, "argh": [9,8,7]}("argh")(2)', '8');

    t('[{"foobar": 123, "maus": 456}](1)("foobar")', '123');

    //t('string-join((["a"], {"1": "o"}, ["x", "y", "z"])(1), " ")', 'a o x');
    //t('string-join((["a"], {"1": "o"}, ["x", "y", "z"])(2), " ")', 'y');

    t('serialize-json(123)', '123');
    t('serialize-json(123.6)', '123.6');
    t('serialize-json("123a")', '"123a"');
    t('serialize-json((1,2,3))', '[1, 2, 3]');
    t('serialize-json((1,2,"3!", true(),false()))', '[1, 2, "3!", true, false]');
    t('serialize-json({"foo": 123, "bar": 0.456})', '{"foo": 123, "bar": 0.456}');
    t('serialize-json({"xml": /})', '{"xml": "<foobar>123</foobar>"}', '<foobar>123</foobar>');
    t('serialize-json({1+2+3: 4+5+6, 2+1*3: 7})', '{"6": 15, "5": 7}');

    t('jn:size(())', '');
    t('jn:size([])', '0');
    t('jn:size([1])', '1');
    t('jn:size([1,2])', '2');
    t('jn:size([1 to 10])', '10');

    t('jn:is-null(jn:null())', 'true');
    t('jn:is-null(127)', 'false');
    t('jn:is-null(json("{''test'': null}").test)', 'true');

    t('serialize-json(jn:object(()))', '{}');
    t('serialize-json(jn:object({"a": 123}))', '{"a": 123}');
    t('serialize-json(jn:object({"a": 123, "b": 456}))', '{"a": 123, "b": 456}');
    t('serialize-json(jn:object(({"a": 123}, {"b": 456})))', '{"a": 123, "b": 456}');
    t('serialize-json(jn:object(({"z": -1}, jn:object(({"a": 123}, {"b": 456})), {"c": 17})))', '{"z": -1, "a": 123, "b": 456, "c": 17}');
    t('serialize-json(jn:object({"a": jn:null()}))', '{"a": null}');
    t('serialize-json(jn:object({"a": ()}))', '{"a": null}');
    t('serialize-json(jn:object({"a": (1,2,3)[0]}))', '{"a": null}');
    t('serialize-json(jn:object({"a": []}))', '{"a": []}');

    t('jn:parse-json(''{"a": 123}'').a instance of xs:integer', 'true');
    t('jn:parse-json(''{"a": 123.0}'').a instance of xs:decimal', 'true');
    t('jn:parse-json(''{"a": 123E1}'').a instance of xs:double', 'true');
    t('jn:parse-json(''{"a": 123E1}'').a instance of xs:decimal', 'false');
    t('serialize-json(xs:long(2))', '2');
    //t('serialize-json(xs:decimal(2))', '2.0');
    //t('serialize-json(xs:double(2))', '2E0');
    t('serialize-json(xs:decimal(2))', '2'); //todo: which one is correct
    t('serialize-json(xs:double(2))', '2');

    t('true', '');
    t('false', '');
    if not ps.StaticContext.strictTypeChecking then  t('jn:is-null(null)', 'false');
    ps.ParsingOptions.AllowJSONLiterals:=true;
    t('true', 'true');
    t('false', 'false');
    t('jn:is-null(null)', 'true');
    t('./true', '');
    t('./false', '');
    if not ps.StaticContext.strictTypeChecking then t('jn:is-null(./null)', 'false');
    t('./(true)', 'true');
    t('./(false)', 'false');
    t('jn:is-null(./(null))', 'true');
    t('count((null, null, null))', '3');
    t('jn:size([null, null, null])', '3');
    t('jn:size([null, (), null])', '2');
    t('jn:size([(), (), ()])', '0');
    t('for $a in true return true', 'true');
    t('for $a in false return false', 'false');
    t('for $a in true return $a', 'true');
    t('for $a in false return $a', 'false');
    t('for $a in false return $a', 'false');
    t('some $x in (true, false), $y in (true, false) satisfies $x eq $y', 'true');
    t('every $x in (true, true), $y in (true, true) satisfies $x eq $y', 'true');

    t('serialize-json([1 to 3, 4 to 6])', '[1, 2, 3, 4, 5, 6]');
    t('serialize-json([[1 to 3], [4 to 6]])', '[[1, 2, 3], [4, 5, 6]]');

    f('serialize-json(jn:object(({"a": 1}, {"b": 2}, {"a": 3})))', 'jerr:JNDY0003');

    //Tests based on examples in the JSONiq spec
(*    t('if (jn:null()) then "T" else "F"', 'F');
    t('if ({}) then "T" else "F"', 'T');
    t('if ({ "foo": false } ) then "T" else "F"', 'T');
    t('if ( { "foo": 3, "bar":4 }) then "T" else "F"', 'T');
    t('if ({ "foo": 3 }) then "T" else "F"', 'T');
    t('if ( [1] ) then "T" else "F"', 'T');
    t('if ( ( [1], jn:null()  ) ) then "T" else "F"', 'T');
    t('if ( [null] ) then "T" else "F"', 'T');
    t('if ( [] ) then "T" else "F"', 'T');
    t('if (()) then "T" else "F"', 'F');
    t('([]) and true()', 'true');
    t('({}) and true()', 'true');
    t('([], 1) and true()', 'true');
    t('({}, 2) and true()', 'true');
    todo FORG0006*)

    t('() + 1', '');
    f('null + 1', 'err:XPTY0004');
    t('1 + ()', '');
    f('1 + null', 'err:XPTY0004');
    f('null - 1', 'err:XPTY0004');
    f('null * 1', 'err:XPTY0004');
    f('null div 1', 'err:XPTY0004');
    f('null idiv 1', 'err:XPTY0004');

    t('() eq 1', '');
    t('null eq 1', 'false');
    t('null ne 1', 'true');
    t('null eq null', 'true');
    t('null lt 1', 'true');
    t('null gt 1', 'false');
    t('1 lt null', 'false');
    t('1 gt null', 'true');

    t('(null, 2) = (1, 3)', 'false');
    t('(1, null, 3) = (1, 3)', 'true');
    t('(null, 1, null, 3) = (1, 3, null)', 'true');
    t('(null, null) = (1, 3, null)', 'true');
    t('null = (1, 3)', 'false');
    t('null = (1, null, 3)', 'true');
    t('null != (1, 3)', 'true');

    t('serialize-json([ "Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ])', '["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]');
    t('serialize-json(          [ [1, 2, 3],            [4, 5, 6],            [7, 8, 9]          ])', '[[1, 2, 3], [4, 5, 6], [7, 8, 9]]');
    t('serialize-json([ 10 to 15 ])', '[10, 11, 12, 13, 14, 15]');
    t('serialize-json({            "id" : 404,     "name" : "Stanco Grease Pot",   "price" : 6.49,    "weight" : 3.8,     "uses" : ["Grease storage","Backpacking pot"] })', '{"id": 404, "name": "Stanco Grease Pot", "price": 6.49, "weight": 3.8, "uses": ["Grease storage", "Backpacking pot"]}');
    t('serialize-json({"Sunday" : 1,     "Monday" : 1 + 1,    "Tuesday" : 3 * 1,    "Wednesday" : 8 div 2,    "Thursday" : 5,   "Friday" : count(for $i in 1 to 6 return $i),           "Saturday": 10 - 3 })','{"Sunday": 1, "Monday": 2, "Tuesday": 3, "Wednesday": 4, "Thursday": 5, "Friday": 6, "Saturday": 7}');


    t('serialize-json( {"a" ?: 123, "b" ?: (), "c" ?: (4,5) } )', '{"a": 123, "c": [4, 5]}');
    t('serialize-json( {"a" ?: () } )', '{}');

    t('serialize-json({|  |})', '{}');
    t('serialize-json({| {}, {}, {} |})', '{}');
    t('serialize-json({| {"a": 123} |})', '{"a": 123}');
    t('serialize-json({| {"a": 123}, {}, {}, {} |})', '{"a": 123}');
    t('serialize-json({| {"a": 123}, {"b": 4, "c": 7} |})', '{"a": 123, "b": 4, "c": 7}');

    ps.ParsingOptions.AllowJSONLiterals:=false;

  end;


  ps.StaticContext.strictTypeChecking:=false;

  t('jn:is-null({"a": ()}("a"))', 'false');
//  f('{"a": (10,20,30,40,50)}.a(3)'); //no error since jsoniq 1.0.1
  t('{"a": (10,20,30,40,50)}.a[3]', '30');
  ps.ParsingOptions.JSONObjectMode:=xqjomJSONiq;
  t('jn:is-null({"a": ()}("a"))', 'true');
  t('{"a": (10,20,30,40,50)}.a(3)', '30');
  t('{"a": (10,20,30,40,50)}.a[3]', '');
  ps.ParsingOptions.JSONObjectMode:=xqjomMapAlias;


  //Tests based on failed XQTS tests
  ps.StaticContext.strictTypeChecking:=strictTypeChecking;

  t('count(a/attribute::*)', '0', '<a></a>');
  t('count(a/attribute::node())', '0', '<a></a>'); //my
  t('count(a/attribute::node())', '2', '<a a="abc" x="foo"></a>'); //my
  t('count(a/attribute())', '0', '<a></a>'); //my
  t('count(a/attribute())', '2', '<a a="abc" x="foo"></a>'); //my
  t('count(a/attribute::attribute())', '0', '<a></a>'); //my
  t('count(a/attribute::attribute())', '2', '<a a="abc" x="foo"></a>'); //my
  t('count(a/attribute(*))', '0', '<a></a>'); //my
  t('count(a/attribute(*))', '2', '<a a="abc" x="foo"></a>'); //my
  t('count(a/attribute::attribute(*))', '0', '<a></a>'); //my
  t('count(a/attribute::attribute(*))', '2', '<a a="abc" x="foo"></a>'); //my
  t('join(for $i in //descendant-or-self::*  return node-name($i), ";")', 'html', '!<html></html>');
  t('join(for $i in .//descendant-or-self::*  return node-name($i), ";")', 'html', '!<html></html>');
  t('join(for $i in .//descendant-or-self::*  return node-name($i), ";")', 'html', '!<html></html>');
  t('join(for $i in /node() return node-name($i), ";")', 'abc;html', '!<?abc?><html/>');
  t('join(for $i in /node() return node-name($i), ";")', 'abc;html', '!<?abc ?><html/>');
  t('join(for $i in /node() return node-name($i), ";")', 'abc;html', '!<?abc foo="bar"?><html/>');
  t('join(for $i in /* return node-name($i), ";")', 'html', '!<?abc?><html/>');
  t('join(for $i in /node() return node-name($i), ";")', 'abc;def;html', '!<?abc?><?def ?><html/>');
  t('join(for $i in /processing-instruction() return node-name($i), ";")', 'abc;def', '!<?abc?><?def ?><html/>');
  t('join(for $i in /processing-instruction(def) return node-name($i), ";")', 'def', '!<?abc?><?def ?><html/>');
  t('join(for $i in /processing-instruction("abc") return node-name($i), ";")', 'abc', '!<?abc?><?def ?><html/>');
  t('join(for $i in /processing-instruction("  abc  ") return node-name($i), ";")', 'abc', '!<?abc?><?def ?><html/>');
  t('string-join(//self::*,";")', 'abcxfoobar;x', '!<html>abc<test>x</test>foobar</html>');
  t('string-join(time/x/a[3]/preceding::*,";")', 'q;1;2', '<time><p>q</p>t<x>u<a>1</a><a>2</a><a>3</a></x></time>');
  t('string-join(time/x/a[3]/preceding::a,";")', '1;2', '<time><p>q</p>t<x>u<a>1</a><a>2</a><a>3</a></x></time>');
               //instance of tests (still failed xqts)
  t('4 instance of item()', 'true', '');
  t('(4,5) instance of item()', 'false', '');
  t('(4,5) instance of item()+', 'true', '');
  t('() instance of item()+', 'false', '');
  t('() instance of empty-sequence()', 'true', '');
  t('(3) instance of empty-sequence()', 'false', '');
  t('/a instance of node()', 'true', '!<a>hallo<!--comment--></a>');
  t('/a instance of element()', 'true', '');
  t('/a instance of comment()', 'false', '');
  t('/a instance of processing-instruction()', 'false', '');
  t('/a instance of text()', 'false', '');
  t('/a/text() instance of node()', 'true', '');
  t('/a/text() instance of element()', 'false', '');
  t('/a/text() instance of comment()', 'false', '');
  t('/a/text() instance of processing-instruction()', 'false', '');
  t('/a/text() instance of text()', 'true', '');
  t('/a/comment() instance of node()', 'true', '');
  t('/a/comment() instance of element()', 'false', '');
  t('/a/comment() instance of comment()', 'true', '');
  t('/a/comment() instance of processing-instruction()', 'false', '');
  t('/a/comment() instance of text()', 'false', '');
  t('/a/node() instance of node()', 'true', '!<a><?option?></a>');
  t('/a/node() instance of text()', 'false', '');
  t('/a/node() instance of element()', 'false', '');
  t('/a/node() instance of comment()', 'false', '');
  t('/a/node() instance of processing-instruction()', 'true', '');
  t('xs:float(1) castable as xs:string', 'true', '');
  t('if (xs:float(1.5) castable as xs:string) then 1 else 2', '1', '');
  t('if (xs:float(1.5) castable as xs:integer) then 1 else 2', '1', '');
  t('if (xs:float("NaN") castable as xs:integer) then 1 else 2', '2', '');
  t('/td[if (true()) then true() else false()]', '', '');
  t('true() instance of anyAtomicType', 'true', '');
//  t('QName("abc")', 'abc', ''); todo?
               //,('QName("abc", "def")', 'abc', '')

  t('xs:gMonth("--12") castable as decimal', 'false', '');
  t('xs:dateTime("2030-12-05T01:01:01") castable as decimal', 'false', '');
  t('(0.0 div 0e1)', 'NaN', '');
  t('(0.0 div /x)', 'NaN', '<x>0</x>');
  t('(0.0 div /x)', '0', '<x>1</x>');
  t('type-of((0.0 div 0e0))', 'double');
  t('(0.0 div 0e0) castable as xs:integer', 'false', '');
  t('(1.0 div 0e1)', 'INF', '');
  t('(-1.0 div 0e1)', '-INF', '');
  f('2 mod 0', 'err:FOAR0001');
  t('(3 idiv xs:untypedAtomic("2"))', '1', '');
  //t('(3 idiv xs:untypedAtomic(" 2 "))', '1', '');
  t('xs:float(0.0 div 0e0) castable as xs:integer', 'false', '');
  t('xs:base64Binary("0FB7")', '0FB7', '');
  t('xs:hexBinary("07fb")', '07FB', '');
  t('xs:hexBinary(base64Binary("YWJj"))', '616263', '');
  t('xs:hexBinary("616263") eq xs:hexBinary(xs:base64Binary("YWJj"))', 'true', '');
  t('xs:hexBinary("616263") castable as xs:boolean', 'false', '');
  t('xs:hexBinary("616263") castable as xs:decimal', 'false', '');
  t('true() castable as xs:decimal', 'true', '');
  t('xs:dateTime("2012-12-12T00:00:00") castable as xs:decimal', 'false', '');
  t('xs:dateTime("2012-12-12T00:00:00") castable as xs:gMonth', 'true', '');
  t('xs:gMonth("--12") castable as xs:dateTime', 'false', '');
  t('xs:gMonthDay("--12-05") castable as xs:gMonth', 'false', '');
  t('xs:gMonth("--12") castable as xs:gMonthDay', 'false', '');
  t('xs:gMonth("--12") castable as xs:boolean', 'false', '');
  t('xs:string("1999-05-31Z") castable as xs:date', 'true', '');
  t('xs:string("ABA") castable as xs:hexBinary', 'false', '');
  t('xs:double(123.456) castable as xs:decimal', 'true', '');
  t('xs:double("INF") castable as xs:decimal', 'false', '');
  t('xs:double("INF") castable as xs:float', 'true', '');
  t('"INF" castable as xs:float', 'true', '');
  t('gDay("---30") castable as xs:hexBinary', 'false', '');
  t('/a castable as xs:integer', 'true', '!<a>100</a>');
  t('/a castable as xs:integer', 'false', '!<a>abc</a>');
  t('xs:date("1999-05-17") cast as xs:dateTime', '1999-05-17T00:00:00', '');
  t('xs:date("-0753-12-05") cast as xs:dateTime', '-0753-12-05T00:00:00', '');
  t('xs:time("12:12:12.5") - xs:time("12:12:12")', 'PT0.5S', '');
  t('fn:month-from-dateTime(fn:dateTime(xs:date("1999-12-31+10:00"), xs:time("23:00:00+10:00")))', '12', '');
  t('xs:dateTime("1999-12-30T20:30:40.5")', '1999-12-30T20:30:40.5', '');
  t('xs:date("1999-07-19") - xs:date("1969-11-30")', 'P10823D', '');
  t('(xs:date("1999-07-19") - xs:date("1969-11-30")) eq xs:dayTimeDuration("P10823D")', 'true', '');
  t('fn:dateTime(date("1999-12-30"), time("20:30:40.23-05:00"))', '1999-12-30T20:30:40.23-05:00', '');
  t('fn:dateTime(date("1999-12-30+04:00"), time("20:30:40.23"))', '1999-12-30T20:30:40.23+04:00', '');
  t('(xs:date("2000-10-30") - xs:date("1999-11-28")) + dayTimeDuration("P1D")', 'P338D', '');
  t('dayTimeDuration("P1D") + (xs:date("2000-10-30") - xs:date("1999-11-28"))', 'P338D', '');
  if baseSchema.version = xsd11 then begin
    bce1 := '0000';
    bce2 := '-0001';
  end else begin
    bce1 := '-0001';
    bce2 := '-0002';
  end;
  t('xs:date("0001-01-01") - xs:date("'+bce1+'-12-31")', 'P1D', '');
  t('xs:date("0001-01-01") + xs:dayTimeDuration("-P1D")', bce1+'-12-31', '');
  t('xs:date("0001-01-01") + xs:yearMonthDuration("-P1M")', bce1+'-12-01', '');
  t('xs:date("0001-01-01") + xs:yearMonthDuration("-P3M")', bce1+'-10-01', '');
  t('xs:date("0001-01-01") + xs:yearMonthDuration("-P12M")', bce1+'-01-01', '');
  t('xs:date("0001-01-01") + xs:yearMonthDuration("-P13M")', bce2+'-12-01', '');
  t('xs:date("0001-01-01") + xs:yearMonthDuration("-P16M")', bce2+'-09-01', '');
  t('years-from-duration(xs:yearMonthDuration("-P16M"))', '-1', '');
  t('fn:seconds-from-duration(xs:dayTimeDuration("P3DT10H12.5S"))', '12.5', '');
  t('fn:seconds-from-duration(xs:dayTimeDuration("-PT256S"))', '-16', '');
  t('fn:minutes-from-duration(xs:dayTimeDuration("-P5DT12H30M"))', '-30', '');
  t('fn:hours-from-duration(xs:dayTimeDuration("PT123H"))', '3', '');
  t('fn:days-from-duration(xs:yearMonthDuration("P3Y5M"))', '0', '');
  t('fn:days-from-duration(xs:dayTimeDuration("P3DT55H"))', '5', '');
  t('xs:yearMonthDuration("P6M")+xs:yearMonthDuration("P6M")', 'P1Y', '');
  t('xs:yearMonthDuration("P18M")-xs:yearMonthDuration("P6M")', 'P1Y', '');
  t('xs:yearMonthDuration("P6M")-xs:yearMonthDuration("P18M")', '-P1Y', '');
  t('xs:yearMonthDuration("P20Y123M")', 'P30Y3M', '');
  t('xs:yearMonthDuration("P3Y36M") div xs:yearMonthDuration("P60Y")  eq 0.1', 'true', '');
  t('xs:date("2004-12-30") castable as gMonth', 'true', '');
  t('xs:time("20:03:04") castable as gMonth', 'false', '');
  t('xs:hexBinary("20") castable as gMonth', 'false', '');
  t('xs:time("20:03:04") castable as duration', 'false', '');
  t('QName("example.com/", "p:ncname") ne QName("example.com/", "p:ncnameNope")', 'true', '');
  t('fn:QName("http://www.example.com/example1", "person") eq fn:QName("http://www.example.com/example2", "person")', 'false', '');
  t('2.e3', '2000', '');
  t('.2', '0.2', '');
  t('join((.1,.2,.3,.4,.5,.6,.7,.8,.9,.111222333444555666777888999111222333444555666777888999))',  '0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.111222333444555666777888999111222333444555666777888999');
  tdouble('10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000E-1000', '1.0E6');
  tdouble('-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000E-1000', '-1.0E6');
  t('000000.000000000000000000000000000000000000000000000E10000000000000000000000000000000000000000000', '0');
  tdoubleOrInvalid('10000000000000000000000000000000000000000000E-10000000000000000000000000000000000000000000', '0');
  tdoubleOrInvalid('10000000000000000000000000000000000000000000E10000000000000000000000000000000000000000000', 'INF');
  tdoubleOrInvalid('-10000000000000000000000000000000000000000000E10000000000000000000000000000000000000000000', '-INF');
  t('""""', '"', '');
  t('''''''''', '''', '');
  ps.StaticContext.strictTypeChecking:=false;
  t('duration("P1YT4H") - duration("P12MT240M")', 'PT0S', '');
  ps.StaticContext.strictTypeChecking:=strictTypeChecking;
  t('sum((dayTimeDuration("PT1S"), dayTimeDuration("PT2S")))', 'PT3S', '');
  t('sum((yearMonthDuration("P1M"), yearMonthDuration("P11M")))', 'P1Y', '');
  t('type-of(sum((dayTimeDuration("PT1S"), dayTimeDuration("PT2S"))))', 'dayTimeDuration', '');
  t('type-of(sum((yearMonthDuration("P1M"), yearMonthDuration("P11M"))))', 'yearMonthDuration', '');
  t('sum(a/b)', '6', '<a><b>1</b><b>2</b><b>3</b></a>');
  t('type-of(sum(a/b))', 'double', '');
  t('max((dayTimeDuration("PT1S"), dayTimeDuration("PT2S")))', 'PT2S', '');
  t('max(a/b)', '3', '');
  t('type-of(max(a/b))', 'double', '');
  t('min((dayTimeDuration("PT1S"), dayTimeDuration("PT2S")))', 'PT1S', '');
  f('max((xs:yearMonthDuration("P1Y"), xs:dayTimeDuration("P1D")))', 'err:FORG0006');
  t('min(a/b)', '1', '');
  t('min((xs:double("NaN"), 1))', 'NaN', '');
  t('type-of(min(a/b))', 'double', '');
  t('avg((dayTimeDuration("PT1S"), dayTimeDuration("PT2S")))', 'PT1.5S', '');
  t('avg(a/b)', '2', '');
  t('type-of(avg(a/b))', 'double', '');
  t('xs:gYearMonth("2005-02Z")', '2005-02Z', '');
  t('xs:gYearMonth("2005-02Z") = xs:gYearMonth("2005-02Z")', 'true', '');
  t('seconds-from-dateTime(xs:dateTime("2000-01-01T18:23:45.123"))', '45.123', '');
  t('xs:dateTime("-0001-12-31T24:00:00")', '0001-01-01T00:00:00', '');
  t('xs:float(1.01)', '1.01', '');
  t('fn:compare("abc", "abc")', '0', '');
  t('fn:compare("abc", ())', '', '');
  t('/a/b/c/lang("de")', 'true', '!<a><b><c xml:lang="de"></c></b></a>');
  t('node-name(//c)', 'c', '!<a><b><c xml:lang="de"></c></b></a>');
  t('for $var in (1,2,3,4,5) return $var', '12345', '');
  t('fn:resolve-uri("abc", "http://www.example.com")' ,'http://www.example.com/abc', '');
  t('fn:resolve-uri("", "http://www.example.com")' ,'http://www.example.com', '');
  t('fn:resolve-uri("", "http://www.example.com/")' ,'http://www.example.com/', '');
  t('fn:resolve-uri("", "http://www.example.com/")' ,'http://www.example.com/', '');
  t('fn:resolve-uri(".", "http://www.example.com/")' ,'http://www.example.com/', '');
  t('fn:resolve-uri(".", "http://www.example.com/")' ,'http://www.example.com/', '');
  t('fn:resolve-uri("././c", "http://www.example.com")' ,'http://www.example.com/c', '');
  t('fn:resolve-uri("././c", "http://www.example.com/")' ,'http://www.example.com/c', '');
  t('fn:tokenize("abc", "def")', 'abc', '');
  t('pxp:type-of(abs(xs:byte(1)))', 'integer', ''); //standard, abs: If the type of $arg is a type derived from one of the numeric types, the result is an instance of the base numeric type.
  t('abs(int("0"))', '0', '');
  t('abs(negativeInteger(-3))', '3', '');
  t('type-of(number(-3))', 'double', '');
  t('number("")', 'NaN', '');
  t('number()', '123', '<x>123</x>');
  t('number()', 'NaN', '<x>foo</x>');
  t('fn:number(xs:float("-3.4028235E38")) eq xs:float("-3.4028235E38")', 'true');
  t('fn:number(xs:float("-3.4028235E38")) eq -3.4028234663852885E38', 'true');
  tdouble('xs:double("-1.7976931348623157E308") eq xs:double("1.7976931348623157E308")', 'false');
  t('xs:double("-1.7976931348623157E308") eq xs:double("-1.7976931348623157E308")', 'true');
  t('not(double("NaN"))', 'true', '');
  t('not(double("INF"))', 'false', '');
  t('not(double("-INF"))', 'false', '');
  t('subsequence((1,2), 4)', '', '');
  t('string-join((), "...") eq ""', 'true', '');
  t('tokenize("", "abc") eq ""', '', ''); //empty sequence returned by tokenize
  t('/', '12', '!<a>12</a>');
  t('(/) castable as xs:decimal', 'true');
  t('(/) castable as xs:float', 'true');
  t('(/) castable as xs:double', 'true');
  t('(/) castable as xs:integer', 'true');
  t('(/) * 3', '36', '');
  t('4 + /', '16', '');
  t('string-join(/a//b, ",")', '1,2,3,4,5,6', '!<a><b>1</b><c><b>2</b><b>3</b></c><d><b>4</b><b>5</b></d><b>6</b></a>');
  t('string-join(/a//b/parent::d, ",")', '45', '');
  t('string-join(/a//b[1], ",")', '1,2,4', '');
  t('string-join(/a//b[2], ",")', '3,5,6', '');
  t('string-join(/a//(b[2]), ",")', '3,5,6', '');
  t('string-join(/a//(if (b[2]) then b[2] else ()), ",")', '3,5,6', '');
  t('string-join(/a//(if (b[2]) then string(b[2]) else ()), ",")', '6,3,5', '');
  t('string-join(/a//b[3], ",")', '', '');
  t('string-join((/a//b)[1], ",")', '1', '');
  t('string-join((/a//b)[2], ",")', '2', '');
  t('string-join((/a//b)[3], ",")', '3', '');
  t('string-join(/a//b[2]/parent::*, ",")', '123456,23,45', '');
  t('string-join((/a//b)[2]/parent::*, ",")', '23', '');
  t('round(xs:double("INF"))', 'INF', '');
  t('round(xs:double("-INF"))', '-INF', '');
  t('round(xs:double("NaN"))', 'NaN', '');
  t('round-half-to-even(xs:double("INF"))', 'INF', '');
  t('round-half-to-even(xs:double("-INF"), 5)', '-INF', '');
  t('xs:float("  5  ")', '5', '');
  t('xs:boolean("  true  ")', 'true', '');
  t('xs:boolean("  false  ")', 'false', ''); //<- not xpath but here
  t('xs:hexBinary("  56  ")', '56', '');
  t('xs:gMonth("  --03  ") eq xs:gMonth("--03")', 'true', '');
  t('xs:anyURI("  http://www.example.com  ") eq xs:anyURI("http://www.example.com")', 'true', '');
  t('xs:string(xs:hexBinary(" abcd ")) eq "ABCD"', 'true', '');
  t('codepoints-to-string(()) eq ""', 'true', '');
  t('xs:date(xs:dateTime("2002-11-23T22:12:23.867-13:37"))', '2002-11-23-13:37', '');
  t('xs:date(xs:dateTime("2002-11-23T22:12:23.867-13:37")) eq xs:date("2002-10-23-13:37")', 'false', '');
  t('xs:date(xs:dateTime("2002-11-23T22:12:23.867-13:37")) eq xs:date("2002-11-23-13:37")', 'true', '');
  t('xs:string(xs:dateTime("2002-02-15T21:01:23.110"))', '2002-02-15T21:01:23.11', '');
  t('xs:string(xs:time("21:01:23.001"))', '21:01:23.001', '');
  t('xs:date(xs:dateTime("2002-11-23T22:12:23.867-13:37")) eq xs:date("2002-11-23-13:37")', 'true', '');
  ps.StaticContext.strictTypeChecking:=false;
  t('xs:time("12:12:12") eq xs:date("2012-12-13")', 'false', '');
  t('xs:dateTime("2002-11-23T22:12:23.867-13:37") eq xs:time("22:12:23.867-13:37")', 'true', '');
  t('xs:dateTime("2002-11-23T22:12:23.867-13:37") eq xs:time("22:12:23-13:37")', 'false', '');
  t('xs:dateTime("2002-11-23T22:12:23.867-13:37") eq xs:time("23:12:23.867-12:37")', 'true', '');
  //             ,('xs:dateTime("2002-11-23T22:12:23.867-13:37") eq xs:time("24:12:23.867-11:37")', 'true', '') should this work?
  t('xs:dateTime("2002-11-23T22:12:23.867-13:37") eq xs:time("11:49:23.867Z")', 'false', ''); //day overflow?
  ps.StaticContext.strictTypeChecking:=strictTypeChecking;
  t('(xs:gYear("2005-12:00") eq xs:gYear("2005+12:00"))', 'false', '');
  t('(xs:gDay("---12") eq xs:gDay("---12Z"))', 'false', '');
  t('xs:time("08:00:00+09:00") eq xs:time("17:00:00-06:00")', 'false', ''); //from xpath standard example
  t('xs:time("21:30:00+10:30") eq xs:time("06:00:00-05:00")', 'true', ''); //from xpath standard example
  t('xs:time("24:00:00+01:00") eq xs:time("00:00:00+01:00")', 'true', ''); //from xpath standard example
  t('xs:duration("-P1YT2.3S")', '-P1YT2.3S', '');
  t('xs:dayTimeDuration(xs:yearMonthDuration("-P543Y456M"))', 'PT0S', '');
  t('deep-equal(xs:float("NaN"),xs:double("NaN"))', 'true', '');
  t('count(distinct-values((xs:float("NaN"),xs:double("NaN"))))', '1', '');
  t('xs:float("3") idiv xs:float("INF")', '0');
  t('xs:float("3") idiv xs:float("INF") eq xs:float(0)', 'true');

  //             ,('count(distinct-values((xs:float("INF"),xs:double("INF"))))', '1', '')

    //           ,('xs:date(1999-05-17) cast as xs:dateTime', '1999-05-31T00:00:00', '')
               //,('/a/processing-instruction(option) instance of node()', 'false', '') todo


               //---------------------------------CSS Selectors-----------------------------------
               //some examples from the standard (their css, my own xml)
  t('', '', '<x><a hreflang="en" class="warning">a1</a><a hreflang="de" id="myid">a2</a><a hreflang="fr-en-de">a3</a></x>');
  t('string-join(css("a"), ",")', 'a1,a2,a3', '');
  t('string-join(css("#myid"), ",")', 'a2', '');
  t('string-join(css("*#myid"), ",")', 'a2', '');
  t('string-join(css(".warning"), ",")', 'a1', '');
  t('string-join(css("*.warning"), ",")', 'a1', '');
  t('string-join(css("*[hreflang=en]"), ",")', 'a1', '');
  t('string-join(css("[hreflang=en]"), ",")', 'a1', '');
  t('string-join(css("*[hreflang|=en]"), ",")', 'a1,a3', '');
  t('string-join(css("[hreflang|=en]"), ",")', 'a1,a3', '');
  t('', '', '<x><h1>A</h1><h1 title="T">B</h1><h1 title>C</h1><span class="example">S1</span><span hello="Cleveland">S2</span><span hello="Cleveland" class="EXAMPLE">S3</span><span goodbye="Columbus">S4</span><span hello="Cleveland" goodbye="Columbus">S5</span><a rel="copyright copyleft copyeditor">A1</a><a href="http://www.w3.org/">A2</a><a rel="copyright" href="http://www.w3.org/index.html">A3</a><a rel="xyzcopyrightfoo">A4</a><DIALOGUE character="romeo">D1</DIALOGUE><DIALOGUE character="juliet">D2</DIALOGUE></x>');
  t('string-join(css("h1[title]"), ",")', 'B,C', '');
  t('string-join(css("span[class=''example'']"), ",")', 'S1,S3', '');
  t('string-join(css("span[hello=''Cleveland''][goodbye=Columbus]"), ",")', 'S5', '');
  t('string-join(css(''a[rel~="copyright"]''), ",")', 'A1,A3', '');
  t('string-join(css(''a[href="http://www.w3.org/"]''), ",")', 'A2', '');
  t('string-join(css(''DIALOGUE[character=romeo]''), ",")', 'D1', '');
  t('string-join(css(''DIALOGUE[character=juliet]''), ",")', 'D2', '');
  t('string-join(css(''object[type^="image/"]''), ",")', 'O1,O2', '<x><object type="image/123">O1</object><object type="IMAGE/">O2</object><object type="image">O3</object></x>');
  t('string-join(css(''a[href$=".html"]''), ",")', 'A3', '<a href="http://xyz.com">A1</a><a href="http://xyz.com/test.gif">A2</a><a href="http://xyz.com/test.html">A3</a>');
  t('string-join(css(''p[title*="hello"]''), ",")', 'P2,P3', '<p title="foobar">P1</p><p title="xyzhelloyzxy">P2</p><p title="hello">P3</p>');
  t('string-join(css(''p[*|title*="hello"]''), ",")', 'P2,P3', '');
  ps.StaticContext.useLocalNamespaces:=true;
  t('string-join(css(''p[xyz|title*="hello"]''), ",")', '', '');
  ps.StaticContext.useLocalNamespaces:=false;
  t('string-join(css(''p[|title*="hello"]''), ",")', 'P2,P3', '');
  t('string-join(css(''*.pastoral''), ",")', 'S1,Very green,P1,P2', '<span class="pastoral">S1</span><H1>Not green</H1><H1 class="pastoral">Very green</H1><p class="pastoral blue aqua marine">P1</p><p class="pastoral blue">P2</p>');
  t('string-join(css(''.pastoral''), ",")', 'S1,Very green,P1,P2', '');
  t('string-join(css(''H1.pastoral''), ",")', 'Very green', '');
  t('string-join(css(''p.pastoral.marine''), ",")', 'P1', '');

  t('string-join(css(''h1#chapter1''), ",")', 'ha', '<body><h1 id="chapter1">ha</h1><span id="z98y">S</span></body>');
  t('string-join(css(''#chapter1''), ",")', 'ha', '');
  t('string-join(css(''*#z98y''), ",")', 'S', '');
  t('string-join(css(''h1#chapter1''), ",")', '', '<body><h2 id="chapter1">ha</h2><span id="z98y">S</span></body>');
  t('string-join(css(''#chapter1''), ",")', 'ha', '');

  t('string-join(css(''tr:nth-child(7)''), ",")', '7', '<table><tr><td>1</td></tr> <tr><td>2</td></tr> <tr><td>3</td></tr> <tr><td>4</td></tr> <tr><td>5</td></tr> <tr><td>6</td></tr> <tr><td>7</td></tr> <tr><td>8</td></tr> <tr><td>9</td></tr> <tr><td>10</td></tr> <tr><td>11</td></tr> <tr><td>12</td></tr> <tr><td>13</td></tr> <tr><td>14</td></tr> <tr><td>15</td></tr> <tr><td>16</td></tr> <tr><td>17</td></tr> <tr><td>18</td></tr> <tr><td>19</td></tr> <tr><td>20</td></tr> </table>');
  t('string-join(css(''tr:nth-child(2n+1)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-child(odd)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-child(2n+0)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-child(even)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css('':nth-child(10n-1)''), ",")', '9,19', '');
  t('string-join(css('':nth-child(10n+9)''), ",")', '9,19', '');
  t('string-join(css('':nth-child(0n+5)''), ",")', '5', '');
  t('string-join(css('':nth-child(5)''), ",")', '5', '');
  t('string-join(css('':nth-child(1n+0)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css('':nth-child(n+0)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css('':nth-child(n)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css(''tr:nth-child(2n)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-child( 3n + 1 )''), ",")', '1,4,7,10,13,16,19', '');
  t('string-join(css(''tr:nth-child( +3n - 2 )''), ",")', '1,4,7,10,13,16,19', '');
  t('string-join(css(''tr:nth-child( -n+ 6)''), ",")', '1,2,3,4,5,6', '');
  t('string-join(css(''tr:nth-child( +6 )''), ",")', '6', '');

  t('string-join(css(''tr:nth-of-type(7)''), ",")', '7', '');
  t('string-join(css(''tr:nth-of-type(2n+1)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-of-type(odd)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-of-type(2n+0)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-of-type(even)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css('':nth-of-type(10n-1)''), ",")', '9,19', '');
  t('string-join(css('':nth-of-type(10n+9)''), ",")', '9,19', '');
  t('string-join(css('':nth-of-type(0n+5)''), ",")', '5', '');
  t('string-join(css('':nth-of-type(5)''), ",")', '5', '');
  t('string-join(css('':nth-of-type(1n+0)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css('':nth-of-type(n+0)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css('':nth-of-type(n)''), ",")', '1234567891011121314151617181920,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20', '');
  t('string-join(css(''tr:nth-of-type(2n)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-of-type( 3n + 1 )''), ",")', '1,4,7,10,13,16,19', '');
  t('string-join(css(''tr:nth-of-type( +3n - 2 )''), ",")', '1,4,7,10,13,16,19', '');
  t('string-join(css(''tr:nth-of-type( -n+ 6)''), ",")', '1,2,3,4,5,6', '');
  t('string-join(css(''tr:nth-of-type( +6 )''), ",")', '6', '');

  t('string-join(css(''tr:nth-last-child(7)''), ",")', '14', '');
  t('string-join(css(''tr:nth-last-child(-n+2)''), ",")', '19,20', '');
  t('string-join(css(''tr:nth-last-child(odd)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-last-child(even)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');

  t('string-join(css(''tr:nth-last-of-type(7)''), ",")', '14', '');
  t('string-join(css(''tr:nth-last-of-type(-n+2)''), ",")', '19,20', '');
  t('string-join(css(''tr:nth-last-of-type(odd)''), ",")', '2,4,6,8,10,12,14,16,18,20', '');
  t('string-join(css(''tr:nth-last-of-type(even)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');

  t('string-join(css(''tr:nth-child(odd):nth-child(3n)''), ",")', '3,9,15', '');
  t('string-join(css(''tr:nth-child(odd):nth-child(7)''), ",")', '7', '');
  t('string-join(css(''tr:nth-child(odd):nth-child(odd)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-child(odd):nth-last-child(odd)''), ",")', '', '');
  t('string-join(css(''tr:nth-child(odd):nth-last-child(even)''), ",")', '1,3,5,7,9,11,13,15,17,19', '');
  t('string-join(css(''tr:nth-child(n+2):nth-last-child(n+2)''), ",")', '2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19', '');
  t('string-join(css(''table  tr:nth-child(n+2):nth-last-child(n+2)''), ",")', '2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19', '');
  t('string-join(css(''table > tr:nth-child(n+2):nth-last-child(n+2)''), ",")', '2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19', '');


  t('string-join(css(''h1:nth-of-type( 2 )''), ",")', 'H2', '<div><h1>H1</h1><h1>H2</h1><span>S1</span><span>S2</span><span>S3</span><span>S4</span></div>');
  t('string-join(css(''span:nth-of-type( 2 )''), ",")', 'S2', '');
  t('string-join(css('':nth-of-type( 2 )''), ",")', 'H2,S2', '');
  t('string-join(css(''h1:nth-child( 2 )''), ",")', 'H2', '');
  t('string-join(css(''span:nth-child( 2 )''), ",")', '', '');
  t('string-join(css('':nth-child( 2 )''), ",")', 'H2', '');
  t('string-join(css(''h1:nth-last-of-type( 2 )''), ",")', 'H1', '');
  t('string-join(css(''span:nth-last-of-type( 2 )''), ",")', 'S3', '');
  t('string-join(css('':nth-last-of-type( 2 )''), ",")', 'H1,S3', '');
  t('string-join(css(''h1:nth-last-child( 2 )''), ",")', '', '');
  t('string-join(css(''span:nth-last-child( 2 )''), ",")', 'S3', '');
  t('string-join(css('':nth-last-child( 2 )''), ",")', 'S3', '');

  t('string-join(css(''h1:first-child''), ",")', 'H1', '');
  t('string-join(css(''h1:last-child''), ",")', '', '');
  t('string-join(css(''span:first-child''), ",")', '', '');
  t('string-join(css(''span:last-child''), ",")', 'S4', '');
  t('string-join(css(''*''), ",")', 'H1H2S1S2S3S4,H1,H2,S1,S2,S3,S4', '');
  t('string-join(css('':first-child''), ",")', 'H1H2S1S2S3S4,H1', '');
  t('string-join(css('':last-child''), ",")', 'H1H2S1S2S3S4,S4', '');

  t('string-join(css(''h1:first-of-type''), ",")', 'H1', '');
  t('string-join(css(''h1:last-of-type''), ",")', 'H2', '');
  t('string-join(css(''span:first-of-type''), ",")', 'S1', '');
  t('string-join(css(''span:last-of-type''), ",")', 'S4', '');
  t('string-join(css('':first-of-type''), ",")', 'H1H2S1S2S3S4,H1,S1', '');
  t('string-join(css('':last-of-type''), ",")', 'H1H2S1S2S3S4,H2,S4', '');

  t('string-join(css(''div > p:first-child''), ",")', 'The first P inside the note.', '<p> The last P before the note.</p> <div class="note">  <p> The first P inside the note.</p> </div>');
  t('string-join(css(''p:only-child''), ",")', 'The first P inside the note.', '');
  t('string-join(css('':first-child''), ",")', 'The last P before the note.,The first P inside the note.', '');
  t('string-join(css('':first-child:last-child''), ",")', 'The first P inside the note.', '');
  t('string-join(css(''p:only-of-type''), ",")', 'The last P before the note.,The first P inside the note.', '');
  t('string-join(css('':only-of-type''), ",")', 'The last P before the note.,The first P inside the note.,The first P inside the note.', '');
  t('string-join(css(''div > p:first-child''), ",")', '', '<p> The last P before the note.</p><div class="note">    <h2> Note </h2>   <p> The first P inside the note.</p></div>');
  t('string-join(css(''dl dt:first-of-type''), ",")', 'gigogne,fusée', '<dl> <dt>gigogne</dt> <dd>  <dl>   <dt>fusée</dt>   <dd>multistage rocket</dd>   <dt>table</dt>   <dd>nest of tables</dd>  </dl> </dd></dl>');

  t('count(css(''p''))', '2', '<x><p></p><foo>bar</foo><foo><bar>bla</bar></foo><foo>this is not <bar>:empty</bar></foo><p>1</p></x>');
  t('count(css(''foo''))', '3', '');
  t('count(css(''p:empty''))', '1', '');
  t('count(css(''foo:empty''))', '0', '');

  t('string-join(css(''*:link''), ",")', 'a2,a3', '<x><a>a1</a><p>p1</p><a href>a2</a><a href="foobar">a3</a><a>a4</a></x>');

  t('string-join(css(''*:not(:link)''), ",")', 'a1p1a2a3a4,a1,p1,a4', '');
  t('string-join(css(''*:link:not(:link)''), ",")', '', '');
  t('string-join(css(''a''), ",")', 'a1,a2,a3,a4', '');
  t('string-join(css(''a:not(a)''), ",")', '', '');
  t('string-join(css(''a:not([href=""])''), ",")', 'a1,a3,a4', ''); //confirmed with firefox/chrome
  t('string-join(css(''a:not([href])''), ",")', 'a1,a4', ''); //confirmed with firefox/chrome
  t('string-join(css(''*:not(a)''), ",")', 'a1p1a2a3a4,p1', '');
  t('string-join(css(''*:not(x)''), ",")', 'a1,p1,a2,a3,a4', '');
  t('string-join(css(''*:not(x):not(a)''), ",")', 'p1', '');


  t('string-join(css(''h1 em''), ",")', 'very,a', '<h1>This <span class="myclass">headline is <em>very</em> important</span><em>a</em></h1>');
  t('string-join(css(''h1 * em''), ",")', 'very', '');
  t('string-join(css(''h1 > em''), ",")', 'a', '');

  t('string-join(css(''*[lang|=fr]''), ",")', 'BJe suis français.', '<body lang=fr>B<p>Je suis français.</p></body>');
  t('string-join(css(''[lang|=fr]''), ",")', 'BJe suis français.', '');
  t('string-join(css(''*:lang(fr)''), ",")', 'BJe suis français.,Je suis français.', '');
  t('string-join(css('':lang(fr)''), ",")', 'BJe suis français.,Je suis français.', '');
  t('string-join(css(''*:lang(  fr  )''), ",")', 'BJe suis français.,Je suis français.', '');
  t('string-join(css('':lang(  fr  )''), ",")', 'BJe suis français.,Je suis français.', '');
  t('string-join(css(''*:lang(de)''), ",")', '', '');
  t('string-join(css('':lang(de)''), ",")', '', '');

  t('string-join(css(''div * p''), ",")', 'A3A4P2', '<div><p><a>A1</a><a href="x">A2</a>P1</p><span><p><a>A3</a><a href="qw">A4</a>P2</p></span><p>P3<a>A5</a><a href="qwq">A6</a></p></div>');
  t('string-join(css(''div p *[href]''), ",")', 'A2,A4,A6', '');
  t('string-join(css(''div > p *[href]''), ",")', 'A2,A6', '');
  t('string-join(css(''div>p *[href]''), ",")', 'A2,A6', '');
  t('string-join(css(''div>*>p *[href]''), ",")', 'A4', '');


  t('string-join(css(''body > p''), ",")', 'P0,P3', '<body><p>P0</p><div><ol><li><p>P!</p></li><div><li><p>pppp</p></li></div></ol><p>P2</p></div><P>P3</p></body>');
  t('string-join(css(''body'#9'p''), ",")', 'P0,P!,pppp,P2,P3', '');
  t('string-join(css(''div ol>li p''), ",")', 'P!', '');

  t('string-join(css(''math + p''), ",")', 'P0,P1', '<math>M0</math><p>P0</p><x><math>M1</math><p>P1</p><math>M2</math><div/><p>P2</p><math>M3</math></x><p>P3</p>');
  t('string-join(css(''math ~ p''), ",")', 'P0,P1,P2,P3', '');
  t('string-join(css(''math + p''), ",")', 'P0,P1,P2', '<math>M0</math><p>P0</p><x><math>M1</math><p>P1</p><math>M2</math>xxxx<p>P2</p><math>M3</math></x><p>P3</p>');
  t('string-join(css(''math + p''), ",")', 'P1', '<x><math>M1</math><p>P1</p><math>M2</math><div/><p>P2</p><math>M3</math></x><p>P3</p>');
  t('string-join(css(''math ~ p''), ",")', 'P1,P2', '');


  t('string-join(css(''h1.opener + h2''), ",")', 'h2ba', '<html><h1 class="x">h1a</h1><h2>h2aa</h2><h2>h2ab</h2><h1 class="opener">h1b</h1><h2>h2ba</h2><h2>h2bb</h2></html>');
  t('string-join(css(''h1.opener + h2''), ",")', 'h2ba', '<html>..<h1 class="x">h1a</h1>..<h2>h2aa</h2>..<h2>h2ab</h2>..<h1 class="opener">h1b</h1>..<h2>h2ba</h2>..<h2>h2bb</h2>..</html>');


  t('string-join(css(''h1 ~ pre''), ",")', 'function a(x) = 12x/13.5', '<h1>Definition of the function a</h1><p>Function a(x) has to be applied to all figures in the table.</p><pre>function a(x) = 12x/13.5</pre>');
  t('string-join(css(''h1 + pre''), ",")', '', '');
  t('string-join(css(''h1 > pre''), ",")', '', '');



  t('string-join(css(''blockquote div > p''), ",")', 'This text should be green.', '<blockquote><div><div><p>This text should be green.</p></div></div></blockquote>'); //89

  t('string-join(css(''p''), ",")', 'This line should have a green background.', '  <p title="hello world">This line should have a green background.</p>'); //7b
  t('string-join(css(''[title~="hello world"]''), ",")', '', '');

  t('string-join(css(''p[class~="b"]''), ",")', 'This paragraph should have green background because CLASS contains b', '<p class="a b c">This paragraph should have green background because CLASS contains b</p><address title="tot foo bar"><span class="a c">This address should also</span>  <span class="a bb c">have green background because the selector in the last rule does not apply to the inner SPANs.</span></address>');
  t('string-join(css(''address[title~="foo"]''), ",")', 'This address should alsohave green background because the selector in the last rule does not apply to the inner SPANs.', '');
  t('string-join(css(''span[class~="b"] ''), ",")', '', '');

  t('string-join(css(''a,c''), ",")', '1,3,1b', '<x><a>1</a><b>2</b><c>3</c><a>1b</a><b>2b</b></x>');
  t('string-join(css(''a,c,b''), ",")', '1,2,3,1b,2b', '');
  t('string-join(css(''a , c''), ",")', '1,3,1b', '');
  t('string-join(css(''a, c''), ",")', '1,3,1b', '');
  t('string-join(css(''a ,c''), ",")', '1,3,1b', '');
  t('string-join(css('',a''), ",")', '', '');
  t('string-join(css(''a,''), ",")', '1,1b', ''); //TODO: how to handle invalid selectors?
  t('string-join(css('',a,''), ",")', '', '');


  //form extension method
  t('', '', '!<html><form action="abc" method="POST"><input name="foo" value="bar"/><input name="X" value="123" type="unknown"/><input name="Y" value="456" type="checkbox" checked/><input name="Z" value="789" type="checkbox"/><button name="btn" value="fu"/></form>'
                + '<form action="abc22"><input name="foo2" value="bar2"/><input name="X" value="123" type="unknown"/><input name="Y" value="456" type="checkbox" checked/><input name="Z" value="789" type="checkbox"/></form>'
                + '<form action="next/haus/bimbam?k=y"><input name="T" value="Z"/><textarea name="fy">ihl</textarea></form>'
                + '<form action="multi" enctype="multipart/form-data" method="POST"><input name="foo" value="bar"/><input name="X" value="123" type="file"/><input name="Y" value="456" type="checkbox" checked/><input name="Z" value="789" type="checkbox"/></form>'
                + '</html>');
  t('form(//form[1]).url', 'pseudo://test/abc', '');
  t('form(//form[1]).headers', '', '');
  t('form(//form[1]).method', 'POST', '');
  t('form(//form[1]).post', 'foo=bar&X=123&Y=456', '');
  t('($f := form(//form[1], "foo=override")).post', 'foo=override&X=123&Y=456', '');
  t('form-combine($f, "foo=cat").post', 'foo=cat&X=123&Y=456', '');
  t('form(//form[1], "Y=override2&Z=override3&Z=override4").post', 'foo=bar&X=123&Y=override2&Z=override3&Z=override4', '');
  t('form(//form[1], "foo=override&Y=override2&Z=override3&Z=override4").post', 'foo=override&X=123&Y=override2&Z=override3&Z=override4', '');

  t('form(//form[1], {"foo": "override", "Y": "override2", "Z": ("override3", "override4")}).post', 'foo=override&X=123&Y=override2&Z=override3&Z=override4', '');
  t('form(//form[1], ({"foo": "override", "Y": "override2", "Z": "override3"}, "Z=override4")).post', 'foo=override&X=123&Y=override2&Z=override3&Z=override4', '');
  t('form(//form[1], ({"Z": ()}, {"foo": "override", "Y": "override2", "Z": "override3"}, "Z=override4", {"Z": ("override5", "override6")})).post', 'foo=override&X=123&Y=override2&Z=override3&Z=override4&Z=override5&Z=override6', '');
  t('form(//form[1], "foo=over%12&ride&Y=override2&Z=override3&Z=override4").post', 'foo=over%12&X=123&Y=override2&ride=&Z=override3&Z=override4', '');
  t('form(//form[1], {"foo": "over%&ride", "Y": "override 2", "Z": ("override3", "override4")}).post', 'foo=over%25%26ride&X=123&Y=override+2&Z=override3&Z=override4', '');
  t('form(//form[1], //form[1]//button).post', 'foo=bar&X=123&Y=456&btn=fu', '');
  t('form(//form[1], {"foo": (), "X": (), "Y": ()}).post', '');
  t('form(//form[1], ({"foo": (), "X": (), "Y": ()}, "Y=1b&X=1a&Y=2b&X=2a")).post', 'X=1a&Y=1b&Y=2b&X=2a');
  t('form(//form[1], ({"foo": (), "X": (7,8), "Y": (9,10,11)}, "Y=1b&X=1a&Y=2b&X=2a")).post', 'X=7&Y=9&X=8&Y=10&Y=11&Y=1b&X=1a&Y=2b&X=2a');
  t('form(//form[1], ({"foo": {}, "X": (7,8), "Y": (9,10,11)})).post', 'foo=bar&X=7&Y=9&X=8&Y=10&Y=11');
  t('form(//form[1], ({"foo": {"value": "def"}, "X": (7,8), "Y": (9,10,11)})).post', 'foo=def&X=7&Y=9&X=8&Y=10&Y=11');

  t('form(//form[1], {"foo": {"x": 1, "y": 2}, "X": (), "Y": ()}).post', 'foo=bar&foo.x=1&foo.y=2'); //or should it override foo?


  t('form(//form[2]).url', 'pseudo://test/abc22?foo2=bar2&X=123&Y=456', '');
  t('form(//form[2]).method', 'GET', '');
  t('form(//form[2]).post', '', '');
  t('form(//form[2], "tt=tttt").url', 'pseudo://test/abc22?foo2=bar2&X=123&Y=456&tt=tttt', '');
  t('form(//form[2], ("tt=tttt", "foo2=maus")).url', 'pseudo://test/abc22?foo2=maus&X=123&Y=456&tt=tttt', '');

  t('count(form(//form))', '4', '');
  t('form(//form)[1].url', 'pseudo://test/abc', '');
  t('form(//form)[2].url', 'pseudo://test/abc22?foo2=bar2&X=123&Y=456', '');
  t('form(//form)[3].url', 'pseudo://test/next/haus/bimbam?k=y&T=Z&fy=ihl', '');

  t('count(form(()))', '0', '');
  t('count(form((), "x=y"))', '0', '');
  t('count(form())', '4', '');
  t('count(form("Y=..."))', '3', '');
  f('count(form("Yxxxxxxxxxx=..."))', 'XPDY0002');
  t('form("foo2=hi").url', 'pseudo://test/abc22?foo2=hi&X=123&Y=456', '');
  t('form("T=hi").url', 'pseudo://test/next/haus/bimbam?k=y&T=hi&fy=ihl', '');
  t('join(form().url)', 'pseudo://test/abc pseudo://test/abc22?foo2=bar2&X=123&Y=456 pseudo://test/next/haus/bimbam?k=y&T=Z&fy=ihl pseudo://test/multi', '');
  t('(//form[1]/form()).url', 'pseudo://test/abc', '');
  t('(//form[2]/form()).url', 'pseudo://test/abc22?foo2=bar2&X=123&Y=456', '');
  t('(//form[3]/form()).url', 'pseudo://test/next/haus/bimbam?k=y&T=Z&fy=ihl', '');
  t('(//form[2]/form("X=xyz")).url', 'pseudo://test/abc22?foo2=bar2&X=xyz&Y=456', '');
  f('(//form[3]/form("X=xyz")).url', 'XPDY0002');
  t('(//form[3]/form("fy=xyz")).url', 'pseudo://test/next/haus/bimbam?k=y&T=Z&fy=xyz', '');
  t('(//form[3]/form({"fy": "xyz"})).url', 'pseudo://test/next/haus/bimbam?k=y&T=Z&fy=xyz', '');

  baseboundary := '---------------------------1212jhjg2ypsdofx0235p2z5as09';
  t('form(//form[4]).headers', 'Content-Type: multipart/form-data; boundary='+baseboundary);
  t('($f := form(//form[4])).post', #13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="foo"'#13#10#13#10'bar'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="Y"'#13#10#13#10'456'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09--');
  t('form-combine($f, {"foo": 17}).post', #13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="foo"'#13#10#13#10'17'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="Y"'#13#10#13#10'456'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09--');
  t('form-combine($f, {"foo": {"value": 17, "headers": "Content-Type: image/png"}}).post', #13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="foo"'#13#10'Content-Type: image/png'#13#10#13#10'17'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="Y"'#13#10#13#10'456'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09--');
  RandSeed:=123;
  t('serialize-json(request-decode($f))', '{"method": "POST", "headers": "Content-Type: multipart/form-data; boundary=---------------------------1212jhjg2ypsdofx0235p2z5as09", "post": "\r\n-----------------------------1212jhjg2ypsdofx0235p2z5as09\r\nContent-Disposition: form-data; name=\"foo\"\r\n\r\nbar\r\n-----------------------------1212jhjg2ypsdofx0235p2z5as09\r\nContent-Disposition: form-data; name=\"Y\"\r\n\r\n456\r\n-----------------------------1212jhjg2ypsdofx0235p2z5as09--", "url": "pseudo://test/multi", "protocol": "pseudo", "host": "test", "params": {"foo": "bar", "Y": "456"}}');
  randomboundary := baseboundary + TMIMEMultipartData.randomLetter;
  RandSeed:=123;
  t('form(//form[4], {"foo": "---------------------------1212jhjg2ypsdofx0235p2z5as09", "t": {"value": 17123, "filename": "xyz"}}).post', #13#10'--'+randomboundary+''#13#10'Content-Disposition: form-data; name="foo"'#13#10#13#10'---------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'--'+randomboundary+''#13#10'Content-Disposition: form-data; name="Y"'#13#10#13#10'456'#13#10'--'+randomboundary+''#13#10'Content-Disposition: form-data; name="t"; filename="xyz"'#13#10#13#10'17123'#13#10'--'+randomboundary+'--');
  t('($f := form(//form[4], {"foo": {"type": "text/html"}, "Y": {"headers": ("a: 1", "b: 2")}, "new": {"type": "text/html", "headers": "Content-Type: override"}})).post', #13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="foo"'#13#10'Content-Type: text/html'#13#10#13#10'bar'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="Y"'#13#10'a: 1'#13#10'b: 2'#13#10#13#10'456'#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09'#13#10'Content-Disposition: form-data; name="new"'#13#10'Content-Type: override'#13#10#13#10#13#10'-----------------------------1212jhjg2ypsdofx0235p2z5as09--');
  RandSeed:=123;
  t('($g := form-combine($f, {"Y": "'+baseboundary+'"})).headers', 'Content-Type: multipart/form-data; boundary='+randomboundary);
  t('$g.post', #13#10'--'+randomboundary+#13#10'Content-Disposition: form-data; name="foo"'#13#10'Content-Type: text/html'#13#10#13#10'bar'#13#10'--'+randomboundary+#13#10'Content-Disposition: form-data; name="Y"'#13#10'a: 1'#13#10'b: 2'#13#10#13#10+baseboundary+#13#10'--'+randomboundary+#13#10'Content-Disposition: form-data; name="new"'#13#10'Content-Type: override'#13#10#13#10#13#10'--'+randomboundary+'--');

  t('form(//form).url', 'abs://hallo?abc=cba', '!<html><form action="abs://hallo"><input name="abc" value="cba"/></form></html>');
  t('form(//form).url', 'abs://foo/bar?abcdef=on', '!<html><form action="abs://foo/bar"><input name="abcdef" type="checkbox" checked/></form></html>');

  t('serialize-json(form-combine({"url": "http://foo/?x=y", "charset": "utf-8"}, {"ä": "ü"}))', '{"url": "http://foo/?x=y&%C3%A4=%C3%BC", "charset": "utf-8"}');
  t('serialize-json(form-combine({"url": "http://foo/?x=y", "charset": "latin1"}, {"ä": "ü"}))', '{"url": "http://foo/?x=y&%E4=%FC", "charset": "latin1"}');
  t('serialize-json(form-combine({"url": "http://foo/?x=y", "charset": "cp1252"}, {"ä": "ü"}))', '{"url": "http://foo/?x=y&%E4=%FC", "charset": "cp1252"}');
  t('serialize-json(form-combine({"url": "http://foo/?x=y", "method": "POST", "post": "a=b", "charset": "utf-8"}, {"ä": "ü"}))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a=b&%C3%A4=%C3%BC", "charset": "utf-8"}');
  t('serialize-json(form-combine({"url": "http://foo/?x=y", "method": "POST", "post": "a=b", "charset": "latin1"}, {"ä": "ü"}))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a=b&%E4=%FC", "charset": "latin1"}');

  //(request-combine supersedes form-combine)
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, {"a[]": "b"}))', '{"url": "http://foo/?x=y&a%5B%5D=b"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, ({"a[]": "b"},"c=d")))', '{"url": "http://foo/?x=y&a%5B%5D=b&c=d"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, ({"a[]": "b"},{"a2[]": "b2"})))', '{"url": "http://foo/?x=y&a%5B%5D=b&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, ({"a[]": "b"},"c=d",{"a2[]": "b2"})))', '{"url": "http://foo/?x=y&a%5B%5D=b&c=d&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, ({"a[]": "b","a3[]": "b3"},"c=d",{"a2[]": "b2"})))', '{"url": "http://foo/?x=y&a%5B%5D=b&a3%5B%5D=b3&c=d&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y"}, ({"a[]": "b"},"c=d",{"a2[]": "b2"},"e=f","g=h")))', '{"url": "http://foo/?x=y&a%5B%5D=b&c=d&a2%5B%5D=b2&e=f&g=h"}');

  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, {"a[]": "b"}))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, ({"a[]": "b"},"c=d")))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b&c=d"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, ({"a[]": "b"},{"a2[]": "b2"})))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, ({"a[]": "b"},"c=d",{"a2[]": "b2"})))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b&c=d&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, ({"a[]": "b","a3[]": "b3"},"c=d",{"a2[]": "b2"})))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b&a3%5B%5D=b3&c=d&a2%5B%5D=b2"}');
  t('serialize-json(request-combine({"url": "http://foo/?x=y", "method": "POST"}, ({"a[]": "b"},"c=d",{"a2[]": "b2"},"e=f","g=h")))', '{"url": "http://foo/?x=y", "method": "POST", "post": "a%5B%5D=b&c=d&a2%5B%5D=b2&e=f&g=h"}');

  t('serialize-json(request-combine("http://www.abc.de", "a=b"))', '{"url": "http://www.abc.de/?a=b"}');
  t('serialize-json(request-combine("http://www.abc.de#abc", "a=b"))', '{"url": "http://www.abc.de/?a=b"}');
  t('serialize-json(request-combine("http://www.abc.de", ("a=b", "d=e")))', '{"url": "http://www.abc.de/?a=b&d=e"}');
  t('serialize-json(request-combine("http://www.abc.de/", "a=b"))', '{"url": "http://www.abc.de/?a=b"}');
  t('request-combine("http://www.abc.de/#abc", "a=b").url', 'http://www.abc.de/?a=b');
  t('request-combine("http://www.abc.de/x", "a=b").url', 'http://www.abc.de/x?a=b');
  t('request-combine("https://www.abc.de/x#abc", "a=b").url', 'https://www.abc.de/x?a=b');
  t('request-combine("https://www.abc.de/y/", "a=b").url', 'https://www.abc.de/y/?a=b');
  t('request-combine("http://www.abc.de/?", "a=b").url', 'http://www.abc.de/?a=b');
  t('request-combine("http://www.abc.de/?x=y", "a=b").url', 'http://www.abc.de/?x=y&a=b');
  t('request-combine("http://www.abc.de?x=y", "a=b").url', 'http://www.abc.de?x=y&a=b'); //does this make sense ?
  t('request-combine("https://www.abc.de/y/?x=y", "a=b").url', 'https://www.abc.de/y/?x=y&a=b');
  t('request-combine("http://www.abc.de/?x=y", "x=z").url', 'http://www.abc.de/?x=z');
  t('request-combine("http://www.abc.de?x=y", "x=z").url', 'http://www.abc.de?x=z');
  t('request-combine("https://www.abc.de/y/?x=y", "x=z").url', 'https://www.abc.de/y/?x=z');

  t('request-combine("", "a=b").url', '?a=b');
  t('request-combine("?", "a=b").url', '?a=b');
  t('request-combine("#abc", "a=b").url', '?a=b');
  t('request-combine("x", "a=b").url', 'x?a=b');
  t('request-combine("/#abc", "a=b").url', '/?a=b');
  t('request-combine("/x", "a=b").url', '/x?a=b');
  t('request-combine("/x#abc", "a=b").url', '/x?a=b');
  t('request-combine("/y/", "a=b").url', '/y/?a=b');
  t('request-combine("/?", "a=b").url', '/?a=b');
  t('request-combine("/?x=y", "a=b").url', '/?x=y&a=b');
  t('request-combine("?x=y", "a=b").url', '?x=y&a=b');
  t('request-combine("/y/?x=y", "a=b").url', '/y/?x=y&a=b');
  t('request-combine("/y/?x=y", "x=z").url', '/y/?x=z');

  t('serialize-json(request-decode("http://a/b/c"))', '{"url": "http://a/b/c", "protocol": "http", "host": "a"}');
  t('serialize-json(request-decode("http://a/b/c?x=y"))', '{"url": "http://a/b/c?x=y", "protocol": "http", "host": "a", "path": "b/c", "query": "x=y", "params": {"x": "y"}}');
  t('serialize-json(request-decode("http://a/b/c?x=y&d=e"))', '{"url": "http://a/b/c?x=y&d=e", "protocol": "http", "host": "a", "path": "b/c", "query": "x=y&d=e", "params": {"x": "y", "d": "e"}}');
  t('join(request-decode("http://a/b/c?x=y&d=e&d=f").params.d)', 'e f');
  t('serialize-json(request-decode("http://a:123/b/c?x=y+f&d=e&e=f#9"))', '{"url": "http://a:123/b/c?x=y+f&d=e&e=f#9", "protocol": "http", "host": "a", "port": "123", "path": "b/c", "query": "x=y+f&d=e&e=f", "target": "9", "params": {"x": "y f", "d": "e", "e": "f"}}');
  t('join(request-decode("http://a/b/c?x=y&d=e&d=f").params.d)', 'e f');
  t('serialize-json(request-decode({"url": "http://x/y", "post": "f=g", "method": "POST"}))', '{"url": "http://x/y", "post": "f=g", "method": "POST", "protocol": "http", "host": "x", "params": {"f": "g"}}');




  t('join(for $r in (random(), random(), random()) return if ($r >= 0 and $r < 1) then "O" else "F")', 'O O O');
  t('join(for $r in (random(10), random(10), random(10.5)) return if ($r >= 0 and $r < 10) then "O" else "F")', 'O O O');
  t('(random(), random-seed(123), random())[last()]  = (random-seed(123), random())[last()]', 'true');


  //Newer tests

  t('QName("", "x") eq QName("","x")', 'true');
  t('QName("", "x") = QName("","x")', 'true');
  t('QName("xml", "x") eq QName("xml","x")', 'true');
  t('QName("xml", "x") eq QName("xml","x")', 'true');
  t('QName("", "x") eq QName("","y")', 'false');
  t('QName("", "x") = QName("","y")', 'false');
  t('QName("xml", "x") eq QName("xml","y")', 'false');
  t('QName("xml", "x") eq QName("xml","y")', 'false');
  t('QName("", "x") eq QName("xml","x")', 'false');
  t('QName("", "x") = QName("xml","x")', 'false');
  t('QName("xml", "x") eq QName("xs","x")', 'false');
  t('QName("xml", "x") eq QName("xs","x")', 'false');
  t('QName("", "x") ne QName("xml","x")', 'true');

  //test attributes as nodes
  t('', '', '!<test><a att1="v1" att2="v2" att3="v3" att4="v4" foo="bar">a node</a>TEST</test>');
  t('count(//a/@*)', '5', '');
  t('(//a/@*/..)', 'a node', '');
  t('(//a/@*/../@att1)', 'v1', '');
  t('(//a/@*/../@att1/..)', 'a node', '');
  t('(//a/@*/../attribute::att2/..)', 'a node', '');
  t('(//a/@*[node-name(.) = QName("","att2") ])', 'v2', '');
  t('(//a/@*[local-name() = "att3"])', 'v3', '');
  t('(//a/@*[local-name(.) = "att4"])', 'v4', '');
  t('(//a/@*[name(.) = "att1"])', 'v1', '');

  t('string-join(//a/@att2/descendant::*, ",")', '', '');
  t('string-join(//a/@att2/attribute::*, ",")', '', '');
  t('string-join(//a/@att2/self::*, ",")', 'v2', '');
  t('string-join(//a/@att2/descendant-or-self::*, ",")', 'v2', '');
  t('string-join(//a/@att2/following-sibling::*, ",")', '', '');
  t('string-join(//a/@att2/following::*, ",")', '', '');

  t('string-join(//a/@att2/parent::*, ",")', 'a node', '');
  t('string-join(//a/@att2/ancestor::*, ",")', 'a nodeTEST,a node', '');
  t('string-join(//a/@att2/preceding-sibling::*, ",")', '', '');
  t('string-join(//a/@att2/preceding::*, ",")', '', '');
  t('string-join(//a/@att2/ancestor-or-self::*, ",")', 'a nodeTEST,a node,v2', '');

  t('string-join(//@*, ",")', 'xyz', '!<a init="xyz"><?foobar maus="123" haus="456"?></a>');
  t('string-join(//processing-instruction(), ",")', 'maus="123" haus="456"', '');
  t('string-join(//processing-instruction(), ",")', '  maus="123" haus="456"    ', '!<a init="xyz"><?foobar   maus="123" haus="456"    ?></a>');

  //Int 65 math
  t('9223372036854775807 + 1', '9223372036854775808', '');
  t('9223372036854775807 - 1', '9223372036854775806', '');
  t('9223372036854775808 - 1', '9223372036854775807', '');
  t('18446744073709551615', '18446744073709551615', '');
  t('9223372036854775807 - 18446744073709551615', '-9223372036854775808', '');
  t('- 18446744073709551615', '-18446744073709551615', '');
  t('int(5)' ,'5', '');
  t('type-of(xs:decimal(6) idiv xs:integer(2))', 'integer', '');
  t('min((-9223372036854775807, 9223372036854775807))', '-9223372036854775807', '');
  t('max((-9223372036854775807, 9223372036854775807))', '9223372036854775807', '');
  t('min((-18446744073709551615, 18446744073709551615))', '-18446744073709551615', '');
  t('max((-18446744073709551615, 18446744073709551615))', '18446744073709551615', '');
  t('xs:long("9223372036854775807")', '9223372036854775807');
  t('xs:long("-9223372036854775808")', '-9223372036854775808');
  t('"9223372036854775807" castable as xs:long', 'true');
  t('"-9223372036854775808" castable as xs:long', 'true');
  t('"9223372036854775808" castable as xs:long', 'false');
  t('"-9223372036854775809" castable as xs:long', 'false');

  t('string-join(//b, ",")', 'A,B,C', '!<a take="1"><b id="1">A</b><b id="2">B</b><b id="3">C</b></a>');
  t('string-join(//b[@id=/a/@take], ",")', 'B', '!<a take="2"><b id="1">A</b><b id="2">B</b><b id="3">C</b></a>');
  t('string-join(//b[/a/@take=@id], ",")', 'B', '!<a take="2"><b id="1">A</b><b id="2">B</b><b id="3">C</b></a>');

  //some namespaces
  ps.GlobalNamespaces.Add(TNamespace.Create('test://a', 'a'));
  ps.GlobalNamespaces.Add(TNamespace.Create('test://b', 'b'));
  t('string-join(/r/b, ",")',   'xB', '!<r xmlns:a="test://a" xmlns:b="test://b"><a:b>AB</a:b><b>xB</b><a:c>AC</a:c><b:a>BA</b:a><b:b>BB</b:b></r>');
  t('string-join(/r/a:b, ",")', 'AB', '');
  t('string-join(/r/b:b, ",")', 'BB', '');
  t('string-join(/r/*:b, ",")', 'AB,xB,BB', '');
  t('string-join(/r/b:*, ",")', 'BA,BB', '');
  t('string-join(/r/a:*, ",")', 'AB,AC', '');
  t('string-join(/r/a, ",")', '', '');
  t('string-join(/r/*:a, ",")', 'BA', '');
  t('string-join(/r/*, ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/element(), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/element(*), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/element(a:b), ",")', 'AB', '');

  t('string-join(/r/child::b, ",")', 'xB', '');
  t('string-join(/r/child::a:b, ",")', 'AB', '');
  t('string-join(/r/child::b:b, ",")', 'BB', '');
  t('string-join(/r/child::*:b, ",")', 'AB,xB,BB', '');
  t('string-join(/r/child::b:*, ",")', 'BA,BB', '');
  t('string-join(/r/child::a:*, ",")', 'AB,AC', '');
  t('string-join(/r/child::a, ",")', '', '');
  t('string-join(/r/child::*:a, ",")', 'BA', '');
  t('string-join(/r/child::*, ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/child::element(), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/child::element(*), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/child::element(a:b), ",")', 'AB', '');

  t('string-join(/r/x/b, ",")',   '', '!<r xmlns:a="test://a" xmlns:b="test://b"><x a:b="AB" b="xB" a:c="AC" b:a="BA" b:b="BB"/></r>');
  t('string-join(/r/x/a:b, ",")', '', '');
  t('string-join(/r/x/@b, ",")',   'xB', '');
  t('string-join(/r/x/@a:b, ",")',   'AB', '');
  t('string-join(/r/x/@b:b, ",")', 'BB', '');
  t('string-join(/r/x/@*:b, ",")', 'AB,xB,BB', '');
  t('string-join(/r/x/@b:*, ",")', 'BA,BB', '');
  t('string-join(/r/x/@a:*, ",")', 'AB,AC', '');
  t('string-join(/r/x/@a, ",")', '', '');
  t('string-join(/r/x/@*:a, ",")', 'BA', '');
  t('string-join(/r/x/@*, ",")', 'AB,xB,AC,BA,BB', '');

  t('string-join(/r/x/attribute::b, ",")',   'xB', '');
  t('string-join(/r/x/attribute::a:b, ",")',   'AB', '');
  t('string-join(/r/x/attribute::b:b, ",")', 'BB', '');
  t('string-join(/r/x/attribute::*:b, ",")', 'AB,xB,BB', '');
  t('string-join(/r/x/attribute::b:*, ",")', 'BA,BB', '');
  t('string-join(/r/x/attribute::a:*, ",")', 'AB,AC', '');
  t('string-join(/r/x/attribute::a, ",")', '', '');
  t('string-join(/r/x/attribute::*:a, ",")', 'BA', '');
  t('string-join(/r/x/attribute::*, ",")', 'AB,xB,AC,BA,BB', '');

  t('string-join(/r/x/attribute(), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/x/attribute(*), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/x/attribute(a:b), ",")', 'AB', '');
  t('string-join(/r/x/attribute::attribute(), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/x/attribute::attribute(*), ",")', 'AB,xB,AC,BA,BB', '');
  t('string-join(/r/x/attribute::attribute(a:b), ",")', 'AB', '');

  t('join(for $i in (1, 2), $j in (3, 4) return ($i, $j), ":")', '1:3:1:4:2:3:2:4');

  //More failed XQTS tests
  //t('xs:untypedAtomic("-10000000") cast as xs:float', '-10000000', ''); this is an fpc bug! (22567)
  t('1*/', '3', '!<a><b>0</b><b>0</b><b>3</b></a>');
  t('/a/b[1*/]', '3', '');
  t('/a/b[-1+/]', '0', '');
  t('xs:long("-92233720368547758") idiv xs:long("-92233720368547758")', '1', '');
  t('xs:long("92233720368547758") idiv xs:long("-92233720368547758")', '-1', '');
  t('xs:long("-92233720368547758") idiv xs:long("92233720368547758")', '-1', '');
  t('xs:long("92233720368547758") idiv xs:long("92233720368547758")', '1', '');
  t('join(92233720368547757 to 92233720368547759, ":")', '92233720368547757:92233720368547758:92233720368547759', '');
  t('join(-92233720368547759 to -92233720368547757, ":")', '-92233720368547759:-92233720368547758:-92233720368547757', '');
  t('join(-3 to 3, ":")', '-3:-2:-1:0:1:2:3', '');
  t('concat(-9223372036854775808 instance of xs:integer, " ", 9223372036854775808 instance of xs:integer, " ", +9223372036854775808 instance of xs:integer)', 'true true true');
  t('join( for $min in -9223372036854775808, $max in 9223372036854775807 return (($min - 1) to $min, " ",$min to ($min + 1),  " ", ($max - 1) to $max, " ",$max to ($max + 1) ), ":")',
    '-9223372036854775809:-9223372036854775808: :-9223372036854775808:-9223372036854775807: :9223372036854775806:9223372036854775807: :9223372036854775807:9223372036854775808', ''); //my test
  //rounding
  t('type-of(ceiling(1.5e20))', 'double');
  t('ceiling(1.5e20)', '1.5E20');
  t('type-of(ceiling(xs:float(1.5e20)))', 'float');
  t('fn:ceiling(xs:float("-3.4028235E38")) instance of xs:float', 'true');
  t('fn:ceiling(xs:float("-3.4028235E38"))', '-3.4028235E38');
  t('fn:floor(xs:float("-3.4028235E38"))', '-3.4028235E38');
  t('fn:abs(xs:float("-3.4028235E38"))', '3.4028235E38');
  t('fn:abs(xs:float("-3.4028235E38"))', '3.4028235E38');
  t('fn:round(xs:float("-3.4028235E38"))', '-3.4028235E38');
  t('fn:round-half-to-even(xs:float("-3.4028235E38"))', '-3.4028235E38');
  for j := 1 to 4 do begin
    case j of
      1: tt := 'integer';
      2: tt := 'decimal';
      3: tt := 'double';
      4: tt := 'byte';
    end;
    t('fn:round-half-to-even(xs:'+tt+'(10), 0)', '10');
    t('fn:round-half-to-even(xs:'+tt+'(10), -1)', '10');
    t('fn:round-half-to-even(xs:'+tt+'(14), -1)', '10');
    t('fn:round-half-to-even(xs:'+tt+'(15), -1)', '20');
    t('fn:round-half-to-even(xs:'+tt+'(24), -1)', '20');
    t('fn:round-half-to-even(xs:'+tt+'(25), -1)', '20');
    t('fn:round-half-to-even(xs:'+tt+'(26), -1)', '30');
    t('fn:round-half-to-even(xs:'+tt+'(124), -1)', '120');
    t('fn:round-half-to-even(xs:'+tt+'(125), -1)', '120');
    t('fn:round-half-to-even(xs:'+tt+'(126), -1)', '130');
    t('fn:round-half-to-even(xs:'+tt+'(-124), -1)', '-120');
    t('fn:round-half-to-even(xs:'+tt+'(-125), -1)', '-120');
    t('fn:round-half-to-even(xs:'+tt+'(-126), -1)', '-130');
    t('fn:round-half-to-even(xs:'+tt+'(126), 1)', '126');
    t('fn:round-half-to-even(xs:'+tt+'(-126), 1)', '-126');
    t('fn:round-half-to-even(xs:'+tt+'(10), -2)', '0');
  end;

  t('fn:round-half-to-even(10.3, -5000)', '0');
  t('fn:round-half-to-even(10.3, 5000)', '10.3');
  t('fn:round-half-to-even(10.3, -999999999999)', '0');
  t('fn:round-half-to-even(10.3, 999999999999)', '10.3');
  t('fn:round-half-to-even(10, -2)', '0');
  t('fn:round-half-to-even(xs:float("3.4028235E38"), -38)', '3.0E38');
  t('fn:round-half-to-even(xs:float("-3.4028235E38"), -38)', '-3.0E38');
  t('fn:round-half-to-even(xs:double("3.4028235E38"), -38)', '3.0E38');
  t('fn:round-half-to-even(xs:double("-3.4028235E38"), -38)', '-3.0E38');
  t('fn:round-half-to-even(xs:double("3.5E38"), -38)', '4.0E38');
  t('fn:round-half-to-even(xs:double("-3.5E38"), -38)', '-4.0E38');
  t('fn:round-half-to-even(10.344, 2)', '10.34');
  t('fn:round-half-to-even(10.345, 2)', '10.34');
  t('fn:round-half-to-even(10.346, 2)', '10.35');
  t('fn:round-half-to-even(10.354, 2)', '10.35');
  t('fn:round-half-to-even(10.355, 2)', '10.36');
  t('fn:round-half-to-even(10.356, 2)', '10.36');
  t('fn:round-half-to-even(-10.344, 2)', '-10.34');
  t('fn:round-half-to-even(-10.345, 2)', '-10.34');
  t('fn:round-half-to-even(-10.346, 2)', '-10.35');
  t('fn:round-half-to-even(-10.354, 2)', '-10.35');
  t('fn:round-half-to-even(-10.355, 2)', '-10.36');
  t('fn:round-half-to-even(-10.356, 2)', '-10.36');
  t('fn:round-half-to-even(-12550, -2)', '-12600');
  t('xs:float(12345.6)', '12345.6');
  t('fn:round-half-to-even(xs:float(12345.6), 2)', '12345.6');
  t('fn:round(xs:nonNegativeInteger("303884545991464527"))', '303884545991464527');
  //node comparison
  t('.//@att', 'xyz', '<a><b><c att="xyz">C</c></b></a>');
  t('outer-xml(root(.//c/@att))', '<a><b><c att="xyz">C</c></b></a>');
  t('outer-xml(root(.//@att))', '<a><b><c att="xyz">C</c></b></a>');
  t('string-join(.//c except .//d, ":")', 'C');
  t('string-join(.//c except .//c/@att, ":")', 'C');
  t('string-join(.//d except .//c, ":")', '');
  //type checking
  t('1.2 eq xs:float(1.2)', 'true');
  tdouble('1.2 eq xs:double(1.2)', 'true');
  t('xs:float(1.2) eq xs:double(1.2)', 'false'); //??? Zorba agrees
  t('1.2 eq xs:float(1.3)', 'false');
  t('1.2 eq xs:double(1.3)', 'false');
  t('xs:float(1.2) eq xs:double(1.3)', 'false');
  t('xs:float(1.2) < 1.2', 'false');
  t('xs:float(1.2) <= 1.2', 'true');
  t('xs:float(1.2) = 1.2', 'true');
  t('xs:float(1.2) >= 1.2', 'true');
  t('xs:float(1.2) > 1.2', 'false');
  t('xs:float(1.2) < xs:double("INF")', 'true');
  t('xs:float(1.2) <= xs:double("INF")', 'true');
  t('xs:float(1.2) = xs:double("INF")', 'false');
  t('xs:float(1.2) >= xs:double("INF")', 'false');
  t('xs:float(1.2) > xs:double("INF")', 'false');
  t('xs:float(-1.2) < xs:double("INF")', 'true');
  t('xs:float(-1.2) <= xs:double("INF")', 'true');
  t('xs:float(-1.2) = xs:double("INF")', 'false');
  t('xs:float(-1.2) >= xs:double("INF")', 'false');
  t('xs:float(-1.2) > xs:double("INF")', 'false');
  t('123 < xs:untypedAtomic("       INF      ")', 'true');
  t('xs:float("INF") eq xs:double("INF")', 'true');
  t('xs:float("INF") eq xs:double("-INF")', 'false');
  t('xs:float("-INF") eq xs:double("INF")', 'false');
  t('xs:float("-INF") eq xs:double("-INF")', 'true');
  //t('xs:float(1.2) = '+untypedAtomic+'("1.2")', 'true'); rounding error
  tdouble('xs:double(1.2) = '+untypedAtomic+'("1.2")', 'true');
  t('string-join(index-of((0,1,2,3),"1"), ":")', '');
  t('join(distinct-values((1, "1", 2, 2.0)),":")', '1:1:2');
  t('deep-equal(1, current-dateTime())', 'false');
  t('deep-equal((1,2,3), ("1", "2", "3"))', 'false');
  t('xs:yearMonthDuration("P0Y") eq xs:dayTimeDuration("P0D")', 'true');
  t('(xs:duration("P1Y") eq xs:duration("P12M"))', 'true');
  t('(xs:duration("PT24H") eq xs:duration("P1D"))', 'true');
  t('(xs:duration("P1Y") eq xs:duration("P365D"))', 'false');
  t('(xs:yearMonthDuration("P0Y") eq xs:dayTimeDuration("P0D"))', 'true');
  t('(xs:yearMonthDuration("P1Y")eq xs:dayTimeDuration("P365D"))', 'false');
  t('(xs:yearMonthDuration("P2Y")eq xs:yearMonthDuration("P24M"))', 'true');
  t('(xs:dayTimeDuration("P10D")eq xs:dayTimeDuration("PT240H"))', 'true');
  t('(xs:duration("P2Y0M0DT0H0M0S")eq xs:yearMonthDuration("P24M"))', 'true');
  t('(xs:duration("P0Y0M10D")eq xs:dayTimeDuration("PT240H"))', 'true');
  t('join(fn:distinct-values((xs:yearMonthDuration("P0Y"), xs:dayTimeDuration("P0D"))),":")', 'P0M');
  t('index-of(./r/a, "y")', '2', '<r foo="z"><a>X</a><a>Y</a><a>Z</a></r>');
  t('index-of(./r/a, ./r/@foo)', '3');
  t('string-join(distinct-values((./r/a, ./r/@foo)), ":")', 'X:Y:Z');
  t('string-join(distinct-values((./r/@foo, ./r/a)), ":")', 'z:X:Y');
  t('deep-equal(r/a[1], r/a[2])', 'false');
  t('deep-equal(r/a[1], r/a[1])', 'true');
  t('deep-equal(r/a[1], "X")', 'false');
  t('deep-equal("Y", r/a[2])', 'false');
  t('deep-equal(r//a, r//a)', 'true');
  t('deep-equal(r/@foo, r/a[3])', 'false');
  t('', '', '!<attendees> <name last="Parker" first="Peter"/> <name last="Barker" first="Bob"/> <name last="Parker"  first="Peter"/> </attendees>'); //taken from the standard
  t('deep-equal(attendees, attendees/*)', 'false');
  t('deep-equal(attendees, attendees)', 'true');
  t('deep-equal(attendees/name[1], attendees/name[1])', 'true');
  t('deep-equal(attendees/name[1], attendees/name[2])', 'false');
  t('deep-equal(attendees/name[1], attendees/name[3])', 'true');
  t('deep-equal(attendees/name[1], "Peter Parker")', 'false');
  t('deep-equal(attendees/name[1]/@last, "Parker")', 'false');
  t('attendees/name[1]/@last eq "Parker"', 'true');
  t('deep-equal((attendees/name[1],attendees/name[2]), (attendees/name[1],attendees/name[2]))', 'true');
  t('deep-equal((attendees/name[1],attendees/name[2]), (attendees/name[3],attendees/name[2]))', 'true');
  t('deep-equal((attendees/name[3],attendees/name[2]), (attendees/name[3],attendees/name[2]))', 'true');
  t('deep-equal((attendees/name[2],attendees/name[3]), (attendees/name[3],attendees/name[2]))', 'false');
  t('deep-equal(/r/a[1],  /r/a[1])', 'true', '!<r foo="z"><a>1<b>2</b>3</a> <a>1<b>2</b>3</a> <a>1<b>X</b>3</a> <a>1<B>2</B>3</a> <a>1<b x="z">2</b>3</a>  </r>');
  t('deep-equal(/r/a[1],  /r/a[2])', 'true');
  t('deep-equal(/r/a[1],  /r/a[3])', 'false');
  t('deep-equal(/r/a[1],  /r/a[4])', 'false');
  t('deep-equal(/r/a[1],  /r/a[5])', 'false');
  t('deep-equal(/r/a[5],  /r/a[1])', 'false');
  //casting tests
  t('10 cast as xs:integer', '10');
  t('() castable as xs:integer', 'false');
  t('() castable as xs:integer?', 'true');
  t('xs:double(5.1) castable as xs:decimal', 'true');
  t('xs:string("5.1") castable as xs:decimal', 'true');
  t('xs:string("5e1") castable as xs:decimal', 'false');
  t('xs:string("INF") castable as xs:decimal', 'false');
  t('xs:string("INF") castable as xs:double', 'true');
  t('xs:string("-INF") castable as xs:decimal', 'false');
  t('xs:string("-INF") castable as xs:double', 'true');
  t('xs:string("NaN") castable as xs:decimal', 'false');
  t('xs:string("NaN") castable as xs:double', 'true');
  t('xs:string("false") castable as xs:boolean', 'true');
  t('xs:string("true") castable as xs:boolean', 'true');
  t('xs:string("falsex") castable as xs:boolean', 'false');
  t('xs:string("false") cast as xs:boolean', 'false');
  t('xs:string("true") cast as xs:boolean', 'true');
  t('data((1,2))', '12');
  t('(1,2) castable as xs:integer?', 'false');
  //effective boolean value tests (most are duplicates to previous tests)
  t('xs:boolean("true")', 'true');
  t('xs:boolean("false")', 'false');
  t('not("true")', 'false');
  t('not("false")', 'false');
  t('not("")', 'true');
  t('"true" and true()', 'true');
  t('"false" and true()', 'true');
  t('"false" or false()', 'true');
  t('"false" and foobar', 'false');
  t('"" and true', 'false');
  t('"" or false', 'false');
  t('join((1,2,3)["false"], ":")', '1:2:3');
  t('join((1,2,3)[""], ":")', '');
  t('join((1,2,3)["abc"], ":")', '1:2:3');
  t('if ("false") then 1 else 2', '1');
  t('some $x in (1,2,3) satisfies "true"', 'true');
  t('some $x in (1,2,3) satisfies "false"', 'true');
  t('some $x in (1,2,3) satisfies ""', 'false');
  t('every $x in (1,2,3) satisfies "true"', 'true');
  t('every $x in (1,2,3) satisfies "false"', 'true');
  t('every $x in (1,2,3) satisfies ""', 'false');
  t('xs:integer(1.1)', '1');
  t('xs:integer(1.5)', '1');
  t('xs:integer(-1.5)', '-1');
  t('xs:integer(1.9999)', '1');
  t('-1.9999 cast as xs:integer', '-1');
  t('1.9999 cast as xs:integer', '1');
  t('join((-1[0 < .], "|", -t/2, "|", +(-())))', '-1 | -2 |', '<t>7</t>');
  if strictTypeChecking then f('-t/"a"', 'err:XPTY0004');


  //more precise type tests
  t('() * 1', '');
  t('() + 1', '');
  t('() - 1', '');
  t('() div 1', '');
  t('() idiv 1', '');
  t('() mod 1', '');
  t('() to 1', '');
  t('1  * () ', '');
  t('1  + () ', '');
  t('1  -  ()', '');
  t('1  div () ', '');
  t('1  idiv ()  ', '');
  t('1  mod  ()' , '');
  t('1  to  ()' , '');
  t('xs:untypedAtomic("1") * 7', '7');
  t('type-of(xs:untypedAtomic("1e6") * 7.0)', 'double');
  t('type-of(xs:untypedAtomic("1e6") * 7)', 'double');
  t('type-of(xs:untypedAtomic("1.0") * 7)', 'double');
  t('type-of(xs:untypedAtomic("1") * 7)', 'double');
  t('xs:untypedAtomic("1") div 8', '0.125');
  t('type-of(xs:untypedAtomic("1e6") div 8)', 'double');
  t('type-of(xs:untypedAtomic("1.0") div 8)', 'double');
  t('type-of(xs:untypedAtomic("1") div 8)', 'double');
  t('1 div 8', '0.125');
  t('type-of(1 div 8)', 'decimal');
  t('xs:untypedAtomic("1") idiv 8', '0');
  t('type-of(xs:untypedAtomic("1e6") idiv 8)', 'integer');
  t('type-of(xs:untypedAtomic("1.0") idiv 8)', 'integer');
  t('type-of(xs:untypedAtomic("1") idiv 8)', 'integer');
  t('xs:untypedAtomic("1") mod 8', '1');
  t('type-of(xs:untypedAtomic("1e6") mod 8)', 'double');
  t('type-of(xs:untypedAtomic("1.0") mod 8)', 'double');
  t('type-of(xs:untypedAtomic("1") mod 8)', 'double');
  t('xs:untypedAtomic("1") + 7', '8');
  t(untypedAtomic+'("1") + 7', '8');
  t(untypedAtomic+'("1") - 7', '-6');
  if strictTypeChecking then f('xs:string("1") + 7', 'err:XPTY0004');
  t('type-of(xs:untypedAtomic("1e6") + 7)', 'double');
  t('type-of(xs:untypedAtomic("1.0") + 7)', 'double');
  t('type-of(xs:untypedAtomic("1") + 7)', 'double');
  t('xs:untypedAtomic("1") - 7', '-6');
  t('type-of(xs:untypedAtomic("1e6") - 7)', 'double');
  t('type-of(xs:untypedAtomic("1.0") - 7)', 'double');
  t('type-of(xs:untypedAtomic("1") - 7)', 'double');
  t('type-of(1 - 7)', 'integer');
  if strictTypeChecking then begin
    f('1 eq xs:untypedAtomic("1")', 'XPTY0004');
    f('1 eq xs:untypedAtomic("1.0")', 'XPTY0004');
    f('0 eq xs:untypedAtomic("fooo")', 'XPTY0004');
    f('"false" eq false()', 'XPTY0004');
    f('xs:untypedAtomic("true") eq true()', 'XPTY0004');
    f('xs:untypedAtomic("10") eq xs:double("10")', 'XPTY0004');
    f('xs:untypedAtomic("NaN") eq xs:double("NaN")', 'XPTY0004');
  end else begin
    t('1 eq xs:untypedAtomic("1")', 'true');
    t('1 eq xs:untypedAtomic("1.0")', 'false');
    t('0 eq xs:untypedAtomic("fooo")', 'false');
    t('"false" eq false()', 'true');
    t('xs:untypedAtomic("10") eq xs:double("10")', 'true');
    t('xs:untypedAtomic("NaN") eq xs:double("NaN")', 'true');
  end;
  t('1 = xs:untypedAtomic("1")', 'true');
  t('1 = xs:untypedAtomic("1.0")', 'true');
  f('0 = xs:untypedAtomic("foo")', 'FORG0001');
  if strictTypeChecking then begin
    f('1 = "1"', 'XPTY0004');
    f('false() = "false"', 'XPTY0004');
  end else begin
    t('1 = "1"', 'true');
    t('false() = "false"', 'true');
  end;
  t('false() = xs:untypedAtomic("false")', 'true');
  t('true() = xs:untypedAtomic("1")', 'true');
  f('false() = xs:untypedAtomic("falseXXX")', 'FORG0001');
  t('xs:dayTimeDuration("P1D") = xs:untypedAtomic("P0DT23H")', 'false');
  t('xs:dayTimeDuration("P1D") = xs:untypedAtomic("P0DT24H")', 'true');
  t('xs:dayTimeDuration("P1D") < xs:untypedAtomic("P0DT23H")', 'false');
  t('xs:dayTimeDuration("P1D") >= xs:untypedAtomic("P0DT24H")', 'true');
  t('xs:dayTimeDuration("-P1D") >= xs:untypedAtomic("P0DT24H")', 'false');
  t('xs:dayTimeDuration("P1D") != xs:untypedAtomic("P0DT24H")', 'false');
  t('xs:untypedAtomic("false") = false()', 'true');
  t('xs:untypedAtomic("true") = false()', 'false');

  t('/a is ()',  '', '!<a/>');
  t('() is /a ', '');
  t('/a << ()',  '');
  t('() << /a ', '');
  t('/a >> ()',  '');
  t('() >> /a ', '');

  for tempb := false to true do begin
    ps.StaticContext.strictTypeChecking:=tempb;

    f('(0.0 div 0.0)', 'err:FOAR0001');
    f('(0   div 0  )', 'err:FOAR0001');
    f('(0.0 div   0)', 'err:FOAR0001');
    f('(  0 div 0.0)', 'err:FOAR0001');
    f('(0.0 idiv 0.0)', 'err:FOAR0001');
    f('(0   idiv 0  )', 'err:FOAR0001');
    f('(0.0 idiv   0)', 'err:FOAR0001');
    f('(  0 idiv 0.0)', 'err:FOAR0001');

    t('(  0 div 0e1)', 'NaN');
    t('(0.0 div 0e1)', 'NaN');
    t('(0e1 div 0  )', 'NaN');
    t('(0e1 div 0.0)', 'NaN');

    for j := 0 to 1 do begin
      if j = 0 then tt := 'xs:string' else tt := 'xs:untypedAtomic';
      if UsingFLRE then t('xs:NCName("Foobar") cast as ' + tt, 'Foobar');
      t('xs:anyURI("http://www.example.org/äöü !") cast as ' + tt, 'http://www.example.org/äöü !');
      t('xs:QName("xml:foo") cast as ' + tt, 'xml:foo');
      t('xs:integer(123) cast as ' + tt, '123');
      t('xs:decimal(0.00001) cast as ' + tt, '0.00001');
      tdouble('xs:double(0.00001) cast as ' + tt, '0.00001');
      t('xs:float(0.00001) cast as ' + tt, '0.00001');
      t('xs:decimal(0.000001) cast as ' + tt, '0.000001');
      tdouble('xs:double(0.000001) cast as ' + tt, '0.000001');
      t('xs:float(0.000001) cast as ' + tt, '0.000001');
      t('xs:decimal(0.0000001) cast as ' + tt, '0.0000001');
      tdouble('xs:double(0.0000001) cast as ' + tt, '1.0E-7');
      t('xs:float(0.0000001) cast as ' + tt, '1.0E-7');
      t('xs:decimal(100000) cast as ' + tt, '100000');
      t('xs:double(100000) cast as ' + tt, '100000');
      t('xs:float(100000) cast as ' + tt, '100000');
      t('xs:decimal(1000000) cast as ' + tt, '1000000');
      t('xs:double(1000000) cast as ' + tt, '1.0E6');
      t('xs:float(1000000) cast as ' + tt, '1.0E6');
      t('xs:decimal(10000000) cast as ' + tt, '10000000');
      t('xs:double(10000000) cast as ' + tt, '1.0E7');
      t('xs:float(10000000) cast as ' + tt, '1.0E7');

      t('xs:decimal(-0.000001) cast as ' + tt, '-0.000001');
      tdouble('xs:double(-0.000001) cast as ' + tt, '-0.000001');
      t('xs:float(-0.000001) cast as ' + tt, '-0.000001');
      t('xs:decimal(-0.0000001) cast as ' + tt, '-0.0000001');
      tdouble('xs:double(-0.0000001) cast as ' + tt, '-1.0E-7');
      t('xs:float(-0.0000001) cast as ' + tt, '-1.0E-7');
      t('xs:decimal(-100000) cast as ' + tt, '-100000');
      t('xs:double(-100000) cast as ' + tt, '-100000');
      t('xs:float(-100000) cast as ' + tt, '-100000');
      t('xs:decimal(-1000000) cast as ' + tt, '-1000000');
      t('xs:double(-1000000) cast as ' + tt, '-1.0E6');
      t('xs:float(-1000000) cast as ' + tt, '-1.0E6');
      t('xs:decimal(-10000000) cast as ' + tt, '-10000000');
      t('xs:double(-10000000) cast as ' + tt, '-1.0E7');
      t('xs:float(-10000000) cast as ' + tt, '-1.0E7');

      t('xs:dateTime("2000-02-03T01:02:03") cast as ' + tt, '2000-02-03T01:02:03');
      t('xs:date(xs:dateTime("2000-02-03T01:02:03")) cast as ' + tt, '2000-02-03');
      t('xs:time(xs:dateTime("2000-02-03T01:02:03")) cast as ' + tt, '01:02:03');
    end;

    t('"foo       bar    def" castable as xs:normalizedString', 'true');
    t('"foo       bar    def" castable as xs:token', 'true');
    t('join((QName("", "lname")  castable as xs:QName, () castable as QName?, () cast as QName?, xs:QName("ncname") castable as xs:QName, (xs:QName("ncname"),xs:QName("ncname")) castable as xs:QName?))', 'true true true false');
    t('QName("", "lname")  cast as xs:QName', 'lname');
    t('"ABC" castable as xs:QName', 'true');
    t('"foo bar" castable as xs:QName', 'false');
    t('"ABC:::+as-::" castable as xs:QName', 'false');
    t('"    foo   " castable as xs:QName', 'true');
    t('"    foo   " cast as xs:QName', 'foo');
    t('true() castable as xs:QName', 'false');
    if UsingFLRE then begin
      t('true() castable as xs:NCName', 'true');
      t('false() castable as xs:NCName', 'true');
      t('177 castable as xs:NCName', 'false');
    end;

    t('"" castable as xs:base64Binary', 'true');
    t('"" cast as xs:base64Binary', '');
    t('"       " castable as xs:base64Binary', 'true');
    t('"       " cast as xs:base64Binary', '');
    t('xs:base64Binary("") eq xs:base64Binary("")', 'true');
    t('xs:string(xs:base64Binary(""))', '');
    t('xs:string(xs:base64Binary("a a a a"))', 'aaaa');

    t('xs:dateTime("2000-02-03T24:00:00")', '2000-02-04T00:00:00');
    t('"2000-02-03" castable as xs:dateTime', 'false');
    t('"24:01:00"   castable as xs:dateTime', 'false');
    t('"2000-02-03T24:01:00"   castable as xs:dateTime', 'false');
    t('"20000-02-03T20:01:00"  castable as xs:dateTime', 'true');
    t('"20000000000000000-02-03T20:01:00"  castable as xs:dateTime', 'false'); //overflow
    t('"0-02-03T20:01:00"  castable as xs:dateTime', 'false'); //no year zero
    t('"200-02-03T20:01:00"    castable as xs:dateTime', 'false');
    t('"2000-13-03T20:01:00"   castable as xs:dateTime', 'false');
    t('"2000-02-03T20:70:00"   castable as xs:dateTime', 'false');
    t('"2000-02-03T20:00:-10"  castable as xs:dateTime', 'false');
    t('"20:00:10"              castable as xs:time', 'true');
    t('"20:00:70"              castable as xs:time', 'false');

    if UsingFLRE then
      for j := 0 to 3 do begin
        case j of
          0: tt := 'xs:NCName';
          1: tt := 'xs:ID';
          3: tt := 'xs:IDREF';
          2: tt := 'xs:ENTITY';
        end;
        t('"   abc   " castable as ' + tt, 'true');
        t('" -    " castable as ' + tt, 'false');
        t('"-" castable as ' + tt, 'false');
        t('"abc:def" castable as ' + tt, 'false');
      end;
    t('"abc:def" castable as xs:Name', 'true');
    t('"abc: def" castable as xs:Name', 'false');
    t('"  abc:def   " castable as xs:Name', 'true');
    t('"0:def" castable as xs:Name', 'false');

{    t('string-join("a b c d" cast as xs:ENTITIES, "|")', 'a|b|c|d');
    t('string-join("a b c d" cast as xs:IDREFS, "|")', 'a|b|c|d');
    t('string-join("a b c d" cast as xs:NMTOKENS, "|")', 'a|b|c|d');}
  end;
  ps.StaticContext.strictTypeChecking:=strictTypeChecking;

  t('element', 'E1aT1E1bD1aA1D1bC1PI1E1c', '!<element>E1a<text>T1</text>E1b<document>D1a<attribute>A1</attribute>D1b<comment>C1</comment><processing-instruction>PI1</processing-instruction></document>E1c</element>');
  t('string-join(for $a in element return $a / text(), ":")', 'E1a:E1b:E1c');
  t('string-join(for $a in element return $a / text, ":")', 'T1');
  t('string-join(for $a in element return $a / document, ":")', 'D1aA1D1bC1PI1');
  t('//(text)', 'T1');
  t('//((text))', 'T1');
  t('//document', 'D1aA1D1bC1PI1');
  t('//((document))', 'D1aA1D1bC1PI1');
  t('join(for $a in element return $a / document / comment, ":")', 'C1');
  t('join(for $a in element return $a / document / processing-instruction, ":")', 'PI1');
  t('join(for $a in element return $a / document / (comment | processing-instruction), ":")', 'C1:PI1');

  f('if (1,2,3) then 5 else 6', 'err:FORG0006');
  t('if (/a,2,3) then 5 else 6',  '5', '!<a>..</a>');
  t('if ({},2,3) then 5 else 6', '5');
  f('/processing-instruction(a b)', 'err:XPST0003');
  f('/processing-instruction(a,b)', 'err:XPST0003');//should be 0004?

  t('string-join(css("b|foobar"), ",")', 'hallo', '<xyz:foobar xmlns:xyz="test://b">hallo</xyz:foobar>');
  t('string-join(css("a|foobar"), ",")', '');
  t('string-join(css("*|foobar"), ",")', 'hallo');
  t('string-join(css("b|*"), ",")', 'hallo');

  t('string-join(css("b|foobar"), ",")', 'hallo', '<foobar xmlns="test://b">hallo</foobar>');
  t('string-join(css("a|foobar"), ",")', '');
  t('string-join(css("*|foobar"), ",")', 'hallo');
  t('string-join(css("b|*"), ",")', 'hallo');

  t('/r', 'test', '!<r>test</r>');
  ps.StaticContext.useLocalNamespaces:=true;
  t('/r', 'test', '!<r xmlns="foobar">test</r>');
  ps.StaticContext.useLocalNamespaces:=false;
  t('/r', '', '');
  TNamespace.assignRC(ps.StaticContext.defaultElementTypeNamespace, TNamespace.create('foobar', ''));
  t('/r', 'test');
  TNamespace.releaseIfNonNil(ps.StaticContext.defaultElementTypeNamespace);

  t('day-from-dateTime(())', '');
  t('doc(())', '');
  t('collection(())', 'foobar');

  t('x / text() / . / . / .', 'y', '<x>y</x>');
  t('x / comment() / . / . / .', 'comment', '<x><!--comment--></x>');
  t('name(x / processing-instruction())', 'PI', '<x><?PI?></x>');
  t('name(x / processing-instruction() / . / . / .)', 'PI', '');
  t('string-join(/a // string-join(for $i in ./ancestor::* return name($i), ":"), ",")', ',a,a:b,a:b:c,a,a', '!<a><b><c>d</c></b><x/><y/></a>');
  t('string-join(/a // string-join(for $i in ./ancestor-or-self::* return name($i), ":"), ",")', 'a,a:b,a:b:c,a:b:c,a:x,a:y', '');
  t('outer-xml(//text()/ancestor::*[1])', '<c>d</c>', '');
  t('outer-xml(//text()/ancestor::*[2])', '<b><c>d</c></b>', '');
  t('outer-xml(//text()/ancestor::*[3])', '<a><b><c>d</c></b><x/><y/></a>', '');
  t('outer-xml(//text()/ancestor::*[4])', '', '');
  t('outer-xml(//text()/ancestor-or-self::*[1])', '<c>d</c>', ''); //self does not match *
  t('outer-xml(//text()/ancestor-or-self::*[2])', '<b><c>d</c></b>', '');
  t('outer-xml(//text()/ancestor-or-self::*[3])', '<a><b><c>d</c></b><x/><y/></a>', '');
  t('outer-xml(//text()/ancestor-or-self::*[4])', '', '');
  t('outer-xml(//text()/ancestor-or-self::node()[1])', 'd', '');
  t('outer-xml(//text()/ancestor-or-self::node()[2])', '<c>d</c>', ''); //self does not match *
  t('outer-xml(//text()/ancestor-or-self::node()[3])', '<b><c>d</c></b>', '');
  t('outer-xml(//text()/ancestor-or-self::node()[4])', '<a><b><c>d</c></b><x/><y/></a>', '');
  t('outer-xml(//text()/ancestor-or-self::node()[5])', '<a><b><c>d</c></b><x/><y/></a>', ''); //document node
  t('outer-xml(//text()/ancestor-or-self::node()[6])', '', '');
  t('', '', '!<a><b><c/></b><x/><y/></a>');
  t('outer-xml(//c/ancestor-or-self::*[1])', '<c/>', '');
  t('outer-xml(//c/ancestor-or-self::*[2])', '<b><c/></b>', '');
  t('outer-xml(//c/ancestor-or-self::*[3])', '<a><b><c/></b><x/><y/></a>', '');
  t('outer-xml(//c/ancestor-or-self::*[4])', '', '');
  t('outer-xml(//c/ancestor::*[1])', '<b><c/></b>', '');
  t('outer-xml(//c/ancestor::*[2])', '<a><b><c/></b><x/><y/></a>', '');
  t('outer-xml(//c/ancestor::*[3])', '', '');
  t('/ < 2', 'true', '<a>1</a>');
  t('/<2', 'true', '<a>1</a>');


  t('base-uri(/)', '', '!<root xml:base="http://www.example.org"><!--comment--><sub1 xml:base="foobar"><sub1a xml:base="123" attrib="maus"/></sub1><sub2 xml:base="test/xyz"><sub2b xml:base="tiu/"/><sub2c xml:base="tiv"/></sub2><sub3 xml:base="456/"/><sub4 xml:base="http://www.benibela.de"/></root>');
  //t('empty(base-uri(/))', 'true'); ??
  t('base-uri(/root)', 'http://www.example.org');
  t('base-uri(/root/sub1)', 'http://www.example.org/foobar'); //check this: there should not be a / between example.org and foobar? just string concatening?
  t('base-uri(/root/sub1)', 'http://www.example.org/foobar');
  t('base-uri(/root/sub1/sub1a)', 'http://www.example.org/123');
  t('base-uri(/root/sub1/sub1a/@attrib)', 'http://www.example.org/123');
  t('base-uri(/root/sub2)', 'http://www.example.org/test/xyz');
  t('base-uri(/root/sub2/sub2b)', 'http://www.example.org/test/tiu/');
  t('base-uri(/root/sub2/sub2c)', 'http://www.example.org/test/tiv');
  t('base-uri(/root/sub4)', 'http://www.benibela.de');
  t('base-uri(/root/comment())', 'http://www.example.org');
  t('base-uri(/root/comment()) instance of xs:string', 'false');
  t('base-uri(/root/comment()) instance of xs:anyURI', 'true');

  t('"  123  " cast as xs:integer', '123');
  t('"    567.566  " cast as xs:double', '567.566');
  t('"    www  " cast as xs:anyURI', 'www');
  t('"    www  " cast as xs:string', '    www  ');
  t('"    www  " cast as xs:normalizedString', '    www  ');
//  t('"    ww   w  " cast as xs:untyped', '    ww   w  ');
  t('"    ww   w  " cast as xs:untypedAtomic', '    ww   w  ');
  t('"    ww'#9'w  " cast as xs:normalizedString', '    ww w  ');
  t('"    123  " cast as xs:int', '123');


  t('xs:untypedAtomic("0") + xs:float(0)', '0');
  t('type-of(xs:untypedAtomic("0") + xs:float(0))', 'double');
  t('xs:untypedAtomic("0") + xs:decimal(0)', '0');
  t('type-of(xs:untypedAtomic("0") + xs:decimal(0))', 'double');
  t('string-join(for $x in (1, xs:decimal(2), xs:float(3), xs:double(4), xs:untypedAtomic(5)), ' +
                '$y in (1, xs:decimal(2), xs:float(3), xs:double(4), xs:untypedAtomic(5)) return type-of($x + $y), " ")',
                'integer decimal float double double decimal decimal float double double float float float double double double double double double double double double double double double'); //XQTS test
  t('string-join(for $x in (1, xs:decimal(2), xs:float(3), xs:double(4), xs:untypedAtomic(5)), ' +
    '$y in (1, xs:decimal(2), xs:float(3), xs:double(4), xs:untypedAtomic(5)) return type-of($x mod $y), " ")',
    'integer decimal float double double decimal decimal float double double float float float double double double double double double double double double double double double'); //XQTS test

  t('(xs:untypedAtomic("3") - 1.1) instance of xs:double', 'true');
  t('(xs:positiveInteger("1") idiv xs:nonPositiveInteger("-999999999999999999")) instance of xs:integer', 'true');
  t('(xs:positiveInteger("1") idiv xs:nonPositiveInteger("-999999999999999999")) instance of xs:positiveInteger', 'false');
  t('(xs:positiveInteger("1") idiv xs:nonPositiveInteger("-999999999999999999")) instance of xs:nonPositiveInteger', 'false');
  t('(xs:positiveInteger("1") idiv xs:positiveInteger("999999999999999999"))', '0');
  t('(xs:positiveInteger("1") idiv xs:positiveInteger("999999999999999999")) instance of xs:integer', 'true');
  t('(xs:positiveInteger("1") idiv xs:positiveInteger("999999999999999999")) instance of xs:positiveInteger', 'false');
  t('(xs:positiveInteger("1") idiv xs:positiveInteger("999999999999999999")) instance of xs:nonPositiveInteger', 'false');
  t('(xs:negativeInteger("-4") idiv xs:negativeInteger("-2"))', '2');
  t('(xs:negativeInteger("-4") idiv xs:negativeInteger("-2")) instance of xs:negativeInteger', 'false');
  t('(xs:negativeInteger("-4") idiv xs:negativeInteger("-2")) instance of xs:positiveInteger', 'false');
  t('(xs:negativeInteger("-4") idiv xs:negativeInteger("-2")) instance of xs:integer', 'true');

  t('xs:unsignedByte("200") * xs:unsignedByte(200)', '40000');
  t('(xs:unsignedByte("200") * xs:unsignedByte(200)) instance of xs:integer', 'true');
  t('(xs:unsignedByte("200") * xs:unsignedByte(200)) instance of xs:unsignedByte', 'false');
  t('xs:unsignedByte("200") + xs:unsignedByte(200)', '400');
  t('(xs:unsignedByte("200") + xs:unsignedByte(200)) instance of xs:integer', 'true');
  t('(xs:unsignedByte("200") + xs:unsignedByte(200)) instance of xs:unsignedByte', 'false');
  t('xs:unsignedByte("200") - xs:unsignedByte(201)', '-1');
  t('(xs:unsignedByte("200") - xs:unsignedByte(201)) instance of xs:integer', 'true');
  t('(xs:unsignedByte("200") - xs:unsignedByte(201)) instance of xs:unsignedByte', 'false');

  t('/ = "hallo" ', 'true', '!<a>hallo</a>');
  t('/ = "hxallo" ', 'false', '!<a>hallo</a>');
  t('/ | / ', 'hallo', '!<a>hallo</a>');
  t('/ + 17', '140', '!<a>123</a>');

  t('xs:float(1e7)', '1.0E7');
  t('xs:double(1e18)', '1.0E18');
  t('xs:decimal(1e18)', '1000000000000000000');
  t('abs(xs:untypedAtomic("-12.3"))', '12.3');
  t('floor(xs:untypedAtomic("12.3"))', '12');
  t('years-from-duration(xs:untypedAtomic("P35M"))', '2');
  t('month-from-date(xs:untypedAtomic("2004-07-09"))', '7');
  t('month-from-dateTime(xs:untypedAtomic("2004-07-09T00:00:00"))', '7');
  t('hours-from-time(xs:untypedAtomic("17:12:34"))', '17');
  t('hours-from-dateTime(xs:untypedAtomic("2004-07-09T12:18:34"))', '12');

  DefaultSystemCodePage := CP_UTF8;
  t('translate("Hallö", "aö", "äo")', 'Hällo');
  DefaultSystemCodePage := CP_LATIN1;
  t('translate("Hallö", "aö", "äo")', 'H'#$C3'll'#$A4'o');
  {
  DefaultSystemCodePage := CP_ACP;
  t('translate("Hallö", "aö", "äo")', 'Hällo');
  }
  DefaultSystemCodePage := CP_UTF8;
  t('normalize-unicode("e'#$CC#$81'")', #$C3#$A9);
  t('normalize-unicode("e'#$CC#$81'", "NFC")', #$C3#$A9);
  t('normalize-unicode("e'#$CC#$81'", "")', 'e'#$CC#$81);
  t('join(string-to-codepoints("XÄY"), " ")', '88 196 89');


  t('local-name(html)', '',  '<html xmlns="foobar"><svg:abc xmlns:svg="svgNS" xmlns:svg2="svgNS" xmlns:svg3="nomatch"> <svg:a/> <svg2:b/> <svg3:c/> </svg:abc> </html>');
  ps.StaticContext.useLocalNamespaces:=true;
  t('local-name(html)', 'html');
  t('local-name(html/svg:*)', 'abc');
  t('local-name(html/svg2:*)', 'abc');
  t('local-name(html/svg3:*)', '');
  t('join(for $i in html/svg:abc/* return local-name($i), " ")', 'a b c');
  t('join(for $i in html/svg:abc/svg:* return local-name($i), " ")', 'a b'); //svg and svg2 have the same ns url, so they are both returned
  t('join(for $i in html/svg:abc/svg2:* return local-name($i), " ")', 'a b');
  t('join(for $i in html/svg:abc/svg3:* return local-name($i), " ")', 'c');

  t('', '',  '<html xmlns="foobar"><svg:abc xmlns:svg="svgNS"> foobar </svg:abc> <svg2:def xmlns:svg2="svgNS"> foobar </svg2:def> <svg:xyz xmlns:svg="NS2"> 123 </svg:xyz>  </html>');
  t('string-join(for $i in html/svg:* return local-name($i), " ")', 'abc xyz');
  t('string-join(for $i in html/svg:* return $i, " ")', 'foobar 123');
  t('', '',  '<html xmlns:test="foobar"><foobar test:abc="123" xmlns:test2="asasdasd" test2:abc="456"/></html>');
  t('string-join(html/foobar/@test:abc, " ")', '123');
  t('html/foobar/string-join(@test:abc, " ")', '123');
  t('string-join(html/foobar/(concat(">", @test:abc, "<")), " ")', '>123<');
  t('string-join(html/foobar/@test2:abc, " ")', '456');
  t('html/foobar/string-join(@test2:abc, " ")', '456');
  t('string-join(html/foobar/(concat(">", @test2:abc, "<")), " ")', '>456<');


  performUnitTest('$abc','alphabet','');
  f('$ABC', 'err:XPST0008');
  performUnitTest('$abc','alphabet','');
  f('$ABC', 'err:XPST0008');


  xml.parseTree('<?xml encoding="utf-8"?><html/>'); if xml.getLastTree.baseEncoding <> CP_UTF8 then raise Exception.Create('xml encoding detection failed 1');
  xml.parseTree('<?xml encoding="windows-1252"?><html/>'); if xml.getLastTree.baseEncoding <> CP_Windows1252 then raise Exception.Create('xml encoding detection failed 2');
  xml.parseTree('<?xml encoding="utf-8" foo="bar"?><html/>'); if xml.getLastTree.baseEncoding <> CP_UTF8 then raise Exception.Create('xml encoding detection failed 3');
  xml.parseTree('<?xml encoding="windows-1252" foo="bar"?><html/>'); if xml.getLastTree.baseEncoding <> CP_WINDOWS1252 then raise Exception.Create('xml encoding detection failed 4');

  //HTML parsing tests
  xml.parsingModel:=pmHTML;
  xml.repairMissingStartTags:=false;
  t('outer-html(/)', '<table></table>', '<table></table>');
  t('outer-html(/)', '<table><td>1</td><td>2</td><td>3</td></table>', '<table><td>1<td>2<td>3</table>');
  t('outer-html(/)', '<table><td><table><td>1</td><td>2</td><td>3</td></table></td></table>', '<table><td><table><td>1<td>2<td>3</table></table>');
  t('outer-html(/)', '<ol><li>1</li><li>2</li><li><ul><li>foo</li><li>bar</li></ul></li></ol>' , '<ol><li>1<li>2<li><ul><li>foo<li>bar');
  t('outer-html(/)', '<select><option>1</option><option>2</option><optgroup><option>4</option><option>5</option></optgroup><optgroup><option>6</option><option>7</option></optgroup></select>' , '<select><option>1<option>2<optgroup><option>4<option>5<optgroup><option>6<option>7');
  t('outer-html(/)', '<omg><button>A</button><button>B</button><button>C</button></omg>' , '<omg><button>A<button>B<button>C</omg>');
  t('outer-html(/)', '<table><col><tr>17</tr></table>' , '<table><col><tr>17</table>');
  t('outer-html(/table/tr)', '<tr>17</tr>' , '<table><col><tr>17</table>');
  t('outer-html(/)', '<table><colgroup></colgroup><tr>17</tr></table>' , '<table><colgroup><tr>17</table>');
  t('outer-html(/)', '<div><p>123</p><p>456</p><p>789</p><div>hi<p>abc</p><p>def</p></div></div>' , '<div><p>123<p>456<p>789<div>hi<p>abc<p>def</div>');
  t('outer-html(/)', '<div><p>123<em>mausi</em></p><p>foo<i><b>bar</b></i></p><p>hallo</p></div>' , '<div><p>123<em>mausi<p>foo<i><b>bar<p>hallo'); //not entirely correct. The end tags are correctly inserted, but additional em/i/b start tags are leaking out
  t('outer-html(/)',  '<div>123456</div>', '<div>123</ br>456</div>'); //firefox turns it into a <!-- br --> comment but we cannot do that
  t('outer-html(/)', '<table><tr><td><TABLE><tr><td>1</td></tr></TABLE></td></tr></table>' {end tags should be uppercase}, '<table><tr><td><TABLE><tr><td>1</td></tr></TABLE></TD></TR></table>');
  t('outer-html(/)', '<table><TR><TD>1</TD><td>2</td><TD>3</TD></TR></table>', '<table><TR><TD>1<td>2<TD>3</TR></table>');

  t('outer-html(/)', '<table><tr><td><br></td><td>'+{firefox: </br>}'</td></tr></table>', '<table><tr><td><br></td><td></br></td></tr></table>');
  t('outer-html(/)', '<body>some texta<http: //some-textb="">some textc</http:></body>','<body>some texta <http://some-textb> some textc</body>'); //this does not make much sense. not removing the last slash from http:// would be better. firefox turns it into an element: <http: some-text=""> some text</http:>
  t('outer-html(/)', '<body><a a="1" /="" b="2"></a><a a="1" /b="2"></a><a a="1" //="" b="2"></a><a a="1" /="" <="" b="2"></a><a a="1" /="">b="2"/&gt;</a></body>', '<body><a a="1" / b="2"/><a a="1" /b="2"/><a a="1" // b="2"/><a a="1" /     < b="2"/><a a="1" /     > b="2"/></body>'); //not exactly what firefox does, but similar enough
  t('outer-html(/)', '<INPUT TYPE="CHECKBOX" VALUE="item:IN=M02 149 507 0;AT=Luk" janenko,="" Sergej="" V./Der="" falsche="" Spiegel''="" NAME="855439" ID="R855439">', '<INPUT TYPE=CHECKBOX VALUE=''item:IN=M02 149 507 0;AT=Luk''janenko, Sergej V./Der falsche Spiegel'' NAME=''855439'' ID=''R855439''>'); //close enough

  xml.repairMissingStartTags:=true;
  t('outer-html(/)', '<html><head></head><body><table></table></body></html>', '<table></table>');
  t('outer-html(/)', '<html><head></head><body><table></table></body></html>', '<body><table></table></body>');
  t('outer-html(/)', '<html><head></head><body><table></table></body></html>', '<html><table></table></html>');
  t('outer-html(/)', '<html><head></head><body><table></table></body></html>', '<html><head></head><table></table></html>');
  t('outer-html(/)', '<html><head></head><body><table></table></body></html>', '<html><head></head><body><table></table></body></html>');
  t('outer-html(/)', '<html><head><meta name="a"><meta name="b"><meta name="c"></head><body><meta name="d"></body></html>', '<html><head><meta name="a"><meta name="b"/></head><meta name="c"><body><meta name="d"></body></html>');
  t('outer-html(/)', '<html><head><meta name="a"><meta name="b"><meta name="c"></head><body><meta name="d"></body></html>', '<head><meta name="a"><meta name="b"/></head><meta name="c"><body><meta name="d"></body></html>');
  t('outer-html(/)', '<html><head><meta name="a"><meta name="b"><meta name="c"></head><body><meta name="d"></body></html>', '<meta name="a"><meta name="b"/></head><meta name="c"><body><meta name="d"></body></html>');
  t('outer-html(/)', '<html><head><meta name="a"><meta name="b"><meta name="c"><meta name="d"></head><body></body></html>', '<meta name="a"><meta name="b"/></head><meta name="c"><meta name="d"></body></html>');
  t('outer-html(/)', '<html><head><meta name="a"><meta name="b"><meta name="c"></head><body>foobar<meta name="d"></body></html>', '<meta name="a"><meta name="b"/></head><meta name="c">foobar<meta name="d"></body></html>');
  t('string-join(/html/head/meta/inner-text(), "")', '');
  t('outer-html(/)', '<html><head><meta name="a"></head><body><unknown><meta name="b"><meta name="c">foobar<meta name="d"></unknown></body></html>', '<meta name="a"><unknown><meta name="b"/></head><meta name="c">foobar<meta name="d"></body></html>');
  t('outer-html(/)', '<html><head></head><body>empty</body></html>', 'empty');
  t('outer-html(/)', '<html><head></head><body>empty</body></html>', '<body>empty</body>');
  t('outer-html(/)', '<html><head></head><body>empty</body></html>', '<html>empty</html>');
  t('outer-html(/)', '<html><head></head><body>empty</body></html>', '<html><head></head>empty</html>');
  t('outer-html(/)', '<html><head></head><body>empty</body></html>', '<html><head></head><body>empty</html>');
  t('outer-html(/)', '<html><head></head><body><unknown></unknown></body></html>', '<unknown/>');
  t('outer-html(/)', '<html><head></head><body><unknown></unknown></body></html>', '<body><unknown/></body>');
  t('outer-html(/)', '<html><head></head><body><unknown></unknown></body></html>', '<html><unknown/></html>');
  t('outer-html(/)', '<html><head></head><body><unknown></unknown></body></html>', '<html><head></head><unknown/></html>');
  t('outer-html(/)', '<html><head></head><body><unknown></unknown></body></html>', '<html><head></head><body><unknown/></html>');

  //  t('outer-html(/)', '<html><head></head><body></body></html>', ' ');
  t('outer-html(/)', '<html><head></head><body></body></html>', '<body></body>');
  t('outer-html(/)', '<html><head></head><body></body></html>', '<html></html>');
  t('outer-html(/)', '<html><head></head><body></body></html>', '<html><head></head></html>');
  t('outer-html(/)', '<html><head></head><body></body></html>', '<html><head></html>');
  t('outer-html(/)', '<html><head></head><body></body></html>', '<html><head></head><body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>empty</body></html>', '<title>Hallo</title>empty');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body><h1>wtf</h1></body></html>', '<title>Hallo</title><h1>wtf</h1>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>abcempty</body></html>', '<title>Hallo</title><head>abc</head>empty');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body></body></html>', '<title>Hallo</title>');
  t('outer-html(/)', '<html><head><meta charset="UTF-8"><style type="text/css">ass</style></head><body></body></html>', '<head><meta charset="UTF-8" /><style type="text/css">ass</style></head>');
  t('outer-html(/)', '<!--[if !(IE 6) | !(IE 7) | !(IE 8) ] | !(IE 9) ><!--><html><!--<![endif]--><head><meta charset="UTF-8"><style type="text/css">ass</style></head><body></body></html>', '<!--[if !(IE 6) | !(IE 7) | !(IE 8) ] | !(IE 9) ><!--><html><!--<![endif]--><head><meta charset="UTF-8" /><style type="text/css">ass</style></head>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body></body></html>', '<title>Hallo</title><html><head></head></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?</body></html>', '<title>Hallo</title><html><head></head><body>WTF?</body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shit</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html>Stupid shit</html></body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupidshitthat html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html>Stupid <body>shit</body>that html is</html></body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shitthathtml is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body>that </html>html is</body></html>');
  xml.trimText:=false;
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shitthat html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html>Stupid <body>shit</body>that html is</html></body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shitthat html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body>that </html>html is</body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shitthat html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body></html>that html is</body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shit<unknown></unknown>that html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body></html><unknown/>that html is</body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shit<div></div>that html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body></html><div></div>that html is</body></html>');
  t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?<div class="abc">Stupid shit<div></div>that html is</div></body></html>', '<title>Hallo</title><html><head></head><body>WTF?<div class="abc"><html><body>Stupid shit</body></html><div></div>that html is</body></html>');
  t('outer-html(/)', '<html><head></head><body><div class="content"> <table><tbody><tr><td colspan="9">Ausweis gültig bis: 05.05.2013</td></tr></tbody></table> <font color="black"><br></font></div></body></html>', '<html><body><div class="content"><html><body> <table><tbody><tr><td colspan="9">Ausweis gültig bis: 05.05.2013</td></tbody></table> </body></html><font color="black"><br></font>');
  //did not work: &#252; is converted to &amp;#252???? t('outer-html(/)', '<html><head></head><body><div class="content"> <table><tbody><tr><td colspan="9">Ausweis gültig bis: 05.05.2013</td></tr></tbody></table> <font color="black"><br></font></body></html>', '<html><body><div class="content"><html><body> <table><tbody><tr><td colspan="9">Ausweis g&#252;ltig bis: 05.05.2013</td></tbody></table> </body></html><font color="black"><br></font>');

  //xml.TargetEncoding:=CP_NONE;
  nbsp := #$C2#$A0;
  //t('outer-html(/)', '<html><head></head><body>&amp;auml;&amp;nbsp;</body></html>', '<html>&auml;&nbsp;</html>');
  xml.TargetEncoding:=CP_UTF8;
  t('outer-html(/)', '<html><head></head><body>ä'+nbsp+'</body></html>', '<html>&auml;&nbsp;</html>');
  t('outer-html(/)', '<html><head><script>&nbsp&auml;&nbsp;</script></head><body></body></html>', '<html><script>&nbsp&auml;&nbsp;</script></html>');
  xml.TargetEncoding:=CP_WINDOWS1252;
  t('outer-html(/)', '<html><head></head><body>'#228#$A0'</body></html>', '<html>&auml;&nbsp;</html>');
  t('outer-html(/)', '<html><head></head><body>'#$A0#228#$A0'</body></html>', '<html>&nbsp&auml;&nbsp;</html>');

  xml.parsingModel:=pmStrict;
  xml.TargetEncoding:=CP_UTF8;
  t('outer-html(/)', '<html><script>ä</script></html>', '<html><script>&auml;</script></html>');
  t('outer-html(/)', '<html><script>&auml;</script></html>', '<html><script><![CDATA[&auml;]]></script></html>');
  t('outer-xml(/)', '<html><script>&amp;auml;</script></html>', '<html><script><![CDATA[&auml;]]></script></html>');
  t('outer-xml(/)', '<xml>'#10'</xml>', '<xml><![CDATA['#13']]></xml>');
  t('outer-xml(/)', '<xml>'#10'</xml>', '<xml><![CDATA['#13#10']]></xml>');

  t('outer-xml(/)', '<foo xmlns:abc="123" abc:def="456"/>', '<foo abc:def="456" xmlns:abc="123"/>');

  xml.parsingModel:=pmHTML;

//  t('outer-html(/)', '', '<html><body><div class="content"><html><body><table width="99%"><tbody><tr><td>12345</td><td>&#160;&#160;>  S.L.<br/>></td><td>Entliehen/Bereitgestellt: 5</td><td>Geb&#252;ühren: 2,00&#160;&#8364;</ €</td></tr></tbody></table><br/>><table><tbody><tr><td colspan="9">Ausweis g&#252;ültig bis: 05.05.2013</td><td></td></tr><tr></tr><tr></tr></tbody></table><br/>></div></body></html><!-- Your output data goes in here --><font color="black"><br></font>');

  //xml.readComments:=true;
  //t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shit<!-- ?? -->that html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body><!-- ?? --></html>that html is</body></html>');
  //t('outer-html(/)', '<html><head><title>Hallo</title></head><body>WTF?Stupid shit<!-- ?? -->that html is</body></html>', '<title>Hallo</title><html><head></head><body>WTF?<html><body>Stupid shit</body></html><!-- ?? -->that html is</body></html>');


  //todo:insert body when nothing is there, insert body before text


  t('outer-html(/)', '<html><head></head><body><table><td>1</td><td>2</td><td>3</td></table></body></html>', '<table><td>1<td>2<td>3</table>');
  t('outer-html(/)', '<html><head></head><body><table><tbody><tr><td>1</td></tr></tbody></table></body></html>', '<table><tr><td>1</td></tr></table>');
  t('outer-html(/)', '<html><head></head><body><table><colgroup><col></colgroup><tbody><tr>17</tr></tbody></table></body></html>' , '<table><col><tr>17</table>');

  t('resolve-html(//a)', 'abc', '<html><head></head><body><a href="abc"/></body></html>');
  t('resolve-html(//a)', 'http://example.org/abc', '<html><head><base href="http://example.org"/></head><body><a href="abc"/></body></html>');
  t('//a/resolve-html()', 'http://example.org/abc');
  t('resolve-html(//a, "http://foobar.org")', 'http://example.org/def', '<html><head><base target="xx"/><base href="http://example.org"/><base href="http://www.google.de"/></head><body><a href="def"/></body></html>');
  t('resolve-html(//form, "http://foobar.org").url', 'http://example.org/def?b=a', '<html><head><base target="xx"/><base href="http://example.org"/><base href="http://www.google.de"/></head><body><form action="def"><input value="a" name="b"></body></html>');
  t('form(//form).url', 'http://example.org/def?b=a');

  //error tests
  t('() and true()', 'false');
  t('(/) and true()', 'true');
  t('(/, 2) and true()', 'true');
  t('(xs:string("")) and true()', 'false');
  t('(xs:untypedAtomic("")) and true()', 'false');
  t('(xs:anyURI("")) and true()', 'false');
  t('(xs:normalizedString("")) and true()', 'false');
  t('(xs:string("a")) and true()', 'true');
  t('(xs:untypedAtomic("s")) and true()', 'true');
  t('(xs:anyURI("d")) and true()', 'true');
  if UsingFLRE then   t('(xs:NCName("e")) and true()', 'true');
  t('(jn:null()) and true()', 'false');
  f('(xs:string("a"), 1) and true()', 'err:FORG0006');
  f('(xs:string("a"), 1) and true()', 'err:FORG0006');
  f('(0, xs:string("a")) and true()', 'err:FORG0006');
  f('(jn:null(), 8) and true()', 'err:FORG0006');
  f('form := uri-combine($confirm-form, {"MakeResTypeDef.Reservation.RecipientLocn": $choose-result]})', 'pxp:OBJ');
  f('},', 'err:XPST0003');
  f(',,', 'err:XPST0003');


  //XQuery/XPath 3 syntax tests which must fail in the old version
  f('"a" || "b"', 'err:XPST0003');
  f('(1,2) ! 7', 'err:XPST0003');
  f('switch (10) case 10 return "a" case 20 return "b" default return "c"', 'err:XPST0003');
  f('fn:map(1,2)', 'err:XPST0017');
  f('1 instance of (xs:integer)', 'err:XPST0003');
  f('fn:string-join(("Blow, ", "blow, ", "thou ", "winter ", "wind!"))', 'err:XPST0017');



  //test differences between value and (value)
  xqv := xqvalue(10);
  xqw := TXQValueSequence.create(xqvalue(10));
  if ps.StaticContext.compareAtomic(xqv,xqw) <> xqcrEqual then raise Exception.Create('eq seq');
  if ps.StaticContext.compareAtomic(xqw,xqv) <> xqcrEqual then raise Exception.Create('seq eq');
  if ps.StaticContext.compareAtomic(xqw,xqw) <> xqcrEqual then raise Exception.Create('seq seq');


  //interface tests
  t('. + 1', '2', '<t>1</t>');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(ps.evaluateXPath2('"&quot;"').toString, '&quot;', 'evaluateXPath2 failed');
  test(ps.evaluateXPath2('2*.', xqvalue(7)).toString, '14', 'evaluateXPath2(ixqvalue) failed');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(TXQueryEngine.evaluateStaticXPath2('1 + 1 + 1').toString, '3', 'evaluateStaticXPath2 a failed');
  test(IXQuery(ps.parserEnclosedExpressionsString('a{$abc}{0}b')).evaluate().toString, 'aalphabet0b', 'xstring0');
  test(IXQuery(ps.parserEnclosedExpressionsString('a{1+2+3}b')).evaluate().toString, 'a6b', 'xstring1');
  test(IXQuery(ps.parserEnclosedExpressionsString('a{concat("x","y","z")}b')).evaluate().toString, 'axyzb', 'xstring2');

  for i := 1 to 5 do begin
    xqv := query('1 to $_1', [xqvalue(i)]);
    for j := 1 to i do begin
      iterator := xqv.GetEnumeratorPtrUnsafe;
      iterator2 := xqv.GetEnumeratorPtrUnsafe;
      iterator.MoveMany(j);
      for k := 1 to j do iterator2.MoveNext;
      if iterator.Current^ <> iterator2.Current^ then raise Exception.create(format('many fail: %i %i %i', [i,j,k]));
    end;

  end;

  test(xqvalue('abc').stringifyNodes.toString, 'abc');
  test(ps.evaluateXPath2('(1, 2, [1,{"abc": [2, "def"]},3])').stringifyNodes.jsonSerialize(tnsText, false), '[1, 2, [1, {"abc": [2, "def"]}, 3]]');

  writeln('XPath 2: ', testid, ' completed');

  ps.free;
  xml.Free;
  //vars.Free;
end;


end.

