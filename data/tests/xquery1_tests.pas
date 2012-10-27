unit xquery1_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, simplehtmltreeparser;

//test for xquery 1 expressions that are not also xpath 2 expressions, or are listed in the XQuery standard
procedure unittests;

implementation

type

{ THelper }

 THelper = class
  func1, func2, func3: TXQTerm;
  constructor create;
  destructor Destroy; override;
  procedure DeclareExternalVariableEvent(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;  const variable: string; var value: IXQValue);
  procedure DeclareExternalFunctionEvent(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;  const functionName: string; var value: TXQValueFunction);
end;

procedure unittests;
var
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;

  procedure performUnitTest(s1,s2,s3: string);
  var got: string;
    rooted: Boolean;
  begin
    if s3 <> '' then begin
      xml.parseTree(s3);
      ps.RootElement := xml.getLastTree;
    end;
    ps.parseXQuery1(s1);
  //    if strContains(s1, '/') then writeln(s1, ': ', ps.debugTermToString(ps.FCurTerm));
    ps.ParentElement := xml.getLastTree;
  //    writeln(s1);
  //    writeln('??');
  //    writeln(ps.debugtermToString(ps.FCurTerm));
    got := ps.evaluate().toString;
    if got<>s2 then
       raise Exception.Create('XQuery Test failed: '+IntToStr(count)+ ': '+s1+#13#10'got: "'+got+'" expected "'+s2+'"');
  end;

  procedure t(a,b: string; c: string = '');
  begin
    try
    count+=1;
    performUnitTest('string-join('+a+', " ")',b,c);

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;

  procedure m(a,b: string; c: string = ''); //main module
  begin
    try
    count+=1;
    performUnitTest(a,b,c);

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;

  procedure mr(s1: string); //module register
  begin
    try
      ps.registerModule(ps.parseXQuery1(s1));
    except on e:exception do begin
      writeln('Error @ "',s1, '"');
      raise;
    end end;
  end;

  procedure timing(s1, s2: string; s3: string = '');
  var
    starttime: TDateTime;
    i: Integer;
    got: string;
  begin
    if s3 <> '' then begin
      xml.parseTree(s3);
      ps.RootElement := xml.getLastTree;
    end;
    ps.parseXQuery1(s1);
  //    if strContains(s1, '/') then writeln(s1, ': ', ps.debugTermToString(ps.FCurTerm));
    ps.ParentElement := xml.getLastTree;
  //    writeln(s1);
  //    writeln('??');
  //    writeln(ps.debugtermToString(ps.FCurTerm));
    starttime := now;
    writeln(stderr, 'Timing '+s1+': ');
    for i := 1 to 200 do ps.evaluate(); //.toString;
    writeln('   => ', (now - starttime) * MSecsPerDay:5:5  );
    got := ps.evaluate().toString;
    if got<>s2 then
       raise Exception.Create('XPath Test failed: '+IntToStr(count)+ ': '+s1+#13#10'got: "'+got+'" expected "'+s2+'"');
  end;

var vars: TXQVariableChangeLog;
  helper: THelper;
begin
//  time := Now;
  vars:= TXQVariableChangeLog.create();

  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezone:=-5 / HoursPerDay;
  ps.OnEvaluateVariable:=@vars.evaluateVariable;
  ps.OnDefineVariable:=@vars.defineVariable;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  t('"12.5"', '12.5');
  t('12', '12');
  t('12.5', '12.5');
  t('125E2', '12500');
  t('"12.5" instance of xs:string', 'true');
  t('12 instance of xs:integer', 'true');
  t('12.5 instance of xs:decimal', 'true');
  t('125E2 instance of xs:double', 'true');
  t('12.5 instance of xs:double', 'false');
  t('125E2 instance of xs:decimal', 'false');
  t('"&quot;"',                   '"',                        ''); //XQuery has different string literals!
  t('''&quot;''',                 '"',                        '');
  t('"x&quot;y"',                   'x"y',                        '');
  t('''x&quot;y''',                 'x"y',                        '');
  t('"Ben &amp; Jerry&apos;s"', 'Ben & Jerry''s');
  t('"&#8364;99.50"', '€99.50'); //assuming utf 8 encoding

  t('xs:date("2001-08-25")', '2001-08-25');
  t('(10, (1, 2), (), (3, 4))', '10 1 2 3 4');
  t('(1 to 100)[. mod 5 eq 0]', '5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100');
  t('(21 to 29)[5]', '25');

  t('r/(a-b)', '1', '<r><a-b>1</a-b><a>10</a><b>20</b></r>');
  t('r/(a - b)', '-10', '');
  t('r/(a -b)', '-10', '');

  t('let $bellcost := 10, $whistlecost := 20 return -$bellcost + $whistlecost', '10');
  t('let $bellcost := 10, $whistlecost := 20 return -($bellcost + $whistlecost)', '-30');

  t('fn:QName("http://example.com/ns1", "this:color")  eq fn:QName("http://example.com/ns1", "that:color")', 'true');
  t('fn:QName("http://example.com/ns1", "this:color")  eq fn:QName("http://example.com/ns2", "that:color")', 'false');



  t('for $i in () where $i < 3 return $i', '');
  t('for $i in () where $i < 3 stable order by $i return $i', '');
  t('for $i in (1,2,3,4,5) where $i < 3 return $i', '1 2');
  t('for $i in 1 to 5 where $i < 3 return $i', '1 2');
  t('for $i in 1 to 5, $j in 2 to 4 where $i = $j return ($i,$j)', '2 2 3 3 4 4');
  t('let $temp := "abc" return $temp', 'abc');
  t('let $temp := (1,2,3)  return $temp', '1 2 3');

  t('for $i in (1,2,3) let $j := $i + 10 return $j', '11 12 13');
  t('let $s := (1,2,4) for $i in $s let $j := $i + 10 return $j', '11 12 14');
  t('let $a := 7, $b := 8, $c := 9, $s := ($a,$b,$c) for $i in $s let $j := $i + 10 return $j', '17 18 19');
  t('let $a := 7 let $b := 8 let $c := 9, $s := ($a,$b,$c) for $i in $s let $j := $i + 10 return $j', '17 18 19');
  t('let $a := 7, $b := 8 let $c := 9 let $s := ($a,$b,$c) for $i in $s let $j := $i + 10 return $j', '17 18 19');
  t('let $a := 7 for $b in 8 let $c := 9, $s := ($a,$b,$c) for $i in $s let $j := $i + 10 return $j', '17 18 19');

  t('for $i in (1,2,3,4,5) where $i < 3 order by $i return $i', '1 2');
  t('for $i in (1,2,3,4,5) where $i < 3 order by -$i return $i', '2 1');
  t('for $i in (1,2,3,4,5) where $i < 3 order by $i descending return $i', '2 1');
  t('for $i in (1,2,3,4,5) where $i < 3 order by -$i descending return $i', '1 2');
  t('for $i in (1,2,3,4,5) order by -$i return $i', '5 4 3 2 1');
  t('for $i in (1,2,3,4,5) order by -$i descending return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) let $j := -$i, $k := -$j order by $i return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) let $j := -$i, $k := -$j order by $j, $i return $i', '5 4 3 2 1');
  t('for $i in (1,2,3,4,5) let $j := -$i, $k := -$j order by $k, $j, $i return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) let $j := 8, $k := -$j order by $j, $i return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) order by $i < 3 return $i', '3 4 5 1 2');
  t('for $i in (1,2,3,4,5) order by $i < 3, $i return $i', '3 4 5 1 2');
  t('for $i in (1,2,3,4,5) order by $i < 3, $i descending return $i', '5 4 3 2 1');
  t('for $i in (1,2,3,4,5) order by $i < 3 descending return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) order by $i < 3 descending, $i return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) order by $i < 3 descending, $i descending return $i', '2 1 5 4 3');

  t('for $i in ("a","z","AB","abc","ab", "A") return $i', 'a z AB abc ab A');
  t('for $i in ("a","z","AB","abc","ab", "A") order by $i return $i', 'a A AB ab abc z');            //everything is case insensitive!
  t('for $i in ("a","z","AB","abc","ab", "A") order by $i descending return $i', 'z abc AB ab a A');
  t('for $i in ("a","z","AB","abc","ab", "A") order by $i collation "http://www.w3.org/2005/xpath-functions/collation/codepoint" return $i', 'A AB a ab abc z');            //but you can change that
  t('for $i in ("a","z","AB","abc","ab", "A") order by $i descending collation "http://www.w3.org/2005/xpath-functions/collation/codepoint" return $i', 'z abc ab a AB A');

  t('for $i in (1,2,3,4,5) order by if ($i < 3) then 0 else 1 return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) order by if ($i < 3) then 0 else 1 descending return $i', '3 4 5 1 2');
  t('for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty least return $i', '1 2 3 4 5');
  t('for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty greatest return $i', '3 4 5 1 2');
  t('for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 return $i', '3 4 5 1 2');

  t('for $x at $i in ("a", "b", "c") return "($i;: $x;)"', '(1: a) (2: b) (3: c)'); //using extended string syntax
  t('for $x at $i in ("a", "b", "c") let $j := $i * 2 return "($j;: $x;)"', '(2: a) (4: b) (6: c)');
  t('for $x at $i in ("a", "b", "c") let $j := $i * 2 where $j != 4 return "($j;: $x;)"', '(2: a) (6: c)');

  t('for $x in ("1", "2", "3") return type-of($x)', 'string string string');
  //t('for $x as xs:integer in ("1", "2", "3") return type-of($x)', 'integer integer integer'); //not actually allowed, read the standard wrong again

  t('let $j := 7 return concat($j, ": ", type-of($j))', '7: integer');
  t('let $j:= 7 return concat($j, ": ", type-of($j))', '7: integer');
  t('let $j :=7 return concat($j, ": ", type-of($j))', '7: integer');
  t('let $j:=7 return concat($j, ": ", type-of($j))', '7: integer');
  {t('let $j as xs:integer  := 7 return concat($j, ": ", type-of($j))', '7: integer');
  t('let $j as xs:decimal  := 7 return concat($j, ": ", type-of($j))', '7: decimal');
  t('let $j as xs:string := 7 return concat($j, ": ", type-of($j))', '7: string');
  t('let $j as string  := 7 return concat($j, ": ", type-of($j))', '7: string');}
  t('let $j as integer := 7 return concat($j, ": ", type-of($j))', '7: integer');
  t('let $j as integer := positiveInteger(7) return concat($j, ": ", type-of($j))', '7: positiveInteger');

  //from the standard
  t('let $j := 7 return (let $i := 5, $j := 20 * $i return $i, $j)', '5 7');
  t('let $j := 7 return (let $i := 5, $j := 20 * $i return ($i, $j))', '5 100');
  t('for $car at $i in ("Ford", "Chevy"), $pet at $j in ("Cat", "Dog") return concat("$i;,$car;,$j;,$pet;")', '1,Ford,1,Cat 1,Ford,2,Dog 2,Chevy,1,Cat 2,Chevy,2,Dog');
  t('let $inputvalues := (1,1141,100,200,144,51551,523) return fn:avg(for $x at $i in $inputvalues where $i mod 3 = 0 return $x)', '25825.5');
  t('for $e in //employee order by $e/salary descending return $e/name','23 Obama Sinclair Momo', '<r><employee><salary>1000000</salary><name>Obama</name></employee><employee><salary>1</salary><name>Momo</name></employee><employee><salary>7000000000000</salary><name>23</name></employee><employee><salary>90000</salary><name>Sinclair</name></employee></r>');
  t('for $b in /books/book[price < 100] order by $b/title return $b', '75.3Caesar 6Das Kapital 42The Hitchhiker''s Guide to the Galaxy', '<books><book><price>42</price><title>The Hitchhiker''s Guide to the Galaxy</title></book><book><price>1101010</price><title>How to use binary</title></book><book><price>6</price><title>Das Kapital</title></book><book><title>Das Kapital</title></book><book><price>753</price><title>Caesar</title></book><book><price>75.3</price><title>Caesar</title></book></books>');
  t('for $b in /books/book stable order by $b/title collation "http://www.w3.org/2005/xpath-functions/collation/codepoint",  $b/price descending empty least return $b','753Caesar 75.3Caesar 6Das Kapital Das Kapital 1101010How to use binary 42The Hitchhiker''s Guide to the Galaxy');
  t('for $b in /books/book stable order by $b/title collation "http://www.w3.org/2005/xpath-functions/collation/codepoint",  $b/price empty least return $b','75.3Caesar 753Caesar Das Kapital 6Das Kapital 1101010How to use binary 42The Hitchhiker''s Guide to the Galaxy');
  t('for $b in /books/book stable order by $b/title collation "http://www.w3.org/2005/xpath-functions/collation/codepoint",  $b/price descending empty greatest return $b','753Caesar 75.3Caesar Das Kapital 6Das Kapital 1101010How to use binary 42The Hitchhiker''s Guide to the Galaxy');
  t('for $b in /books/book stable order by $b/title collation "http://www.w3.org/2005/xpath-functions/collation/codepoint",  $b/price empty greatest return $b','75.3Caesar 753Caesar 6Das Kapital Das Kapital 1101010How to use binary 42The Hitchhiker''s Guide to the Galaxy');

  t('outer-xml(<hallo/>)', '<hallo/>');
  t('outer-xml(<hallo a="b"/>)', '<hallo a="b"/>');
  t('outer-xml(<hallo a="b" foo="bar"   triple = ''middle''/>)', '<hallo a="b" foo="bar" triple="middle"/>');
  t('outer-xml(<hallo>innertext</hallo>)', '<hallo>innertext</hallo>');
  t('outer-xml(<hallo>  preserved space  </hallo>)', '<hallo>  preserved space  </hallo>');
  t('outer-xml(<hallo><nestling/></hallo>)', '<hallo><nestling/></hallo>');
  t('outer-xml(<hallo> surrounded <nestling/> surrounded2 </hallo>)', '<hallo> surrounded <nestling/> surrounded2 </hallo>');
  t('outer-xml(<hallo> surrounded <nestling>double</nestling> surrounded2 </hallo>)', '<hallo> surrounded <nestling>double</nestling> surrounded2 </hallo>');
  t('outer-xml(<hallo> surrounded <nestling atti = "matti">double</nestling> surrounded2 </hallo>)', '<hallo> surrounded <nestling atti="matti">double</nestling> surrounded2 </hallo>');
  t('outer-xml(<hallo>&quot;</hallo>)', '<hallo>&quot;</hallo>');
  t('outer-xml(<hallo> inline entity: &quot;</hallo>)', '<hallo> inline entity: &quot;</hallo>');
  t('outer-xml(<hallo> inline entities: &lt;&amp;&gt;</hallo>)', '<hallo> inline entities: &lt;&amp;&gt;</hallo>'); //lt is escaped again (todo: also escape amp again)
  t('outer-xml(<hallo>{{brackets}}</hallo>)', '<hallo>{brackets}</hallo>');
  t('outer-xml(<hallo>surr{{brackets}}ounded</hallo>)', '<hallo>surr{brackets}ounded</hallo>');
  t('outer-xml(<hallo>1 + 2 + 3</hallo>)', '<hallo>1 + 2 + 3</hallo>');
  t('outer-xml(<hallo>{1 + 2 + 3}</hallo>)', '<hallo>6</hallo>');
  t('outer-xml(<hallo>{1} {2 + 3}</hallo>)', '<hallo>1 5</hallo>');
  t('outer-xml(<hallo>{1}{2 + 3}</hallo>)', '<hallo>15</hallo>');
  t('outer-xml(<hallo>{1}{2}{3}</hallo>)', '<hallo>123</hallo>');
  t('outer-xml(<book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book>)',
               '<book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book>');
  t('let $b := <book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book> return outer-xml($b)',
              '<book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book>');
  t('let $b := <book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book> return $b/title', 'Harold and the Purple Crayon');
  t('let $b := <book isbn="isbn-0060229357"><title>Harold and the Purple Crayon</title><author><first>Crockett</first><last>Johnson</last></author></book> return outer-xml(<example> <p> Here is a query. </p>  <eg> $b/title </eg>  <p> Here is the result of the query. </p>  <eg>{ $b/title }</eg>  </example>)',
         '<example> <p> Here is a query. </p>  <eg> $b/title </eg>  <p> Here is the result of the query. </p>  <eg><title>Harold and the Purple Crayon</title></eg>  </example>');
  t('let $a := <hallo>test</hallo>, $b := <def>{$a}</def> return outer-xml($b)', '<def><hallo>test</hallo></def>');
  t('let $a := <hallo foo="bar">test</hallo>, $b := <def>{$a}</def> return outer-xml($b)', '<def><hallo foo="bar">test</hallo></def>');
  t('let $a := <hallo foo="bar">test</hallo>, $b := <def>{$a/@*}</def> return outer-xml($b)', '<def foo="bar"/>');
  t('let $a := <hallo foo="bar" do="little" do2="more">test</hallo> return $a/@do', 'little');
  t('/hallo/@*', 'bar little more', '<hallo foo="bar" do="little" do2="more">test</hallo>');
  t('let $a := <hallo foo="bar" do="little" do2="more">test</hallo> return $a/@*', 'bar little more');
  t('let $a := <hallo foo="bar" do="little" do2="more">test</hallo>, $b := <def>{$a/@*}</def> return outer-xml($b)', '<def foo="bar" do="little" do2="more"/>');
  t('<a>5</a> eq <a>5</a>', 'true');
  t('<a>5</a> eq <b>5</b>', 'true');
  t('<a>5</a> is <a>5</a>', 'false');
  t('outer-xml(<a test="{1+2}">5</a>)', '<a test="3">5</a>');
  t('outer-xml(<a test="MAUS{1+2}HAUS">5</a>)', '<a test="MAUS3HAUS">5</a>');
  t('outer-xml(<a test="{1}{2}{3}">5</a>)', '<a test="123">5</a>');
  t('outer-xml(<a test="&apos;">5</a>)', '<a test="&apos;">5</a>');
  t('outer-xml(<a test="foo&apos;bar">5</a>)', '<a test="foo&apos;bar">5</a>');
  t('<a test="foo&apos;bar">5</a> / @test', 'foo''bar');
  t('outer-xml(<a test=''{1+2}''>5</a>)', '<a test="3">5</a>');
  t('outer-xml(<a test=''MAUS{1+2}HAUS''>5</a>)', '<a test="MAUS3HAUS">5</a>');
  t('outer-xml(<a test  =  ''MAUS{1+2}HAUS''   >5</a>)', '<a test="MAUS3HAUS">5</a>');
  t('outer-xml(<a test=''{1}{2}{3}''>5</a>)', '<a test="123">5</a>');
  t('outer-xml(<a test=''&apos;''>5</a>)', '<a test="&apos;">5</a>');
  t('<a test=''&apos;''>5</a> / @test', '''');
  t('outer-xml(<a test=''foo&apos;bar''>5</a>)', '<a test="foo&apos;bar">5</a>');
  t('outer-xml(<a test="xpa""th">5</a>)', '<a test="xpa&quot;th">5</a>');
  t('outer-xml(<a test=''xpa''''th''>5</a>)', '<a test="xpa&apos;th">5</a>');
  t('outer-xml(<a test="{<temp>dingdong</temp>}">5</a>)', '<a test="dingdong">5</a>');
  t('outer-xml(<a test="foo{{123}}bar">5</a>)', '<a test="foo{123}bar">5</a>');
  t('outer-xml(<a test="&#x61;&#x20;&#x61;{61}">&#x61;&#x20;&#x61;{61}</a>)', '<a test="a a61">a a61</a>');
  t('outer-xml(<a><![CDATA[]]></a>)', '<a/>');
  t('outer-xml(<a><![CDATA[123]]></a>)', '<a>123</a>');
  t('outer-xml(<a>foo<![CDATA[123]]>bar</a>)', '<a>foo123bar</a>');
  t('outer-xml(<a>{1+2}<![CDATA[{1+2}]]>{1+3}</a>)', '<a>3{1+2}4</a>');
  t('outer-xml(<a>&#x7d;{1+2}<![CDATA[{1+2}]]>{1+3}&#x7b;</a>)', '<a>}3{1+2}4{</a>');
  t('outer-xml(<a>{1, 2, 3}</a>)', '<a>1 2 3</a>');

  t('outer-xml(<shoe size="7"/>)', '<shoe size="7"/>');
  t('outer-xml(<shoe size="{7}"/>)', '<shoe size="7"/>');
  t('outer-xml(<shoe size="{()}"/>)', '<shoe size=""/>');
  t('outer-xml(<chapter ref="[{1, 5 to 7, 9}]"/>)', '<chapter ref="[1 5 6 7 9]"/>');
  t('outer-xml(let $hat := <ham size="123"/> return <shoe size="As big as {$hat/@size}"/>)', '<shoe size="As big as 123"/>');
  t('outer-xml(<a>{1}</a>)', '<a>1</a>');
  t('outer-xml(<a>{1, 2, 3}</a>)', '<a>1 2 3</a>');
  t('outer-xml(<c>{1}{2}{3}</c>)', '<c>123</c>');
  t('outer-xml(<b>{1, "2", "3"}</b>)', '<b>1 2 3</b>');
  t('outer-xml(<fact>I saw 8 cats.</fact>)', '<fact>I saw 8 cats.</fact>');
  t('outer-xml(<fact>I saw {5 + 3} cats.</fact>)', '<fact>I saw 8 cats.</fact>');
  t('outer-xml(<fact>I saw <howmany>{5 + 3}</howmany> cats.</fact>)', '<fact>I saw <howmany>8</howmany> cats.</fact>');

  t('outer-xml(<!--comment-->)', '<!--comment-->');
  t('outer-xml(<a><!--comment--></a>)', '<a><!--comment--></a>');
  t('outer-xml(<a><!-- comment -->{1,2,3}</a>)', '<a><!-- comment -->1 2 3</a>');
  t('outer-xml(<a><!-- co<! <? <aas aas asa sas mment -->{1,2,3}</a>)', '<a><!-- co<! <? <aas aas asa sas mment -->1 2 3</a>');
  t('outer-xml(<!-- Tags are ignored in the following section -->)', '<!-- Tags are ignored in the following section -->');

  t('outer-xml(<?piempty?>)', '<?piempty ?>');
  t('outer-xml(<?piempty       ?>)', '<?piempty ?>');
  t('outer-xml(<?pifull     foobar?>)', '<?pifull foobar?>');
  t('outer-xml(<?pispace     balls   ?>)', '<?pispace balls   ?>');
  t('outer-xml(<a><?piempty?></a>)', '<a><?piempty ?></a>');
  t('outer-xml(<a><?piempty       ?></a>)', '<a><?piempty ?></a>');
  t('outer-xml(<a><?pispace     balls   ?></a>)', '<a><?pispace balls   ?></a>');
  t('outer-xml(<a>{1,2}<?pispace     balls   ?>8</a>)', '<a>1 2<?pispace balls   ?>8</a>');
  t('outer-xml(<?format role="output" ?>)', '<?format role="output" ?>');


  t('<hallo>welt</hallo> / text()', 'welt');
  t('<hallo>welt</hallo>/text()', 'welt');
  t('<hallo>welt</hallo> /text()', 'welt');
  t('<hallo foo="bar" maus="haus">welt</hallo> / @*', 'bar haus');
  t('<hallo foo="bar" maus="haus">welt</hallo>/@*', 'bar haus');
  t('outer-xml(let $x := (1,2,<a>s</a>,<a>t</a> / text(),3) return <t h="{$x}">(: ass :){(: ass :)$x}</t>)', '<t h="1 2 s t 3">(: ass :)1 2<a>s</a>t3</t>');
  t('outer-xml(<t h="{(1,2,<a>s</a>,<a>t</a> / text(),3)}">(: ass :){(: ass :)(1,2,<a>s</a>,<a>t</a> / text(),3)(:?:)(::)}</t>)', '<t h="1 2 s t 3">(: ass :)1 2<a>s</a>t3</t>');




  t('outer-xml(element empty {})', '<empty/>');
  t('outer-xml(element main {"haus"})', '<main>haus</main>');
  t('outer-xml(element main { element foobar {} })', '<main><foobar/></main>');
  t('outer-xml(element main { element foobar {1+2,3+4} })', '<main><foobar>3 7</foobar></main>');
  t('outer-xml(element {"niam"} { element {concat("foo", "BAR")} {1+2,3+4} })', '<niam><fooBAR>3 7</fooBAR></niam>');

  t('outer-xml(element empty {attribute test {} })', '<empty test=""/>');
  t('outer-xml(element empty {attribute test {1+2+3} })', '<empty test="6"/>');
  t('outer-xml(element empty {attribute test {1 to 3} })', '<empty test="1 2 3"/>');
  t('outer-xml(element empty {attribute test {1 to 3}, attribute maus {16} })', '<empty test="1 2 3" maus="16"/>');

  t('outer-xml(element a { text { 13 } })', '<a>13</a>');
  t('outer-xml(element a { text { 13 }, text{ 17 } })', '<a>1317</a>');
  t('outer-xml(element a { text { 13, 17 }, text{18,19,20} })', '<a>13 1718 19 20</a>');
  t('outer-xml(element a { text { 13, <el>*</el> }, text{18,19,20} })', '<a>13 *18 19 20</a>');
  t('outer-xml(element a { text { 13, <el>*</el>, <el>?</el> }, text{18,19,20} })', '<a>13 * ?18 19 20</a>');
  t('count(element a {  } / text())', '0');
  t('count(<a>{ () }</a> / text())', '0');
  t('count(<a>{ text { 13 }, "23", text{ 17 } }</a> / text())', '1');
  t('count(<a> a {1} b {2} c  </a> / text())', '1');
  t('count(<a> a {1} b <foobar/>{2} c  </a> / text())', '2');
  t('count(element a { text { 13 } } / text())', '1');
  t('count(element a { text { 13 }, text{ 17 } } / text())', '1');
  t('count(element a { text { 13 }, "23", text{ 17 } } / text())', '1');

  t('outer-xml(element a { processing-instruction pipi {  } })', '<a><?pipi ?></a>');
  t('outer-xml(element a { processing-instruction pipi { 13 } })', '<a><?pipi 13?></a>');
  t('outer-xml(element a { processing-instruction pipi { 13, 14, 15 } })', '<a><?pipi 13 14 15?></a>');
  t('outer-xml(  processing-instruction pipi {  })', '<?pipi ?>');
  t('outer-xml(  processing-instruction pipi { 13  })', '<?pipi 13?>');
  t('outer-xml( processing-instruction pipi { 13, 14, 15  })', '<?pipi 13 14 15?>');
  t('outer-xml(  processing-instruction {concat("pi", "PI") } { "lang" })', '<?piPI lang?>');

  t('outer-xml(element a { comment { () } })', '<a><!----></a>');
  t('outer-xml(element a { comment { 13 } })', '<a><!--13--></a>');
  t('outer-xml(element a { comment { 13, 14, 15 } })', '<a><!--13 14 15--></a>');
  t('outer-xml(  comment { 13  })', '<!--13-->');
  t('outer-xml( comment { 13, 14, 15  })', '<!--13 14 15-->');

  t('outer-xml(let $e := <foo bar="bacon" a="tla" appa="jipjip">21</foo> return element {fn:node-name($e)} {$e/@*, 2 * fn:data($e)})', '<foo bar="bacon" a="tla" appa="jipjip">42</foo>');
  t('let $e := <address>123 Roosevelt Ave. Flushing, NY 11368</address> return   name($e)', 'address');
  t('let $e := <address>123 Roosevelt Ave. Flushing, NY 11368</address> return   $e/node()', '123 Roosevelt Ave. Flushing, NY 11368');
  t('let $dict := <dictionary><entry word="address"><variant xml:lang="de">Adresse</variant><variant xml:lang="it">indirizzo</variant></entry></dictionary> return $dict/entry/@word', 'address');
  t('let $dict := <dictionary><entry word="address"><variant xml:lang="de">Adresse</variant><variant xml:lang="it">indirizzo</variant></entry></dictionary> return $dict/entry/variant/@xml:lang', 'de it');
  t('outer-xml(let $dict := <dictionary><entry word="address"><variant xml:lang="de">Adresse</variant><variant xml:lang="it">indirizzo</variant></entry></dictionary>, $e := <address>123 Roosevelt Ave. Flushing, NY 11368</address> return   element{$dict/entry[@word=name($e)]/variant[@xml:lang="it"]}    {$e/@*, $e/node()})',
    '<indirizzo>123 Roosevelt Ave. Flushing, NY 11368</indirizzo>');
  t('outer-xml(<r> { let $sex := "M" return attribute   { if ($sex = "M") then "husband" else "wife" }   { <a>Hello</a>, 1 to 3, <b>Goodbye</b> } } </r>)', '<r husband="Hello 1 2 3 Goodbye">  </r>');
  t('outer-xml(<r> { let $sex := "www" return attribute   { if ($sex = "M") then "husband" else "wife" }   { <a>Hello</a>, 1 to 3, <b>Goodbye</b> } } </r>)', '<r wife="Hello 1 2 3 Goodbye">  </r>');
  t('outer-xml(document{  <author-list>{ "ralf isau" }</author-list> })', '<author-list>ralf isau</author-list>');
  t('outer-xml(let $target := "audio-output", $content := "beep" return processing-instruction {$target} {$content})', '<?audio-output beep?>');
  t('outer-xml(let $homebase := "Houston" return comment {fn:concat($homebase, ", we have a problem.")})', '<!--Houston, we have a problem.-->');


  t('let $s := (<one/>, <two/>, <three/>) return outer-xml(<out>{$s}</out>)', '<out><one/><two/><three/></out>');
  t('for $s in (<one/>, <two/>, <three/>) return outer-xml(<out>{$s}</out>)', '<out><one/></out> <out><two/></out> <out><three/></out>');
  t('outer-xml(let $bib := <bib>   <book>     <title>TCP/IP Illustrated</title>     <author>Stevens</author>     <publisher>Addison-Wesley</publisher>   </book>   <book>     <title>Advanced Programming in the Unix Environment</title>     <author>Stevens</author>     <publisher>Addison-Wesley</publisher>   </book>   <book>     <title>Data on the Web</title>     <author>Abiteboul</author>     <author>Buneman</author>     <author>Suciu</author>   </book> </bib>' +
    'return <authlist>  {    for $a in fn:distinct-values($bib/book/author)    order by $a    return      <author>         <name> {$a} </name>         <books>           {             for $b in $bib/book[author = $a]             order by $b/title             return $b/title            }         </books>      </author>  } </authlist>)',
    '<authlist>  <author>         <name> Abiteboul </name>         <books>           <title>Data on the Web</title>         </books>      </author><author>         <name> Buneman </name>         <books>           <title>Data on the Web</title>         </books>      </author><author>         <name> Stevens </name>         <books>           <title>Advanced Programming in the Unix Environment</title><title>TCP/IP Illustrated</title>         </books>      </author><author>         <name> Suciu </name>         <books>           <title>Data on the Web</title>         </books>      </author> </authlist>');

  t('<a>{5}</a> instance of xs:integer', 'false');
  t('(5, 6) instance of xs:integer+', 'true');

  t('every $x in (for $i in (1 to 100) return let $a := <a/>, $b := <b/> return ((($a << $b) or ($a >> $b))) and not((($a << $b) and ($a >> $b))))  satisfies $x', 'true');

  //some examples from wikibooks
  t('let $doc := <doc><books>  <book><title>HHGTTG</title><price>42</price></book>  <book><title>Mistborn</title><price>123</price></book>  <book><title>Das Kapital</title><price>0</price></book>  <book><title>Pinguin</title><price>57</price></book> </books></doc> '+
     'for $book in $doc/books/book let $title := $book/title/text() let $price := $book/price/text() where xs:decimal($price) gt 50.00  order by $title '+
     'return outer-xml(<book><title>{$title}</title><price>{$price}</price></book>)',
     '<book><title>Mistborn</title><price>123</price></book> <book><title>Pinguin</title><price>57</price></book>');
  t('outer-xml(let $message := ''Hello World!'' return <results><message>{$message}</message></results>)', '<results><message>Hello World!</message></results>');
  t('outer-xml(<list>{for $i in (1 to 10)  return <value>{$i}</value> }</list>)', '<list><value>1</value><value>2</value><value>3</value><value>4</value><value>5</value><value>6</value><value>7</value><value>8</value><value>9</value><value>10</value></list>');
  t('outer-xml(let $my-doc := <doc><terms><term><term-name>Object</term-name><definition>A set of ideas...</definition></term><term><term-name>Organization</term-name><definition>A unit...</definition></term><term><term-name>Organization</term-name><definition>BankOfAmerica</definition></term></terms></doc>'+
    'return <html><head><title>Terms</title> </head>  <body> <table border="1"> <thead> <tr>  <th>Term</th>  <th>Definition</th>  </tr> </thead>'+
           '<tbody>{ for $term at $count in  for $item in $my-doc/terms/term  let $term-name := $item/term-name/text()  order by upper-case($term-name)  return $item ' +
                   'return  <tr> {if ($count mod 2) then (attribute bgcolor {''Lavender''}) else ()} <td>{$term/term-name/text()}</td>  <td>{$term/definition/text()}</td>  </tr>       }</tbody>  </table> </body> </html>)',
    '<html><head><title>Terms</title> </head>  <body> <table border="1"> <thead> <tr>  <th>Term</th>  <th>Definition</th>  </tr> </thead><tbody><tr bgcolor="Lavender">  <td>Object</td>  <td>A set of ideas...</td>  </tr><tr>  <td>Organization</td>  <td>A unit...</td>  </tr><tr bgcolor="Lavender">  <td>Organization</td>  <td>BankOfAmerica</td>  </tr></tbody>  </table> </body> </html>');

  ps.StaticContext.StripBoundarySpace := true;

  t('outer-xml(<a>  {1}  </a>)', '<a>1</a>');
  t('outer-xml(<a> {1} {2} {3} </a>)', '<a>123</a>');
  t('outer-xml(<a>{1,2,3} </a>)', '<a>1 2 3</a>');
  t('outer-xml(<a>  <empty/>  </a>)', '<a><empty/></a>');
  t('outer-xml(<a>  <b> as </b>  </a>)', '<a><b> as </b></a>');
  t('outer-xml(<a>  {1} {"  "} {3}  </a>)', '<a>1  3</a>');
  t('outer-xml(<a><empty att="  hallo  "/></a>)', '<a><empty att="  hallo  "/></a>');
  t('outer-xml(<a><empty att="  {''hallo''}  "/></a>)', '<a><empty att="  hallo  "/></a>');

  t('outer-xml(<a>  {"abc"}  </a>)', '<a>abc</a>');
  t('outer-xml(<a> z {"abc"}</a>)', '<a> z abc</a>');
  t('outer-xml(<a>&#x20;{"abc"}</a>)', '<a> abc</a>');
  t('outer-xml(<a>{"  "}</a>)', '<a>  </a>');



  t('typeswitch (1) case xs:integer return "int" case xs:decimal return "deci" case xs:double return "double" default return "unknown"', 'int');
  t('typeswitch (1.0) case xs:integer return "int" case xs:decimal return "deci" case xs:double return "double" default return "unknown"', 'deci');
  t('typeswitch (1.0E1) case xs:integer return "int" case xs:decimal return "deci" case xs:double return "double" default return "unknown"', 'double');
  t('typeswitch ("as") case xs:integer return "int" case xs:decimal return "deci" case xs:double return "double" default return "unknown"', 'unknown');
  t('typeswitch (1) default return "defalone"', 'defalone');
  t('typeswitch (1) default $var return "defalone: $var;"', 'defalone: 1');
  t('typeswitch (1) case $i as xs:integer return "int: $i;" default $var return "def: $var;"', 'int: 1');
  t('typeswitch (1.0) case $i as xs:integer return "int: $i;" default $var return "def: $var;"', 'def: 1');
  t('typeswitch (1) case xs:integer return "int" default return "unknown"', 'int');
  t('typeswitch (1) case integer return "int" default return "unknown"', 'int');
  t('let $a := 1 return typeswitch($a) case $x as integer return $x * 20 default $b return $b', '20');
  t('let $a := 1.0 return typeswitch($a) case $x as integer return $x * 20 default $b return $b', '1');
  t('let $a := 1 return typeswitch($a) case integer return $a * 20 default return $a', '20');
  t('let $a := 1.0 return typeswitch($a) case integer return $a * 20 default return $a', '1');
  t('typeswitch(<element>123</element>) case $x as element() return outer-xml($x) default $y return "atomic: $y;"', '<element>123</element>');
  t('typeswitch(12345) case $x as element() return outer-xml($x) default $y return "atomic: $y;"', 'atomic: 12345');
  t('typeswitch(<element>123</element>) case $x as element(element) return outer-xml($x) default $y return "atomic: $y;"', '<element>123</element>');
  t('typeswitch(<element>123</element>) case $x as element(foobar) return outer-xml($x) default $y return "atomic: $y;"', 'atomic: 123');
  t('typeswitch(<foobar>123</foobar>) case $x as element(foobar) return outer-xml($x) default $y return "atomic: $y;"', '<foobar>123</foobar>');
  t('typeswitch(<element>123</element>) case $x as element(*) return outer-xml($x) default $y return "atomic: $y;"', '<element>123</element>');
  t('typeswitch(<!--comment!-->) case $x as element(*) return outer-xml($x) default $y return "atomic: $y;"', 'atomic: comment!');
  t('typeswitch(<!--comment!-->) case $x as element(*) return outer-xml($x) case comment() return "comm" default $y return "atomic: $y;"', 'comm');
  t('typeswitch(<?PI?>) case $x as element(*) return outer-xml($x) case comment() return "comm" case processing-instruction() return "pipi" default $y return "atomic: $y;"', 'pipi');
  t('typeswitch(<?PI?>) case $x as element(*) return outer-xml($x) case comment() return "comm" case processing-instruction(PI) return "pipi" default $y return "atomic: $y;"', 'pipi');
  t('typeswitch(<?PI?>) case $x as element(*) return outer-xml($x) case comment() return "comm" case processing-instruction(pim) return "pipi" default $y return "atomic: $y;"', 'atomic: ');
  t('typeswitch(<?PI?>) case $x as element(*) return outer-xml($x) case comment() return "comm" case processing-instruction("PI") return "pipi" default $y return "atomic: $y;"', 'pipi');
  t('typeswitch(<?PI?>) case $x as element(*) return outer-xml($x) case comment() return "comm" case processing-instruction("pim") return "pipi" default $y return "atomic: $y;"', 'atomic: ');

  t('<element>123</element> instance of element()', 'true');
  t('12345 instance of element()', 'false');
  t('<element>123</element> instance of element(element)', 'true');
  t('<element>123</element> instance of element(foobar)', 'false');
  t('<foobar>123</foobar> instance of element(foobar)', 'true');
  t('<element>123</element> instance of element(*)', 'true');
  t('<element>123</element> instance of comment()', 'false');
  t('<!--comment!--> instance of element(*)', 'false');
  t('<!--comment!--> instance of comment()', 'true');
  t('(<!--comment!-->, <!--abc-->) instance of comment()', 'false');
  t('(<!--comment!-->, <!--abc-->) instance of comment()+', 'true');
  t('(<!--comment!-->, <!--abc-->) instance of comment()*', 'true');
  t('() instance of comment()', 'false');
  t('() instance of comment()*', 'true');
  t('() instance of comment()?', 'true');
  t('(<foobar/>, <foobar/>) instance of element(foobar)', 'false');
  t('(<foobar/>, <foobar/>) instance of element(foobar)+', 'true');
  t('(<foobar/>, <foobar/>) instance of element(foobar)*', 'true');
  t('(<foobar/>, <foobar/>) instance of element(foobar)  +', 'true');
  t('(<foobar/>, <foobar/>, <abc/>) instance of element(foobar)+', 'false');

  ps.GlobalNamespaces.Add(TNamespace.Create('test://abc', 'abc'));
  t('<abc:element>123</abc:element> instance of element()', 'true');
  t('<abc:element>123</abc:element> instance of element(*)', 'true');
  t('<abc:element>123</abc:element> instance of element(element)', 'false');
  t('<abc:element>123</abc:element> instance of element(*:element)', 'true');
  t('<abc:element>123</abc:element> instance of element(abc:element)', 'true');


  t('unordered { <element>abc</element> }', 'abc');
  t('ordered { <element>abc</element> }', 'abc');
  t('for $i in unordered { <element>abc</element> } return string-length($i/text())', '3');
  t('for $i in ordered { <element>abc</element> } return string-length($i/text())', '3');
  //t('validate strict { <element>abc</element> }', 'abc');


  t('(# foobar:def abc #) {7}', '7');
  t('(#foobar:def#) {7, 8}', '7 8');

  m('xquery version "1.0"; 7', '7');
  m('xquery version "1.0" encoding "utf-8"; 7', '7');

  m('declare boundary-space preserve ; outer-xml(<a>  {7}  </a>)', '<a>  7  </a>');
  m('declare boundary-space strip ; outer-xml(<a>  {7}  </a>)', '<a>7</a>');
  m('declare boundary-space preserve; outer-xml(<a>  {7}  </a>)', '<a>  7  </a>');
  m('declare boundary-space strip; outer-xml(<a>  {7}  </a>)', '<a>7</a>');
  m('declare boundary-space preserve; outer-xml(<a>  7  </a>)', '<a>  7  </a>');
  m('declare boundary-space strip; outer-xml(<a>  7  </a>)', '<a>  7  </a>');

  m('declare variable $x := 7.5; $x', '7.5');
  m('declare variable $x as xs:integer := 7; $x', '7');
  m('declare variable $x as xs:string := "abc"; $x', 'abc');

  m('declare function foobar() { 17 }; foobar() ', '17');
  m('declare function succi($a as integer) { $a + 1 }; succi(20) ', '21');
  m('declare function summi($a as integer, $b as integer) { $a + $b }; summi(20, 10) ', '30');
  m('declare function mul($a as integer, $b as integer) { $a * $b }; mul(20, 10) ', '200');
  m('declare function succi($a as integer) { $a + 1 }; declare function mul($a as integer, $b as integer) { $a * $b }; succi(mul(2,3)) ', '7');
  m('declare function succi($a as integer) { $a + 1 }; declare function mulsucc($a as integer, $b as integer) { succi($a * $b) }; mulsucc(5,6) ', '31');
  m('declare function fac($a as integer) { if ($a = 0) then 1 else $a * fac($a - 1)  }; fac(4) ', '24');
  m('declare function fac($a as integer) { if ($a = 0) then 1 else $a * fac($a - 1)  }; fac(10) ', '3628800');
  m('declare function fibi($a as integer) { if ($a <= 0) then 1 else fibi($a - 1) + fibi($a - 2)  }; fibi(5) ', '13');
  m('declare function fibi($a as integer) { if ($a <= 0) then 1 else fibi($a - 1) + fibi($a - 2)  }; fibi(6) ', '21');

  m('declare variable $var := 123;  declare function wrapper($a as integer) { $var * $a }; wrapper(1) ', '123');
  m('declare variable $var := 123;  declare function wrapper($a as integer) { $var * $a }; wrapper(2) ', '246');
  m('declare function wrapper($a as integer) { $var * $a }; declare variable $var := 123;  wrapper(3) ', '369'); //forward variable reference (if i read the standard correctly that is not allowed. But it is easier to implement this way and in Zorba it also works)
  m('declare function odd($a as integer) { if ($a = 0) then false() else even($a - 1)}; declare function even($a as integer) { if ($a = 0) then true() else odd($a - 1)}; string-join(for $i in 0 to 9 return odd($i), " ") ', 'false true false true false true false true false true');

  //some realworld examples from stackoverflow
  t('let $x := 1, $seq := (2,4,7,11,16) for $temp at $pos in $seq return $seq[$pos] - if ($pos eq 1) then $x else $seq[$pos - 1]', '1 2 3 4 5');
  t('let $pVal := 1, $vList := (2,4,7,11,16), $vList2 := ($pVal, subsequence($vList, 1, count($vList)-1)) return for $i in 1 to count($vList) return $vList[$i] - $vList2[$i]', '1 2 3 4 5');


  mr('module namespace test = "pseudo://test-module"; declare function test:internalref(){ concat($test:var, ":", test:func()) } declare variable $test:var := 123; declare function test:func(){ 456 }');
  m('import module "pseudo://test-module"; $test:var', '123');
  m('import module "pseudo://test-module"; test:func()', '456');
  m('import module "pseudo://test-module"; test:internalref()', '123:456');
  m('import module namespace rename = "pseudo://test-module"; $rename:var', '123');
  m('import module namespace rename = "pseudo://test-module"; rename:func()', '456');
  m('import module namespace rename = "pseudo://test-module"; rename:internalref()', '123:456');
  m('import module namespace rename = "pseudo://test-module"; $test:var', ''); //or raise error?

  mr('module namespace test2 = "pseudo://test-module2"; import module "pseudo://test-module"; declare function test2:sumcalc($param){ concat("SUM: ", sum($param)) } declare function test2:wrapwrap() { test:internalref() } ');
  m('import module "pseudo://test-module2"; test2:sumcalc((1,2,3))', 'SUM: 6');
  m('import module "pseudo://test-module2"; test2:wrapwrap()', '123:456');
  m('import module namespace renamed = "pseudo://test-module2"; renamed:sumcalc((1,2,3))', 'SUM: 6');
  m('import module namespace renamed = "pseudo://test-module2"; renamed:wrapwrap()', '123:456');
  m('import module "pseudo://test-module2"; declare namespace indirect = "pseudo://test-module2"; indirect:sumcalc((90,1))', 'SUM: 91');
  m('import module "pseudo://test-module2"; declare namespace indirect = "pseudo://test-module2"; indirect:wrapwrap()', '123:456');
  m('import module "pseudo://test-module2"; declare namespace indirect = "pseudo://test-module2"; concat(test2:wrapwrap(), indirect:sumcalc((90,1)))', '123:456SUM: 91');

  m('declare function local:test(){15}; local:test()', '15');
  m('declare namespace xyz = "foobar"; declare function xyz:test(){-6}; xyz:test()', '-6');
  m('declare namespace xyz = "foobar"; declare namespace alias = "foobar"; declare function xyz:test(){"assaa"}; alias:test()', 'assaa');
  m('declare namespace xyz = "foobar"; declare namespace alias = "foobar"; declare function alias:test(){"aqe"}; xyz:test()', 'aqe');

  m('declare variable $local:test := 123; $local:test', '123');
  m('declare namespace xyz = "foobar"; declare variable $xyz:test := -666; $xyz:test', '-666');
  m('declare namespace xyz = "foobar"; declare namespace alias = "foobar"; declare variable $xyz:test := "oddy"; $alias:test', 'oddy');
  m('declare namespace xyz = "foobar"; declare namespace alias = "foobar"; declare variable $alias:test := "nemo"; $xyz:test', 'nemo');

  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; string-join(for $xsy:abc in (1,2,3) return $xsy:abc, " ")', '1 2 3');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; string-join(for $xsy:abc in (1,2,3, 4) return $foobar:abc, " ")', '1 2 3 4');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; string-join(for $foobar:abc in (1,2,3) return $xsy:abc, " ")', '1 2 3');

  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $xsy:abc in (1,2,3) satisfies $xsy:abc mod 2 = 1', 'false');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $xsy:abc in (1,3) satisfies $xsy:abc mod 2 = 1', 'true');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $xsy:abc in (1,2,3) satisfies $foobar:abc mod 2 = 1', 'false');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $xsy:abc in (1,3) satisfies $foobar:abc mod 2 = 1', 'true');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $foobar:abc in (1,2,3) satisfies $xsy:abc mod 2 = 1', 'false');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; every $foobar:abc in (1,3) satisfies $xsy:abc mod 2 = 1', 'true');

  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $xsy:abc in (1,2,3) satisfies $xsy:abc mod 2 = 0', 'true');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $xsy:abc in (1,3) satisfies $xsy:abc mod 2 = 0', 'false');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $xsy:abc in (1,2,3) satisfies $foobar:abc mod 2 = 0', 'true');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $xsy:abc in (1,3) satisfies $foobar:abc mod 2 = 0', 'false');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $foobar:abc in (1,2,3) satisfies $xsy:abc mod 2 = 0', 'true');
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $foobar:abc in (1,3) satisfies $xsy:abc mod 2 = 0', 'false');

  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $foobar:abc in (1,2,3) satisfies $abc mod 2 = 0', 'false'); //undefined variable becomes (). Raise error?
  m('declare namespace xsy = "abc"; declare namespace foobar = "abc"; some $abc in (1,2,3) satisfies $xsy:abc mod 2 = 0', 'false');

  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; "abc" eq "ABC"', 'true');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-sensitive-clever"; "abc" eq "ABC"', 'false');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; "ABCx" eq "ABC"', 'false');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; "ABC" eq "ABC"', 'true');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-sensitive-clever"; "ABC" eq "ABC"', 'true');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; "9foobar" lt "10foobar"', 'true');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; "9foobar" gt "10foobar"', 'false');
  m('declare default collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; "9foobar" lt "10foobar"', 'false');
  m('declare default collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; "9foobar" gt "10foobar"', 'true');

  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'first second');
  m('declare default collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; string-join(<r><A>first</A><a>second</a></r> / A, " ") ', 'first second');
  m('declare default collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'second');
  m('declare default collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; string-join(<r><A>first</A><a>second</a></r> / A, " ") ', 'first');

  m('declare namespace temp = "http://www.benibela.de/2012/pxp/extensions"; declare option temp:default-node-collation "http://www.benibela.de/2012/pxp/case-insensitive-clever" string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'first second');
  m('declare namespace temp = "http://www.benibela.de/2012/pxp/extensions"; declare option temp:default-node-collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; string-join(<r><A>first</A><a>second</a></r> / A, " ") ', 'first second');
  m('declare namespace temp = "http://www.benibela.de/2012/pxp/extensions"; declare option temp:default-node-collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'second');
  m('declare namespace temp = "http://www.benibela.de/2012/pxp/extensions"; declare option temp:default-node-collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; string-join(<r><A>first</A><a>second</a></r> / A, " ") ', 'first');
  m('declare option pxp:default-node-collation "http://www.benibela.de/2012/pxp/case-insensitive-clever" string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'first second');
  m('declare option pxp:default-node-collation "http://www.benibela.de/2012/pxp/case-insensitive-clever"; string-join(<r><A>first</A><a>second</a></r> / A, " ") ', 'first second');
  m('declare option pxp:default-node-collation "http://www.w3.org/2005/xpath-functions/collation/codepoint"; string-join(<r><A>first</A><a>second</a></r> / a, " ") ', 'second');
  m('declare option pxp:extended-strings "on"; declare variable $foobar := 123; "var is $foobar;."', 'var is 123.');
  m('declare option pxp:extended-strings "off"; declare variable $foobar := 123; "var is $foobar;."', 'var is $foobar;.');
  m('declare option pxp:extended-strings "on"; declare option pxp:extended-strings "toggle";  declare variable $foobar := 123; "var is $foobar;."', 'var is $foobar;.');
  m('declare option pxp:extended-strings "off"; declare option pxp:extended-strings "toggle";  declare variable $foobar := 123; "var is $foobar;."', 'var is 123.');
  m('xquery version "1.0"; declare option pxp:extended-strings "off"; declare option pxp:extended-strings "toggle";  declare variable $foobar := 123; "var is $foobar;."', 'var is 123.');


  m('declare default order empty least; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty least return $i, " ")', '1 2 3 4 5');
  m('declare default order empty least; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty greatest return $i, " ")', '3 4 5 1 2');
  m('declare default order empty least; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 return $i, " ")', '1 2 3 4 5');
  m('declare default order empty greatest; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty least return $i, " ")', '1 2 3 4 5');
  m('declare default order empty greatest; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 empty greatest return $i, " ")', '3 4 5 1 2');
  m('declare default order empty greatest; string-join(for $i in (1,2,3,4,5) order by if ($i < 3) then () else 1 return $i, " ")', '3 4 5 1 2');

  m('import schema namespace test="http://www.w3.org/2001/XMLSchema"; 5 instance of test:integer', 'true');
  m('import schema namespace test="http://www.w3.org/2001/XMLSchema"; 5 instance of xs:integer', 'true');
  m('import schema namespace foobar="http://www.w3.org/2001/XMLSchema"; 5 instance of foobar:integer', 'true');
  m('xquery version "1.0"; import schema namespace test="http://www.w3.org/2001/XMLSchema"; 5 instance of test:integer', 'true');
  m('import schema namespace foobar="xyz"; 5 instance of foobar:integer', 'true'); //TODO: arbitrary schemas
  m('import schema namespace test="http://www.w3.org/2001/XMLSchema"; 5 instance of test:double', 'false');
  m('import schema namespace test="http://www.w3.org/2001/XMLSchema"; 5 instance of xs:double', 'false');
  m('import schema namespace foobar="http://www.w3.org/2001/XMLSchema"; 5 instance of foobar:double', 'false');
  m('import schema namespace foobar="xyz"; 5 instance of foobar:double', 'false'); //TODO: arbitrary schemas

  helper := THelper.Create;
  ps.OnDeclareExternalVariable:=@helper.DeclareExternalVariableEvent;
  ps.OnDeclareExternalFunction:=@helper.DeclareExternalFunctionEvent;

  m('declare variable $test-import1 external; $test-import1', '42');
  m('declare variable $test-import1 as xs:integer external; $test-import1', '42');
  m('declare variable $test-import1 as xs:decimal external; $test-import1', '42');
  m('declare variable $test-import2 external; $test-import2', 'hallo');
  m('declare variable $test-import1 external; declare variable $test-import2 as xs:string external; concat($test-import1, $test-import2)', '42hallo');
  m('declare variable $test-importNS external; $test-importNS', '');
  m('declare namespace foobar = "xyz"; declare variable $foobar:test-importNS external; $foobar:test-importNS', 'xyz');
  m('declare namespace foobar = "tripple"; declare variable $foobar:test-importNS external; $foobar:test-importNS', 'tripple');

  m('declare function test-importfunc1() external; test-importfunc1()', 'func-result');
  m('declare function test-importfunc2($a as integer, $b as integer) external; test-importfunc2(5,6)', '30');
  m('declare function test-importfunc2($a as integer, $b as integer) external; test-importfunc2(4,10)', '40');
  m('declare function test-importfunc1()  as xs:string external; test-importfunc1()', 'func-result');
  m('declare function test-importfunc2($a as integer, $b as integer) as xs:integer external; test-importfunc2(5,6)', '30');
  m('declare function test-importfunc3() external; test-importfunc3()', 'native!');

  m('declare variable $test-import1 external;  declare function test-importfunc2($a as integer, $b as integer) external; test-importfunc2($test-import1, 10)', '420');
  m('xquery version "1.0"; declare variable $test-import1 external;  declare function test-importfunc2($a as integer, $b as integer) external; test-importfunc2($test-import1, 10)', '420');


  m('declare namespace xx = "http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/xx:bing', 'Lentils'); //global trim trims returned values even if the tree contains the whitespace
  m('declare namespace xx="http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/xx:bing', 'Lentils'); //global trim trims returned values even if the tree contains the whitespace
  m('xquery version "1.0"; declare namespace xx="http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/xx:bing', 'Lentils'); //global trim trims returned values even if the tree contains the whitespace
  m('declare namespace xx = "http://example.org/"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/xx:bing', '');
  m('declare namespace xx = "http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return namespace-uri($i/xx:bing)', 'http://example.org');
  m('declare namespace foo = "http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/foo:bing', 'Lentils');
  m('declare namespace foo = "http://example.org/"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/foo:bing', '');
  m('declare default element namespace "http://example.org"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/bing', 'Lentils');
  m('declare default element namespace "http://example.org/"; let $i := <foo:bar xmlns:foo = "http://example.org"> <foo:bing> Lentils </foo:bing>  </foo:bar> return $i/bing', '');
  m('declare default element namespace "http://example.org"; let $i := <bar> <bing> Lentils </bing>  </bar> return $i/bing', 'Lentils');
  m('declare default element namespace "http://example.org"; let $i := <bar> <bing> Lentils </bing>  </bar> return fn:namespace-uri($i/bing)', 'http://example.org');
  m('declare default element namespace "http://example.org"; let $i := <bar> <bing> Lentils </bing>  </bar> return namespace-uri($i/bing)', 'http://example.org');
  m('declare default element namespace "http://example.org"; let $i := <bar xmlns="override"> <bing> Lentils </bing>  </bar> return $i/bing', '');
  m('declare default element namespace "http://example.org"; declare namespace test = "override"; let $i := <bar xmlns="override"> <bing> Lentils </bing>  </bar> return $i/test:bing', 'Lentils');
  m('declare default element namespace "http://example.org"; declare namespace test = "override"; let $i := <bar xmlns="override"> <bing xmlns="override2"> Lentils </bing>  </bar> return $i/test:bing', '');
  m('declare default element namespace "http://example.org"; declare namespace test = "override"; declare namespace foobar = "override2"; let $i := <bar xmlns="override"> <bing xmlns="override2"> Lentils </bing>  </bar> return $i/test:bing', '');
  m('declare default element namespace "http://example.org"; declare namespace test = "override"; declare namespace foobar = "override2"; let $i := <bar xmlns="override"> <bing xmlns="override2"> Lentils </bing>  </bar> return $i/foobar:bing', 'Lentils');
  m('declare default element namespace "http://example.org"; declare namespace test = "override";  let $i := <bar xmlns="http://example.org"> <bing> Lentils </bing>  </bar> return $i/test:bing', '');



  //Tests based on failed XQTS tests
  t('empty(text {"some text"}/..)', 'true');
  t('empty(document-uri(attribute name {"content"}))', 'true');
  t('attribute foobar {} instance of attribute()', 'true');
  t('attribute foobar {} instance of element()', 'false');
  t('attribute foobar {} instance of document-node()', 'false');
  t('document {element a {}} instance of attribute()', 'false');
  t('document {element a {}} instance of element()', 'false');
  t('document {element a {}} instance of document-node()', 'true');
  t('<foobar>abcxyz</foobar> / (/) / (/)', 'abcxyz');
  t('<foobar>abcxyz</foobar> / (root()) / (/) / (root())', 'abcxyz');
  t('document { <foobar>abcxyz</foobar> } / (root()) / (/) / (root())', 'abcxyz');
  t('<foobar>abcxyz</foobar> / (root()) / (/) / (root()) instance of element()', 'true');
  t('document { <foobar>abcxyz</foobar> } / (root()) / (/) / (root()) instance of element()', 'false');
  t('<foobar>abcxyz</foobar> / (root()) / (/) / (root()) instance of document-node()', 'false');
  t('document { <foobar>abcxyz</foobar> } / (root()) / (/) / (root()) instance of document-node()', 'true');
  t('count(<a/> | <a/>)' , '2');
  t('let $a := <a/> return count($a | $a)' , '1');
  t('let $a := <a/> return count($a | $a | $a)' , '1');
  t('let $a := <a/> return count($a | $a | $a | <a/> | <a/>)' , '3');
  t('node-name(<pre:foobarx xmlns:pre="testNSX"/>) instance of xs:QName', 'true');
  t('node-name(<pre:foobarx xmlns:pre="testNSX"/>)', 'pre:foobarx');
  t('local-name-from-QName(node-name(<foobar xmlns="testNS"/>))', 'foobar');
  t('prefix-from-QName(node-name(<foobar xmlns="testNS"/>))', '');
  t('namespace-uri-from-QName(node-name(<foobar xmlns="testNS"/>))', 'testNS');
  t('local-name-from-QName(node-name(<pre:foobarx xmlns:pre="testNSX"/>))', 'foobarx');
  t('prefix-from-QName(node-name(<pre:foobarx xmlns:pre="testNSX"/>))', 'pre');
  t('namespace-uri-from-QName(node-name(<pre:foobarx xmlns:pre="testNSX"/>))', 'testNSX');
  t('QName("http://www.w3.org/2005/xpath-functions", "prefix:local") eq xs:QName("fn:local")', 'true');
  t('QName("http://www.w3.org/2005/xpath-functionsX", "prefix:local") eq xs:QName("fn:local")', 'false');
  t('QName("http://www.w3.org/2005/xpath-functions", "prefix:local") eq ("fn:local" cast as xs:QName)', 'true');
  t('QName("http://www.w3.org/2005/xpath-functionsX", "prefix:local") eq ("fn:local" cast as xs:QName)', 'false');
  t('outer-xml(element {"elem"} {"text"})', '<elem>text</elem>');
  t('outer-xml(element {(), "elem"} {"text"})', '<elem>text</elem>');
  t('outer-xml(element {"elem", ()} {"text"})', '<elem>text</elem>');
  //t('outer-xml(element {"elem", "ent"} {"text"})', '<elem>text</elem>'); should be error
  t('(# abc:def #) {123}', '123');
  t('(# abc:def "afas" #) {123}', '123');
  t('(# abc:def "afas#" #) {123}', '123');
  t('(# abc:def # # ) # ) #) {123}', '123');
  t('(# abc:def # # ) # ) #) (# h:k #) {123}', '123');
  t('(# abc:def # # ) # ) #) (: ..(#. :) (# h:k #) {123}', '123');
  t('let $i := <e xml:lang="en"> <b xml:lang="de"/> </e> return lang("de", $i)', 'false');
  t('fn:local-name(processing-instruction PITarget {"PIcontent"})', 'PITarget');
  t('fn:name(processing-instruction PITarget {"PIcontent"})', 'PITarget');
  t('fn:local-name-from-QName(node-name(processing-instruction PITarget {"PIcontent"}))', 'PITarget');
  t('empty(<e/>/../.)', 'true');
  t('empty(<e/>/../..)', 'true');
  t('empty(<e/>/../ (/) / ..)', 'true');
  t('empty(<e/>/../ (/) / .. / root() / .. / . / (/) / ..)', 'true');
  t('empty(fn:root(<e/>/..))', 'true');
  t('some $i in (1,2,3) satisfies $i eq 2', 'true');
  t('every $i in (1,2,3) satisfies $i eq 2', 'false');
  t('some $i as xs:integer in (1,2,3) satisfies $i eq 2', 'true');
  t('every $i as xs:integer  in (1,2,3) satisfies $i eq 2', 'false');
  t('some $i as xs:integer  in (1,2,3), $j as xs:integer  in (1,2,3)  satisfies $i eq $j', 'true');
  t('every $i as xs:integer  in (1,2,3), $j as xs:integer  in (1,2,3)  satisfies $i eq $j', 'false');
  m('xquery version ''1.0''; 1+2+3', '6');
  t('for $x in ((<a>1</a>, <a>2</a>) except (<a>3</a>, <a>4</a>)) order by $x return $x', '1 2');
  t('for $x in (let $a := <a>0</a> return ($a, <a>1</a>, <a>2</a>) except ($a, <a>3</a>, <a>4</a>)) order by $x return $x', '1 2');
  t('for $x in (let $a := <a>0</a> return ($a, <a>1</a>, <a>2</a>, <a>3</a>, <a>4</a>) except ($a, <a>3</a>, <a>4</a>)) order by $x return $x', '1 2 3 4');
  t('for $x in (let $a := <a>0</a>, $b := <a>7</a> return ($a, $b, <a>1</a>, <a>2</a>, <a>3</a>, <a>4</a>) except ($a, $b, <a>3</a>, <a>4</a>)) order by $x return $x', '1 2 3 4');
  t('let $a := <a>0</a> return ($a, <a>1</a>, <a>2</a>) intersect ($a, <a>3</a>, <a>4</a>)', '0');
  t('for $x in (let $a := <a>0</a>, $b := <a>-1</a> return ($a, $b, <a>1</a>, <a>2</a>) intersect ($a, $b, <a>3</a>, <a>4</a>)) order by $x return $x', '-1 0' );
  t('element a {"b"}', 'b');
  t('document { document { element a {"b"}}}', 'b');
  t('outer-xml(<a xmlns="example">abc</a>)', '<a xmlns="example">abc</a>');
  t('outer-xml(<test:a xmlns:test="123">abc</test:a>)', '<test:a xmlns:test="123">abc</test:a>');
  m('declare namespace test = "foobar"; outer-xml(<test:a>abc</test:a>)', '<test:a xmlns:test="foobar">abc</test:a>');
  m('declare default element namespace "www.example.org"; outer-xml(<a xmlns="www.example.org">abc</a>)', '<a xmlns="www.example.org">abc</a>');
  m('declare namespace test = "foobar"; outer-xml(<test:a xmlns:test="123">abc</test:a>)', '<test:a xmlns:test="123">abc</test:a>');
  m('declare namespace test = "foobar"; declare namespace abc = "def";  outer-xml(<test:a><abc:xyz/></test:a>)', '<test:a xmlns:test="foobar"><abc:xyz xmlns:abc="def"/></test:a>');
  m('declare namespace test = "foobar"; declare namespace abc = "def";  outer-xml(<test:a abc:u=""><abc:xyz/></test:a>)', '<test:a xmlns:test="foobar" xmlns:abc="def" abc:u=""><abc:xyz/></test:a>');
  t('outer-xml(<a xmlns:abc="123"/>)', '<a xmlns:abc="123"/>');
  t('outer-xml(<abc:a xmlns:abc="123"/>)', '<abc:a xmlns:abc="123"/>');
  t('outer-xml(<a xmlns:abc="123" abc:xyz="123"/>)', '<a xmlns:abc="123" abc:xyz="123"/>');
  t('outer-xml(<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem>)', '<elem xmlns:foo="http://www.example.com/foo"><elem foo:attr=""/></elem>');
  m('declare namespace p="http://example.com/ns/p"; declare namespace q="http://example.com/ns/q"; declare namespace f="http://example.com/ns/f"; outer-xml(<p:a q:b="{2}" xmlns:r="http://example.com/ns/r"/>)', '<p:a xmlns:r="http://example.com/ns/r" xmlns:p="http://example.com/ns/p" xmlns:q="http://example.com/ns/q" q:b="2"/>');
  m('declare namespace p="http://example.com/ns/p"; declare namespace q="http://example.com/ns/q"; declare namespace f="http://example.com/ns/f"; string-join(in-scope-prefixes(<p:a q:b="{2}" xmlns:r="http://example.com/ns/r"/>), " ")', 'xml r p q');
  t('<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @xmlns:*', '');
  t('<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @xmlns:foo', '');
  t('<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @*:foo', '');
  t('count(<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @xmlns:*)', '0');
  t('count(<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @xmlns:foo)', '0');
  t('count(<elem xmlns:foo="http://www.example.com/foo">{element elem {attribute {"foo:attr"} {}}}</elem> / @*:foo)', '0');
  t('outer-xml(element {"hallo"} {()})', '<hallo/>');
  m('declare namespace xyz = "foobar"; outer-xml(element {"xyz:hallo"} {()})', '<xyz:hallo xmlns:xyz="foobar"/>');
  m('declare namespace abc = "123"; outer-xml(element {"abc:hallo"} {()})', '<abc:hallo xmlns:abc="123"/>');
  t('outer-xml(element {fn:QName("ans", "pref:hallo")} {()})', '<pref:hallo xmlns:pref="ans"/>');
  t('outer-xml(element {fn:QName("ans", "hallo")} {()})', '<hallo xmlns="ans"/>');
  m('declare namespace xyz = "foobar"; outer-xml(element {"hallo"} {attribute {"xyz:test"} {123} })', '<hallo xmlns:xyz="foobar" xyz:test="123"/>');
  m('declare namespace abc = "123"; outer-xml(element {"hallo"} {attribute {"abc:test"} {1234}})', '<hallo xmlns:abc="123" abc:test="1234"/>');
  t('outer-xml(element hallo {attribute {fn:QName("ans", "pref:test")} {1234}})', '<hallo xmlns:pref="ans" pref:test="1234"/>');
  t('outer-xml(element hallo {attribute {fn:QName("ans", "test")} {1234}})', '<hallo xmlns:XXX="ans" XXX:test="1234"/>');
  m('declare namespace xyz = "foobar"; outer-xml(element {"hallo"} {attribute {"xyz:test"} {123} ,"#" })', '<hallo xmlns:xyz="foobar" xyz:test="123">#</hallo>');
  m('declare namespace abc = "123"; outer-xml(element {"hallo"} {attribute {"abc:test"} {1234} ,"#"})', '<hallo xmlns:abc="123" abc:test="1234">#</hallo>');
  t('outer-xml(element hallo {attribute {fn:QName("ans", "pref:test")} {1234} ,"#"})', '<hallo xmlns:pref="ans" pref:test="1234">#</hallo>');
  t('outer-xml(element hallo {attribute {fn:QName("ans", "test")} {1234} ,"#"})', '<hallo xmlns:XXX="ans" XXX:test="1234">#</hallo>');
  m('declare namespace xyz = "foobar"; outer-xml(<hallo>{attribute {"xyz:test"} {123} ,"#" }</hallo>)', '<hallo xmlns:xyz="foobar" xyz:test="123">#</hallo>');
  m('declare namespace abc = "123"; outer-xml(<hallo>{attribute {"abc:test"} {1234} ,"#"}</hallo>)', '<hallo xmlns:abc="123" abc:test="1234">#</hallo>');
  t('outer-xml(<hallo>{attribute {fn:QName("ans", "pref:test")} {1234} ,"#"}</hallo>)', '<hallo xmlns:pref="ans" pref:test="1234">#</hallo>');
  t('outer-xml(<hallo>{attribute {fn:QName("ans", "test")} {1234} ,"#"}</hallo>)', '<hallo xmlns:XXX="ans" XXX:test="1234">#</hallo>');
  t('let $a := 7 let $a := $a + 10 return $a', '17');
  t('nilled( <!-- abc --> )', '');

  t('attribute foobar { } instance of attribute()', 'true');
  t('attribute foobar { } instance of attribute(foobar)', 'true');
  t('attribute foobar { } instance of attribute(xyz)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute pre1:foobar { } instance of attribute(pre1:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute pre1:foobar { } instance of attribute(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute pre2:foobar { } instance of attribute(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {"pre1:foobar"} { } instance of attribute(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {"pre2:foobar"} { } instance of attribute(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {fn:QName("pp2", "pre1:foobar")} { } instance of attribute(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {fn:QName("pp2", "pre2:foobar")} { } instance of attribute(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {fn:QName("pp1", "pre1:foobar")} { } instance of attribute(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; attribute {fn:QName("pp1", "pre2:foobar")} { } instance of attribute(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; attribute foobar { } instance of attribute(foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; attribute foobar { } instance of attribute(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; attribute foobar { } instance of attribute(pre1:foobar)', 'false');
  t('attribute foobar { } instance of attribute(*)', 'true');
  t('attribute foobar { } instance of attribute(*, xs:untypedAtomic)', 'true');
  t('attribute foobar { } instance of attribute(*, xs:integer)', 'false');
  t('attribute foobar { } instance of element()', 'false');
  t('attribute foobar { } instance of element(*)', 'false');

  t('element foobar { } instance of element()', 'true');
  t('element foobar { } instance of element(foobar)', 'true');
  t('element foobar { } instance of element(xyz)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element pre1:foobar { } instance of element(pre1:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element pre1:foobar { } instance of element(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element pre2:foobar { } instance of element(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {"pre1:foobar"} { } instance of element(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {"pre2:foobar"} { } instance of element(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {fn:QName("pp2", "pre1:foobar")} { } instance of element(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {fn:QName("pp2", "pre2:foobar")} { } instance of element(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {fn:QName("pp1", "pre1:foobar")} { } instance of element(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; element {fn:QName("pp1", "pre2:foobar")} { } instance of element(pre2:foobar)', 'false');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; element foobar { } instance of element(foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; element foobar { } instance of element(pre2:foobar)', 'true');
  m('declare namespace pre1 = "pp1"; declare namespace pre2 = "pp2"; declare default element namespace "pp2"; element foobar { } instance of element(pre1:foobar)', 'false');
  t('element foobar { } instance of element(*)', 'true');
  t('element foobar { } instance of element(*, xs:anyType)', 'true');
  t('element foobar { } instance of element(*, xs:untyped)', 'true');
  t('element foobar { } instance of element(*, xs:integer)', 'false');
  t('element foobar { } instance of attribute()', 'false');
  t('element foobar { } instance of attribute(*)', 'false');

  t('document { element xyz {} } instance of document-node()', 'true');
  t('element abc { element xyz {} } instance of document-node()', 'false');
  t('document { element xyz {} } instance of document-node(element())', 'true');
  t('document { element xyz {} } instance of document-node(element(*))', 'true');
  t('document { element xyz {} } instance of document-node(element(foobar))', 'false');
  t('document { element foobar {} } instance of document-node(element())', 'true');
  t('document { element foobar {} } instance of document-node(element(*))', 'true');
  t('document { element foobar {} } instance of document-node(element(foobar))', 'true');
  t('element foobar { element foobar {} } instance of document-node(element(foobar))', 'false');
  t('document { <foobar/> } instance of document-node(element(foobar))', 'true');
  t('document { <foobar xmlns="abc"/> } instance of document-node(element(foobar))', 'false');
  m('declare namespace def = "abc"; document { <foobar xmlns="abc"/> } instance of document-node(element(def:foobar))', 'true');
  m('declare namespace def = "abc"; document { <foobar xmlns="abc"/>, <hallo/> } instance of document-node(element(def:foobar))', 'false');

  t('let $x := document { 123 } return <element>{$x, $x}</element>', '123123');
  t('let $x := text { "xyz" } return <element>{$x, $x}</element>', 'xyzxyz');
  t('let $x := element a { "xyz" } return <element>{$x, $x}</element>', 'xyzxyz');
  t('outer-xml(let $x := element a { "xyz" } return <element>{$x, $x}</element>)', '<element><a>xyz</a><a>xyz</a></element>');
  t('let $x := attribute a { "xyz" } return <element>{$x, $x}</element>', '');

  t('let $x := <a/> return deep-equal($x, $x)', 'true');
  t('let $x := <abc/> return deep-equal($x, $x)', 'true');
  t('let $x := <abc/> return deep-equal($x, ($x, 123))', 'false');
  t('let $x := <abc/> return deep-equal(($x, 123), ($x, 123))', 'true');
  t('let $x := <abc/> return deep-equal(($x, <h>123</h>), ($x, <h>123</h>))', 'true');
  t('let $x := <abc/> return deep-equal(($x, <h>123<foo/></h>), ($x, <h>123</h>))', 'false');
  t('let $x := <abc/> return deep-equal(($x, <h>123<foo/></h>), ($x, <h>123<foo/></h>))', 'true');
  t('let $x := <abc/> return deep-equal(($x, <h>123<foo/></h>), ($x, <h>123<foo>123</foo></h>))', 'false');
  t('let $x := <abc/> return deep-equal(($x, <h>123<foo>{$x}</foo></h>), ($x, <h>123<foo>{$x}</foo></h>))', 'true');
  t('let $x := <abc/> return deep-equal(($x, <h>123<foo>{$x}</foo></h>), ($x, <h>123<foo t="a">{$x}</foo></h>))', 'false');
  //t('outer-xml(let $x := attribute a { "xyz" } return <element>{$x, $x}</element>)', '<element a="xyz" a="xyz"/>');

  //timing('subsequence((1 to 1000), 200, 600)[0]', '');
  //timing('(for $i in (1 to 50), $j in (1 to 50)  return ($i))[0]', '');

  t('let $x as element() := <abc/> return name($x)', 'abc');
  t('let $x as element()+ := <abc/> return name($x)', 'abc');
  t('let $x as element()* := <abc/> return name($x)', 'abc');
  t('let $x as element()? := <abc/> return name($x)', 'abc');
  t('let $x as element(abc) := <abc/> return name($x)', 'abc');
  t('let $x as element(abc)+ := <abc/> return name($x)', 'abc');
  t('let $x as element(abc)* := <abc/> return name($x)', 'abc');
  t('let $x as element(abc)? := <abc/> return name($x)', 'abc');
  t('let $x as element():= <abc/> return name($x)', 'abc');
  t('let $x as element()+:= <abc/> return name($x)', 'abc');
  t('let $x as element()*:= <abc/> return name($x)', 'abc');
  t('let $x as element()?:= <abc/> return name($x)', 'abc');
  t('let $x as element(abc):= <abc/> return name($x)', 'abc');
  t('let $x as element(abc)+:= <abc/> return name($x)', 'abc');
  t('let $x as element(abc)*:= <abc/> return name($x)', 'abc');
  t('let $x as element(abc)?:= <abc/> return name($x)', 'abc');

  m('import schema default element namespace "http://www.example.com/typedecl"; fn:concat(1,2,3)', '123');
  m('import schema default element namespace "http://www.example.com/typedecl"; pxp:concat(1,2,3)', '123');
  m('import schema default element namespace "http://www.example.com/typedecl"; concat(1,2,3)', '123');
  m('import schema "http://www.example.com/typedecl"; concat(1,2,3)', '123');
  m('import schema ''http://www.example.com/typedecl''; concat(1,2,3)', '123');

  m('declare function f($a as xs:anyAtomicType) { $a }; f(10) instance of xs:integer', 'true');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(10) instance of xs:decimal', 'true');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(10) instance of xs:double', 'false');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(10.0) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(10.0) instance of xs:decimal', 'true');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(10.0) instance of xs:double', 'false');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(1e1) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(1e1) instance of xs:decimal', 'false');
  m('declare function f($a as xs:anyAtomicType) { $a }; f(1e1) instance of xs:double', 'true');

  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10) instance of xs:integer', 'true');
  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10) instance of xs:decimal', 'true');
  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10) instance of xs:double', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10.0) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10.0) instance of xs:decimal', 'true');
  m('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(10.0) instance of xs:double', 'false');
  {f('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(1e1) instance of xs:integer', 'false');
  f('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(1e1) instance of xs:decimal', 'false');
  f('declare function f($a as xs:anyAtomicType) as xs:decimal { $a }; f(1e1) instance of xs:double', 'true'); conversion fail}

  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10) instance of xs:decimal', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10) instance of xs:double', 'true');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10.0) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10.0) instance of xs:decimal', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(10.0) instance of xs:double', 'true');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(1e1) instance of xs:integer', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(1e1) instance of xs:decimal', 'false');
  m('declare function f($a as xs:anyAtomicType) as xs:double { $a }; f(1e1) instance of xs:double', 'true');

  //m('declare function f($a as xs:anyAtomicType) { $a }; f((10,20)) ', 'false');
  t('() instance of xs:anyAtomicType', 'false');
  //m('declare function f($a as xs:anyAtomicType) { $a }; f(()) ', 'false');
  m('declare function f($a as xs:anyAtomicType?) { $a }; empty(f(())) ', 'true');
  m('declare function f($a as xs:anyAtomicType+) { $a }; string-join( f((10,20)) , " ")', '10 20');
  m('declare function f($a as xs:anyAtomicType*) { $a }; string-join( f(()) , " ")', '');
  m('declare function f($a as xs:anyAtomicType*) { $a }; string-join( f((10)) , " ")', '10');
  m('declare function f($a as xs:anyAtomicType*) { $a }; string-join( f((10,20)) , " ")', '10 20');

  m('declare function f($a as xs:decimal) {  $a instance of xs:integer }; string-join(for $i in (1, 1.0) return f($i), " ")', 'true false');
  m('declare function f($a as xs:double) {  $a instance of xs:integer }; string-join(for $i in (1, 1.0, 1e1) return f($i), " ")', 'false false false');

  m('declare function f($a as xs:double+) { $a }; string-join(for $i in f((1, 1.0, xs:float(1), xs:double(1))) return ($i instance of xs:double), " ")', 'true true true true');
  m('declare function f($a as xs:decimal+) { $a }; string-join(for $i in f((1, 1.0, xs:short(7))) return ($i instance of xs:decimal), " ")', 'true true true');
  m('declare function f($a as xs:decimal+) { $a }; string-join(for $i in f((1, 1.0, xs:short(7))) return ($i instance of xs:integer), " ")', 'true false true');
  m('declare function f($a as xs:decimal+) { $a }; string-join(for $i in f((1, 1.0, xs:short(7))) return ($i instance of xs:short), " ")', 'false false true');
  m('declare function f($a as xs:decimal+) { $a }; string-join(for $i in f((1, 1.0, xs:short(7))) return ($i instance of xs:double), " ")', 'false false false');
  m('declare function f($a as xs:decimal+) as xs:double+ { $a }; string-join(for $i in f((1, 1.0, xs:short(7), text { " 8 " })) return ($i instance of xs:double), " ")', 'true true true true');

  t('outer-xml(<a xmlns="foobar"><b xmlns="xyz"/></a>/*:b)', '<b xmlns="xyz"/>');
  t('outer-xml(<a xmlns="foobar"><b xmlns=""/></a>/b)', '<b/>');
  t('outer-xml(<a xmlns="foobar"><b xmlns=""/></a>)', '<a xmlns="foobar"><b xmlns=""/></a>');
  t('outer-xml(<a xmlns:pref="foobar"><b xmlns:pref="xyz"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz"><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:pref="foobar"><b xmlns:pref=""><pref:c>..</pref:c></b></a>/b)', '<b><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:pref="foobar" xmlns:a1="a1" xmlns:a2="a2"><b xmlns:pref="xyz"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz" xmlns:a1="a1" xmlns:a2="a2"><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:pref="foobar" xmlns:a1="a1" xmlns:a2="a2"><b xmlns:pref=""><pref:c>..</pref:c></b></a>/b)', '<b xmlns:a1="a1" xmlns:a2="a2"><pref:c>..</pref:c></b>'); //should raise error
  t('outer-xml(<a xmlns:a1="a1" xmlns:a2="a2" xmlns:pref="foobar"><b xmlns:pref="xyz"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz" xmlns:a1="a1" xmlns:a2="a2"><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:a1="a1" xmlns:a2="a2" xmlns:pref="foobar"><b xmlns:pref=""><pref:c>..</pref:c></b></a>/b)', '<b xmlns:a1="a1" xmlns:a2="a2"><pref:c>..</pref:c></b>'); //should raise error


  m('declare function local:f($local:v){ $local:v + 1}; local:f(2)', '3');
  m('declare variable $local:v := 17; declare function local:f($local:v){ $local:v + 1}; local:f(2)', '3');


  t('outer-xml(<a xmlns:pref="foobar"><b xmlns:pref="xyz" xmlns:b1="b1" xmlns:b2="b2"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz" xmlns:b1="b1" xmlns:b2="b2"><pref:c>..</pref:c></b>');
  m('declare default function namespace "foobar"; pxp:outer-xml(<foobar/>)', '<foobar/>');

  //no copying => option independent
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)',              '<c xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)',              '<c xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def" xmlns:g="abc"/>');

  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g=""></c></b></a> // *:c)',              '<c xmlns:h="def"/>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def"/>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g=""></c></b></a> // *:c)',              '<c xmlns:h="def"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def"/>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def"/>');

  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)',              '<c xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)',              '<c xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)', '<c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)', '<c xmlns="test" xmlns:h="def" xmlns:g="abc"/>');

  //with copying
  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)}</k>)',              '<k><c/></k>');
  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c/></k>');
  m('declare copy-namespaces no-preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test"/></k>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)}</k>)',              '<k><c/></k>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c/></k>');
  m('declare copy-namespaces no-preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test"/></k>');

  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)}</k>)',              '<k><c xmlns:h="def" xmlns:g="abc"/></k>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/></k>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test" xmlns:h="def" xmlns:g="abc"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c></c></b></a> // *:c)}</k>)',              '<k><c xmlns:h="def" xmlns:g="abc"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c xmlns:t=".." xmlns:h="def" xmlns:g="abc"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test" xmlns:h="def" xmlns:g="abc"/></k>');

  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g=""></c></b></a> // *:c)}</k>)',              '<k><c xmlns:h="def"/></k>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c xmlns:t=".." xmlns:h="def"/></k>');
  m('declare copy-namespaces    preserve, no-inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test" xmlns:h="def"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g=""></c></b></a> // *:c)}</k>)',              '<k><c xmlns:h="def"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns:t=".."></c></b></a> // *:c)}</k>)', '<k><c xmlns:t=".." xmlns:h="def"/></k>');
  m('declare copy-namespaces    preserve,    inherit ; outer-xml(<k>{(<a xmlns:g="abc"><b xmlns:h="def"><c xmlns:g="" xmlns="test"></c></b></a> // *:c)}</k>)', '<k><c xmlns="test" xmlns:h="def"/></k>');

  //inheriting tests
  m('declare copy-namespaces preserve, inherit; let $prev := <x>y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x xmlns:uv="4">y</x>');
  m('declare copy-namespaces preserve, no-inherit; let $prev := <x>y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x>y</x>');
  m('declare copy-namespaces preserve, inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x xmlns:abc="foobar" xmlns:uv="4">y</x>');
  m('declare copy-namespaces preserve, no-inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x xmlns:abc="foobar">y</x>');
  m('declare copy-namespaces no-preserve, inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x xmlns:uv="4">y</x>');
  m('declare copy-namespaces no-preserve, no-inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<t xmlns:uv="4"> { $prev } </t> // x)', '<x>y</x>');

  m('declare copy-namespaces preserve, inherit; let $prev := <x>y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x xmlns:uv="4">y</x>');
  m('declare copy-namespaces preserve, no-inherit; let $prev := <x>y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x>y</x>');
  m('declare copy-namespaces preserve, inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x xmlns:abc="foobar" xmlns:uv="4">y</x>');
  m('declare copy-namespaces preserve, no-inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x xmlns:abc="foobar">y</x>');
  m('declare copy-namespaces no-preserve, inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x xmlns:uv="4">y</x>');
  m('declare copy-namespaces no-preserve, no-inherit; let $prev := <x xmlns:abc="foobar">y</x> return outer-xml(<r><t xmlns:uv="4"> { $prev } </t></r> // x)', '<x>y</x>');

  //t('outer-xml(<a xmlns:pref="foobar"><b xmlns:pref="" xmlns:b1="b1" xmlns:b2="b2"><pref:c>..</pref:c></b></a>/b)', '<b xmlns:pref=""><pref:c>..</pref:c></b>');
  {t('outer-xml(<a xmlns:pref="foobar" xmlns:a1="a1" xmlns:a2="a2"><b xmlns:pref="xyz" xmlns:b1="b1" xmlns:b2="b2"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz" xmlns:b1="b1" xmlns:b2="b2" xmlns:a1="a1" xmlns:a2="a2"><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:pref="foobar" xmlns:a1="a1" xmlns:a2="a2"><b xmlns:pref="" xmlns:b1="b1" xmlns:b2="b2"><pref:c>..</pref:c></b></a>/b)', '<b xmlns:pref=""><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:a1="a1" xmlns:a2="a2" xmlns:pref="foobar"><b xmlns:b1="b1" xmlns:b2="b2" xmlns:pref="xyz"><pref:c>..</pref:c></b></a>/*:b)', '<b xmlns:pref="xyz"><pref:c>..</pref:c></b>');
  t('outer-xml(<a xmlns:a1="a1" xmlns:a2="a2" xmlns:pref="foobar"><b xmlns:b1="b1" xmlns:b2="b2" xmlns:pref=""><pref:c>..</pref:c></b></a>/b)', '<b xmlns:pref=""><pref:c>..</pref:c></b>');
  t('outer-xml(<e xmlns="http://www.example.com/A" xmlns:A="http://www.example.com/C"><b xmlns:B="http://www.example.com/C" xmlns=""/></e>/b)', '<b xmlns=""/>'); //XQTS test
  }

  m('string-join(in-scope-prefixes(<a></a>), ":")', 'xml');
  m('string-join(in-scope-prefixes(<a xmlns="abc"></a>), ":")', 'xml:');
  m('string-join(in-scope-prefixes(<a xmlns:pre="abc"></a>), ":")', 'xml:pre');
  m('string-join(in-scope-prefixes(<a xmlns:pre="abc"><b></b></a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces preserve, inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc"><b></b></a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces no-preserve, inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc"><b></b></a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces preserve, no-inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc"><b></b></a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces no-preserve, no-inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc"><b></b></a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces preserve, inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc">{<b></b>}</a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces no-preserve, inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc">{<b></b>}</a> / b), ":")', 'xml:pre');
  m('declare copy-namespaces preserve, no-inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc">{<b xmlns:pre="foobar"></b>}</a> / b), ":")', 'xml:pre'); //b imports pre during its construction
  m('declare copy-namespaces preserve, no-inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc">{<b></b>}</a> / b), ":")', 'xml:pre'); //b imports pre during its construction
  m('declare copy-namespaces no-preserve, no-inherit; string-join(in-scope-prefixes(<a xmlns:pre="abc">{<b></b>}</a> / b), ":")', 'xml');

  //check that default is preserve, inherit
  m('outer-xml(<a xmlns:pre="abc"><b></b></a> / b)', '<b xmlns:pre="abc"/>');
  m('let $b := <a xmlns:pre="abc"><b></b></a> / b return  outer-xml(<c>{$b}</c> / b)', '<b xmlns:pre="abc"/>');
  m('let $b := <a xmlns:pre="abc"><b></b></a> / b return  outer-xml(<c xmlns:inher="it">{$b}</c> / b)', '<b xmlns:pre="abc" xmlns:inher="it"/>');

  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz xmlns="foobar" xmlns:t="u"/> return outer-xml(<b:x><c>{$a}</c></b:x> / c)', '<c xmlns:b="BNS"><a:xyz xmlns="foobar" xmlns:t="u" xmlns:a="ANS"/></c>' );
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces no-preserve, inherit;    let $a := <a:xyz xmlns="foobar" xmlns:t="u"/> return outer-xml(<b:x><c>{$a}</c></b:x> / c)', '<c xmlns:b="BNS"><a:xyz xmlns:a="ANS"/></c>'  );
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, no-inherit;    let $a := <a:xyz xmlns="foobar" xmlns:t="u"/> return string-join(in-scope-prefixes(<b:x><c>{$a}</c></b:x> / c / a:*), ",")', 'xml,,t,a');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, no-inherit;    let $a := <a:xyz xmlns="foobar" xmlns:t="u"/> return outer-xml(<b:x><c>{$a}</c></b:x> / c)', '<c xmlns:b="BNS"><a:xyz xmlns="foobar" xmlns:t="u" xmlns:a="ANS"/></c>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces no-preserve, no-inherit; let $a := <a:xyz xmlns="foobar" xmlns:t="u"/> return outer-xml(<b:x><c>{$a}</c></b:x> / c)', '<c xmlns:b="BNS"><a:xyz xmlns:a="ANS"/></c>');

  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <b:foo>{$a}</b:foo> return outer-xml($b)', '<b:foo xmlns:b="BNS"><a:xyz xmlns:a="ANS"/></b:foo>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <b:foo>{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns:b="BNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces no-preserve, inherit;    let $a := <a:xyz/>, $b := <b:foo>{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns:b="BNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces preserve, no-inherit;    let $a := <a:xyz/>, $b := <b:foo>{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare copy-namespaces no-preserve, no-inherit; let $a := <a:xyz/>, $b := <b:foo>{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');

  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare namespace c = "CNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <b:foo c:def="123">{$a}</b:foo> return outer-xml($b)', '<b:foo xmlns:b="BNS" xmlns:c="CNS" c:def="123"><a:xyz xmlns:a="ANS"/></b:foo>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare namespace c = "CNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <b:foo c:def="123">{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns:b="BNS" xmlns:c="CNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare namespace c = "CNS"; declare copy-namespaces no-preserve, inherit;    let $a := <a:xyz/>, $b := <b:foo c:def="123">{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns:b="BNS" xmlns:c="CNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare namespace c = "CNS"; declare copy-namespaces preserve, no-inherit;    let $a := <a:xyz/>, $b := <b:foo c:def="123">{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare namespace c = "CNS"; declare copy-namespaces no-preserve, no-inherit; let $a := <a:xyz/>, $b := <b:foo c:def="123">{$a}</b:foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');

  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare default element namespace "defNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <foo b:def="123">{$a}</foo> return outer-xml($b)', '<foo xmlns="defNS" xmlns:b="BNS" b:def="123"><a:xyz xmlns:a="ANS"/></foo>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare default element namespace "defNS"; declare copy-namespaces preserve, inherit;       let $a := <a:xyz/>, $b := <foo b:def="123">{$a}</foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns="defNS" xmlns:b="BNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare default element namespace "defNS"; declare copy-namespaces no-preserve, inherit;    let $a := <a:xyz/>, $b := <foo b:def="123">{$a}</foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS" xmlns="defNS" xmlns:b="BNS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare default element namespace "defNS"; declare copy-namespaces preserve, no-inherit;    let $a := <a:xyz/>, $b := <foo b:def="123">{$a}</foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');
  m('declare namespace a = "ANS"; declare namespace b = "BNS"; declare default element namespace "defNS"; declare copy-namespaces no-preserve, no-inherit; let $a := <a:xyz/>, $b := <foo b:def="123">{$a}</foo> return outer-xml($b / a:*)', '<a:xyz xmlns:a="ANS"/>');

  t('let $x := <a xmlns="foobar"><b>c</b></a> return outer-xml(<x>{$x}</x>)', '<x><a xmlns="foobar"><b>c</b></a></x>');
  t('let $x := <a xmlns="foobar"><b>c</b></a> return outer-xml(<x>{$x}</x> / *:a)', '<a xmlns="foobar"><b>c</b></a>');
  t('let $x := <a xmlns="foobar"><b>c</b></a> return outer-xml(<x>{$x}</x> / *:a / *:b)', '<b xmlns="foobar">c</b>');
  t('let $x := <x xmlns:preserve="http://www.example.com/preserve"><z/></x> return outer-xml(<y xmlns:inherit="http://www.example.com/inherit">{$x}</y>/x/z)', '<z xmlns:preserve="http://www.example.com/preserve" xmlns:inherit="http://www.example.com/inherit"/>'); //directly taken from XQTS
  m('declare copy-namespaces no-preserve, inherit; let $x := <x xmlns:preserve="http://www.example.com/preserve"><z/></x> return outer-xml(<y xmlns:inherit="http://www.example.com/inherit">{$x}</y>/x/z)', '<z xmlns:inherit="http://www.example.com/inherit"/>'); //directly taken from XQTS
  m('declare copy-namespaces preserve, no-inherit; let $x := <x xmlns:preserve="http://www.example.com/preserve"><z/></x> return outer-xml(<y xmlns:preserve="http://www.example.com/preserve">{$x}</y>/x/z)', '<z xmlns:preserve="http://www.example.com/preserve"/>'); //directly taken from XQTS
  m('declare copy-namespaces no-preserve, no-inherit; let $x := <x xmlns:preserve="http://www.example.com/preserve"><z/></x> return outer-xml(<y xmlns:inherit="http://www.example.com/inherit">{$x}</y>/x/z)', '<z/>'); //directly taken from XQTS

  t('let $d := document { <abc/> } return outer-xml(<foobar>{$d}</foobar>)', '<foobar><abc/></foobar>');
  t('let $d := document { <abc/> } return outer-xml(<foobar>{$d}</foobar> / *)', '<abc/>');
  t('let $d := document { <abc/> } return outer-xml(<foobar>{$d}</foobar> / * / .. )', '<foobar><abc/></foobar>');
  m('declare copy-namespaces preserve, no-inherit; let $d := document { <abc/> } return outer-xml(<foobar xmlns:inh="erit">{$d}</foobar>)', '<foobar xmlns:inh="erit"><abc/></foobar>');
  m('declare copy-namespaces preserve, no-inherit; let $d := document { <abc/> } return outer-xml(<foobar xmlns:inh="erit">{$d}</foobar> / *)', '<abc/>');
  m('declare copy-namespaces preserve, no-inherit; let $d := document { <abc/> } return string-join(for $c in <foobar xmlns:inh="erit">{$d}</foobar> / * return outer-xml($c), ":")', '<abc/>');
  m('declare copy-namespaces preserve, no-inherit; let $d := document { <abc/>, <def/> } return string-join(for $c in <foobar xmlns:inh="erit">{$d}</foobar> / * return outer-xml($c), ":")', '<abc/>:<def/>');
  m('declare copy-namespaces preserve, inherit; let $d := document { <abc/>, <def/> } return string-join(for $c in <foobar xmlns:inh="erit">{$d}</foobar> / * return outer-xml($c), ":")', '<abc xmlns:inh="erit"/>:<def xmlns:inh="erit"/>');

  t('<a>b</a> instance of element(*, xs:anyType)', 'true');
  t('<a>b</a> instance of element(a, xs:anyType)', 'true');
  t('<a>b</a> instance of element(b, xs:anyType)', 'false');
  t('<r><a>b</a></r> / element(*, xs:anyType)', 'b');
  t('<r><a>b</a></r> / element(a, xs:anyType)', 'b');
  t('<r><a>b</a></r> / element(b, xs:anyType)', '');

  t('let $ a := 123 return $'#13'a', '123');
  t('for $ a in 123 return $                a', '123');

  t('string-join(<a><b><c>d</c></b></a> // name(.), ":")', 'a:b:c:');
  t('string-join(<a><b><c>d</c></b><x/></a> // name(.), ":")', 'a:b:c::x');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> / b / following-sibling::* / name(.), ":")', 'x:y');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> / y / preceding-sibling::* / name(.), ":")', 'b:x');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> / b / following::* / name(.), ":")', 'x:y');
  t('string-join(/a/y / preceding::* / name(.), ":")', 'b:c:x', '<a><b><c>d</c></b><x/><y/></a>');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> / y / preceding::* / name(.), ":")', 'b:c:x');
  t('<foobar>text</foobar> / text() / .. / name(.)', 'foobar');
  t('<foobar>text</foobar> / text() / ancestor::* / name(.)', 'foobar');
  t('<foobar><t>text</t></foobar> / * / text() / .. / name(.)', 't');
  t('<foobar><t>text</t></foobar> / * / text() / ancestor::* / name(.)', 'foobar t');
  t('<a><b><c>d</c></b><x/><y/></a> // (concat(name(.),":",.))', 'a:d b:d c:d :d x: y:');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> // string-join(for $i in ./ancestor-or-self::* return name($i), ":"), ",")', 'a,a:b,a:b:c,a:b:c,a:x,a:y');
  t('string-join(<a><b><c>d</c></b><x/><y/></a> // string-join(for $i in ./ancestor::* return name($i), ":"), ",")', ',a,a:b,a:b:c,a,a');
  //repeat some of the XPath 2 tests with constructed elements:
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c[2]/text(), ",")', 'c2,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c[position()=(2,3)]/text(), ",")', 'c2,c3,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c/c/text(), ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c/text(), ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//b/c[2]/text(), ",")', 'c2,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[2]/text(), ",")', 'c2,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[position()=(2,3)]/text(), ",")', 'c2,c3,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[7]/text(), ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c[1]/text(), ",")', 'c1,cx1,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[1]/text(), ",")', 'c1,cx1,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[2]/text(), ",")', 'c2,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b//c[position()=last()]/text(), ",")', 'c4,cx2,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/*/text(), ",")', 'b1,b2,d1,d2,d3,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/node()/text(), ",")', 'b1,b2,d1,d2,d3,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/*/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/node()/c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c, ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/d, ",")', 'd1,d2,d3dxe1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/d/text(), ",")', 'd1,d2,d3', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/f, ",")', 'f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/f/node(), ",")', 'f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/(d,f), ",")', 'd1,d2,d3dxe1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/(d,f)/text(), ",")', 'd1,d2,d3,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b) / (c/c), ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b[2]/c[1]/c[1], ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b[2]/c[2]/c[1], ",")', 'CC1', '');
                //concattenate,union,intersect,except
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b, a/f), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f, a/b), ",")', 'f1,f2,b1c1c2c3c4,b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f, a/b, a/f), ",")', 'f1,f2,b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b | a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/f | a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b union a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/f union a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b | a/f | a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/f | a/b | a/f |a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b[1] | a/f, ",")', 'b1c1c2c3c4,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b[1] | a/f[2], ",")', 'b1c1c2c3c4,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a/b | a/f[2], ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b, a/d) / (e, c/c), ",")', 'CC1,dxe1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b|a/d) / (e, c/c), ",")', 'CC1,dxe1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b, a/d) / (e|c/c), ",")', 'CC1,dxe1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/b|a/d) / (e|c/c), ",")', 'CC1,dxe1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c except a/b/c/c , ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c except a/b/c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() except a/b/c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() except a/b/c/c , ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c except a//c/c , ",")', 'c1,c2,c3,c4,cx1,cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c except a//c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2CC1,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() except a//c/c/text() , ",")', 'c1,c2,c3,c4,cx1,cx2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() except a//c/c , ",")', 'c1,c2,c3,c4,cx1,cx2,CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c intersect a/b/c/c , ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c intersect a/b/c/c/text() , ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() intersect a/b/c/c/text() , ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() intersect a/b/c/c , ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c intersect a//c/c , ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c intersect a//c/c/text() , ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() intersect a//c/c/text() , ",")', 'CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(a//c/text() intersect a//c/c , ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect a/b, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect a/f, ",")', 'f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect a/b[2], ",")', 'b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect a/f[2], ",")', 'f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect (a/b | a/f), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect (a/f | a/d), ",")', 'f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect (a/d), ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) intersect (), ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(() intersect (a/f | a/d), ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except a/b, ",")', 'f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except a/f, ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except a/b[2], ",")', 'b1c1c2c3c4,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except a/f[2], ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except (a/b | a/f), ",")', '', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except (a/f | a/d), ",")', 'b1c1c2c3c4,b2cx1cx2CC1', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except (a/d), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join((a/f | a/b) except (), ",")', 'b1c1c2c3c4,b2cx1cx2CC1,f1,f2', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / string-join(() except (a/f | a/d), ",")', '', '');
                //is,<<,>>
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] is a/b[2])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2] is a/b[1])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] is a/b[1])', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1]/c[1] is a/b/c/c)', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[1] is a/b/c/c)', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[2]/c[1] is a/b/c/c)', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] << a/b[2])', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2] << a/b[1])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] << a/b[1])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1]/c[1] << a/b/c/c)', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[1] << a/b/c/c)', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[2]/c[1] << a/b/c/c)', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] >> a/b[2])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2] >> a/b[1])', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1] >> a/b[1])', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/d[1] >> a/b[1])', 'true');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[1]/c[1] >> a/b/c/c)', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[1] >> a/b/c/c)', 'false');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/b[2]/c[2]/c[1] >> a/b/c/c)', 'false');
                //axes
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (a/child::b)', 'b1c1c2c3c4 b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (child::a/child::b)', 'b1c1c2c3c4 b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (child::a/child::b/child::text())', 'b1 b2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::b,","))', 'b1c1c2c3c4,b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(child::a/child::b,","))', 'b1c1c2c3c4,b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(child::a/child::b/child::text(),","))', 'b1,b2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join((child::a/child::b)[2],","))', 'b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join((child::a/child::b/child::text())[1],","))', 'b1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (node-name((child::a/child::b)[1]))', 'b');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (node-name((child::a/child::b/child::text())[1]))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (node-name((child::a/child::b/child::text()/..)[1]))', 'b');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (node-name((child::a/child::b/child::text()/../..)[1]))', 'a');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/self::a/self::a/self::a/child::b,","))', 'b1c1c2c3c4,b2cx1cx2CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/self::a//text(),","))', 'b1,c1,c2,c3,c4,b2,cx1,cx2,CC1,al,d1,d2,d3,dxe1,f1,f2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/self::b//text(),","))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::b/parent::a/child::b/text(),","))', 'b1,b2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::b/parent::x/child::b/text(),","))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::b[1]/following::c,","))', 'cx1,cx2CC1,CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::b/following::c,","))', 'cx1,cx2CC1,CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/descendant::c/text(),","))', 'c1,c2,c3,c4,cx1,cx2,CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/descendant-or-self::c/text(),","))', 'c1,c2,c3,c4,cx1,cx2,CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::c/descendant::c/text(),","))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/child::c/descendant-or-self::c/text(),","))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/child::c/descendant::c/text(),","))', 'CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/child::c/descendant-or-self::c/text(),","))', 'c1,c2,c3,c4,cx1,cx2,CC1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/following::d/text(),","))', 'd1,d2,d3');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/following-sibling::d/text(),","))', 'd1,d2,d3');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/following::d/text(),","))', 'd1,d2,d3');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/following-sibling::d/text(),","))', '');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/ancestor::c/text(),","))', 'cx2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/ancestor::b/text(),","))', 'b2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/ancestor::a/text(),","))', 'al');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/ancestor::*/text(),","))', 'b2,cx2,al');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/ancestor-or-self::*/text(),","))', 'b2,cx2,CC1,al');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/f/ancestor-or-self::*/text(),","))', 'al,f1,f2');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/f/ancestor::*/text(),","))', 'al');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/f/preceding-sibling::*/text(),","))', 'b1,b2,d1,d2,d3,f1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/f/preceding::*/text(),","))', 'b1,c1,c2,c3,c4,b2,cx1,cx2,CC1,d1,d2,d3,dxe1,f1');
  t('<r><a><b>b1<c>c1</c><c>c2</c><c>c3</c><c>c4</c></b><b>b2<c>cx1</c><c>cx2<c>CC1</c></c></b>al<d>d1</d><d>d2</d><d>d3<e>dxe1</e></d><f>f1</f><f>f2</f></a></r> / (string-join(a/b/c/c/preceding::*/text(),","))', 'b1,c1,c2,c3,c4,cx1');


  t('outer-xml(<a><b><c/></b><x/><y/></a> //c/ ancestor-or-self::*[1])', '<c/>');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 4 return (name(./ancestor-or-self::*[$i])))', 'c b a ');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 4 return (name(ancestor-or-self::*[$i])))', 'c b a ');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 4 return (outer-xml(./ancestor-or-self::*[$i])))', '<c/> <b><c/></b> <a><b><c/></b><x/><y/></a> ');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 4 return (name((ancestor-or-self::*)[$i])))', 'a b c ');

  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 3 return (name(./ancestor::*[$i])))', 'b a ');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 3 return (name(ancestor::*[$i])))', 'b a ');
  t('<a><b><c/></b><x/><y/></a> //c/ (for $i in 1 to 3 return (name((ancestor::*)[$i])))', 'a b ');

  t('outer-xml(<a xml:id="foobar"/>)', '<a xml:id="foobar"/>');
  t('outer-xml(<a>{attribute {QName("'+XMLNamespaceUrl_XML+'", "id")} {123}}</a>)', '<a xml:id="123"/>');

  m('declare base-uri "http://example.org"; base-uri(document { element a {1} })', 'http://example.org');
  m('declare base-uri "http://example.org"; base-uri(element a {1} )', 'http://example.org');
  m('declare base-uri "http://example.org"; base-uri(attribute a {1} )', '');
  m('declare base-uri "http://example.org"; base-uri(comment {1} )', '');
  m('declare base-uri "http://example.org"; base-uri(processing-instruction foobar {1} )', '');
  m('declare base-uri "http://example.org"; base-uri(text {1} )', '');
  m('declare base-uri "http://example.org"; empty(base-uri(attribute a {1} ))', 'true');
  m('declare base-uri "http://example.org"; empty(base-uri(comment {1} ))', 'true');
  m('declare base-uri "http://example.org"; empty(base-uri(processing-instruction foobar {1} ))', 'true');
  m('declare base-uri "http://example.org"; empty(base-uri(text {1} ))', 'true');
  m('declare base-uri "http://example.org"; (base-uri(<a>{attribute a {1} }</a> / @*))', 'http://example.org');
  m('declare base-uri "http://example.org"; (base-uri(<a>{comment {1} }</a> / comment()))', 'http://example.org');
  m('declare base-uri "http://example.org"; (base-uri(<a>{processing-instruction foobar {1} }</a> / processing-instruction() ))', 'http://example.org');
  m('declare base-uri "http://example.org"; (base-uri(<a>{text {1} }</a> / text()))', 'http://example.org');
  m('declare base-uri "http://example.org"; (base-uri(<a xml:base="test">{attribute a {1} }</a> / @a))', 'http://example.org/test');
  m('declare base-uri "http://example.org"; (base-uri(<a xml:base="test">{comment {1} }</a> / comment()))', 'http://example.org/test');
  m('declare base-uri "http://example.org"; (base-uri(<a xml:base="test">{processing-instruction foobar {1} }</a> / processing-instruction() ))', 'http://example.org/test');
  m('declare base-uri "http://example.org"; (base-uri(<a xml:base="test">{text {1} }</a> / text()))', 'http://example.org/test');

  m('declare base-uri "http://example.org"; document-uri(document { element a {1} })', '');
  m('declare base-uri "http://example.org"; document-uri(document { element a {1}, element b {1} })', '');
  m('declare base-uri "http://example.org"; empty(document-uri(document { element a {1}, element b {1} }))', 'true');
  m('declare base-uri "http://example.org"; empty(document-uri(element a {1} ))', 'true');
  m('declare base-uri "http://example.org"; document-uri(document { element a {1}, element b {1} }) instance of xs:string', 'false');

  m('declare base-uri "abc"; static-base-uri()', 'abc');
  m('declare base-uri "   http://abc    def     "; static-base-uri()', 'http://abc def');
  m('declare base-uri "abc"; static-base-uri() instance of xs:string', 'false');
  m('declare base-uri "abc"; static-base-uri() instance of xs:anyURI', 'true');
  m('declare base-uri "   http://abc    def     "; base-uri(<a/>)', 'http://abc def');

  t('count(text {""})', '1');
  t('count(text {()})', '0');
  t('count((text {""}, text {""}))', '2');
  t('let $a := () return count((text {""}, text {$a}, text {""}))', '2');
  t('let $ a := () return count(<a>{$a}</a> / text())', '0');
  t('let $ a := "" return count(<a>{$a}</a> / text())', '0');

  t('count(<a>{(comment {"foobar"})}</a> / comment())', '1');
  t('outer-xml(<a>{""}</a>)', '<a/>');
  t('outer-xml(<a>{("", "", "")}</a>)', '<a>  </a>');
  t('outer-xml(<a>{(text {""})}</a>)', '<a/>');
  t('count(<a>{(text {""})}{""}</a> / text())', '0');
  t('count(<a>{(comment {""})}{""}</a> / comment())', '1');
  t('count(<a>{""}{(comment {""})}{""}</a> / comment())', '1');
  t('count(<a>{" "}{(comment {""})}{""}</a> / comment())', '1');
  t('outer-xml(<a>{ processing-instruction { "  abc  "} { () }}</a>)', '<a><?abc ?></a>');
  t('outer-xml(<a>{ processing-instruction { "  abc  "} { ("   ", "  foo ", "  bar ", "   ") }}</a>)', '<a><?abc foo    bar     ?></a>');

  t('let $x := <x xmlns=""/> return outer-xml(<a xmlns="ANS">{$x}</a>)', '<a xmlns="ANS"><x xmlns=""/></a>');
  t('let $x := <y xmlns=""><x/></y> / *:x return outer-xml(<a xmlns="ANS">{$x}</a>)', '<a xmlns="ANS"><x xmlns=""/></a>');
  t('let $x := <y xmlns=""><x foo="bar"/></y> / *:x return outer-xml(<a xmlns="ANS">{$x}</a>)', '<a xmlns="ANS"><x xmlns="" foo="bar"/></a>');
  t('let $y := <y xmlns=""><x foo="bar"/></y> return outer-xml(<a xmlns="ANS">{$y // *:x}</a>)', '<a xmlns="ANS"><x xmlns="" foo="bar"/></a>');
  t('let $y := <y xmlns="" xmlns:abc="def"><x foo="bar" xmlns:foo="..."/></y> return outer-xml(<a xmlns="ANS">{$y // *:x}</a>)', '<a xmlns="ANS"><x xmlns:foo="..." xmlns="" xmlns:abc="def" foo="bar"/></a>');
  t('let $y := <y xmlns=""><x foo="bar" xmlns:foo="..."/></y> return outer-xml(<a xmlns="ANS">{$y // *:x}</a>)', '<a xmlns="ANS"><x xmlns:foo="..." xmlns="" foo="bar"/></a>');
  m('declare default element namespace "abc"; let $y := <y><x foo="bar" xmlns:foo="..."/></y> return outer-xml(<a xmlns="ANS">{$y // *:x}</a>)', '<a xmlns="ANS"><x xmlns:foo="..." xmlns="abc" foo="bar"/></a>');
  t('let $y := <y><x foo="bar" xmlns:foo="..."/></y> return outer-xml(<a xmlns="ANS">{$y // *:x}</a>)', '<a xmlns="ANS"><x xmlns:foo="..." xmlns="" foo="bar"/></a>');
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><child2 attr="child"/></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><child2 xmlns:foo="http://www.example.com/parent2" xmlns="" attr="child"/></new>'); //XQTS test
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><foo:child2 attr="child"/></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><foo:child2 xmlns:foo="http://www.example.com/parent2" attr="child"/></new>'); //unprefixed attributes are in no ns
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><foo:child2 foo:attr="child"/></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><foo:child2 xmlns:foo="http://www.example.com/parent2" foo:attr="child"/></new>');
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><foo:child2 foo:attr="child">foobar</foo:child2></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><foo:child2 xmlns:foo="http://www.example.com/parent2" foo:attr="child">foobar</foo:child2></new>');
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><foo:child2 foo:attr="child"><!--x--></foo:child2></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><foo:child2 xmlns:foo="http://www.example.com/parent2" foo:attr="child"><!--x--></foo:child2></new>');
  t('outer-xml(for $x in <parent2 xmlns:foo="http://www.example.com/parent2" foo:attr2="attr2"><foo:child2 foo:attr="child"><?pi?></foo:child2></parent2> return <new xmlns="http://www.example.com">{$x//*:child2}</new>)', '<new xmlns="http://www.example.com"><foo:child2 xmlns:foo="http://www.example.com/parent2" foo:attr="child"><?pi ?></foo:child2></new>');
  t('let $a := <a/> return outer-xml(<x xmlns="hallo">{$a}</x>)', '<x xmlns="hallo"><a xmlns=""/></x>');
  t('let $a := element a {()}  return outer-xml(<x xmlns="hallo">{$a}</x>)', '<x xmlns="hallo"><a xmlns=""/></x>');
  t('let $a := <a/> return outer-xml(<x xmlns="hallo">{element {xs:QName("a")} {()}}</x>)', '<x xmlns="hallo"><a/></x>');
  t('let $a := <a/> return outer-xml(<x xmlns="hallo">{element {fn:QName("", "a")} {()}}</x>)', '<x xmlns="hallo"><a xmlns=""/></x>');
  t('let $a := <a/> return outer-xml(<x xmlns="hallo">{element abc {attribute {fn:QName("", "a")} {127} }}</x>)', '<x xmlns="hallo"><abc a="127"/></x>');
  t('let $a := <a/> return outer-xml(<x xmlns="hallo">{element abc {attribute {xs:QName("a")} {127} }}</x>)', '<x xmlns="hallo"><abc xmlns:XXX="hallo" XXX:a="127"/></x>');

  m('outer-xml(<a><b xmlns="foobar"/><c xmlns="foobar"/></a>)', '<a><b xmlns="foobar"/><c xmlns="foobar"/></a>');
  m('declare default element namespace "foobar"; outer-xml(<a><b xmlns="foobar"/></a>)', '<a xmlns="foobar"><b/></a>');
  m('declare default element namespace "foobar"; let $b := <b xmlns="foobar"/> return outer-xml(<a>{$b}</a>)', '<a xmlns="foobar"><b/></a>');
  m('declare default element namespace "foobar"; let $b := <b xmlns="foobar"/> return outer-xml(<a><x xmlns=""/>{$b}</a>)', '<a xmlns="foobar"><x xmlns=""/><b/></a>');

  m('<a xmlns:a="foobar"><b/></a> / string-join(in-scope-prefixes(. / b), ":")', 'xml:a');
  m('<a xmlns:a="foobar"></a> / string-join(in-scope-prefixes(. / <b/>), ":")', 'xml');
  m('let $b := <b/> return <a xmlns:a="foobar">{string-join(in-scope-prefixes($b), ":")}</a> / text()', 'xml');
  m(' <a xmlns:a="foobar">{string-join(in-scope-prefixes(<b/>), ":")}</a> / text()', 'xml:a');


  m('(data(document { element a {"hallo"} }))', 'hallo');
  m('(data(document { <a>hallo</a> }))', 'hallo');
  m('type-of(data(document { element a {"hallo"} }))', 'untypedAtomic');
  m('type-of(data(document { <a>hallo</a> }))', 'untypedAtomic');
  m('(data( element a {"hallo"} ))', 'hallo');
  m('(data( <a>hallo</a> ))', 'hallo');
  m('type-of(data(element a {"hallo"} ))', 'untypedAtomic');
  m('type-of(data(<a>hallo</a> ))', 'untypedAtomic');
  m('(data(  attribute a {"hallo"} ))', 'hallo');
  m('type-of(data( attribute a {"hallo" }))', 'untypedAtomic');
  m('(data(<?hallo welt?>))', 'welt');
  m('(data(processing-instruction hallo {"welt"}))', 'welt');
  m('type-of(data(<?hallo welt?>))', 'string');
  m('type-of(data( processing-instruction hallo {"welt"}))', 'string');
  m('(data(<!--hallo-->))', 'hallo');
  m('(data(comment {"hallo"}))', 'hallo');
  m('type-of(data(<!--hallo-->))', 'string');
  m('type-of(data(comment {"hallo"}))', 'string');
  m('(data(text {"hallo"}))', 'hallo');
  m('type-of(data(text {"hallo"}))', 'untypedAtomic');


  t('<a>1</a> instance of element()', 'true');
  t('max(<a>1</a>) instance of element()', 'false');
  t('min(<a>1</a>) instance of element()', 'false');
  t('sum(<a>1</a>) instance of element()', 'false');
  t('avg(<a>1</a>) instance of element()', 'false');
  t('max(<a>1</a>) instance of xs:double', 'true');
  t('min(<a>1</a>) instance of xs:double', 'true');
  t('sum(<a>1</a>) instance of xs:double', 'true');
  t('avg(<a>1</a>) instance of xs:double', 'true');
  t('max(xs:short(1)) instance of xs:short', 'true');
  t('min(xs:short(1)) instance of xs:short', 'true');
  t('sum(xs:short(1)) instance of xs:short', 'true');
  t('avg(xs:short(1)) instance of xs:short', 'true');
  t('max(xs:untypedAtomic(1)) instance of xs:double', 'true');
  t('min(xs:untypedAtomic(1)) instance of xs:double', 'true');
  t('sum(xs:untypedAtomic(1)) instance of xs:double', 'true');
  t('avg(xs:untypedAtomic(1)) instance of xs:double', 'true');

  m('declare default element namespace "foobar"; namespace-uri-from-QName(xs:QName("localName"))', 'foobar');
  m('declare default element namespace "foobar"; <wiz xmlns="123">{namespace-uri-from-QName(xs:QName("localName"))}</wiz>', '123');
  m('declare default element namespace "foobar"; <wiz xmlns="">{namespace-uri-from-QName(xs:QName("localName"))}</wiz>', '');

  t('outer-xml(<a att0="x'#13'y" att1="ab'#13#13'c" att2="ab'#9#10#13'cd" att3="{"ab'#9#10#13'cd"}" att4="x'#13#10#13#10'y" />)', '<a att0="x y" att1="ab  c" att2="ab   cd" att3="ab'#9#10#10'cd" att4="x  y"/>');
  t('outer-xml(<a>{attribute att2 {"ab'#9#10#13'cd"}}</a>)', '<a att2="ab'#9#10#10'cd"/>');
  t('outer-xml(<a x="&#x9;"/>)', '<a x="'#9'"/>');
  t('outer-xml(<a x="&#xA;"/>)', '<a x="'#10'"/>');
  t('outer-xml(<a x="&#xD;"/>)', '<a x="&#xD;"/>');
  t('outer-xml(<a>&#x9;</a>)', '<a>'#9'</a>');
  t('outer-xml(<a>&#xA;</a>)', '<a>'#10'</a>');
  t('outer-xml(<a>&#xD;</a>)', '<a>&#xD;</a>');
  t('outer-xml(<a>{"", "", ""}</a>)', '<a>  </a>');
  t('outer-xml(<a>{"", (), ""}</a>)', '<a> </a>');
  t('outer-xml(<a x="{"", (), ""}"></a>)', '<a x=" "/>');
  t('outer-xml(<a x="{"", (), ""}">{""}</a>)', '<a x=" "/>');
  t('outer-xml(<a>'#9'</a>)', '<a/>');
  t('outer-xml(<a>'#10'</a>)', '<a/>');
  t('outer-xml(<a>'#13'</a>)', '<a/>');
  m('declare boundary-space preserve; outer-xml(<a>'#9'</a>)', '<a>'#9'</a>');
  m('declare boundary-space preserve; outer-xml(<a>'#10'</a>)', '<a>'#10'</a>');
  m('declare boundary-space preserve; outer-xml(<a>'#13'</a>)', '<a>'#10'</a>');
  t('outer-xml(<a>'#13'</a>)', '<a/>');
  t('outer-xml(<a>'#13'<![CDATA[-]]>   </a>)', '<a>'#10'-   </a>');
  t('outer-xml(<a>'#13'<![CDATA[ ]]>   </a>)', '<a>'#10'    </a>');
  t('outer-xml(<a>'#13'<![CDATA[]]>   </a>)', '<a>'#10'   </a>');
  t('outer-xml(<a><![CDATA[]]>   </a>)', '<a>   </a>');
  t('outer-xml(<a> <![CDATA[]]></a>)', '<a> </a>');
  t('outer-xml(<a>&lt;</a>)', '<a>&lt;</a>');
  t('outer-xml(<a><![CDATA[&lt;]]></a>)', '<a>&amp;lt;</a>');
  t('outer-xml(<!--&lt;-->)', '<!--&lt;-->');
//  t('data(<!--&lt;-->)', '<');
  t('outer-xml(<?abc &lt;?>)', '<?abc &lt;?>');
//  t('data(<?abc &lt;?>', '<');
  m('declare boundary-space strip; outer-xml(<a x="&#x9;"/>)', '<a x="'#9'"/>');
  m('declare boundary-space strip; outer-xml(<a x="&#xA;"/>)', '<a x="'#10'"/>');
  m('declare boundary-space strip; outer-xml(<a x="&#xD;"/>)', '<a x="&#xD;"/>');

  t('outer-xml(<a id="  a  b "/>)', '<a id="  a  b "/>');
  t('outer-xml(<a xml:id="  a  b "/>)', '<a xml:id="a b"/>');
  t('(text {""}, text {""}, text {""}, comment {()}) /(position())', '1 2 3 4');
  m('declare namespace test = "foobar"; <a xmlns:test="foobar" test:test="123"/> / @test:test', '123');
  m('declare namespace test = "foobar"; <a xmlns:test="foobar" test:test="123"/> / namespace-uri(@test:test)', 'foobar');
  m('declare namespace test = "foobar"; <a xmlns:test="foobar" test:test="123"/> / node-name(@test:test)', 'test:test');
  m('declare namespace test = "foobar"; <a xmlns:test="foobar" test:test="123"/> / node-name(.)', 'a');
  m('declare default element namespace "foobar"; <a xmlns:test="foobar" test:test="123"/> / node-name(.)', 'a');


  helper.free;
  xml.free;
  FreeAndNil(ps.GlobalNamespaces);
  ps.free;
  vars.free
end;


type

{ TTermExtension }

 TTermExtension = class(TXQTerm)
  function evaluate(const context: TEvaluationContext): IXQValue; override;
end;

{ TTermExtension }

function TTermExtension.evaluate(const context: TEvaluationContext): IXQValue;
begin
  result := xqvalue('native!');
end;

{ THelper }

constructor THelper.create;
begin
  func1 := TXQTermString.create('func-result');
  func2 := TXQTermBinaryOp.create('*', TXQTermVariable.Create('a', nil), TXQTermVariable.Create('b', nil));;
  func3 := TTermExtension.Create;
end;

destructor THelper.Destroy;
begin
  func1.free;
  func2.free;
  func3.free;
  inherited Destroy;
end;

procedure THelper.DeclareExternalVariableEvent(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;
  const variable: string; var value: IXQValue);
begin
  case variable of
  'test-import1': value := xqvalue(42);
  'test-import2': value := xqvalue('hallo');
  'test-importNS':
    if namespace = nil then value := xqvalue()
    else value := xqvalue(namespace.getURL);
  end;
end;

procedure THelper.DeclareExternalFunctionEvent(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;
  const functionName: string; var value: TXQValueFunction);
begin
  case functionName of
  'test-importfunc1': value.body := func1;
  'test-importfunc2': value.body := func2;
  'test-importfunc3': value.body := func3;
  end;
end;

end.

