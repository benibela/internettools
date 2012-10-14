unit xquery1_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, simplehtmltreeparser;

//test for xquery 1 expressions that are not also xpath 2 expressions, or are listed in the XQuery standard
procedure unittests;

implementation

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
       raise Exception.Create('XPath Test failed: '+IntToStr(count)+ ': '+s1+#13#10'got: "'+got+'" expected "'+s2+'"');
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
var vars: TXQVariableChangeLog;
begin
//  time := Now;
  vars:= TXQVariableChangeLog.create();

  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticBaseUri := 'pseudo://test';
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
  t('outer-xml(<hallo>&quot;</hallo>)', '<hallo>"</hallo>');
  t('outer-xml(<hallo> inline entity: &quot;</hallo>)', '<hallo> inline entity: "</hallo>');
  t('outer-xml(<hallo> inline entities: &lt;&amp;&gt;</hallo>)', '<hallo> inline entities: &lt;&></hallo>'); //lt is escaped again (todo: also escape amp again)
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
  t('outer-xml(<a test="&apos;">5</a>)', '<a test="''">5</a>');
  t('outer-xml(<a test="foo&apos;bar">5</a>)', '<a test="foo''bar">5</a>');
  t('outer-xml(<a test=''{1+2}''>5</a>)', '<a test="3">5</a>');
  t('outer-xml(<a test=''MAUS{1+2}HAUS''>5</a>)', '<a test="MAUS3HAUS">5</a>');
  t('outer-xml(<a test  =  ''MAUS{1+2}HAUS''   >5</a>)', '<a test="MAUS3HAUS">5</a>');
  t('outer-xml(<a test=''{1}{2}{3}''>5</a>)', '<a test="123">5</a>');
  t('outer-xml(<a test=''&apos;''>5</a>)', '<a test="''">5</a>');
  t('outer-xml(<a test=''foo&apos;bar''>5</a>)', '<a test="foo''bar">5</a>');
  t('outer-xml(<a test="xpa""th">5</a>)', '<a test="xpa"th">5</a>');        //TODO: fix tree output
  t('outer-xml(<a test=''xpa''''th''>5</a>)', '<a test="xpa''th">5</a>');
  t('outer-xml(<a test="{<temp>dingdong</temp>}">5</a>)', '<a test="dingdong">5</a>');
  t('outer-xml(<a test="foo{{123}}bar">5</a>)', '<a test="foo{123}bar">5</a>');
  t('outer-xml(<a test="&#x61;&#x20;&#x61;{61}">&#x61;&#x20;&#x61;{61}</a>)', '<a test="a a61">a a61</a>');
  t('outer-xml(<a><![CDATA[]]></a>)', '<a></a>');
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

  ps.StripBoundarySpace := true;

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

  xml.free;
  ps.free;
  vars.free
end;

end.

