program xqueryExample;

uses heaptrc, xquery,  simpleinternet;

var v: IXQValue;
begin
  writeln('Print the numbers of 1 to 5 and multiply by 10:');
  for v in query('1 to 5') do
    writeln('  ', v.toString, ' * 10 = ', v.toInt64 * 10);
  writeln;
  writeln('Print the numbers of 1 to 5 and multiply by 10 (in XPath):');
  for v in query('(1 to 5) ! x"  {.} * 10 = {. * 10}"') do
    writeln(v.toString);

  writeln;


  writeln('Get fpc''s website title(old interface):');
  writeln('  ', process('http://www.freepascal.org/', '//title').toString);
  writeln('Get fpc''s website title (new interface 1): ');
  writeln('  ', xqvalue('http://www.freepascal.org/').retrieve().map('//title').toString);
  writeln('Get fpc''s website title (new interface 2): ');
  writeln('  ', query('"http://www.freepascal.org/"').retrieve().map('//title').toString);


  writeln;


  writeln('Print the first 10 links on the fpc website: ');
  for v in process('http://www.freepascal.org', 'subsequence(//a,1,10)') do
    writeln('  ', v.toString + ' => ', v.toNode['href']);


  WriteLn();


  writeln('Use pattern matching to read a string:');
  writeln('  ', process('<hello>world</hello>', '<hello>{.}</hello>').toString);


  writeln();

  writeln('Use pattern matching to read several strings:');
  for v in process('<hello><a>s1</a><a>s2</a><a>s3</a><b>ignored</b><c>s4</c></hello>', '<hello><a>{.}</a>*<c>{.}</c>*</hello>') do
    writeln('  ', v.toString);


  WriteLn();

  writeln('Use pattern matching to read values into an object:');
  v := process('<html><a>V1</a><b>V2</b></html>', '<html>{obj:=object()}<a>{obj.x:=.}</a><b>{obj.y:=.}</b></html>').getProperty('obj');
  writeln('  a: ',v.getProperty('x').toString, '  b: ',v.getProperty('y').toString);

  writeln();

  writeln('Use pattern matching to read values in multiple objects:');
  for v in process('<html><X><a>V1a</a><b>V2</b></X><X><a>V3a</a><b>V4</b></X></html>', '<html><X>{obj:=object()}<a>{obj.x:=.}</a><b>{obj.y:=.}</b></X>*</html>').getProperty('obj') do
    writeln('  a: ',v.getProperty('x').toString, '  b: ',v.getProperty('y').toString);

  WriteLn();

  Writeln('Use CSS Selectors:');
  writeln('  ', process('<html><div class="foobar">something</div></html>', 'css(".foobar")').toString);

  WriteLn();

  WriteLn('Use XQuery to find all primes below 30:');
  for v in query(
    'xquery version "1.0";'                                +
    'declare function local:isprime($p){'                  +
    '  every $i in 2 to $p - 1 satisfies ($p mod $i != 0)' +
    '};'                                                   +
    'for $i in 2 to 30 where local:isprime($i) return $i') do
    writeln(v.toString);

  writeln;  writeln;  writeln;  writeln;  writeln;
end.


