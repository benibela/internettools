unit simpleinternet_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commontestutils;

procedure unitTests();

implementation

uses simpleinternet, bbutils;

procedure unitTests();
var
  filename: String;
begin
  //test input guessing
  test(process('<xml>Hallo Welt</xml>', '/xml/text()'), 'Hallo Welt');
  test(process('<html>Hallo Welt</html>', '/html/body/text()'), 'Hallo Welt');
  test(process('{"foo": 123}', '($json).foo'), '123');
  test(process('[1,2,3]', '$json()'), '1 2 3');

  filename := 'internettoolstest.12312124lasdalkvsd 90wsu2q4124124.html';
  strSaveToFileUTF8(fileName, '<a>test</a>');
  test(process(filename, '/html/body/a'), 'test');
  test(process(filename, '<a>{.}</a>'), 'test');

  DeleteFile(filename);


  //test query kind guessing
  test(process('', '"&amp;"'), '&amp;');
  test(process('', 'xquery version "1.0"; "&amp;"'), '&');
  test(process('<html>1<a id="foobar">2</a>3</html>', '#foobar'), '2');
  test(process('<html>1<div class="a">div</div> <div class="b">b</div> <div class="a">xyz</div></html>', '.a'), 'div xyz');

  writeln('simpleinternet tested');
end;

end.

