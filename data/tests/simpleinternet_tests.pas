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
  test(process('<xml>Hallo Welt</xml>', '/xml/text()'), 'Hallo Welt');
  test(process('<html>Hallo Welt</html>', '/html/body/text()'), 'Hallo Welt');
  test(process('{"foo": 123}', '($json).foo'), '123');
  test(process('[1,2,3]', '$json()'), '1 2 3');

  filename := 'internettoolstest.12312124lasdalkvsd 90wsu2q4124124.html';
  strSaveToFileUTF8(fileName, '<a>test</a>');
  test(process(filename, '/html/body/a'), 'test');
  test(process(filename, '<a>{.}</a>'), 'test');

  DeleteFile(filename);

  writeln('simpleinternet tested');
end;

end.

