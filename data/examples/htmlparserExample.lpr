program htmlparserExample;

{$mode objfpc}{$H+}

uses
  Classes,
  extendedhtmlparser,
  rcmdline //<< if you don't have this command line parser unit, you can download it from www.benibela.de
  { you can add units after this };

var mycmdLine: TCommandLineReader;
    htmlparser:THtmlTemplateParser;
    files:TStringArray;
    i: Integer;
    j: Integer;
begin
  mycmdLine:=TCommandLineReader.create;
  mycmdLine.declareFlag('list-var','Should it list all variables');
  mycmdLine.declareString('template','Template file');

  mycmdLine.parse();

  files:=mycmdLine.readNamelessFiles();

  htmlparser:=THtmlTemplateParser.create;

  htmlparser.parseTemplateFile(mycmdLine.readString('template'));

  for i:=0 to high(files) do begin
    if files[i]='' then continue;
    writeln('Parse file:'+files[i]);
    htmlparser.parseHtmlFile(files[i]);

    if mycmdLine.readFlag('list-var') then begin
      writeln(#9'Current variable state:');
      for j:=0 to htmlparser.variables.count-1 do
        writeln(#9#9,htmlparser.variables[j]);
    end;
  end;
  htmlparser.free;
  mycmdLine.free;

end.

