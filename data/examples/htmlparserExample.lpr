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
  mycmdLine.declareFlag('immediate-vars','List the variable state after every file');
  mycmdLine.declareFlag('immediate-var-changelog','List the variable changelog after every file');
  mycmdLine.declareFlag('var-changelog','List the variable changelog after all files');
  mycmdLine.declareFlag('vars-changelog','List the variable changelog after all files');
  mycmdLine.declareString('template','Template file');

  mycmdLine.parse();

  files:=mycmdLine.readNamelessFiles();

  htmlparser:=THtmlTemplateParser.create;

  htmlparser.parseTemplateFile(mycmdLine.readString('template'));

  for i:=0 to high(files) do begin
    if files[i]='' then continue;
    writeln('Parse file:'+files[i]);
    htmlparser.parseHtmlFile(files[i]);

    if mycmdLine.readFlag('immediate-vars') then begin
      writeln(stderr,#9'Current variable state:');
      for j:=0 to htmlparser.variables.count-1 do
        writeln(#9#9,htmlparser.variables[j]);
    end;

    if mycmdLine.readFlag('immediate-var-changelog') then begin
      writeln(#9'Current variable changelog:');
      for j:=0 to htmlparser.variableChangeLog.count-1 do
        writeln(#9#9,htmlparser.variableChangeLog[j]);
    end;
  end;
  if mycmdLine.readFlag('vars') and not mycmdLine.readFlag('immediate-vars') then begin
    writeln(stderr,#9'Final variable state:');
    for j:=0 to htmlparser.variables.count-1 do
      writeln(#9#9,htmlparser.variables[j]);
  end;
  if mycmdLine.readFlag('var-changelog') and not mycmdLine.readFlag('immediate-var-changelog') then begin
    writeln(#9'Final variable changelog:');
    for j:=0 to htmlparser.variableChangeLog.count-1 do
      writeln(#9#9,htmlparser.variableChangeLog[j]);
  end;
  htmlparser.free;
  mycmdLine.free;

end.

