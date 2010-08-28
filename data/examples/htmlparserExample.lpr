program htmlparserExample;

{$mode objfpc}{$H+}

uses
  Classes,
  extendedhtmlparser, FileUtil,
  rcmdline //<< if you don't have this command line parser unit, you can download it from www.benibela.de
  { you can add units after this };

var mycmdLine: TCommandLineReader;
    htmlparser:THtmlTemplateParser;
    files:TStringArray;
    i: Integer;
    j: Integer;

{$R *.res}

begin
  mycmdLine:=TCommandLineReader.create;
  mycmdLine.declareFlag('no-header','Just prints the variables, not the file name');
  mycmdLine.declareFlag('immediate-vars','List the variable state after every file');
  mycmdLine.declareFlag('immediate-vars-changelog','List the variable changelog after every file');
  mycmdLine.declareFlag('vars','List the variable changelog after all files');
  mycmdLine.declareFlag('vars-changelog','List the variable changelog after all files');
  mycmdLine.declareString('template','Template file');

  mycmdLine.parse();

  files:=mycmdLine.readNamelessFiles();

  htmlparser:=THtmlTemplateParser.create;

  htmlparser.parseTemplateFile(mycmdLine.readString('template'));

  for i:=0 to high(files) do begin
    if (files[i]='') or ((length(files[i]) < 2) and (not FileExistsUTF8(files[i]))) then continue;

    if not mycmdLine.readFlag('no-header') then writeln('**** Parse file:'+files[i]+' ****');
    htmlparser.parseHtmlFile(files[i]);

    if mycmdLine.readFlag('immediate-vars') then begin
      if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Current variable state: **');
      for j:=0 to htmlparser.variables.count-1 do writeln(htmlparser.variables[j]);
    end;

    if mycmdLine.readFlag('immediate-vars-changelog') then begin
      if not mycmdLine.readFlag('no-header') then writeln('** Current variable changelog: **');
      for j:=0 to htmlparser.variableChangeLog.count-1 do writeln(htmlparser.variableChangeLog[j]);
    end;
  end;
  if mycmdLine.readFlag('vars') and not mycmdLine.readFlag('immediate-vars') then begin
    if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Final variable state: **');
    for j:=0 to htmlparser.variables.count-1 do writeln(htmlparser.variables[j]);
  end;
  if mycmdLine.readFlag('vars-changelog') and not mycmdLine.readFlag('immediate-vars-changelog') then begin
    if not mycmdLine.readFlag('no-header') then writeln('** Final variable changelog: **' );
    for j:=0 to htmlparser.variableChangeLog.count-1 do writeln(htmlparser.variableChangeLog[j]);
  end;
  htmlparser.free;
  mycmdLine.free;

end.

