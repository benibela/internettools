program htmlparserExample;

{$mode objfpc}{$H+}

uses
  Classes,
  extendedhtmlparser, FileUtil,sysutils,
  rcmdline //<< if you don't have this command line parser unit, you can download it from www.benibela.de
  { you can add units after this };

var mycmdLine: TCommandLineReader;
    htmlparser:THtmlTemplateParser;
    files:TStringArray;
    i: Integer;
    j: Integer;

{$R *.res}

begin
  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';

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
    if (files[i]='') or ((length(files[i]) < 5) and (not FileExistsUTF8(files[i]))) then continue;

    if not mycmdLine.readFlag('no-header') then writeln('**** Parse file:'+files[i]+' ****');
    try
    htmlparser.parseHtmlFile(files[i]);
    except on e: EHTMLParseException do begin
      writeln(stderr, 'Parsing error:');
      writeln(stderr, e.Message);
      writeln(stderr, 'Partial matching:');
      writeln(stderr, htmlparser.debugMatchings(50));

      raise;
    end;
    end;

    if mycmdLine.readFlag('immediate-vars') then begin
      if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Current variable state: **');
      for j:=0 to htmlparser.variables.count-1 do writeln(htmlparser.variables.getVariableName(j) + '='+ htmlparser.variables.getVariableValueString(j));
    end;

    if mycmdLine.readFlag('immediate-vars-changelog') then begin
      if not mycmdLine.readFlag('no-header') then writeln('** Current variable changelog: **');
      for j:=0 to htmlparser.variableChangeLog.count-1 do writeln(htmlparser.variableChangeLog.getVariableName(j) + '='+htmlparser.variableChangeLog.getVariableValueString(j));
    end;
  end;
  if mycmdLine.readFlag('vars') and not mycmdLine.readFlag('immediate-vars') then begin
    if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Final variable state: **');
    for j:=0 to htmlparser.variables.count-1 do writeln(htmlparser.variables.getVariableName(j) + '='+htmlparser.variables.getVariableValueString(j));
  end;
  if mycmdLine.readFlag('vars-changelog') and not mycmdLine.readFlag('immediate-vars-changelog') then begin
    if not mycmdLine.readFlag('no-header') then writeln('** Final variable changelog: **' );
    for j:=0 to htmlparser.variableChangeLog.count-1 do writeln(htmlparser.variableChangeLog.getVariableName(j) + '='+htmlparser.variableChangeLog.getVariableValueString(j));
  end;
  htmlparser.free;
  mycmdLine.free;

end.

