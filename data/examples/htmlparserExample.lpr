program htmlparserExample;

{$mode objfpc}{$H+}

uses
  Classes,
  extendedhtmlparser,  pseudoxpath, FileUtil,sysutils, bbutils,
  rcmdline //<< if you don't have this command line parser unit, you can download it from www.benibela.de
  { you can add units after this };

var mycmdLine: TCommandLineReader;
    htmlparser:THtmlTemplateParser;
    files:TStringArray;
    i: Integer;
    temp: TStringArray;
    s: shortString;
    j: Integer;

{$R *.res}

procedure printVars(vars: TPXPVariableChangeLog);
var j:integer;
begin
  if mycmdline.readFlag('type-annotated') then begin
    for j:=0 to vars.count-1 do
      writeln(vars.getVariableName(j) + '='+ vars.getVariableValue(j).debugAsStringWithTypeAnnotation);
  end else begin
    for j:=0 to vars.count-1 do
      writeln(vars.getVariableName(j) + '='+ vars.getVariableValueString(j));
  end;
end;

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
  mycmdLine.declareFlag('immediate-vars-changelog-condensed','List the variable changelog after every file, with object property changes reduced to the final object state');
  mycmdLine.declareFlag('vars','List the variable state after all files');
  mycmdLine.declareFlag('vars-changelog','List the variable changelog after all files');
  mycmdLine.declareFlag('vars-changelog-condensed','List the variable changelog after every file, with object property changes reduced to the final object state');
  mycmdLine.declareFlag('type-annotated','Prints all variable values with type annotations (i.e. string: abc, instead of abc)');
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
      temp := strSplit(htmlparser.debugMatchings(50), #13); //print line by line, or the output "disappears"
      for j := 0 to high(temp) do  writeln(stderr, temp[j]);

      raise;
    end;
    end;


    if mycmdLine.readFlag('immediate-vars') then begin
      if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Current variable state: **');
      printVars(htmlparser.variables);
    end;

    if mycmdLine.readFlag('immediate-vars-changelog') then begin
      if not mycmdLine.readFlag('no-header') then writeln('** Current variable changelog: **');
      printVars(htmlparser.variableChangeLog);
    end;

    if mycmdLine.readFlag('immediate-vars-changelog-condensed') then begin
      if not mycmdLine.readFlag('no-header') then writeln('** Current condensed variable changelog: **');
      printVars(htmlparser.VariableChangeLogCondensed);
    end;
  end;
  if mycmdLine.readFlag('vars') and not mycmdLine.readFlag('immediate-vars') then begin
    if not mycmdLine.readFlag('no-header') then writeln(stderr,'** Final variable state: **');
    printVars(htmlparser.variables);
  end;
  if mycmdLine.readFlag('vars-changelog') and not mycmdLine.readFlag('immediate-vars-changelog') then begin
    if not mycmdLine.readFlag('no-header') then writeln('** Final variable changelog: **' );
    printVars(htmlparser.variableChangeLog );
  end;
  if mycmdLine.readFlag('vars-changelog') and not mycmdLine.readFlag('immediate-vars-changelog-condensed') then begin
    if not mycmdLine.readFlag('no-header') then writeln('** Final condensed variable changelog: **' );
    printVars(htmlparser.VariableChangeLogCondensed );
  end;
  htmlparser.free;
  mycmdLine.free;

end.

