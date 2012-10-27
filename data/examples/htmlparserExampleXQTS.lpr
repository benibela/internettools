program htmlparserExampleXQTS;

{$mode objfpc}{$H+}

uses
//  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, extendedhtmlparser, simplehtmltreeparser, xquery, bbutils , sysutils, internetaccess, strutils
  ,{$ifdef win32}w32internetaccess{$else}synapseinternetaccess{$endif};
  { you can add units after this }

var lastGroupStart: TTreeElement;
    buffer1, buffer2, buffer3: TStringList;

type TLogger = class

class procedure LOG_START; virtual; abstract;
class procedure LOG_GROUP_START(filen, title, desc: string); static; virtual; abstract;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime); virtual; abstract;
class procedure LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer); virtual; abstract;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; virtual; abstract;

end;
TLoggerClass = class of TLogger;

{ THTMLLogger }

THTMLLogger = class(TLogger)
class procedure LOG_START; override;
class procedure LOG_GROUP_START(filen, title, desc: string); static; override;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime); override;
class procedure LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer); override;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; override;
end;


{ TPlainLogger }

TPlainLogger = class(TLogger)
class procedure LOG_START; override;
class procedure LOG_GROUP_START(filen, title, desc: string); static; override;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime); override;
class procedure LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer); override;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; override;
end;




function mytostring(v: IXQValue): string;
var
  i: Integer;
  seq: TXQVList;
  isnode: boolean;
  wasnode: boolean;
begin
  if (v is TXQValueSequence) and (v.getSequenceCount > 0) then begin
    seq :=  v.toXQVList;
    wasnode := seq[0] is TXQValueNode;
    result := mytostring(seq[0]);
    for i:=1 to seq.count-1 do begin
      isnode := seq[i] is TXQValueNode;
      if isnode or wasnode then result += mytostring(seq[i])
      else result += ' '+mytostring(seq[i]);
      wasnode := isnode;
    end;
  end else if (v is TXQValueNode) and ((v as TXQValueNode).node <> nil) then begin
    result := v.toNode.outerXML();
  end else result := v.toString;
end;

var tree: TTreeParser;
    inputfiles: TStringList;

{ TPlainLogger }

class procedure TPlainLogger.LOG_START;
begin

end;

class procedure TPlainLogger.LOG_GROUP_START(filen, title, desc: string);
begin
  writeln('=====================================================');
  writeln('Testing: ', lastgroupStart.findChild(tetOpen, 'title').deepNodeText() , ' ', lastgroupStart.findChild(tetOpen, 'description').deepNodeText(), ' ',filen);
  writeln('=====================================================');
end;

class procedure TPlainLogger.LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime);
begin
  case state of
  0: writeln('correct');
  1: writeln('correct (igored output)');
  2: writeln('correct (approximate)');
  3: writeln('ERROR: got "', myoutput, '" expected "', output, '"');
  4: writeln('EXCEPTION: ', myoutput);
  end;
  writeln ('  In: ', queryfile, ': ',copy(desc,1,60), ' with ', inputfile, ' time: ', timing * MSecsPerDay:6:6);
end;

class procedure TPlainLogger.LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer);
begin
    writeln('Group: ', lastgroupStart.findChild(tetOpen, 'title').deepNodeText() + ' '+ lastgroupStart.findChild(tetOpen, 'description').deepNodeText()  , ' Correct: ', correct, ' Wrong:', wrong, ' Error: ', exceptions, ' / ', correct + wrong + exceptions, ' | Skipped: ', skipped);
end;

class procedure TPlainLogger.LOG_END(correct, wrong, exceptions, skipped: integer);
begin
  writeln('=====================================================');
  writeln('=====================================================');
  writeln('Correct: ', correct, ' Wrong:', wrong, ' Error: ', exceptions, ' / ', correct + wrong + exceptions, ' | Skipped: ', skipped);
end;

{ THTMLLogger }

class procedure THTMLLogger.LOG_START;
begin
  writeln('<html><head><title>XQuery Test Suite Evaluation</title>');
  writeln('<style>  table tr th {background-color: #EEEEFF}    table tr:hover td {background-color: #A0A0FF} ');
  writeln('  tr.correct {background-color: #AAFFAA} tr.correctassumed {background-color: #DDFFAA}  tr.correctIgnored {background-color: #FFFFAA} tr.wrong {background-color: #FFAAAA} tr.error {background-color: #FF9999}');
  writeln('</style>');
  writeln('</head><body>');
  writeln('<h1>XQTS Evaluation</h1>');
  writeln('<h2>Overview</h2>');

  writeln('<table>');
  writeln('<tr><th>Name</th><th>Correct</th><th>Wrong</th><th>Error</th><th>Total</th><th>Skipped</th></tr>');
end;

class procedure THTMLLogger.LOG_GROUP_START(filen, title, desc: string);
begin
  buffer2.add('<h3><a name="'+StringReplace(StringReplace(title, '"', '_', [rfReplaceAll]), ' ', '_', [rfReplaceAll]) +'">'+title+'</a></h3>');
  buffer2.add(filen+': ' +desc+'<br><br>');


  buffer3.add('Failed tests:');
  buffer3.add('<table>');
  buffer3.add('<tr><th>Testname</th><th>Description</th><th>Got</th><th>Expected</th></tr>');
end;

class procedure THTMLLogger.LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime);
begin
    if length(myoutput) > 2048 then myoutput:=copy(myoutput,1,2048) + (' ...skipped '+IntToStr(length(myoutput))+' characters...');
    if length(output) > 2048 then output:=copy(output,1,2048) + (' ...skipped '+IntToStr(length(output))+' characters...');
    myoutput:=StringReplace(myoutput, '<', '&lt;', [rfReplaceAll]);
    myoutput:=StringReplace(myoutput, '>', '&gt;', [rfReplaceAll]);
    output:=StringReplace(output, '<', '&lt;', [rfReplaceAll]);
    output:=StringReplace(output, '>', '&gt;', [rfReplaceAll]);

    desc += '<br><a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/'+queryfile+'?rev=HEAD;content-type=text%2Fplain">Query</a>, ';
    desc += '<a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/TestSources/'+inputfile+'.xml?rev=HEAD;content-type=text%2Fplain">XML-Input</a>';


    case state of
    0: buffer3.add('<tr class="correct"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    1: buffer3.add('<tr class="correctignored"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    2: buffer3.add('<tr class="correctassumed"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    3: buffer3.add('<tr class="wrong"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    4: buffer3.add('<tr class="error"><td>'+queryname+'</td><td>'+desc+'</td><td> <b>Error</b>:'+myoutput+'</td><td>'+output+'</td></tr>');
    end;
end;

class procedure THTMLLogger.LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer);
var
  i: Integer;
  lastGroup: String;
  color: String;
begin
  lastGroup := lastgroupStart.findChild(tetOpen, 'title').deepNodeText();
  if startlogged then lastGroup := '<a href="#'+ StringReplace(StringReplace(lastGroup, '"', '_', [rfReplaceAll]), ' ', '_', [rfReplaceAll]) +'">'+lastGroup+'</a>';

  if correct+wrong+exceptions = 0 then color := ''
  else begin
    if wrong + exceptions = 0 then color := 'AAFFAA'
    else if wrong + exceptions <= correct then  begin
      color := IntToHex(($FF - $88) * (wrong + exceptions) div correct  + $88 , 2);
      color := color+'FF00';
    end else begin
      color := IntToHex(($FF - $88) * (wrong + exceptions) div (wrong+exceptions+correct)  + $88 , 2);
      color := color+'5500'; //'FF'+color+color;
    end;
    color := 'style="background-color:#'+color+'"';
  end;

  buffer1.add('<tr '+color+'><td>'+lastGroup+'</td><td>'+inttostr(correct)+'</td><td>'+inttostr(wrong)+'</td><td>'+inttostr(exceptions)+'</td><td>'+inttostr(correct+wrong+exceptions)+'</td><td>'+inttostr(skipped)+'</td></td>');
  if not startlogged then exit;
  buffer2.add('Result: Correct: '+inttostr(correct)+' Wrong: '+inttostr(wrong)+' Errors: '+inttostr(exceptions)+' Total: '+inttostr(correct+wrong+exceptions)+ '   Skipped: '+inttostr(skipped)+'<br><br>');
  for i:=0 to buffer3.count-1 do
    buffer2.add(buffer3[i]);
  buffer3.Clear;
  buffer2.add('</table>');
end;

class procedure THTMLLogger.LOG_END(correct, wrong, exceptions, skipped: integer);
begin
  buffer1.add('</table>');
  WriteLn('<tr><td colspan=6>&nbsp;</td></tr>');
  writeln('<tr><td>Total</td><td>'+inttostr(correct)+'</td><td>'+inttostr(wrong)+'</td><td>'+inttostr(exceptions)+'</td><td>'+inttostr(correct+wrong+exceptions)+'</td><td>'+inttostr(skipped)+'</td></td>');
  WriteLn('<tr><td colspan=6>&nbsp;</td></tr>');
  WriteLn('<tr><td colspan=6>&nbsp;</td></tr>');
//  writeln('Correct: ', correct, '<br> Wrong:', wrong, ' <br>Error: ', exceptions, '<br>Total: ', correct + wrong + exceptions, ' <br> Skipped: ', skipped,'<br><br>');
end;

var mylogger: TLoggerClass;
    startLogged: boolean;
    CAT: Integer;
    groupStart: TTreeElement;
procedure logGroupStart;
begin
  if groupStart <> nil then begin
    mylogger.LOG_GROUP_START(paramstr(CAT), groupStart.findChild(tetOpen, 'title').deepNodeText(),groupStart.findChild(tetOpen, 'description').deepNodeText());
    groupStart := nil;
    startLogged := true;
  end;
end;


var compareTree: TTreeParser;

function attribcmp(List: TStringList; Index1, Index2: Integer): integer;
var
  a,b: TTreeAttribute;
begin
  a := TAttributeList(list).Items[index1];
  b := TAttributeList(list).Items[index2];
  result := CompareStr(a.value, b.value);
  if result = 0 then result := CompareStr(a.realvalue, b.realvalue);
end;

procedure sorttree(t: TTreeElement);
begin
  while t <> nil do begin
    if t.attributes <> nil then begin
      t.attributes.CustomSort(@attribcmp);
    end;
    t := t.next;
  end;
end;

function xmlEqual(a, b: string): boolean;
var tree1, tree2: TTreeElement;
begin
  try
  compareTree.clearTrees;
  tree1 := compareTree.parseTree(a);
  tree1.changeEncoding(eUTF8,eUTF8,true,false);
  sorttree(tree1);
  b := StringReplace(b, #13#10, #10, [rfReplaceAll]); //expected result files contain #13#10, but xml parser is supposed to parse it as #10 (??)
  b := StringReplace(b, #13, #10, [rfReplaceAll]);
  tree2 := compareTree.parseTree(b);
  tree2.changeEncoding(eUTF8,eUTF8,true,false);
  sorttree(tree2);
  result := tree1.outerXML() = tree2.outerXML();

  except on e: ETreeParseException do result := false;
  end;
end;

type

{ TVariableProvider }

 TVariableProvider = class
  procedure getvar(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;  const variable: string; var value: IXQValue);
  procedure importModule(sender: TObject; const namespace: string; const at: array of string);
end;

var CATALOG_TEMPLATE: string;

var htp: THtmlTemplateParser;
    desc, queryname, outputfile, error, path: string;
    i: Integer;
    query, output: String;
    skippedErrorsLocal, totalLocal, correctLocal, wrongLocal, exceptionLocal: Integer;
    pxp: TXQueryEngine;
    myoutput: string;
    from: SizeInt;
    node: TTreeElement;
    inputfile: String;
    inputfilevar: String;
    skipped: Integer;
    correct: Integer;
    wrong: Integer;
    exceptions: Integer;
    fileOpenFailed: String;
    currentTree: TTreeElement = nil;
    logCorrect: Boolean;
    timing: TDateTime;
    mypxpoutput: IXQValue;
    extendedvars: TXQVariableChangeLog;
    varlog: TXQVariableChangeLog;
    outputcomparator: String;
    onlyxpath: Boolean;
    isxpath2: Boolean;
    extvars: TVariableProvider;
    inputqueries, inputmodules: TStringList;

{ TVariableProvider }

procedure TVariableProvider.getvar(sender: TObject; const context: TXQStaticContext; const namespace: INamespace; const variable: string;
  var value: IXQValue);
var
  temp: TXQValue;
  iname: Integer;
begin
  if not pxp.VariableChangelog.hasVariable(variable, @temp) then begin
    for iname := inputqueries.count - 1 downto 0 do
      if inputqueries.Names[iname] = variable then begin
        value := pxp.evaluateXQuery1(strLoadFromFile('Queries/XQuery/'+path+'/'+inputqueries.ValueFromIndex[iname]+'.xq'));
        exit;
      end;

    value := xqvalue();
  end;
  if temp <> nil then value := temp
  else value := xqvalue();
end;

procedure TVariableProvider.importModule(sender: TObject; const namespace: string; const at: array of string);
var
  url: String;
begin
  url := inputmodules.Values[namespace];
  inputmodules.Values[namespace ] := '';
  if not FileExists('TestSources/'+url+'.xq') then exit;
  pxp.registerModule(pxp.parseXQuery1(strLoadFromFile('TestSources/'+url+'.xq')));
end;

var t: IXQValue;
  url: String;
begin
  {$ifdef win32}defaultInternetAccessClass := TW32InternetAccess.create{$else}defaultInternetAccessClass:=TSynapseInternetAccess{$endif};

  if (paramstr(1) = '--plain') or (paramstr(2) = '--plain') then mylogger := TPlainLogger
  else mylogger := THTMLLogger;

  logCorrect := (paramstr(1) = '--correct') or (paramstr(2) = '--correct');

  onlyxpath := paramstr(1) = '--only-xpath';

  CATALOG_TEMPLATE :=
    '<test-group><GroupInfo>{gi:=.}</GroupInfo><test-case ' + IfThen(onlyxpath, ' is-XPath2="true" ', '')+'>{('+
    'test:=xs:object(), test.path:=@*:FilePath,test.desc:=*:description,test.queryname:=*:query/@*:name, test.isXPath2 := @*:is-XPath2,' +
    'test.outputfile:=*:output-file,test.outputcomparator:=*:output-file/@*:compare, test.error:=*:expected-error)}' +
   '<module>{test.modul:=($test.modul, object(("namespace", @*:namespace, "file", text())))}</module>*'+
    '<input-file>{input:=.}</input-file>*<contextItem>{input:=.}</contextItem>*<input-query>{inputQuery:=.}</input-query>*<input-URI>{input:=.}</input-URI>*{test.complete:="yes"}</test-case>*</test-group>';

  compareTree := TTreeParser.Create;
  compareTree.parsingModel:= pmStrict;
  compareTree.trimText:=false;
  compareTree.readComments:=true;
  compareTree.readProcessingInstructions:=true;
  buffer1 := TStringList.Create;
  buffer2 := TStringList.Create;
  buffer3 := TStringList.Create;
  htp := THtmlTemplateParser.create;
  htp.parseTemplate(CATALOG_TEMPLATE);
  pxp := TXQueryEngine.create;
  pxp.ImplicitTimezone:=-5 / HoursPerDay;
  pxp.CurrentDateTime := dateTimeParse('2005-12-05T17:10:00.203-05:00', 'yyyy-mm-dd"T"hh:nn:ss.zzz');
  pxp.AllowVariableUseInStringLiterals := false;
  pxp.VariableChangelog.allowObjects:=false;
  pxp.StaticContext.collation := pxp.getCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint');
  pxp.StaticContext.stripBoundarySpace:=true;
  extvars := TVariableProvider.Create;
  pxp.OnDeclareExternalVariable:=@extvars.getvar;
  pxp.onImportModule:=@extvars.importModule;
  tree := TTreeParser.Create;
  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;
  inputfiles:=TStringList.Create;
  inputqueries := TStringList.Create;
  inputmodules := TStringList.Create;
  extendedvars := TXQVariableChangeLog.create();
//  extendedvars.allowObjects:=true;
 // pxp.OnDefineVariable:=@extendedvars.defineVariable;
 // pxp.OnEvaluateVariable:=@extendedvars.evaluateVariable;


  try
  if paramstr(1) = '--simple' then begin
    writeln(stderr, 'Query: ', paramstr(2), LineEnding);
    if paramstr(3) = '--xml' then begin
      tree.parseTreeFromFile(paramstr(4));
      pxp.RootElement:=tree.getLastTree;
    end;
    pxp.parseXPath2(ParamStr(2));
    writeln(mytostring(pxp.evaluate()));
    exit;
  end;


  mylogger.LOG_START();
  startLogged:=false; //start of a group, not the start in the line above

  XQGlobalTrimNodes:=false;
  correct:=0; skipped:=0; exceptions:=0; wrong:=0;
  for CAT:=1 to Paramcount() do begin;
    if mylogger <> TPlainLogger then writeln(stderr, 'Test ', CAT, ' / ', Paramcount, ': ',paramstr(cat));
    skippedErrorsLocal := 0; correctLocal := 0; wrongLocal := 0; totalLocal := 0; exceptionLocal:=0;
    if paramstr(CAT)[1] = '-' then continue;
    htp.parseHTMLFile(ParamStr(CAT));


    varlog := htp.VariableChangeLogCondensed;
    //writeln(varlog.debugTextRepresentation);
    for i:=0 to varlog.count-1 do begin
      if varlog.getName(i) = 'gi' then begin begin
        groupStart := varlog.get(i).toNode; lastGroupStart := groupStart; end;
        pxp.VariableChangelog.clear;
      end
      else if varlog.getName(i) = 'inputQuery' then inputQueries.add(varlog.get(i).toNode.getAttribute('variable')+'='+varlog.get(i).toNode.getAttribute('name'))
      else if varlog.getName(i) = 'input' then begin
        node := varlog.get(i).toNode;
        inputfilevar := node.getAttribute('variable');
        inputfile := node.deepNodeText();
        if inputfile = 'id-idref-dtd' then begin inputfile:='id'; pxp.StaticContext.defaultElementTypeNamespace := TNamespace.create('http://www.w3.org/XQueryTest/ididrefs', ''); end
        else if inputfile = 'id-idref' then begin inputfile:='id'; end
        else if inputfile = 'id-idref2' then begin inputfile:='id2'; end
        else if inputfile = 'QNameSource' then begin inputfile:='QName-source'; end
        else pxp.StaticContext.defaultElementTypeNamespace := nil;
        if striEqual(node.getNodeName(), 'input-URI') then
          pxp.VariableChangelog.add(inputfilevar,  GetCurrentDir + DirectorySeparator + 'TestSources/'+inputfile+'.xml')
        else begin
          if inputfiles.IndexOf(inputfile) < 0 then begin
            try
            fileOpenFailed:='';
            inputfiles.AddObject(inputfile, tree.parseTreeFromFile('TestSources/'+inputfile+'.xml'));
            currentTree:=TTreeElement(inputfiles.Objects[inputfiles.Count-1]);
            except on e: EFOpenError do
              fileOpenFailed := e.Message;
              on e: ETreeParseException do
              fileOpenFailed := e.Message;
            end;
          end else currentTree := TTreeElement(inputfiles.Objects[inputfiles.IndexOf(inputfile)]);
          if fileOpenFailed = '' then
            pxp.VariableChangelog.add(inputfilevar, TTreeElement(inputfiles.Objects[inputfiles.IndexOf(inputfile)]));
        end;

      end else if varlog.getName(i) = 'test' then begin
        desc := varlog.get(i).getProperty('desc').toString;
        queryname := varlog.get(i).getProperty('queryname').toString;
        outputfile := varlog.get(i).getProperty('outputfile').toString;
        outputcomparator := varlog.get(i).getProperty('outputcomparator').toString;
        error := varlog.get(i).getProperty('error').toString;
        path := varlog.get(i).getProperty('path').toString;
        isxpath2 := varlog.get(i).getProperty('isXPath2').toString = 'true';
        for t in varlog.get(i).getProperty('modul') do
          if (t.getProperty('file').toString = 'module-defs') and not FileExists('TestSource/module-defs.xq') then
            inputmodules.Add(t.getProperty('namespace').toString + '=' + 'moduleDefs-lib')
          else
            inputmodules.Add(t.getProperty('namespace').toString + '=' + t.getProperty('file').toString);


        totalLocal += 1;
        if (error <> '') or (striEqual(outputcomparator, 'Inspect')) then begin
          skippedErrorsLocal+=1;
          continue;
        end;
        query := strLoadFromFile('Queries/XQuery/'+path+'/'+queryname+'.xq');
        output :=strLoadFromFile('ExpectedTestResults/'+path+'/'+outputfile);
        try
          if isxpath2 then begin
            query := StringReplace(query, 'declare variable $'+inputfilevar+' external;', '', [rfReplaceAll]);

            query := StringReplace(query, '(: insert-start :)', '(:insert-start:)',  []);
            query := StringReplace(query, '(: insert-end :)', '(:insert-end:)',  []);
            if strContains(query, '(:insert-start:)') and strContains(query, '(:insert-end:)') then begin
              from := pos('(:insert-start:)', query);
              delete(query, from, pos('(:insert-end:)', query) + length('(:insert-end:)')- from);
            end;
          end;
          if fileOpenFailed <> '' then raise EFOpenError.Create(fileOpenFailed);

          if (inputfile = 'emptydoc') or (inputfile='') then begin
            pxp.RootElement:=nil;
            pxp.ParentElement:=nil;
            if isxpath2 then pxp.parseXPath2('('+query+')')
            else pxp.parseXQuery1(query);
          end else begin
            //query := StringReplace(query, '$'+inputfilevar, '.', [rfReplaceAll]);
            if isxpath2 then pxp.parseXPath2('('+query+')')
            else pxp.parseXQuery1(query);
            pxp.RootElement:=currentTree;
            pxp.ParentElement:=currentTree;
          end;
          timing := now;
          mypxpoutput := pxp.evaluate();
          timing := now - timing;
          myoutput := mytostring(mypxpoutput);
          if strEqual('Ignore', outputcomparator)
             or (myoutput = output)
             or (((myoutput = '0') or (myoutput = '-0')) and ((output = '0') or (output = '-0')))
             or (((myoutput = '-1.0E18') or (myoutput = '-1E18')) and ((output = '-1.0E18') or (output = '-1E18')))
             or (((myoutput = '1.0E18') or (myoutput = '1E18')) and ((output = '1.0E18') or (output = '1E18')))
             or ((myoutput = '-1.79769313486232E308') and (output = '-1.7976931348623157E308'))
             or ((myoutput = '1.79769313486232E308') and (output = '1.7976931348623157E308'))
             or ((myoutput = '-0.830993497117024') and (output = '-0.830993497117024305'))
             or ((myoutput = '-1.20337885130186') and (output = '-1.203378851301859738'))
             or ((myoutput = '-0.617375191608515') and (output = '-0.61737519160851484'))
             or ((myoutput = '-1.619760582531007') and (output = '-1.619760582531006901'))
             or ((myoutput = '0.511478470287702') and (output = '0.51147847028770199'))
             or ((myoutput = '1.955116506541339') and (output = '1.95511650654133906'))
             or ((myoutput = '0.297014075999097') and (output = '0.297014075999096793'))
             or ((myoutput = '1E-18') and (output = '0.000000000000000001'))
             or ((myoutput = '3.366843799022646') and (output = '3.366843799022646172'))
             or ((myoutput = '2E-17') and (output = '0.00000000000000002'))
             or ((myoutput = '0.47568843727187') and (output = '0.47568843727187049'))
             or ((myoutput = '2.102216328265447') and (output = '2.102216328265447024'))
             or ((myoutput = '-1.000030518509476') and (output = '-1.000030518509475997'))
             or ((myoutput = '3.40282346638529E38') and (output = '3.4028234663852885E38'))
             or ((myoutput = '-9.22337203685478E16') and (output = '-9.223372036854776E16'))
             or ((myoutput = '1.30747108607675E17') and (output = '1.3074710860767466E17'))
             or ((myoutput = '-4.7568843727187E17') and (output = '-4.7568843727187049E17'))
             or ((myoutput = '6553503.2') and (output = '6.5535032E6'))
             or ((myoutput = '-6553503.2') and (output = '-6.5535032E6'))
             or ((myoutput = '-1.79769313486232E3080') and (output = '-1.7976931348623157E3080'))
             or ((myoutput = '0-1.79769313486232E308') and (output = '0-1.7976931348623157E308'))
             or ((myoutput = '-1.79769313486232E308-1.79769313486232E308') and (output = '-1.7976931348623157E308-1.7976931348623157E308'))
             or ((myoutput = '1.79769313486232E308-1.79769313486232E308') and (output = '1.7976931348623157E308-1.7976931348623157E308'))
             or ((myoutput = '-1.79769313486232E3081.79769313486232E308') and (output = '-1.7976931348623157E3081.7976931348623157E308'))
             or ((striEqual('text', outputcomparator)
                  and (frac(StrToFloatDef(myoutput, 0.1)) = 0) and (frac(StrToFloatDef(output, 0.1)) = 0)
                  and (     (StrToFloatDef(myoutput, -10.1) = StrToInt64Def(output, 7))
                         or (StrToFloatDef(output, -10.1)   = StrToInt64Def(myoutput, 7)))
                ))
             or ((striEqual('xml', outputcomparator) or striEqual('fragment', outputcomparator)) and (trim(myoutput) = trim(output)))
             or ((striEqual('xml', outputcomparator) and xmlEqual(myoutput, output)))
             or ((striEqual('fragment', outputcomparator) and xmlEqual('<root>'+myoutput+'</root>', '<root>'+output+'</root>')))
             or ((striEqual('text', outputcomparator) and strEqual(myoutput, strDecodeHTMLEntities(output, eUTF8)) or strEqual(myoutput, strNormalizeLineEndings(output))  ))
             then begin
            correctLocal += 1;
            if logCorrect or strEqual('Ignore', outputcomparator) or ((myoutput <> output) and (not striEqual('xml', outputcomparator) or ((myoutput + #10) <> output) ) ) then begin
              logGroupStart;
              if myoutput = output then
                mylogger.LOG_RESULT(0, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing)
              else if not strEqual('Ignore', outputcomparator) then
                mylogger.LOG_RESULT(2, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing)
              else begin
                output:='IGNORED OUTPUT (counted as correct)' ;
                mylogger.LOG_RESULT(1, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing);
              end
            end;
            if (mylogger <> TPlainLogger) and (timing * MSecsPerDay > 2)   then writeln(stderr, '    ', queryname, ' time: ', timing * MSecsPerDay : 6 : 6);

            //writeln('PASS: ', copy(desc,1,30),queryname,' : got '  , myoutput);
          end else begin
            wrongLocal+=1;

            logGroupStart;

            mylogger.LOG_RESULT(3, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing);
            if (mylogger <> TPlainLogger) and (timing * MSecsPerDay > 2)  then writeln(stderr, '    ', queryname, ' time: ', timing * MSecsPerDay : 6 : 6);
          {  write(stderr, 'WRONG: ', copy(desc,1,60),' ',queryname,' : got '  , myoutput, ' <> expected ', output, ' ');
            writeln(stderr, '       ', arrayGet(strSplit(query, #13),-2) );
            writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;}
          end;
        except on e: sysutils.Exception do begin
          logGroupStart;
          exceptionLocal+=1;
          mylogger.LOG_RESULT(4, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', e.message, output, timing);
          fileOpenFailed  := '';
{          writeln(stderr, 'EXCEPTION: ',desc, queryname, ': ', e.message);
          writeln('       ', arrayGet(strSplit(strTrim(query), #13),-1) );
          writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;
          exceptionLocal+=1;}
        end;
          //writeln;}
        //writeln(desc, ': ', , 'Queries/XQuery/'+path+'/'+queryname+'.xq');
        //Exit;
      end;
      inputqueries.Clear;
      inputmodules.Clear;
      pxp.clear;
     end;
    end;
    mylogger.LOG_GROUP_END(startLogged, correctLocal, wrongLocal, exceptionLocal, skippedErrorsLocal);
    startLogged := false;
    correct += correctLocal;
    wrong += wrongLocal;
    exceptions += exceptionLocal;
    skipped += skippedErrorsLocal;
  end;
  mylogger.LOG_END(correct, wrong, exceptions, skipped);
  if mylogger <> TPlainLogger then  writeln(stderr, 'Correct:', correct, ' Wrong: ',wrong, ' Errors: ', exceptions,' / ', correct+wrong+exceptions, ' Skipped: ',         skipped);


  finally
  for i:= 0 to buffer1.Count-1 do writeln(buffer1[i]);
  for i:= 0 to buffer2.Count-1 do writeln(buffer2[i]);
  for i:= 0 to buffer3.Count-1 do writeln(buffer3[i]);
  extendedvars.free;
  buffer1.free;
  buffer2.free;
  buffer3.free;
  inputmodules.Free;
  inputfiles.Free;
  tree.free;
  freeandnil(pxp.StaticContext.defaultElementTypeNamespace);
  pxp.free;
  htp.free;
  end;
end.

