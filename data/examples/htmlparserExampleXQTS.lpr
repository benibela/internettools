program htmlparserExampleXQTS;

{$mode objfpc}{$H+}

uses
//  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, extendedhtmlparser, simplehtmltreeparser, xquery, bbutils , sysutils, internetaccess
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




function mytostring(v: TXQValue): string;
var
  i: Integer;
  seq: TXQVList;
  isnode: boolean;
  wasnode: boolean;
begin
  if v is TXQValueSequence then begin
    seq :=  v.toSequence;
    wasnode := seq[0] is TXQValueNode;
    result := mytostring(seq[0]);
    for i:=1 to seq.count-1 do begin
      isnode := seq[i] is TXQValueNode;
      if isnode or wasnode then result += mytostring(seq[i])
      else result += ' '+mytostring(seq[i]);
      wasnode := isnode;
    end;
    seq.freeNonRecursive;
  end else if (v is TXQValueNode) and (TXQValueNode(v).node <> nil) then begin
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
  1: writeln('ERROR: got "', myoutput, '" expected "', output, '"');
  2: writeln('EXCEPTION: ', myoutput);
  end;
  writeln ('  In: ', queryfile, ': ',copy(desc,1,60), ' with ', inputfile, ' time: ', timing * MSecsPerDay);
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
  writeln('<style>  table tr th {background-color: #EEEEFF}    table tr:hover {background-color: #EEEEFF}   </style>');
  writeln('</head><body>');
  writeln('<h1>XQTS Evaluation</h1>');
  writeln('<h2>Overview</h2>');

  writeln('<table>');
  writeln('<tr><th>Name</th><th>Correct</th><th>Wrong</th><th>Error</th><th>Total</th><th>Skipped</th></tr>');
end;

class procedure THTMLLogger.LOG_GROUP_START(filen, title, desc: string);
begin
  buffer2.add('<h3><a name="'+title+'">'+title+'</a></h3>');
  buffer2.add(filen+': ' +desc+'<br><br>');


  buffer3.add('Failed tests:');
  buffer3.add('<table>');
  buffer3.add('<tr><th>Testname</th><th>Description</th><th>Got</th><th>Expected</th></tr>');
end;

class procedure THTMLLogger.LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string;timing: TDateTime);
begin

    myoutput:=StringReplace(myoutput, '<', '&lt;', [rfReplaceAll]);
    myoutput:=StringReplace(myoutput, '>', '&gt;', [rfReplaceAll]);
    output:=StringReplace(output, '<', '&lt;', [rfReplaceAll]);
    output:=StringReplace(output, '>', '&gt;', [rfReplaceAll]);

    desc += '<br><a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/'+queryfile+'?rev=1.1;content-type=text%2Fplain">Query</a>, ';
    desc += '<a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/TestSources/'+inputfile+'.xml?rev=1.1;content-type=text%2Fplain">XML-Input</a>';


    case state of
    0: buffer3.add('<tr class="correct"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    1: buffer3.add('<tr class="wrong"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    2: buffer3.add('<tr class="error"><td>'+queryname+'</td><td>'+desc+'</td><td> <b>Error</b>:'+myoutput+'</td><td>'+output+'</td></tr>');
    end;
end;

class procedure THTMLLogger.LOG_GROUP_END(startlogged: boolean; correct, wrong, exceptions, skipped: integer);
var
  i: Integer;
  lastGroup: String;
begin
  lastGroup := lastgroupStart.findChild(tetOpen, 'title').deepNodeText();
  if startlogged then lastGroup := '<a href="#'+lastGroup+'">'+lastGroup+'</a>';


  buffer1.add('<tr><td>'+lastGroup+'</td><td>'+inttostr(correct)+'</td><td>'+inttostr(wrong)+'</td><td>'+inttostr(exceptions)+'</td><td>'+inttostr(correct+wrong+exceptions)+'</td><td>'+inttostr(skipped)+'</td></td>');
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

const CATALOG_TEMPLATE = '<test-group><GroupInfo>{gi:=.}</GroupInfo><test-case is-XPath2="true" >{('+
                         'test:=xs:object(), test.path:=@FilePath,test.desc:=description,test.queryname:=query/@name,' +
                         'test.outputfile:=output-file,test.outputcomparator:=output-file/@compare, test.error:=expected-error)}' +
                         '<input-file>{input:=.}</input-file>*<input-URI>{input:=.}</input-URI>*{test.complete:="yes"}</test-case>*</test-group>';

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
    mypxpoutput: TXQValue;
    extendedvars: TXQVariableChangeLog;
    j: Integer;
    varlog: TXQVariableChangeLog;
    outputcomparator: String;
begin
  {$ifdef win32}defaultInternetAccessClass := TW32InternetAccess.create{$else}defaultInternetAccessClass:=TSynapseInternetAccess{$endif};

  if (paramstr(1) = '--plain') or (paramstr(2) = '--plain') then mylogger := TPlainLogger
  else mylogger := THTMLLogger;

  logCorrect := (paramstr(1) = '--correct') or (paramstr(2) = '--correct');


  buffer1 := TStringList.Create;
  buffer2 := TStringList.Create;
  buffer3 := TStringList.Create;
  htp := THtmlTemplateParser.create;
  htp.parseTemplate(CATALOG_TEMPLATE);
  pxp := TXQueryEngine.create;
  pxp.ImplicitTimezone:=-5 / HoursPerDay;
  pxp.CurrentDateTime := dateTimeParse('2005-12-05T17:10:00.203-05:00', 'yyyy-mm-dd"T"hh:nn:ss.zzz');
  pxp.AllowVariableUseInStringLiterals := false;
  pxp.setDefaultCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint');
  tree := TTreeParser.Create;
  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;
  inputfiles:=TStringList.Create;
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


    //writeln(htp.variableChangeLog.debugTextRepresentation);
    varlog := htp.VariableChangeLogCondensed;
    for i:=0 to varlog.count-1 do begin
      if varlog.getVariableName(i) = 'gi' then begin begin
        groupStart := varlog.getVariableValueNode(i); lastGroupStart := groupStart; end;
        pxp.VariableChangelog.clear;
      end
      else if varlog.getVariableName(i) = 'input' then begin
        node := varlog.getVariableValueNode(i);
        inputfilevar := node.getAttribute('variable');
        inputfile := node.deepNodeText();
        if inputfile = 'id-idref-dtd' then inputfile:='id';
        if striEqual(node.getNodeName(), 'input-URI') then
          pxp.VariableChangelog.addVariable(inputfilevar,  GetCurrentDir + DirectorySeparator + 'TestSources/'+inputfile+'.xml')
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
            pxp.VariableChangelog.addVariable(inputfilevar, TTreeElement(inputfiles.Objects[inputfiles.IndexOf(inputfile)]));
        end;

      end else if varlog.getVariableName(i) = 'test' then begin
        desc := varlog.getVariableValueObject(i).getAsString('desc');
        queryname := varlog.getVariableValueObject(i).getAsString('queryname');
        outputfile := varlog.getVariableValueObject(i).getAsString('outputfile');
        outputcomparator := varlog.getVariableValueObject(i).getAsString('outputcomparator');
        error := varlog.getVariableValueObject(i).getAsString('error');
        path := varlog.getVariableValueObject(i).getAsString('path');


        totalLocal += 1;
        if (error <> '') or (striEqual(outputcomparator, 'Inspect')) then begin
          skippedErrorsLocal+=1;
          continue;
        end;
        query := strLoadFromFile('Queries/XQuery/'+path+'/'+queryname+'.xq');
        output := strDecodeHTMLEntities(strLoadFromFile('ExpectedTestResults/'+path+'/'+outputfile),eUTF8);
        try

          query := StringReplace(query, 'declare variable $'+inputfilevar+' external;', '', [rfReplaceAll]);

          query := StringReplace(query, '(: insert-start :)', '(:insert-start:)',  []);
          query := StringReplace(query, '(: insert-end :)', '(:insert-end:)',  []);
          if strContains(query, '(:insert-start:)') and strContains(query, '(:insert-end:)') then begin
            from := pos('(:insert-start:)', query);
            delete(query, from, pos('(:insert-end:)', query) + length('(:insert-end:)')- from);
          end;

          if fileOpenFailed <> '' then raise EFOpenError.Create(fileOpenFailed);

          if (inputfile = 'emptydoc') or (inputfile='') then begin
            pxp.RootElement:=nil;
            pxp.ParentElement:=nil;
            pxp.parseXPath2('('+query+')');
          end else begin
            //query := StringReplace(query, '$'+inputfilevar, '.', [rfReplaceAll]);
            pxp.parseXPath2('('+query+')');
            pxp.RootElement:=currentTree;
            pxp.ParentElement:=currentTree;
          end;
          timing := now;
          mypxpoutput := pxp.evaluate();
          timing := now - timing;
          myoutput := mytostring(mypxpoutput);
          if (myoutput = output) or (((myoutput = '0') or (myoutput = '-0')) and ((output = '0') or (output = '-0')))
             or (((myoutput = '-1.0E18') or (myoutput = '-1E18')) and ((output = '-1.0E18') or (output = '-1E18')))
             or (((myoutput = '1.0E18') or (myoutput = '1E18')) and ((output = '1.0E18') or (output = '1E18')))
             or ((striEqual('xml', outputcomparator) or striEqual('fragment', outputcomparator)) and (trim(myoutput) = trim(output)))
             then begin
            correctLocal += 1;
            if logCorrect then begin
              logGroupStart;
              mylogger.LOG_RESULT(0, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing);
            end;
            //writeln('PASS: ', copy(desc,1,30),queryname,' : got '  , myoutput);
          end else begin
            wrongLocal+=1;

            logGroupStart;

            mylogger.LOG_RESULT(1, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output, timing);
          {  write(stderr, 'WRONG: ', copy(desc,1,60),' ',queryname,' : got '  , myoutput, ' <> expected ', output, ' ');
            writeln(stderr, '       ', arrayGet(strSplit(query, #13),-2) );
            writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;}
          end;
        except on e: sysutils.Exception do begin
          logGroupStart;
          exceptionLocal+=1;
          mylogger.LOG_RESULT(2, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', e.message, output, timing);
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
  //writeln(stderr, 'Correct:', correctLocal, ' Wrong: ',wrongLocal, ' Skipped: ',skippedErrorsLocal,' Crashed: ', exceptionLocal,' / ', totalLocal);


  finally
  for i:= 0 to buffer1.Count-1 do writeln(buffer1[i]);
  for i:= 0 to buffer2.Count-1 do writeln(buffer2[i]);
  for i:= 0 to buffer3.Count-1 do writeln(buffer3[i]);
  extendedvars.free;
  buffer1.free;
  buffer2.free;
  buffer3.free;
  inputfiles.Free;
  tree.free;
  pxp.free;
  htp.free;
  end;
end.

