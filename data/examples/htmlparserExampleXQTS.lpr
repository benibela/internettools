program htmlparserExampleXQTS;

{$mode objfpc}{$H+}

uses
//  bbheaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, extendedhtmlparser, simplehtmltreeparser, pseudoxpath, bbutils , sysutils, internetaccess
  ,{$ifdef win32}w32internetaccess{$else}synapseinternetaccess{$endif};
  { you can add units after this }

var lastGroup: string;
    buffer1, buffer2, buffer3: TStringList;

type TLogger = class

class procedure LOG_START; virtual; abstract;
class procedure LOG_GROUP_START(filen, title, desc: string); static; virtual; abstract;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string); virtual; abstract;
class procedure LOG_GROUP_END(correct, wrong, exceptions, skipped: integer); virtual; abstract;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; virtual; abstract;

end;
TLoggerClass = class of TLogger;

{ THTMLLogger }

THTMLLogger = class(TLogger)
class procedure LOG_START; override;
class procedure LOG_GROUP_START(filen, title, desc: string); static; override;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string); override;
class procedure LOG_GROUP_END(correct, wrong, exceptions, skipped: integer); override;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; override;
end;


{ TPlainLogger }

TPlainLogger = class(TLogger)
class procedure LOG_START; override;
class procedure LOG_GROUP_START(filen, title, desc: string); static; override;
class procedure LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string); override;
class procedure LOG_GROUP_END(correct, wrong, exceptions, skipped: integer); override;
class procedure LOG_END(correct, wrong, exceptions, skipped: integer); static; override;
end;

const CATALOG_TEMPLATE = '<test-group><GroupInfo>{gi:=.}</GroupInfo><test-case is-XPath2="true" >{(path:=@FilePath,desc:=description,queryname:=query/@name,' +
                         'outputfile:=output-file,error:=expected-error)}' +
                         '<input-file>{input:=.}</input-file>*{complete:="yes"}</test-case>*</test-group>';

type

{ twrapper }

 twrapper = class
  procedure eval(sender: TObject; const variable: string; var value: TPXPValue);
end;

{ twrapper }


procedure high(attributes: TStringList);
begin

end;



function mytostring(v: TPXPValue): string;
var
  i: Integer;
  seq: TPXPList;
begin
  if v is TPXPValueSequence then begin
    seq :=  v.toSequence;
    result := mytostring(seq[0]);
    for i:=1 to seq.count-1 do begin
      if seq[i] is TPXPValueNode then result += mytostring(seq[i])
      else result += ' '+mytostring(seq[i]);
    end;
    seq.freeNonRecursive;
  end else if (v is TPXPValueNode) and (TPXPValueNode(v).node <> nil) then begin
    result := v.toNode.outerXML();
  end else result := v.toString;
end;

var tree: TTreeParser;
    inputfiles, variables: TStringList;

{ TPlainLogger }

class procedure TPlainLogger.LOG_START;
begin

end;

class procedure TPlainLogger.LOG_GROUP_START(filen, title, desc: string);
begin
  lastGroup:= title + ' '+ desc;
  writeln('=====================================================');
  writeln('Testing: ', title, ' ', desc, ' ',filen);
  writeln('=====================================================');
end;

class procedure TPlainLogger.LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string);
begin
  case state of
  1: writeln('ERROR: got "', myoutput, '" expected "', output, '"');
  2: writeln('EXCEPTION: ', myoutput);
  end;
  writeln ('  In: ', queryfile, ': ',copy(desc,1,60), ' with ', inputfile);
end;

class procedure TPlainLogger.LOG_GROUP_END(correct, wrong, exceptions, skipped: integer);
begin
    writeln('Group: ', lastGroup, ' Correct: ', correct, ' Wrong:', wrong, ' Error: ', exceptions, ' / ', correct + wrong + exceptions, ' | Skipped: ', skipped);
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
  writeln('<html><head><title>XQTS Evaluation</title>');
  writeln('</head><body>');
  writeln('<h1>XQTS Evaluation</h1>');
  writeln('<h2>Overview</h2>');

  writeln('<table>');
  writeln('<tr><th>Name</th><th>Correct</th><th>Wrong</th><th>Error</th><th>Total</th><th>Skipped</th></tr>');
end;

class procedure THTMLLogger.LOG_GROUP_START(filen, title, desc: string);
begin
  lastGroup := title;
  buffer2.add('<h3><a name="'+title+'">'+title+'</a></h3>');
  buffer2.add(filen+': ' +desc+'<br><br>');


  buffer3.add('Failed tests:');
  buffer3.add('<table>');
  buffer3.add('<tr><th>Testname</th><th>Description</th><th>Got</th><th>Expected</th></tr>');
end;

class procedure THTMLLogger.LOG_RESULT(state: integer; desc, queryname, query, inputfile, queryfile, myoutput, output: string);
begin

    myoutput:=StringReplace(myoutput, '<', '&lt;', [rfReplaceAll]);
    myoutput:=StringReplace(myoutput, '>', '&gt;', [rfReplaceAll]);
    output:=StringReplace(output, '<', '&lt;', [rfReplaceAll]);
    output:=StringReplace(output, '>', '&gt;', [rfReplaceAll]);

    desc += '<br><a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/'+queryfile+'?rev=1.1;content-type=text%2Fplain">Query</a>, ';
    desc += '<a href="http://dev.w3.org/cvsweb/2006/xquery-test-suite/TestSuiteStagingArea/TestSources/'+inputfile+'.xml?rev=1.1;content-type=text%2Fplain">XML-Input</a>';


    case state of
    1: buffer3.add('<tr class="wrong"><td>'+queryname+'</td><td>'+desc+'</td><td>'+myoutput+'</td><td>'+output+'</td></tr>');
    2: buffer3.add('<tr class="error"><td>'+queryname+'</td><td>'+desc+'</td><td colspan=2> <b>Error</b>:'+myoutput+'</td></tr>');
    end;
end;

class procedure THTMLLogger.LOG_GROUP_END(correct, wrong, exceptions, skipped: integer);
var
  i: Integer;
begin
  buffer1.add('<tr><td><a href="#'+lastGroup+'">'+lastGroup+'</td><td>'+inttostr(correct)+'</td><td>'+inttostr(wrong)+'</td><td>'+inttostr(exceptions)+'</td><td>'+inttostr(correct+wrong+exceptions)+'</td><td>'+inttostr(skipped)+'</td></td>');
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

procedure twrapper.eval(sender: TObject; const variable: string; var value: TPXPValue);
var
  i: Integer;
begin
  i := variables.IndexOf(variable);
//  writeln(variable, ':', i);
  if i < 0 then exit;
  pxpvalueAssign(value, TTreeElement(variables.Objects[i]));
  //if variable = 'input-context' then value := pxpvalue(tree.getLastTree);
end;

var htp: THtmlTemplateParser;
    desc, queryname,  outputfile, error, path: string;
    i: Integer;
    query, output: String;
    skippedErrorsLocal, totalLocal, correctLocal, wrongLocal, exceptionLocal: Integer;
    pxp: TPseudoXPathParser;
    myoutput: string;
    wrap: twrapper;
    CAT: Integer;
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
    mylogger: TLoggerClass;
begin
  {$ifdef win32}defaultInternetAccessClass := TW32InternetAccess.create{$else}defaultInternetAccessClass:=TSynapseInternetAccess{$endif};

  if paramstr(1) = '--plain' then mylogger := TPlainLogger
  else mylogger := THTMLLogger;

  buffer1 := TStringList.Create;
  buffer2 := TStringList.Create;
  buffer3 := TStringList.Create;
  htp := THtmlTemplateParser.create;
  htp.parseTemplate(CATALOG_TEMPLATE);
  pxp := TPseudoXPathParser.create;
  pxp.OnEvaluateVariable:=@wrap.eval;
  pxp.ImplicitTimezone:=-5 / HoursPerDay;
  pxp.setDefaultCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint');
  tree := TTreeParser.Create;
  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;
  variables:=TStringList.Create;
  variables.Sorted:=true;
  inputfiles:=TStringList.Create;

  try
  if paramstr(1) = '--simple' then begin
    writeln(stderr, 'Query: ', paramstr(2), LineEnding);
    if paramstr(3) = '--xml' then begin
      tree.parseTreeFromFile(paramstr(4));
      pxp.RootElement:=tree.getLastTree;
    end;
    writeln(mytostring(pxp.evaluate(paramstr(2),tree.getLastTree)));
    exit;
  end;

  mylogger.LOG_START();

  PXPGlobalTrimNodes:=false;
  for CAT:=1 to Paramcount() do begin;
    if mylogger <> TPlainLogger then writeln(stderr, 'Test ', CAT, ' / ', Paramcount, ': ',paramstr(cat));
    skippedErrorsLocal := 0; correctLocal := 0; wrongLocal := 0; totalLocal := 0; exceptionLocal:=0;
    if paramstr(CAT)[1] = '-' then continue;
    htp.parseHTMLFile(ParamStr(CAT));


    //writeln(htp.variableChangeLog.debugTextRepresentation);
    for i:=0 to htp.variableChangeLog.count-1 do begin
      if htp.variableChangeLog.getVariableName(i) = 'gi' then mylogger.LOG_GROUP_START(paramstr(CAT), htp.variableChangeLog.getVariableValueNode(i).findChild(tetOpen, 'title').deepNodeText(),htp.variableChangeLog.getVariableValueNode(i).findChild(tetOpen, 'description').deepNodeText())
      else if htp.variableChangeLog.getVariableName(i) = 'desc' then desc := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'queryname' then queryname := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'inputfile' then inputfile := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'outputfile' then outputfile := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'error' then error := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'path' then path := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'input' then begin
        node := htp.variableChangeLog.getVariableValueNode(i);
        inputfilevar := node.getAttribute('variable');
        inputfile := node.deepNodeText();
        if inputfile = 'id-idref-dtd' then inputfile:='id';
        if inputfiles.IndexOf(inputfile) < 0 then begin
          try
          fileOpenFailed:='';
          inputfiles.AddObject(inputfile, tree.parseTreeFromFile('TestSources/'+inputfile+'.xml'));
          currentTree:=TTreeElement(inputfiles.Objects[inputfiles.Count-1]);
          except on e: EFOpenError do
            fileOpenFailed := e.Message;
          end;
        end else currentTree := TTreeElement(inputfiles.Objects[inputfiles.IndexOf(inputfile)]);
        if fileOpenFailed = '' then begin
          if variables.IndexOf(inputfilevar) < 0 then variables.AddObject(inputfilevar, nil);
          variables.Objects[variables.IndexOf(inputfilevar)] :=inputfiles.Objects[inputfiles.IndexOf(inputfile)];
        end;
      end else if htp.variableChangeLog.getVariableName(i) = 'complete' then begin
        totalLocal += 1;
        if error <> '' then begin
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
            pxp.parse('('+query+')');
            myoutput := mytostring(pxp.evaluate())
          end else begin
            //query := StringReplace(query, '$'+inputfilevar, '.', [rfReplaceAll]);
            pxp.parse('('+query+')');
            pxp.RootElement:=currentTree;
            pxp.ParentElement:=currentTree;
            myoutput := mytostring(pxp.evaluate())
          end;
          if (myoutput = output) or (((myoutput = '0') or (myoutput = '-0')) and ((output = '0') or (output = '-0')))  then begin
            correctLocal += 1;
            //writeln('PASS: ', copy(desc,1,30),queryname,' : got '  , myoutput);
          end else begin
            wrongLocal+=1;
            mylogger.LOG_RESULT(1, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', myoutput, output);
          {  write(stderr, 'WRONG: ', copy(desc,1,60),' ',queryname,' : got '  , myoutput, ' <> expected ', output, ' ');
            writeln(stderr, '       ', arrayGet(strSplit(query, #13),-2) );
            writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;}
          end;
        except on e: sysutils.Exception do begin
          exceptionLocal+=1;
          mylogger.LOG_RESULT(2, desc, queryname, query, inputfile, 'Queries/XQuery/'+path+'/'+queryname+'.xq', e.message, output);
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
    mylogger.LOG_GROUP_END(correctLocal, wrongLocal, exceptionLocal, skippedErrorsLocal);
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

  buffer1.free;
  buffer2.free;
  buffer3.free;
  variables.Free;
  inputfiles.Free;
  tree.free;
  pxp.free;
  htp.free;
  end;
end.

