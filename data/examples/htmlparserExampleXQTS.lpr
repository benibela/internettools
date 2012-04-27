program htmlparserExampleXQTS;

{$mode objfpc}{$H+}

uses //bbheaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, extendedhtmlparser, simplehtmltreeparser, pseudoxpath, bbutils , sysutils
  { you can add units after this };

const CATALOG_TEMPLATE = '<test-group><test-case is-XPath2="true" >{(path:=@FilePath,desc:=description,queryname:=query/@name,inputfile:=input-file,inputfilevar:=input-file/@variable,outputfile:=output-file,error:=expected-error, complete:="yes")}</test-case>*</test-group>';

type

{ twrapper }

 twrapper = class
  procedure eval(sender: TObject; const variable: string; var value: TPXPValue);
end;

{ twrapper }


procedure high(attributes: TStringList);
begin

end;

function mytostring(n: TTreeElement): string;
var
  sub: TTreeElement;
  i: Integer;
begin
  case n.typ of
    tetText: result := n.value;
    tetClose: result := '</'+n.value+'>';
    tetComment: result := '<!--'+n.value+'-->';
    tetProcessingInstruction: begin
      result := '<?'+n.value;
      if n.attributes <> nil then begin
        for i:=0 to n.attributes.Count-1 do
          if n.attributes.ValueFromIndex[i] = '' then result += ' ' +n.attributes.Names[i]
          else result += ' ' +n.attributes[i];
        end;
      result += '?>';
    end;
    tetOpen: begin
      result := '<'+n.value;
      if n.attributes <> nil then begin
        for i:=0 to n.attributes.Count - 1 do begin
          result += ' ' + n.attributes.names[i]+'="'+n.attributes.ValueFromIndex[i]+'"';
        end;
      end;
      result+='>';
      sub := n.next;
      while sub <> n.reverse do begin
        result += mytostring(sub);
        if sub.typ <> tetOpen then sub:=sub.next
        else sub := sub.reverse.next;
      end;
      result+='</'+n.value+'>';
    end;
  end;
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
  end else if (v is TPXPValueNode) and (TPXPValueNode(v).node <> nil) then begin
    result := mytostring(TPXPValueNode(v).node);
  end else result := v.toString;
end;

var tree: TTreeParser;

procedure twrapper.eval(sender: TObject; const variable: string; var value: TPXPValue);
begin
  writeln(variable);
  if variable = 'input-context' then value := pxpvalue(tree.getTree);
end;

var htp: THtmlTemplateParser;
    desc, queryname, inputfile, outputfile, error, path: string;
    i: Integer;
    query, output: String;
    skippedErrors, total, correct, wrong, exception: Integer;
    pxp: TPseudoXPathParser;
    myoutput: string;
    wrap: twrapper;
    CAT: Integer;
    inputfilevar: String;
    from: SizeInt;

begin
  htp := THtmlTemplateParser.create;
  htp.parseTemplate(CATALOG_TEMPLATE);
  pxp := TPseudoXPathParser.create;
  pxp.OnEvaluateVariable:=@wrap.eval;
  pxp.ImplicitTimezone:=-5 / HoursPerDay;
  tree := TTreeParser.Create;
  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;

  if paramstr(1) = '--simple' then begin
    if paramstr(3) = '--xml' then begin
      tree.parseTreeFromFile(paramstr(4));
      pxp.RootElement:=tree.getTree;
    end;
    writeln(pxp.evaluate(paramstr(2),tree.getTree).toString);
    exit;
  end;

  skippedErrors := 0; correct := 0; wrong := 0; total := 0; exception:=0;
  PXPGlobalTrimNodes:=false;
  for CAT:=1 to Paramcount() do begin;
    htp.parseHTMLFile(ParamStr(CAT));


    //writeln(htp.variableChangeLog.debugTextRepresentation);
    for i:=0 to htp.variableChangeLog.count-1 do begin
      if htp.variableChangeLog.getVariableName(i) = 'desc' then desc := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'queryname' then queryname := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'inputfile' then inputfile := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'outputfile' then outputfile := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'error' then error := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'path' then path := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'inputfilevar' then inputfilevar := htp.variableChangeLog.getVariableValueString(i)
      else if htp.variableChangeLog.getVariableName(i) = 'complete' then begin
        total += 1;
        if error <> '' then begin
          skippedErrors+=1;
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
          if (inputfile = 'emptydoc') or (inputfile='') then begin
            pxp.RootElement:=nil;
            pxp.ParentElement:=nil;
            pxp.parse('('+query+')');
            myoutput := mytostring(pxp.evaluate())
          end else begin

            tree.parseTreeFromFile('TestSources/'+inputfile+'.xml');
            query := StringReplace(query, '$'+inputfilevar, '.', [rfReplaceAll]);
            pxp.parse('('+query+')');
            pxp.RootElement:=tree.getTree;
            pxp.ParentElement:=tree.getTree;
            myoutput := mytostring(pxp.evaluate())
          end;
          if (myoutput = output) or (((myoutput = '0') or (myoutput = '-0')) and ((output = '0') or (output = '-0')))  then begin
            correct += 1;
            //writeln('PASS: ', copy(desc,1,30),queryname,' : got '  , myoutput);
          end else begin
            wrong+=1;
            write(stderr, 'WRONG: ', copy(desc,1,60),' ',queryname,' : got '  , myoutput, ' <> expected ', output, ' ');
            writeln(stderr, '       ', arrayGet(strSplit(query, #13),-2) );
            writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;
          end;
        except on e: sysutils.Exception do begin
          writeln(stderr, 'EXCEPTION: ',desc, queryname, ': ', e.message);
          writeln('       ', arrayGet(strSplit(strTrim(query), #13),-1) );
          writeln('      TestSources/'+inputfile+'.xml', '  |  ','Queries/XQuery/'+path+'/'+queryname+'.xq','    |   ', 'ExpectedTestResults/'+path+'/'+outputfile); writeln;
          exception+=1;
        end;
          //writeln;}
        //writeln(desc, ': ', , 'Queries/XQuery/'+path+'/'+queryname+'.xq');
        //Exit;
      end;
     end;
    end;
  end;
  writeln(stderr, 'Correct:', correct, ' Wrong: ',wrong, ' Skipped: ',skippedErrors,' Crashed: ', exception,' / ', total);
  tree.free;
  pxp.free;
  htp.free;
end.

