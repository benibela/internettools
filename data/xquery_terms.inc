function convertElementTestToMatchingOptions(select: string): TXQPathMatchingKinds;
begin
  if select = 'node' then
    exit([qmText,qmComment,qmElement,qmProcessingInstruction,qmAttribute])
  else if select = 'text' then exit([qmText])
  else if select = 'comment' then exit([qmComment])
  else if select = 'element' then exit([qmElement,qmExcludeRoot])
  else if select = 'processing-instruction' then exit([qmProcessingInstruction])
  else raise Exception.Create('Unkown element test: '+select);
end;


{ TXQTermReadProperty }

constructor TXQTermReadObjectProperty.create(apropname: string);
begin
  propname := apropname;
end;

function TXQTermReadObjectProperty.evaluate(const context: TEvaluationContext): TXQValue;
var
  obj: TXQValue;
begin
  if length(children) = 0 then raiseEvaluationError('Found no term to read property from');
  if length(children) > 1 then raiseEvaluationError('Can only read property from one term');
  obj:=children[0].evaluate(context);
  if not (obj is TXQValueObject) then raiseEvaluationError('Expected object, got: '+obj.toString);
  result := TXQValueObject(obj).getClone(propname);
  obj.free;
end;

{ TXQTermIf }

function TXQTermIf.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if length(children)<>3 then raiseEvaluationError('if must have 3 arguments');
  if children[0].evaluate(context).toBoolean then result:=children[1].evaluate(context)
  else result:=children[2].evaluate(context);
end;

{ TXQTermFilterSequence }

constructor TXQTermFilterSequence.create(seq: TXQTerm; filter: TXQTerm);
begin
  push(seq);
  push(filter);
end;

function TXQTermFilterSequence.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if length(children) <> 2 then raiseEvaluationError('Invalid [] sequence filter');
  result := children[0].evaluate(context);
  context.sender.filterSequence(result, children[1], context);
  xqvalueSeqSqueeze(result);
end;

function TXQTermFilterSequence.toQueryCommand: TXQPathMatchingStep;
begin
  if length(children) <> 2 then raiseEvaluationError('invalid [] construct');
  result := children[0].toQueryCommand;
  setlength(result.filters, length(result.filters)+1);
  result.filters[high(result.filters)] := children[1];
end;

procedure TXQTermFilterSequence.addToQueryList(var path: TXQPathMatching);
begin
  if length(children) <> 2 then raiseEvaluationError('Invalid [] sequence filter');

  children[0].addToQueryList(path);
  setlength(path[high(path)].filters, length(path[high(path)].filters) + 1);
  path[high(path)].filters[high(path[high(path)].filters)] := children[1];
end;

{ TXQTermDefineVariable }

constructor TXQTermDefineVariable.create(varname: TXQTerm; value: TXQTerm);
begin
  if varname is TXQTermNodeMatcher then variablename := TXQTermNodeMatcher(varname).select
  else if varname is TXQTermVariable then variablename := TXQTermVariable(varname).value
  else raiseParsingError('Invalid term to assign a variable to: '+varname.debugTermToString);
  varname.Free;
  push(value);
end;

function TXQTermDefineVariable.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if not assigned(context.sender.OnDefineVariable) then raiseEvaluationError(':= can''t be used without a method assigned to the OnDefineVariable event');
  if length(children) = 0 then raiseEvaluationError('Value missing');
  if length(children) > 1 then raiseEvaluationError('Only one value can be assigned to a variable');
  result := children[0].evaluate(context);

  context.sender.OnDefineVariable(self, variablename, result.clone);
end;

{ TXQTermType }

constructor TXQTermType.create(const avalue: string);
begin
  value := avalue;
end;

function TXQTermType.evaluate(const context: TEvaluationContext): TXQValue;
begin
  result := xqvalue(value)
end;

{ TXQTermSequence }

function TXQTermSequence.evaluate(const context: TEvaluationContext): TXQValue;
var
  i: Integer;
begin
  if length(children) = 0 then exit(xqvalue);
  if length(children) = 1 then exit(children[0].evaluate(context));
  result := TXQValueSequence.create(length(children));
  for i := 0 to high(children) do
    TXQValueSequence(result).addChild(children[i].evaluate(context));
end;

{ TXQTermSomeEvery }

constructor TXQTermSomeEvery.create(every: boolean);
begin
  isEvery:=every;
end;

function TXQTermSomeEvery.evaluate(const context: TEvaluationContext): TXQValue;
var tempContext:TEvaluationContext;
  function evaluateSomeEvery(const pos:integer): boolean;
  var tempSeq: TXQValue;
      i:integer;
  begin
    if pos = high(children) then exit(children[pos].evaluate(tempContext).toBoolean);
    if not (children[pos] is TXQTermVariable) then raiseEvaluationError('Expected variable, but got '+children[pos].debugTermToString);

    tempSeq:=children[pos+1].evaluate(tempContext);
    case tempSeq.kind of
      pvkUndefined: begin
        result := isEvery;
        tempSeq.Free;
      end;
      pvkSequence: begin
        result:=isEvery;
        for i:=TXQValueSequence(tempSeq).seq.Count-1 downto 0 do begin
          tempContext.temporaryVariables.pushAll;
          tempContext.temporaryVariables.addVariable(TXQTermVariable(children[pos]).value, TXQValueSequence(tempSeq).seq[i]);
          TXQValueSequence(tempSeq).seq.Delete(i);
          if evaluateSomeEvery(pos+2) <> (isEvery) then begin
            tempContext.temporaryVariables.popAll;
            result := not isEvery;
            break;
          end;
          tempContext.temporaryVariables.popAll;
        end;
        tempSeq.Free;
      end;
      else begin
        tempContext.temporaryVariables.pushAll;
        tempContext.temporaryVariables.addVariable(TXQTermVariable(children[pos]).value, tempSeq);
        result := evaluateSomeEvery(pos+2);
        tempContext.temporaryVariables.popAll;
      end;
    end;
  end;

begin
  tempContext:=context;
  if context.temporaryVariables=nil then tempContext.temporaryVariables:=TXQVariableChangeLog.create();
  result:=xqvalue(evaluateSomeEvery(0));
  xqvalueSeqSqueeze(result);
  if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables) //iff context.temporaryVariables = nil, we created the change log here
end;

{ TXQTermFor }

function TXQTermFor.evaluate(const context: TEvaluationContext): TXQValue;
var
  tempContext: TEvaluationContext;
  function evaluateForLoop(const pos:integer): TXQValue;
   var tempSeq: TXQValue;
       i:integer;
   begin
     if pos = high(children) then exit(children[pos].evaluate(tempContext));
     if not (children[pos] is TXQTermVariable) then raiseEvaluationError('Expected variable, but got '+children[pos].debugTermToString);

     tempSeq:=children[pos+1].evaluate(tempContext);
     if tempSeq.wasUndefined then exit(xqvalue);
     if tempSeq is TXQValueSequence then begin
       result:=TXQValueSequence.create(TXQValueSequence(tempSeq).seq.Count);
       for i:=0 to TXQValueSequence(tempSeq).seq.Count-1 do begin
         tempContext.temporaryVariables.pushAll;
         tempContext.temporaryVariables.addVariable(TXQTermVariable(children[pos]).value, TXQValueSequence(tempSeq).seq[i]);
         TXQValueSequence(result).addChild(evaluateForLoop(pos+2));
         tempContext.temporaryVariables.popAll;
       end;
       TXQValueSequence(tempSeq).freeNonRecursive;
     end else begin
       tempContext.temporaryVariables.pushAll;
       tempContext.temporaryVariables.addVariable(TXQTermVariable(children[pos]).value, tempSeq);
       result := evaluateForLoop(pos+2);
       tempContext.temporaryVariables.popAll;
     end;
   end;
begin
  tempContext:=context;
  if context.temporaryVariables=nil then tempContext.temporaryVariables:=TXQVariableChangeLog.create();
  result:=evaluateForLoop(0);
  xqvalueSeqSqueeze(result);
  if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables) //iff context.temporaryVariables = nil, we created the change log here
end;

{ TXQTermNamedFunction }

constructor TXQTermNamedFunction.create(const akind: TXQTermNamedFunctionKind; const aindex: integer);
begin
  kind := akind;
  index := aindex;
end;

constructor TXQTermNamedFunction.create(const name: string);
begin
  if not findKindIndex(name, kind, index, true) then raiseParsingError('Unknown function: '+name);
end;

constructor TXQTermNamedFunction.create(const name: string; args: array of TXQTerm);
begin
  create(name);
  push(args);
end;

class function TXQTermNamedFunction.createIfExists(const name: string; checkForOperators: boolean): TXQTermNamedFunction;
var akind: TXQTermNamedFunctionKind;
    aindex: integer;
begin
  if not findKindIndex(name, akind, aindex, checkForOperators) then exit(nil);
  result := TXQTermNamedFunction.Create(akind, aindex);
end;

function TXQTermNamedFunction.evaluate(const context: TEvaluationContext): TXQValue;
var
  evilkids: TXQVArray;
begin
  evaluateChildren(context, evilkids);

  assert(index >= 0);

  case kind of
    xqfkBasic: begin
      result := xqvalue();
      PXQBasicFunctionInfo(basicFunctions.Objects[index])^.func(evilkids, result)
    end;
    xqfkComplex: begin
      result := xqvalue();
      PXQComplexFunctionInfo(complexFunctions.Objects[index])^.func(context, evilkids, result);
    end;
    xqfkWrappedOperator: begin
      if length(evilkids) <> 2 then raiseEvaluationError('Need two arguments for function: op:' + binaryOpFunctions[index]);
      result := xqvalue();
      TXQBinaryOp(binaryOpFunctions.Objects[index])(context, evilkids[0], evilkids[1], result);
    end;
    xqfkTypeConstructor:
      result := TXQValueClass(types.Objects[index]).createFromValue(evilkids);

  end;
end;

procedure xqFunctionGeneralConstructor(args: array of TXQValue; var result: TXQValue);forward;

class function TXQTermNamedFunction.findKindIndex(const name: string; out akind: TXQTermNamedFunctionKind; out aindex: integer; checkForOps: boolean): boolean;
var
  i: Integer;
begin
  aindex := basicFunctions.IndexOf(name);
  if aindex >= 0 then begin
    if PXQBasicFunctionInfo(basicFunctions.Objects[aindex])^.func = @xqfunctionGeneralConstructor then begin
      akind:=xqfkTypeConstructor;
      aindex:=types.indexof(name);;
      exit(true);
    end;
    akind:=xqfkBasic;
    exit(true);
  end;

  aindex := complexFunctions.IndexOf(name);
  if aindex >= 0 then begin
    akind:=xqfkComplex;
    exit(true);
  end;

  aindex := -1;
  for i:=0 to  binaryOpFunctions.Count-1 do
    if strContains(name, binaryOpFunctions[i]) then begin
      aindex := i;
      akind := xqfkWrappedOperator;
      exit(true);
    end;;

  exit(false);
end;



{ TXQTermBinaryOp }

constructor TXQTermBinaryOp.create(const op: string; arg1: TXQTerm; arg2: TXQTerm);
begin
  index := binaryOps.IndexOf(op);
  if arg1 <> nil then begin
    push(arg1);
    if arg2 <> nil then push(arg2);
  end;
end;

constructor TXQTermBinaryOp.create(arg1: TXQTerm; const op: string; arg2: TXQTerm);
begin
  create(op, arg1, arg2);
end;

function TXQTermBinaryOp.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if length(children)<2 then raiseEvaluationError('Not enough subterms');
  if length(children)>2 then raiseEvaluationError('Too many subterms');

  result := xqvalue();

  if (index = binaryOps.IndexOf('/')) or (index = binaryOps.IndexOf('//')) then begin
    context.sender.evaluateAccessList(self, context, result);
    exit;
  end;

  operatorInfo^.func(context, children[0].evaluate(context), children[1].evaluate(context),result);
end;

function TXQTermBinaryOp.operatorInfo: PXQOperatorInfo;
begin
  if (index < 0) or (index >= binaryOps.Count) then raiseEvaluationError('Invalid bin op index');
  result := PXQOperatorInfo(binaryOps.Objects[index])
end;

procedure TXQTermBinaryOp.addToQueryList(var path: TXQPathMatching);
var
  isDoubleSlash: Boolean;
  last: Integer;
begin
  isDoubleSlash := (index = binaryOps.IndexOf('//'));
  if  not (isDoubleSlash or (index = binaryOps.IndexOf('/'))) then begin
    inherited addToQueryList(path);
    exit;
  end;

  children[0].addToQueryList(path);
  if isDoubleSlash then begin
    setlength(path, length(path) + 1);
    path[high(path)].typ:=qcSameOrDescendant;
    path[high(path)].matching:=[qmElement, qmText, qmComment, qmProcessingInstruction];
  end;
  children[1].addToQueryList(path);

  //optimization
  //two descendants following each other can be condensed to a single descendant (but this changes the indices of the sequence, so it only work if there are no numeric filters)
  last := high(path);
  if (path[last - 1].typ = qcSameOrDescendant) and (path[last - 1].matching = [qmElement, qmText, qmComment, qmProcessingInstruction]) and (length(path[last - 1].filters) = 0)
      and (path[last].typ in [qcDirectChild, qcDescendant]) and (length(path[last].filters) = 0) then begin
    path[last - 1] := path[last];
    path[last - 1].typ := qcDescendant;
    SetLength(path, last);
  end;
end;

{ TXQTermNodeMatcher }

constructor TXQTermNodeMatcher.Create(const avalue: string; asfunction: boolean);
begin
  axis := '';
  select := avalue;
  if strContains(select,'::') then axis:=strSplitGet('::',select);
  func := asfunction;
end;

function TXQTermNodeMatcher.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if (select = '.') and (context.SeqValue <> nil) then result := context.SeqValue.clone
  else if func and (axis = '') and (select = 'text') and (context.sender.TextElement<>nil) then result := xqvalue(context.sender.TextElement)
  else begin
    result := context.sender.evaluateSingleStepQuery(toQueryCommand,context);
    xqvalueSeqSqueeze(result);
  end;
end;

function TXQTermNodeMatcher.toQueryCommand: TXQPathMatchingStep;
  function getAxis(const axis: string): TXQPathMatchingAxis;
  begin
     //forward
     if (axis ='') or (axis='child') then exit(qcDirectChild);
     if axis='descendant' then exit(qcDescendant);
     if axis='self' then exit(qcSameNode);
     if axis='parent' then exit(qcDirectParent);
     //if axis='attribute' then exit(qcDescendant);
     if axis='descendant-or-self' then exit(qcSameOrDescendant);
     if axis='following-sibling' then exit(qcFollowingSibling);
     if axis='following' then exit(qcFollowing);

     //reverse
     if axis='ancestor' then exit(qcAncestor);
     if axis='preceding-sibling' then exit(qcPrecedingSibling);
     if axis='preceding' then exit(qcPreceding);
     if axis='ancestor-or-self' then exit(qcSameOrAncestor);
     raise EXQEvaluationException.Create('Unknown axis: '+axis);
  end;

begin
  if func then begin
    result.typ:=getAxis(axis);
    result.matching:=convertElementTestToMatchingOptions(select);
    if (result.matching = [qmProcessingInstruction]) and (length(children) > 0) then begin
      if children[0] is TXQTermNodeMatcher then begin;
        if TXQTermNodeMatcher(children[0]).axis <> '' then raiseEvaluationError('axis within element test is not allowed');
        result.value := TXQTermNodeMatcher(children[0]).select;
      end else if children[0] is TXQTermString then
        result.value:=TXQTermString(children[0]).value
      else raiseEvaluationError('Invalid parameter for processing-instruction kind test: '+children[0].ToString);
      include(result.matching, qmValue) ;
    end;
  end else case select of
    '/': result.typ := qcDocumentRoot;
    '.': begin
      result.typ:=qcSameNode; //same node /./
      result.matching:=[qmElement, qmAttribute];
    end;
    '..': begin
      result.typ:=qcDirectParent; //parent /../
      result.matching:=[qmElement];
    end;
    '*': begin
      result.typ:=getAxis(axis); //any child /*/
      result.matching:=[qmElement,qmAttribute, qmExcludeRoot];
    end;
    else begin
      result.typ := getAxis(axis);
      result.matching := [qmValue,qmElement];
      result.value:=select;
    end;
  end;
end;

{ TXQTermUnaryOp }

constructor TXQTermUnaryOp.create(const op: string; arg: TXQTerm);
begin
  index := binaryOps.IndexOf(op);
  if arg <> nil then push(arg);
end;

function TXQTermUnaryOp.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if length(children)<1 then raiseEvaluationError( 'Not enough subterms')
  else if length(children)>1 then raiseEvaluationError('Too many subterms');
  if index < 0 then raiseEvaluationError('Unknown unary op');
  result := xqvalue();
  PXQOperatorInfo(binaryOps.Objects[index])^.func(context, xqvalue(0),children[0].evaluate(context),result);
end;

{ TXQTermNumber }

constructor TXQTermNumber.create(const avalue: string);
begin
  if strContains(avalue,'e') or strContains(avalue,'E') then value := TXQValue_double.create(StrToFloat(avalue,XQFormats))
  else if strContains(avalue,'.') then value := xqvalue(StrToFloat(avalue,XQFormats))
  else value := xqvalue(StrToInt65(avalue));
end;

function TXQTermNumber.evaluate(const context: TEvaluationContext): TXQValue;
begin
  result := value.clone;
end;

destructor TXQTermNumber.destroy;
begin
  value.free;
  inherited destroy;
end;

{ TXQTermReadAttribute }

constructor TXQTermReadAttribute.create(avalue: string);
begin
  value := avalue;
end;

function TXQTermReadAttribute.evaluate(const context: TEvaluationContext): TXQValue;
var
  tempEl: TTreeElement;
begin
  if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then
    tempEl := TXQValueNode(context.SeqValue).node
   else
    tempEl:=context.ParentElement;
  if tempEl.typ = tetProcessingInstruction then
    exit(xqvalue);
  if value = '*' then begin
    if tempEl.attributes = nil then result := xqvalue()
    else if tempEl.attributes.next = nil then begin
      assert(tempEl.attributes.reverse <> nil);
      result := xqvalue(tempEl.attributes.reverse)
    end else begin
      result := TXQValueSequence.create();
      tempEl := tempEl.attributes;
      while tempEl <> nil do begin
        assert(tempEl.reverse <> nil);
        TXQValueSequence(result).addChild(xqvalue(tempEl.reverse));
        tempEl := tempEl.next;
      end;
    end;
  end else if tempEl.getAttributeTry(value, tempEl) then  //TODO: multi attributes with the same name
    result:=xqvalue(tempEl)
  else
    result:=xqvalue();
end;

{ TXQTermVariable }

constructor TXQTermVariable.create(const avalue: string);
begin
  value := avalue;
  if value = '' then raiseEvaluationError('variable with empty name');
  if value[1] <> '$' then raiseEvaluationError('variable name does not start with $:  ' + value);
  delete(value,1,1);
  if value[length(value)] = ';' then delete(value, length(value), 1);
end;

function TXQTermVariable.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if context.temporaryVariables <> nil then begin
    result := nil;
    if context.temporaryVariables.hasVariableOrObject(value, @result) then begin
      if result <> nil then result := result.clone
      else result := xqvalue();
      exit();
    end;
  end;
  if value = '$' then result := xqvalue('$') //default $$; as $
  else if value = 'line-ending' then result := xqvalue(LineEnding) //default $line-ending; as #13#10
  else result := xqvalue();
  if assigned(context.sender.OnEvaluateVariable) then context.sender.OnEvaluateVariable(self, value, result );
end;
{ TXQTermString }

constructor TXQTermString.create(avalue: string);
begin
  value := avalue;
end;

function TXQTermString.evaluate(const context: TEvaluationContext): TXQValue;
begin
  Result := xqvalue(value);
end;


