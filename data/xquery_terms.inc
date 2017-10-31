{
Copyright (C) 2008 - 2015 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

procedure copyAnnotations(var target: TXQAnnotations; const source: TXQAnnotations);
var
  i: Integer;
  j: Integer;
begin
  target:= source;
  SetLength(target, length(target));
  for i := 0 to high(target) do
    with target[i] do begin
      name := name.clone;
      SetLength(params, length(params));
      for j := 0 to high(params) do
        params[j] := params[j].clone;
    end;
end;

procedure freeAnnotations(annotations: TXQAnnotations);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to high(annotations) do begin
    annotations[i].name.Free;
    for j := 0 to high(annotations[i].params) do
      annotations[i].params[j].free;
  end;

end;

function TXQTerm.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := nil;
  raise EXQEvaluationException.create('pxp:INTERNAL', debugTermToString + ' cannot be evaluated');
end;

function TXQTerm.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;

function TXQTerm.debugTermToString: string;
begin
  result := '(' + ClassName + ')';
end;

procedure TXQTerm.raiseParsingError(const errcode, s: string);
begin
  raise EXQParsingException.Create(errcode, s);
end;

procedure TXQTerm.raiseEvaluationError(const errcode, s: string);
begin
  raise EXQEvaluationException.Create(errcode, s, nil, nil, self);
end;

procedure TXQTerm.raiseTypeError0004(const s: string);
begin
  raiseEvaluationError('XPTY0004', s);
end;

procedure TXQTerm.raiseTypeError0004(const s: string; const got: IXQValue);
begin
  raise EXQEvaluationException.Create('XPTY0004', s + ' GOT: '+ got.toXQuery(), nil, got, self);
end;

function TXQTerm.visitchildren(intentionallyUnusedParameter: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := xqtvaContinue;
end;


function TXQTerm.clone: TXQTerm;
begin
  result := txqterm(newinstance);
end;

{ TXQTerm }

function TXQTermWithChildren.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES;
end;

function TXQTermWithChildren.debugTermToString: string;
var
  i: Integer;
begin
  result := '(' + ClassName;
  if length(children) >= 1 then result += ': '+children[0].debugTermToString();
  for i:= 1 to high(children) do result += ', '+children[1].debugTermToString();
  result += ')';
end;

destructor TXQTermWithChildren.destroy;
var
  i: Integer;
begin
  for i := 0 to high(children) do children[i].free;
  inherited destroy;
end;

procedure TXQTermWithChildren.push(t: TXQTerm);
begin
  SetLength(children, length(children) + 1);
  children[high(children)] := t;
end;

function TXQTermWithChildren.push(t: array of TXQTerm): TXQTerm;
var
  i: Integer;
begin
  for i := 0 to high(t) do push(t[i]);
  result := self;
end;

procedure TXQTermWithChildren.evaluateChildren(var context: TXQEvaluationContext; out results: TXQVArray);
var
  i: Integer;
begin
  setlength(results, length(children));
  for i:=0 to high(children) do
    results[i] := children[i].evaluate(context);
end;

function TXQTermWithChildren.getChildrenContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  for i := 0 to high(children) do
    result += children[i].getContextDependencies;
end;


function TXQTermWithChildren.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
begin
  result := xqtvaContinue;
  for i := 0 to high(children) do begin
    result := visitor.simpleTermVisit(@children[i], self);
    case result of xqtvaAbort: exit; end;
  end;
end;

function TXQTermWithChildren.clone: TXQTerm;
var
  i: Integer;
  tr: TXQTermWithChildren;
begin
  result := TXQTermWithChildren(newinstance);
  tr := TXQTermWithChildren(result);
  SetLength(tr.children, length(children));
  for i := 0 to high(children) do
    tr.children[i] := children[i].clone;
end;

var globalUnnamedVariable: TXQTermVariable;

function TXQTermWithChildrenOnStack.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  tempvar: TXQTermVariable;
  i: Integer;
begin
  result := xqtvaContinue;
  tempvar := globalUnnamedVariable;
  for i := 0 to high(children) do begin
    result := visitor.simpleTermVisit(@children[i], self);
    case result of xqtvaAbort: exit; end;
    visitor.declare(@tempvar);
  end;
  for i := high(children) downto 0 do
    visitor.undeclare(@tempvar);
end;

{ TXQTerm_Visitor }

procedure TXQTerm_Visitor.declare(intentionallyUnusedParameter: PXQTermVariable);
begin

end;

procedure TXQTerm_Visitor.undeclare(intentionallyUnusedParameter: PXQTermVariable);
begin

end;

function TXQTerm_Visitor.visit(intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction;
begin
  result := xqtvaContinue;
end;

function TXQTerm_Visitor.leave(intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction;
begin
  Result := xqtvaContinue;
end;

class function TXQTerm_Visitor.startVisiting(term: PXQTerm): TXQTerm_VisitAction;
var
  tempVisitor: TXQTerm_Visitor;
begin
  tempVisitor := ClassType.Create as TXQTerm_Visitor;
  result := tempVisitor.simpleTermVisit(term, nil);
  tempVisitor.free;
end;

procedure TXQTerm_Visitor.replace(term: PXQTerm; newterm: TXQTerm);
begin
  term^.free;
  term^ := newterm;
end;

function TXQTerm_Visitor.simpleTermVisit(term: PXQTerm; theparent: TXQTerm): TXQTerm_VisitAction;
begin
  parent := theparent;
  result := visit(term);
  case result of
    xqtvaAbort: exit;
    xqtvaNoRecursion: ;
    else result := term^.visitchildren(self);
  end;
  //case result of xqtvaAbort: exit; end;

  parent := theparent;
  result := leave(term);
  //case result of xqtvaAbort: exit; end;
end;

procedure TXQTerm_Visitor.declare(v: PXQTermVariable; theparent: TXQTerm);
begin
  parent := theparent;
  declare(v);
end;

procedure TXQTerm_Visitor.undeclare(v: PXQTermVariable; theparent: TXQTerm);
begin
  parent := theparent;
  undeclare(v);
end;

function TXQTerm_Visitor.simpleTermVisitNoRecurse(term: PXQTerm; theparent: TXQTerm): TXQTerm_VisitAction;
begin
  parent := theparent;
  result := visit(term);
  case result of xqtvaAbort: exit;  end;

  parent := theparent;
  result := leave(term);
end;




{ TXQTermReadProperty }

constructor TXQTermReadObjectProperty.create(apropname: string);
begin
  propname := apropname;
end;

function TXQTermReadObjectProperty.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  obj: IXQValue;
  seq: TXQVList;
  v: PIXQValue;
begin
  if length(children) = 0 then raiseEvaluationError('pxp:OBJ', 'Found no term to read property from');
  if length(children) > 1 then raiseEvaluationError('pxp:OBJ', 'Can only read property from one term');
  obj:=children[0].evaluate(context);

  seq := TXQVList.create(obj.getSequenceCount);
  for v in obj.GetEnumeratorPtrUnsafe do
    if v^.kind = pvkObject then
       seq.add(v^.getProperty(propname));
  xqvalueSeqSqueezed(result, seq)
//  if not (obj is TXQValueObject) then raiseEvaluationError('pxp:OBJ', 'Expected object, got: '+obj.toString);
//  result := (obj as TXQValueObject).getProperty(propname);
end;

function TXQTermReadObjectProperty.getContextDependencies: TXQContextDependencies;
begin
  Result:=children[0].getContextDependencies;
end;

function TXQTermReadObjectProperty.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermReadObjectProperty(result).propname := propname;
end;

{ TXQTermIf }

constructor TXQTermIf.create;
begin

end;

constructor TXQTermIf.createLogicOperation(const isOr: boolean; a, b: TXQTerm);
begin
  push(a);
  b := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'boolean', [b]);
  if isOr then begin
    push(TXQTermConstant.create(xqvalueTrue));
    push(b);
  end else begin
    push(b);
    push(TXQTermConstant.create(xqvalueFalse));
  end;
end;

function TXQTermIf.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  if length(children)<>3 then raiseEvaluationError('XPST0003', 'if must have 3 arguments');
  if children[0].evaluate(context).toBooleanEffective then result:=children[1].evaluate(context)
  else result:=children[2].evaluate(context);
end;

function TXQTermIf.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;

{ TXQTermFilterSequence }

constructor TXQTermFilterSequence.create(seq: TXQTerm; filter: TXQTerm);
begin
  push(seq);
  if filter <> nil then push(filter);
end;

function TXQTermFilterSequence.evaluate(var context: TXQEvaluationContext): IXQValue;
var temp: TXQPathMatchingStepFilter;
  list: TXQVList;
  seq: TXQValueSequence;
begin
  list := TXQVList.create();
  seq := TXQValueSequence.create(list);
  result := seq;

  temp.filter := children[1];
  temp.dependencies := temp.filter.getContextDependencies;

  TXQueryEngine.filterSequence(children[0].evaluate(context), list, temp, context);
  xqvalueSeqSqueeze(result);
end;

function TXQTermFilterSequence.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  if length(children) = 0 then exit;
  result := children[0].getContextDependencies;
  for i := 1 to high(children) do
    result += children[i].getContextDependencies - ALL_CONTEXT_DEPENDENCIES_FOCUS; //since the focus are the sequence values there are no dependancies on the old focus
end;


{ TXQTermDefineVariable }

constructor TXQTermDefineVariable.create(avarname: string; anamespace: INamespace);
begin
  variable := TXQTermVariable.create(avarname, anamespace);
end;

constructor TXQTermDefineVariable.create(vari: TXQTerm; value: TXQTerm);
  function checkAssignable(t: TXQTerm): boolean;
  begin
    if (t is TXQTermFilterSequence) and (length(TXQTermFilterSequence(t).children) = 1) then t := TXQTermFilterSequence(t).children[0];
    if t is TXQTermFilterSequence then t := TXQTermFilterSequence(t).children[0];
    while t <> nil do begin
      if (t is TXQTermSequence) and (length(TXQTermSequence(t).children) = 1) then
         t := TXQTermSequence(t).children[0]
      else if (t is TXQTermNodeMatcher) or (t is TXQTermVariable) or (t is TXQTermEQNameToken) then
        exit(true)
      else if (t is TXQTermReadObjectProperty) or (t is TXQTermDynamicFunctionCall) then
        t := TXQTermReadObjectProperty(t).children[0]
      else
        exit(false);
    end;
    exit(false);
  end;
begin
  if not checkAssignable(vari) then
    raiseParsingError('XPST0003', 'Invalid term to assign a variable to: '+vari.debugTermToString);
  variable := vari;
  if value <> nil then push(value);
end;

function TXQTermDefineVariable.evaluate(var context: TXQEvaluationContext): IXQValue;
var varname: string;
    varnamespace: string;
    properties: TStringArray; //properties or array elements
    v: TXQTerm;
    assignmentMode: (amOverride, amArrayOverride, amArrayInsert, amArrayAppend);
    arrayIndex: integer;
    value: IXQValue;

    seq: TXQValueSequence;
    temp: PIXQValue;

    procedure applyProperties;
    var i: Integer;
      temp: TXQValueJSONArray;
      j: Integer;
    begin
      for i := 0 to high(properties) do
        case value.kind of
          pvkObject: value := value.getProperty(properties[i]);
          pvkArray: begin
            temp := value as TXQValueJSONArray;
            j := StrToInt(properties[i]);
            if j < 0 then raiseEvaluationError('pxp:VAR', 'Negative array elements cannot be assigned to');
            if (j = 0) or (j > temp.seq.Count) then value := xqvalue()
            else value := temp.seq.get(j - 1);
          end;
          else begin
            value := xqvalue();
            exit;
          end;
        end;
    end;
begin
  if length(children) = 0 then raiseEvaluationError('XPST0003', 'Value missing');
  if length(children) > 1 then raiseEvaluationError('XPST0003', 'Only one value can be assigned to a variable');
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot assign global variables without a xquery engine (e.g. from an interpreted function in a native module)');
  result := children[0].evaluate(context);
  properties := nil;
  assignmentMode := amOverride;

  v := variable;
  while true do begin
    if v is TXQTermSequence then begin
      if length(TXQTermSequence(v).children) > 1 then raiseEvaluationError('pxp:VAR', 'Only one variable can be assigned to');
      if length(TXQTermSequence(v).children) = 0 then raiseEvaluationError('pxp:VAR', 'Cannot assign to empty sequence');
      v := TXQTermSequence(v).children[0];
    end else if v is TXQTermNodeMatcher then begin
      raiseEvaluationError('pxp:VAR', 'node matcher term as variable name in assignment has been deprecated. (but this error should never occur, since the term type is changed during parsing time)');
      //varname:=TXQTermNodeMatcher(v).select;
      //break;
    end else if v is TXQTermVariable then begin
      varname := TXQTermVariable(v).value;
      varnamespace := TXQTermVariable(v).namespace;
      break;
    end else if v is TXQTermReadObjectProperty then begin
      arrayPrepend(properties, TXQTermReadObjectProperty(v).propname);
      v := TXQTermReadObjectProperty(v).children[0];
    end else if v is TXQTermDynamicFunctionCall then begin
      if length(TXQTermDynamicFunctionCall(v).children) <> 2 then raiseEvaluationError('pxp:VAR', 'invalid argument count for object property assignment');
      arrayPrepend(properties, TXQTermDynamicFunctionCall(v).children[1].evaluate(context).toString);
      v := TXQTermDynamicFunctionCall(v).children[0];
    end else if v is TXQTermFilterSequence then begin
      if not (assignmentMode in [amOverride, amArrayAppend]) or (length(properties) > 0) then raiseEvaluationError('pxp:VAR', 'Array assignment cannot be followed by property assignment');
      if length(TXQTermFilterSequence(v).children) = 2 then begin
        arrayIndex := TXQTermFilterSequence(v).children[1].evaluate(context).toInt64;
        if arrayIndex < 0 then raiseEvaluationError('pxp:VAR', 'Cannot assign to negative array index');
        if assignmentMode <> amArrayAppend then assignmentMode := amArrayOverride
        else assignmentMode := amArrayInsert; //due to automatical flattening, appending to element i, is equal to insertion after element i
      end else assignmentMode := amArrayAppend;
      v := TXQTermFilterSequence(v).children[0];
    end else raiseEvaluationError('pxp:VAR', 'Unexpected term. Expected variable/object-property name, got '+v.debugTermToString);
  end;

  case assignmentMode of
    amOverride: value := Result;
    amArrayOverride, amArrayInsert: begin
      if not context.hasGlobalVariable(varname, value, varnamespace) then value := xqvalue;
      applyProperties;
      if ({%H-}arrayIndex = 1) and (value.getSequenceCount <= 1) then value := result
      else if arrayIndex > value.getSequenceCount then begin
        seq := TXQValueSequence.create(value);
        seq.add(result);
        value := seq;
      end
      else begin
        seq := TXQValueSequence.create(max(arrayIndex, value.getSequenceCount));
        if arrayIndex = 0 then seq.seq.add(result);
        for temp in value.GetEnumeratorPtrUnsafe do begin
          dec(arrayIndex);
          if arrayIndex <> 0 then seq.seq.add(temp^)
          else begin
            if assignmentMode = amArrayInsert then
              seq.seq.add(temp^);
            seq.seq.add(result);
          end;
        end;
        if arrayIndex > 0 then seq.seq.add(result);
        value := seq;
      end;
    end;
    amArrayAppend:
      if not context.hasGlobalVariable(varname, value, varnamespace) then
        value := Result
      else begin
       applyProperties;
       seq := TXQValueSequence.create(value);
       seq.add(result);
       value := seq;
     end;
  end;

  if length(properties) = 0 then //test is unnessesary, as it should work with only the last branch. But being explicit looks better
    context.staticContext.sender.VariableChangelog.add(varname, value{%H-}, varnamespace)
   else
    context.staticContext.sender.VariableChangelog.addObjectModification(varname, value, varnamespace, properties)


  (*exit;

  variable.;

  //this is never called for true XQuery variables, they are handled from the module term.
  if not assigned(context.staticContext.sender.OnDefineVariable) then raiseEvaluationError('XPST0001', ':= can''t be used without a method assigned to the OnDefineVariable event');

  if namespace <> nil then context.staticContext.sender.OnDefineVariable(self, namespace.getPrefix + ':' + variablename, result)
  else context.staticContext.sender.OnDefineVariable(self, variablename, result);*)
end;

//returns the value, if the definition is a standard xquery 'declare variable $ ...'
function TXQTermDefineVariable.getClassicValue(var context: TXQEvaluationContext): IXQValue;
var
  hasExpression, extern, hasTypeDeclaration: Boolean;
  name, nsu: string;
  expression: TXQTerm;
  sequenceType: TXQTermSequenceType;
begin
  sequenceType := getSequenceType;
  hasTypeDeclaration := sequenceType <> nil;
  expression := getExpression;
  hasExpression := expression <> nil;
  extern := not hasExpression;
  if not extern and (length(annotations) > 0) then
    extern := annotations[high(annotations)].name.isEqual(XMLNamespaceURL_MyExtensionsNew, 'external');

  result := nil;
  if hasExpression then
    result := expression.evaluate(context);

  if extern then begin
    if (context.staticContext.sender <> nil) and assigned(context.staticContext.sender.OnDeclareExternalVariable) then begin
     //raiseParsingError('XPST0001','External variable declared, but no callback registered to OnDeclareExternalVariable.');
      name := (variable as TXQTermVariable).value;
      nsu := (variable as TXQTermVariable).namespace;
      context.staticContext.sender.OnDeclareExternalVariable(context.staticContext.sender, context.staticContext, nsu, name, result);
    end;
  end;
  if result = nil then raiseEvaluationError('XPDY0002', 'No value for external variable ' + getVariable.ToString+ ' given.');

  if hasTypeDeclaration then
    if not sequenceType.instanceOf(result, context) then
      raiseEvaluationError('XPTY0004', 'Variable '+getVariable.ToString + ' with value ' +result.toXQuery() + ' has not the correct type '+TXQTermSequenceType(children[0]).serialize);
end;

function TXQTermDefineVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
  if length(children) > 0 then result := children[high(children)].getContextDependencies;
  result += [xqcdContextVariables];
end;

function TXQTermDefineVariable.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  v: PXQTerm;
begin
  Result:=inherited visitchildren(visitor);

  v := @variable;
  while result = xqtvaContinue do begin
    if v^ is TXQTermSequence then begin
      if length(TXQTermSequence(v^).children) <> 1 then raiseEvaluationError('pxp:VAR', 'Only one variable can be assigned to');
      v := @TXQTermSequence(v^).children[0];
    end else if v^ is TXQTermNodeMatcher then begin
      raiseEvaluationError('pxp:VAR', 'node matcher term as variable name in assignment has been deprecated. (but this error should never occur, since the term type is changed during parsing time)');
    end else if (v^ is TXQTermVariable) or (v^ is TXQTermPendingEQNameToken {only happens during parsing}) then begin
      visitor.declare(PXQTermVariable(v), self);
      result := visitor.simpleTermVisit(v, self);
      visitor.undeclare(PXQTermVariable(v), self);
      break;
    end else begin
      visitor.simpleTermVisitNoRecurse(v, self);
      if v^ is TXQTermReadObjectProperty then begin
        v := @TXQTermReadObjectProperty(v^).children[0];
      end else if (v^ is TXQTermDynamicFunctionCall) or (v^ is TXQTermFilterSequence) then begin
        case length(TXQTermWithChildren(v^).children) of
          0, 1: ;
          2: visitor.simpleTermVisit(@TXQTermWithChildren(v^).children[1], v^);
          else raiseEvaluationError('pxp:VAR', 'invalid argument count for object property assignment');
        end;
        v := @TXQTermWithChildren(v^).children[0];
      end else raiseEvaluationError('pxp:VAR', 'Unexpected term. Expected variable/object-property name, got '+v^.debugTermToString);
    end;
  end;
end;

function TXQTermDefineVariable.clone: TXQTerm;
var
  other: TXQTermDefineVariable;
begin
  Result:=inherited clone;
  other := TXQTermDefineVariable(result);
  other.variable:=variable.clone;
  copyAnnotations(other.annotations, annotations);
end;

function TXQTermDefineVariable.getVariable: TXQTermVariable;
var
  v: TXQTerm;
begin
  if (variable.ClassType = TXQTermVariable) or (variable is TXQTermVariable) then
    exit(TXQTermVariable(variable));
  v := variable;
  while true do begin
    if v is TXQTermSequence then v := TXQTermSequence(v).children[0]
    else if v is TXQTermVariable then exit(TXQTermVariable(v))
    else if v is TXQTermReadObjectProperty then v := TXQTermReadObjectProperty(v).children[0]
    else if v is TXQTermDynamicFunctionCall then  v := TXQTermDynamicFunctionCall(v).children[0]
    else if v is TXQTermFilterSequence then v := TXQTermFilterSequence(v).children[0]
    else raiseInternalError(1651217);
  end;
  raiseInternalError(1651218);
end;

function TXQTermDefineVariable.getExpression: TXQTerm;
begin
  if (length(children) > 0) and not (children[high(children)] is TXQTermSequenceType) then exit(children[high(children)])
  else exit(nil);
end;

function TXQTermDefineVariable.getSequenceType: TXQTermSequenceType;
begin
  if (length(children) > 0) and (children[0] is TXQTermSequenceType) then exit(TXQTermSequenceType(children[0]))
  else exit(nil);
end;

function TXQTermDefineVariable.isExternal: boolean;
begin
  result := (getExpression = nil) or (
    (length(annotations) > 0)
    and (annotations[high(annotations)].name.isEqual(XMLNamespaceURL_MyExtensionsNew, 'external')) );
end;

destructor TXQTermDefineVariable.destroy;
begin
  freeAnnotations(annotations);
  variable.Free;
  inherited destroy;
end;

{ TXQTermType }

constructor TXQTermSequenceType.create();
begin
end;

constructor TXQTermSequenceType.create(atomic: TXSType; aallowNone: boolean);
begin
  kind := tikAtomic;
  atomicTypeInfo := atomic;
  allowNone := aallowNone;
end;

destructor TXQTermSequenceType.destroy;
var
  i: Integer;
begin
  for i := 0 to high(arguments) do arguments[i].free;
  if kind = tikElementTest then
    nodeMatching.destroy;
  inherited destroy;
end;


function TXQTermSequenceType.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := TXQValueFunction.create(self); //return a function returning this term. This allows returning the type itself, without having a "type"-type in the type system.
end;

function TXQTermSequenceType.serialize: string;
var
  i: Integer;
begin
  result := name;
  case kind of
    tikNone: exit( 'empty-sequence()' );
    tikAny: result := 'item()';
    tikAtomic: begin
      if atomicTypeInfo = nil then exit(name + '(TYPE NOT FOUND)');
      if atomicTypeInfo.schema <> baseSchema then result := 'Q{' + atomicTypeInfo.schema.url + '}'
      else result :=  '';
      result += atomicTypeInfo.name;
    end;
    tikFunctionTest: result := 'function';
    tikElementTest: result := nodeMatching.serialize();
    tikUnion: begin
      result := '';
      for i := 0 to high(children) do
        result += '| ' + (children[i] as TXQTermSequenceType).serialize;
      delete(result, 1, 2);
    end;
  end;
  if allowNone and allowMultiple then result += '*'
  else if allowMultiple then result += '+'
  else if allowNone then result += '?';
  //incomplete??
end;

function TXQTermSequenceType.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;

function TXQTermSequenceType.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
begin
  Result:=inherited visitchildren(visitor);
  if result <> xqtvaContinue then exit;
  for i := 0 to high(arguments) do result := visitor.simpleTermVisit(@arguments[i], self);
end;

function TXQTermSequenceType.isSingleType(): boolean;
begin
  result := (kind = tikAtomic) and not allowMultiple;
end;

function TXQTermSequenceType.castableAsBase(v: IXQValue; staticContext: TXQStaticContext): boolean;
begin
  case v.getSequenceCount of
    0: result := allowNone;
    1: begin
      if v.kind = pvkSequence then v := v.get(1);  //does this matter?
      if atomicTypeInfo.storage <> TXQValueQName then result := atomicTypeInfo.tryCreateValue(v) = xsceNoError
      else result := (atomicTypeInfo as TXSQNameType).castable(v, staticContext);
    end;
    else result := false;
  end;
end;

function TXQTermSequenceType.castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
begin
  v := xqvalueAtomize(v);
  case v.getSequenceCount of
    0: begin
      if not allowNone then raiseXPTY0004TypeError(v, debugTermToString);
      result := xqvalue();
    end;
    1: begin
      if atomicTypeInfo.storage <> TXQValueQName then result := atomicTypeInfo.createValue(v)
      else result := (atomicTypeInfo as TXSQNameType).cast(v, context);
    end;
    else begin raiseXPTY0004TypeError(v, debugTermToString); result := nil; end;
  end;
end;

function TXQTermSequenceType.castableAs(v: IXQValue; staticContext: TXQStaticContext): boolean;
begin
  if kind <> tikAtomic then raiseEvaluationError('XPST0051', 'need atomic type for castable as');
  v := xqvalueAtomize(v);
  result := castableAsBase(v, staticContext);
end;

function TXQTermSequenceType.instanceOf(const ta: IXQValue; const context: TXQEvaluationContext): boolean;


  function instanceOfSingleType(const sub: IXQValue): boolean;
  var
    t: TXSType;
    fun: TXQValueFunction;
    i: Integer;
  begin
    case kind of
      //tikAny, tikNone: ; //handled before
      tikAtomic: begin
        t := sub.typeAnnotation;;
        result := t.derivedFrom(atomicTypeInfo);
      end;
      tikFunctionTest: begin
        if sub.kind <> pvkFunction then exit(false);
        if length(arguments) = 0 then exit(true);
        fun := sub as TXQValueFunction;
        if (length(arguments) <> length(fun.parameters) + 1) then exit(false);
        for i := 0 to high(fun.parameters) do
          if (fun.parameters[i].seqtype <> nil) and not arguments[i].subtypeOf(fun.parameters[i].seqtype) then exit(false);
        if fun.resulttype <> nil then result := fun.resulttype.subtypeOf(arguments[high(arguments)])
        else result := arguments[high(arguments)].isItemStar();
      end;
      else result := false; //hide warning
    end;
  end;

  function elementTest(): boolean;
  var nodeCondition: TXQPathNodeCondition;
    v: PIXQValue;
    node: TTreeNode;
  begin
    //this is a sub function, so the f/initialization of nodeCondition is only done for tests that actually are element tests
    TXQueryEngine.unifyQuery(nil, self.nodeMatching, nodeCondition);
    if qmCheckNamespacePrefix in self.nodeMatching.matching then begin
      include(nodeCondition.options, xqpncCheckNamespace);
      if qmAttribute in nodeMatching.matching then nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkUnknown)
      else nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkElementType);
    end;
    if (context.staticContext <> nil) and (context.staticContext.nodeCollation <> nil) then
      nodeCondition.equalFunction := @context.staticContext.nodeCollation.equal;
    result := true;
    for v in ta.GetEnumeratorPtrUnsafe do begin
      node := v^.toNode;
      result := result and assigned(node) and TXQueryEngine.nodeMatchesQueryLocally(nodeCondition, v^.toNode);
      if not result then exit;
    end;
  end;

var
  count: Integer;
  i: Integer;
  v: PIXQValue;
begin
  //compare count with sequence count
  case kind of
    tikNone: exit(ta.isUndefined);
    tikUnion: begin
      for i := 0 to high(children) do
        if TXQTermSequenceType(children[i]).instanceOf(ta, context) then exit(true);
      exit(false);
    end;
  end;
  count := ta.getSequenceCount;
  if ((count = 0) and not allowNone) or
     ((count > 1) and not allowMultiple) then
    exit(false);
  if (count = 0) then
    exit(true);
  //compare item type
  if kind = tikAny then
    exit(true);
  if kind = tikElementTest then
    exit(elementTest);
  result := true;
  for v in ta.GetEnumeratorPtrUnsafe do begin
    result := result and instanceOfSingleType(v^);
    if not result then exit;
  end;
end;

function TXQTermSequenceType.instanceOf(const ta: IXQValue): boolean;
var temp: TXQEvaluationContext;
begin
  temp.staticContext := nil;
  result := instanceOf(ta, temp);
end;

{$ImplicitExceptions off}
function TXQTermSequenceType.instanceOf(const node: TTreeNode): boolean;
begin
  result := instanceof(xqvalue(node));
end;
{$ImplicitExceptions on}

function TXQTermSequenceType.subtypeOf(tb: TXQTermSequenceType): boolean;
begin
  if kind = tikNone then exit((tb.kind = tikNone) or tb.allowNone);
  if tb.kind = tikNone then
    exit(allowNone and (kind = tikAtomic) and (atomicTypeInfo = baseSchema.error)); //special case: xs:error? is empty-sequence()
  if (allowNone and not tb.allowNone) or (allowMultiple and not tb.allowMultiple) then exit(false);
  result := subtypeItemTypeOf(tb);
end;


function TXQTermSequenceType.subtypeItemTypeOf(tb: TXQTermSequenceType): boolean;
  function subtypeItemTypeAtomic(xsa, xsb: TXSType): boolean;
  var
    i: Integer;
  begin
    if xsa.derivedFrom(xsb) then exit(true);
    result := false;
    if xsa is TXSUnionType then begin
      //should test for "Ai is a pure union type, and every type t in the transitive membership of Ai satisfies subtype-itemType(t, Bi)."
      //does it work???
      result := true;
      for i := 0 to high(TXSUnionType(atomicTypeInfo).members) do
        if not subtypeItemTypeAtomic(TXSUnionType(atomicTypeInfo).members[i], xsb) then
          exit(false);
    end;
  end;
var
  i: Integer;
begin
  if (kind = tikAtomic) and (tb.kind = tikAtomic) and subtypeItemTypeAtomic(atomicTypeInfo, tb.atomicTypeInfo) then
    exit(true);
  if tb.kind = tikAny then exit(true);
  if (kind = tikElementTest) and (tb.kind = tikElementTest) then begin
    if tb.nodeMatching.matching  = MATCH_ALL_NODES then exit(true);
    if nodeMatching.matching * MATCH_ALL_NODES <> tb.nodeMatching.matching * MATCH_ALL_NODES then exit(false);
    if (qmValue in tb.nodeMatching.matching) and (not (qmValue in nodeMatching.matching) or (tb.nodeMatching.value <> nodeMatching.value))  then exit(false);
    if (qmCheckNamespaceURL in tb.nodeMatching.matching) and (not (qmCheckNamespaceURL in nodeMatching.matching) or (tb.nodeMatching.namespaceURLOrPrefix <> nodeMatching.namespaceURLOrPrefix))  then exit(false);
    if (qmCheckNamespacePrefix in tb.nodeMatching.matching) and (not (qmCheckNamespacePrefix in nodeMatching.matching) or (tb.nodeMatching.namespaceURLOrPrefix <> nodeMatching.namespaceURLOrPrefix))  then exit(false);
    if (tb.nodeMatching.requiredType <> nil)
        and ((tb.nodeMatching.requiredType.kind <> tikAtomic) or (tb.nodeMatching.requiredType.atomicTypeInfo.name <> 'anyType') or (tb.nodeMatching.requiredType.atomicTypeInfo.base <> nil)) then
      exit((nodeMatching.requiredType <> nil) and (nodeMatching.requiredType.subtypeOf(tb.nodeMatching.requiredType)));
    exit(true);
  end;
  if (kind = tikFunctionTest) and (tb.kind = tikFunctionTest) then begin
    if length(tb.arguments) > 0 then begin
      if length(arguments) <> length(tb.arguments) then exit(false);
      for i := 0 to high(arguments) - 1 do
        if not tb.arguments[i].subtypeOf(arguments[i]) then exit(false);
      if not arguments[high(arguments)].subtypeOf(tb.arguments[high(tb.arguments)]) then exit(false);
    end;
    //subtype-assertions(AnnotationsA, AnnotationsB) is not defined (??)
    exit(true);
  end;
  exit(false);
end;

function TXQTermSequenceType.functionCoercion(const v: IXQValue): IXQValue;
var
  fun: TXQValueFunction;
  needCoercion: Boolean;
  f: TXQValueFunction;
  i: Integer;
  fresult: TXQValueFunction;
  tempvar: TXQTermVariable;
begin
  //function coercion f(v, w, ..)=>s  passed to f(a, b, ..)=>r becomes f(a intersect v, b intersect w, ..)=>r intersect s
  //if v.kind <> pvkFunction then raise EXQEvaluationException.Create('XPTY0004', 'Expected function, but got : '+result.toXQuery());
  if length(arguments) = 0 then exit(v);
  fun := v as TXQValueFunction;
  if (length(arguments) <> length(fun.parameters) + 1) then raise EXQEvaluationException.Create('XPTY0004', 'Invalid argument count');
  needCoercion := false;
  for i := 0 to high(fun.parameters) do
    if (not arguments[i].isItemStar)
       and ((fun.parameters[i].seqtype = nil) or not fun.parameters[i].seqtype.subtypeOf(arguments[i])) then begin
      needCoercion:=true;
      break;
    end;
  if not needCoercion then
    if (not arguments[high(arguments)].isItemStar) and (((fun.resulttype = nil) or not fun.resulttype.subtypeOf(arguments[high(arguments)])))
        then
          needCoercion := true;
  if not needCoercion then exit(v);

  //see TXQTermDefineFunction.defineDynamicPartialApplication
  f := fun;
  fresult := TXQValueFunction.create();
  fresult.namespaceURL := f.namespaceURL;
  fresult.namespacePrefix := f.namespacePrefix;
  fresult.name:=f.name;
  copyAnnotations(fresult.annotations, f.annotations);
  fresult.body := TXQTermDynamicFunctionCall.create(TXQTermConstant.Create(f));
  setlength(fresult.parameters, length(arguments) - 1);
  for i := 0 to high(f.parameters) do begin
    tempvar := TXQTermVariable.create(IntToStr(i)+'.');
    tempvar.index := high(f.parameters);
    TXQTermDynamicFunctionCall(fresult.body).push(tempvar);
    fresult.parameters[i].variable := TXQTermVariable(TXQTermDynamicFunctionCall(fresult.body).children[high(TXQTermDynamicFunctionCall(fresult.body).children)].clone);
    fresult.parameters[i].seqtype := TXQTermSequenceType(arguments[i].clone);
  end;
  fresult.ownsTerms := true;
  fresult.resulttype := TXQTermSequenceType(arguments[high(arguments)].clone);
  fresult.context := f.context;
  result := fresult;
end;

function TXQTermSequenceType.isItemStar: boolean;
var
  i: Integer;
begin
  result := (kind = tikAny) and allowMultiple and allowNone;
  if kind = tikUnion then
    for i := 0 to high(children) do result := result or TXQTermSequenceType(children[i]).isItemStar();
end;

function TXQTermSequenceType.clone: TXQTerm;
var
  other: TXQTermSequenceType;
  i: Integer;
begin
  result := inherited clone;
  other := TXQTermSequenceType(result);
  other.name := name;
  other.allowNone := allowNone;
  other.allowMultiple := allowMultiple;
  other.kind := kind;
  other.atomicTypeInfo := atomicTypeInfo;
  other.nodeMatching := nodeMatching.clone;
  SetLength(other.arguments, length(arguments));
  for i:=0 to high(arguments) do
    other.arguments[i] := arguments[i].clone as TXQTermSequenceType;
end;

{ TXQTermSequence }

function TXQTermSequence.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  i: Integer;
  tempSeq: TXQValueSequence;
  targetLength, ccount, stacksize: Integer;
  stack: TXQEvaluationStack;
  evilkids: PIXQValue;
begin
  ccount := length(children);
  if ccount = 0 then exit(xqvalue);
  if ccount = 1 then exit(children[0].evaluate(context));
  stack := context.temporaryVariables;
  stacksize := stack.Count;
  targetLength := 0;
  for i := 0 to ccount - 1 do begin
    stack.push(children[i].evaluate(context));
    targetLength += stack.topptr()^.getSequenceCount;
  end;
  evilkids := stack.topptr(ccount - 1);
  case targetLength of
    0: result := xqvalue;
    1: begin
      result := evilkids[0];
      for i := 1 to ccount - 1 do
        if not evilkids[i].isUndefined then begin
          result := evilkids[i];
          break;
        end;
    end
    else begin
      tempSeq := TXQValueSequence.create(targetLength);
      result := tempSeq;
      for i := 0 to ccount - 1 do
        tempSeq.add(evilkids[i]);
    end;
  end;
  stack.popTo(stacksize);
end;

function TXQTermSequence.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  for i:= 0 to high(children) do result += children[i].getContextDependencies;
end;

{ TXQTermArray }

function TXQTermJSONArray.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  i: Integer;
  tempSeq: TXQValueJSONArray;
begin
  tempSeq := TXQValueJSONArray.create(length(children));
  for i := 0 to high(children) do
    tempSeq.add(children[i].evaluate(context));
  result := tempSeq;
end;

function TXQTermJSONArray.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  for i:= 0 to high(children) do result += children[i].getContextDependencies;
end;


{ TXQTermSomeEvery }

constructor TXQTermSomeEvery.create(every: boolean);
begin
  isEvery:=every;
end;

function TXQTermSomeEvery.evaluate(var context: TXQEvaluationContext): IXQValue;
  function evaluateSomeEvery(pos:integer): boolean;
  var seq: IXQValue;
      typeDelta: integer;
      v: PIXQValue;
  begin
    if pos = high(children) then exit(children[pos].evaluate(context).toBooleanEffective);
    if not (children[pos] is TXQTermVariable) then raiseEvaluationError('err:XPST0003', 'Expected variable, but got '+children[pos].debugTermToString);
    if children[pos + 1] is TXQTermSequenceType then typeDelta:=1
    else typeDelta:=0;

    result := isEvery;
    seq := children[pos+1+typeDelta].evaluate(context);
    for v in seq.GetEnumeratorPtrUnsafe do begin
      if typeDelta <> 0 then
        if not TXQTermSequenceType(children[pos+typeDelta]).instanceOf(v^, context) then
          raiseEvaluationError('XPTY0004', 'Invalid type for variable '+ v^.toString+ ' expected ' + TXQTermSequenceType(children[pos+typeDelta]).serialize);
      Context.temporaryVariables.push(TXQTermVariable(children[pos]), v^);
      if evaluateSomeEvery(pos+2+typeDelta) <> (isEvery) then exit(not isEvery);
      Context.temporaryVariables.pop;
    end;
  end;

var
  stackCount: Integer;
begin
  stackCount := context.temporaryVariables.Count;
  result:=xqvalue(evaluateSomeEvery(0));
  context.temporaryVariables.popTo(stackCount);
end;

function TXQTermSomeEvery.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;

function TXQTermSomeEvery.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
  function visitSomeEvery(pos:integer): TXQTerm_VisitAction;
  var typeDelta: integer;
  begin
    if pos = high(children) then
      exit(visitor.simpleTermVisit(@children[pos], self));
    if children[pos + 1] is TXQTermSequenceType then begin
      result := visitor.simpleTermVisit(@children[pos+1], self);
      case result of xqtvaAbort: exit; end;
      typeDelta:=1;
    end else typeDelta:=0;

    result := visitor.simpleTermVisit(@children[pos+1+typeDelta], self);
    case result of xqtvaAbort: exit; end;

    visitor.declare(@children[pos], self);

    result := visitSomeEvery(pos+2+typeDelta);
    case result of xqtvaAbort: exit; end;

    visitor.undeclare(@children[pos], self);
  end;
begin
  result := visitSomeEvery(0);
end;

function TXQTermSomeEvery.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermSomeEvery(result).isEvery:=isEvery;
end;

{ TXQTermFor }

{ TXQTermFlowerSubClause }

class function TXQTermFlowerSubClause.kind: TXQTermFlowerSubClauseKind;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'flower fail');
  result := xqtfcOrder;
end;

function TXQTermFlowerSubClause.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  raiseEvaluationError('pxp:INTERNAL', 'A flower subclause cannot be evaluated');
  result := nil;
end;

function TXQTermFlowerSubClause.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  ignore(visitor);
  raiseEvaluationError('pxp:INTERNAL', 'An abstract flower subclause cannot be visited');
  Result:=xqtvaAbort;
end;

procedure TXQTermFlowerSubClause.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  ignore(visitor);
end;


{ TXQTermFlowerLet }

class function TXQTermFlowerLet.kind: TXQTermFlowerSubClauseKind;
begin
  result := xqtfcLet;
end;

function TXQTermFlowerLet.getContextDependencies: TXQContextDependencies;
begin
  result := [xqcdContextVariables];
  //if Assigned(loopvar) then result += loopvar.getContextDependencies;
  //if Assigned(pattern) then result += pattern.getContextDependencies;
  //if Assigned(sequenceTyp) then result += sequenceTyp.getContextDependencies;
  if Assigned(expr) then result += expr.getContextDependencies;
end;

function TXQTermFlowerLet.clone: TXQTerm;
begin
  Result:=inherited clone();
  TXQTermFlowerLet(result).loopvar := TXQTermVariable(loopvar.clone);
  if Assigned(sequenceTyp) then TXQTermFlowerLet(result).sequenceTyp := TXQTermSequenceType(sequenceTyp.clone);
  TXQTermFlowerLet(result).expr := expr.clone;
end;

destructor TXQTermFlowerLet.destroy;
begin
  loopvar.free;
  sequenceTyp.free;
  expr.free;
  inherited destroy;
end;

function TXQTermFlowerLet.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@expr, self);
  case result of xqtvaAbort: exit; end;
  if sequenceTyp <> nil then begin
    result := visitor.simpleTermVisit(@sequenceTyp, self);
    case result of xqtvaAbort: exit; end;
  end;
  //todo: visit loopvar, too?
  visitor.declare(@loopvar);
end;

procedure TXQTermFlowerLet.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  visitor.undeclare(@loopvar);
end;

{ TXQTermFlowerFor }

class function TXQTermFlowerFor.kind: TXQTermFlowerSubClauseKind;
begin
  result := xqtfcFor;
end;

function TXQTermFlowerFor.clone: TXQTerm;
begin
  Result:=inherited clone;
  if Assigned(positionVar) then TXQTermFlowerFor(result).positionVar := TXQTermVariable(positionVar.clone);
end;

destructor TXQTermFlowerFor.destroy;
begin
  positionVar.Free;
  inherited destroy;
end;

function TXQTermFlowerFor.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  Result:=inherited visitchildren(visitor);
  if (positionVar <> nil) then
   visitor.declare(@positionVar);
end;

procedure TXQTermFlowerFor.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  if (positionVar <> nil) then
   visitor.undeclare(@positionVar);
  inherited visitchildrenToUndeclare(visitor);
end;

procedure TXQTermFlowerWindowVarsAndCondition.assign(const other: TXQTermFlowerWindowVarsAndCondition);
begin
  if assigned(other.when) then begin
    when := other.when.clone;
    if Assigned(other.currentItem) then currentItem := TXQTermVariable(other.currentItem.clone);
    if Assigned(other.positionVar) then positionVar := TXQTermVariable(other.positionVar.clone);
    if Assigned(other.previousItem) then previousItem := TXQTermVariable(other.previousItem.clone);
    if Assigned(other.nextItem) then nextItem := TXQTermVariable(other.nextItem.clone);
  end;
end;

function TXQTermFlowerWindowVarsAndCondition.visitchildren(visitor: TXQTerm_Visitor; parent: txqterm): TXQTerm_VisitAction;
begin
  if assigned(when) then begin
    visitor.parent := parent;
    if Assigned(currentItem) then visitor.declare(@currentItem);
    if Assigned(positionVar) then visitor.declare(@positionVar);
    if Assigned(previousItem) then visitor.declare(@previousItem);
    if Assigned(nextItem) then visitor.declare(@nextItem);
    result := visitor.simpleTermVisit(@when, parent);
  end else result := xqtvaContinue;
end;

procedure TXQTermFlowerWindowVarsAndCondition.visitchildrenToUndeclare(visitor: TXQTerm_Visitor; parent: txqterm);
begin
  if assigned(when) then begin
    visitor.parent := parent;
    if Assigned(nextItem) then visitor.undeclare(@nextItem);
    if Assigned(previousItem) then visitor.undeclare(@previousItem);
    if Assigned(positionVar) then visitor.undeclare(@positionVar);
    if Assigned(currentItem) then visitor.undeclare(@currentItem);
  end;
end;

procedure TXQTermFlowerWindowVarsAndCondition.freeAll;
begin
  when.free;
  currentItem.Free;
  positionVar.Free;
  previousItem.Free;
  nextItem.Free;
end;

class function TXQTermFlowerWindow.kind: TXQTermFlowerSubClauseKind;
begin
  Result:=xqtfcWindow;
end;

function TXQTermFlowerWindow.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlowerWindow(result).flags := flags;
  TXQTermFlowerWindow(result).startCondition.assign(startCondition);
  TXQTermFlowerWindow(result).endCondition.assign(endCondition);
end;

function TXQTermFlowerWindow.getContextDependencies: TXQContextDependencies;
begin
  Result:=(inherited getContextDependencies) + startCondition.when.getContextDependencies;
  if Assigned(endCondition.when) then result += endCondition.when.getContextDependencies;
end;

destructor TXQTermFlowerWindow.destroy;
begin
  startCondition.freeAll;
  endCondition.freeAll;
  inherited destroy;
end;

function TXQTermFlowerWindow.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@expr, self);
  case result of xqtvaAbort: exit; end;
  if sequenceTyp <> nil then begin
    result := visitor.simpleTermVisit(@sequenceTyp, self);
    case result of xqtvaAbort: exit; end;
  end;

  startCondition.visitchildren(visitor, self);
  endCondition.visitchildren(visitor, self);

  visitor.declare(@loopvar);
end;

procedure TXQTermFlowerWindow.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  visitor.undeclare(@loopvar);

  endCondition.visitchildrenToUndeclare(visitor, self);
  startCondition.visitchildrenToUndeclare(visitor, self);

end;

procedure TXQTermFlowerWindow.visitlocalvariables(callback: TXQTermFlowerWindowVariableCallback; data: pointer);
  procedure subcondition(const c: TXQTermFlowerWindowVarsAndCondition);
  begin
    with c do begin
      if assigned(currentItem) then callback(data, currentItem);
      if assigned(positionVar) then callback(data, positionVar);
      if assigned(previousItem) then callback(data, previousItem);
      if assigned(nextItem) then callback(data, nextItem);
    end;
  end;
begin
  callback(data, loopvar);
  subcondition(startCondition);
  if assigned(endCondition.when) then subcondition(endCondition);
end;


procedure XQTermFlowerWindowVariableCallbackFindDuplicatedVariable(data: pointer; v: TXQTermVariable);
begin
  TFPList(data).Add(v);
end;

function TXQTermFlowerWindow.findDuplicatedVariable: TXQTermVariable;
var
  l: TFPList;
  i,j: Integer;
begin
  l := TFPList.Create;
  try
    visitlocalvariables(@XQTermFlowerWindowVariableCallbackFindDuplicatedVariable, l);
    for i := 0 to l.Count - 1 do
      for j := 0 to i - 1 do
        if TXQTermVariable(l[i]).equalsVariable(TXQTermVariable(l[j])) then
          exit(TXQTermVariable(l[j]));
    result := nil;
  finally
    l.free;
  end;
end;

procedure XQTermFlowerWindowVariableCallbackCount(data: pointer; v: TXQTermVariable);
begin
  ignore(v);
  inc(PInteger(data)^);
end;

function TXQTermFlowerWindow.variableCount: integer;
begin
  visitlocalvariables(@XQTermFlowerWindowVariableCallbackCount, @result);
end;

{ TXQTermFlowerLetPattern }

class function TXQTermFlowerLetPattern.kind: TXQTermFlowerSubClauseKind;
begin
  Result:=xqtfcLetPattern;
end;

function TXQTermFlowerLetPattern.getContextDependencies: TXQContextDependencies;
begin
  result := expr.getContextDependencies + pattern.getContextDependencies + [xqcdContextVariables];
end;

function TXQTermFlowerLetPattern.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlowerLetPattern(result).expr := expr.clone;
  TXQTermFlowerLetPattern(result).pattern := TXQTermPatternMatcher(pattern.clone);
  if Assigned(sequenceTyp) then TXQTermFlowerLetPattern(result).sequenceTyp := TXQTermSequenceType(sequenceTyp.clone);
end;

function TXQTermFlowerLetPattern.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  j: Integer;
begin
  result := visitor.simpleTermVisit(@expr, self);
  case result of xqtvaAbort: exit; end;

  if sequenceTyp <> nil then begin
    result := visitor.simpleTermVisit(@sequenceTyp, self);
    case result of xqtvaAbort: exit; end;
  end;

  result := visitor.simpleTermVisit(@pattern, self);
  case result of xqtvaAbort: exit; end;

  for j := 0 to high(pattern.vars) do
    visitor.declare(@pattern.vars[j]);
end;

procedure TXQTermFlowerLetPattern.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
var
  j: Integer;
begin
  for j := high(pattern.vars) downto 0 do
    visitor.undeclare(@pattern.vars[j]);
end;

destructor TXQTermFlowerLetPattern.destroy;
begin
  expr.Free;
  pattern.Free;
  sequenceTyp.Free;
  inherited destroy;
end;

{ TXQTermFlowerForPattern }

class function TXQTermFlowerForPattern.kind: TXQTermFlowerSubClauseKind;
begin
  Result:=xqtfcForPattern;
end;


{ TXQTermFlowerWhere }

class function TXQTermFlowerWhere.kind: TXQTermFlowerSubClauseKind;
begin
  result := xqtfcWhere;
end;

function TXQTermFlowerWhere.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@test, self);
end;

function TXQTermFlowerWhere.getContextDependencies: TXQContextDependencies;
begin
  result := test.getContextDependencies;
end;

function TXQTermFlowerWhere.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlowerWhere(result).test := test.clone;
end;

destructor TXQTermFlowerWhere.destroy;
begin
  test.free;
  inherited destroy;
end;


{ TXQTermFlowerOrder }

class function TXQTermFlowerOrder.kind: TXQTermFlowerSubClauseKind;
begin
  result := xqtfcOrder;
end;

function TXQTermFlowerOrder.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@expr, self);
end;

function TXQTermFlowerOrder.getContextDependencies: TXQContextDependencies;
begin
  result := expr.getContextDependencies;
end;

function TXQTermFlowerOrder.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlowerOrder(result).expr := expr.clone;
  TXQTermFlowerOrder(result).descending := descending;
  TXQTermFlowerOrder(result).emptyOrder := emptyOrder;
  TXQTermFlowerOrder(result).collation := collation;
end;

destructor TXQTermFlowerOrder.destroy;
begin
  expr.Free;
  inherited destroy;
end;

class function TXQTermFlowerCount.kind: TXQTermFlowerSubClauseKind;
begin
  Result:=xqtfcCount;
end;

function TXQTermFlowerCount.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  visitor.declare(@countvar, self);
  result := xqtvaContinue;
end;

procedure TXQTermFlowerCount.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  visitor.undeclare(@countvar, self);
end;

function TXQTermFlowerCount.getContextDependencies: TXQContextDependencies;
begin
  result := [xqcdContextVariables];
end;

function TXQTermFlowerCount.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlowerCount(result).countvar := TXQTermVariable(countvar.clone);

end;

destructor TXQTermFlowerCount.destroy;
begin
  countvar.free;
  inherited destroy;
end;


class function TXQTermFlowerGroup.kind: TXQTermFlowerSubClauseKind;
begin
  Result:=xqtfcGroup;
end;

function TXQTermFlowerGroup.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
begin
  for i := 0 to high(vars) do
    visitor.simpleTermVisit(@vars[i], self);
  for i := 0 to high(seqtypes) do
    if seqtypes[i]<> nil then
      visitor.simpleTermVisit(@seqtypes[i], self);

  result := xqtvaContinue;
end;

procedure TXQTermFlowerGroup.visitchildrenToUndeclare(visitor: TXQTerm_Visitor);
begin
  ignore(visitor);
end;

function TXQTermFlowerGroup.getContextDependencies: TXQContextDependencies;
begin
  result := [xqcdContextCollation, xqcdContextVariables];
end;

function TXQTermFlowerGroup.clone: TXQTerm;
var
  i: Integer;
begin
  Result:=inherited clone;
  TXQTermFlowerGroup(result).collation := collation;
  SetLength(TXQTermFlowerGroup(result).vars, length(vars) );
  for i:= 0 to high(vars) do
    TXQTermFlowerGroup(result).vars[i] := TXQTermVariable(vars[i].clone);
  SetLength(TXQTermFlowerGroup(result).seqtypes, length(seqtypes) );
  for i:= 0 to high(seqtypes) do
    if seqtypes[i] <> nil then
      TXQTermFlowerGroup(result).seqtypes[i] := TXQTermSequenceType(seqtypes[i].clone);
end;

destructor TXQTermFlowerGroup.destroy;
var
  i: Integer;
begin
  for i := 0 to high(vars) do
    vars[i].free;
  for i := 0 to high(seqtypes) do
    seqtypes[i].free;
  inherited destroy;
end;




type TOrderedTuple = class
  orders: array of IXQValue;
  vars: array of IXQValue;
  result: IXQValue;
end;
  TFlowerSortingData = record
    //flower: TXQTermFlower;
    minOrderIndex, maxOrderIndex: integer;
    orders: array of TXQTermFlowerOrder;
    collations: array of TXQCollation;
    context: ^TXQEvaluationContext;
  end;
  PFlowerSortingData = ^TFlowerSortingData;

function flowerTupleCompareFunction(data: TObject; xa, xb: pointer): longint;
var meta: PFlowerSortingData;

  function isGreaterThan(w, v: IXQValue; emptyLeast: boolean; const collation: TXQCollation): boolean;
  var
    temp: integer;
  begin
    result := false;
    if emptyLeast then begin
      if v.isUndefined and not w.isUndefined then
        exit(true);
      if ((v is TXQValueFloat) and IsNan(v.toFloat)) and not w.isUndefined and not ((w is TXQValueFloat) and IsNan(w.toFloat)) then
        exit(true);
    end else begin
      if w.isUndefined and not v.isUndefined then
        exit(true);
      if ((w is TXQValueFloat) and IsNan(w.toFloat)) and not v.isUndefined and not ((v is TXQValueFloat) and IsNan(v.toFloat)) then
        exit(true);
    end;

    if (collation <> nil) and (v is TXQValueString) and (w is TXQValueString) then
      exit(collation.compare(v.toString, w.toString) < 0);
    temp := meta^.context^.staticContext.compareAtomic(v,w,nil) ;
    result := (temp <> -2) and (temp < 0);
  end;

var
  a, b: TOrderedTuple;
  orders: array of TXQTermFlowerOrder;
  i: Integer;
  emptyLeast: Boolean;
  order: TXQTermFlowerOrder;
begin
  meta := PFlowerSortingData(data);
  orders := meta^.orders;
  a := TOrderedTuple(ppointer(xa)^);
  b := TOrderedTuple(ppointer(xb)^);

  result := 0;

  for i := meta^.maxOrderIndex downto meta^.minOrderIndex do begin
    order := orders[i];
    emptyLeast := (order.emptyOrder = xqeoEmptyLeast) or ((order.emptyOrder = xqeoStatic) and (meta^.context^.staticContext.emptyOrderSpec = xqeoEmptyLeast));
    if isGreaterThan(a.orders[i], b.orders[i], emptyLeast, meta^.collations[i]) then result := 1
    else if isGreaterThan(b.orders[i], a.orders[i], emptyLeast, meta^.collations[i]) then result := -1;
    if orders[i].descending then result := -Result;
    if result <> 0 then exit;
  end;
end;

type TFlowerGroupingData = record
  //flower: TXQTermFlower;
  groupingIndices: array of integer;
  collation: TXQCollation;
  context: ^TXQEvaluationContext;
  function compareTuples(a, b: TOrderedTuple): integer; inline;
end;
PFlowerGroupingData = ^TFlowerGroupingData;

function TFlowerGroupingData.compareTuples(a, b: TOrderedTuple): integer;
var
  i: Integer;
  k: LongInt;
begin
  result := 0;
  for i := 0 to high(groupingIndices) do begin
    k := groupingIndices[i];
    case a.vars[k].getSequenceCount of
      0: case b.vars[k].getSequenceCount of
        0: exit(0);
        1: exit(-1); //randomly choosen
      end;
      1: if b.vars[k].getSequenceCount = 0 then exit(1);
    end;
    result :=  context^.staticContext.compareDeepAtomic(a.vars[k], b.vars[k], collation);
    if result <> 0 then exit;
  end;
end;

function flowerTupleGroupCompareFunction(data: TObject; xa, xb: pointer): longint;
var meta: PFlowerGroupingData;
begin
  meta := PFlowerGroupingData(data);
  result := meta^.compareTuples(TOrderedTuple(ppointer(xa)^), TOrderedTuple(ppointer(xb)^));
end;


function TXQTermFlower.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  sortingCache: TFPList;
  finalResult: TXQValueSequence;
  currentTuple: TOrderedTuple;
  counts: array of integer;


  procedure evaluateForLoop(const pos: Integer);
  var clausekind: TXQTermFlowerSubClauseKind;
    procedure addVariableBinding(v: IXQValue; index: integer = 1);
    var
      letclause: TXQTermFlowerLet;
      stackSize: Integer;
    begin
      letclause := TXQTermFlowerLet(children[pos]);
      if (letclause.sequenceTyp <> nil) and not (letclause.sequenceTyp.instanceOf(v, context)) then
        raiseTypeError0004('Invalid variable type, expected: type '+letclause.sequenceTyp.serialize, v);

      stackSize := context.temporaryVariables.Count;
      context.temporaryVariables.push(letclause.loopvar, v);
      if needFullTuple then
        currentTuple.vars[clauseIndex[pos]] := v;
      if (clausekind = xqtfcFor) and (TXQTermFlowerFor(letclause).positionVar <> nil) then begin
        context.temporaryVariables.push(TXQTermFlowerFor(letclause).positionVar, xqvalue(index));
        if needFullTuple then
          currentTuple.vars[clauseIndex[pos] + 1] := context.temporaryVariables.top(0);
      end;
      evaluateForLoop(pos+1);
      context.temporaryVariables.popTo(stackSize);
    end;

    procedure addWindowVariableBinding();
    var
      variableIndex: integer;
      procedure addVar(const v: TXQTermVariable; const value: IXQValue);
      begin
        context.temporaryVariables.push(v, value);
        if needFullTuple and (variableIndex <> -1) then begin
         currentTuple.vars[variableIndex] := value;
         inc(variableIndex);
        end;
      end;

      procedure addVarAs(const v: TXQTermVariable; st: TXQTermSequenceType; const value: IXQValue);
      begin
        if (st <> nil) and not (st.instanceOf(value, context)) then
          raiseTypeError0004('Invalid variable type, expected: type '+st.serialize, value);
        addVar(v, value);
      end;

    var
      binding: IXQValue;
      stackSizes: array[1..2] of integer;
      stackSizeIndex: integer;

      procedure popAll;
      begin
        dec(stackSizeIndex);
        context.temporaryVariables.popTo(stackSizes[stackSizeIndex]);
      end;

      function subsequence(const from, toi: integer): IXQValue;
      var
        seq: TXQValueSequence;
        i: Integer;
      begin
        if from = toi then exit(binding.get(from));
        seq := TXQValueSequence.create(toi-from+1);
        for i := from to toi do
          seq.seq.add(binding.get(i));
        result := seq;
      end;

      function checkCondition(at: integer; const condition: TXQTermFlowerWindowVarsAndCondition; forceTrue: boolean = false): boolean;
      var
        oldIndex: Integer;
      begin
        oldIndex := variableIndex;
        stackSizes[stackSizeIndex] := context.temporaryVariables.Count;
        inc(stackSizeIndex);
        with condition do begin
          if Assigned(currentItem) then addVar(currentItem, binding.get(at));
          if Assigned(positionVar) then addVar(positionVar, xqvalue(at));
          if Assigned(previousItem) then addVar(previousItem, binding.get(at-1));
          if Assigned(nextItem) then addVar(nextItem, binding.get(at+1));
          result := forceTrue or when.evaluate(context).toBooleanEffective;
        end;
        if not result then begin
          popAll;
          variableIndex := oldIndex;
        end;
      end;

      var start, endi, length: Integer;
          window: TXQTermFlowerWindow;

      procedure nextStep;
      begin
        if endi <= length then begin
         addVarAs(window.loopvar, window.sequenceTyp, subsequence(start, endi));
         evaluateForLoop(pos+1);
         popAll;
        end;
        if needFullTuple then variableIndex := clauseIndex[pos];
      end;

    var
      matchIdxList: array of integer;
      matchIdxListLength, i: SizeInt;
    begin
      stackSizeIndex := 1;
      window := TXQTermFlowerWindow(children[pos]);
      binding := window.expr.evaluate(context);
      start := 1;
      endi := -1;
      length := binding.getSequenceCount;
      if needFullTuple then
        variableIndex := clauseIndex[pos];


      if Assigned(window.endCondition.when) then begin
        while start <= length do begin
          if checkCondition(start, window.startCondition ) then begin
             endi := start;

             while (endi < length) and not checkCondition(endi, window.endCondition) do inc(endi);
             if endi = length then if not checkCondition(endi, window.endCondition, not (xqtfwEndOnlyWhen in window.flags) ) then
               inc(endi);

             nextStep;
             popAll;
             if xqtfwSliding in window.flags then inc(start)
             else start := endi + 1;
          end else inc(start);
        end
      end else begin
        matchIdxListLength := 0;
        matchIdxList := nil;
        for start := 1 to length do
          if checkCondition(start, window.startCondition) then begin
            arrayAddFast(matchIdxList, matchIdxListLength, start);
            popAll;
          end;
        if matchIdxListLength > 0 then begin
          arrayAddFast(matchIdxList, matchIdxListLength, length + 1);
          for i := 0 to matchIdxListLength - 2 do begin
            start := matchIdxList[i];
            checkCondition(start, window.startCondition, true);
            endi := matchIdxList[i+1] - 1;
            nextStep;
          end;
        end;
      end;
    end;

    procedure addPatternVariableBinding(node: IXQValue);
    var
        patternClause: TXQTermFlowerLetPattern;
        function hasStaticVariable(const name, url: string): integer;
        var
          i: Integer;
        begin
          for i := 0 to high(patternClause.pattern.vars) do //todo: optimize stringhash
            if (patternClause.pattern.vars[i].value = name)
               and (patternClause.pattern.vars[i].namespace = url) then exit(i);
          result := -1;
        end;

    var oldContextItem: IXQValue;
        oldSeqIndex: Integer;
        oldSeqLength: Integer;
    var
        oldLog: TXQVariableChangeLog;
        i: Integer;
        log: TXQVariableChangeLog;
        hasContextItem: Boolean;
        varIndex, curcount, stackSize: integer;
        value, empty: IXQValue;
    begin
      patternClause := TXQTermFlowerLetPattern(children[pos]);
      log := patternMatcherMatch(patternClause.pattern.node, node.toNode, context, true);
      oldlog := log;
      try
        case clausekind of
          xqtfcLetPattern: log := log.condensedCollected;
          xqtfcForPattern: log := log.condensed;
        end;
        oldLog.Free;
        if (patternClause.sequenceTyp <> nil) then
          for i := 0 to log.count - 1 do
            if not (patternClause.sequenceTyp.instanceOf(log[i], context)) then
              raiseEvaluationError('XPTY0004', 'Invalid variable type, expected: type '+patternClause.sequenceTyp.serialize+' got value '+log[i].toXQuery());

        hasContextItem := patternClause.pattern.hasDefaultVariable;

        if hasContextItem then begin
          context.getContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
          context.setContextItem(xqvalue());
        end;
        stackSize := context.temporaryVariables.count;
        try
          if clausekind = xqtfcLetPattern then begin
            if hasContextItem and log.hasVariable('$', value) then begin
              if value.getSequenceCount <> 1 then raiseEvaluationError('pxp:PATTERN1', 'Only singletons can be assigned to .');
              context.setContextItem(value);
            end;
            for i := 0 to high(patternClause.pattern.vars) do
              if log.hasVariable(patternClause.pattern.vars[i], value) then begin
                context.temporaryVariables.push(patternClause.pattern.vars[i], value);
                if needFullTuple then currentTuple.vars[clauseIndex[pos]+i] := value;
              end else begin
                context.temporaryVariables.push(patternClause.pattern.vars[i], xqvalue());
                if needFullTuple then currentTuple.vars[clauseIndex[pos]+i] := xqvalue();
              end;
            evaluateForLoop(pos+1);
          end else begin // for
            curcount := context.temporaryVariables.count;
            empty := xqvalue();
            for i := 0 to high(patternClause.pattern.vars) do
              context.temporaryVariables.push(patternClause.pattern.vars[i], empty);
            if needFullTuple then
              for i := 0 to high(patternClause.pattern.vars) do
                currentTuple.vars[clauseIndex[pos]+i] := empty;

            for i := 0 to log.count - 1 do begin
              if log.getName(i) = '$' then begin
                if hasContextItem then begin
                  context.setContextItem(log[i]);
                  evaluateForLoop(pos+1);
                  context.setContextItem(xqvalue());
                end;
              end else begin
                varIndex := hasStaticVariable(log.getName(i), log.varstorage[i].namespaceURL);
                if varIndex > -1 then begin
                  context.temporaryVariables.fbuffer[curcount + varIndex] := log.get(i);
                  if needFullTuple then
                    currentTuple.vars[clauseIndex[pos]+varIndex] := log.get(i);
                  evaluateForLoop(pos+1);
                  context.temporaryVariables.fbuffer[curcount + varIndex] := empty;
                end;
              end;
            end;
          end;
        finally
          context.temporaryVariables.popTo(stackSize);
          if hasContextItem then context.setContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
        end;
      finally
        log.free;
      end;
    end;

   var i, stackSize:integer;
       clause: TXQTermFlowerSubClause;
       newTuple: TOrderedTuple;
       tempv: PIXQValue;
       tempSeq: IXQValue;
   begin
     if pos = high(children) then begin
       if currentTuple = nil then
         finalResult.add(children[pos].evaluate(context))
        else begin
         newTuple := TOrderedTuple.Create;
         newTuple.orders := currentTuple.orders;
         SetLength(newTuple.orders, length(newTuple.orders));
         newTuple.result := children[pos].evaluate(context);
         sortingCache.Add(newTuple);
        end;
        exit;
     end;
     clause := TXQTermFlowerSubClause(children[pos]);
     clausekind := clause.kind;
     case clausekind of
       xqtfcLet:
         addVariableBinding(TXQTermFlowerLet(clause).expr.evaluate(context));
       xqtfcFor: begin
         tempSeq := TXQTermFlowerFor(clause).expr.evaluate(context);
         case tempSeq.getSequenceCount of
           0: if TXQTermFlowerFor(clause).allowingEmpty then addVariableBinding(xqvalue(), 0);
           1: addVariableBinding(tempSeq, 1);
           else begin
             i := 1;
             for tempv in tempSeq.GetEnumeratorPtrUnsafe do begin
               addVariableBinding(tempv^, i);
               inc(i);
             end;
           end;
         end;
       end;
       xqtfcWindow: addWindowVariableBinding();
       xqtfcLetPattern: addPatternVariableBinding(TXQTermFlowerLetPattern(clause).expr.evaluate(context));
       xqtfcForPattern: begin
         tempSeq := TXQTermFlowerForPattern(clause).expr.evaluate(context);
         for tempv in tempSeq.GetEnumeratorPtrUnsafe do
           addPatternVariableBinding(tempv^);
       end;
       xqtfcWhere: if TXQTermFlowerWhere(clause).test.evaluate(context).toBooleanEffective then
         evaluateForLoop(pos + 1);
       xqtfcOrder: begin
         currentTuple.orders[clauseIndex[pos]] := TXQTermFlowerOrder(clause).expr.evaluate(context);
         evaluateForLoop(pos + 1);
       end;
       xqtfcCount: if needFullTuple then begin
         newTuple := TOrderedTuple.Create;
         newTuple.orders := currentTuple.orders;
         SetLength(newTuple.orders, length(newTuple.orders));
         newTuple.vars := currentTuple.vars;
         SetLength(newTuple.vars, length(newTuple.vars));
         sortingCache.Add(newTuple);
       end else begin
         counts[clauseIndex[pos]] += 1;
         stackSize := context.temporaryVariables.count;
         context.temporaryVariables.push(TXQTermFlowerCount(clause).countvar, xqvalue(counts[clauseIndex[pos]]));
         evaluateForLoop(pos + 1);
         context.temporaryVariables.popTo(stackSize);
       end;
       xqtfcGroup: begin
         newTuple := TOrderedTuple.Create;
         newTuple.orders := currentTuple.orders;
         SetLength(newTuple.orders, length(newTuple.orders));
         newTuple.vars := currentTuple.vars;
         SetLength(newTuple.vars, length(newTuple.vars));
         sortingCache.Add(newTuple);
       end;
     end;


   end;

  procedure groupTuple(pos: integer);
  var data: TFlowerGroupingData;
    group: TXQTermFlowerGroup;
    varindex: LongInt;
    i, j: Integer;
    tuple: TOrderedTuple;
    k: LongInt;
    oldSortingCache: TFPList;
    lastTuple: TOrderedTuple;
    //Grouping collects for all tuples that have the same value in the grouping vars all values of the nonGroupingVars
    //We put these values for each nonGroupingVars in a tempList
    tempLists: array of TXQVList;
    nonGroupingVars: array of integer;

    procedure closeGroupedTuple;
    var k: integer;
    begin
      for k in nonGroupingVars do
        case tempLists[k].Count of
          0: lastTuple.vars[k] := xqvalue();
          1: lastTuple.vars[k] := tempLists[k][0];
          else begin
            lastTuple.vars[k] := TXQValueSequence.create(tempLists[k]);
            tempLists[k] := nil;
          end;
        end;
    end;

    procedure newGroupedTuple;
    var k: integer;
    begin
      if lastTuple <> nil then closeGroupedTuple;
      sortingCache.Add(oldSortingCache[i]);
      lastTuple := TOrderedTuple(oldSortingCache[i]);
      for k in nonGroupingVars do begin
        if tempLists[k] = nil then tempLists[k] := TXQVList.create(1)
        else tempLists[k].clear;
        tempLists[k].add(lastTuple.vars[k]);
      end;
    end;

  begin
    if sortingCache.Count = 0 then exit;
    group := TXQTermFlowerGroup(children[pos]);
    data.context := @context;
    data.collation := group.collation;
    varindex := clauseIndex[pos];
    with group do begin
      SetLength(data.groupingIndices, length(vars));
      for i := 0 to high(vars) do
        for j := varindex - 1 downto 0 do
          if vars[i].equalsVariable(variables[j]) then begin
            data.groupingIndices[i] := j;
            break;
          end;
      SetLength(nonGroupingVars, varindex - length(vars));
      j := 0;
      for i := 0 to high(nonGroupingVars) do begin
        while arrayContains(data.groupingIndices, j) do inc(j);
        nonGroupingVars[i] := j;
        inc(j);
      end;

      for i := 0 to sortingCache.Count - 1 do begin
        tuple := TOrderedTuple(sortingCache[i]);
        for j := 0 to high(vars) do begin
          k := data.groupingIndices[j];
          if tuple.vars[k].getSequenceCount > 1 then raiseTypeError0004('singleton', tuple.vars[k]);
          tuple.vars[k] := xqvalueAtomize(tuple.vars[k]);
          if (group.seqtypes[j] <> nil) and not group.seqtypes[j].instanceOf(tuple.vars[k], context) then
            raiseTypeError0004(group.seqtypes[j].serialize, tuple.vars[k]);
        end;
      end;

      stableSort(ppointer(sortingCache.List^), ppointer(sortingCache.List^) + (sortingCache.Count-1) , sizeof(pointer), @flowerTupleGroupCompareFunction, TObject(@data));

      if length(nonGroupingVars) > 0 then
        SetLength(tempLists, nonGroupingVars[high(nonGroupingVars)] + 1);

      oldSortingCache := sortingCache;
      sortingCache := TFPList.Create;
      i := 0;
      lastTuple := nil;
      newGroupedTuple;
      i := 1;
      while i < oldSortingCache.Count do begin
        if data.compareTuples(lastTuple, TOrderedTuple(oldSortingCache[i])) = 0 then begin
          for k in nonGroupingVars do
            {%H-}tempLists[k].add(TOrderedTuple(oldSortingCache[i]).vars[k]);
          tobject(oldSortingCache[i]).free;
        end else newGroupedTuple;
        inc(i);
      end;
      closeGroupedTuple;
      for k in nonGroupingVars do tempLists[k].free;
      oldSortingCache.free;
    end;

  end;

var i: Integer;
    sortingData: TFlowerSortingData;
    j: Integer;
    oldSortingCache: TFPList;
    isJoinClause: Boolean;
    joinVar: LongInt;
    k, stackSize: Integer;


begin
  //see precompute
  finalResult := TXQValueSequence.create();
  result := finalResult;
  oldSortingCache := nil;
  currentTuple := nil;

  if needFullTuple or (orderCount > 0) then begin
    sortingData.orders := orders;
    sortingData.context:=@context;
    sortingData.minOrderIndex:=0;
    sortingData.maxOrderIndex:=-1;
    setlength(sortingData.collations, length(orders));
    for i := 0 to high(orders) do sortingData.collations[i] := orders[i].collation;

    sortingCache := tfplist.Create;
    currentTuple := TOrderedTuple.Create;
    SetLength(currentTuple.orders, orderCount);
  end else sortingCache := nil;
  if needFullTuple then begin
    if currentTuple = nil then currentTuple := TOrderedTuple.Create;
    SetLength(currentTuple.vars, varCount);
  end else SetLength(counts, countCount);

  try
    evaluateForLoop(0);
    if needFullTuple then
    for i := 0 to high(children) - 1 do begin
      isJoinClause := TXQTermFlowerSubClause(children[i]).kind in [xqtfcCount, xqtfcGroup];
      if not isJoinClause then begin
        case TXQTermFlowerSubClause(children[i]).kind of
          xqtfcOrder: sortingData.maxOrderIndex := clauseIndex[i];
        end;
      end else begin
        if (sortingData.maxOrderIndex >= sortingData.minOrderIndex) and (sortingCache.Count > 0) then begin
          stableSort(ppointer(sortingCache.List^), ppointer(sortingCache.List^) + (sortingCache.Count-1) , sizeof(pointer), @flowerTupleCompareFunction, TObject(@sortingData));
          sortingData.minOrderIndex := sortingData.maxOrderIndex + 1;
        end;
        joinVar := clauseIndex[i];
        case TXQTermFlowerSubClause(children[i]).kind of
          xqtfcCount: begin
            for j := 0 to sortingCache.Count-1 do
              TOrderedTuple(sortingCache[j]).vars[joinVar] := xqvalue(j+1);
          end;
          xqtfcGroup: begin
            joinVar -= 1;
            //joinVar += high(TXQTermFlowerGroup(children[i]).specs);
            groupTuple(i);
          end;
        end;
        oldSortingCache := sortingCache;
        sortingCache := TFPList.Create;
        for j := 0 to oldSortingCache.Count-1 do begin
          stackSize := context.temporaryVariables.Count;
          currentTuple.free;
          currentTuple := TOrderedTuple(oldSortingCache[j]);
          for k := 0 to joinVar do
            context.temporaryVariables.push(variables[k], currentTuple.vars[k]);
          evaluateForLoop(i+1);
          context.temporaryVariables.popTo(stackSize);
        end;
        oldSortingCache.free;
      end
    end;
    if sortingCache <> nil then begin
      sortingData.maxOrderIndex := orderCount - 1;
      if (sortingData.maxOrderIndex >= sortingData.minOrderIndex) and (sortingCache.Count > 0) then
        stableSort(ppointer(sortingCache.List^), ppointer(sortingCache.List^) + (sortingCache.Count-1) , sizeof(pointer), @flowerTupleCompareFunction, TObject(@sortingData));

      for i := 0 to sortingCache.Count - 1 do begin
        finalResult.add(TOrderedTuple(sortingCache[i]).result);
        TOrderedTuple(sortingCache[i]).Free;
      end;
    end;
    xqvalueSeqSqueeze(result);
  finally
    FreeAndNil(sortingCache);
    FreeAndNil(currentTuple);
  end;
end;

procedure TXQTermFlower.precompute;

var
  i, j: Integer;

  procedure initialAddVariable(v: TXQTermVariable);
  begin
    variables[varCount] := v;
    inc(varCount);
  end;
  procedure initialAddWindowVariables(w: TXQTermFlowerWindow);
    procedure addCondition(const condition: TXQTermFlowerWindowVarsAndCondition);
    begin
      with condition do begin
        if not Assigned(when) then exit;
        if Assigned(currentItem) then initialAddVariable(currentItem);
        if Assigned(positionVar) then initialAddVariable(positionVar);
        if Assigned(previousItem) then initialAddVariable(previousItem);
        if Assigned(nextItem) then initialAddVariable(nextItem);
      end;
    end;
  begin
    addCondition(w.startCondition);
    addCondition(w.endCondition);
    initialAddVariable(w.loopvar);
  end;

begin
  orderCount := 0;
  varCount := 0;
  countCount := 0;
  needFullTuple := false;
  orders := nil;
  variables := nil;
  clauseIndex := nil;


  { There are three different strategies used to evaluate a Flower expressions, depending on its complexity.
    In any case every sub clause is evaluated and the variable values of the previous clauses are kept on a stack.
    Then it does one of these:

    1. Slightly optimized version: Just add the returned value to finalResult
    2. With ordering:    Put the value of every order clause in a TOrderedTuple together with the returned value,
       (orderCount>0)    and add this tuple to sortingCache. In the end sortingCache is sorted and the items moved to finalResult.
    3. Full tuple stream: Put the value of every order clause and every variable assignment in a TOrderedTuple and add it to sortingCache.
       (needFullTuple)    Interrupt the stack based evaluation at certain clauses and sort the complete sortingCache.
                          Then resume the evaluation for each tuple by copying the variables from the tuple back on the stack.
  }
  for i := 0 to high(children) - 1 do
    case TXQTermFlowerSubClause(children[i]).kind of
      xqtfcLet: inc(varCount);
      xqtfcFor: begin
        inc(varCount);
        if assigned(TXQTermFlowerFor(children[i]).positionVar) then inc(varCount);
      end;
      xqtfcWindow: inc(varCount, TXQTermFlowerWindow(children[i]).variableCount);
      xqtfcForPattern, xqtfcLetPattern: with TXQTermFlowerLetPattern(children[i]).pattern do begin
        inc(varCount, length(vars));
        if hasDefaultVariable then inc(varCount);
      end;
      xqtfcWhere: ;
      xqtfcOrder: inc(orderCount);
      xqtfcCount: begin
        if orderCount > 0 then needFullTuple := true;
        inc(varCount);
        inc(countCount);
      end;
      xqtfcGroup: needFullTuple := true;
    end;

  if needFullTuple or (orderCount > 0) or (countCount > 0) then SetLength(clauseIndex, length(children) - 1);
  if needFullTuple or (orderCount > 0) then begin
    SetLength(orders, orderCount);
    orderCount := 0;
    for i := 0 to high(children) - 1 do
      case TXQTermFlowerSubClause(children[i]).kind of
        xqtfcOrder: begin
          orders[orderCount] := TXQTermFlowerOrder(children[i]);
          clauseIndex[i] := orderCount;
          inc(orderCount);
        end;
      end;
  end;
  if needFullTuple then begin
    SetLength(variables, varCount);
    varCount := 0;
    for i := 0 to high(children) - 1 do
      case TXQTermFlowerSubClause(children[i]).kind of
        xqtfcLet: begin
          clauseIndex[i] := varCount;
          initialAddVariable(TXQTermFlowerLet(children[i]).loopvar);
        end;
        xqtfcFor: begin
          clauseIndex[i] := varCount;
          initialAddVariable(TXQTermFlowerLet(children[i]).loopvar);
          if assigned(TXQTermFlowerFor(children[i]).positionVar) then
            initialAddVariable(TXQTermFlowerFor(children[i]).positionVar);
        end;
        xqtfcWindow: initialAddWindowVariables(TXQTermFlowerWindow(children[i]));
        xqtfcForPattern, xqtfcLetPattern: with TXQTermFlowerLetPattern(children[i]).pattern do begin
          clauseIndex[i] := varCount;
          if hasDefaultVariable then raiseEvaluationError('XQTS0003', 'Too complex flowr expression for pattern changing .');
          for j := 0 to high(vars) do
            variables[varCount + j] := vars[j];
          inc(varCount, length(vars));
        end;
        //xqtfcWhere: ;
        //xqtfcOrder: inc(orderCount);
        xqtfcCount: begin
          clauseIndex[i] := varCount;
          initialAddVariable(TXQTermFlowerCount(children[i]).countvar);
        end;
        xqtfcGroup: clauseIndex[i] := varCount;
      end;
  end else if countCount > 0 then begin
    countCount := 0;
    for i := 0 to high(children) - 1 do
      case TXQTermFlowerSubClause(children[i]).kind of
        xqtfcCount: begin
          clauseIndex[i] := countCount;
          inc(countCount);
        end;
      end;
  end;
end;

function TXQTermFlower.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;

function TXQTermFlower.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
begin
  for i := 0 to high(children) - 1 do
    visitor.simpleTermVisit(@children[i], self);

  result := visitor.simpleTermVisit(@children[high(children)],self);
  case result of xqtvaAbort: exit; end;

  visitor.parent := self;
  for i := high(children) - 1 downto 0 do
    TXQTermFlowerSubClause(children[i]).visitchildrenToUndeclare(visitor);
end;

function TXQTermFlower.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermFlower(result).precompute;
end;

destructor TXQTermFlower.destroy;
begin
  inherited destroy;
end;

constructor TXQTermNamedFunction.Create;
begin

end;

{ TXQTermNamedFunction }
                         {
constructor TXQTermNamedFunction.create(const akind: TXQTermNamedFunctionKind; const afunc: TXQAbstractFunctionInfo);
begin
  kind := akind;
  func := afunc;
end;
  }
constructor TXQTermNamedFunction.create(const anamespace, alocalname: string; arity: integer; const staticContext: TXQStaticContext);
begin
  if not findKindIndex(anamespace, alocalname, arity, staticContext, kind, func) then begin
    kind := xqfkUnknown;
    name := TXQEQName.Create.create(anamespace, alocalname);
  end;
end;

constructor TXQTermNamedFunction.create(const anamespace, alocalname: string; args: array of TXQTerm; const staticContext: TXQStaticContext);
begin
  create(anamespace, alocalname, length(args), staticContext);
  push(args);
end;

destructor TXQTermNamedFunction.destroy;
begin
  name.free;
  inherited destroy;
end;

function TXQTermNamedFunction.evaluate(var context: TXQEvaluationContext): IXQValue;
//{$define checkreturntypes}  there is no point in testing the return type for native functions. but it might spot internal inconsistencies
var
  evilkids: PIXQValue;
  argcount, i, stacksize: Integer;
  stack: TXQEvaluationStack;
begin
  //evaluateChildren(context, evilkids);
  stack := context.temporaryVariables;
  stacksize := stack.Count;
  argcount := length(children);
  assert((kind = xqfkUnknown) or (func <> nil));

  for i:=0 to high(children) do
    stack.push(children[i].evaluate(context));

  evilkids := stack.topptr(high(children)); //can only get this pointer here as children evaluate might change the stack base


  if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, argcount, evilkids);
  if version <> nil then
    version^.checkOrConvertTypes(@evilkids[0], argcount, context, self);

  case kind of
    xqfkBasic:
      result := TXQBasicFunctionInfo(func).func(argcount, evilkids);
    xqfkComplex:
      result := TXQComplexFunctionInfo(func).func(context, argcount, evilkids);
    xqfkNativeInterpreted: begin
      assert(TXQInterpretedFunctionInfo(func).func <> nil);
      TXQInterpretedFunctionInfo(func).func.contextOverrideParameterNames(context, argcount);
      result := TXQInterpretedFunctionInfo(func).func.evaluateInContext(context, self);
    end;
    xqfkUnknown: begin
      init(context.staticContext);
      interpretedFunction.contextOverrideParameterNames(context, argcount);
      result := interpretedFunction.evaluate(context, self);
    end;
    else begin assert(false); result := nil; end;
  end;

  {$ifdef checkreturntypes}
  if version <> nil then
    if not version.returnType.instanceOf(result, context) then
      raiseEvaluationError('XPTY0004', 'Invalid return value, '+result.toXQuery()+' does not have type '+func.versions[version].returnType.serialize);
  {$endif}
  if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, -1, nil);
  stack.popTo(stacksize);
end;


function TXQTermNamedFunction.getContextDependencies: TXQContextDependencies;

begin
  case kind of
    xqfkBasic:             result := getChildrenContextDependencies + []; //all basic functions are pure
    xqfkComplex:           result := getChildrenContextDependencies + TXQComplexFunctionInfo(func).contextDependencies;
    xqfkNativeInterpreted: result := getChildrenContextDependencies + TXQInterpretedFunctionInfo(func).contextDependencies;
    else                   Result := ALL_CONTEXT_DEPENDENCIES;
  end;
end;

procedure TXQTermNamedFunction.assignWithoutChildren(source: TXQTermNamedFunction);
begin
  name := source.name;
  if name <> nil then name := name.clone;
  kind:=source.kind;
  func:=source.func;
  version := source.version;

  interpretedFunction := source.interpretedFunction;
  functionStaticContext := source.functionStaticContext;
end;

function TXQTermNamedFunction.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermNamedFunction(result).assignWithoutChildren(self);
end;

function TXQTermNamedFunction.ToString: ansistring;
var
  i: Integer;
begin
  result := '';
  if name <> nil then
    result += name.ToString;
  result += '(';
  for i := 0 to high(children) do begin
    if i <> 0 then result += ', ';
    result += children[i].ToString;
  end;
  result += ')';
end;

class function TXQTermNamedFunction.findKindIndex(const anamespace, alocalname: string; const argcount: integer; const staticContext: TXQStaticContext; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
var
  aindex: Integer;
  module: TXQNativeModule;
  t: TXSType;
  model: TXQParsingModel;
  schema: TXSSchema;
begin
  module := TXQueryEngine.findNativeModule(anamespace);

  if staticContext <> nil then model := staticContext.model
  else model := xqpmXQuery3;

  if (module <> nil) then begin
    afunc := module.findBasicFunction(alocalname, argcount, model);
    if afunc <> nil then begin
      akind:=xqfkBasic;
      exit(true);
    end;

    afunc := module.findComplexFunction(alocalname, argcount, model);
    if afunc <> nil then begin
      akind:=xqfkComplex;
      exit(true);
    end;

    afunc := module.findInterpretedFunction(alocalname, argcount, model);
    if afunc <> nil then begin
      akind:=xqfkNativeInterpreted;
      exit(true);
    end;

    if argcount = 2 then begin
      aindex := module.binaryOpFunctions.IndexOf(alocalname);
      if aindex >= 0 then begin
        afunc := TXQOperatorInfo(module.binaryOpFunctions.Objects[aindex]);
        exit(true);
      end;
    end;
  end;

  if argcount = 1 then begin
    if anamespace = baseSchema.url then schema := baseSchema
    else if staticContext <> nil then schema := staticContext.findSchema(anamespace)
    else schema := nil;
    if schema <> nil then begin
      t := schema.findType(alocalname);
      if (t <> nil) and not (baseSchema.isAbstractType(t)) and not (baseSchema.isValidationOnlyType(t)) then begin
        akind:=xqfkTypeConstructor;
        afunc := TXQAbstractFunctionInfo(TObject(t));
        exit(true)
      end;
    end;
  end;

  exit(false);
end;

function TXQTermNamedFunction.convertToTypeConstructor: TXQTermNamedFunction;
begin
  result := TXQTermNamedFunctionTypeConstructor.Create;
  result.kind := xqfkTypeConstructor;
  result.func := func;
  result.children := children;
  result.name := name;
  children := nil;
  name := nil;
  free;
end;

procedure TXQTermNamedFunction.init(const context: TXQStaticContext);
var
  i: Integer;
  f: TXQValueFunction;
begin
  if not (kind in [xqfkNativeInterpreted, xqfkUnknown]) or (interpretedFunction <> nil) then exit;
  case kind of
    xqfkUnknown: begin
      functionStaticContext := context.findModuleStaticContext(name.namespaceURL);
      if functionStaticContext <> nil then
        for i := 0 to high(functionStaticContext.functions) do begin
          f :=  functionStaticContext.functions[i];
          if (f.name = name.localname)
             and (length(f.parameters) = length(children))
             and equalNamespaces(f.namespaceURL, name.namespaceURL) then begin
            interpretedFunction := f;
            break;
          end;
        end;
      if interpretedFunction <> nil then begin
        if functionStaticContext <> context {not equalNamespaces(vfunc.namespace, context.staticContext.moduleNamespace)} then
          for i := 0 to high(interpretedFunction.annotations) do
            if interpretedFunction.annotations[i].name.isEqual(XMLNamespaceUrl_XQuery, 'private') then
              raiseParsingError('XPST0017', interpretedFunction.name + ' is private');
      end else begin
        findKindIndex(name.namespaceURL, name.localname, length(children), context, kind, func);
        if kind <> xqfkUnknown then exit();
        raiseEvaluationError('XPST0017', 'Function '+name.ToString+'#'+IntToStr(length(children))+ ' not found. ');
      end;
    end;
    xqfkNativeInterpreted: begin
      if TXQInterpretedFunctionInfo(func).func = nil then
        TXQInterpretedFunctionInfo(func).initialize();
      interpretedFunction :=  TXQInterpretedFunctionInfo(func).func;
      functionStaticContext := context;
    end;
  end;
end;


function TXQTermNamedFunctionTypeConstructor.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  t: TXSType;
begin
  result := children[0].evaluate(context);
  case result.getSequenceCount of
    0: ;
    1: begin
      t := TXSType(TObject(func));
      if t.storage <> TXQValueQName then result := TXSType(TObject(func)).createValue(result)
      else result := (t as TXSQNameType).cast(result, context);
    end
    else raiseEvaluationError('XPTY0004', 'Need singleton value to cast as '+TXSType(TObject(func)).name);
  end;
end;

function TXQTermNamedFunctionTypeConstructor.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;



{ TXQDynamicFunctionCall }

constructor TXQTermDynamicFunctionCall.create(func: TXQTerm; arg: TXQTerm);
begin
  if func <> nil then begin
    push(func);
    if arg <> nil then push(arg);
  end;
end;

function TXQTermDynamicFunctionCall.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  func: IXQValue;
  index: IXQValue;
  ara: TXQValueJSONArray;
  i, stackSize: Integer;
  sl: TStringList;
  resultSeq: TXQVList;
  v: PIXQValue;
  funcref: TXQValueFunction;
begin
  //if length(children) < 2 then raiseEvaluationError('XPST0003', 'Only one argument given, need function AND arguments');
  func := children[0].evaluate(context);
  if func.isUndefined then
    if AllowJSONDefaultInternal then exit(xqvalue())
    else raise EXQEvaluationException.create('XPTY0004', 'Cannot use an empty sequence as function, if JSONiq is disabled');
  if (func.kind = pvkSequence) and (func.getSequenceCount = 1) then func := func.get(1);

  if func is TXQValueFunction then begin
    stackSize := context.temporaryVariables.Count;
    for i:=1 to high(children) do
      context.temporaryVariables.push(children[i].evaluate(context));
    funcref := func as TXQValueFunction;
    if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, length(children)-1, context.temporaryVariables.topptr(high(children)-1));
    funcref.contextOverrideParameterNames(context, high(children));
    result := funcref.evaluate(context, self);
    if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, -1, nil);
    context.temporaryVariables.popTo(stackSize);
    exit;
  end;

  if length(children) = 1 then begin
    resultSeq := TXQVList.create();
    for v in func.GetEnumeratorPtrUnsafe do begin
      case v^.kind of
        pvkObject: begin
          sl := TStringList.Create;
          sl.CaseSensitive := true;
          (v^ as TXQValueObject).enumerateKeys(sl);
          resultSeq.add(xqvalue(sl));
          sl.free;
        end;
        pvkArray: begin
          ara := v^ as TXQValueJSONArray;;
          for i := 0 to ara.seq.Count-1 do
            resultSeq.add(ara.seq[i]);
        end;
        else if not AllowJSONDefaultInternal then raise EXQEvaluationException.create('XPTY0004', 'Invalid type for function call: '+v^.toXQuery());
      end;
    end;
    xqvalueSeqSqueezed(result, resultSeq);
    exit;
  end;

  if Length(children) <> 2 then raiseEvaluationError('JNTY0018', 'Multiple array indices or object prop given');
  index := children[1].evaluate(context);
  resultSeq := TXQVList.create();
  for v in func.GetEnumeratorPtrUnsafe do
    case v^.kind of
      pvkArray: begin
        if (index.toInt64 < 1) or (index.toInt64 > (v^ as TXQValueJSONArray).seq.Count) then continue;
        resultSeq.add((v^ as TXQValueJSONArray).seq[index.toInt64-1]);
      end;
      pvkObject: begin
        resultSeq.add(v^.getProperty(index.ToString));
      end;
      else if not AllowJSONDefaultInternal then //not an error since JSONiq 1.0.1
        raiseEvaluationError('XPTY0004', 'Invalid call, expected function, object or array, got: '+func.toXQuery());
    end;
  xqvalueSeqSqueezed(result, resultSeq)
end;

function TXQTermDynamicFunctionCall.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  tempvar: TXQTermVariable;
  i: Integer;
begin
  result := visitor.simpleTermVisit(@children[0], self);
  case result of xqtvaAbort: exit; end;
  tempvar := globalUnnamedVariable;
  for i := 1 to high(children) do begin
    result := visitor.simpleTermVisit(@children[i], self);
    case result of xqtvaAbort: exit; end;
    visitor.declare(@tempvar);
  end;
  for i := high(children) downto 1 do
    visitor.undeclare(@tempvar);
end;

function TXQTermDynamicFunctionCall.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;




{ TXQTermBinaryOp }

constructor TXQTermBinaryOp.create(const aop: string; arg1: TXQTerm; arg2: TXQTerm);
begin
  op := TXQueryEngine.findOperator(pchar(aop));
  if op = nil then raiseParsingError('XPST0003', 'No operator: ' + aop);
  if arg1 <> nil then begin
    push(arg1);
    if arg2 <> nil then push(arg2);
  end;
end;

constructor TXQTermBinaryOp.create(arg1: TXQTerm; const aop: string; arg2: TXQTerm);
begin
  create(aop, arg1, arg2);
end;

constructor TXQTermBinaryOp.create(opinfo: TXQOperatorInfo);
begin
  op := opinfo;
end;

constructor TXQTermBinaryOp.createUnary(const aop: string; arg: TXQTerm);
begin
  create('unary~hack' + aop, TXQTermConstant.create(xqvalue()), arg);
end;

function TXQTermBinaryOp.evaluate(var context: TXQEvaluationContext): IXQValue;

var
  i, oldsize: Integer;
  stack: TXQEvaluationStack;
  evilkids: PIXQValue;
begin
  stack := context.temporaryVariables;
  oldsize := stack.Count;
  stack.push(children[0].evaluate(context));
  stack.push(children[1].evaluate(context));
  evilkids := stack.topptr(1);
  if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, 2, @evilkids[0]);
  if context.staticContext.strictTypeChecking  then begin
    if xqofCastUntypedToString in op.flags then begin
      for i := 0 to 1 do
        case evilkids[i].kind of
          pvkNode: evilkids[i] := xqvalue(evilkids[i].toString);
          pvkString: if evilkids[i].instanceOf(baseSchema.untypedAtomic) then evilkids[i] := xqvalue(evilkids[i].toString);
        end;
    end else if xqofCastUntypedToDouble in op.flags then
      for i := 0 to 1 do
        case evilkids[i].kind of
          pvkString, pvkNode: evilkids[i] := xqvalue(evilkids[i].toFloatChecked(context.staticContext) );
        end;
    op.checkOrConvertTypes(@evilkids[0], 2, context, self);
  end;
  result := op.func(context, evilkids[0], evilkids[1]);
  if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, -1, nil);
  stack.popTo(oldsize);
end;

function TXQTermBinaryOp.debugTermToString: string;
var
  i: Integer;
begin
  result := '(' + ClassName + ' ';
  if length(children) >= 1 then result += children[0].debugTermToString();
  result += ' ' + op.name;
  for i:= 1 to high(children) do result += ' ' + children[1].debugTermToString();
  result += ')';
end;

function TXQTermBinaryOp.getContextDependencies: TXQContextDependencies;
begin
    result := op.contextDependencies + getChildrenContextDependencies;
end;

function TXQTermBinaryOp.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermBinaryOp(result).op := op;
end;




function TXQTermSimpleMap.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  seq, oldContextItem: IXQValue;
  list: TXQVList;
  pe: TTreeNode;
  oldSeqIndex, oldSeqLength: integer;
  pv: PIXQValue;
begin
  seq := children[0].evaluate(context);
  pe := context.ParentElement; //todo: this is irrelevant?
  context.getContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
  context.ParentElement := nil;
  context.SeqIndex := 1;
  context.SeqLength := seq.getSequenceCount;
  list := TXQVList.create(context.SeqLength);
  context.SeqValue := nil;
  try
    try
      for pv in seq.GetEnumeratorPtrUnsafe do begin
        xqvalueMoveNoRefCount(pv^, context.SeqValue);
        list.add(children[1].evaluate(context));
        context.SeqIndex += 1;
      end;
      xqvalueSeqSqueezed(result, list)
    except
      list.free;
      raise;
    end;
  finally
    xqvalueVaporize(context.SeqValue);
    context.setContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
    context.ParentElement := pe;
  end;
end;


function TXQTermSimpleMap.debugTermToString: string;
begin
  Result:='(' + children[0].debugTermToString() + ' ! ' + children[1].debugTermToString + ')';
end;

function TXQTermSimpleMap.getContextDependencies: TXQContextDependencies;
begin
  Result:=children[0].getContextDependencies +
          ( children[1].getContextDependencies - [xqcdFocusItem,  xqcdFocusPosition, xqcdFocusLast] );
end;



constructor TXQTermPath.create(b: TXQTerm);
begin
  addToPath(b);
end;

destructor TXQTermPath.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(path) do path[i].destroy;
  inherited Destroy;
end;

function TXQTermPath.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  i: Integer;
begin
  result := TXQueryEngine.evaluateSingleStepQuery(path[0], context, high(path) = 0);
  for i:=1 to high(path) do
    result := TXQueryEngine.expandSequence(result, path[i], context, high(path) = i);

  xqvalueSeqSqueeze(result);
end;

function TXQTermPath.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i, j: Integer;
begin
  result := xqtvaContinue;
  for i := 0 to high(path) do begin
    if path[i].typ = qcFunctionSpecialCase then visitor.simpleTermVisit(@path[i].specialCase, self);
    if path[i].requiredType <> nil then visitor.simpleTermVisit(@path[i].requiredType, self);
    for j := 0 to high(path[i].filters) do begin
      result := visitor.simpleTermVisit(@path[i].filters[j], self);
      case result of xqtvaAbort: exit; end;
    end;
  end;
end;

function TXQTermPath.debugTermToString: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to high(path) do begin
    if result <> '' then result += '/';
    result += path[i].serialize;
  end;
end;

function TXQTermPath.getContextDependencies: TXQContextDependencies;
var
  i, j: Integer;
begin
  if length(path) = 0 then exit([]);
  if path[0].typ = qcFunctionSpecialCase then result := path[0].specialCase.getContextDependencies
  else result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusPosition, xqcdFocusLast, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
  for i := 0 to high(path) do begin
    if path[i].typ = qcFunctionSpecialCase then result += path[i].specialCase.getContextDependencies - [xqcdFocusItem,  xqcdFocusPosition, xqcdFocusLast];
    for j := 0 to high(path[i].filters) do
      result += path[i].filters[j].dependencies - [xqcdFocusItem,  xqcdFocusPosition, xqcdFocusLast];
  end;
end;

procedure TXQTermPath.addToPath(term: TXQTerm);
var
  last, len: Integer;
  b: TXQTermBinaryOp;
  isDoubleSlash: Boolean;
  filter: TXQTermFilterSequence;
  other: TXQTermPath;
begin
  if term is TXQTermNodeMatcher then begin
    SetLength(path, length(path) + 1);
    move(TXQTermNodeMatcher(term).queryCommand, path[high(path)], sizeof(path[high(path)]));
    FillChar(TXQTermNodeMatcher(term).queryCommand, sizeof(TXQTermNodeMatcher(term).queryCommand), 0);
    term.free;
  end else if term is TXQTermBinaryOp then begin
    b := TXQTermBinaryOp(term);
    isDoubleSlash := b.op.name = '//';
    addToPath(b.children[0]);
    if isDoubleSlash then begin
      setlength(path, length(path) + 1);
      path[high(path)].typ:=qcSameOrDescendant;
      path[high(path)].matching:=[qmDocument, qmElement, qmText, qmComment, qmProcessingInstruction];
    end;
    addToPath(b.children[1]);

    //optimization
    //two descendants following each other can be condensed to a single descendant (but this changes the indices of the sequence, so it only work if there are no numeric filters)
    last := high(path);
    if (path[last - 1].typ = qcSameOrDescendant) and (path[last - 1].matching = [qmDocument, qmElement, qmText, qmComment, qmProcessingInstruction]) and (length(path[last - 1].filters) = 0)
        and (path[last].typ in [qcDirectChild, qcDirectChildImplicit, qcDescendant]) and (length(path[last].filters) = 0) then begin
      path[last - 1] := path[last];
      path[last - 1].typ := qcDescendant;
      SetLength(path, last);
    end;
    b.children := nil;
    b.free;
  end else if term is TXQTermFilterSequence then begin
    filter := TXQTermFilterSequence(term);
    addToPath(filter.children[0]);
    if length(filter.children) > 1 then begin//fail later with a better message than range check error
      setlength(path[high(path)].filters, length(path[high(path)].filters) + 1);
      path[high(path)].filters[high(path[high(path)].filters)].filter := filter.children[1];
      path[high(path)].filters[high(path[high(path)].filters)].dependencies := filter.children[1].getContextDependencies;
    end;
    filter.children := nil;
    filter.free;
  end else if term is TXQTermPath then begin
    other := TXQTermPath(term);
    len := length(path);
    setlength(path, length(path) + length(other.path));
    move(other.path[0], path[len], sizeof(other.path[0]) * length(other.path));
    fillchar(other.path[0], sizeof(other.path[0]) * length(other.path), 0);
    other.path := nil;
    other.free;
  end else begin
    SetLength(path, length(path) + 1);
    path[high(path)].typ := qcFunctionSpecialCase;
    path[high(path)].specialCase:=term;
  end;
end;

function TXQTermPath.clone: TXQTerm;
var
  i: Integer;
begin
  Result := inherited;
  SetLength(TXQTermPath(Result).path, length(path));
  for i := 0 to high(path) do
    TXQTermPath(Result).path[i] := path[i].clone;
end;

constructor TXQTermNodeMatcher.Create;
begin

end;

constructor TXQTermNodeMatcher.Create(direct: TXQTermNodeMatcherDirect);
begin
  case direct of
    xqnmdRoot: queryCommand.typ:=qcDocumentRoot;
    xqnmdParent: begin
      queryCommand.typ:=qcDirectParent; //parent /../
      queryCommand.matching:=[qmDocument, qmElement];
    end;
  end;
end;

constructor TXQTermNodeMatcher.Create(const aaxis: string; const avalue: string);
var
  namespaceURLOrPrefix: String;
  namespaceCheck: TXQNamespaceMode;
begin
  queryCommand.value := avalue;
  if strContains(queryCommand.value,'::') then
    raisePXPInternalError;
  if strContains(queryCommand.value,':') then begin
    namespaceURLOrPrefix:=strSplitGet(':',queryCommand.value);
    if namespaceURLOrPrefix = '*' then namespaceCheck := xqnmNone
    else namespaceCheck := xqnmPrefix;
  end else begin
    namespaceURLOrPrefix := '';
    if queryCommand.value <> '*' then namespaceCheck := xqnmPrefix
    else namespaceCheck := xqnmNone;
  end;
  if namespaceCheck <> xqnmNone then setNamespace(namespaceCheck, namespaceURLOrPrefix);

  //result.requiredType := nil;
  if queryCommand.value = '*' then begin
    queryCommand.matching:= queryCommand.matching + [qmElement,qmAttribute];
    queryCommand.value := '';
  end else begin
    queryCommand.matching := queryCommand.matching + [qmValue,qmElement];
    queryCommand.valueHash := nodeNameHash(queryCommand.value);
  end;
  setAxis(aaxis);
end;

destructor TXQTermNodeMatcher.Destroy;
begin
  queryCommand.destroy;
  inherited Destroy;
end;

function TXQTermNodeMatcher.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  if (queryCommand.matching = [qmText]) and (queryCommand.typ = qcDirectChildImplicit) and (context.SeqValue = nil) and (context.TextNode<>nil) then
    result := xqvalue(context.TextNode)
  else begin
    result := TXQueryEngine.evaluateSingleStepQuery(queryCommand,context, true);
    xqvalueSeqSqueeze(result);
  end
end;


function TXQTermNodeMatcher.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusPosition, xqcdFocusLast, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
end;

function TXQTermNodeMatcher.debugTermToString: string;
begin
  result := queryCommand.serialize;
end;

function TXQTermNodeMatcher.clone: TXQTerm;
var
  other: TXQTermNodeMatcher;
begin
  Result:=inherited clone;
  other := TXQTermNodeMatcher(result);
  other.queryCommand := queryCommand.clone;
end;

procedure TXQTermNodeMatcher.setNamespace(namespaceCheck: TXQNamespaceMode; namespaceURLOrPrefix: string);
begin
  queryCommand.matching := queryCommand.matching - [qmCheckNamespacePrefix, qmCheckNamespaceURL];
  if namespaceCheck <> xqnmNone then begin
    queryCommand.namespaceURLOrPrefix:=namespaceURLOrPrefix;
    case namespaceCheck of
      xqnmPrefix: Include(queryCommand.matching, qmCheckNamespacePrefix);
      xqnmURL: Include(queryCommand.matching, qmCheckNamespaceURL);
    end;
  end
end;

procedure TXQTermNodeMatcher.setAxis(const axis: string);
  function getAxis(const axis: string): TXQPathMatchingAxis;
  begin
     //forward
     case axis of
     '': result := qcDirectChildImplicit;
     'child': result :=  qcDirectChild;
     'descendant': result := qcDescendant;
     'self': result := qcSameNode;
     'parent': result := qcDirectParent;

     'descendant-or-self': result := qcSameOrDescendant;
     'following-sibling': result := qcFollowingSibling;
     'following': result := qcFollowing;

     //reverse
     'ancestor': result := qcAncestor;
     'preceding-sibling': result := qcPrecedingSibling;
     'preceding': result := qcPreceding;
     'ancestor-or-self': result := qcSameOrAncestor;

     'attribute': result := qcAttribute;
     else raise EXQEvaluationException.Create('XPST0003', 'Unknown axis: '+axis);
     end;
  end;
begin
  queryCommand.typ := getAxis(axis);
end;

{ TXQTermNumber }

constructor TXQTermConstant.createNumber(const avalue: string);
begin
  if strContains(avalue,'e') or strContains(avalue,'E') then value := baseSchema.double.createValue(avalue)
  else if strContains(avalue,'.') then value := baseSchema.decimal.createValue(avalue)
  else value := baseSchema.integer.createValue(avalue);
end;

constructor TXQTermConstant.create(const avalue: string);
begin
  value := xqvalue(avalue)
end;

constructor TXQTermConstant.create(const avalue: IXQValue);
begin
  value := avalue;
end;

function TXQTermConstant.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := value;
end;

function TXQTermConstant.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
end;

function TXQTermConstant.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermConstant(result).value := value; //do not clone since most values are treated as immutable
end;

{ TXQTermTemporaryNode }

function TXQTermPatternMatcher.clone: TXQTerm;
begin
  Result:=inherited clone;
  with TXQTermPatternMatcher(result) do begin
    node := self.node.clone;
    vars := self.vars;
    setlength(vars, length(vars));
    hasDefaultVariable := self.hasDefaultVariable;
    contextDependancies:=self.contextDependancies;
  end;
end;

function TXQTermPatternMatcher.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  Result:=patternMatcherVisit(self, visitor);
end;

destructor TXQTermPatternMatcher.destroy;
begin
  node.deleteAll();
  inherited destroy;
end;

function TXQTermPatternMatcher.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  raiseEvaluationError('pxp:INTERNAL', 'TXQTermPatternMatcher.evaluate');
  result := nil;
end;

function TXQTermPatternMatcher.getContextDependencies: TXQContextDependencies;
begin
  result := contextDependancies;
end;

{ TXQTermVariable }

constructor TXQTermVariable.create(const avalue: string; const anamespace: INamespace);
begin
  value := avalue;
  namespace := namespaceGetURL(anamespace);
  index := -1;
end;

constructor TXQTermVariable.create(const alocalname: string; const anamespace: string);
begin
  value := alocalname;
  namespace:= anamespace;
  index := -1;
end;

function TXQTermVariable.equalsVariable(v: TXQTermVariable): boolean;
begin
  result := (v.value = value) and equalNamespaces(namespace, v.namespace);
end;

function TXQTermVariable.evaluate(var context: TXQEvaluationContext): IXQValue;
  procedure fail;
  begin
    raise EXQEvaluationException.create('pxp:INTERNAL', 'Variable without index: ' + value);
  end;

begin
  if index >= 0 then
    result := context.temporaryVariables.top(self, index)
  else if index = -2 then
    result := context.getGlobalVariable(value, namespace)
  else begin
    fail;
    result := nil;
  end;
end;

function TXQTermVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[xqcdContextVariables];
end;

function TXQTermVariable.clone: TXQTerm;
var
  other: TXQTermVariable;
begin
  Result:=inherited clone;
  other := TXQTermVariable(result);
  other.namespace := namespace;
  other.value := value;
  other.index := index;
end;

function TXQTermVariable.ToString: ansistring;
begin
  result := '$';
  if namespace <> '' then result += 'Q{'+namespace+'}';
  result += value;
end;



function TXQTermVariableGlobal.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  if context.globallyDeclaredVariables.hasVariable(definition.getVariable, result) then
    exit();
  Result:=evaluateInitial(context);
end;

function TXQTermVariableGlobal.getContextDependencies: TXQContextDependencies;
begin
  result := [xqcdContextVariables];
end;

function TXQTermVariableGlobal.clone: TXQTerm;
begin
  Result:=TXQTermVariableGlobal.Create;
  TXQTermVariableGlobal(result).definition := definition;
end;

function TXQTermVariableGlobal.ToString: ansistring;
begin
  Result := definition.getVariable.ToString;
end;

function TXQTermVariableGlobal.evaluateInitial(var context: TXQEvaluationContext): IXQValue;
var
  v: TXQTermVariable;
  pendingname: String;
begin
  if context.globallyDeclaredVariables = nil then context.globallyDeclaredVariables := TXQVariableChangeLog.create();

  v := definition.getVariable;

  if context.staticContext.model in PARSING_MODEL3 then begin
    pendingname := #0 + v.value;
    if context.globallyDeclaredVariables.hasVariable(pendingname, v.namespace) then
      raise EXQEvaluationException.create('XQDY0054', 'Dependancy cycle: ' + v.ToString );
    context.globallyDeclaredVariables.add(pendingname, xqvalue(), v.namespace);
  end;

  result := definition.getClassicValue(context);
  context.globallyDeclaredVariables.add(v, result);
end;

function TXQTermVariableGlobalImported.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  oldSC: TXQStaticContext;
begin
  if context.globallyDeclaredVariables.hasVariable(definition.getVariable, result) then
    exit;
  oldSC := context.staticContext;
  context.staticContext := staticContext;
  Result:=evaluateInitial(context);
  context.staticContext := oldSC;
end;

function TXQTermVariableGlobalImported.clone: TXQTerm;
begin
  Result:=TXQTermVariableGlobalImported.Create;
  TXQTermVariableGlobalImported(result).definition := definition;
  TXQTermVariableGlobalImported(result).staticContext := staticContext;
end;


{ TXQTermTypeSwitch }


function TXQTermTypeSwitch.TXQTermTypeSwitchClause.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  j: Integer;
begin
  if pattern = nil then begin
    if variable <> nil then visitor.declare(@variable, self);
    if typ <> nil then begin
      result := visitor.simpleTermVisit(@typ, self);
      case result of xqtvaAbort: exit; end;
    end;
    result := visitor.simpleTermVisit(@expr, self);
    case result of xqtvaAbort: exit; end;
    if variable <> nil then visitor.undeclare(@variable, self);
  end else begin
    result := visitor.simpleTermVisit(@pattern, self);
    case result of xqtvaAbort: exit; end;

    visitor.parent := self;
    with pattern do
      for j := 0 to high(vars) do
        visitor.declare(@vars[j]);

    result := visitor.simpleTermVisit(@expr, self);
    case result of xqtvaAbort: exit; end;

    visitor.parent := self;
    with pattern do
      for j := 0 to high(vars) do
        visitor.undeclare(@vars[j]);
  end;
end;

function TXQTermTypeSwitch.TXQTermTypeSwitchClause.getContextDependencies: TXQContextDependencies;
begin
  if expr <> nil then result := expr.getContextDependencies
  else result := []
end;

function TXQTermTypeSwitch.TXQTermTypeSwitchClause.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  raiseInternalError(1604301730);
  result := nil;
end;

function TXQTermTypeSwitch.TXQTermTypeSwitchClause.clone: TXQTerm;
begin
  Result:=TXQTermTypeSwitch.TXQTermTypeSwitchClause.Create;
  if pattern <> nil then TXQTermTypeSwitch.TXQTermTypeSwitchClause(result).pattern := TXQTermPatternMatcher(pattern.clone);
  if variable <> nil then TXQTermTypeSwitch.TXQTermTypeSwitchClause(result).variable := TXQTermVariable(variable.clone);
  if typ <> nil then TXQTermTypeSwitch.TXQTermTypeSwitchClause(result).typ := TXQTermSequenceType(typ.clone);
  TXQTermTypeSwitch.TXQTermTypeSwitchClause(result).expr := expr.clone;
end;

destructor TXQTermTypeSwitch.TXQTermTypeSwitchClause.Destroy;
begin
  pattern.free;
  variable.free;
  typ.free;
  expr.free;
  inherited Destroy;
end;

function TXQTermTypeSwitch.TXQTermTypeSwitchClause.matched(var context: TXQEvaluationContext; const v: IXQValue; out res: IXQValue
  ): boolean;
  function evaluateWithChangedVariable(const value: IXQValue): IXQValue;
  begin
    if variable = nil then exit(expr.evaluate(context));
    context.temporaryVariables.push(variable, value);
    result := expr.evaluate(context);
    context.temporaryVariables.pop();
  end;
  function evaluateWithChangedVariableLog(log: TXQVariableChangeLog; const vars: array of TXQtermVariable): IXQValue;
  var
    value: IXQValue;
    i: Integer;

    oldContextItem: IXQValue;
    oldSeqIndex: Integer;
    oldSeqLength, stackSize: Integer;
    hasContextItem: Boolean;
    oldlog: TXQVariableChangeLog;
  begin
    oldlog := log;
    log := log.condensedCollected;
    try
      stackSize := context.temporaryVariables.count;
      hasContextItem := log.hasVariable('$', value); //todo should be statically?
      if  hasContextItem then begin
        if value.getSequenceCount <> 1 then raiseEvaluationError('pxp:PATTERN1', 'Can only assign singleton to context item');
        context.getContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
        context.setContextItem(value);
      end;
      for i := 0 to high(vars) do
         context.temporaryVariables.push(vars[i], log.get(vars[i].value, vars[i].namespace) );
      result := expr.evaluate(context);
    finally
      context.temporaryVariables.popTo(stackSize);
      if hasContextItem then context.setContextItem(oldContextItem,oldSeqIndex,oldSeqLength);
      oldlog.free       ;
      log.Free;
    end;
  end;

var
  log: TXQVariableChangeLog;
begin
  if pattern <> nil then begin
    log := patternMatcherMatch(pattern.node, v.toNode, context);
    result := log <> nil;
    if result then res := evaluateWithChangedVariableLog(log, pattern.vars);
    exit;
  end;

  if (typ <> nil) and not typ.instanceOf(v, context) then
    exit(false);

  result := true;
  res := evaluateWithChangedVariable(v);
end;




function TXQTermTypeSwitch.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  v: IXQValue;
  i: Integer;
begin
  v := children[0].evaluate(context);
  for i:=1 to high(children) do
    if TXQTermTypeSwitchClause(children[i]).matched(context, v, result) then
      exit;
  raiseInternalError(164301750);
  result := nil;
end;

function TXQTermTypeSwitch.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@children[0], self);
  case result of xqtvaAbort: exit; end;
  Result:=inherited visitchildren(visitor);
end;





function TXQTermSwitch.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  v: IXQValue;
  i, j: Integer;
  operand: IXQValue;
  seq: TXQTermWithChildren;
begin
  v := children[0].evaluate(context);
  if v.getSequenceCount > 1 then raiseXPTY0004TypeError(v, 'singleton');
  v := xqvalueAtomize(v);
  for i:=1 to high(children) - 1 do begin
    if not (children[i] is TXQTermWithChildren) then raiseEvaluationError('XPST0003', 'Invalid case clause');
    seq := TXQTermWithChildren(children[i]);
    for j := 0 to high(seq.children) - 1 do begin
      operand := xqvalueAtomize(seq.children[j].evaluate(context));
      if operand.getSequenceCount > 1 then raiseXPTY0004TypeError(operand, 'Sequences are not allowed as case operands in a switch statement');
      if xqvalueDeep_equal(context, v, operand, context.staticContext.collation) then
        exit(seq.children[high(seq.children)].evaluate(context));
    end;
  end;
  i := high(children);
  exit(children[i].evaluate(context));
end;

function TXQTermSwitch.getContextDependencies: TXQContextDependencies;
begin
  Result:=getChildrenContextDependencies;
end;


{ TXQTermConstructor }

constructor TXQTermConstructor.create(atype: TTreeNodeType; aname: txqterm = nil);
begin
  typ := atype;
  nameValue := aname;
end;

function TXQTermConstructor.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  temp: Integer;
begin
  temp := 0;
  result := evaluate(context, nil, temp);
end;

function TXQTermConstructor.evaluate(var context: TXQEvaluationContext; root: TTreeNode; var baseOffset: longint): IXQValue;
var
  tree: TTreeNode;
  kid: TTreeNode;
  lastKid: TTreeNode;
  subcontext: ^TXQEvaluationContext;
  oldnamespacecount: integer;
  procedure addKid;
  begin
    if (kid.typ = tetText) and (kid.value = '') then begin
      kid.free;
      kid := nil;
      exit;
    end;
    if tree.next = nil then begin
      tree.next := kid;
      kid.previous := tree;
    end else begin
      if (kid.typ = tetText) and (lastKid.typ = tetText) then begin
        lastKid.value += kid.value;
        kid.free;
        exit;
      end;
      lastKid.next := kid;
      kid.previous := lastKid;
    end;
    kid.document := root;
    kid.parent := tree;
    //kid.offset := baseOffset;
    //baseOffset += 1;
    if kid.reverse <> nil then lastKid := kid.reverse
    else lastKid := kid;
  end;
  function needNamespaceList: TNamespaceList;
  begin
    if (subcontext^.namespaces = nil) then begin
      new(subcontext);
      subcontext^ := context;
      subcontext^.namespaces := TNamespaceList.Create;
    end;
    result := subcontext^.namespaces;
  end;

  function declareNewNamespace(const url, prefix: string): INamespace;
  begin
    needNamespaceList;
    result := subcontext^.findNamespace(prefix, xqdnkAny);
    if (result <> nil) and (result.getURL = url) then begin
      subcontext^.namespaces.Add(result);
      exit;
    end;
    result := TNamespace.make(url, prefix);
    subcontext^.namespaces.Add(result);
  end;

  function chooseRandomPrefix(): string;
  begin
    result := 'XXX'; //just like Zorba
    while subcontext^.findNamespace(result, xqdnkAny) <> nil do result += 'X';
  end;

  procedure addNodeAsKid;
  var
    doc: TTreeNode;
    n: TTreeNode;
    attrib: TTreeAttribute;
    i: Integer;
    namespaceIndex: Integer;
  begin
    kid.parent := tree;
    case kid.typ of
      tetAttribute: begin
        if (kid.namespace <> nil) and (kid.namespace.getPrefix <> '') then begin
          //ensure that there is no conflict between new namespace nodes and default namespace nodes by renaming latter
          //todo: what happens if a later namespace node needs to override the new replacement namespace?
          if not TTreeAttribute(kid).isNamespaceNode then begin
            namespaceIndex := needNamespaceList.lastIndexOfNamespacePrefix(kid.namespace.getPrefix);
            if (namespaceIndex < 0) or not equalNamespaces(subcontext^.namespaces.items[namespaceIndex], kid.namespace) then begin
              //if the namespace prefix is not already correctly bound, we need to add it. But if we have already a namespace for that prefix, we need use another prefix
              //Also this does not actually create namespace nodes. Real namespace nodes might be inherited by the children, but these implicit namespaces nodes must not.
              if namespaceIndex >= oldnamespacecount then kid.namespace := TNamespace.make(kid.namespace.getURL, chooseRandomPrefix());
              subcontext^.namespaces.add(kid.namespace)
            end;
          end else begin
            if (tree.namespace <> nil) and (tree.namespace.getPrefix = kid.value) and (tree.namespace.getURL <> TTreeAttribute(kid).realvalue) then begin
             tree.namespace := TNamespace.make(tree.namespace.getURL, chooseRandomPrefix());
              tree.addAttribute(tree.namespace.getPrefix, tree.namespace.getURL, XMLNamespace_XMLNS as TNamespace);
              needNamespaceList.add(tree.namespace);
            end;
          end;
        end;
        if tree.attributes = nil then tree.attributes := TAttributeList.Create;
        tree.attributes.add(kid);
        //kid.offset:=baseOffset;
        kid.parent:=tree;
        kid.document:=tree.document;
        //baseOffset+=1;

        //errors need to be thrown after attribute has been added or we get memory leaks
        if tree.typ = tetDocument then raiseTypeError0004('Document node must not contain attributes');
        if tree.getFirstChild() <> nil then
          raiseEvaluationError('XQTY0024', 'Attribute node after element node');
        for i := 0 to tree.attributes.Count - 2 do
          with tree.attributes.Items[i] do if (value = kid.value) and equalNamespaces(namespace, kid.namespace) then begin
            if not equalNamespaces(namespace, XMLNamespace_XMLNS) then
             raiseEvaluationError('XQDY0025', 'Duplicate attribute: '+kid.value)
            else if (kid as TTreeAttribute).realvalue <> tree.attributes.Items[i].realvalue then
             raiseEvaluationError('XQDY0102', 'Mismatched namespace: '+kid.value);
          end;
        if (kid as TTreeAttribute).isNamespaceNode then begin
          if (kid.value = '') and ((tree.namespace = nil) or (tree.namespace.getPrefix = '')) and (TTreeAttribute(kid).realvalue <> namespaceGetURL(tree.namespace)) then
            raiseEvaluationError('XQDY0102', 'Namespace override');
          needNamespaceList.addIfNewPrefixUrl(TNamespace.make(TTreeAttribute(kid).realvalue, kid.value));
        end;
      end;
      tetOpen, tetComment, tetText, tetProcessingInstruction:
        addKid;
      tetDocument: begin
        doc := kid;
        kid := kid.getFirstChild();
        while kid <> nil do begin
          n := kid.getNextSibling();
          if doc.attributes <> nil then begin //a document should not have attributes, but when namespace overrides were added by addValue, they were added there
            if kid.attributes = nil then kid.attributes := TAttributeList.Create;
            for attrib in doc.attributes do kid.attributes.add(attrib.clone);
          end;
          addKid;
          kid := n;
        end;
        doc.next := doc.reverse;
        doc.reverse.previous := doc;
        doc.deleteAll();
      end
      else raiseEvaluationError('pxp:INTERNAL', 'Unknown child node');
    end;
  end;

  procedure addValue(const v: IXQValue; firstAtomic: boolean = true);
  var temp: TTreeNode;
      i: integer;
      tempNamespaces: TNamespaceList;
      x: PIXQValue;
  begin
    case v.kind of
      pvkUndefined:;
      pvkNode: begin
        kid := v.toNode.clone;
        if root = tree then begin
          if kid.getFirstChild() = nil then kid.document := root
          else begin
            temp := kid;
            while (temp <> nil) and (temp <> kid.reverse) do begin
              temp.document := root;
              temp.offset := baseOffset;
              if (temp.next = nil) or (temp.next.offset <= temp.offset) then baseOffset+=1+temp.getAttributeCount()
              else baseOffset += temp.next.offset - temp.offset;
              temp := temp.next;
            end;
          end;
        end;
        case kid.typ of
          tetOpen, tetDocument: begin
            if not context.staticContext.copyNamespacePreserve then begin
              //remove old namespaces that are not used
              temp := kid;
              while (temp <> nil) and (temp <> kid.reverse) do begin
                if temp.attributes <> nil then
                  for i := temp.attributes.Count - 1 downto 0 do
                    if (temp.attributes.Items[i].isNamespaceNode) and not (temp.isNamespaceUsed(temp.attributes.Items[i].toNamespace)) then
                      temp.attributes.Delete(i);
                temp := temp.next;
              end;
            end;
            if v.toNode.parent <> nil then begin
              //copy namespaces of old parent
              tempNamespaces := TNamespaceList.Create;
              v.toNode.parent.getAllNamespaces(tempNamespaces);
              for i:=0 to tempNamespaces.Count - 1 do
                if context.staticContext.copyNamespacePreserve or (kid.isNamespaceUsed(tempNamespaces.items[i])) then
                  kid.addNamespaceDeclaration(tempNamespaces.items[i], false);
              {if not tempNamespaces.hasNamespacePrefix('') then begin
                v.toNode.getOwnNamespaces(tempNamespaces);
                if not tempNamespaces.hasNamespacePrefix('') then
                  kid.addNamespaceDeclaration(TNamespace.make('', ''), false);
              end;}
              tempNamespaces.free;
            end;
            if (not context.staticContext.copyNamespaceInherit) and (subcontext^.namespaces <> nil) then begin
              //remove namespaces of new parent
              for i := 0 to subcontext^.namespaces.Count - 1 do
                if not kid.isNamespaceUsed(subcontext^.namespaces.items[i]) then begin
                  if kid.attributes = nil then kid.attributes := TAttributeList.Create;
                  if subcontext^.namespaces.items[i].getPrefix = '' then kid.attributes.add('xmlns', '')
                  else kid.attributes.add(subcontext^.namespaces.items[i].getPrefix, '', XMLNamespace_XMLNS);
                end;
            end;
          end;
        end;
        addNodeAsKid;
      end;
      pvkSequence: begin
        firstAtomic := true;
        for x in v.GetEnumeratorPtrUnsafe do begin
          addValue(x^, firstAtomic);
          firstAtomic := x^.kind = pvkNode;
        end;
      end;
      pvkFunction: raiseEvaluationError('XQTY0105', 'Functions passed to element constructor')
      else if (lastKid = nil) or (lastKid.typ <> tetText) then begin
        kid := TTreeNode.create(tetText);
        if firstAtomic then kid.value:=v.toString
        else kid.value := ' '+ v.toString;
        kid.offset:=baseOffset;
        baseOffset+=1;
        addKid;
      end else begin
        if lastKid.typ <> tetText then raiseEvaluationError('pxp:INTERNAL', 'No atomic value before this one exist, but this one is not the first atomic value.');
        if firstAtomic then lastKid.value += v.toString
        else lastKid.value += ' ' + v.toString;
      end;
    end;
  end;

  function valueToString(const v: IXQValue): string;
  var
    first: Boolean;
    x: PIXQValue;
  begin
    first := true;
    result := '';
    for x in v.GetEnumeratorPtrUnsafe do begin
      if not first then result += ' ';
      if x^.kind = pvkFunction then raiseEvaluationError('FOTY0013', 'Cannot serialize function');
      result += x^.toString;
      first := false;
    end;
  end;

  procedure setTreeHash;
  begin
    if nameHash <> 0 then tree.hash := nameHash
    else tree.hash := nodeNameHash(tree.value);
  end;

  procedure setTreeNodeNameFromTerm(t: TXQTerm; defkind: TXQDefaultNamespaceKind);

    procedure setTreeNodeNameFromQName(const url, aprefix, local: string; defkind: TXQDefaultNamespaceKind);
    var
      prefix: String;
    begin
      prefix := aprefix;
      if (url = '') and (prefix = '') then
        tree.namespace := nil //something weird happens, if there is a non-nil Q{} Qname. Children of this node that have an empty-prefix namespace inherit this non-nil non existing namespace, so copying and serializing them, duplicates there namespace declaration attribute
      else begin
        tree.namespace := subcontext^.findNamespace(prefix, defkind);
        if ((tree.namespace = nil) or (tree.namespace.getURL <> url))
            and ((defkind = xqdnkElementType) or (url <> '')) then begin
          if (prefix = '') and (url = XMLNamespaceUrl_XML) then prefix := 'xml';
          if (prefix = '') and (defkind <> xqdnkElementType) then prefix := chooseRandomPrefix;
          tree.namespace := declareNewNamespace(url, prefix);
        end;
      end;
      tree.value := local;
    end;

    procedure setTreeNodeNameFromValue(v: IXQValue; defkind: TXQDefaultNamespaceKind);
    var
      vname: TXQValueQName;
    begin
      if (v.kind = pvkSequence) and (v.getSequenceCount = 1) then v := v.get(1);
      if (v.instanceOf(baseSchema.QName))  then begin
        vname := (v as TXQValueQName);
        setTreeNodeNameFromQName(vname.url, vname.prefix, vname.local, defkind);
      end else if v.instanceOf(baseSchema.string_) or v.instanceOf(baseSchema.untypedAtomic) or v.instanceOf(baseSchema.node) then begin
        tree.value := v.toString;
        if pos(':', tree.value) = 0 then tree.namespace := subcontext^.findNamespace('', defkind)
        else begin
          subcontext^.splitRawQName(tree.namespace, tree.value, defkind);
          if tree.namespace = nil then raiseEvaluationError('XQDY0074', 'Failed to find namespace for '+v.toString);
        end;
      end else raiseEvaluationError('XPTY0004', 'Expected string value');
      if tree.namespace <> nil then
        with tree.namespace as TNamespace do
          if ((prefix = 'xml') <> (url = XMLNamespaceUrl_XML)) or (prefix = 'xmlns') or (url = XMLNamespaceUrl_XMLNS) then
            raiseEvaluationError(IfThen(tree.typ <> tetAttribute, 'XQDY0096', 'XQDY0044'), 'Invalid namespace: '+tree.namespace.serialize);
      if not baseSchema.isValidNCName(tree.value) then
        raiseEvaluationError('XQDY0074', 'Invalid name: '+tree.value);
    end;
  var
    tqname: TXQTermEQNameToken;
  begin
    if t.ClassType = TXQTermEQNameToken then begin
      tqname := TXQTermEQNameToken(t);
      setTreeNodeNameFromQName(tqname.namespaceurl, tqname.namespaceprefix, tqname.localpart, defkind);
    end else setTreeNodeNameFromValue(t.evaluate(subcontext^), defkind);
    setTreeHash;
    if (tree.typ = tetAttribute) and (tree.value = 'xmlns') and (tree.namespace = nil) then
      raiseEvaluationError('XQDY0044', 'Attribute must not be called xmlns');
  end;

var i: integer;
  nameValueEvaluated: IXQValue;
begin
  if not (typ in [tetOpen,tetAttribute, tetDocument]) then nameValueEvaluated := nameValue.evaluate(context);
  if (typ = tetText) and (nameValueEvaluated.isUndefined) then exit(xqvalue);

  subcontext:=@context;
  oldnamespacecount := 0;
  if subcontext^.namespaces <> nil then oldnamespacecount := subcontext^.namespaces.Count;

  if (typ = tetDocument) then begin
    tree := TTreeDocument.create(nil);
    TTreeDocument(tree).baseURI := context.staticContext.baseURI;
  end else if typ in  [tetAttribute, tetNamespace] then tree := TTreeAttribute.create('','')
  else tree := TTreeNode.create(typ);
  if root = nil then root := tree;
  tree.document := root;
  if baseOffset = 0 then begin
      if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot create new nodespxp:eval without a xquery engine (e.g. from an interpreted function in a native module)');
    if context.staticContext.sender.FInternalDocuments = nil then context.staticContext.sender.FInternalDocuments := TFPList.Create;
    context.staticContext.sender.FInternalDocuments.Add(tree);
  end;
  try
    tree.offset:=baseOffset;
    baseOffset+=1;
    case typ of
      tetAttribute, tetNamespace: begin
        if typ = tetAttribute then setTreeNodeNameFromTerm(nameValue, xqdnkUnknown)
        else if nameValueEvaluated.instanceOf(baseSchema.string_) or nameValueEvaluated.instanceOf(baseSchema.untypedAtomic) or nameValueEvaluated.instanceOf(baseSchema.node)
                or nameValueEvaluated.isUndefined then begin
           tree.value := nameValueEvaluated.toString;
           if (tree.value <> '') and not baseSchema.isValidNCName(tree.value) then raise EXQEvaluationException.create('XQDY0074', 'Invalid namespace prefix');
           setTreeHash;
           tree.namespace := XMLNamespace_XMLNS;
        end else raiseTypeError0004('Invalid namespace prefix', nameValueEvaluated);
        for i:= 0 to high(children) do
          TTreeAttribute(tree).realvalue := TTreeAttribute(tree).realvalue + valueToString(children[i].evaluate(subcontext^));
        if (typ = tetNamespace) or ( (tree.namespace <> nil) and (tree.value = 'id') and (tree.namespace.getURL = XMLNamespaceUrl_XML) ) then begin
          TTreeAttribute(tree).realvalue := strTrimAndNormalize(TTreeAttribute(tree).realvalue);
          if typ = tetNamespace then
            if ((tree.value = 'xml') <> (TTreeAttribute(tree).realvalue = XMLNamespaceUrl_XML)) or
               (tree.value = 'xmlns') or
               (TTreeAttribute(tree).realvalue = XMLNamespaceUrl_XMLNS) or
               (TTreeAttribute(tree).realvalue = '') then
                 raise EXQEvaluationException.create('XQDY0101', 'Invalid namespace binding');
        end;
      end;
      tetOpen, tetDocument: begin
        if implicitNamespaces <> nil then begin
          needNamespaceList;
          for i:= 0 to implicitNamespaces.Count - 1 do begin
            tree.addNamespaceDeclaration(implicitNamespaces.namespaces[i], true);
            subcontext^.namespaces.add(implicitNamespaces.namespaces[i]);
          end;
        end;

        lastKid := nil;
        {for i:=0 to high(children) do           <-         that became deprecated, when namespace constructors where replaced by implicitNamespaces;
          if (children[i] is TXQTermConstructor) and (TXQTermConstructor(children[i]).isNamespaceConstructor) then begin
            if tree.typ = tetDocument then raiseTypeError0004('Document node must not contain namespaces');
            kid := TXQTermConstructor(children[i]).evaluate(subcontext^, root, baseOffset).toNode;
            addNodeAsKid;
          end;}

        if nameValue <> nil then begin
          setTreeNodeNameFromTerm(nameValue, xqdnkElementType);
          if tree.namespace <> nil then
            needNamespaceList.addIfNewPrefixUrl(tree.namespace);
        end;

        if (root = tree) and (oldnamespacecount > 0)  then
          //a newly constructed nodes has all current namespaces binding. But only necessary to add them to the root node, because otherwise they are inherited anyways
          for i := 0 to oldnamespacecount - 1 do
            tree.addNamespaceDeclaration(context.namespaces.items[i], false);


        for i:=0 to high(children) do begin
          if children[i] is TXQTermConstructor then begin
            //if TXQTermConstructor(children[i]).isNamespaceConstructor then continue;
            kid := TXQTermConstructor(children[i]).evaluate(subcontext^, root, baseOffset).toNode;
            {if (kid.typ = tetAttribute) then begin
              if (kid.namespace <> nil) then needNamespaceList.addIfNewPrefixUrl(kid.namespace);
            end;}
            addNodeAsKid;
          end else
            addValue(children[i].evaluate(subcontext^));
        end;

        kid := TTreeNode.create(tetClose, tree.value);
        kid.namespace := tree.namespace;
        kid.offset:=baseOffset;
        baseOffset+=1;
        addKid;
        kid.parent := tree.parent;
        tree.reverse := kid; kid.reverse := tree;
      end;
      tetProcessingInstruction: begin
        xqvalueSeqSqueeze(nameValueEvaluated);
        if nameValueEvaluated.instanceOf(baseSchema.NCName) then tree.value := nameValueEvaluated.toString
        else if nameValueEvaluated.instanceOf(baseSchema.string_) or nameValueEvaluated.instanceOf(baseSchema.untypedAtomic) or nameValueEvaluated.instanceOf(baseSchema.node) then begin
          tree.value := strTrim(nameValueEvaluated.toString, WHITE_SPACE);
          if not baseSchema.isValidNCName(tree.value) then raiseEvaluationError('XQDY0041', 'Invalid NCName');
        end else raiseTypeError0004('Need NCName', nameValueEvaluated);
        if striEqual(tree.value, 'XML') then raiseEvaluationError('XQDY0064', '"XML" cannot be used as PI name');
        setTreeHash;
        if length(children) = 0 then tree.addAttribute('', '')
        else begin
          tree.addAttribute('', strTrimLeft(valueToString(children[0].evaluate(subcontext^)), WHITE_SPACE));
          if strContains(tree.attributes.Items[0].realvalue, '?>') then
            raiseEvaluationError('XQDY0026', '?> in PI content');
        end;
      end;
      tetText: tree.value := valueToString(nameValueEvaluated);
      tetComment: begin
        tree.value := valueToString(nameValueEvaluated);
        if strContains(tree.value, '--') or strEndsWith(tree.value, '-') then raiseEvaluationError('XQDY0072', 'Invalid comment content');
      end
      else raiseEvaluationError('pxp:INTERNAL','Invalid type for constructor');
    end;
    result := xqvalue(tree);
  finally
    if subcontext <> @context then begin
      subcontext^.namespaces.free;
      Dispose(subcontext);
    end else if context.namespaces <> nil then
      context.namespaces.deleteFrom(oldnamespacecount);
  end;
end;

function TXQTermConstructor.getContextDependencies: TXQContextDependencies;
begin
  Result:=getChildrenContextDependencies + [xqcdContextOther {namespaces}];
end;

function TXQTermConstructor.isNamespaceConstructor: boolean;
begin
  result := ((typ = tetAttribute)
            and (nameValue is TXQTermConstant)
            and ((TXQTermConstant(nameValue).value.toString = 'xmlns') or strBeginsWith(TXQTermConstant(nameValue).value.toString, 'xmlns:')))
            or (typ = tetNamespace);
end;

function TXQTermConstructor.clone: TXQTerm;
begin
  Result:=inherited clone;
  with TXQTermConstructor(result) do begin
    typ := self.typ;
    if self.nameValue <> nil then
      nameValue := self.nameValue.clone;
    if self.implicitNamespaces <> nil then
      implicitNamespaces := Self.implicitNamespaces.clone;
  end;
end;

destructor TXQTermConstructor.destroy;
begin
  nameValue.Free;
  implicitNamespaces.Free;
  inherited destroy;
end;

function TXQTermConstructor.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
begin
  if nameValue <> nil then begin
    result := visitor.simpleTermVisit(@nameValue, self);
    case result of xqtvaAbort: exit; end;
  end;
  result := inherited visitchildren(visitor);
end;

{ TXQTermJSONObjectConstructor }

function TXQTermJSONObjectConstructor.evaluate(var context: TXQEvaluationContext): IXQValue;
  function transformValue(v: IXQValue): IXQValue;
  begin
    if v.getSequenceCount = 0 then
      exit(TXQValueJSONNull.create());
    if v.kind = pvkSequence then
      if v.getSequenceCount > 1 then begin
        result := TXQValueJSONArray.create(v.getSequenceCount);
        (result as TXQValueJSONArray).seq.add(v);
        exit;
      end else v := v.get(1);
    if v is TXQValueNode then
      exit(xqvalue(v.toNode.outerXML(false)));
    result := v;
  end;

var
  obj: TXQValueObject;
  i: Integer;
  v: IXQValue;
begin
  obj := TXQValueObject.create();
  if length(children) and 1 = 1 then raiseEvaluationError('pxp:OBJ', 'Need an even number of names+values in an object constructor');
  for i := 0 to length(children) div 2 - 1 do begin
    v := children[i*2+1].evaluate(context);
    if (length(optionals) > i) and (optionals[i]) and (v.getSequenceCount = 0) then continue;
    if context.staticContext.objectsRestrictedToJSONTypes then v := transformValue(v);
    obj.setMutable(children[i*2].evaluate(context).toString, v);
  end;
  result := obj;
end;

function TXQTermJSONObjectConstructor.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;


type
  TXQTerm_VisitorReplaceKnownVariables = class(TXQTerm_VisitorTrackKnownVariables)
    context: PXQEvaluationContext;
    //uncertainVariables: TXQVariableChangeLog; //pattern variables cannot be are not know statically
    patterns: array of TXQTermPatternMatcher;
    function visit(term: PXQTerm): TXQTerm_VisitAction; override;
    function leave(term: PXQTerm): TXQTerm_VisitAction; override;
    destructor Destroy; override;
  end;


constructor TXQTerm_VisitorTrackKnownVariables.create;
begin
  overridenVariables := TXQVariableChangeLog.create();
  tempValue := xqvalue();
end;

destructor TXQTerm_VisitorTrackKnownVariables.Destroy;
begin
  overridenVariables.Free;
  inherited Destroy;
end;

procedure TXQTerm_VisitorTrackKnownVariables.declare(v: PXQTermVariable);
begin
  overridenVariables.add(v^.value, tempValue, v^.namespace);
end;

procedure TXQTerm_VisitorTrackKnownVariables.undeclare(v: PXQTermVariable);
begin
  Assert((overridenVariables.varstorage[overridenVariables.count-1].name = v^.value) and
         equalNamespaces(overridenVariables.varstorage[overridenVariables.count-1].namespaceURL, v^.namespace));
  overridenVariables.removeLast;
end;

function TXQTerm_VisitorReplaceKnownVariables.visit(term: PXQTerm): TXQTerm_VisitAction;
var
  temp: IXQValue;
  v: TXQTermVariable;
  //selfdefined: Boolean;
begin
  if term^ is TXQTermVariable then begin
    if not overridenVariables.hasVariable(TXQTermVariable(term^), temp) then begin
      v := TXQTermVariable(term^);
      if v.index >= 0 then replace(term, TXQTermConstant.create(context^.temporaryVariables.top(v.index-overridenVariables.count)))
      else if v.index = -2 then replace(term, TXQTermConstant.create(v.evaluate(context^)))
      else raisePXPInternalError;

    end(* else if length(patterns) > 0 then begin
      {not sure what to do with these
      selfdefined := false;
      with patterns[high(patterns)] do
        for i := 0 to high(vars) do
          if vars[i].equalsVariable(TXQTermVariable(term^)) then
            selfdefined := true;}
      if uncertainVariables = nil then uncertainVariables := TXQVariableChangeLog.create();
      if not uncertainVariables.hasVariable(TXQTermVariable(term^), temp) then
        uncertainVariables.add(TXQTermVariable(term^), tempValue);
    end*);
  end else if term^ is TXQTermPatternMatcher then begin
    SetLength(patterns, length(patterns) + 1);
    patterns[high(patterns)] := TXQTermPatternMatcher(term^);
  end;
  result := xqtvaContinue;
end;

function TXQTerm_VisitorReplaceKnownVariables.leave(term: PXQTerm): TXQTerm_VisitAction;
begin
  if (length(patterns) > 0) and (patterns[high(patterns)] = term^) then
    setlength(patterns, high(patterns));
  result := xqtvaContinue;
end;

destructor TXQTerm_VisitorReplaceKnownVariables.Destroy;
begin
  //uncertainVariables.Free;
  inherited Destroy;
end;

{ TXQTermDefineFunction }



function makeNumberedVariable(i: integer; index: integer = -1): TXQTermVariable;
var temp: string;
begin
  SetLength(temp, sizeof(i)+1);
  temp[1] := '0';
  move(i, temp[2], sizeof(i));
  result := TXQTermVariable.create(temp, XMLNamespaceURL_MyExtensionOperators);
  if index < 0 then index := i;
  result.index := index;
end;

constructor TXQTermDefineFunction.createReference(const fun: TXQTermNamedFunction; arity: integer);
var
  i: Integer;
begin
  parameterCount := arity;
  SetLength(children, arity + 1);
  SetLength(fun.children, arity);
  for i := 1 to arity do begin
    children[i-1] := TXQTermDefineVariable.create(makeNumberedVariable(i));
    fun.children[i-1] := makeNumberedVariable(i);
    TXQTermVariable(fun.children[i-1]).index := arity - 1;
  end;
  children[high(children)] := fun;
  kind := xqtdfNamedReference;
end;

function TXQTermDefineFunction.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  temp: TXQValueFunction;
  tempVisitor: TXQTerm_VisitorReplaceKnownVariables;
  i: Integer;
  dfv: IXQValue;
begin

  case kind of
    xqtdfUserDefined: temp := define(context, true);
    xqtdfNamedReference: begin
      initNamedFunctionReference(context);
      if (children[high(children)] is TXQTermNamedFunction)
         and (TXQTermNamedFunction(children[high(children)]).kind in [xqfkUnknown, xqfkNativeInterpreted])
         and (length(TXQTermNamedFunction(children[high(children)]).interpretedFunction.parameters) = parameterCount)
         then begin
        temp := TXQTermNamedFunction(children[high(children)]).interpretedFunction.directClone as TXQValueFunction;
        assert(Assigned(context.staticContext) and Assigned(context.staticContext.sender));
        if temp.context.staticContext.sender = nil then temp.context.staticContext := context.staticContext.sender.StaticContext; //only for native interpreted functions
      end else temp := define(context, false);
    end;
    xqtdfStaticPartialApplication: temp := defineStaticPartialApplication(context);
    xqtdfDynamicPartialApplication: begin
      assert(children[0] is TXQTermDynamicFunctionCall);
      dfv := TXQTermDynamicFunctionCall(children[0]).children[0].evaluate(context);
      if not (dfv is TXQValueFunction) then raiseEvaluationError('XPTY0004', 'Need function for partial application, got: '+dfv.toXQuery());
      temp := defineDynamicPartialApplication(context, dfv as TXQValueFunction);
    end;
  end;
  if not temp.ownsTerms then
    temp.assignCopiedTerms(temp);
  result := temp;
  if kind <> xqtdfUserDefined then exit;
  tempVisitor := TXQTerm_VisitorReplaceKnownVariables.Create();
  try
    for i := 0 to high(temp.parameters) do
      tempVisitor.declare(@temp.parameters[i].variable);
    tempVisitor.context := @context;
    tempVisitor.simpleTermVisit(@temp.body, nil);
    {if tempVisitor.uncertainVariables <> nil then begin
      tempterm := TXQTermFlower.Create;
      setlength(tempterm.children, tempVisitor.uncertainVariables.count + 1);
      for i := 0 to tempVisitor.uncertainVariables.count - 1 do begin
        tempterm.children[i] := TXQTermFlowerLet.Create;
        with TXQTermFlowerLet(tempterm.children[i]) do begin
          loopvar := TXQTermVariable.create(tempVisitor.uncertainVariables.varstorage[i].name, tempVisitor.uncertainVariables.varstorage[i].namespaceURL);
          if not context.temporaryVariables.hasVariable(loopvar, tempv) then  //todo, statically?
            if not context.hasGlobalVariable(loopvar, tempv) then  //todo, statically?
              tempv := xqvalue();
          expr := TXQTermConstant.Create(tempv);
        end;
      end;
      tempterm.children[high(tempterm.children)] := temp.body;
      temp.body := tempterm;
    end; }
  finally
    tempVisitor.free;
  end;
end;

function TXQTermDefineFunction.define(var context: TXQEvaluationContext; const clearFocus: boolean): TXQValueFunction;
var
  i: Integer;
  v: TXQTermVariable;
begin
  result := TXQValueFunction.create();
  if name <> nil then begin
    result.namespaceURL := name.namespaceURL;
    result.namespacePrefix := name.namespacePrefix;
    result.name:=name.localname;
  end;
  result.annotations := annotations;
  setlength(result.parameters, parameterCount);
  for i := 0 to high(result.parameters) do begin
    v := ((children[i] as TXQTermDefineVariable).variable as TXQTermVariable);
    result.parameters[i].variable := v;
    if length((children[i] as TXQTermDefineVariable).children) > 0 then
      result.parameters[i].seqtype:=(children[i] as TXQTermDefineVariable).children[0] as TXQTermSequenceType
     else
      result.parameters[i].seqtype:=nil;
  end;
  if length(children) > parameterCount then begin
    if (length(children) > 1) and (children[high(children) - 1] is TXQTermSequenceType) then result.resulttype := children[high(children)-1] as TXQTermSequenceType
    else if children[high(children)] is TXQTermSequenceType then result.resulttype := children[high(children)] as TXQTermSequenceType;

    if not (children[high(children)] is TXQTermSequenceType) then
      result.body := children[high(children)];
  end;
  result.context := context;
  if clearFocus then begin
    result.context.ParentElement := nil;
    result.context.RootElement := nil;
    result.context.TextNode := nil;
    result.context.SeqValue := nil;
    result.context.SeqIndex := 0;
    result.context.SeqLength := 0;
  end;
end;

function TXQTermDefineFunction.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - ALL_CONTEXT_DEPENDENCIES_FOCUS;
end;

function nativeMetaTypeToActualType(t: TXQTermSequenceType): TXQTermSequenceType;
begin
  if t = nil then exit(nil);
  result := TXQTermSequenceType(t.clone);
  if (t.kind = tikAtomic) then
    if ((t.atomicTypeInfo = baseSchema.numericPseudoType)) then
      result.atomicTypeInfo := baseSchema.anyAtomicType;
end;

function isPseudoVariable(v: TXQTerm): boolean; inline;
begin
  result := (v.ClassType = TXQTermPlaceholderVariable);
end;

function TXQTermDefineFunction.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
  varDef: TXQTermDefineVariable;
begin
  visitor.parent := self;
  result := xqtvaContinue;
  case kind of
    xqtdfUserDefined, xqtdfNamedReference: begin
      for i := 0 to parameterCount - 1 do begin
        varDef := children[i] as TXQTermDefineVariable;
        visitor.declare(@varDef.variable);
        if length(varDef.children) > 0 then begin
          result := visitor.simpleTermVisit(@children[i], self);
          case result of xqtvaAbort: exit; end;
          visitor.parent := self;
        end;
      end;
      for i := parameterCount to high(children) do begin
        result := visitor.simpleTermVisit(@children[i], self);
        case result of xqtvaAbort: exit; end;
      end;
      visitor.parent := self;
      for i := parameterCount - 1 downto 0 do
        visitor.undeclare(@((children[i] as TXQTermDefineVariable).variable));
    end;
    xqtdfStaticPartialApplication, xqtdfDynamicPartialApplication: begin
      visitor.simpleTermVisit(@children[0], self);
    end;
  end;

end;

function TXQTermDefineFunction.clone: TXQTerm;
var
  other: TXQTermDefineFunction;
begin
  Result:=inherited clone;
  other := TXQTermDefineFunction(result);
  other.name := name;
  if name <> nil then other.name := name.clone;
  other.parameterCount := parameterCount;
  copyAnnotations(other.annotations, annotations);
  other.kind := kind;
end;

destructor TXQTermDefineFunction.destroy;
begin
  freeAnnotations(annotations);
  name.free;
  inherited destroy;
end;

function TXQTermDefineFunction.findNamedFunctionVersion(const context: TXQEvaluationContext): PXQFunctionParameterTypes;
var
  f: TXQTermNamedFunction;
  i: Integer;
begin
  f := TXQTermNamedFunction(children[high(children)]);
  if f.kind in [xqfkUnknown, xqfkNativeInterpreted] then
    f.init(context.staticContext);

  result := nil;
  case f.kind  of
    xqfkBasic, xqfkComplex:
      for i := 0 to high(f.func.versions) do
        if length(f.func.versions[i].types) = length(f.children) then
          exit(@f.func.versions[i]);
    xqfkTypeConstructor: if length(f.children) = 1 then exit;
    xqfkUnknown, xqfkNativeInterpreted: exit;
  end;

  if (result = nil) and context.staticContext.strictTypeChecking and ((f.kind <> xqfkBasic) or (TXQBasicFunctionInfo(f.func).func <> @xqFunctionConcat)) then
    raiseParsingError('XPST0017', 'Failed to find function (mismatch arg count?)');
end;

function functionReferenceGetResultType(f: TXQTermNamedFunction): TXQTermSequenceType;
var
  t: TXSType;
begin
  if f.kind = xqfkTypeConstructor then begin
    t := TXSType(TObject(f.func));
    result := TXQTermSequenceType.create(t, true);
    if t is TXSListType then begin
      result.atomicTypeInfo := TXSListType(t).itemType;
      result.allowMultiple := true;
    end;
  end else result := nil;
end;

procedure TXQTermDefineFunction.initNamedFunctionReference(const context: TXQEvaluationContext);


var
  nc: array of TXQTerm;
  version: ^TXQFunctionParameterTypes;
  resulttype: TXQTermSequenceType;
  f: TXQTermNamedFunction;
  i: Integer;
begin
  if (kind <> xqtdfNamedReference) or initialized then exit;

  if (children[high(children)] is TXQTermNamedFunction) then begin
    version := findNamedFunctionVersion(context);
    f := TXQTermNamedFunction(children[high(children)]);

//    if (name = nil) and (f.name <> nil) then name := f.name.clone;

    resulttype := nil;
    if version <> nil then begin
      for i := 0 to parameterCount-1 do
        if version^.types[i] <> nil then
          (children[i] as TXQTermDefineVariable).push(nativeMetaTypeToActualType(version^.types[i]));
      resulttype := nativeMetaTypeToActualType(version^.returnType);
    end else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then begin
      for i := 0 to parameterCount-1 do
        (children[i] as TXQTermDefineVariable).push(TXQTermSequenceType.create(baseSchema.anyAtomicType, true));
      resulttype := TXQTermSequenceType.create(baseSchema.string_);
    end else if f.kind = xqfkTypeConstructor then begin
      for i := 0 to parameterCount - 1 do
        (children[i] as TXQTermDefineVariable).push(TXQTermSequenceType.create(baseSchema.anyAtomicType, true));
      resulttype := functionReferenceGetResultType(f);
      children[high(children)] := f.convertToTypeConstructor;
    end;

    if (f.kind in [xqfkUnknown, xqfkNativeInterpreted]) and (length(f.interpretedFunction.annotations) > 0) then
      copyAnnotations(annotations, f.interpretedFunction.annotations);

    if resulttype <> nil then begin
      nc := children;
      SetLength(nc, length(nc) + 1);
      nc[high(nc)] := nc[high(nc)-1];
      nc[high(nc)-1] := resulttype;
      children := nc;
    end;
  end;
  initialized := true;
end;

function TXQTermDefineFunction.defineStaticPartialApplication(var context: TXQEvaluationContext): TXQValueFunction;
var
  f: TXQTermNamedFunction;
  version: PXQFunctionParameterTypes;
  j: Integer;
  i, stacksize: Integer;
begin
  version := findNamedFunctionVersion(context);
  f := TXQTermNamedFunction(children[high(children)]);

  result := TXQValueFunction.create();
  if (f.kind in [xqfkUnknown, xqfkNativeInterpreted]) and (length(f.interpretedFunction.annotations) > 0) then
    copyAnnotations(result.annotations, f.interpretedFunction.annotations);
  if f.kind = xqfkTypeConstructor then result.body := TXQTermNamedFunctionTypeConstructor.create()
  else result.body := TXQTermNamedFunction.create();
  TXQTermNamedFunction(result.body).assignWithoutChildren(f);

  setlength(result.parameters, parameterCount);
  j := 0;
  stacksize := context.temporaryVariables.Count;
  for i := 0 to high(f.children) do begin
    if isPseudoVariable(f.children[i]) then begin
      //Parameter index, e.g.
      //v0 v1 v2 v3 | v0 n v1 n n v2
      // 3  2  1 0 <--/    |       |
      //    4  3 2     1 0<|       |
      //       6 5     4 3  2 1 0<-|
      TXQTermNamedFunction(result.body).push(makeNumberedVariable(parameterCount - j - 1, parameterCount - j - 1 + i));
      result.parameters[j].variable := makeNumberedVariable(parameterCount - j - 1);
      if version <> nil then
        result.parameters[j].seqtype := nativeMetaTypeToActualType(version^.types[i])
       else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then begin
        result.parameters[j].seqtype := TXQTermSequenceType.create(baseSchema.anyAtomicType, true);
       end else if f.kind in [xqfkUnknown, xqfkNativeInterpreted] then begin
        if f.interpretedFunction.parameters[i].seqtype <> nil then
          result.parameters[j].seqtype := TXQTermSequenceType(f.interpretedFunction.parameters[i].seqtype.clone);
       end else if f.kind = xqfkTypeConstructor then begin
         result.parameters[j].seqtype := TXQTermSequenceType.create(baseSchema.anyAtomicType, true);
       end;
      inc(j);
     end else
      TXQTermNamedFunction(result.body).push(TXQTermConstant.create(f.children[i].evaluate(context)));
     context.temporaryVariables.push(xqvalue); //if this was a normal call, parameters would be pushed, so the offsets in the terms expect them
  end;
  context.temporaryVariables.popTo(stackSize);
  assert(j = parameterCount);
  result.ownsTerms := true;
  if version <> nil then
    result.resulttype := nativeMetaTypeToActualType(version^.returnType)
  else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then
    result.resulttype := TXQTermSequenceType.create(baseSchema.string_)
  else if (f.kind in [xqfkUnknown, xqfkNativeInterpreted]) and (f.interpretedFunction.resulttype <> nil)  then
    result.resulttype := TXQTermSequenceType(f.interpretedFunction.resulttype.clone)
  else result.resulttype := functionReferenceGetResultType(f);
  result.context := context;
end;

function TXQTermDefineFunction.defineDynamicPartialApplication(var context: TXQEvaluationContext; f: TXQValueFunction): TXQValueFunction;
var
  j: Integer;
  i, stackSize: Integer;
  ch: array of TXQTerm;
begin
  ch := (children[0] as TXQTermWithChildren).children;
  if length(ch) <> length(f.parameters) + 1 then raiseParsingError('XPTY0004', 'Invalid argument count');
  result := TXQValueFunction.create();
  copyAnnotations(result.annotations, f.annotations);
  result.body := TXQTermDynamicFunctionCall.create(TXQTermConstant.Create(f));
  setlength(result.parameters, parameterCount);
  stackSize := context.temporaryVariables.Count;
  j := 0;
  for i := 0 to high(f.parameters) do begin
    if isPseudoVariable(ch[i+1]) then begin
      TXQTermDynamicFunctionCall(result.body).push(makeNumberedVariable(parameterCount - j - 1, parameterCount - j - 1 + i ));
      result.parameters[j].variable := TXQTermVariable(makeNumberedVariable(parameterCount - j - 1));
      if f.parameters[i].seqtype <> nil then
        result.parameters[j].seqtype := TXQTermSequenceType(f.parameters[i].seqtype.clone);
      inc(j);
     end else
      TXQTermDynamicFunctionCall(result.body).push(TXQTermConstant.create(ch[i+1].evaluate(context)));
     context.temporaryVariables.push(xqvalue); //if this was a normal call, parameters would be pushed, so the offsets in the terms expect them
  end;
  context.temporaryVariables.popTo(stackSize);
  assert(j = parameterCount);
  result.ownsTerms := true;
  //it should be possible to change this to share the type instead cloning it
  //since the body contains a reference to f, so f will not be freed before result and the type remain valid
  //(same for the parameters)
  //but it is tricky to get right
  if f.resulttype <> nil then result.resulttype := TXQTermSequenceType(f.resulttype.clone);
  result.context := f.context;
end;


{ TXQTermTryCatch }

constructor TXQTermTryCatch.create(abody: TXQTerm);
begin
  body := abody;
  infovars[0] := TXQTermVariable.create('code', XMLNamespaceURL_XQTErrors);
  infovars[1] := TXQTermVariable.create('description', XMLNamespaceURL_XQTErrors);
  infovars[2] := TXQTermVariable.create('value', XMLNamespaceURL_XQTErrors);
  infovars[3] := TXQTermVariable.create('module', XMLNamespaceURL_XQTErrors);
  infovars[4] := TXQTermVariable.create('line-number', XMLNamespaceURL_XQTErrors);
  infovars[5] := TXQTermVariable.create('column-number', XMLNamespaceURL_XQTErrors);
  infoVars[6] := TXQTermVariable.create('additional', XMLNamespaceURL_XQTErrors);
end;

function TXQTermTryCatch.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  ns: String;
  matches: Boolean;
  i: Integer;
  j, stackSize: Integer;
  nsprefix: String;
begin
  stackSize := context.temporaryVariables.count;
  try
    if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, 0, nil);
    result := body.evaluate(context);
    if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, -1, nil);
  except
    on e: EXQEvaluationException do begin
      if Assigned(XQOnGlobalDebugTracing) then XQOnGlobalDebugTracing(self, context, -1, nil);
      if e.namespace <> nil then begin
        ns := e.namespace.getURL;
        nsprefix := e.namespace.getPrefix;
      end else begin
        ns := XMLNamespaceURL_XQTErrors;
        nsprefix := 'err';
      end;

      for i := 0 to high(catches) do begin
        matches := false;
        for j := 0 to high(catches[i].tests) do
          if ((catches[i].tests[j].name.localname = '*') or (catches[i].tests[j].name.localname = e.errorCode) )  and
             ((catches[i].tests[j].ignoreNamespace) or (catches[i].tests[j].name.namespaceURL = ns) ) then begin
            matches := true;
            break;
          end;
        if matches then begin
          context.temporaryVariables.popTo(stackSize);
          context.temporaryVariables.push(infoVars[0], TXQValueQName.create(ns, nsprefix, e.errorCode));
          context.temporaryVariables.push(infoVars[1], xqvalue(e.rawMessage));
          if e.value = nil then e.value := xqvalue();
          context.temporaryVariables.push(infoVars[2],e.value);
          context.temporaryVariables.push(infoVars[3], xqvalue());
          context.temporaryVariables.push(infoVars[4],xqvalue());
          context.temporaryVariables.push(infoVars[5],xqvalue());
          context.temporaryVariables.push(infoVars[6],xqvalue());

          result := catches[i].expr.evaluate(context);
          context.temporaryVariables.popTo(stackSize);
          exit;
        end;
      end;

      raise;
    end;
  end;
  context.temporaryVariables.popTo(stackSize);
end;

function TXQTermTryCatch.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := body.getContextDependencies;
  for i := 0 to high(catches) do
    result += catches[i].expr.getContextDependencies;
end;

function TXQTermTryCatch.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
begin
  result := visitor.simpleTermVisit(@body, self);
  case result of xqtvaAbort: exit; end;
  for i := 0 to high(infoVars) do
    visitor.declare(PXQTermVariable(@infoVars[i]));

  for i := 0 to high(catches) do begin
    result := visitor.simpleTermVisit(@catches[i].expr, self);
    case result of xqtvaAbort: break; end;
  end;
  for i := high(infoVars) downto 0 do
    visitor.undeclare(PXQTermVariable(@infoVars[i]));
end;

function TXQTermTryCatch.clone: TXQTerm;
var
  i: Integer;
begin
  result := TXQTermTryCatch.create(body);
  TXQTermTryCatch(result).catches := catches;
  SetLength(TXQTermTryCatch(result).catches, length(catches));
  for i := 0 to high(catches) do
    TXQTermTryCatch(result).catches[i].expr := catches[i].expr.clone;
end;

destructor TXQTermTryCatch.Destroy;
var
  i: Integer;
  j: Integer;
begin
  body.free;
  for i := 0 to high(catches) do begin
    for j := 0 to high(catches[i].tests) do
      catches[i].tests[j].name.free;
    catches[i].expr.free;
  end;
  for i := 0 to high(infoVars) do
    infoVars[i].free;
  inherited Destroy;
end;

