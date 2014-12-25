{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
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
      SetLength(params, length(params));
      for j := 0 to high(params) do
        params[j] := params[j].clone;
    end;
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
  raise EXQEvaluationException.Create(errcode, s);
end;

function TXQTerm.toQueryCommand: TXQPathMatchingStep;
begin
  result.typ:=qcFunctionSpecialCase;
  result.specialCase:=self;
end;

procedure TXQTerm.addToQueryList(var path: TXQPathMatching);
begin
  SetLength(path, length(path) + 1);
  path[high(path)] := toQueryCommand;
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

procedure TXQTermWithChildren.evaluateChildren(const context: TXQEvaluationContext; out results: TXQVArray);
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
  if length(children) = 0 then exit(xqtvaContinue);
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




{ TXQTermReadProperty }

constructor TXQTermReadObjectProperty.create(apropname: string);
begin
  propname := apropname;
end;

function TXQTermReadObjectProperty.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  obj, v: IXQValue;
begin
  if length(children) = 0 then raiseEvaluationError('pxp:OBJ', 'Found no term to read property from');
  if length(children) > 1 then raiseEvaluationError('pxp:OBJ', 'Can only read property from one term');
  obj:=children[0].evaluate(context);

  result := nil;
  for v in obj do
    if v is TXQValueObject then
       xqvalueSeqAdd(result, v.getProperty(propname));
  if result = nil then result := xqvalue();
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

function TXQTermIf.evaluate(const context: TXQEvaluationContext): IXQValue;
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

function TXQTermFilterSequence.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if length(children) <> 2 then raiseEvaluationError('XPST0003', 'Invalid [] sequence filter');
  if children[0] is TXQTermNodeMatcher then begin
    result := TXQueryEngine.evaluateAccessList(self, context);
  end else begin
    result := children[0].evaluate(context);
    TXQueryEngine.filterSequence(result, children[1], context);
    if result = nil then result := xqvalue()
    else xqvalueSeqSqueeze(result);
  end;
end;

function TXQTermFilterSequence.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  if length(children) = 0 then exit;
  result := children[0].getContextDependencies;
  for i := 1 to high(children) do
    result += children[i].getContextDependencies - [xqcdFocusDocument, xqcdFocusOther]; //since the focus are the sequence values there are no dependancies on the old focus
end;

function TXQTermFilterSequence.toQueryCommand: TXQPathMatchingStep;
begin
  if length(children) <> 2 then raiseEvaluationError('XPST0003', 'invalid [] construct');
  result := children[0].toQueryCommand;
  setlength(result.filters, length(result.filters)+1);
  result.filters[high(result.filters)] := children[1];
end;

procedure TXQTermFilterSequence.addToQueryList(var path: TXQPathMatching);
begin
  if length(children) <> 2 then raiseEvaluationError('XPST0003', 'Invalid [] sequence filter');

  children[0].addToQueryList(path);
  setlength(path[high(path)].filters, length(path[high(path)].filters) + 1);
  path[high(path)].filters[high(path[high(path)].filters)] := children[1];
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
      else if (t is TXQTermNodeMatcher) or (t is TXQTermVariable) then
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

function TXQTermDefineVariable.evaluate(const context: TXQEvaluationContext): IXQValue;
var varname: string;
    varnamespace: string;
    properties: TStringArray; //properties or array elements
    v: TXQTerm;
    assignmentMode: (amOverride, amArrayOverride, amArrayInsert, amArrayAppend);
    arrayIndex: integer;
    value, temp: IXQValue;

    seq: TXQValueSequence;

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
      varnamespace := TXQTermVariable(v).namespaceURL;
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
      if not context.hasVariable(varname, value, varnamespace) then value := xqvalue;
      applyProperties;
      if (arrayIndex = 1) and (value.getSequenceCount <= 1) then value := result
      else if arrayIndex > value.getSequenceCount then xqvalueSeqAdd(value, result)
      else begin
        seq := TXQValueSequence.create(max(arrayIndex, value.getSequenceCount));
        if arrayIndex = 0 then seq.seq.add(result);
        for temp in value do begin
          dec(arrayIndex);
          if arrayIndex <> 0 then seq.seq.add(temp)
          else begin
            if assignmentMode = amArrayInsert then
              seq.seq.add(temp);
            seq.seq.add(result);
          end;
        end;
        if arrayIndex > 0 then seq.seq.add(result);
        value := seq;
      end;
    end;
    amArrayAppend:
      if not context.hasVariable(varname, value, varnamespace) then
        value := Result
      else begin
       applyProperties;
       xqvalueSeqAdd(value, result);
     end;
  end;

  if length(properties) = 0 then //test is unnessesary, as it should work with only the last branch. But being explicit looks better
    context.staticContext.sender.VariableChangelog.add(varname, value, varnamespace)
   else
    context.staticContext.sender.VariableChangelog.addObjectModification(varname, value, varnamespace, properties)


  (*exit;

  variable.;

  //this is never called for true XQuery variables, they are handled from the module term.
  if not assigned(context.staticContext.sender.OnDefineVariable) then raiseEvaluationError('XPST0001', ':= can''t be used without a method assigned to the OnDefineVariable event');

  if namespace <> nil then context.staticContext.sender.OnDefineVariable(self, namespace.getPrefix + ':' + variablename, result)
  else context.staticContext.sender.OnDefineVariable(self, variablename, result);*)
end;

function TXQTermDefineVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
  if length(children) > 0 then result := children[high(children)].getContextDependencies;
  result += [xqcdContextVariables];
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

destructor TXQTermDefineVariable.destroy;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to high(annotations) do for j := 0 to high(annotations[i].params) do
    FreeAndNil(annotations[i].params[j]);
  variable.Free;
  inherited destroy;
end;

{ TXQTermType }

constructor TXQTermSequenceType.create();
begin
end;

constructor TXQTermSequenceType.create(atomic: TXSType);
begin
  kind := tikAtomic;
  atomicTypeInfo := atomic;
end;

destructor TXQTermSequenceType.destroy;
var
  i: Integer;
begin
  for i := 0 to high(arguments) do arguments[i].free;
  inherited destroy;
end;


function TXQTermSequenceType.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := TXQValueFunction.create(self); //return a function returning this term. This allows returning the type itself, without having a "type"-type in the type system.
end;

function TXQTermSequenceType.serialize: string;
begin
  result := name;
  if Result = '' then
    case kind of
      tikNone: result := 'empty-sequence()';
      tikAny: result := 'item()*';
      tikAtomic: result := 'xs:'+atomicTypeInfo.name;
      tikFunctionTest: result := 'function';
      tikElementTest: result := 'node()';
    end;
  //incomplete
end;

function TXQTermSequenceType.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;

function TXQTermSequenceType.isSingleType(): boolean;
begin
  result := (kind = tikAtomic) and not allowMultiple;
end;

function TXQTermSequenceType.castableAsBase(v: IXQValue; staticContext: TXQStaticContext): boolean;
begin
  if v.isUndefined then result := allowNone
  else if v.getSequenceCount > 1 then result := false
  else if v is TXQValueSequence then result := atomicTypeInfo.tryCreateValue((v as TXQValueSequence).seq[0]) = xsceNoError
  else if atomicTypeInfo.storage <> TXQValueQName then result := atomicTypeInfo.tryCreateValue(v) = xsceNoError
  else result := staticQNameCast(true, atomicTypeInfo as TXSQNameType, v, staticContext).toBoolean;
end;

function TXQTermSequenceType.castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
begin
  v := xqvalueAtomize(v);
  if (v.isUndefined and not allowNone) or (v.getSequenceCount > 1) then
    raiseEvaluationError('XPTY0004', 'impossible cast as');
  if atomicTypeInfo.storage <> TXQValueQName then result := atomicTypeInfo.createValue(v)
  else result := staticQNameCast(false, atomicTypeInfo as TXSQNameType, v, context.staticContext)
end;

function TXQTermSequenceType.castableAs(v: IXQValue; staticContext: TXQStaticContext): boolean;
begin
  if kind <> tikAtomic then raiseEvaluationError('XPST0051', 'need atomic type for castable as');
  v := xqvalueAtomize(v);
  result := castableAsBase(v, staticContext);
end;

function TXQTermSequenceType.instanceOf(ta: IXQValue; const context: TXQEvaluationContext): boolean;
var nodeCondition: TXQPathNodeCondition;

  function instanceOfSingleType(sub: IXQValue): boolean;
  var
    t: TXSType;
    fun: TXQValueFunction;
    i: Integer;
  begin
    case kind of
      //tikAny, tikNone: ; //handled before
      tikAtomic: begin
        t := sub.typeAnnotation;;
        if t.derivedFrom(atomicTypeInfo) then exit(true);
        if atomicTypeInfo is TXSUnionType then exit(TXSUnionType(atomicTypeInfo).containsTransitive(t));
        exit(false);
      end;
      tikElementTest: result  := TXQueryEngine.nodeMatchesQueryLocally(nodeCondition, sub.toNode);
      tikFunctionTest: begin
        if not (sub is TXQValueFunction) then exit(false);
        if length(arguments) = 0 then exit(true);
        fun := sub as TXQValueFunction;
        if (length(arguments) <> length(fun.parameters) + 1) then exit(false);
        for i := 0 to high(fun.parameters) do
          if (fun.parameters[i].seqtype <> nil) and not arguments[i].subtypeOf(fun.parameters[i].seqtype) then exit(false);
        if fun.resulttype <> nil then result := fun.resulttype.subtypeOf(arguments[high(arguments)])
        else result := arguments[high(arguments)].isItemStar();
      end;
    end;
  end;
var
  count: Integer;
  v: IXQValue;
begin
  //compare count with sequence count
  if kind = tikNone then exit(ta.isUndefined);
  count := ta.getSequenceCount;
  if ((count = 0) and not allowNone) or
     ((count > 1) and not allowMultiple) then
    exit(false);
  if (count = 0) then
    exit(true);
  //compare item type
  if kind = tikAny then
    exit(true);
  if kind = tikElementTest then begin
    TXQueryEngine.unifyQuery(nil, self.nodeMatching, nodeCondition);
    if qmCheckNamespacePrefix in self.nodeMatching.matching then begin
      include(nodeCondition.options, xqpncCheckNamespace);
      if qmAttribute in nodeMatching.matching then nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkUnknown)
      else nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkElementType);
    end;
    nodeCondition.equalFunction := @context.staticContext.nodeCollation.equal;
  end;
  result := true;
  for v in ta do begin
    result := result and instanceOfSingleType(v);
    if not result then exit;
  end;
end;

function TXQTermSequenceType.instanceOf(const ta: IXQValue): boolean;
var temp: TXQEvaluationContext;
begin
  temp.staticContext := nil;
  result := instanceOf(ta, temp);
end;

function TXQTermSequenceType.subtypeOf(tb: TXQTermSequenceType): boolean;
begin
  if kind = tikNone then exit((tb.kind = tikNone) or tb.allowNone);
  if tb.kind = tikNone then exit(false);
  if (allowNone and not tb.allowNone) or (allowMultiple and not tb.allowMultiple) then exit(false);
  result := subtypeItemTypeOf(tb);
end;

class function TXQTermSequenceType.staticQNameCast(castableOnly: boolean; target: TXSQNameType; const v: IXQValue; context: TXQStaticContext
  ): ixqvalue;
var
  castAllowed: Boolean;
  namespace: INamespace;
  s: String;
begin
  if v.typeAnnotation.storage = TXQValueQName then begin
    if castableOnly then exit(xqvalueTrue)
    else exit(target.createValue(v));
  end;
  if not (context.model in [xqpmXPath3, xqpmXQuery3]) then
    if castableOnly then exit(xqvalueFalse)
    else raise EXQEvaluationException.create('FORG0001', 'Non literal string to QName cast: '+v.debugAsStringWithTypeAnnotation());
  if v.kind <> pvkString then
    if castableOnly then exit(xqvalueFalse)
    else raise EXQEvaluationException.create('FORG0001', 'Invalid type for cast: '+v.debugAsStringWithTypeAnnotation());
  s := trim(v.toString);
  castAllowed := (target <> baseSchema.NOTATION) and (baseSchema.QName.qnameRegex.Exec(s));
  if castableOnly then  exit(xqvalue(castAllowed));
  if not castAllowed then
    raise EXQEvaluationException.create('FORG0001', 'Invalid string for cast: '+v.debugAsStringWithTypeAnnotation());
  if pos(':', s) > 0 then begin
    namespace := context.findNamespace(strSplitGet(':', s), xqdnkElementType);
    if namespace = nil then raise EXQEvaluationException.create('FONS0004', 'Failed to find namespace of: '+v.toString);
  end else namespace := context.findNamespace('', xqdnkElementType);
  result := TXQValueQName.create(target, namespace, s);
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
begin
  //function coercion f(v, w, ..)=>s  passed to f(a, b, ..)=>r becomes f(a intersect v, b intersect w, ..)=>r intersect s
  //if v.kind <> pvkFunction then raise EXQEvaluationException.Create('XPTY0004', 'Expected function, but got : '+result.debugAsStringWithTypeAnnotation());
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
    if (not arguments[high(arguments)].isItemStar) and (((fun.resulttype = nil) or not fun.resulttype.subtypeOf(arguments[i])))
        then
          needCoercion := true;
  if not needCoercion then exit(v);

  //see TXQTermDefineFunction.defineDynamicPartialApplication
  f := fun;
  fresult := TXQValueFunction.create();
  fresult.namespace := f.namespace;
  fresult.name:=f.name;
  copyAnnotations(fresult.annotations, f.annotations);
  fresult.body := TXQTermDynamicFunctionCall.create(TXQTermConstant.Create(f));
  setlength(fresult.parameters, length(arguments) - 1);
  for i := 0 to high(f.parameters) do begin
    TXQTermDynamicFunctionCall(fresult.body).push(TXQTermVariable.create(IntToStr(i)+'.'));
    fresult.parameters[i].variable := TXQTermVariable(TXQTermDynamicFunctionCall(fresult.body).children[high(TXQTermDynamicFunctionCall(fresult.body).children)].clone);
    fresult.parameters[i].seqtype := TXQTermSequenceType(arguments[i].clone);
  end;
  fresult.ownsTerms := true;
  fresult.resulttype := TXQTermSequenceType(arguments[high(arguments)].clone);
  fresult.context := f.context;
  result := fresult;
end;

function TXQTermSequenceType.isItemStar: boolean;
begin
  result := (kind = tikAny) and allowMultiple and allowNone;
end;

function TXQTermSequenceType.clone: TXQTerm;
var
  other: TXQTermSequenceType;
begin
  result := inherited clone;
  other := TXQTermSequenceType(result);
  other.name := name;
  other.allowNone := allowNone;
  other.allowMultiple := allowMultiple;
  other.kind := kind;
  other.atomicTypeInfo := atomicTypeInfo;
  other.nodeMatching := nodeMatching;
end;

{ TXQTermSequence }

function TXQTermSequence.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  i: Integer;
  tempSeq: TXQValueSequence;
begin
  if length(children) = 0 then exit(xqvalue);
  if length(children) = 1 then exit(children[0].evaluate(context));
  tempSeq := TXQValueSequence.create(length(children));
  try
    for i := 0 to high(children) do
      tempSeq.addChild(children[i].evaluate(context));
    result := tempSeq;
  except
    tempSeq.free;
    raise;
  end;
end;

function TXQTermSequence.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  for i:= 0 to high(children) do result += children[i].getContextDependencies;
end;

{ TXQTermArray }

function TXQTermJSONArray.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  i: Integer;
  tempSeq: TXQValueJSONArray;
begin
  tempSeq := TXQValueJSONArray.create(length(children));
  for i := 0 to high(children) do
    tempSeq.addChild(children[i].evaluate(context));
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

function TXQTermSomeEvery.evaluate(const context: TXQEvaluationContext): IXQValue;
var tempContext:TXQEvaluationContext;
  function evaluateSomeEvery(pos:integer): boolean;
  var v: IXQValue;
      typeDelta: integer;
  begin
    if pos = high(children) then exit(children[pos].evaluate(tempContext).toBooleanEffective);
    if not (children[pos] is TXQTermVariable) then raiseEvaluationError('err:XPST0003', 'Expected variable, but got '+children[pos].debugTermToString);
    if children[pos + 1] is TXQTermSequenceType then typeDelta:=1
    else typeDelta:=0;

    result := isEvery;
    for v in children[pos+1+typeDelta].evaluate(tempContext) do begin
      if typeDelta <> 0 then
        if not TXQTermSequenceType(children[pos+typeDelta]).instanceOf(v, context) then
          raiseEvaluationError('XPTY0004', 'Invalid type for variable '+ v.toString+ ' expected ' + TXQTermSequenceType(children[pos+typeDelta]).serialize);
      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.add(TXQTermVariable(children[pos]).value, v, TXQTermVariable(children[pos]).namespaceURL);
      if evaluateSomeEvery(pos+2+typeDelta) <> (isEvery) then begin
        tempContext.temporaryVariables.popAll;
        result := not isEvery;
        break;
      end;
      tempContext.temporaryVariables.popAll;
    end;
  end;

begin
  tempContext:=context;
  if context.temporaryVariables=nil then begin
    tempContext.temporaryVariables:=TXQVariableChangeLog.create();
  end;
  try
    result:=xqvalue(evaluateSomeEvery(0));
    xqvalueSeqSqueeze(result);
  finally
    if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables); //iff context.temporaryVariables = nil, we created the change log here
  end;
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
    if children[pos + 1] is TXQTermSequenceType then typeDelta:=1
    else typeDelta:=0;

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

type TOrderedTuple = class
  orders: array of IXQValue;
  result: IXQValue;
end;
  TFlowerSortingData = record
    flower: TXQTermFlower;
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
    temp := meta^.context^.compareAtomicBase(v,w) ;
    result := (temp <> -2) and (temp < 0);
  end;

var
  a, b: TOrderedTuple;
  orders: array of TXQTermFlowerOrder;
  i: Integer;
  emptyLeast: Boolean;
begin
  meta := PFlowerSortingData(data);
  orders := meta^.flower.orders;
  a := TOrderedTuple(ppointer(xa)^);
  b := TOrderedTuple(ppointer(xb)^);

  result := 0;

  for i := 0 to high(orders) do begin
    emptyLeast := (orders[i].emptyOrder = xqeoEmptyLeast) or ((orders[i].emptyOrder = xqeoStatic) and (meta^.context^.staticContext.emptyOrderSpec = xqeoEmptyLeast));
    if isGreaterThan(a.orders[i], b.orders[i], emptyLeast, meta^.collations[i]) then result := 1
    else if isGreaterThan(b.orders[i], a.orders[i], emptyLeast, meta^.collations[i]) then result := -1;
    if orders[i].descending then result := -Result;
    if result <> 0 then exit;
  end;
end;

function TXQTermFlower.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  tempContext: TXQEvaluationContext;
  sortingCache: TFPList;
  finalResult: IXQValue;

  procedure evaluateForLoop(const pos: Integer);
    procedure continueWithValue(v: IXQValue; index: integer = 1);
    begin
      if (vars[pos].sequenceTyp <> nil) and not (vars[pos].sequenceTyp.instanceOf(v, context)) then
        raiseEvaluationError('XPTY0004', 'Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+v.debugAsStringWithTypeAnnotation());

      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.add(vars[pos].loopvar.value, v, vars[pos].loopvar.namespaceURL);
      if vars[pos].positionVar <> nil then
        tempContext.temporaryVariables.add(vars[pos].positionVar.value, xqvalue(index), vars[pos].positionVar.namespaceURL);
      evaluateForLoop(pos+1);
      tempContext.temporaryVariables.popAll();

    end;

    procedure continueWithValues(log: TXQVariableChangeLog);
    var oldContextItem: IXQValue;
        contextValue: TXQValue;
        oldSeqIndex: Integer;
        oldSeqLength: Integer;
        oldLog: TXQVariableChangeLog;
        i: Integer;

        procedure pushContextItem;
        begin
          oldContextItem := tempContext.SeqValue;
          oldSeqIndex := tempContext.SeqIndex;
          oldSeqLength := tempContext.SeqLength;
        end;
        procedure popContextItem;
        begin
          tempContext.SeqValue := oldContextItem;
          tempContext.SeqIndex := oldSeqIndex;
          tempContext.SeqLength:= oldSeqLength;
        end;
        procedure replaceContextItem(const newItem: IXQValue);
        begin
          tempContext.SeqValue := newItem;
          tempContext.SeqIndex := 1;
          tempContext.SeqLength:= 1;
        end;

    begin
      try

        if vars[pos].kind = xqfkLet then begin
          oldLog := log;   log := log.condensedCollected;  oldLog.free;
          if (vars[pos].sequenceTyp <> nil) then
            for i := 0 to log.count - 1 do
              if not (vars[pos].sequenceTyp.instanceOf(log[i], context)) then
                raiseEvaluationError('XPTY0004', 'Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+log[i].debugAsStringWithTypeAnnotation());
          contextValue := nil;
          if log.hasVariable('$', @contextValue) then begin
            if contextValue.getSequenceCount <> 1 then raiseEvaluationError('pxp:PATTERN1', 'Only singletons can be assigned to .');
            replaceContextItem(contextValue);
            pushContextItem;
          end;
          oldLog := tempContext.temporaryVariables;
          log.parentLog := tempContext.temporaryVariables;
          tempContext.temporaryVariables := log;
          for i := 0 to high(vars[pos].pattern.vars) do
            if not log.hasVariable(vars[pos].pattern.vars[i].value, nil, vars[pos].pattern.vars[i].namespaceURL) then
              tempContext.temporaryVariables.add(vars[pos].pattern.vars[i].value, xqvalue(), vars[pos].pattern.vars[i].namespaceURL);
          try
            evaluateForLoop(pos+1);
          finally
            tempContext.temporaryVariables := oldLog;
            if contextValue <> nil then
              popContextItem;
          end;

        end else begin // for
          oldLog := log;   log := log.condensed;  oldLog.free;
          if (vars[pos].sequenceTyp <> nil) then
            for i := 0 to log.count - 1 do
              if not (vars[pos].sequenceTyp.instanceOf(log[i], context)) then
                raiseEvaluationError('XPTY0004', 'Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+log[i].debugAsStringWithTypeAnnotation());

          if vars[pos].positionVar <> nil then raiseEvaluationError('err:XPST0003', 'at and pattern cannot be combined');
               //tempContext.temporaryVariables.add(vars[pos].positionVarname, xqvalue(index), vars[pos].positionVarNamespace);
          pushContextItem;
          tempContext.temporaryVariables.pushAll;
          for i := 0 to high(vars[pos].pattern.vars) do
            tempContext.temporaryVariables.add(vars[pos].pattern.vars[i].value, xqvalue(), vars[pos].pattern.vars[i].namespaceURL);

          for i := 0 to log.count - 1 do begin
            if log.getName(i) = '$' then begin
              replaceContextItem(log[i]);
              evaluateForLoop(pos+1);
              popContextItem;
            end
            else begin
              tempContext.temporaryVariables.pushAll;
              tempContext.temporaryVariables.add(log.getName(i), log.get(i), log.vars[i].namespaceURL);
              evaluateForLoop(pos+1);
              tempContext.temporaryVariables.popAll();
            end;
          end;
          tempContext.temporaryVariables.popAll;
        end;
      finally
        log.free;
      end;
    end;

   var tempSeq: IXQValue;
       i:integer;
       tuple: TOrderedTuple;
   begin
     if pos = length(vars) then begin
       if (where <> nil) and not where.evaluate(tempContext).toBoolean then
         exit;

       if sortingCache = nil then begin
         xqvalueSeqAdd(finalResult, returned.evaluate(tempContext));
         exit;
       end else begin
         tuple := TOrderedTuple.Create;
         tuple.result := returned.evaluate(tempContext);
         setlength(tuple.orders, length(orders));
         for i:= 0 to high(orders) do
           tuple.orders[i] := orders[i].expr.evaluate(tempContext);
         sortingCache.Add(tuple);
         exit;
       end;
     end;

     tempSeq:=vars[pos].expr.evaluate(tempContext);

     if vars[pos].pattern = nil then begin
       if vars[pos].kind = xqfkLet then
         continueWithValue(tempSeq)
       else case tempSeq.kind of
         pvkUndefined:;
         pvkSequence:
           for i:=0 to (tempSeq as TXQValueSequence).seq.Count-1 do
             continueWithValue((tempSeq as TXQValueSequence).seq[i], i + 1);
         else continueWithValue(tempSeq);
       end;
     end else if vars[pos].kind = xqfkLet then
       continueWithValues(patternMatcherMatch(vars[pos].pattern.node, tempSeq.toNode, context, true))
     else case tempSeq.kind of
       pvkUndefined:;
       pvkSequence:
         for i:=0 to (tempSeq as TXQValueSequence).seq.Count-1 do
           continueWithValues(patternMatcherMatch(vars[pos].pattern.node, (tempSeq as TXQValueSequence).seq[i].toNode, context, true){, i + 1});
       else continueWithValues(patternMatcherMatch(vars[pos].pattern.node, tempSeq.toNode, context, true));
     end;
   end;
var i: Integer;
    sortingData: TFlowerSortingData;

begin
  tempContext:=context;
  if context.temporaryVariables=nil then begin
    tempContext.temporaryVariables:=TXQVariableChangeLog.create();
  end;

  //oldVarCount := tempContext.temporaryVariables.count;

  finalResult := nil;
  if length(orders) = 0 then sortingCache := nil
  else sortingCache := tfplist.Create;

  {oldlen := length(tempContext.temporaryVariables.vars);
  for i := 0 to high(vars) do begin
    tempContext.temporaryVariables.add(vars[i].varname, xqvalue(), vars[i].namespace);
    if vars[i].positionVarname <> '' then
      tempContext.temporaryVariables.add(vars[i].positionVarname, xqvalue(), vars[i].positionVarNamespace)
  end;}
  try
    evaluateForLoop(0);
    //setlength(tempContext.temporaryVariables.vars, oldlen);

    if sortingCache <> nil then begin
      if sortingCache.Count > 0 then begin
        sortingData.flower:=self;
        sortingData.context:=@tempContext;
        setlength(sortingData.collations, length(orders));
        for i := 0 to high(orders) do
          if orders[i].collation = '' then sortingData.collations[i] := nil
          else sortingData.collations[i] := TXQueryEngine.getCollation(orders[i].collation, context.staticContext.baseURI);
        stableSort(ppointer(sortingCache.List^), ppointer(sortingCache.List^) + (sortingCache.Count-1) , sizeof(pointer), @flowerTupleCompareFunction, TObject(@sortingData));
        for i := 0 to sortingCache.Count - 1 do begin
          xqvalueSeqAdd(finalResult, TOrderedTuple(sortingCache[i]).result);
          TOrderedTuple(sortingCache[i]).Free;
        end;
      end;
      FreeAndNil(sortingCache);
    end;

    if finalResult = nil then result := xqvalue()
    else result := finalResult;
    xqvalueSeqSqueeze(result);
  finally
    if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables); //iff context.temporaryVariables = nil, we created the change log here
    FreeAndNil(sortingCache);
  end;
end;

function TXQTermFlower.getContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := getChildrenContextDependencies;
  if where <> nil then result += where.getContextDependencies;
  if returned <> nil then Result += returned.getContextDependencies;
  if Result = ALL_CONTEXT_DEPENDENCIES then exit;
  for i:=0 to high(vars) do
    result += vars[i].expr.getContextDependencies;
  for i:=0 to high(orders) do
    result += orders[i].expr.getContextDependencies;
end;

function TXQTermFlower.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i, j: Integer;
begin
  for i := 0 to high(vars) do begin
    result := visitor.simpleTermVisit(@vars[i].expr, self);
    case result of xqtvaAbort: exit; end;

    visitor.parent := self;
    if vars[i].pattern = nil then begin
      visitor.declare(@vars[i].loopvar);
      if vars[i].positionVar <> nil then
        visitor.declare(@vars[i].positionVar);
    end else begin
      for j := 0 to high(vars[i].pattern.vars) do
        visitor.declare(@vars[i].pattern.vars[j]);
      visitor.simpleTermVisit(@vars[i].pattern, self);
    end;
  end;
  if where <> nil then begin
    result := visitor.simpleTermVisit(@where, self);
    case result of xqtvaAbort: exit; end;
  end;
  for i := 0 to high(orders) do begin
    result := visitor.simpleTermVisit(@orders[i].expr, self);
    case result of xqtvaAbort: exit; end;
  end;
  result := visitor.simpleTermVisit(@returned,self);
  case result of xqtvaAbort: exit; end;

  visitor.parent := self;
  for i := high(vars) downto 0 do begin
    if vars[i].pattern = nil then begin
      if vars[i].positionVar <> nil then
        visitor.undeclare(@vars[i].positionVar);
      visitor.undeclare(@vars[i].loopvar);
    end else for j := high(vars[i].pattern.vars) downto 0 do
      visitor.undeclare(@vars[i].pattern.vars[j]);
  end;
end;

function TXQTermFlower.clone: TXQTerm;
var
  i: Integer;
begin
  Result:=inherited clone;
  with TXQTermFlower(result) do begin
    vars := self.vars;
    setlength(vars, length(vars));
    for i := 0 to high(vars) do
      with vars[i] do begin
        if loopvar <> nil then loopvar := TXQTermVariable(loopvar.clone);
        if pattern <> nil then pattern := TXQTermPatternMatcher(pattern.clone);
        if positionVar <> nil then positionVar := TXQTermVariable(positionVar.clone);
        if expr <> nil then expr := expr.clone;
      end;
    if self.where <> nil then where := self.where.clone;
    orders := self.orders;
    SetLength(orders, length(orders));
    for i := 0 to high(orders) do
      orders[i].expr := orders[i].expr.clone;
    returned := self.returned.clone;
  end;
end;

destructor TXQTermFlower.destroy;
var
  i: Integer;
begin
  for i:=0 to high(vars) do begin
    vars[i].expr.Free;
    vars[i].sequenceTyp.Free;
    vars[i].pattern.Free;
    vars[i].loopvar.free;
    vars[i].positionVar.free;
  end;
  for i:= 0 to high(orders) do
    orders[i].expr.Free;
  where.Free;
  returned.Free;
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
constructor TXQTermNamedFunction.create(const anamespace, alocalname: string; arity: integer; const model: TXQParsingModel);
begin
  if not findKindIndex(anamespace, alocalname, arity, model, kind, func) then begin
    kind := xqfkUnknown;
    funcname:=alocalname;
    namespaceUrl := anamespace;
  end;
end;

constructor TXQTermNamedFunction.create(const anamespace, alocalname: string; args: array of TXQTerm; const model: TXQParsingModel);
begin
  create(anamespace, alocalname, length(args), model);
  push(args);
end;

function TXQTermNamedFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
//{$define checkreturntypes}  there is no point in testing the return type for native functions. but it might spot internal inconsistencies
var
  evilkids: TXQVArray;
  t: TXSType;
  {$ifdef checkreturntypes}version: Integer;{$endif}
begin
  evaluateChildren(context, evilkids);

  assert((kind = xqfkUnknown) or (func <> nil));

  if context.staticContext.strictTypeChecking and (kind in [xqfkBasic, xqfkComplex,xqfkNativeInterpreted]) then
    {$ifdef checkreturntypes}version := {$endif}func.checkOrConvertTypes(evilkids, context)
   {$ifdef checkreturntypes}
   else
    version := -1{$endif};



  case kind of
    xqfkBasic:
      result := TXQBasicFunctionInfo(func).func(evilkids);
    xqfkComplex:
      result := TXQComplexFunctionInfo(func).func(context, evilkids);
    xqfkWrappedOperator: begin
      if length(evilkids) <> 2 then raiseEvaluationError('XPST0003', 'Need two arguments for function: op:' + funcname);
      result := TXQOperatorInfo(func).func(context, evilkids[0], evilkids[1]);
    end;
    xqfkTypeConstructor: begin
      if length(evilkids) <> 1 then raiseEvaluationError('XPST0017', 'Invalid argument count for constructor function');
      case evilkids[0].getSequenceCount of
        0: result := xqvalue();
        1: begin
          t := TXSType(TObject(func));
          if t.storage <> TXQValueQName then result := TXSType(TObject(func)).createValue(evilkids[0])
          else result := TXQTermSequenceType.staticQNameCast(false, t as TXSQNameType, evilkids[0], context.staticContext);
        end
        else raiseEvaluationError('FORG0001', 'Need singleton value to cast as '+TXSType(TObject(func)).name);
      end;
    end;
    xqfkUnknown, xqfkNativeInterpreted: begin
      init(context);
      result := interpretedFunction.evaluate(evilkids);
    end;
  end;

  {$ifdef checkreturntypes}
  if version <> -1 then begin
    if not func.versions[version].returnType.instanceOf(result, context) then
      raiseEvaluationError('XPTY0004', 'Invalid return value, '+result.debugAsStringWithTypeAnnotation()+' does not have type '+func.versions[version].returnType.serialize);
  end;
  {$endif}
end;

function TXQTermNamedFunction.getContextDependencies: TXQContextDependencies;

begin
  case kind of
    xqfkBasic:             result := getChildrenContextDependencies + []; //all basic functions are pure
    xqfkComplex:           result := getChildrenContextDependencies + TXQComplexFunctionInfo(func).contextDependencies;
    xqfkWrappedOperator:   result := getChildrenContextDependencies + TXQOperatorInfo(func).contextDependencies;
    xqfkNativeInterpreted: result := getChildrenContextDependencies + TXQInterpretedFunctionInfo(func).contextDependencies;
    xqfkTypeConstructor:   result := getChildrenContextDependencies;
    else                   Result := ALL_CONTEXT_DEPENDENCIES;
  end;
end;

procedure TXQTermNamedFunction.assignWithoutChildren(source: TXQTermNamedFunction);
begin
  namespaceUrl:=source.namespaceUrl;
  kind:=source.kind;
  func:=source.func;
  funcname:=source.funcname;
  //todo: also clone interpretedFunction?
end;

function TXQTermNamedFunction.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermNamedFunction(result).assignWithoutChildren(self);
end;

class function TXQTermNamedFunction.findKindIndex(const anamespace, alocalname: string; const argcount: integer; const model: TXQParsingModel; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
var
  aindex: Integer;
  module: TXQNativeModule;
  t: TXSType;
begin
  module := TXQueryEngine.findNativeModule(anamespace);

  if (module = nil) then exit(false);

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
      akind := xqfkWrappedOperator;
      exit(true);
    end;
  end;

  if argcount = 1 then begin
    t:=baseSchema.findType(alocalname);
    if t <> nil then begin
      akind:=xqfkTypeConstructor;
      afunc := TXQAbstractFunctionInfo(TObject(t));
      exit(true)
    end;
  end;

  exit(false);
end;

procedure TXQTermNamedFunction.init(const context: TXQEvaluationContext);
  function suggestions: string;
  function strSimilar(const s, ref: string): boolean;
  begin
    result := strContains(s, ref) or strContains(ref, s)
              or (strSimilarity(funcname, ref) <= min(5, min(length(s) div 2, length(ref) div 2)));
  end;
  var
    module: TXQNativeModule;
    i: Integer;
  begin
    result := '';
    module := TXQueryEngine.findNativeModule(namespaceUrl);
    while module <> nil do begin
      for  i := 0 to module.basicFunctions.Count - 1 do
        if strSimilar(funcname, module.basicFunctions[i]) then
          result += module.basicFunctions[i]+' ';
      for i := 0 to module.complexFunctions.Count - 1 do
        if strSimilar(funcname, module.complexFunctions[i]) then
          result += module.complexFunctions[i]+' ';
      module := module.parent;
    end;
    if result <> '' then result := LineEnding +'Did you mean: '+ result;
  end;
var
  i: Integer;
  f: TXQValueFunction;
begin
  if not (kind in [xqfkNativeInterpreted, xqfkUnknown]) or (interpretedFunction <> nil) then exit;
  case kind of
    xqfkUnknown: begin
      functionStaticContext := context.findModuleStaticContext(namespaceUrl);
      if functionStaticContext <> nil then
        for i := 0 to high(functionStaticContext.functions) do begin
          f :=  functionStaticContext.functions[i];
          if (f.name = funcname)
             and (length(f.parameters) = length(children))
             and equalNamespaces(namespaceGetURL(f.namespace), namespaceUrl) then begin
            interpretedFunction := f;
            break;
          end;
        end;
      if interpretedFunction <> nil then begin
        if functionStaticContext <> context.staticContext {not equalNamespaces(vfunc.namespace, context.staticContext.moduleNamespace)} then
          for i := 0 to high(interpretedFunction.annotations) do
            if (interpretedFunction.annotations[i].name = 'private') and (interpretedFunction.annotations[i].namespace = XMLNamespaceUrl_XQuery ) then
              raiseParsingError('XPST0017', interpretedFunction.name + ' is private');
      end else begin
        findKindIndex(namespaceUrl, funcname, length(children), context.staticContext.model, kind, func);
        if kind <> xqfkUnknown then exit();
        raiseEvaluationError('XPST0017', 'Function Q{'+namespaceUrl+'}'+ funcname+'#'+IntToStr(length(children))+ ' not found. '+suggestions);
      end;
    end;
    xqfkNativeInterpreted: begin
      if TXQInterpretedFunctionInfo(func).func = nil then
        TXQInterpretedFunctionInfo(func).initialize();
      interpretedFunction :=  TXQInterpretedFunctionInfo(func).func;
      functionStaticContext := context.staticContext;
    end;
  end;
end;

{ TXQDynamicFunctionCall }

constructor TXQTermDynamicFunctionCall.create(func: TXQTerm; arg: TXQTerm);
begin
  if func <> nil then begin
    push(func);
    if arg <> nil then push(arg);
  end;
end;

function TXQTermDynamicFunctionCall.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  func, v: IXQValue;
  index: IXQValue;
  ara: TXQValueJSONArray;
  i: Integer;
  sl: TStringList;
  evilkids: TXQVArray;
begin
  //if length(children) < 2 then raiseEvaluationError('XPST0003', 'Only one argument given, need function AND arguments');
  func := children[0].evaluate(context);
  if func.isUndefined then
    if AllowJSONDefaultInternal then exit(xqvalue())
    else raise EXQEvaluationException.create('XPTY0004', 'Cannot use an empty sequence as function, if JSONiq is disabled');
  if (func is TXQValueSequence) and (func.getSequenceCount = 1) then func := func.getChild(0);

  if func is TXQValueFunction then begin
    setlength(evilkids, length(children)-1);
    for i:=1 to high(children) do
      evilkids[i-1] := children[i].evaluate(context);
    exit((func as TXQValueFunction).evaluate(evilkids));
  end;

  if length(children) = 1 then begin
    result := nil;
    for v in func do begin
      if v is TXQValueObject then begin
        sl := TStringList.Create;
        (v as TXQValueObject).enumerateKeys(sl);
        xqvalueSeqAdd(result, xqvalue(sl));
        sl.free;
      end else if v is TXQValueJSONArray then begin
        ara := v as TXQValueJSONArray;;
        for i := 0 to ara.seq.Count-1 do
          xqvalueSeqAdd(result, ara.seq[i]);
      end else if not AllowJSONDefaultInternal then raise EXQEvaluationException.create('XPTY0004', 'Invalid type for function call: '+v.debugAsStringWithTypeAnnotation());
    end;
    if result = nil then result := xqvalue();
    exit;
  end;

  if Length(children) <> 2 then raiseEvaluationError('JNTY0018', 'Multiple array indices or object prop given');
  index := children[1].evaluate(context);
  result := nil;
  for v in func do begin
    if v is TXQValueJSONArray then begin
      if (index.toInt64 < 1) or (index.toInt64 > (v as TXQValueJSONArray).seq.Count) then continue;
      xqvalueSeqAdd(result, (v as TXQValueJSONArray).seq[index.toInt64-1]);
    end else if v is TXQValueObject then begin
      xqvalueSeqAdd(result, v.getProperty(index.ToString));
    end else if not AllowJSONDefaultInternal then //not an error since JSONiq 1.0.1
    //end else
      raiseEvaluationError('XPTY0004', 'Invalid call, expected function, object or array, got: '+func.debugAsStringWithTypeAnnotation());
  end;
  //this returns a sequence of a single value instead that value, if func is a sequence with multiple values (from which only one has that property). Does it matter?
  if result = nil then result := xqvalue();

end;

function TXQTermDynamicFunctionCall.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;




{ TXQTermBinaryOp }

constructor TXQTermBinaryOp.create(const aop: string; arg1: TXQTerm; arg2: TXQTerm);
begin
  op := TXQueryEngine.findOperator(pchar(aop));
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

function TXQTermBinaryOp.evaluate(const context: TXQEvaluationContext): IXQValue;
  function evaluateSimpleMap: IXQValue;
  var
    tempContext: TXQEvaluationContext;
    seq: IXQValue;
  begin
    result := nil;
    seq := children[0].evaluate(context);
    tempContext := context;
    tempContext.ParentElement := nil;
    tempContext.SeqIndex := 1;
    tempContext.SeqLength := seq.getSequenceCount;
    for tempContext.SeqValue in seq do begin
      xqvalueSeqAdd(result, children[1].evaluate(tempContext));
      tempContext.SeqIndex += 1;
    end;
    if result = nil then result := xqvalue();
  end;

var evilkids: TXQVArray;
begin
  if length(children)<2 then raiseEvaluationError('XPST0003', 'Not enough subterms');
  if length(children)>2 then raiseEvaluationError('XPST0003', 'Too many subterms');


  case op.name of
    '/', '//': exit(TXQueryEngine.evaluateAccessList(self, context));
    '!': exit(evaluateSimpleMap);
  end;



  if not context.staticContext.strictTypeChecking  then
    result := op.func(context, children[0].evaluate(context), children[1].evaluate(context))
  else begin
    evaluateChildren(context, evilkids);
    op.checkOrConvertTypes(evilkids, context);
    result := op.func(context, evilkids[0], evilkids[1]);
  end;
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

procedure TXQTermBinaryOp.addToQueryList(var path: TXQPathMatching);
var
  isDoubleSlash: Boolean;
  last: Integer;
begin
  isDoubleSlash := op.name = '//';
  if  not (isDoubleSlash or (op.name = '/')) then begin
    inherited addToQueryList(path);
    exit;
  end;

  children[0].addToQueryList(path);
  if isDoubleSlash then begin
    setlength(path, length(path) + 1);
    path[high(path)].typ:=qcSameOrDescendant;
    path[high(path)].matching:=[qmDocument, qmElement, qmText, qmComment, qmProcessingInstruction];
  end;
  children[1].addToQueryList(path);

  //optimization
  //two descendants following each other can be condensed to a single descendant (but this changes the indices of the sequence, so it only work if there are no numeric filters)
  last := high(path);
  if (path[last - 1].typ = qcSameOrDescendant) and (path[last - 1].matching = [qmDocument, qmElement, qmText, qmComment, qmProcessingInstruction]) and (length(path[last - 1].filters) = 0)
      and (path[last].typ in [qcDirectChild, qcDescendant]) and (length(path[last].filters) = 0) then begin
    path[last - 1] := path[last];
    path[last - 1].typ := qcDescendant;
    SetLength(path, last);
  end;
end;

{ TXQTermNodeMatcher }

constructor TXQTermNodeMatcher.Create;
begin

end;

constructor TXQTermNodeMatcher.Create(const avalue: string; asfunction: boolean);
begin
  select := avalue;
  if strContains(select,'::') then axis:=strSplitGet('::',select);
  if strContains(select,':') then begin
    namespaceURLOrPrefix:=strSplitGet(':',select);
    if namespaceURLOrPrefix = '*' then namespaceCheck := xqnmNone
    else namespaceCheck := xqnmPrefix;
  end else if select <> '*' then namespaceCheck := xqnmPrefix
  else namespaceCheck := xqnmNone;
  func := asfunction;
end;

constructor TXQTermNodeMatcher.Create(const aaxis: string; const anamespaceMode: TXQNamespaceMode; const anamespaceUrlOrPrefix, aLocalPart: string);
begin
  axis := aaxis;
  namespaceCheck := anamespaceMode;
  namespaceURLOrPrefix := anamespaceUrlOrPrefix;
  select := aLocalPart;
end;

function TXQTermNodeMatcher.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if (select = '.') and (context.SeqValue <> nil) then result := context.SeqValue
  else if func and (context.SeqValue = nil) and (axis = '') and (select = 'text') and (context.TextElement<>nil) then
    result := xqvalue(context.TextElement)
  else if axis <> 'attribute' then begin
    result := TXQueryEngine.evaluateSingleStepQuery(toQueryCommand,context);
    xqvalueSeqSqueeze(result);
  end else result := evaluateAttribute(context);
end;

function TXQTermNodeMatcher.evaluateAttribute(const context: TXQEvaluationContext): IXQValue;
var
  tempEl: TTreeNode;
  testValue: Boolean;
  readvalue: String;
  readnamespace: String;
  temp: INamespace;
  i: Integer;
  attrib: TTreeAttribute;
  nsCheck: TXQNamespaceMode;
begin
  tempEl := nil;
  if context.SeqValue <> nil then begin
    if context.SeqValue is TXQValueNode then tempEl := context.SeqValue.toNode
  end else if context.ParentElement <> nil then tempEl := context.ParentElement
  else raise EXQEvaluationException.create('XPDY0002', 'Need context item to read attributes');
  if tempEl = nil then
    raise EXQEvaluationException.create('XPTY0020', 'Need node as context item to read attributes');
  if (tempEl.typ = tetProcessingInstruction) or (tempel.attributes = nil) then
    exit(xqvalue);

  if length(children) > 0 then begin
    if not (children[0] is TXQTermNodeMatcher) then raiseEvaluationError('XPST0003', 'Invalid subtype');
    readvalue := TXQTermNodeMatcher(children[0]).select;
    nsCheck := TXQTermNodeMatcher(children[0]).namespaceCheck;
    readnamespace := TXQTermNodeMatcher(children[0]).namespaceURLOrPrefix;
  end else begin
    if func then readvalue := '*'
    else readvalue := select;
    nsCheck := namespaceCheck;
    readnamespace := namespaceURLOrPrefix;
  end;
  if nsCheck = xqnmPrefix then begin
    nsCheck := xqnmURL;
    temp := context.findNamespace(readnamespace, xqdnkUnknown);
    if temp <> nil then readnamespace:=temp.getURL
    else if readnamespace <> '' then begin
      if not context.staticContext.useLocalNamespaces then raiseEvaluationError('XPST0081', 'Unknown namespace prefix: '+readnamespace+' for reading attribute: '+readvalue);
      if (context.SeqValue<>nil) and (context.SeqValue.toNode <> nil) then readnamespace:=context.SeqValue.toNode.getNamespaceURL(readnamespace)
      else readnamespace:='';
      if readnamespace = '' then exit(xqvalue); //do not raise an exception if the namespace is unknown and the local namespace extension is enabled (so you can use unknown namespaces in css)
    end;
  end;

  result := nil;

  testValue := readvalue <> '*';
  for i := 0 to tempel.attributes.Count - 1 do begin
    attrib := tempel.attributes.items[i];
    if      (not testValue or striEqual(attrib.value, readvalue))
        and ((nsCheck <> xqnmURL) or ( attrib.getNamespaceURL() = readnamespace))
        and not attrib.isNamespaceNode
        then
      xqvalueSeqAdd(result, xqvalue(tempEl.attributes.Items[i]));
  end;
  if result = nil then result:=xqvalue();
end;


function TXQTermNodeMatcher.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusOther, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
end;

function TXQTermNodeMatcher.debugTermToString: string;
begin
  result := '';
  if axis <> '' then result += axis + '::';
  case namespaceCheck of
    xqnmNone: result += '*:';
    xqnmPrefix: result += namespaceURLOrPrefix + ':';
    xqnmURL: result += 'Q{'+namespaceURLOrPrefix+'}';
  end;
  result += select;
end;

function TXQTermNodeMatcher.clone: TXQTerm;
var
  other: TXQTermNodeMatcher;
begin
  Result:=inherited clone;
  other := TXQTermNodeMatcher(result);
  other.axis := axis;
  other.namespaceURLOrPrefix := namespaceURLOrPrefix;
  other.select := select;
  other.namespaceCheck := namespaceCheck;
  other.func := func;
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
     raise EXQEvaluationException.Create('XPST0010', 'Unknown axis: '+axis);
  end;

begin
  if axis = 'attribute' then exit(inherited);
  if func then begin
    result := convertElementTestToPathMatchingStep(select, children);
    result.typ:=getAxis(axis);
    exit;
  end;
  result.requiredType := nil;
  case select of
    '/': result.typ := qcDocumentRoot;
    '.': begin
      result.typ:=qcSameNode; //same node /./
      result.matching:=[qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument];
    end;
    '..': begin
      result.typ:=qcDirectParent; //parent /../
      result.matching:=[qmDocument, qmElement];
    end;
    '*': begin
      result.typ:=getAxis(axis); //any child /*/
      result.matching:=[qmElement,qmAttribute];
      assignNamespaceToMatchingStep(result);
    end;
    else begin
      result.typ := getAxis(axis);
      result.matching := [qmValue,qmElement];
      result.value:=select;
      assignNamespaceToMatchingStep(result);
    end;
  end;
end;

procedure TXQTermNodeMatcher.assignNamespaceToMatchingStep(var step: TXQPathMatchingStep);
begin
  if namespaceCheck <> xqnmNone then begin
    step.namespaceURLOrPrefix:=namespaceURLOrPrefix;
    case namespaceCheck of
      xqnmPrefix: Include(step.matching, qmCheckNamespacePrefix);
      xqnmURL: Include(step.matching, qmCheckNamespaceURL);
    end;
  end;
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

function TXQTermConstant.evaluate(const context: TXQEvaluationContext): IXQValue;
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

{ TXQTermVariable }

constructor TXQTermVariable.create(const avalue: string; staticContext: TXQStaticContext);
begin
  value := avalue;
  if value = '' then raiseEvaluationError('XPST0003', 'variable with empty name');
  //if value[1] = '$' then raiseEvaluationError('variable name starts with a double $:  ' + value);
  if value[length(value)] = ';' then delete(value, length(value), 1);
  if staticContext <> nil then staticContext.splitRawQName(namespaceURL, value, xqdnkUnknown);
end;

constructor TXQTermVariable.create(const avalue: string; const anamespace: INamespace);
begin
  value := avalue;
  namespaceURL := namespaceGetURL(anamespace);
end;

constructor TXQTermVariable.create(const alocalname: string; const anamespace: string);
begin
  value := alocalname;
  namespaceURL:= anamespace;
end;

function TXQTermVariable.equalsVariable(v: TXQTermVariable): boolean;
begin
  result := (v.value = value) and equalNamespaces(namespaceURL, v.namespaceURL);
end;

function TXQTermVariable.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  result := context.getVariable(value, namespaceURL);
end;

function TXQTermVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[xqcdContextVariables];
end;

class function TXQTermVariable.splitForDotNotation(v: TXQTermVariable): TXQTerm;
var
  name: String;
  prop: String;
begin
  name := v.value;
  v.value := strSplitGet('.', name);
  result := v;
  for prop in strSplit(name, '.') do
    result := TXQTermReadObjectProperty.create(prop).push([result]);
end;

function TXQTermVariable.clone: TXQTerm;
var
  other: TXQTermVariable;
begin
  Result:=inherited clone;
  other := TXQTermVariable(result);
  other.namespaceURL := namespaceURL;
  other.value := value;
end;

function TXQTermVariable.ToString: ansistring;
begin
  result := '$';
  if namespaceURL <> '' then result += 'Q{'+namespaceURL+'}';
  result += value;
end;




{ TXQTermTypeSwitch }

function TXQTermTypeSwitch.evaluate(const context: TXQEvaluationContext): IXQValue;
  function evaluateWithChangedVariable(eval, vari: TXQTerm; const value: IXQValue): IXQValue;
  var
    tempContext: TXQEvaluationContext;
  begin
    tempContext := context;
    if vari is TXQTermVariable then begin
      tempContext.beginSubContextWithVariables;
      tempContext.temporaryVariables.add(TXQTermVariable(vari).value, value);
    end;
    result := eval.evaluate(tempContext);
    if vari is TXQTermVariable then tempContext.endSubContextWithVariables(context);
  end;
  function evaluateWithChangedVariableLog(eval: TXQTerm; log: TXQVariableChangeLog; const vars: array of TXQtermVariable): IXQValue;
  var
    tempContext: TXQEvaluationContext;
    value: TXQValue;
    truecount: Integer;
    i: Integer;
  begin
    tempContext := context;
    tempContext.temporaryVariables := log.condensedCollected;
    try
      truecount := tempContext.temporaryVariables.count;
      if tempContext.temporaryVariables.hasVariable('$', @value) then begin
        if value.getSequenceCount <> 1 then raiseEvaluationError('pxp:PATTERN1', 'Can only assign singleton to context item');
        tempContext.SeqIndex:=1;
        tempContext.SeqLength:=1;
        tempContext.SeqValue := value;
        truecount -= 1;
      end;
      if truecount <> length(vars) then
        for i := 0 to high(vars) do
          if not tempContext.temporaryVariables.hasVariable(vars[i], nil) then
            tempContext.temporaryVariables.add(vars[i], xqvalue());
      tempContext.temporaryVariables.parentLog := context.temporaryVariables;
      result := eval.evaluate(tempContext);
    finally
      tempContext.temporaryVariables.Free;
      log.Free;
    end;
  end;

var
  v: IXQValue;
  start: integer;
  i, j: Integer;
  log: TXQVariableChangeLog;
  seq: TXQTermSequence;
begin
  v := children[0].evaluate(context);
  for i:=1 to high(children) - 1 do begin
    if not (children[i] is TXQTermSequence) then raiseEvaluationError('XPST0003', 'Invalid case clause');
    seq := TXQTermSequence(children[i]);
    if (seq.children[0] is TXQTermPatternMatcher) and (v.toNode <> nil) then begin
      log := patternMatcherMatch(TXQTermPatternMatcher(seq.children[0]).node, v.toNode, context);
      if log <> nil then exit(evaluateWithChangedVariableLog(seq.children[high(seq.children)], log, TXQTermPatternMatcher(seq.children[0]).vars));
      continue;
    end
    else if seq.children[0] is TXQTermVariable then start := 1
    else start := 0;
    for j := start to high(seq.children) - 1 do
      if (seq.children[j] as TXQTermSequenceType).instanceOf(v, context) then
        exit(evaluateWithChangedVariable(seq.children[high(seq.children)], seq.children[0], v));
  end;
  i := high(children);
  if not (children[i] is TXQTermSequence) then raiseEvaluationError('XPST0003', 'Invalid case clause');
  seq := TXQTermSequence(children[i]);
  result := evaluateWithChangedVariable(seq.children[high(seq.children)], seq.children[0], v);
end;

function TXQTermTypeSwitch.getContextDependencies: TXQContextDependencies;
begin
  Result:=getChildrenContextDependencies;
end;

function TXQTermTypeSwitch.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
  j: Integer;
  seq: TXQTermSequence;
begin
  result := visitor.simpleTermVisit(@children[0], self);
  case result of xqtvaAbort: exit; end;
  for i:=1 to high(children) - 1 do begin
    seq := TXQTermSequence(children[i]);
    if (seq.children[0] is TXQTermPatternMatcher) then begin
      visitor.parent := self;
      with TXQTermPatternMatcher(seq.children[0]) do
        for j := 0 to high(vars) do
          visitor.declare(@vars[j]);
      visitor.simpleTermVisit(@seq.children[0], self);
    end else if seq.children[0] is TXQTermVariable then
      visitor.declare(@seq.children[0], self);
    result := visitor.simpleTermVisit(@seq.children[high(seq.children)], self);
    case result of xqtvaAbort: exit; end;
    if (seq.children[0] is TXQTermPatternMatcher) then begin
      visitor.parent := self;
      with TXQTermPatternMatcher(seq.children[0]) do
        for j := 0 to high(vars) do
          visitor.undeclare(@vars[j]);
    end else if seq.children[0] is TXQTermVariable then
      visitor.undeclare(@seq.children[0], self);
  end;

  result := visitor.simpleTermVisit(@children[high(children)], self);
  case result of xqtvaAbort: exit; end;
end;

{ TXQTermSwitch }

function TXQTermSwitch.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  v: IXQValue;
  i, j: Integer;
  operand: IXQValue;
  seq: TXQTermSequence;
begin
  v := children[0].evaluate(context);
  for i:=1 to high(children) - 1 do begin
    if not (children[i] is TXQTermSequence) then raiseEvaluationError('XPST0003', 'Invalid case clause');
    seq := TXQTermSequence(children[i]);
    for j := 0 to high(seq.children) - 1 do begin
      operand := xqvalueAtomize(seq.children[j].evaluate(context));
      if operand.getSequenceCount > 1 then raiseEvaluationError('XPTY0004', 'Sequences are not allowed as case operands in a switch statement');
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

function TXQTermConstructor.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  temp: Integer;
begin
  temp := 0;
  result := evaluate(context, nil, temp);
end;

function TXQTermConstructor.evaluate(const context: TXQEvaluationContext; root: TTreeNode; var baseOffset: longint): IXQValue;
var
  tree: TTreeNode;
  kid: TTreeNode;
  lastKid: TTreeNode;
  subcontext: ^TXQEvaluationContext;
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
    result := TNamespace.Create(url, prefix);
    subcontext^.namespaces.Add(result);
  end;

  procedure addNodeAsKid;
  var
    doc: TTreeNode;
    n: TTreeNode;
    attrib: TTreeAttribute;
  begin
    kid.parent := tree;
    case kid.typ of
      tetAttribute: begin
        if tree.attributes = nil then tree.attributes := TAttributeList.Create;
        tree.attributes.add(kid);
        //kid.offset:=baseOffset;
        kid.parent:=tree;
        kid.document:=tree.document;
        //baseOffset+=1;
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
  var x: IXQValue;
      temp: TTreeNode;
      i: integer;
      tempNamespaces: TNamespaceList;
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
        if kid.typ in [tetOpen, tetDocument] then begin
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
                kid.addNamespaceDeclaration(TNamespace.create('', ''), false);
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
        addNodeAsKid;
      end;
      pvkSequence: begin
        firstAtomic := true;
        for x in v do begin
          addValue(x, firstAtomic);
          if x is TXQValueNode then firstAtomic := true
          else if not (x is TXQValueString) then firstAtomic := false
          else {if  (x.toString <> '')  then }firstAtomic:=false;
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

  function valueToString(v: IXQValue): string;
  var
    first: Boolean;
    x: IXQValue;
  begin
    first := true;
    result := '';
    for x in v do begin
      if not first then result += ' ';
      if x.kind = pvkFunction then raiseEvaluationError('FOTY0013', 'Cannot serialize function');
      result += x.toString;
      first := false;
    end;
  end;

  procedure setTreeNodeNameFromValue(const v: IXQValue; defkind: TXQDefaultNamespaceKind);
    function chooseRandomPrefix(): string;
    begin
      result := 'XXX'; //just like Zorba
      while subcontext^.findNamespace(result, xqdnkAny) <> nil do result += 'X';
    end;

  var
    vname: TXQValueQName;
    prefix: String;
  begin
    if (v.instanceOf(baseSchema.QName))  and ((defkind = xqdnkElementType) or ((v as TXQValueQName).url <> ''))  then begin
      vname := (v as TXQValueQName);
      prefix := vname.prefix;
      if (prefix = '') and (vname.url = XMLNamespaceUrl_XML) then prefix := 'xml';
      if (prefix = '') and (defkind <> xqdnkElementType) then prefix := chooseRandomPrefix;
      tree.namespace := declareNewNamespace(vname.url, prefix);
      tree.value := vname.local;
    end else begin
      tree.value := v.toString;
      subcontext^.splitRawQName(tree.namespace, tree.value, defkind);
    end;
  end;

var i, oldnamespacecount: integer;
  nameValueEvaluated: IXQValue;
begin
  if not (typ in [tetOpen,tetDocument]) then nameValueEvaluated := nameValue.evaluate(context);
  if (typ = tetText) and (nameValueEvaluated.isUndefined) then exit(xqvalue);

  subcontext:=@context;
  oldnamespacecount := 0;
  if subcontext^.namespaces <> nil then oldnamespacecount := subcontext^.namespaces.Count;

  if (typ = tetDocument) then begin
    tree := TTreeDocument.create(nil);
    TTreeDocument(tree).baseURI := context.staticContext.baseURI;
  end else if typ = tetAttribute then tree := TTreeAttribute.create('','')
  else tree := TTreeNode.create(typ);
  if root = nil then root := tree;
  tree.document := root;
  if baseOffset = 0 then begin
      if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot create new nodespxp:eval without a xquery engine (e.g. from an interpreted function in a native module)');
    if context.staticContext.sender.FInternalDocuments = nil then context.staticContext.sender.FInternalDocuments := TFPList.Create;
    context.staticContext.sender.FInternalDocuments.Add(tree);
  end;
  tree.offset:=baseOffset;
  baseOffset+=1;
  case typ of
    tetAttribute: begin
      setTreeNodeNameFromValue(nameValueEvaluated, xqdnkUnknown);
      for i:= 0 to high(children) do
        TTreeAttribute(tree).realvalue := TTreeAttribute(tree).realvalue + valueToString(children[i].evaluate(subcontext^));
      if (tree.namespace <> nil) and (tree.value = 'id') and (tree.namespace.getURL = XMLNamespaceUrl_XML) then
        TTreeAttribute(tree).realvalue := strTrimAndNormalize(TTreeAttribute(tree).realvalue);
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
      for i:=0 to high(children) do
        if (children[i] is TXQTermConstructor) and (TXQTermConstructor(children[i]).isNamespaceConstructor) then begin
          kid := TXQTermConstructor(children[i]).evaluate(subcontext^, root, baseOffset).toNode;
          addNodeAsKid;
        end;

      if nameValue <> nil then begin
        setTreeNodeNameFromValue(nameValue.evaluate(subcontext^), xqdnkElementType);
        if tree.namespace <> nil then
          needNamespaceList.addIfNewPrefixUrl(tree.namespace);
      end;

      if (root = tree) and (oldnamespacecount > 0)  then
        //a newly constructed nodes has all current namespaces binding. But only necessary to add them to the root node, because otherwise they are inherited anyways
        for i := 0 to oldnamespacecount - 1 do
          tree.addNamespaceDeclaration(context.namespaces.items[i], false);


      for i:=0 to high(children) do
        if children[i] is TXQTermConstructor then begin
          if TXQTermConstructor(children[i]).isNamespaceConstructor then continue;
          kid := TXQTermConstructor(children[i]).evaluate(subcontext^, root, baseOffset).toNode;
          if (kid.typ = tetAttribute) and (kid.namespace <> nil) then needNamespaceList.addIfNewPrefixUrl(kid.namespace);
          addNodeAsKid;
        end else
          addValue(children[i].evaluate(subcontext^));

      kid := TTreeNode.create(tetClose, tree.value);
      kid.namespace := tree.namespace;
      kid.offset:=baseOffset;
      baseOffset+=1;
      addKid;
      kid.parent := tree.parent;
      tree.reverse := kid; kid.reverse := tree;
    end;
    tetProcessingInstruction: begin
      tree.value := strTrim(nameValueEvaluated.toString, [#9,#10,#13,' ']);
      if length(children) = 0 then tree.addAttribute('', '')
      else tree.addAttribute('', strTrimLeft(valueToString(children[0].evaluate(subcontext^)), [#9,#10,#13,' ']));
    end;
    tetText, tetComment: tree.value := valueToString(nameValueEvaluated);
    else raiseEvaluationError('pxp:INTERNAL','Invalid type for constructor');
  end;
  result := xqvalue(tree);
  if subcontext <> @context then begin
    subcontext^.namespaces.free;
    Dispose(subcontext);
  end else if context.namespaces <> nil then
    for i:=oldnamespacecount to context.namespaces.Count-1 do
      context.namespaces.Delete(context.namespaces.Count-1);
end;

function TXQTermConstructor.getContextDependencies: TXQContextDependencies;
begin
  Result:=getChildrenContextDependencies + [xqcdContextOther {namespaces}];
end;

function TXQTermConstructor.isNamespaceConstructor: boolean;
begin
  result := (typ = tetAttribute)
            and (nameValue is TXQTermConstant)
            and ((TXQTermConstant(nameValue).value.toString = 'xmlns') or strBeginsWith(TXQTermConstant(nameValue).value.toString, 'xmlns:'));
end;

function TXQTermConstructor.clone: TXQTerm;
begin
  Result:=inherited clone;
  with TXQTermConstructor(result) do begin
    typ := self.typ;
    if self.nameValue <> nil then
      nameValue := self.nameValue.clone;
    if implicitNamespaces <> nil then
      implicitNamespaces := Self.implicitNamespaces.clone;
  end;
end;

destructor TXQTermConstructor.destroy;
begin
  nameValue.Free;
  implicitNamespaces.Free;
  inherited destroy;
end;

{ TXQTermJSONObjectConstructor }

function TXQTermJSONObjectConstructor.evaluate(const context: TXQEvaluationContext): IXQValue;
  function transformValue(v: IXQValue): IXQValue;
  begin
    if not context.staticContext.objectsRestrictedToJSONTypes then
      exit(v);

    if v.getSequenceCount = 0 then
      exit(TXQValueJSONNull.create());
    if v is TXQValueSequence then
      if v.getSequenceCount > 1 then begin
        result := TXQValueJSONArray.create(v.getSequenceCount);
        (result as TXQValueJSONArray).seq.add(v);
        exit;
      end else v := v.getChild(1);
    if v is TXQValueNode then
      exit(xqvalue(v.toNode.outerXML(false)));
    result := v;
  end;

var
  obj: TXQValueObject;
  i: Integer;
begin
  obj := TXQValueObject.create();
  if length(children) and 1 = 1 then raiseEvaluationError('pxp:OBJ', 'Need an even number of names+values in an object constructor');
  for i := 0 to length(children) div 2 - 1 do
    obj.setMutable(children[i*2].evaluate(context).toString, transformValue(children[i*2+1].evaluate(context)));   //todo: empty sequence becomes null, sequence becomes array
  result := obj;
end;

function TXQTermJSONObjectConstructor.getContextDependencies: TXQContextDependencies;
begin
  result := getChildrenContextDependencies;
end;


type
  PXQEvaluationContext = ^TXQEvaluationContext;

  TXQTerm_VisitorTrackKnownVariables = class(TXQTerm_Visitor)
    overridenVariables: {set of string} TXQVariableChangeLog;
    tempValue: IXQValue;
    constructor create;
    destructor Destroy; override;

    procedure declare(v: PXQTermVariable); override;
    procedure undeclare(v: PXQTermVariable); override;
  end;

  { TXQTerm_VisitorReplaceKnownVariables }

  TXQTerm_VisitorReplaceKnownVariables = class(TXQTerm_VisitorTrackKnownVariables)
    context: PXQEvaluationContext;
    uncertainVariables: TXQVariableChangeLog; //pattern variables cannot be are not know statically
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
  overridenVariables.add(v^.value, tempValue, v^.namespaceURL);
end;

procedure TXQTerm_VisitorTrackKnownVariables.undeclare(v: PXQTermVariable);
begin
  Assert((overridenVariables.vars[overridenVariables.count-1].name = v^.value) and
         equalNamespaces(overridenVariables.vars[overridenVariables.count-1].namespaceURL, v^.namespaceURL));
  overridenVariables.removeLast;
end;

function TXQTerm_VisitorReplaceKnownVariables.visit(term: PXQTerm): TXQTerm_VisitAction;
var
  temp: TXQValue;
  //selfdefined: Boolean;
begin
  if term^ is TXQTermVariable then begin
    if not overridenVariables.hasVariable(TXQTermVariable(term^), @temp) then
      replace(term, TXQTermConstant.create(context^. getVariable(TXQTermVariable(term^))))
    else if length(patterns) > 0 then begin
      {not sure what to do with these
      selfdefined := false;
      with patterns[high(patterns)] do
        for i := 0 to high(vars) do
          if vars[i].equalsVariable(TXQTermVariable(term^)) then
            selfdefined := true;}
      if uncertainVariables = nil then uncertainVariables := TXQVariableChangeLog.create();
      if not uncertainVariables.hasVariable(TXQTermVariable(term^), @temp) then
        uncertainVariables.add(TXQTermVariable(term^), tempValue);
    end;
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
  uncertainVariables.Free;
  inherited Destroy;
end;

{ TXQTermDefineFunction }

constructor TXQTermDefineFunction.createReference(const anamespace: INamespace; aname: string; arity: integer);
var
  call: TXQTermNamedFunction;
  i: Integer;
begin
  parameterCount := arity;
  funcname:=aname;
  namespace := anamespace;
  call := TXQTermNamedFunction.create(anamespace.getURL, aname, arity);
  for i := 1 to arity do begin
    push(TXQTermDefineVariable.create(inttostr(i)+'.', XMLNamespace_MyExtensions));
    call.push(TXQTermVariable.create(inttostr(i)+'.', XMLNamespace_MyExtensions));
  end;
  push(call);
  kind := xqtdfNamedReference;
end;

constructor TXQTermDefineFunction.createReference(anamespaceUrl: string; aname: string; arity: integer);
begin
  createReference(TNamespace.create(anamespaceUrl, 'prf'), aname, arity);
end;

function TXQTermDefineFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  temp: TXQValueFunction;
  tempVisitor: TXQTerm_VisitorReplaceKnownVariables;
  i: Integer;
  tempterm: TXQTermFlower;
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
      end else temp := define(context, false);
    end;
    xqtdfStaticPartialApplication: temp := defineStaticPartialApplication(context);
    xqtdfDynamicPartialApplication: begin
      assert(children[0] is TXQTermDynamicFunctionCall);
      dfv := TXQTermDynamicFunctionCall(children[0]).children[0].evaluate(context);
      if not (dfv is TXQValueFunction) then raiseEvaluationError('XPTY0004', 'Need function for partial application, got: '+dfv.debugAsStringWithTypeAnnotation());
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
    if tempVisitor.uncertainVariables <> nil then begin
      tempterm := TXQTermFlower.Create;
      tempterm.returned := temp.body;
      temp.body := tempterm;
      setlength(tempterm.vars, tempVisitor.uncertainVariables.count);
      for i := 0 to tempVisitor.uncertainVariables.count - 1 do begin
        tempterm.vars[i].kind := xqfkLet;
        tempterm.vars[i].loopvar := TXQTermVariable.create(tempVisitor.uncertainVariables.vars[i].name, tempVisitor.uncertainVariables.vars[i].namespaceURL);
        tempterm.vars[i].expr := TXQTermConstant.Create(context.getVariable(tempterm.vars[i].loopvar));
      end;
    end;
  finally
    tempVisitor.free;
  end;
end;

function TXQTermDefineFunction.define(const context: TXQEvaluationContext; const clearFocus: boolean): TXQValueFunction;
var
  i: Integer;
  v: TXQTermVariable;
begin
  result := TXQValueFunction.create();
  result.namespace := namespace;
  result.name:=funcname;
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
    result.context.TextElement := nil;
    result.context.SeqValue := nil;
    result.context.SeqIndex := 0;
    result.context.SeqLength := 0;
  end;
end;

function TXQTermDefineFunction.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusDocument, xqcdFocusOther];
end;

function nativeMetaTypeToActualType(t: TXQTermSequenceType): TXQTermSequenceType;
begin
 result := TXQTermSequenceType(t.clone);
 if (t.kind = tikAtomic) and ((t.atomicTypeInfo = baseSchema.numericPseudoType) or (t.atomicTypeInfo = baseSchema.trueNumericPseudoType)) then
   result.atomicTypeInfo := baseSchema.anyAtomicType;
end;

function isPseudoVariable(v: TXQTerm): boolean; inline;
begin
  result := (v is TXQTermVariable) and (TXQTermVariable(v).value[1] in ['0'..'9']);
end;

function TXQTermDefineFunction.visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  i: Integer;
  subchildren: array of txqterm;
begin
  visitor.parent := self;
  case kind of
    xqtdfUserDefined, xqtdfNamedReference: begin
      for i := 0 to parameterCount - 1 do
        visitor.declare(@((children[i] as TXQTermDefineVariable).variable));
      if not (children[high(children)] is TXQTermSequenceType) then
        result := visitor.simpleTermVisit(@children[high(children)], self);
      visitor.parent := self;
      for i := parameterCount - 1 downto 0 do
        visitor.undeclare(@((children[i] as TXQTermDefineVariable).variable));
    end;
    xqtdfStaticPartialApplication, xqtdfDynamicPartialApplication: begin
      subchildren := (children[0] as TXQTermWithChildren).children;
      for i := 0 to high(subchildren) do
        if isPseudoVariable(subchildren[i]) then
          visitor.declare(@subchildren[i]);
      result := visitor.simpleTermVisit(@children[0], self);
      visitor.parent := self;
      for i := high(subchildren) downto 0 do
        if isPseudoVariable(subchildren[i]) then
          visitor.undeclare(@subchildren[i]);
    end;
  end;

end;

function TXQTermDefineFunction.clone: TXQTerm;
var
  other: TXQTermDefineFunction;
begin
  Result:=inherited clone;
  other := TXQTermDefineFunction(result);
  other.namespace := namespace;
  other.funcname := funcname;
  other.parameterCount := parameterCount;
  copyAnnotations(other.annotations, annotations);
  other.kind := kind;
end;

destructor TXQTermDefineFunction.destroy;
var i,j: integer;
begin
  for i := 0 to high(annotations) do for j := 0 to high(annotations[i].params) do
    FreeAndNil(annotations[i].params[j]);
  inherited destroy;
end;

function TXQTermDefineFunction.findNamedFunctionVersion(const context: TXQEvaluationContext): PXQFunctionParameterTypes;
var
  f: TXQTermNamedFunction;
  i: Integer;
begin
  f := TXQTermNamedFunction(children[high(children)]);
  if f.kind in [xqfkUnknown, xqfkNativeInterpreted] then
    f.init(context);

  result := nil;
  case f.kind  of
    xqfkBasic, xqfkComplex, xqfkWrappedOperator:
      for i := 0 to high(f.func.versions) do
        if length(f.func.versions[i].types) = length(f.children) then
          exit(@f.func.versions[i]);
    xqfkTypeConstructor: if length(f.children) = 1 then exit;
    xqfkUnknown, xqfkNativeInterpreted: exit;
  end;

  if (result = nil) and context.staticContext.strictTypeChecking and ((f.kind <> xqfkBasic) or (TXQBasicFunctionInfo(f.func).func <> @xqFunctionConcat)) then
    raiseParsingError('XPST0017', 'Failed to find function (mismatch arg count?)');
end;

procedure TXQTermDefineFunction.initNamedFunctionReference(const context: TXQEvaluationContext);


var
  nc: array of TXQTerm;
  version: ^TXQFunctionParameterTypes;
  resulttype: TXQTermSequenceType;
  f: TXQTermNamedFunction;
  i: Integer;
  temp: TXQTermSequenceType;
begin
  if (kind <> xqtdfNamedReference) or initialized then exit;

  if (children[high(children)] is TXQTermNamedFunction) then begin
    version := findNamedFunctionVersion(context);
    f := TXQTermNamedFunction(children[high(children)]);

    resulttype := nil;
    if version <> nil then begin
      for i := 0 to parameterCount-1 do
        (children[i] as TXQTermDefineVariable).push(nativeMetaTypeToActualType(version^.types[i]));
      resulttype := nativeMetaTypeToActualType(version^.returnType);
    end else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then begin
      for i := 0 to parameterCount-1 do begin
        temp := TXQTermSequenceType.create;
        temp.kind := tikAny;
        temp.allowNone := true;
        (children[i] as TXQTermDefineVariable).push(temp);
      end;
      resulttype := TXQTermSequenceType.create(baseSchema.string_);
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

function TXQTermDefineFunction.defineStaticPartialApplication(const context: TXQEvaluationContext): TXQValueFunction;
var
  f: TXQTermNamedFunction;
  version: PXQFunctionParameterTypes;
  j: Integer;
  i: Integer;
begin
  version := findNamedFunctionVersion(context);
  f := TXQTermNamedFunction(children[high(children)]);

  result := TXQValueFunction.create();
  result.namespace := namespaceReverseLookup(f.namespaceUrl);
  result.name:=f.funcname;
  if (f.kind in [xqfkUnknown, xqfkNativeInterpreted]) and (length(f.interpretedFunction.annotations) > 0) then
    copyAnnotations(result.annotations, f.interpretedFunction.annotations);
  result.body := TXQTermNamedFunction.create();
  TXQTermNamedFunction(result.body).assignWithoutChildren(f);

  setlength(result.parameters, parameterCount);
  j := 0;
  for i := 0 to high(f.children) do
    if isPseudoVariable(f.children[i]) then begin
      TXQTermNamedFunction(result.body).push(f.children[i].clone);
      result.parameters[j].variable := TXQTermVariable(f.children[i].clone);
      if version <> nil then
        result.parameters[j].seqtype := nativeMetaTypeToActualType(version^.types[i])
       else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then begin
        result.parameters[j].seqtype := TXQTermSequenceType.create();
        with result.parameters[j].seqtype do begin
          kind := tikAny;
          allowNone := true;
        end;
       end else if f.kind in [xqfkUnknown, xqfkNativeInterpreted] then begin
        if f.interpretedFunction.parameters[i].seqtype <> nil then
          result.parameters[j].seqtype := TXQTermSequenceType(f.interpretedFunction.parameters[i].seqtype.clone);
       end;
      inc(j);
     end else
      TXQTermNamedFunction(result.body).push(TXQTermConstant.create(f.children[i].evaluate(context)));
  assert(j = parameterCount);
  result.ownsTerms := true;
  if version <> nil then
    result.resulttype := nativeMetaTypeToActualType(version^.returnType)
  else if (f.kind = xqfkBasic) and (TXQBasicFunctionInfo(f.func).func = @xqFunctionConcat) then
    result.resulttype := TXQTermSequenceType.create(baseSchema.string_)
  else if (f.kind in [xqfkUnknown, xqfkNativeInterpreted]) and (f.interpretedFunction.resulttype <> nil)  then
    result.resulttype := TXQTermSequenceType(f.interpretedFunction.resulttype.clone);
  result.context := context;
end;

function TXQTermDefineFunction.defineDynamicPartialApplication(const context: TXQEvaluationContext; f: TXQValueFunction): TXQValueFunction;
var
  j: Integer;
  i: Integer;
  ch: array of TXQTerm;
begin
  ch := (children[0] as TXQTermWithChildren).children;
  if length(ch) <> length(f.parameters) + 1 then raiseParsingError('XPTY0004', 'Invalid argument count');
  result := TXQValueFunction.create();
  result.namespace := f.namespace;
  result.name:=f.name;
  copyAnnotations(result.annotations, f.annotations);
  result.body := TXQTermDynamicFunctionCall.create(TXQTermConstant.Create(f));
  setlength(result.parameters, parameterCount);
  j := 0;
  for i := 0 to high(f.parameters) do begin
    if isPseudoVariable(ch[i+1]) then begin
      TXQTermDynamicFunctionCall(result.body).push(ch[i+1].clone);
      result.parameters[j].variable := TXQTermVariable(ch[i+1].clone); //TXQTermDynamicFunctionCall(result.body).children[i+1] as TXQTermVariable;
      if f.parameters[i].seqtype <> nil then
        result.parameters[j].seqtype := TXQTermSequenceType(f.parameters[i].seqtype.clone);
      inc(j);
     end else
      TXQTermDynamicFunctionCall(result.body).push(TXQTermConstant.create(ch[i+1].evaluate(context)));
  end;
  assert(j = parameterCount);
  result.ownsTerms := true;
  //it should be possible to change this to share the type instead cloning it
  //since the body contains a reference to f, so f will not be freed before result and the type remain valid
  //(same for the parameters)
  //but it is tricky to get right
  if f.resulttype <> nil then result.resulttype := TXQTermSequenceType(f.resulttype.clone);
  result.context := f.context;
end;



