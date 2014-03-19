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


{ TXQTerm }

function TXQTerm.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES;
end;

function TXQTerm.debugTermToString: string;
var
  i: Integer;
begin
  result := '(' + ClassName;
  if length(children) >= 1 then result += ': '+children[0].debugTermToString();
  for i:= 1 to high(children) do result += ', '+children[1].debugTermToString();
  result += ')';
end;

destructor TXQTerm.destroy;
var
  i: Integer;
begin
  for i := 0 to high(children) do children[i].free;
  inherited destroy;
end;

procedure TXQTerm.push(t: TXQTerm);
begin
  SetLength(children, length(children) + 1);
  children[high(children)] := t;
end;

function TXQTerm.push(t: array of TXQTerm): TXQTerm;
var
  i: Integer;
begin
  for i := 0 to high(t) do push(t[i]);
  result := self;
end;

procedure TXQTerm.raiseParsingError(const errcode, s: string);
begin
  raise EXQParsingException.Create(errcode, s);
end;

procedure TXQTerm.raiseEvaluationError(const errcode, s: string);
begin
  raise EXQEvaluationException.Create(errcode, s);
end;

procedure TXQTerm.evaluateChildren(const context: TXQEvaluationContext; out results: TXQVArray);
var
  i: Integer;
begin
  setlength(results, length(children));
  for i:=0 to high(children) do
    results[i] := children[i].evaluate(context);
end;

function TXQTerm.getChildrenContextDependencies: TXQContextDependencies;
var
  i: Integer;
begin
  result := [];
  for i := 0 to high(children) do
    result += children[i].getContextDependencies;
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
    if (t is TXQTermFilterSequence) and (length(t.children) = 1) then t := t.children[0];
    if t is TXQTermFilterSequence then t := t.children[0];
    while t <> nil do begin
      if (t is TXQTermSequence) and (length(TXQTermSequence(t).children) = 1) then
         t := TXQTermSequence(t).children[0]
      else if (t is TXQTermNodeMatcher) or (t is TXQTermVariable) then
        exit(true)
      else if (t is TXQTermReadObjectProperty) or (t is TXQTermDynamicFunctionCall) then
        t := t.children[0]
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
    varnamespace: INamespace;
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
      v := v.children[0];
    end else if v is TXQTermDynamicFunctionCall then begin
      if length(TXQTermDynamicFunctionCall(v).children) <> 2 then raiseEvaluationError('pxp:VAR', 'invalid argument count for object property assignment');
      arrayPrepend(properties, TXQTermDynamicFunctionCall(v).children[1].evaluate(context).toString);
      v := v.children[0];
    end else if v is TXQTermFilterSequence then begin
      if not (assignmentMode in [amOverride, amArrayAppend]) or (length(properties) > 0) then raiseEvaluationError('pxp:VAR', 'Array assignment cannot be followed by property assignment');
      if length(TXQTermFilterSequence(v).children) = 2 then begin
        arrayIndex := TXQTermFilterSequence(v).children[1].evaluate(context).toInt64;
        if arrayIndex < 0 then raiseEvaluationError('pxp:VAR', 'Cannot assign to negative array index');
        if assignmentMode <> amArrayAppend then assignmentMode := amArrayOverride
        else assignmentMode := amArrayInsert; //due to automatical flattening, appending to element i, is equal to insertion after element i
      end else assignmentMode := amArrayAppend;
      v := v.children[0];
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

destructor TXQTermDefineVariable.destroy;
begin
  variable.Free;
  inherited destroy;
end;

{ TXQTermType }

constructor TXQTermSequenceType.create();
begin
end;


function TXQTermSequenceType.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := TXQValueFunction.create(self); //return a function returning this term. This allows returning the type itself, without having a "type"-type in the type system.
end;

function TXQTermSequenceType.serialize: string;
begin
  result := name;
end;

function TXQTermSequenceType.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;

function TXQTermSequenceType.isSingleType(): boolean;
begin
  result := (kind = tikAtomic) and not allowMultiple;
end;

function TXQTermSequenceType.castableAsBase(v: IXQValue): boolean;
begin
  if v.isUndefined then result := allowNone
  else if v.getSequenceCount > 1 then result := false
  else if v is TXQValueSequence then result := atomicTypeInfo.tryCreateValue((v as TXQValueSequence).seq[0])
  else result := atomicTypeInfo.tryCreateValue(v);
end;

function TXQTermSequenceType.castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
var
  temp: String;
  ns: INamespace;
begin
  v := xqvalueAtomize(v);
  if not castableAsBase(v) then raiseEvaluationError('XPTY0004', 'impossible cast as');
  if not atomicTypeInfo.derivedFrom(baseSchema.QName) then result := atomicTypeInfo.createValue(v)
  else begin
    temp := trim(v.toString);
    context.splitRawQName(ns, temp, xqdnkAny);
    result := TXQValueQName.create(atomicTypeInfo, ns, temp);
  end;
end;

function TXQTermSequenceType.castableAs(v: IXQValue): boolean;
begin
  if kind <> tikAtomic then raiseEvaluationError('XPST0051', 'need atomic type for castable as');
  v := xqvalueAtomize(v);
  result := castableAsBase(v);
end;

function TXQTermSequenceType.instanceOf(ta: IXQValue; const context: TXQEvaluationContext): boolean;
var nodeCondition: TXQPathNodeCondition;

  function instanceOfSingleType(sub: IXQValue): boolean;
  var
    t: TXSType;
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
    if xqpncCheckNamespace in nodeCondition.options then
      if qmAttribute in nodeMatching.matching then nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkUnknown)
      else nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkElementType);
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
      tempContext.temporaryVariables.add(TXQTermVariable(children[pos]).value, v, TXQTermVariable(children[pos]).namespace);
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
        raiseEvaluationError('XPTY0004', 'Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+v.toString);

      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.add(vars[pos].varname, v, vars[pos].namespace);
      if vars[pos].positionVarname <> '' then
        tempContext.temporaryVariables.add(vars[pos].positionVarname, xqvalue(index), vars[pos].positionVarNamespace);
      evaluateForLoop(pos+1);
      tempContext.temporaryVariables.popAll();

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

     if vars[pos].kind = xqfkLet then
       continueWithValue(tempSeq)
     else case tempSeq.kind of
       pvkUndefined:;
       pvkSequence:
         for i:=0 to (tempSeq as TXQValueSequence).seq.Count-1 do
           continueWithValue((tempSeq as TXQValueSequence).seq[i], i + 1);
       else continueWithValue(tempSeq);
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

destructor TXQTermFlower.destroy;
var
  i: Integer;
begin
  for i:=0 to high(vars) do begin
    vars[i].expr.Free;
    vars[i].sequenceTyp.Free;
  end;
  for i:= 0 to high(orders) do
    orders[i].expr.Free;
  where.Free;
  returned.Free;
  inherited destroy;
end;


{ TXQTermNamedFunction }

constructor TXQTermNamedFunction.create(const akind: TXQTermNamedFunctionKind; const afunc: TXQAbstractFunctionInfo);
begin
  kind := akind;
  func := afunc;
end;

constructor TXQTermNamedFunction.create(const ns: INamespace; const name: string);
begin
  if not findKindIndex(ns, name, kind, func) then begin
    kind := xqfkUnknown;
    funcname:=name;
    namespace := ns;
  end;
end;

constructor TXQTermNamedFunction.create(const ns: INamespace; const name: string; args: array of TXQTerm);
begin
  create(ns, name);
  push(args);
end;

class function TXQTermNamedFunction.createIfExists(const name: string; const sc: TXQStaticContext): TXQTermNamedFunction;
var akind: TXQTermNamedFunctionKind;
    afunc: TXQAbstractFunctionInfo;
    localName: string;
    ns: INamespace;
begin
  localName:=name;
  sc.splitRawQName(ns, localName, xqdnkFunction);
  if findKindIndex(ns, localName, akind, afunc) then exit(TXQTermNamedFunction.Create(akind, afunc))
  else if pos(':', name) = 0 then begin
     if sc.defaultElementTypeNamespace <> nil then begin
       if findKindIndex(sc.defaultElementTypeNamespace, localName, akind, afunc) then exit(TXQTermNamedFunction.create(akind, afunc))
     end else if sc.defaultTypeNamespace <> nil then
       if findKindIndex(sc.defaultTypeNamespace, localName, akind, afunc) then exit(TXQTermNamedFunction.create(akind, afunc));
  end;
  result := TXQTermNamedFunction.create(ns, localName); //cannot know if the function is valid yet
end;

function TXQTermNamedFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
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
    if namespace = nil then exit();
    module := TXQueryEngine.findNativeModule(namespace.getURL);
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
  evilkids: TXQVArray;
  tempcontext: TXQEvaluationContext;
  i: Integer;
  j: Integer;
  vfunc: TXQValueFunction;
  sc: TXQStaticContext;
  ns: INamespace;
begin
  evaluateChildren(context, evilkids);

  assert((kind = xqfkUnknown) or (func <> nil));

  if context.staticContext.strictTypeChecking and (kind in [xqfkBasic, xqfkComplex,xqfkNativeInterpreted]) then
    if not func.checkTypes(evilkids, context) then
      raiseEvaluationError('XPTY0004', 'Invalid types for function call: '+func.versions[0].name);


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
        1: result := TXSType(TObject(func)).createValue(evilkids[0])
        else raiseEvaluationError('FORG0001', 'Need singleton value to cast as '+TXSType(TObject(func)).name);
      end;
    end;
    xqfkUnknown, xqfkNativeInterpreted: begin
      if kind <> xqfkNativeInterpreted then begin
        vfunc := nil;
        ns := namespace;
        sc := context.findModuleStaticContext(ns);
        if sc <> nil then
          for i := 0 to high(sc.functions) do
            if (sc.functions[i].name = funcname)
               and  (length(sc.functions[i].parameters) = length(children))
               and ((sc.functions[i].namespace = ns) or ((ns <> nil) and (sc.functions[i].namespace <> nil) and (sc.functions[i].namespace.getURL = ns.getURL))) then begin
              vfunc := sc.functions[i];
              break;
            end;
        if vfunc = nil then begin
          findKindIndex(namespace, funcname, kind, func);
          if kind <> xqfkUnknown then exit(evaluate(context));
          //if namespace <> nil then raiseEvaluationError('XPST0017', 'Function {'+ namespace.geturl+'}'+namespace.getPrefix+':'+funcname+' not found')
          //else }
          raiseEvaluationError('XPST0017', 'Function '+ funcname+' not found. '+suggestions);
        end;
      end else begin
        if TXQInterpretedFunctionInfo(func).func = nil then
          TXQInterpretedFunctionInfo(func).initialize();
        vfunc :=  TXQInterpretedFunctionInfo(func).func;
        sc := context.staticContext;
      end;

      tempcontext := context;
      tempcontext.temporaryVariables := TXQVariableChangeLog.create();
      tempcontext.staticContext := sc;
      for j := 0 to high(vfunc.parameters) do
        tempcontext.temporaryVariables.add(vfunc.parameters[j].name, TXQAbstractFunctionInfo.convertType(evilkids[j], vfunc.parameters[j].seqtype, context), vfunc.parameters[j].namespace);
      result := vfunc.body.evaluate(tempcontext);
      result := TXQAbstractFunctionInfo.convertType(result, vfunc.resulttype, context);
      tempcontext.temporaryVariables.free;
    end;
  end;
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

class function TXQTermNamedFunction.findKindIndex(const ns: INamespace; const name: string; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
var
  aindex: Integer;
  module: TXQNativeModule;
  t: TXSType;
begin
  if (ns = nil) then exit(false);

  module := TXQueryEngine.findNativeModule(ns.getURL);

  if (module = nil) then exit(false);

  afunc := module.findBasicFunction(name);
  if afunc <> nil then begin
    akind:=xqfkBasic;
    exit(true);
  end;

  afunc := module.findComplexFunction(name);
  if afunc <> nil then begin
    akind:=xqfkComplex;
    exit(true);
  end;

  afunc := module.findInterpretedFunction(name);
  if afunc <> nil then begin
    akind:=xqfkNativeInterpreted;
    exit(true);
  end;

  aindex := module.binaryOpFunctions.IndexOf(name);
  if aindex >= 0 then begin
    afunc := TXQOperatorInfo(module.binaryOpFunctions.Objects[aindex]);
    akind := xqfkWrappedOperator;
    exit;
  end;

  t:=baseSchema.findType(name);
  if t <> nil then begin
    akind:=xqfkTypeConstructor;
    afunc := TXQAbstractFunctionInfo(TObject(t));
    exit(true)
  end;

  exit(false);
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
begin
  //if length(children) < 2 then raiseEvaluationError('XPST0003', 'Only one argument given, need function AND arguments');
  func := children[0].evaluate(context);
  if func.isUndefined then exit(xqvalue());
  if (func is TXQValueSequence) and (func.getSequenceCount = 1) then func := func.getChild(0);

  if func is TXQValueFunction then begin
    raiseEvaluationError('XPST0003', 'Dynamic function call is not yet implemented');
    //TODO
  end;

  if length(children) = 1 then begin
    result := nil;
    for v in func do begin
      if v is TXQValueObject then begin
        sl := TStringList.Create;
        (v as TXQValueObject).enumerateKeys(sl);
        xqvalueSeqAdd(result, xqvalue(sl));
        sl.free;
      end;
      if v is TXQValueJSONArray then begin
        ara := v as TXQValueJSONArray;;
        for i := 0 to ara.seq.Count-1 do
          xqvalueSeqAdd(result, ara.seq[i]);
      end;
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
    end;
    //end else raiseEvaluationError('XPST003', 'Invalid call, expected function, object or array, got: '+func.debugAsStringWithTypeAnnotation()); no error since JSONiq 1.0.1
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
    if not op.checkTypes(evilkids, context) then
      raiseEvaluationError('XPTY0004', 'Invalid types for binary operator: '+op.name+ ' '+op.followedBy);
    result := op.func(context, evilkids[0], evilkids[1]);
  end;
end;

function TXQTermBinaryOp.getContextDependencies: TXQContextDependencies;
begin
    result := op.contextDependencies + getChildrenContextDependencies;
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

constructor TXQTermNodeMatcher.Create(const avalue: string; asfunction: boolean);
begin
  axis := '';
  select := avalue;
  if strContains(select,'::') then axis:=strSplitGet('::',select);
  hadNamespace := strContains(select,':');
  if hadNamespace then  namespace:=strSplitGet(':',select);
  func := asfunction;
end;

function TXQTermNodeMatcher.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if (select = '.') and (context.SeqValue <> nil) then result := context.SeqValue
  else if func and (context.SeqValue = nil) and (axis = '') and (select = 'text') and (context.staticContext.sender.TextElement<>nil) then
    result := xqvalue(context.staticContext.sender.TextElement)
  else begin
    result := TXQueryEngine.evaluateSingleStepQuery(toQueryCommand,context);
    xqvalueSeqSqueeze(result);
  end;
end;

function TXQTermNodeMatcher.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusOther, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
end;

function TXQTermNodeMatcher.debugTermToString: string;
begin
  result := '';
  if axis <> '' then result += axis + '::';
  if namespace <> '' then result += namespace + ':';
  result += select;
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
      if (hadNamespace) and (namespace <> '*') then begin
        Include(Result.matching, qmCheckNamespace);
        result.namespacePrefix:=namespace;
      end;
    end;
    else begin
      result.typ := getAxis(axis);
      result.matching := [qmValue,qmElement];
      result.value:=select;
      if (namespace <> '*') then begin
        Include(Result.matching, qmCheckNamespace);
        result.namespacePrefix:=namespace;
      end;
    end;
  end;
end;

{ TXQTermNumber }

constructor TXQTermNumber.create(const avalue: string);
begin
  if strContains(avalue,'e') or strContains(avalue,'E') then value := baseSchema.double.createValue(avalue)
  else if strContains(avalue,'.') then value := baseSchema.decimal.createValue(avalue)
  else value := baseSchema.integer.createValue(avalue);
end;

constructor TXQTermNumber.create(const avalue: IXQValue);
begin
  value := avalue;
end;

function TXQTermNumber.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := value;
end;

function TXQTermNumber.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
end;

{ TXQTermReadAttribute }

constructor TXQTermReadAttribute.create(avalue: string; func: boolean);
begin
  if func then begin
    if pos('::', avalue) > 0 then if strSplitGet('::', avalue) <> 'attribute' then raiseParsingError('XPST0003', 'Invalid axis for attribute test');
    if (avalue <> 'attribute') and (avalue <> 'node') then raiseParsingError('XPST0003', 'invalid attribute test: '+avalue);
    attribName := '*';
  end else begin
    if pos(':', avalue) > 0 then namespace := strSplitGet(':', avalue);
    attribName := avalue;
  end;
end;

function TXQTermReadAttribute.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  tempEl: TTreeNode;
  testValue: Boolean;
  testNS: Boolean;
  readvalue: String;
  readnamespace: String;
  temp: INamespace;
  i: Integer;
  attrib: TTreeAttribute;
begin
  tempEl := nil;
  if context.SeqValue <> nil then begin
    if context.SeqValue is TXQValueNode then tempEl := context.SeqValue.toNode
  end else if context.ParentElement <> nil then tempEl := context.ParentElement
  else raise EXQEvaluationException.create('XPDY0002', 'Need context item to read attributes');
  if tempEl = nil then
    raise EXQEvaluationException.create('XPTY0020', 'Need node as context item to read attributes');
  if tempEl.typ = tetProcessingInstruction then
    exit(xqvalue);

  readvalue := attribName;
  readnamespace := namespace;
  if length(children) > 0 then begin
    if not (children[0] is TXQTermNodeMatcher) then raiseEvaluationError('XPST0003', 'Invalid subtype');
    readvalue := TXQTermNodeMatcher(children[0]).select;
    readnamespace := TXQTermNodeMatcher(children[0]).namespace;
  end;

  result := nil;
  if tempEl.attributes <> nil then begin
    testValue := readvalue <> '*';
    testNS := (readnamespace <> '*') and (testValue or (readnamespace <> ''));
    if testNS then begin
      temp := context.findNamespace(readnamespace, xqdnkUnknown);
      if temp <> nil then readnamespace:=temp.getURL
      else if readnamespace <> '' then begin
        if not context.staticContext.useLocalNamespaces then raiseEvaluationError('XPST0081', 'Unknown namespace prefix: '+readnamespace+' for reading attribute: '+readvalue);
        if context.SeqValue.toNode <> nil then readnamespace:=context.SeqValue.toNode.getNamespaceURL(readnamespace)
        else readnamespace:='';
        if readnamespace = '' then exit(xqvalue); //do not raise an exception if the namespace is unknown and the local namespace extension is enabled (so you can use unknown namespaces in css)
      end;
    end;
    for i := 0 to tempel.attributes.Count - 1 do begin
      attrib := tempel.attributes.items[i];
      if      (not testValue or striEqual(attrib.value, readvalue))
          and (not testNS or ( attrib.getNamespaceURL() = readnamespace))
          and not attrib.isNamespaceNode
          then
        xqvalueSeqAdd(result, xqvalue(tempEl.attributes.Items[i]));
    end;
  end;
  if result = nil then result:=xqvalue();
end;

function TXQTermReadAttribute.getContextDependencies: TXQContextDependencies;
begin
    result := ALL_CONTEXT_DEPENDENCIES - [xqcdContextTime, xqcdContextVariables];
end;

{ TXQTermVariable }

constructor TXQTermVariable.create(const avalue: string; staticContext: TXQStaticContext);
begin
  value := avalue;
  if value = '' then raiseEvaluationError('XPST0003', 'variable with empty name');
  //if value[1] = '$' then raiseEvaluationError('variable name starts with a double $:  ' + value);
  if value[length(value)] = ';' then delete(value, length(value), 1);
  if staticContext <> nil then staticContext.splitRawQName(namespace, value, xqdnkUnknown);
end;

constructor TXQTermVariable.create(const avalue: string; const anamespace: INamespace);
begin
  value := avalue;
  namespace := anamespace;
end;

function TXQTermVariable.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  result := context.getVariable(value, namespace);
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

{ TXQTermString }

constructor TXQTermString.create(avalue: string);
begin
  value := avalue;
end;

function TXQTermString.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  Result := xqvalue(value);
end;

function TXQTermString.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;




{ TXQTermTypeSwitch }

function TXQTermTypeSwitch.evaluate(const context: TXQEvaluationContext): IXQValue;
  function evaluateWithChangedVariable(eval, vari: TXQTerm; const value: IXQValue): IXQValue;
  var
    tempContext: TXQEvaluationContext;
  begin
    tempContext := context;
    if vari is TXQTermVariable then begin
      if tempContext.temporaryVariables = nil then tempContext.temporaryVariables := TXQVariableChangeLog.create();
      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.add(TXQTermVariable(vari).value, value);
    end;
    result := eval.evaluate(tempContext);
    if vari is TXQTermVariable then begin
      tempContext.temporaryVariables.popAll();
      if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables);
    end;
  end;

var
  v: IXQValue;
  start: integer;
  i, j: Integer;
begin
  v := children[0].evaluate(context);
  for i:=1 to high(children) - 1 do begin
    if not (children[i] is TXQTermSequence) then raiseEvaluationError('XPST0003', 'Invalid case clause');
    if children[i].children[0] is TXQTermVariable then start := 1
    else start := 0;
    for j := start to high(children[i].children) - 1 do
      if (children[i].children[j] as TXQTermSequenceType).instanceOf(v, context) then
        exit(evaluateWithChangedVariable(children[i].children[high(children[i].children)], children[i].children[0], v));
  end;
  i := high(children);
  result := evaluateWithChangedVariable(children[i].children[high(children[i].children)], children[i].children[0], v);
end;

function TXQTermTypeSwitch.getContextDependencies: TXQContextDependencies;
begin
  Result:=getChildrenContextDependencies;
end;

{ TXQTermSwitch }

function TXQTermSwitch.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  v: IXQValue;
  i, j: Integer;
  operand: IXQValue;
begin
  v := children[0].evaluate(context);
  for i:=1 to high(children) - 1 do begin
    if not (children[i] is TXQTermSequence) then raiseEvaluationError('XPST0003', 'Invalid case clause');
    for j := 0 to high(children[i].children) - 1 do begin
      operand := xqvalueAtomize(children[i].children[j].evaluate(context));
      if operand.getSequenceCount > 1 then raiseEvaluationError('XPTY0004', 'Sequences are not allowed as case operands in a switch statement');
      if xqvalueDeep_equal(context, v, operand, context.staticContext.collation) then
        exit(children[i].children[high(children[i].children)].evaluate(context));
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
      end
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
            and (nameValue is TXQTermString)
            and ((TXQTermString(nameValue).value = 'xmlns') or strBeginsWith(TXQTermString(nameValue).value, 'xmlns:'));
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


{ TXQTermDefineFunction }

constructor TXQTermDefineFunction.create(aname: string);
begin
  funcname:=aname;
end;

function TXQTermDefineFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  raiseEvaluationError('pxp:INTERNAL', 'Dead branch called');
  ignore(context);
  result := nil;
end;

function TXQTermDefineFunction.define(): TXQValueFunction;
var
  i: Integer;
  v: TXQTermVariable;
begin
  result := TXQValueFunction.create();
  result.namespace := namespace;
  result.name:=funcname;
  setlength(result.parameters, parameterCount);
  for i := 0 to high(result.parameters) do begin
    v := ((children[i] as TXQTermDefineVariable).variable as TXQTermVariable);
    result.parameters[i].namespace:=v.namespace;
    result.parameters[i].name:=v.value;
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
end;

function TXQTermDefineFunction.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusDocument, xqcdFocusOther];
end;

