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
  result := [xqcdFocusDocument, xqcdFocusOther, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
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

procedure TXQTerm.raiseParsingError(const s: string);
begin
  raise EXQParsingException.Create(s);
end;

procedure TXQTerm.raiseEvaluationError(const s: string);
begin
  raise EXQEvaluationException.Create(s);
end;

procedure TXQTerm.evaluateChildren(const context: TXQEvaluationContext; out results: TXQVArray);
var
  i: Integer;
begin
  setlength(results, length(children));
  for i:=0 to high(children) do
    results[i] := children[i].evaluate(context);
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
  obj: IXQValue;
begin
  if length(children) = 0 then raiseEvaluationError('Found no term to read property from');
  if length(children) > 1 then raiseEvaluationError('Can only read property from one term');
  obj:=children[0].evaluate(context);
  if not (obj is TXQValueObject) then raiseEvaluationError('Expected object, got: '+obj.toString);
  result := (obj as TXQValueObject).getProperty(propname);
end;

{ TXQTermIf }

function TXQTermIf.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if length(children)<>3 then raiseEvaluationError('if must have 3 arguments');
  if children[0].evaluate(context).toBooleanEffective then result:=children[1].evaluate(context)
  else result:=children[2].evaluate(context);
end;

{ TXQTermFilterSequence }

constructor TXQTermFilterSequence.create(seq: TXQTerm; filter: TXQTerm);
begin
  push(seq);
  push(filter);
end;

function TXQTermFilterSequence.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if length(children) <> 2 then raiseEvaluationError('Invalid [] sequence filter');
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
  for i := 0 to high(children) do
    result += children[i].getContextDependencies;
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

constructor TXQTermDefineVariable.create(avarname: string; anamespace: INamespace);
begin
  variablename := avarname;
  namespace := anamespace;
end;

constructor TXQTermDefineVariable.create(varname: TXQTerm; anamespace: INamespace; value: TXQTerm);
begin
  if varname is TXQTermNodeMatcher then variablename := TXQTermNodeMatcher(varname).debugTermToString
  else if varname is TXQTermVariable then variablename := TXQTermVariable(varname).value
  else raiseParsingError('Invalid term to assign a variable to: '+varname.debugTermToString);
  varname.Free;
  namespace := anamespace;
  if value <> nil then push(value);
end;

function TXQTermDefineVariable.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  //this is never called for true XQuery variables, they are handled from the module term.
  if not assigned(context.staticContext.sender.OnDefineVariable) then raiseEvaluationError(':= can''t be used without a method assigned to the OnDefineVariable event');
  if length(children) = 0 then raiseEvaluationError('Value missing');
  if length(children) > 1 then raiseEvaluationError('Only one value can be assigned to a variable');
  result := children[0].evaluate(context);

  if namespace <> nil then context.staticContext.sender.OnDefineVariable(self, namespace.getPrefix + ':' + variablename, result)
  else context.staticContext.sender.OnDefineVariable(self, variablename, result);
end;

function TXQTermDefineVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
  if length(children) > 1 then result := children[high(children)].getContextDependencies;
  result += [xqcdContextVariables];
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
  else if v is TXQValueSequence then result := (v as TXQValueSequence).seq[0].canConvertToType(atomicTypeInfo)
  else result := v.canConvertToType(atomicTypeInfo)
end;

function TXQTermSequenceType.castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
var
  temp: String;
  ns: INamespace;
begin
  v := xqvalueAtomize(v);
  if not castableAsBase(v) then raiseEvaluationError('impossible cast as');
  if not atomicTypeInfo.InheritsFrom(TXQValue_QName) then result := atomicTypeInfo.createFromValue(v)
  else begin
    temp := trim(v.toString);
    context.splitRawQName(ns, temp, xqdnkAny);
    result := qnameMake(ns, temp, TXQValueStringClass(atomicTypeInfo));
  end;
end;

function TXQTermSequenceType.castableAs(v: IXQValue): boolean;
begin
  if kind <> tikAtomic then raiseEvaluationError('need atomic type for castable as');
  v := xqvalueAtomize(v);
  result := castableAsBase(v);
end;

function TXQTermSequenceType.instanceOf(ta: IXQValue; const context: TXQEvaluationContext): boolean;
var nodeCondition: TXQPathNodeCondition;

  function instanceOfSingleType(sub: IXQValue): boolean;
  begin
    case kind of
      //tikAny, tikNone: ; //handled before
      tikAtomic:
        if atomicTypeInfo = TXQValueNumericPseudoType then
          result := (sub is TXQValueInt65) or (sub is TXQValueDecimal)
         else
          result := sub.instanceOfInternal(atomicTypeInfo);
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
  for i := 0 to high(children) do
    tempSeq.addChild(children[i].evaluate(context));
  result := tempSeq;
end;

function TXQTermSequence.getContextDependencies: TXQContextDependencies;
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
    if not (children[pos] is TXQTermVariable) then raiseEvaluationError('Expected variable, but got '+children[pos].debugTermToString);
    if children[pos + 1] is TXQTermSequenceType then typeDelta:=1
    else typeDelta:=0;

    result := isEvery;
    for v in children[pos+1+typeDelta].evaluate(tempContext) do begin
      if typeDelta <> 0 then
        if not TXQTermSequenceType(children[pos+typeDelta]).instanceOf(v, context) then
          raiseEvaluationError('Invalid type for variable '+ v.toString+ ' expected ' + TXQTermSequenceType(children[pos+typeDelta]).serialize);
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
    tempContext.temporaryVariables.allowObjects:=context.staticContext.sender.VariableChangelog.allowObjects;
  end;
  result:=xqvalue(evaluateSomeEvery(0));
  xqvalueSeqSqueeze(result);
  if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables) //iff context.temporaryVariables = nil, we created the change log here
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
      if ((v is TXQValueDecimal) and IsNan(v.toDecimal)) and not w.isUndefined and not ((w is TXQValueDecimal) and IsNan(w.toDecimal)) then
        exit(true);
    end else begin
      if w.isUndefined and not v.isUndefined then
        exit(true);
      if ((w is TXQValueDecimal) and IsNan(w.toDecimal)) and not v.isUndefined and not ((v is TXQValueDecimal) and IsNan(v.toDecimal)) then
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
        raiseEvaluationError('Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+v.toString);

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
    tempContext.temporaryVariables.allowObjects:=context.staticContext.sender.VariableChangelog.allowObjects;
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
    sortingCache.free;
  end;

  if finalResult = nil then result := TXQValueUndefined.create
  else result := finalResult;
  xqvalueSeqSqueeze(result);
  if context.temporaryVariables = nil then FreeAndNil(tempContext.temporaryVariables) //iff context.temporaryVariables = nil, we created the change log here
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
  else if pos(':', name) = 0 then
     if sc.defaultElementTypeNamespace <> nil then begin
       if findKindIndex(sc.defaultElementTypeNamespace, localName, akind, afunc) then exit(TXQTermNamedFunction.create(akind, afunc))
     end else if sc.defaultTypeNamespace <> nil then
       if findKindIndex(sc.defaultTypeNamespace, localName, akind, afunc) then exit(TXQTermNamedFunction.create(akind, afunc));
  result := TXQTermNamedFunction.create(ns, localName); //cannot know if the function is valid yet
end;

function TXQTermNamedFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
  function functionTypeConversion(const v: IXQValue; const typ: TXQTermSequenceType): IXQValue;
    function conversionSingle(const w: IXQValue): IXQValue;
    begin
      result := w;
      if result is TXQValueNode then result := xqvalueAtomize(result);
      if typ.instanceOf(result, context) then exit;
      if (result is TXQValue_untypedAtomic)
         or (typ.atomicTypeInfo.InheritsFrom(TXQValue_double) and ((w is TXQValue_float) or (w is TXQValue_double)))
         or ((w.instanceOfInternal(TXQValueDecimal) and (typ.atomicTypeInfo.InheritsFrom(TXQValue_double) or typ.atomicTypeInfo.InheritsFrom(TXQValue_float) )) )
         or (w.instanceOfInternal(TXQValue_anyURI) and (typ.atomicTypeInfo.instanceOf(TXQValueString))) then
           exit(typ.castAs(result, context));
      raiseEvaluationError('Invalid type for function. Expected '+typ.serialize+' got '+w.debugAsStringWithTypeAnnotation());
    end;

  var
    i: Integer;
  begin
    result := v;
    if typ = nil then exit;
    if typ.instanceOf(result, context) then exit;
    if typ.kind = tikAtomic then begin
      if not (result is TXQValueSequence) then
        exit(conversionSingle(result));
      if ((not typ.allowMultiple) and (result.getSequenceCount > 1)) then raiseEvaluationError('Expected singleton, but got sequence: '+result.debugAsStringWithTypeAnnotation());
      if ((not typ.allowNone) and (result.getSequenceCount = 0)) then raiseEvaluationError('Expected value, but got empty sequence.');
      for i := 0 to result.getSequenceCount - 1 do
        (result as TXQValueSequence).seq[i] := conversionSingle((result as TXQValueSequence).seq[i]);
    end;
  end;

var
  evilkids: TXQVArray;
  tempcontext: TXQEvaluationContext;
  i: Integer;
  j: Integer;
  vfunc: TXQValueFunction;
  sc: TXQStaticContext;
  ns: INamespace;
  temp: String;
begin
  evaluateChildren(context, evilkids);

  assert((kind = xqfkUnknown) or (func <> nil));

  if context.staticContext.strictTypeChecking and (kind in [xqfkBasic, xqfkComplex]) then
    if not func.checkTypes(evilkids, context) then
      raiseEvaluationError('Invalid types for function call: '+func.versions[0].name);


  case kind of
    xqfkBasic:
      result := TXQBasicFunctionInfo(func).func(evilkids);
    xqfkComplex:
      result := TXQComplexFunctionInfo(func).func(context, evilkids);
    xqfkWrappedOperator: begin
      if length(evilkids) <> 2 then raiseEvaluationError('Need two arguments for function: op:' + funcname);
      result := TXQOperatorInfo(func).func(context, evilkids[0], evilkids[1]);
    end;
    xqfkTypeConstructor:
      if TXQValueClass(func).InheritsFrom(TXQValue_QName) then begin
        temp := evilkids[0].toString;
        context.splitRawQName(ns, temp, xqdnkAny);
        result := qnameMake(ns, temp,  TXQValueStringClass(func));
      end else if length(evilkids) = 0 then result := TXQValueClass(func).createFromValue(nil)
      else if length(evilkids) = 1 then result := TXQValueClass(func).createFromValue(evilkids[0])
      else raiseEvaluationError('Invalid argument count for constructor function');
    xqfkUnknown: begin
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
        raiseEvaluationError('Function '+funcname+' not found');
      end;

      tempcontext := context;
      tempcontext.temporaryVariables := TXQVariableChangeLog.create();
      tempcontext.staticContext := sc;
      for j := 0 to high(vfunc.parameters) do
        tempcontext.temporaryVariables.add(vfunc.parameters[j].name, TXQAbstractFunctionInfo.convertType(evilkids[j], vfunc.parameters[j].seqtype, context), vfunc.parameters[j].namespace);
      result := vfunc.body.evaluate(tempcontext);
      result := TXQAbstractFunctionInfo.convertType(result, vfunc.resulttype, context);
      tempcontext.temporaryVariables.free;
      exit;
    end;
  end;
end;

class function TXQTermNamedFunction.findKindIndex(const ns: INamespace; const name: string; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
var
  i, aindex: Integer;
  module: TXQNativeModule;
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

  aindex := module.binaryOpFunctions.IndexOf(name);
  if aindex >= 0 then begin
    afunc := TXQOperatorInfo(module.binaryOpFunctions.Objects[aindex]);
    akind := xqfkWrappedOperator;
    exit;
  end;

  aindex:=module.types.IndexOf(name);
  if aindex >= 0 then begin
    akind:=xqfkTypeConstructor;
    afunc := TXQAbstractFunctionInfo(module.types.Objects[aindex]);
    exit(true)
  end;

  exit(false);
end;



{ TXQTermBinaryOp }

constructor TXQTermBinaryOp.create(const aop: string; arg1: TXQTerm; arg2: TXQTerm);
begin
  op := TXQueryEngine.findOperator(aop);
  if arg1 <> nil then begin
    push(arg1);
    if arg2 <> nil then push(arg2);
  end;
end;

constructor TXQTermBinaryOp.create(arg1: TXQTerm; const aop: string; arg2: TXQTerm);
begin
  create(aop, arg1, arg2);
end;

function TXQTermBinaryOp.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if length(children)<2 then raiseEvaluationError('Not enough subterms');
  if length(children)>2 then raiseEvaluationError('Too many subterms');


  if (op.name = '/') or (op.name = '//') then
    exit(TXQueryEngine.evaluateAccessList(self, context));

  result := op.func(context, children[0].evaluate(context), children[1].evaluate(context));
end;

function TXQTermBinaryOp.getContextDependencies: TXQContextDependencies;
begin
    result := [xqcdContextCollation, xqcdContextTime, xqcdContextOther] + children[0].getContextDependencies + children[1].getContextDependencies;
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
  if (select = '.') and (context.SeqValue <> nil) then result := context.SeqValue.clone
  else if func and (axis = '') and (select = 'text') and (context.staticContext.sender.TextElement<>nil) then result := xqvalue(context.staticContext.sender.TextElement)
  else begin
    result := TXQueryEngine.evaluateSingleStepQuery(toQueryCommand,context);
    xqvalueSeqSqueeze(result);
  end;
end;

function TXQTermNodeMatcher.getContextDependencies: TXQContextDependencies;
begin
    result := [xqcdFocusDocument, xqcdFocusOther, xqcdContextCollation, xqcdContextOther];
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
     raise EXQEvaluationException.Create('Unknown axis: '+axis);
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

{ TXQTermUnaryOp }

constructor TXQTermUnaryOp.create(const aop: string; arg: TXQTerm);
begin
  op := TXQueryEngine.findOperator(aop);
  if arg <> nil then push(arg);
end;

function TXQTermUnaryOp.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if length(children)<1 then raiseEvaluationError( 'Not enough subterms')
  else if length(children)>1 then raiseEvaluationError('Too many subterms');
  if op = nil then raiseEvaluationError('Unknown unary op');
  result := xqvalue();
  result := op.func(context, xqvalue(0),children[0].evaluate(context));
end;

function TXQTermUnaryOp.getContextDependencies: TXQContextDependencies;
begin
  result := children[0].getContextDependencies;
end;

{ TXQTermNumber }

constructor TXQTermNumber.create(const avalue: string);
begin
  if strContains(avalue,'e') or strContains(avalue,'E') then value := TXQValue_double.create(StrToFloat(avalue,XQFormats))
  else if strContains(avalue,'.') then value := xqvalue(StrToFloat(avalue,XQFormats))
  else value := xqvalue(StrToInt65(avalue));
end;

function TXQTermNumber.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  result := value.clone;
end;

function TXQTermNumber.getContextDependencies: TXQContextDependencies;
begin
  Result:=[];
end;

{ TXQTermReadAttribute }

constructor TXQTermReadAttribute.create(avalue: string; func: boolean);
begin
  if func then begin
    if pos('::', avalue) > 0 then if strSplitGet('::', avalue) <> 'attribute' then raiseParsingError('Invalid axis for attribute test');
    if (avalue <> 'attribute') and (avalue <> 'node') then raiseParsingError('invalid attribute test: '+avalue);
    value := '*';
  end else begin
    if pos(':', avalue) > 0 then namespace := strSplitGet(':', avalue);
    value := avalue;
  end;
end;

function TXQTermReadAttribute.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  tempEl: TTreeElement;
  testValue: Boolean;
  testNS: Boolean;
  readvalue: String;
  readnamespace: String;
  temp: INamespace;
  i: Integer;
  attrib: TTreeAttribute;
begin
  if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then
    tempEl := context.SeqValue.toNode
   else
    tempEl:=context.ParentElement;
  if tempEl.typ = tetProcessingInstruction then
    exit(xqvalue);

  readvalue := value;
  readnamespace := namespace;
  if length(children) > 0 then begin
    if not (children[0] is TXQTermNodeMatcher) then raiseEvaluationError('Invalid subtype');
    readvalue := TXQTermNodeMatcher(children[0]).select;
    readnamespace := TXQTermNodeMatcher(children[0]).namespace;
  end;

  result := nil;
  if tempEl.attributes <> nil then begin
    testValue := readvalue <> '*';
    testNS := (readnamespace <> '*') and (testValue or (readnamespace <> ''));
    if testNS then begin
      temp := context.findNamespace(readnamespace, xqdnkUnknown);
      if temp <> nil then readnamespace:=temp.getURL;
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
    result := [xqcdFocusDocument, xqcdFocusOther, xqcdContextCollation, xqcdContextOther];
end;

{ TXQTermVariable }

constructor TXQTermVariable.create(const avalue: string; staticContext: TXQStaticContext);
begin
  value := avalue;
  if value = '' then raiseEvaluationError('variable with empty name');
  //if value[1] = '$' then raiseEvaluationError('variable name starts with a double $:  ' + value);
  if value[length(value)] = ';' then delete(value, length(value), 1);
  if staticContext <> nil then staticContext.splitRawQName(namespace, value, xqdnkUnknown);
end;

function TXQTermVariable.evaluate(const context: TXQEvaluationContext): IXQValue;
var
  temp: TXQValue;
  sc: TXQStaticContext;
  ns: INamespace;
begin
  ns := namespace;
  if context.temporaryVariables <> nil then begin
    temp := nil;
    if context.temporaryVariables.hasVariableOrObject(value, @temp, ns) then begin
      if temp = nil then result := xqvalue()
      else result := temp;
      exit();
    end;
  end;
  sc := context.findModuleStaticContext(ns);
  if (sc <> nil) and (sc.moduleVariables <> nil) then begin
    temp := nil;
    if sc.moduleVariables.hasVariableOrObject(value, @temp, ns) then begin
      if temp = nil then result := xqvalue()
      else result := temp;
      exit();
    end;
  end;
  if value = '$' then result := xqvalue('$') //default $$; as $
  else if value = 'line-ending' then result := xqvalue(LineEnding) //default $line-ending; as #13#10
  else result := xqvalue();
  if assigned(context.staticContext.sender.OnEvaluateVariable) then context.staticContext.sender.OnEvaluateVariable(self, value, result );
end;

function TXQTermVariable.getContextDependencies: TXQContextDependencies;
begin
  Result:=[xqcdContextVariables];
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
    if not (children[i] is TXQTermSequence) then raiseEvaluationError('Invalid case clause');
    if children[i].children[0] is TXQTermVariable then start := 1
    else start := 0;
    for j := start to high(children[i].children) - 1 do
      if (children[i].children[j] as TXQTermSequenceType).instanceOf(v, context) then
        exit(evaluateWithChangedVariable(children[i].children[high(children[i].children)], children[i].children[0], v));
  end;
  i := high(children);
  result := evaluateWithChangedVariable(children[i].children[high(children[i].children)], children[i].children[0], v);
end;

{ TXQTermConstructor }

constructor TXQTermConstructor.create(atype: TTreeElementType; aname: txqterm = nil);
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

function TXQTermConstructor.evaluate(const context: TXQEvaluationContext; root: TTreeElement; var baseOffset: longint): IXQValue;
var
  tree: TTreeElement;
  kid: TTreeElement;
  lastKid: TTreeElement;
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
    doc: TTreeElement;
    n: TTreeElement;
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
      else raiseEvaluationError('Unknown child node');
    end;
  end;

  procedure addValue(const v: IXQValue; firstAtomic: boolean = true);
  var x: IXQValue;
      temp: TTreeElement;
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
        kid := TTreeElement.create(tetText);
        if firstAtomic then kid.value:=v.toString
        else kid.value := ' '+ v.toString;
        kid.offset:=baseOffset;
        baseOffset+=1;
        addKid;
      end else begin
        if lastKid.typ <> tetText then raiseEvaluationError('Internal error: No atomic value before this one exist, but this one is not the first atomic value.');
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
    tempqname: TStringArray;
  begin
    if (v is TXQValue_QName) and ((defkind = xqdnkElementType) or   strContains((v as TXQValueString).str, #2)) then begin
      tempqname := qnameSplit((v as TXQValueString).str);
      if (tempqname[1] = '') and (tempqname[0] = XMLNamespaceUrl_XML) then tempqname[1] := 'xml';
      if (tempqname[1] = '') and (defkind <> xqdnkElementType) then tempqname[1] := chooseRandomPrefix;
      tree.namespace := declareNewNamespace(tempqname[0], tempqname[1]);
      tree.value:=tempqname[2];
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
  else tree := TTreeElement.create(typ);
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

      kid := TTreeElement.create(tetClose, tree.value);
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
    else raiseEvaluationError('Invalid type for constructor');
  end;
  result := xqvalue(tree);
  if subcontext <> @context then begin
    subcontext^.namespaces.free;
    Dispose(subcontext);
  end else if context.namespaces <> nil then
    for i:=oldnamespacecount to context.namespaces.Count-1 do
      context.namespaces.Delete(context.namespaces.Count-1);
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


{ TXQTermDefineFunction }

constructor TXQTermDefineFunction.create(aname: string);
begin
  funcname:=aname;
end;

function TXQTermDefineFunction.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  raiseEvaluationError('Dead branch called');
  ignore(context);
  result := nil;
end;

function TXQTermDefineFunction.define(): TXQValueFunction;
var
  i: Integer;
begin
  result := TXQValueFunction.create();
  result.namespace := namespace;
  result.name:=funcname;
  setlength(result.parameters, parameterCount);
  for i := 0 to high(result.parameters) do begin
    result.parameters[i].namespace:=(children[i] as TXQTermDefineVariable).namespace;
    result.parameters[i].name:=(children[i] as TXQTermDefineVariable).variablename;
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
  result := [xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
end;

