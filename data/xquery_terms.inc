
{ TXQTermReadProperty }

constructor TXQTermReadObjectProperty.create(apropname: string);
begin
  propname := apropname;
end;

function TXQTermReadObjectProperty.evaluate(const context: TEvaluationContext): IXQValue;
var
  obj: IXQValue;
begin
  if length(children) = 0 then raiseEvaluationError('Found no term to read property from');
  if length(children) > 1 then raiseEvaluationError('Can only read property from one term');
  obj:=children[0].evaluate(context);
  if not (obj is TXQValueObject) then raiseEvaluationError('Expected object, got: '+obj.toString);
  result := (obj as TXQValueObject).getClonedValue(propname);
end;

{ TXQTermIf }

function TXQTermIf.evaluate(const context: TEvaluationContext): IXQValue;
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

function TXQTermFilterSequence.evaluate(const context: TEvaluationContext): IXQValue;
begin
  if length(children) <> 2 then raiseEvaluationError('Invalid [] sequence filter');
  result := children[0].evaluate(context);
  TXQueryEngine.filterSequence(result, children[1], context);
  if result = nil then result := xqvalue()
  else xqvalueSeqSqueeze(result);
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

constructor TXQTermDefineVariable.create(avarname: string; anamespace: TNamespace);
begin
  variablename := avarname;
  namespace := anamespace;
end;

constructor TXQTermDefineVariable.create(varname: TXQTerm; anamespace: TNamespace; value: TXQTerm);
begin
  if varname is TXQTermNodeMatcher then variablename := TXQTermNodeMatcher(varname).debugTermToString
  else if varname is TXQTermVariable then variablename := TXQTermVariable(varname).value
  else raiseParsingError('Invalid term to assign a variable to: '+varname.debugTermToString);
  varname.Free;
  namespace := anamespace;
  if value <> nil then push(value);
end;

function TXQTermDefineVariable.evaluate(const context: TEvaluationContext): IXQValue;
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


function TXQTermSequenceType.evaluate(const context: TEvaluationContext): IXQValue;
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

function TXQTermSequenceType.castAs(v: IXQValue): IXQValue;
begin
  if not isSingleType() then raiseEvaluationError('need singletype for cast as');
  v := xqvalueAtomize(v);
  if not castableAsBase(v) then raiseEvaluationError('impossible cast as');
  result := atomicTypeInfo.createFromValue(v);
end;

function TXQTermSequenceType.castableAs(v: IXQValue): boolean;
begin
  if not isSingleType() then raiseEvaluationError('need singletype for castable as');
  v := xqvalueAtomize(v);
  result := castableAsBase(v);
end;

function TXQTermSequenceType.instanceOf(ta: IXQValue; const context: TEvaluationContext): boolean;
var nodeCondition: TXQPathNodeCondition;

  function instanceOfSingleType(sub: IXQValue): boolean;
  begin
    case kind of
      //tikAny, tikNone: ; //handled before
      tikAtomic: result := sub.instanceOfInternal(atomicTypeInfo);
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
    if nodeCondition.checkNamespace then
      nodeCondition.requiredNamespaceURL:=context.findNamespaceURL(nodeCondition.requiredNamespaceURL, xqdnkElementType);
    nodeCondition.equalFunction := @context.staticContext.nodeCollation.equal;
  end;
  result := true;
  for v in ta do begin
    result := result and instanceOfSingleType(v);
    if not result then exit;
  end;
end;
{ TXQTermSequence }

function TXQTermSequence.evaluate(const context: TEvaluationContext): IXQValue;
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

function TXQTermSomeEvery.evaluate(const context: TEvaluationContext): IXQValue;
var tempContext:TEvaluationContext;
  function evaluateSomeEvery(const pos:integer): boolean;
  var v: IXQValue;
  begin
    if pos = high(children) then exit(children[pos].evaluate(tempContext).toBooleanEffective);
    if not (children[pos] is TXQTermVariable) then raiseEvaluationError('Expected variable, but got '+children[pos].debugTermToString);

    result := isEvery;
    for v in children[pos+1].evaluate(tempContext) do begin
      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.addVariable(TXQTermVariable(children[pos]).value, v, TXQTermVariable(children[pos]).namespace);
      if evaluateSomeEvery(pos+2) <> (isEvery) then begin
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
    context: ^TEvaluationContext;
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

function TXQTermFlower.evaluate(const context: TEvaluationContext): IXQValue;
var
  tempContext: TEvaluationContext;
  sortingCache: TFPList;
  finalResult: IXQValue;

  procedure evaluateForLoop(const pos:integer);
    procedure continueWithValue(v: IXQValue; index: integer = 1);
    begin
      tempContext.temporaryVariables.pushAll;
      //sequenceTyp: TXQTermType;
      tempContext.temporaryVariables.addVariable(vars[pos].varname, v, vars[pos].namespace);
      if (vars[pos].sequenceTyp <> nil) and not (vars[pos].sequenceTyp.instanceOf(v, context)) then
        raiseEvaluationError('Invalid variable type, expected: type '+vars[pos].sequenceTyp.serialize+' got value '+v.toString);
      if vars[pos].positionVarname <> '' then ;
        tempContext.temporaryVariables.addVariable(vars[pos].positionVarname, xqvalue(index), vars[pos].positionVarNamespace);
      evaluateForLoop(pos+1);
      tempContext.temporaryVariables.popAll;
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

  evaluateForLoop(0);

  if sortingCache <> nil then begin
    if sortingCache.Count > 0 then begin
      sortingData.flower:=self;
      sortingData.context:=@tempContext;
      setlength(sortingData.collations, length(orders));
      for i := 0 to high(orders) do
        if orders[i].collation = '' then sortingData.collations[i] := nil
        else sortingData.collations[i] := TXQueryEngine.getCollation(orders[i].collation);
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

constructor TXQTermNamedFunction.create(const akind: TXQTermNamedFunctionKind; const aindex: integer);
begin
  kind := akind;
  index := aindex;
end;

constructor TXQTermNamedFunction.create(const ns: TNamespace; const name: string);
begin
  if not findKindIndex(ns, name, kind, index) then begin
    kind := xqfkUnknown;
    funcname:=name;
    namespace := ns;
  end;
end;

constructor TXQTermNamedFunction.create(const ns: TNamespace; const name: string; args: array of TXQTerm);
begin
  create(ns, name);
  push(args);
end;

class function TXQTermNamedFunction.createIfExists(const name: string; const sc: TXQStaticContext): TXQTermNamedFunction;
var akind: TXQTermNamedFunctionKind;
    aindex: integer;
    localName: string;
    ns: TNamespace;
begin
  localName:=name;
  sc.splitRawQName(ns, localName, xqdnkFunction);
  if findKindIndex(ns, localName, akind, aindex) then result := TXQTermNamedFunction.Create(akind, aindex)
  else result := TXQTermNamedFunction.create(ns, localName); //cannot know if the function is valid yet
end;

function TXQTermNamedFunction.evaluate(const context: TEvaluationContext): IXQValue;
var
  evilkids: TXQVArray;
  tempcontext: TEvaluationContext;
  i: Integer;
  j: Integer;
  func: TXQValueFunction;
  moduleIndex: Integer;
  sc: TXQStaticContext;
  ns: TNamespace;
begin
  evaluateChildren(context, evilkids);

  assert((kind = xqfkUnknown) or (index >= 0));

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
      if length(evilkids) = 0 then result := TXQValueClass(types.Objects[index]).createFromValue(nil)
      else if length(evilkids) = 1 then result := TXQValueClass(types.Objects[index]).createFromValue(evilkids[0])
      else raiseEvaluationError('Invalid argument count for constructor function');
    xqfkUnknown: begin
      func := nil;
      ns := namespace;
      sc := context.findModuleStaticContext(ns);
      if sc <> nil then
        for i := 0 to high(sc.functions) do
          if (sc.functions[i].name = funcname)
             and  (length(sc.functions[i].parameters) = length(children))
             and ((sc.functions[i].namespace = ns) or ((ns <> nil) and (sc.functions[i].namespace <> nil) and (sc.functions[i].namespace.url = ns.url))) then begin
            func := sc.functions[i];
            break;
          end;
      if func = nil then
        raiseEvaluationError('Function '+funcname+' not found');

      tempcontext := context;
      tempcontext.temporaryVariables := TXQVariableChangeLog.create();
      tempcontext.staticContext := sc;
      for j := 0 to high(func.parameters) do
        tempcontext.temporaryVariables.addVariable(func.parameters[j].name, evilkids[j]);
      result := func.body.evaluate(tempcontext);
      tempcontext.temporaryVariables.free;
      exit;
    end;
  end;
end;

procedure xqFunctionGeneralConstructor(args: TXQVArray; var result: IXQValue);forward;

class function TXQTermNamedFunction.findKindIndex(const ns: TNamespace; const name: string; out akind: TXQTermNamedFunctionKind; out aindex: integer): boolean;
var
  i: Integer;
begin
  if (ns = nil) then exit(false);

  if (ns.url = XMLNamespaceURL_XPathFunctions) or (ns.url = XMLNamespaceURL_MyExtensions) then begin
    aindex := basicFunctions.IndexOf(name);
    if aindex >= 0 then begin
      if PXQBasicFunctionInfo(basicFunctions.Objects[aindex])^.func = @xqfunctionGeneralConstructor then begin
        if ns.url <> XMLNamespaceURL_MyExtensions then exit(false);
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
  end;
  if (ns.url = XMLNamespaceURL_XMLSchema) or (ns.url = XMLNamespaceURL_MyExtensions)  then begin
    aindex := basicFunctions.IndexOf('xs:'+name);
    if aindex >= 0 then
      if PXQBasicFunctionInfo(basicFunctions.Objects[aindex])^.func = @xqfunctionGeneralConstructor then begin
        akind:=xqfkTypeConstructor;
        aindex:=types.indexof(name);;
        exit(true);
      end;

    exit(false);
  end else if ns.url = XMLNamespaceURL_MyExtensionOperators then begin;
    aindex := -1;
    for i:=0 to  binaryOpFunctions.Count-1 do
      if strContains(name, binaryOpFunctions[i]) then begin
        aindex := i;
        akind := xqfkWrappedOperator;
        exit(true);
      end;;
  end;

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

function TXQTermBinaryOp.evaluate(const context: TEvaluationContext): IXQValue;
begin
  if length(children)<2 then raiseEvaluationError('Not enough subterms');
  if length(children)>2 then raiseEvaluationError('Too many subterms');

  result := xqvalue();

  if (index = binaryOps.IndexOf('/')) or (index = binaryOps.IndexOf('//')) then begin
    TXQueryEngine.evaluateAccessList(self, context, result);
    exit;
  end;

  operatorInfo^.func(context, children[0].evaluate(context), children[1].evaluate(context),result);
end;

function TXQTermBinaryOp.getContextDependencies: TXQContextDependencies;
begin
    result := [xqcdContextCollation, xqcdContextTime, xqcdContextOther] + children[0].getContextDependencies + children[1].getContextDependencies;
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
  hadNamespace := strContains(select,':');
  if hadNamespace then  namespace:=strSplitGet(':',select);
  func := asfunction;
end;

function TXQTermNodeMatcher.evaluate(const context: TEvaluationContext): IXQValue;
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
  case select of
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

constructor TXQTermUnaryOp.create(const op: string; arg: TXQTerm);
begin
  index := binaryOps.IndexOf(op);
  if arg <> nil then push(arg);
end;

function TXQTermUnaryOp.evaluate(const context: TEvaluationContext): IXQValue;
begin
  if length(children)<1 then raiseEvaluationError( 'Not enough subterms')
  else if length(children)>1 then raiseEvaluationError('Too many subterms');
  if index < 0 then raiseEvaluationError('Unknown unary op');
  result := xqvalue();
  PXQOperatorInfo(binaryOps.Objects[index])^.func(context, xqvalue(0),children[0].evaluate(context),result);
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

function TXQTermNumber.evaluate(const context: TEvaluationContext): IXQValue;
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

function TXQTermReadAttribute.evaluate(const context: TEvaluationContext): IXQValue;
var
  tempEl: TTreeElement;
  testValue: Boolean;
  testNS: Boolean;
  readvalue: String;
  readnamespace: String;
  temp: TNamespace;
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
  tempEl := tempEl.attributes;
  testValue := readvalue <> '*';
  testNS := (readnamespace <> '*') and (testValue or (readnamespace <> ''));
  if testNS then begin
    temp := context.findNamespace(readnamespace, xqdnkUnknown);
    if temp <> nil then readnamespace:=temp.url;
  end;
  while tempEl <> nil do begin
    if      (not testValue or striEqual(tempEl.value, readvalue))
        and (not testNS or ( tempEl.getNamespaceURL() = readnamespace))  then
      xqvalueSeqAdd(result, xqvalue(tempEl.reverse));
    tempEl := tempEl.next;
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
  if value[1] <> '$' then raiseEvaluationError('variable name does not start with $:  ' + value);
  delete(value,1,1);
  if value[length(value)] = ';' then delete(value, length(value), 1);
  if staticContext <> nil then staticContext.splitRawQName(namespace, value, xqdnkUnknown);
end;

function TXQTermVariable.evaluate(const context: TEvaluationContext): IXQValue;
var
  temp: TXQValue;
  sc: TXQStaticContext;
  ns: TNamespace;
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

function TXQTermString.evaluate(const context: TEvaluationContext): IXQValue;
begin
  ignore(context);
  Result := xqvalue(value);
end;

function TXQTermString.getContextDependencies: TXQContextDependencies;
begin
  result := [];
end;




{ TXQTermTypeSwitch }

function TXQTermTypeSwitch.evaluate(const context: TEvaluationContext): IXQValue;
  function evaluateWithChangedVariable(eval, vari: TXQTerm; const value: IXQValue): IXQValue;
  var
    tempContext: TEvaluationContext;
  begin
    tempContext := context;
    if vari is TXQTermVariable then begin
      if tempContext.temporaryVariables = nil then tempContext.temporaryVariables := TXQVariableChangeLog.create();
      tempContext.temporaryVariables.pushAll;
      tempContext.temporaryVariables.addVariable(TXQTermVariable(vari).value, value);
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

function TXQTermConstructor.evaluate(const context: TEvaluationContext): IXQValue;
begin
  result := evaluate(context, nil, 0);
end;

function TXQTermConstructor.evaluate(const context: TEvaluationContext; root: TTreeElement; baseOffset: longint): IXQValue;
var
  tree: TTreeElement;
  i: Integer;
  kid: TTreeElement;
  lastAttribute: TTreeElement;
  lastKid: TTreeElement;
  subcontext: ^TEvaluationContext;
  procedure addKid;
  begin
    if tree.next = nil then tree.next := kid
    else begin
      if (kid.typ = tetText) and (lastKid.typ = tetText) then begin
        lastKid.value += kid.value;
        kid.free;
        exit;
      end;
      lastKid.next := kid;
      kid.previous := lastKid;
    end;
    kid.document := root;
    kid.offset := baseOffset;
    baseOffset += 1;
    if kid.reverse <> nil then lastKid := kid.reverse
    else lastKid := kid;
  end;
  procedure addNodeAsKid;
  begin
    kid.parent := tree;
    case kid.typ of
      tetAttributeName:
        if ((kid.namespace = nil) and (kid.value = 'xmlns')) or ((kid.namespace <> nil) and (kid.namespace.url = XMLNamespaceUrl_XMLNS)) then begin
          if (subcontext^.namespaces = nil) then begin
            new(subcontext);
            subcontext^ := context;
            subcontext^.namespaces := TNamespaceList.Create;
          end;
          if kid.namespace = nil then subcontext^.namespaces.Add(TNamespace.Create(kid.reverse.value, ''))
          else subcontext^.namespaces.Add(TNamespace.Create(kid.reverse.value, kid.value));
          if subcontext^.staticContext.sender.FGarbageNamespaces = nil then subcontext^.staticContext.sender.FGarbageNamespaces := TNamespaceList.Create;
          subcontext^.staticContext.sender.FGarbageNamespaces.Add(subcontext^.namespaces[subcontext^.namespaces.Count-1]);
          kid.reverse.Free;
          kid.free;
        end else begin
          if lastAttribute = nil then tree.attributes := kid
          else begin
            lastAttribute.next := kid;
            kid.previous := lastAttribute;
            lastAttribute.reverse.next := kid.reverse;
            kid.reverse.previous := lastAttribute.reverse;
          end;
          kid.reverse.parent := tree;
          lastAttribute := kid;
          kid.offset:=baseOffset;
          kid.reverse.offset:=baseOffset;
          baseOffset+=1;
        end;
      tetOpen, tetComment, tetText, tetProcessingInstruction:
        addKid;
      else raiseEvaluationError('Unknown child node');
    end;
  end;

  procedure addValue(const v: IXQValue; firstAtomic: boolean = true);
  var x: IXQValue;
      temp: TTreeElement;
  begin
    case v.kind of
      pvkUndefined:;
      pvkNode: begin
        kid := v.toNode.clone;
        addNodeAsKid;
        if root = tree then begin
          if kid.getFirstChild() = nil then kid.document := root
          else begin
            temp := kid;
            while (temp <> nil) and (temp <> kid.reverse) do begin
              temp.document := root;
              temp.offset := baseOffset;
              if (temp.next = nil) or (temp.next.offset <= temp.offset) then baseOffset+=8
              else baseOffset += temp.next.offset - temp.offset;
              temp := temp.next;
            end;
          end;
        end;
      end;
      pvkSequence: begin
        firstAtomic := true;
        for x in v do begin
          addValue(x, firstAtomic);
          if x is TXQValueNode then firstAtomic := true
          else firstAtomic:=false;
        end;
      end
      else if firstAtomic then begin
        kid := TTreeElement.create(tetText);
        kid.parent := tree;
        kid.document := root;
        kid.value := v.toString;
        addKid;
      end else begin
        if lastKid.typ <> tetText then raiseEvaluationError('Internal error: No atomic value before this one exist, but this one is not the first atomic value.');
        lastKid.value += ' ' + v.toString;
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

var oldnamespacecount: integer;
begin
  subcontext:=@context;
  oldnamespacecount := 0;
  if subcontext^.namespaces <> nil then oldnamespacecount := subcontext^.namespaces.Count;

  if (typ = tetOpen) and (nameValue = nil) then tree := TTreeDocument.create(typ)
  else tree := TTreeElement.create(typ);
  if root = nil then root := tree;
  tree.document := root;
  if baseOffset = 0 then begin
    if context.staticContext.sender.FInternalDocuments = nil then context.staticContext.sender.FInternalDocuments := TFPList.Create;
    context.staticContext.sender.FInternalDocuments.Add(tree);
  end;
  baseOffset += 1;
  lastAttribute := nil;
  case typ of
    tetAttributeName: begin
      tree.value := nameValue.evaluate(subcontext^).toString;
      subcontext^.splitRawQName(tree.namespace, tree.value, xqdnkUnknown);
      tree.reverse := TTreeElement.create();
      tree.reverse.document := root;
      tree.reverse.reverse := tree;
      tree.reverse.typ := tetAttributeValue;
      for i:= 0 to high(children) do
        tree.reverse.value := tree.reverse.value + valueToString(children[i].evaluate(subcontext^));
    end;
    tetOpen: begin
      for i:=0 to high(children) do
        if children[i] is TXQTermConstructor then begin
          kid := TXQTermConstructor(children[i]).evaluate(subcontext^, root, baseOffset).toNode;
          addNodeAsKid;
        end else
          addValue(children[i].evaluate(subcontext^));

      if nameValue <> nil then begin
        tree.value := nameValue.evaluate(subcontext^).toString;
        subcontext^.splitRawQName(tree.namespace, tree.value, xqdnkElementType);
      end;
      kid := TTreeElement.create(tetClose, tree.value);
      kid.namespace := tree.namespace;
      addKid;
      tree.reverse := kid; kid.reverse := tree;
    end;
    tetProcessingInstruction: begin
      tree.value := nameValue.evaluate(subcontext^).toString;
      if length(children) = 0 then tree.addAttribute('', '')
      else tree.addAttribute('', valueToString(children[0].evaluate(subcontext^)));
    end;
    tetText: tree.value := valueToString(nameValue.evaluate(subcontext^));
    tetComment: tree.value := valueToString(nameValue.evaluate(subcontext^));
    else raiseEvaluationError('Invalid type for constructor');
  end;
  result := xqvalue(tree);
  if subcontext <> @context then begin
    subcontext^.namespaces.clearNoFree;
    subcontext^.namespaces.free;
    Dispose(subcontext);
  end else if context.namespaces <> nil then
    for i:=oldnamespacecount to context.namespaces.Count-1 do
      context.namespaces.Delete(i);
end;

destructor TXQTermConstructor.destroy;
begin
  nameValue.Free;
  inherited destroy;
end;


{ TXQTermDefineFunction }

constructor TXQTermDefineFunction.create(aname: string);
begin
  funcname:=aname;
end;

function TXQTermDefineFunction.evaluate(const context: TEvaluationContext): IXQValue;
var
  resfunc: TXQValueFunction;
  i: Integer;
begin
  raiseEvaluationError('Dead branch called');
{  resfunc := TXQValueFunction.create();
  i := 0;
  while (i < high(children)) and (children[i] is TXQTermDefineVariable) do begin
    setlength(resfunc.parameters, length(resfunc.parameters) + 1);
    resfunc.parameters[high(resfunc.parameters)].name:=TXQTermDefineVariable(children[i]).variablename;
    if length(children[i].children) > 0 then
      resfunc.parameters[high(resfunc.parameters)].seqtype := TXQTermDefineVariable(children[i]).children[0] as TXQTermSequenceType;
    i+=1;
  end;
  if children[i] is TXQTermSequenceType then begin
    resfunc.resulttype := children[i] as TXQTermSequenceType;
    i+=1;
  end;
  if i <> high(children) then raiseEvaluationError('Invalid function definition');
  resfunc.namespace := namespace;
  resfunc.body:=children[i];
  resfunc.context:=context;
  result := resfunc;          }
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

