function xqFunctionFunction_lookup(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  qname: TXQValueQName;
  temp: TXQTermDefineFunction;
  //funcbody: TXQTermNamedFunction;
  resfunc: TXQValueFunction;
  i: Integer;
begin
  requiredArgCount(args, 2);
  requiredArgType(args[0], baseSchema.QName);
  qname := args[0] as TXQValueQName;
  temp := TXQTermDefineFunction.createReference(TNamespace.create(qname.url, qname.prefix), qname.local, args[1].toInt64);
  try
    result := temp.evaluate(context);
  except
    on e: EXQEvaluationException do
      if e.errorCode = 'XPST0017' then result := xqvalue() //todo: do not use exceptions for control flow
      else raise;
  end;
  if result.getSequenceCount > 0 then begin
    resfunc := (result as TXQValueFunction);
    if pointer(resfunc.name) = pointer(qname.local) {
        if the pointer is the same, it is the name we passed to createReference, and the function was created by temp: TXQTermDefineFunction;
        If it is not the same, the name of the function comes from somewhere else, i.e. the entire function comes from somewhere else
        In former case the function signature (types) have to be freed by our return value, in latter case it is freed by the original source.
        }
    then begin
      include(resfunc.owningFlags, xqfofSignature);
      {it is already init? funcbody := resfunc.body as TXQTermNamedFunction;
      try
        funcbody.init(context);
      except
        on e: EXQEvaluationException do
          if e.errorCode = 'XPST0017' then result := xqvalue() //todo: do not use exceptions for control flow
          else raise;
      end;}
      for i := 0 to temp.parameterCount - 1 do begin
        TXQTermDefineVariable(temp.children[i]).variable := nil;
        setlength(TXQTermDefineVariable(temp.children[i]).children, 0);
      end;
      if (length(temp.children) > 0) and (temp.children[high(temp.children)-1] is TXQTermSequenceType) then
        temp.children[high(temp.children)-1] := nil;
    end;
  end;
  temp.free;
end;

function xqFunctionFunction_name(const args: TXQVArray): IXQValue;
var
  f: TXQValueFunction;
begin
  requiredArgCount(args, 1);
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].debugAsStringWithTypeAnnotation());
  f := args[0] as TXQValueFunction;
  if f.name = '' then exit(xqvalue);
  result := TXQValueQName.create(f.namespace, f.name);
end;

function xqFunctionFunction_arity(const args: TXQVArray): IXQValue;
var
  f: TXQValueFunction;
begin
  requiredArgCount(args, 1);
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].debugAsStringWithTypeAnnotation());
  f := args[0] as TXQValueFunction;
  result := xqvalue(length(f.parameters));
end;



function xqFunctionFold(left: boolean; const args: TXQVArray): IXQValue;
var
  baseType: TXSType;
  func: TXQValueFunction;
  newargs: TXQVArray;
  count: Integer;
  v: IXQValue;
  i: Integer;
begin
  requiredArgCount(args, 3);
  func := args[2] as TXQValueFunction;
  count := args[0].getSequenceCount;
  if count = 0 then exit(args[1]);
  SetLength(newargs, 2);
  if left then begin
    //fn:fold-left(fn:tail($seq), $f($zero, fn:head($seq)), $f)
    newargs[0] := args[1];
    for v in args[0] do begin
      newargs[1] := v;
      newargs[0] := func.evaluate(newargs);
    end;
    result := newargs[0];
  end else begin
    //$f(fn:head($seq), fn:fold-right(fn:tail($seq), $zero, $f))
    newargs[1] := args[1];
    for i := count downto 1 do begin
      newargs[0] := args[0].getChild(i);
      newargs[1] := func.evaluate(newargs);
    end;
    result := newargs[1];
  end;
end;


function xqFunctionFold_left(const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(true, args);
end;

function xqFunctionFold_right(const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(false, args);
end;


function xqFunctionFor_each_pair(const args: TXQVArray): IXQValue;
var
  seq1: TXQValue;
  seq2: TXQValue;
  func: TXQValueFunction;
  newargs: TXQVArray;
  count: Integer;
  resseq: TXQValueSequence;
  i: Integer;
begin
  requiredArgCount(args, 3);
  seq1 := args[0] as TXQValue;
  seq2 := args[1] as TXQValue;
  func := args[2] as TXQValueFunction;

  count := min(seq1.getSequenceCount, seq2.getSequenceCount);
  SetLength(newargs, 2);
  resseq := TXQValueSequence.create(count);
  for i := 1 to count do begin
    newargs[0] := seq1.getChild(i);
    newargs[1] := seq2.getChild(i);
    resseq.addChild(func.evaluate(newargs));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

