
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


function xqFunctionFoldLeft(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(true, args);
end;

function xqFunctionFoldRight(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionFold(true, args);
end;


function xqFunctionForEachPair(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
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

