unit xquery.internals.protectionbreakers;

{
Copyright (C) 2008 - 2019 Benito van der Zander (BeniBela)
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


{$mode objfpc}{$H+}{$ModeSwitch typehelpers}

interface

uses xquery, simplehtmltreeparser, classes;

type TXQBoxedDateTimeHelper = class helper for TXQBoxedDateTime
    procedure multiplyComponents(fac: xqfloat); //Multiply all components of value with fac
    procedure divideComponents(fac: xqfloat); //Multiply all components of value with fac
    procedure addDuration(const D: TXQValueDateTimeData); //Adds a duration to the current datetime/duration
    procedure subtractDuration(D: TXQValueDateTimeData);
    procedure truncateRangeH();
  end;

  TXQueryEngineHelper = class helper for TXQueryEngine
  public
    function getPatternMatcherTextStart: TTreeNode; inline;
    function getPatternMatcherTextEnd: TTreeNode; inline;
    class procedure freeCommonCachesH;
    function parserEnclosedExpressionsString(s: string): IXQuery;
    procedure addAWeirdGlobalVariableH(const namespace, local: string); inline;
    function parseTermH(const str:string; model: TXQParsingModel; context: TXQStaticContext = nil): TXQuery; inline;
    procedure setPatternMatcherTextRange(textStart, textEnd: TTreeNode);
  end;



implementation

uses math, xquery.internals.common;

function TXQueryEngineHelper.getPatternMatcherTextStart: TTreeNode;
begin
  result :=PatternMatcherTextStart;
end;

function TXQueryEngineHelper.getPatternMatcherTextEnd: TTreeNode;
begin
  result :=PatternMatcherTextEnd;
end;

class procedure TXQueryEngineHelper.freeCommonCachesH;
begin
  freeCommonCaches
end;

function TXQueryEngineHelper.parserEnclosedExpressionsString(s: string): IXQuery;
begin
  result := parseXStringNullTerminated(s);
end;

procedure TXQueryEngineHelper.addAWeirdGlobalVariableH(const namespace, local: string);
begin
  addAWeirdGlobalVariable(namespace, local);
end;

function TXQueryEngineHelper.parseTermH(const str: string; model: TXQParsingModel; context: TXQStaticContext): TXQuery;
begin
  result := parseTerm(str, model, context)
end;

procedure TXQueryEngineHelper.setPatternMatcherTextRange(textStart, textEnd: TTreeNode);
begin
  PatternMatcherTextStart := textStart;
  PatternMatcherTextEnd := textEnd;
end;



procedure TXQBoxedDateTimeHelper.multiplyComponents(fac: xqfloat);
begin
  if IsNan(fac) then raise EXQEvaluationException.create('FOCA0005', 'Cannot multiply NaN * ' + toXQuery())
  else if IsInfinite(fac) then raise EXQEvaluationException.create('FODT0002', 'Cannot multiply INF * ' + toXQuery());
       setMonths(value, integer(xqround(value.toMonths * fac)), true);
  setDayTime(value, xqround(value.toDayTime * fac));
  truncateRange();
end;

procedure TXQBoxedDateTimeHelper.divideComponents(fac: xqfloat);
begin
  if IsNan(fac) then raise EXQEvaluationException.create('FOCA0005', 'Cannot multiply NaN * ' + toXQuery())
  else if fac = 0 then raise EXQEvaluationException.create('FODT0002', 'Cannot divide duration by zero: ' + toXQuery());
  setMonths(value, integer(xqround(value.toMonths / fac)), true);
  setDayTime(value, xqround(value.toDayTime / fac));
  truncateRange();
end;

procedure TXQBoxedDateTimeHelper.addDuration(const D: TXQValueDateTimeData);
var temp: TXQValueDateTimeData;
begin
  if typeAnnotationType.isDuration then begin
    setMonths(value, value.toMonths + D.toMonths, true);
    setDayTime(value, value.toDayTime + d.toDayTime);
  end else begin
    addDurationDToDateS(value, D, temp);
    value := temp;
  end;
  truncateRange;
end;

procedure TXQBoxedDateTimeHelper.subtractDuration(D: TXQValueDateTimeData);
var temp: TXQValueDateTimeData;
  i: Integer;
begin
  if typeAnnotationType.isDuration then begin
    setMonths(value, value.toMonths - D.toMonths, true);
    setDayTime(value, value.toDayTime - d.toDayTime);
  end else begin
    for i := 1 to 7 do D.values[i] := -D.values[i];
    addDurationDToDateS(value, D, temp);
    value := temp;
  end;
  truncateRange;
end;

procedure TXQBoxedDateTimeHelper.truncateRangeH();
begin
  truncateRange();
end;

end.

