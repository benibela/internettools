unit xquery.internals.rng;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$RangeChecks off}
{$OverflowChecks off}

interface

uses
  Classes, SysUtils;

type
  //**Random number generator based on xoshiro** by Sebastiano Vigna
  Txoshiro256ss = record
    s: array[0..3] of QWord;
    procedure randomize(seed: QWord);
    function nextQWord: QWord;
  end;

  //**A very bad random number generator, only used internally to improve the seed for xoshiro
  TSplitMix64 = record
    state: QWord;
    procedure randomize(seed: QWord);
    function nextQword: QWord;
  end;

  //**Generic random number generator. Turns any qword generating RNG into a RNG for other types
  generic TGenericRandomNumberGenerator<TQWordGenerator> = record
  private
    qwordGenerator: TQWordGenerator;
  public
    procedure randomize;
    procedure randomize(seed: QWord);
    function nextQWord: QWord; inline;
    function nextDouble: Double;
    function next(const l: longint): longint;
    function next(const l: qword): qword;
    function next(const l: int64): int64;
    procedure shuffle(var a: array of integer);
    procedure shuffle(var a: array of int64);
  end;

  //**Default random number generator type.
  TRandomNumberGenerator = specialize TGenericRandomNumberGenerator<Txoshiro256ss>;

implementation

procedure TSplitMix64.randomize(seed: QWord);
begin
  state := seed;
end;

function TSplitMix64.nextQword: QWord;
begin
  result := state;

  state := result + $9E3779B97f4A7C15;
  result := (result xor (result shr 30)) * $BF58476D1CE4E5B9;
  result := (result xor (result shr 27)) * $94D049BB133111EB;
  result := result xor (result shr 31);

end;

procedure Txoshiro256ss.randomize(seed: QWord);
var mix: TSplitMix64;
begin
  mix.state:=seed;
  s[0] := mix.nextQword;
  s[1] := mix.nextQword;
  s[2] := mix.nextQword;
  s[3] := mix.nextQword;
end;

function Txoshiro256ss.nextQword: QWord;
var
  t: QWord;
begin
  result := RolQWord(s[1] * 5, 7) * 9;
  t := s[1] shl 17;

  s[2] := s[2] xor s[0];
  s[3] := s[3] xor s[1];
  s[1] := s[1] xor s[2];
  s[0] := s[0] xor s[3];

  s[2] := s[2] xor t;

  s[3] := RolQWord(s[3], 45);
end;

procedure TGenericRandomNumberGenerator.randomize(seed: QWord);
begin
  qwordGenerator.randomize(seed);
end;


procedure TGenericRandomNumberGenerator.randomize;
var temp: double;
begin
  temp := now;
  qwordGenerator.randomize(PQWord(@temp)^);
end;


function TGenericRandomNumberGenerator.nextQWord: QWord;
begin
  result := qwordGenerator.nextQWord;
end;

function TGenericRandomNumberGenerator.nextDouble: Double;
var
  n: QWord;
begin
  n := nextQWord;
  result := n / double(QWord($FFFFFFFFFFFFFFFF));
end;

function TGenericRandomNumberGenerator.next(const l: longint): longint;
begin
  result := nextQWord mod QWord(l);
end;

function TGenericRandomNumberGenerator.next(const l: qword): qword;
begin
  result := nextQWord mod QWord(l);
end;

function TGenericRandomNumberGenerator.next(const l: int64): int64;
begin
  result := int64(nextQWord mod QWord(l));
end;

procedure TGenericRandomNumberGenerator.shuffle(var a: array of integer);
var i, j: sizeint;
  temp: Integer;
begin
  for i := high(a) downto 1 do begin
    j := next(i);
    temp := a[j];
    a[j] := a[i];
    a[i] := temp;
  end;
end;

procedure TGenericRandomNumberGenerator.shuffle(var a: array of int64);
var i, j: sizeint;
  temp: int64;
begin
  for i := high(a) downto 1 do begin
    j := next(i);
    temp := a[j];
    a[j] := a[i];
    a[i] := temp;
  end;
end;

end.

