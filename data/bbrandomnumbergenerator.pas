{
Copyright (C) 2021 - 2022  Benito van der Zander (BeniBela)
                           benito@benibela.de
                           www.benibela.de

This file is distributed under under the same license as Lazarus and the LCL itself:

This file is distributed under the Library GNU General Public License
with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

}
(***
Random number generators.

Use TRandomNumberGenerator, which is TGenericRandomNumberGenerator specialized with Txoshiro256ss.

*)
unit bbrandomnumbergenerator;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$RangeChecks off}
{$OverflowChecks off}

interface

uses
  SysUtils;

type
  //**Random number generator based on xoshiro** by Sebastiano Vigna
  Txoshiro256ss = record
    s: array[0..3] of QWord;
    //**Initializes the RNG with a seed
    procedure randomize(seed: QWord);
    //**Returns a random QWord
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
    //**Initializes the RNG
    procedure randomize;
    //**Initializes the RNG with a seed
    procedure randomize(seed: QWord);
    //**Returns a random QWord
    function nextQWord: QWord; inline;
    //**Returns a random double between 0 and 1
    function nextDouble: Double;
    //**Returns a random number in [0, l[
    function next(const l: longint): longint;
    //**Returns a random number in [0, l[
    function next(const l: qword): qword;
    //**Returns a random number in [0, l[
    function next(const l: int64): int64;
    //**Orders an array randomly
    procedure shuffle(var a: array of integer);
    //**Orders an array randomly
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

