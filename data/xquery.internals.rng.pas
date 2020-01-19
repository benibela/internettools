unit xquery.internals.rng;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$RangeChecks off}
{$OverflowChecks off}

interface

uses
  Classes, SysUtils;

type Txoshiro256ss = record
  s: array[0..3] of QWord;
  procedure randomize;
  procedure randomize(seed: QWord);
  function nextQWord: QWord;
  function nextDouble: Double;
  function next(const l: longint): longint;
end;

type TSplitMix64 = record
  state: QWord;
  function nextQword: QWord;
end;

(*


uint64_t rol64(uint64_t x, int k)
{
	return (x << k) | (x >> (64 - k));
}

struct xoshiro256ss_state {
	uint64_t s[4];
};

uint64_t xoshiro256ss(struct xoshiro256ss_state *state)
{
	uint64_t *s = state->s;
	uint64_t const result = rol64(s[1] * 5, 7) * 9;
	uint64_t const t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;
	s[3] = rol64(s[3], 45);

	return result;
}

*)



implementation

function TSplitMix64.nextQword: QWord;
begin
  result := state;

  state := result + $9E3779B97f4A7C15;
  result := (result xor (result shr 30)) * $BF58476D1CE4E5B9;
  result := (result xor (result shr 27)) * $94D049BB133111EB;
  result := result xor (result shr 31);

end;

procedure Txoshiro256ss.randomize;
var temp: double;
begin
  temp := now;
  randomize(PQWord(@temp)^);
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

function Txoshiro256ss.nextDouble: Double;
var
  n: QWord;
begin
  n := nextQWord;
  result := n / double(QWord($FFFFFFFFFFFFFFFF));
end;

function Txoshiro256ss.next(const l: longint): longint;
begin
  result := nextQWord mod QWord(l);
end;

end.

