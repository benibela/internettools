unit bbnormalizeunicode;

(* Upgraded to utf8proc v1.3.1 and simplified: 2016 Benito van der Zander
*)

(*
 * FreePascal translation of the utf8proc library plus some additions: 2008 Theo Lustenberger
 * Original license of the C version read below.
 * See http://www.flexiguided.de/publications.utf8proc.en.html
 *)

(*
 *  Copyright (c) 2006-2007 Jan Behrens, FlexiGuided GmbH, Berlin
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
 *)

(*
 *  This library contains derived data from a modified version of the
 *  Unicode data files.
 *
 *  The original data files are available at
 *  http://www.unicode.org/Public/UNIDATA/
 *
 *  Please notice the copyright statement in the file "uniinfo_data.inc".
 *)

{$IFDEF fpc}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

const
  UTF8PROC_NULLTERM = 1 shl 0;
  UTF8PROC_STABLE = 1 shl 1;
  UTF8PROC_COMPAT = 1 shl 2;
  UTF8PROC_COMPOSE = 1 shl 3;
  UTF8PROC_DECOMPOSE = 1 shl 4;
  UTF8PROC_NLF2LS = 1 shl 7;
  UTF8PROC_NLF2PS = 1 shl 8;
  UTF8PROC_NLF2LF = UTF8PROC_NLF2LS or UTF8PROC_NLF2PS;
  UTF8PROC_STRIPCC = 1 shl 9;

  UTF8PROC_HANGUL_SBASE = $AC00;
  UTF8PROC_HANGUL_LBASE = $1100;
  UTF8PROC_HANGUL_VBASE = $1161;
  UTF8PROC_HANGUL_TBASE = $11A7;
  UTF8PROC_HANGUL_LCOUNT = 19;
  UTF8PROC_HANGUL_VCOUNT = 21;
  UTF8PROC_HANGUL_TCOUNT = 28;
  UTF8PROC_HANGUL_NCOUNT = 588;
  UTF8PROC_HANGUL_SCOUNT = 11172;
  UTF8PROC_HANGUL_L_START = $1100;
  UTF8PROC_HANGUL_L_END = $115A;
  UTF8PROC_HANGUL_L_FILLER = $115F;
  UTF8PROC_HANGUL_V_START = $1160;
  UTF8PROC_HANGUL_V_END = $11A3;
  UTF8PROC_HANGUL_T_START = $11A8;
  UTF8PROC_HANGUL_T_END = $11FA;
  UTF8PROC_HANGUL_S_START = $AC00;
  UTF8PROC_HANGUL_S_END = $D7A4;


  UTF8PROC_DECOMP_TYPE_FONT = 1;
  UTF8PROC_DECOMP_TYPE_NOBREAK = 2;
  UTF8PROC_DECOMP_TYPE_INITIAL = 3;
  UTF8PROC_DECOMP_TYPE_MEDIAL = 4;
  UTF8PROC_DECOMP_TYPE_FINAL = 5;
  UTF8PROC_DECOMP_TYPE_ISOLATED = 6;
  UTF8PROC_DECOMP_TYPE_CIRCLE = 7;
  UTF8PROC_DECOMP_TYPE_SUPER = 8;
  UTF8PROC_DECOMP_TYPE_SUB = 9;
  UTF8PROC_DECOMP_TYPE_VERTICAL = 10;
  UTF8PROC_DECOMP_TYPE_WIDE = 11;
  UTF8PROC_DECOMP_TYPE_NARROW = 12;
  UTF8PROC_DECOMP_TYPE_SMALL = 13;
  UTF8PROC_DECOMP_TYPE_SQUARE = 14;
  UTF8PROC_DECOMP_TYPE_FRACTION = 15;
  UTF8PROC_DECOMP_TYPE_COMPAT = 16;

  UTF8PROC_ERROR_NOMEM = -(1);
  UTF8PROC_ERROR_OVERFLOW = -(2);
  UTF8PROC_ERROR_INVALIDUTF8 = -(3);
  UTF8PROC_ERROR_NOTASSIGNED = -(4);
  UTF8PROC_ERROR_INVALIDOPTS = -(5);

  SSIZE_MAX = (High(longword) - 1) div 2;

  utf8proc_utf8class: array[0..Pred(256)] of Shortint =
  (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0);


type

  PPByte = ^PByte;

  utf8proc_propval_t = Smallint;
  utf8proc_property_t = packed record
    combining_class: utf8proc_propval_t;
    decomp_type: utf8proc_propval_t;
    decomp_mapping: utf8proc_propval_t;
    comb1st_index: longint;
    comb2nd_index: longint;
    comp_exclusion: boolean;
  end;
  putf8proc_property_t = ^utf8proc_property_t;

function utf8proc_NFD(str: PChar): PChar;
function utf8proc_NFC(str: PChar): PChar;
function utf8proc_NFKD(str: PChar): PChar;
function utf8proc_NFKC(str: PChar): PChar;
function utf8proc_codepoint_valid(uc: longint): boolean;
function utf8proc_iterate(str: PByte; strlen: longint; dst: pLongInt): longint;
function utf8proc_encode_char(uc: longint; dst: PByte): longint;
function utf8proc_get_property(uc: longint): putf8proc_property_t;
function utf8proc_decompose_char(uc: longint; dst: plongint; bufsize: longint; options: integer): longint;
function utf8proc_decomposer(str: PByte; strlen: longint; buffer: plongint; bufsize: longint; options: integer): longint;
function utf8proc_map(str: PByte; strlen: longint; dstptr: PPByte; options: integer): longint;
function utf8proc_reencode(buffer: plongint; length: longint; options: integer): longint;
//function utf8proc_getinfostring(pr: putf8proc_property_t; Chara: Longint = -1): string;

implementation

{$I bbnormuniinfo_data.inc}

function utf8proc_errmsg(errcode: longint): pchar;
begin
  case errcode of
    UTF8PROC_ERROR_NOMEM:
      result := 'Memory for processing UTF-8 data could not be allocated.';
    UTF8PROC_ERROR_OVERFLOW:
      result := 'UTF-8 string is too long to be processed.';
    UTF8PROC_ERROR_INVALIDUTF8:
      result := 'Invalid UTF-8 string';
    UTF8PROC_ERROR_NOTASSIGNED:
      result := 'Unassigned Unicode code point found in UTF-8 string.';
    UTF8PROC_ERROR_INVALIDOPTS:
      result := 'Invalid options for UTF-8 processing chosen.';
  else
    result := 'An unknown error occured while processing UTF-8 data.';
  end;
end;

function utf8proc_codepoint_valid(uc: longint): boolean;
begin
  if ((uc < 0) or (uc >= $110000) or
    (((uc and $FFFF) >= $FFFE)) { or ((uc >= $D800) and (uc < $E000)) or
    ((uc >= $FDD0) and (uc < $FDF0))}) then result := false else result := true;
end;

function utf8proc_iterate(str: PByte; strlen: longint; dst: pLongInt): longint;
var
  length: integer;
  i: integer;
  uc: longint;
begin
  uc := -1;
  dst^ := -1;
  if strlen = 0 then
  begin
    result := 0;
    exit;
  end;
  length := utf8proc_utf8class[(str[0])];
  if length = 0 then
  begin
    result := UTF8PROC_ERROR_INVALIDUTF8;
    exit;
  end;
  if ((strlen >= 0) and (length > strlen)) then
  begin
    result := UTF8PROC_ERROR_INVALIDUTF8;
    exit;
  end;
  for i := 1 to Pred(length) do
  begin
    if ((str[i]) and $C0) <> $80 then
    begin
      result := UTF8PROC_ERROR_INVALIDUTF8;
      exit;
    end;
  end;
  case length of
    1:
      begin
        uc := (str[0]);
      end;
    2:
      begin
        uc := (((str[0]) and $1F) shl 6) + ((str[1]) and $3F);
        if uc < $80 then uc := -1;
      end;
    3:
      begin
        uc := (((str[0]) and $0F) shl 12) + (((str[1]) and $3F) shl 6) + ((str[2]) and $3F);
        if (uc < $800) {or ((uc >= $D800) and (uc < $E000)) or ((uc >= $FDD0) and (uc < $FDF0)) }then uc := -1;
      end;
    4:
      begin
        uc := (((str[0]) and $07) shl 18) + (((str[1]) and $3F) shl 12) + (((str[2]) and $3F) shl 6) + ((str[3]) and $3F);
        if (uc < $10000) or (uc >= $110000) then uc := -1;
      end;
  end;
  if (uc < 0) or ((uc and $FFFF) >= $FFFE) then
  begin
    result := UTF8PROC_ERROR_INVALIDUTF8;
    exit;
  end;
  dst^ := uc;
  result := length;
end;

function utf8proc_encode_char(uc: longint; dst: PByte): longint;
begin
  if uc < $00 then
  begin
    result := 0;
    exit;
  end else
    if uc < $80 then
    begin
      dst[0] := (uc);
      begin
        result := 1;
        exit;
      end;
    end else
      if uc < $800 then
      begin
        dst[0] := ($C0 + (uc shr 6));
        dst[1] := ($80 + (uc and $3F));
        begin
          result := 2;
          exit;
        end;
      end else
        if uc = $FFFF then
        begin
          dst[0] := ($FF);
          begin
            result := 1;
            exit;
          end;
        end else
          if uc = $FFFE then
          begin
            dst[0] := ($FE);
            begin
              result := 1;
              exit;
            end;
          end else
            if uc < $10000 then
            begin
              dst[0] := ($E0 + (uc shr 12));
              dst[1] := ($80 + ((uc shr 6) and $3F));
              dst[2] := ($80 + (uc and $3F));
              begin
                result := 3;
                exit;
              end;
            end else
              if uc < $110000 then
              begin
                dst[0] := ($F0 + (uc shr 18));
                dst[1] := ($80 + ((uc shr 12) and $3F));
                dst[2] := ($80 + ((uc shr 6) and $3F));
                dst[3] := ($80 + (uc and $3F));
                begin
                  result := 4;
                  exit;
                end;
              end else
              begin
                result := 0;
                exit;
              end;
end;

function utf8proc_get_property(uc: longint): putf8proc_property_t;
begin
  Result := @utf8proc_properties[utf8proc_stage2table[utf8proc_stage1table[uc shr 8] + (uc and $FF)]]
end;


function utf8proc_decompose_char(uc: longint; dst: plongint; bufsize: longint; options: integer): longint;
var aproperty: putf8proc_property_t;
  casefold_entry: plongint;
  decomp_entry: plongint;
  hangul_sindex: longint;
  hangul_tindex: longint;
  written: longint;
  temp: longint;
  tbc, lbc: integer;
  boundary: boolean;

begin
  aproperty := utf8proc_get_property(uc);
  hangul_sindex := uc - UTF8PROC_HANGUL_SBASE;

  if (options and (UTF8PROC_COMPOSE or UTF8PROC_DECOMPOSE)) <> 0 then
  begin
    if (hangul_sindex >= 0) and (hangul_sindex < UTF8PROC_HANGUL_SCOUNT) then
    begin
      if bufsize >= 1
        then
      begin
        dst[0] := UTF8PROC_HANGUL_LBASE + hangul_sindex div UTF8PROC_HANGUL_NCOUNT;
        if bufsize >= 2 then
          dst[1] := UTF8PROC_HANGUL_VBASE + (hangul_sindex mod UTF8PROC_HANGUL_NCOUNT) div UTF8PROC_HANGUL_TCOUNT;
      end;
      hangul_tindex := hangul_sindex mod UTF8PROC_HANGUL_TCOUNT;
      if hangul_tindex = 0 then
      begin
        result := 2;
        exit;
      end;
      if bufsize >= 3 then
        dst[2] := UTF8PROC_HANGUL_TBASE + hangul_tindex;
      begin
        result := 3;
        exit;
      end;
    end;
  end;


  if (options and (UTF8PROC_COMPOSE or UTF8PROC_DECOMPOSE)) <> 0 then
  begin
    if (aproperty^.decomp_mapping >= 0) and ((aproperty^.decomp_type = 0) or (options and UTF8PROC_COMPAT <> 0)) then
    begin
      written := 0;
      decomp_entry := @utf8proc_sequences[aproperty^.decomp_mapping];
      while decomp_entry^ >= 0 do
      begin
        if (bufsize > written) then temp := (bufsize - written) else temp := 0;
        written := written + utf8proc_decompose_char(decomp_entry^, dst + written, temp, options);
        inc(decomp_entry);
        if (written < 0) then begin result := UTF8PROC_ERROR_OVERFLOW; exit; end;
      end;
      Result := written;
      exit;
    end;
  end;

  if bufsize >= 1
    then
    dst^ := uc;
  begin
    result := 1;
    exit;
  end;
end;

function utf8proc_decomposer(str: PByte; strlen: longint; buffer: plongint; bufsize: longint; options: integer): longint;
var
  property1: putf8proc_property_t;
  property2: putf8proc_property_t;
  wpos: longint;
  uc: longint;
  rpos: longint;
  decomp_result: longint;
  pos: longint;
  uc1: longint;
  uc2: longint;
  temp: longint;
begin
  wpos := 0;
  if (options and UTF8PROC_COMPOSE <> 0) and (options and UTF8PROC_DECOMPOSE <> 0)
    then
  begin
    result := UTF8PROC_ERROR_INVALIDOPTS;
    exit;
  end;
  if (0 = (options and UTF8PROC_COMPOSE)) and (0 = (options and UTF8PROC_DECOMPOSE))
    then
  begin
    result := UTF8PROC_ERROR_INVALIDOPTS;
    exit;
  end;
  begin

    rpos := 0;

    while true do
    begin
      if (options and UTF8PROC_NULLTERM <> 0) then
      begin
        rpos := rpos + (utf8proc_iterate(str + rpos, -1, @uc));
        if uc < 0 then
        begin
          result := UTF8PROC_ERROR_INVALIDUTF8;
          exit;
        end;
        if rpos < 0 then
        begin
          result := UTF8PROC_ERROR_OVERFLOW;
          exit;
        end;
        if uc = 0 then break;
      end
      else
      begin
        if rpos >= strlen then break;
        rpos := rpos + (utf8proc_iterate(str + rpos, strlen - rpos, @uc));
        if uc < 0 then
        begin
          result := UTF8PROC_ERROR_INVALIDUTF8;
          exit;
        end;
      end;
      if (bufsize > wpos) then temp := bufsize - wpos else temp := 0;
      decomp_result := utf8proc_decompose_char(uc, buffer + wpos, temp, options);
      if decomp_result < 0 then
      begin
        result := decomp_result;
        exit;
      end;
      wpos := wpos + (decomp_result);
      if (wpos < 0) or (wpos > SSIZE_MAX div sizeof(longint) div 2) then
      begin
        result := UTF8PROC_ERROR_OVERFLOW;
        exit;
      end;
    end;
  end;

  if ((options and (UTF8PROC_COMPOSE or UTF8PROC_DECOMPOSE) <> 0) and (bufsize >= wpos)) then
  begin
    pos := 0;
    while pos < wpos - 1 do
    begin

      uc1 := buffer[pos];
      uc2 := buffer[pos + 1];
      property1 := utf8proc_get_property(uc1);
      property2 := utf8proc_get_property(uc2);
      if ((property1^.combining_class > property2^.combining_class) and (property2^.combining_class > 0)) then
      begin
        buffer[pos] := uc2;
        buffer[pos + 1] := uc1;
        if pos > 0 then dec(pos) else inc(pos);
      end
      else
      begin
        inc(pos);
      end;
    end;
  end;
  begin
    result := wpos;
    exit;
  end;
end;

function utf8proc_reencode(buffer: plongint; length: longint; options: integer): longint;
var
  starter_property: putf8proc_property_t;
  current_property: putf8proc_property_t;
  rpos: longint;
  wpos: longint;
  uc: longint;
  starter: plongint;
  current_char: longint;
  max_combining_class: utf8proc_propval_t;
  composition: longint;
  hangul_lindex: longint;
  hangul_sindex: longint;
  hangul_vindex: longint;
  hangul_tindex: longint;
begin
  starter_property := nil;
  if (options and (UTF8PROC_NLF2LS or UTF8PROC_NLF2PS or UTF8PROC_STRIPCC) <> 0) then
  begin
    wpos := 0;
    rpos := 0;
    while rpos < length do
    begin
      uc := buffer[rpos];
      if ((uc = $000D) and (rpos < length - 1) and (buffer[rpos + 1] = $000A)) then inc(rpos);
      if ((uc = $000A) or (uc = $000D) or (uc = $0085) or ((options and UTF8PROC_STRIPCC <> 0) and ((uc = $000B) or (uc = $000C)))) then
      begin
        if (options and UTF8PROC_NLF2LS) <> 0 then
        begin
          if (options and UTF8PROC_NLF2PS) <> 0 then
          begin
            buffer[wpos] := $000A;
            inc(wpos);
          end
          else
          begin
            buffer[wpos] := $2028;
            inc(wpos);
          end;
        end
        else
        begin
          if (options and UTF8PROC_NLF2PS) <> 0 then
          begin
            buffer[wpos] := $2029;
            inc(wpos);
          end
          else
          begin
            buffer[wpos] := $0020;
            inc(wpos);
          end;
        end;
      end
      else
        if ((options and UTF8PROC_STRIPCC <> 0) and ((uc < $0020) or ((uc >= $007F) and (uc < $00A0)))) then
        begin
          if uc = $0009 then
          begin
            buffer[wpos] := $0020;
            inc(wpos);
          end;
        end
        else
        begin
          buffer[wpos] := uc;
          inc(wpos);
        end;
    end;
    inc(rpos);
    length := wpos;
  end;
  if (options and UTF8PROC_COMPOSE) <> 0 then
  begin
    starter := nil;
    starter_property := nil;
    max_combining_class := -1;

    wpos := 0;

    for rpos := 0 to Pred(length) do
    begin
      current_char := buffer[rpos];
      current_property := utf8proc_get_property(current_char);
      if (starter <> nil) and (current_property^.combining_class > max_combining_class) then
      begin
        hangul_lindex := starter^ - UTF8PROC_HANGUL_LBASE;
        if (hangul_lindex >= 0) and (hangul_lindex < UTF8PROC_HANGUL_LCOUNT) then
        begin
          hangul_vindex := current_char - UTF8PROC_HANGUL_VBASE;
          if (hangul_vindex >= 0) and (hangul_vindex < UTF8PROC_HANGUL_VCOUNT) then
          begin
            starter^ := UTF8PROC_HANGUL_SBASE + (hangul_lindex * UTF8PROC_HANGUL_VCOUNT + hangul_vindex) * UTF8PROC_HANGUL_TCOUNT;
            starter_property := nil;
            continue;
          end;
        end;
        hangul_sindex := starter^ - UTF8PROC_HANGUL_SBASE;
        if (hangul_sindex >= 0) and (hangul_sindex < UTF8PROC_HANGUL_SCOUNT) and ((hangul_sindex mod UTF8PROC_HANGUL_TCOUNT) = 0) then
        begin
          hangul_tindex := current_char - UTF8PROC_HANGUL_TBASE;
          if (hangul_tindex >= 0) and (hangul_tindex < UTF8PROC_HANGUL_TCOUNT) then
          begin
            starter^ := starter^ + hangul_tindex;
            starter_property := nil;
            continue;
          end;
        end;
        if starter_property = nil then
        begin
          starter_property := utf8proc_get_property(starter^);
        end;
        if (starter_property^.comb1st_index >= 0) and (current_property^.comb2nd_index >= 0) then
        begin
          composition := utf8proc_combinations[starter_property^.comb1st_index + current_property^.comb2nd_index];
          if ((composition >= 0) and
            ((0 = (options and UTF8PROC_STABLE)) or (not utf8proc_get_property(composition)^.comp_exclusion))) then
          begin
            starter^ := composition;
            starter_property := nil;
            continue;
          end;
        end;
      end;
      buffer[wpos] := current_char;
      if current_property^.combining_class <> 0 then
      begin
        if current_property^.combining_class > max_combining_class then
          max_combining_class := current_property^.combining_class;
      end
      else
      begin
        starter := buffer + wpos;
        starter_property := nil;
        max_combining_class := -1;
      end;
      inc(wpos);
    end;
    length := wpos;
  end;
  begin
    wpos := 0;
    for rpos := 0 to Pred(length) do
    begin
      uc := buffer[rpos];
      wpos := wpos + utf8proc_encode_char(uc, PByte(buffer) + wpos);
    end;
    PByte(buffer)[wpos] := (0);
    result := wpos;
  end;
end;

function utf8proc_map(str: PByte; strlen: longint; dstptr: PPByte; options: integer): longint;

var
  buffer: plongint;
  aresult: longint;
  newptr: plongint;
begin
  dstptr^ := nil;
  aresult := utf8proc_decomposer(str, strlen, nil, 0, options);
  if aresult < 0 then
  begin
    result := aresult;
    exit;
  end;
  buffer := GetMem(aresult * sizeof(longint) + 1);
  if buffer = nil then
  begin
    result := UTF8PROC_ERROR_NOMEM;
    exit;
  end;
  aresult := utf8proc_decomposer(str, strlen, buffer, aresult, options);
  if aresult < 0 then
  begin
    freemem(buffer);
    result := aresult;
    exit;
  end;
  aresult := utf8proc_reencode(buffer, aresult, options);
  if aresult < 0 then
  begin
    freemem(buffer);
    result := aresult;
    exit;
  end;
  begin
    newptr := reallocmem(buffer, aresult + 1);
    if newptr <> nil then buffer := newptr;
  end;
  dstptr^ := PByte(buffer);
  result := aresult;
end;

function utf8proc_NFD(str: PChar): PChar;
var
  retval: PByte;
begin
  utf8proc_map(PByte(str), 0, @retval, UTF8PROC_NULLTERM or UTF8PROC_STABLE or UTF8PROC_DECOMPOSE);
  result := PChar(retval);
end;

function utf8proc_NFC(str: PChar): PChar;
var
  retval: PByte;
begin
  utf8proc_map(PByte(str), 0, @retval, UTF8PROC_NULLTERM or UTF8PROC_STABLE or UTF8PROC_COMPOSE);
  result := PChar(retval);
end;

function utf8proc_NFKD(str: PChar): PChar;
var
  retval: PByte;
begin
  utf8proc_map(PByte(str), 0, @retval, UTF8PROC_NULLTERM or UTF8PROC_STABLE or UTF8PROC_DECOMPOSE or UTF8PROC_COMPAT);
  result := PChar(retval);
end;

function utf8proc_NFKC(str: PChar): PChar;
var
  retval: PByte;
begin
  utf8proc_map(PByte(str), 0, @retval, UTF8PROC_NULLTERM or UTF8PROC_STABLE or UTF8PROC_COMPOSE or UTF8PROC_COMPAT);
  result := PChar(retval);
end;

end.
