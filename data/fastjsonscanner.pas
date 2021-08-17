{
    This file was part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org
                  2021 by Benito van der Zander

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$ModeSwitch advancedrecords}
{ $INLINE ON}

unit fastjsonscanner;

interface

uses SysUtils, Classes, bbutils;

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'string exceeds end of line %d';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joIgnoreTrailingComma,joIgnoreDuplicates,joBOMCheck);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type
  TAppendEscapeFunction = procedure (var sb: TStrBuilder; p: pchar; l: integer) of object;
  TJSONEscapeCharacters = (jecEscapeNothing, jecEscapeForXML10, jecEscapeForXML11, jecEscapeAll);
  TJSONScanner = record
  private
    FSource: RawByteString;
    FSourceEnd: PAnsiChar;
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenLength: Integer;
    FCurTokenStart: PAnsiChar;
    FTokenStr:  PAnsiChar;
    FLineStart: PAnsiChar; //for column reporting
    FOptions : TJSONOptions;
    function GetCurColumn: Integer; inline;
    function GetCurLine: string;
    procedure MarkLineStart;
  //protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string;  Const Args: array of const);overload;
//    function DoFetchToken: TJSONToken; inline;
  public
    procedure init(Source: TStream; AOptions: TJSONOptions);
    procedure init(const aSource: RawByteString; AOptions: TJSONOptions);
    procedure done;

    function FetchToken: TJSONToken;

    class function decodeJSONString(strStart: pchar; strLength: SizeInt; escapeCharacters: TJSONEscapeCharacters; escapeFunction: TAppendEscapeFunction): string; static;

    property CurLine: string read GetCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenStart: PAnsiChar read FCurTokenStart;
    property CurTokenLength: Integer read FCurTokenLength;

    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation

procedure TJSONScanner.init(Source: TStream; AOptions: TJSONOptions);

  procedure SkipStreamBOM;
  Var
    OldPos : integer;
    Header : array[0..3] of byte;
  begin
    OldPos := Source.Position;
    FillChar(Header{%H-}, SizeOf(Header), 0);
    if Source.Read(Header, 3) = 3 then
      if (Header[0]=$EF) and (Header[1]=$BB) and (Header[2]=$BF) then
        exit;
    Source.Position := OldPos;
  end;


Var
  S : RawByteString;

begin
  if (joBOMCheck in aOptions) then
    SkipStreamBom;
  S:='';
  SetLength(S,Source.Size-Source.Position);
  if Length(S)>0 then
    Source.ReadBuffer(S[1],Length(S));
  init(S,AOptions)
end;


procedure TJSONScanner.init(const aSource: RawByteString; AOptions: TJSONOptions);
begin
  FSource:=aSource;
  FTokenStr:=PAnsiChar(FSource);
  FLineStart := FTokenStr;
  FSourceEnd:=FTokenStr + length(FSource);
  if FTokenStr<>Nil then
    FCurRow:=1;
  FOptions:=AOptions;
end;

procedure TJSONScanner.done;
begin

end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := FTokenStr - FLineStart;
end;


procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string; const Args: array of const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.FetchToken: TJSONToken;

(*
  procedure dumpcurrent;

  begin
  Writeln('Start of line : ',FCurLine);
  Writeln('Cur pos : ',FCurPos);
  Writeln('Start of token : ',FTokenstr);
  Writeln('End of line : ',FTokenstr);
  end;
*)

var
  it : TJSONToken;
  C: char;

begin
  if (FTokenStr = nil) or (FTokenStr>=FSourceEnd) then
    begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
    end;
  case FTokenStr^ of
    #0:
      begin
        if joStrict in Options then Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        Result := tkWhitespace;
      end;
    #9, ' ', #10, #13:
      begin
      Result := tkWhitespace;
      FCurTokenStart := FTokenStr;
      while true do begin
        case FTokenStr^ of
          #9, ' ':;
          #10: MarkLineStart;
          #13: begin
            if FTokenStr[1] = #10 then inc(FTokenStr);
            MarkLineStart;
          end;
          else break;
        end;
        inc(FTokenStr);
      end;
      FCurTokenLength := FTokenStr - FCurTokenStart;
      end;
    '"','''':
      begin
        C:=FTokenStr^;
        If (C='''') and (joStrict in Options) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        Inc(FTokenStr);
        FCurTokenStart := FTokenStr;
        while true do
          case FTokenStr^ of
            '\': begin
              Inc(FTokenStr);
              // Read escaped token
              Case FTokenStr^ of
                '"','''','t','b','n','r','f','\','/': inc(FTokenStr); //single letter escapes
                'u' : begin
                      inc(FTokenStr);
                      if (not (FTokenStr[0] in ['0'..'9','A'..'F','a'..'f'])) or
                         (not (FTokenStr[1] in ['0'..'9','A'..'F','a'..'f'])) or
                         (not (FTokenStr[2] in ['0'..'9','A'..'F','a'..'f'])) or
                         (not (FTokenStr[3] in ['0'..'9','A'..'F','a'..'f']))
                      then
                         Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                      inc(FTokenStr, 4);
                      end;
                #0  : Error(SErrOpenString,[FCurRow]);
              else
                Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
              end;
            end;
            '''', '"': if FTokenStr^ = c then break
                       else inc(FTokenStr);
            #0..#$19: if FTokenStr^ = #0 then Error(SErrOpenString,[FCurRow])
                      else if joStrict in Options then Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]])
                      else begin
                        if FTokenStr^ in [#13, #10] then begin
                          if (FTokenStr^ = #13) and (FTokenStr[1] = #10) then inc(FTokenStr);
                          MarkLineStart;
                        end;
                        inc(FTokenStr);
                      end
            else inc(FTokenStr);
          end;
        FCurTokenLength := FTokenStr - FCurTokenStart;
        inc(FTokenStr);
        Result := tkString;
      end;
    ',':
      begin
        Inc(FTokenStr);
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        FCurTokenStart := FTokenStr;
        if FTokenStr^ = '-' then inc(FTokenStr);
        case FTokenStr^ of
          '1'..'9': Inc(FTokenStr);
          '0': begin
            Inc(FTokenStr);
            if (joStrict in Options) and (FTokenStr^ in ['0'..'9']) then
              Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          end;
          '.': if joStrict in Options then
                 Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          else
            Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        end;
        while true do
        begin
          case FTokenStr^ of
            '0'..'9': inc(FTokenStr);
            '.':
              begin
                case FTokenStr[1] of
                  '0'..'9': Inc(FTokenStr, 2);
                  'e', 'E': begin
                    if joStrict in Options then
                      Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                    Inc(FTokenStr);
                  end;
                  else Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                end;
                while FTokenStr^ in ['0'..'9'] do
                  inc(FTokenStr);
                break;
              end;
          else
            break;
          end;
        end;
        if FTokenStr^ in ['e', 'E'] then begin
          Inc(FTokenStr);
          if FTokenStr^ in ['-','+']  then
            Inc(FTokenStr);
          if not (FTokenStr^ in ['0'..'9']) then
            Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          repeat
            Inc(FTokenStr);
          until not (FTokenStr^ in ['0'..'9']);
        end;
        if {(FTokenStr<>FEOL) and }not (FTokenStr^ in [#13,#10,#0,'}',']',',',#9,' ']) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        FCurTokenLength := FTokenStr - FCurTokenStart;
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(FTokenStr);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceClose;
      end;  
    '[':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceClose;
      end;
    'a'..'z','A'..'Z','_':
      begin
        FCurTokenStart := FTokenStr;
        Result:=tkIdentifier;
        case FTokenStr^ of
          't': if (FTokenStr[1] = 'r') and (FTokenStr[2] = 'u') and (FTokenStr[3] = 'e') then
            Result:=tkTrue;
          'f': if (FTokenStr[1] = 'a') and (FTokenStr[2] = 'l') and (FTokenStr[3] = 's') and (FTokenStr[4] = 'e') then
            Result:=tkFalse;
          'n': if (FTokenStr[1] = 'u') and (FTokenStr[2] = 'l') and (FTokenStr[3] = 'l') then
            Result:=tkNull;
        end;
        if result <> tkIdentifier then inc(FTokenStr, length(TokenInfos[result]) - 1);
        repeat
          Inc(FTokenStr);
        until not (FTokenStr^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        FCurTokenLength := FTokenStr - FCurTokenStart;
        if (result = tkIdentifier) or (FCurTokenLength <> length(TokenInfos[result])) then begin
          if (joStrict in Options) then
            Error(SErrInvalidCharacter, [CurRow,CurColumn - FCurTokenLength, FCurTokenStart[0]]);
          for it := tkTrue to tkNull do
            if (Length(TokenInfos[it]) = FCurTokenLength) and (strlicomp(FCurTokenStart, PAnsiChar(TokenInfos[it]), FCurTokenLength) = 0) then
              begin
              Result := it;
              FCurToken := Result;
              exit;
              end;
        end;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
    result := tkEOF;
  end;
  FCurToken := Result{%H-};
end;

class function TJSONScanner.decodeJSONString(strStart: pchar; strLength: SizeInt;
  escapeCharacters: TJSONEscapeCharacters;
  escapeFunction: TAppendEscapeFunction): string;
//  const SUSPICIOUS_CHARS_NORMAL = [#0..#8, #11, #12, #14..#$1F, #$80..#$C1, #$ED, #$EF, #$F4..#$FF];
//  const SUSPICIOUS_CHARS_ESCAPE = [#0..#$1F, '\', #$80..#$C1, #$ED, #$EF, #$F4..#$FF];
  const REPLACEMENT_CHARACTER_CP = $FFFD;
  var sb: TStrBuilder;
    //append 00..1F or \
    procedure appendLowCodePoint(toEscape: char);
    var temp: string[6];
    label NoEscape, EscapeSingleLetter, EscapeU00xx, AppendEscapedTemp;
    begin
      case escapeCharacters of
        jecEscapeNothing: goto NoEscape;
        jecEscapeForXML10:
          case toEscape of
            '\', #$9, #10, #13: goto NoEscape;
            #8, #12: goto EscapeSingleLetter;
            else goto EscapeU00xx;
          end;
        jecEscapeForXML11:
          if toEscape = #0 then goto EscapeU00xx
          else goto NoEscape;
        jecEscapeAll:
          case toEscape of
            '\', #8, #9, #10, #12, #13: goto EscapeSingleLetter;
            else goto EscapeU00xx;
          end;
      end;

      NoEscape:
        sb.append(@toEscape, 1);
        exit;

      EscapeSingleLetter:
        temp := '\x';
        case toEscape of
          '\': temp[2] := '\';
          #8: temp[2] := 'b';
          #9: temp[2] := 't';
          #10: temp[2] := 'n';
          #12: temp[2] := 'f';
          #13: temp[2] := 'r';
        end;
        goto AppendEscapedTemp;

      EscapeU00xx:
        temp := '\u00XX';
        temp[5] := chr(ord(toEscape) shr 4 + ord('0'));
        temp[6] := charEncodeHexDigitUp(ord(toEscape) and $F);

      AppendEscapedTemp:
        if escapeFunction <> nil then escapeFunction(sb, @temp[1], length(temp))
        else if escapeCharacters = jecEscapeAll then sb.append(@temp[1], length(temp))
        else sb.appendCodePoint(REPLACEMENT_CHARACTER_CP);
    end;
    procedure appendInvalidUnicode(buxxxx: pchar);
    begin
      if escapeFunction <> nil then escapeFunction(sb, buxxxx, 6)
      else if escapeCharacters = jecEscapeAll then sb.append(buxxxx, 6)
      else sb.appendCodePoint(REPLACEMENT_CHARACTER_CP);
    end;
    procedure appendSurrogatePair(buxxxxbuxxxx: pchar);
    var
      tempA, tempB, codePoint: UInt32;
    begin
      strHexToUIntTry(buxxxxbuxxxx + 2, buxxxxbuxxxx + 2 + 4, tempA);
      strHexToUIntTry(buxxxxbuxxxx + 6+2, buxxxxbuxxxx + 6+2 + 4, tempB);
      codePoint := ((tempA and $03ff) shl 10) or (ord(tempB) and $03ff);
      sb.appendCodePoint(codePoint + $10000);
    end;
    procedure appendSuspiciousCodepoint(codepoint: Int32; errpchar: pchar);
    var temp: string[6];
      function errstring: pchar;
      begin
        if errpchar <> nil then result := errpchar
        else begin
          temp := '\u' + IntToHex(codepoint, 4);
          result := @temp;
        end;
      end;

    begin
      case codePoint of //see isValidXMLCharacter
        $20..ord('\')-1, ord('\')+1..$7E, $A0..$D7FF, $E000..$FFFD, $10000..$10FFFF: sb.appendCodePoint(codePoint);
        $0..$1F, ord('\'):  appendLowCodePoint(chr(codePoint));
        $7F..$9F: if escapeCharacters = jecEscapeAll then appendInvalidUnicode(errstring)
                  else sb.appendCodePoint(codepoint);
        else appendInvalidUnicode(errstring)
      end;
    end;
    procedure appendUnicodeEscape(buxxxx: pchar);
    var
      codePoint: UInt32;
    begin
      strHexToUIntTry(buxxxx + 2, buxxxx + 2 + 4, codePoint);
      appendSuspiciousCodepoint(codePoint, buxxxx);
    end;

  var
    escapeChar: Char;
    p, pend, sectionstart, unicodeEscape1, unicodeEscape2: PAnsiChar;
    procedure appendSectionAndAdvance(skip: integer);
    begin
      sb.append(sectionstart, p - sectionstart);
      inc(p, skip);
      sectionstart := p;
    end;
  begin
    sb.init(@result, strLength);
    p := strStart;
    sectionstart := p;
    pend := p + strLength;
    while p < pend do begin
      case p^ of
        '\': begin
          escapeChar := (p+1)^;
          case escapeChar of
            '"', '''', '/' : begin
              appendSectionAndAdvance(2);
              sb.append(escapeChar);
              continue;
            end;
            '\': escapeChar := '\';
            'b': escapeChar:=#8;
            't': escapeChar:=#9;
            'n': escapeChar:=#10;
            'f': escapeChar:=#12;
            'r': escapeChar:=#13;
            'u': begin
              unicodeEscape1 := p;
              appendSectionAndAdvance(2);
              if (p[0] in ['D', 'd']) and (p[1] in ['8','9','A','B','a','b']) then begin
                unicodeEscape2 := p + 4;
                //first surrogate
                if ((unicodeEscape2+2+4) > pend)
                   or not (( ((unicodeEscape2)[2] in ['D', 'd']) and ((unicodeEscape2)[3] in ['C'..'F', 'c'..'f']) )) then begin
                  appendInvalidUnicode(unicodeEscape1);
                end else begin
                  appendSurrogatePair(unicodeEscape1);
                  inc(p, 6);
                end;
              end else appendUnicodeEscape(unicodeEscape1);
              inc(p, 4);
              sectionstart := p;
              continue;
            end;
          end;
          if escapeCharacters = jecEscapeAll then begin
           inc(p); //do nothing, section copy will copy the already escaped char
          end else begin
            appendSectionAndAdvance(2);
            appendLowCodePoint(escapeChar);
          end;
        end;
        #0..#$1F: begin
          escapeChar := p^;
          appendSectionAndAdvance(1);
          appendLowCodePoint(escapeChar);
        end;
        {#$80..#$C1,} #$C0, #$C1, #$ED, #$EF, #$F4..#$FF: begin
          sb.append(sectionstart, p - sectionstart);
          appendSuspiciousCodepoint(strDecodeUTF8Character(p, pend), nil);
          sectionstart := p;
        end;
        else inc(p);
      end;
    end;
    appendSectionAndAdvance(0);
    sb.final;
end;

{function TJSONScanner.FetchToken: TJSONToken;

begin
  Result:=DoFetchToken;
end;}

function TJSONScanner.GetCurLine: string;
var eol: pchar;
begin
  Result:='';
  eol := FLineStart;
  while (eol < FSourceEnd) and not (eol^ in [#0,#10,#13]) do inc(eol);
  SetString(result, FLineStart, eol - FLineStart);
end;

procedure TJSONScanner.MarkLineStart;
begin
  inc(fCurRow);
  FLineStart := FTokenStr;
  if FLineStart^ in [#10,#13] then inc(FLineStart);
end;

end.
