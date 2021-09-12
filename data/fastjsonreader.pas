unit fastjsonreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastjsonscanner;

type TJSONParsingPhase = ( jppArrayExpectValue, jppArrayExpectComma,
                          jppObjectExpectKey, jppObjectExpectComma, jppObjectExpectValue,
                          jppRoot);
  TJSONParserOption = (jpoAllowMultipleTopLevelItems, jpoLiberal, jpoAllowTrailingComma, jpoEscapeCharacters, jpoJSONiq);
  //**Parsing options
  //**@value jpoAllowMultipleTopLevelItems allow @code([], [], {}) as input
  //**@value jpoLiberal             does not set joStrict for fpc's json scanner
  //**@value jpoAllowTrailingComma  sets joIgnoreTrailingComma for fpc's json scanner
  //**@value jpoEscapeCharacters    escapes characters for XPath/XQuery 3.1, e.g. return strings with \n or \uxxxx
  //**@value jpoJSONiq              change from XPath/XQuery 3.1 parsing mode to JSONiq:
  //**       @table(
  //**       @rowHead(@cell(input)   @cell(XPath/XQuery 3.1)    @cell(  JSONiq))
  //**       @row(@cell(number)      @cell(double  )            @cell(  int, decimal, double))
  //**       @row(@cell(null)        @cell(empty sequence, () ) @cell(  null          ))
  //**       @row(@cell(invalid)     @cell(err:FOJS0001)        @cell(  jerr:JNDY0021))
  //**       )
  TJSONParserOptions = set of TJSONParserOption;
type TJsonReader = class
protected
  parsingPhase: TJSONParsingPhase;
  scanner: TJSONScanner;
  escapeCharactersMode: TJSONEscapeCharacters;
  appendEscapeFunction: TAppendEscapeFunction;
  jsoniqMode: boolean;
  function decodeStringToken: string;
public
  options: TJSONParserOptions;
protected
  procedure readArray; virtual; abstract;
  procedure readObject; virtual; abstract;
  procedure readArrayEnd(var newParsingPhase: TJSONParsingPhase); virtual; abstract;
  procedure readObjectEnd(var newParsingPhase: TJSONParsingPhase); virtual; abstract;
  procedure readKey; virtual; abstract;
  procedure readString; virtual; abstract;
  procedure readNumber; virtual; abstract;
  procedure readNumberInf; virtual;
  procedure readNumberNaN; virtual;
  procedure readFalse; virtual; abstract;
  procedure readTrue; virtual; abstract;
  procedure readNull; virtual; abstract;
  procedure raiseError(message: string); virtual;
  procedure raiseError; virtual;

  procedure initJSONScanner(const data: string); virtual;
  procedure beginParsing; virtual;
  procedure endParsing; virtual;

public
  procedure parse(const s: string);
end;

implementation

uses bbutils;

function TJsonReader.decodeStringToken: string;
begin
  result := TJSONScanner.decodeJSONString(scanner.CurTokenStart, scanner.CurTokenLength, escapeCharactersMode, appendEscapeFunction);
end;

procedure TJsonReader.readNumberInf;
begin
  //ignore
end;

procedure TJsonReader.readNumberNaN;
begin
  //ignore
end;

procedure TJsonReader.raiseError(message: string);
begin
  if message = '' then message := 'Failed to parse JSON';
  raise EScannerError.Create(message+' at ' + scanner.CurTokenErrorMessage);
end;

procedure TJsonReader.raiseError;
begin
  raiseError('');
end;

procedure TJsonReader.initJSONScanner(const data: string);
var scannerOptions: TJSONOptions;
begin
  jsoniqMode := jpoJSONiq in options;
  parsingPhase := jppRoot;
  scannerOptions := [joUTF8];
  if not (jpoLiberal in options) then include(scannerOptions, joStrict);
  if jpoAllowTrailingComma in options then include(scannerOptions, joIgnoreTrailingComma);
  scanner := default(TJSONScanner);
  scanner.init(data, scannerOptions);
end;

procedure TJsonReader.beginParsing;
begin
  //ignore event
end;

procedure TJsonReader.endParsing;
begin
  //ignore event
end;

procedure TJsonReader.parse(const s: string);
  procedure readKeyInternal;
  begin
    parsingPhase := jppObjectExpectValue;
    readKey;
    if parsingPhase = jppObjectExpectValue then
      if scanner.FetchTokenNoWhitespace <> tkColon then
        raiseError('Expected : between property name and value');
  end;

begin
  initJSONScanner(s);
  beginParsing;
  try
    if scanner.FetchTokenNoWhitespace = tkEOF then
      if not (jpoLiberal in options) then
        raiseError('No data');
    while true do begin
      case scanner.CurToken of
        tkEOF: break;
        tkString:
          if parsingPhase = jppObjectExpectKey then readKeyInternal
          else readString;
        tkNumber: readNumber;
        tkFalse: readFalse;
        tkTrue: readTrue;
        tkNull: readNull;

        tkComma: case parsingPhase of
          jppObjectExpectComma: parsingPhase := jppObjectExpectKey;
          jppArrayExpectComma: parsingPhase := jppArrayExpectValue;
          else raiseError();
        end;
        //tkColon,                 // ':'
        tkCurlyBraceOpen: begin
          readObject;
          case scanner.FetchTokenNoWhitespace of
            tkCurlyBraceClose: readObjectEnd(parsingPhase);
            else begin
              parsingPhase := jppObjectExpectKey;
              continue;
            end;
          end;
        end;
        tkCurlyBraceClose:
          if (parsingPhase = jppObjectExpectComma) or ((parsingPhase = jppObjectExpectKey) and (jpoAllowTrailingComma in options)) then readObjectEnd(parsingPhase)
          else raiseError();
        tkSquaredBraceOpen: begin
          readArray;
          case scanner.FetchTokenNoWhitespace of
            tkSquaredBraceClose: readArrayEnd(parsingPhase);
            else begin
              parsingPhase := jppArrayExpectValue;
              continue;
            end;
          end;
        end;
        tkSquaredBraceClose:
          if (parsingPhase = jppArrayExpectComma) or ((parsingPhase = jppArrayExpectValue) and (jpoAllowTrailingComma in options)) then readArrayEnd(parsingPhase)
          else raiseError();
        tkIdentifier:
          if jpoLiberal in options then begin
            if parsingPhase = jppObjectExpectKey then readKeyInternal
            else if strliEqual(scanner.CurTokenStart,  'INF', scanner.CurTokenLength)
                    or strliEqual(scanner.CurTokenStart, 'INFinity', scanner.CurTokenLength) then readNumberInf
            else if strliEqual(scanner.CurTokenStart, 'NAN', scanner.CurTokenLength) then readNumberNan
            else raiseError();
          end else raiseError();
        //tkComment:
        //tkUnknown
        else raiseError();
      end;
      scanner.FetchTokenNoWhitespace;
    end;

  finally
    endParsing;
    scanner.done
  end;
end;

end.

