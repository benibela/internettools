program tests;

{$mode objfpc}{$H+}

uses
  //bbheaptrc,
  //heaptrc,
  {$ifdef unix}cwstring,{$endif}
  Classes, bbutils,
  xpath2_tests, extendedhtmlparser_tests, bbutils_tests,  sysutils, xquery1_tests,

  simplehtmltreeparser, xquery, internetaccess_tests, xpath3_tests, xquery3_tests, bigdecimal_tests, parsertests, simpleinternet_tests,
commontestutils, extendedhtmlparser;

var
  start: TDateTime;
  testerrors: boolean = false;
  commonEncodings: array [0..3] of TSystemCodePage = (CP_UTF8, CP_LATIN1, CP_ASCII, CP_WINDOWS1252);
  e: TSystemCodePage;

begin
  testerrors := true;
  if testerrors then begin
    try raise EXQEvaluationException.create('pxp:INTERNAL', 'These tests test several error conditions. These exceptions should be disabled in the debugger.'); except on EXQEvaluationException do ; end;
    try raise EXQParsingException.create('pxp:INTERNAL', 'These tests test several error conditions. These exceptions should be disabled in the debugger.'); except on EXQParsingException do ; end;
    //try raise EHTMLParseException.create('These tests test several error conditions. These exceptions should be disabled in the debugger.'); except on EHTMLParseException do ; end;
    try raise EHTMLParseMatchingException.create('These tests test several error conditions. These exceptions should be disabled in the debugger.', nil); except on EHTMLParseMatchingException do ; end;
    //try raise ETreeParseException.create('These tests test several error conditions. These exceptions should be disabled in the debugger.'); except on ETreeParseException do ; end;
  end;

  start := now;
  //bigdecimal_tests.unittests;
  bbutils.registerFallbackUnicodeConversion; //that seems to be more stable across platforms. I think I saw some systems where unicode 10ffff was converted to latin1 '?' and others where it was '??'
  bbutils_tests.unitTests;

  internetaccess_tests.unittests;
  for e in commonEncodings do begin
    DefaultSystemCodePage := e;
    parsertests.unittests(testerrors);
  end;

  xpath2_tests.unittests(testerrors);
  xquery1_tests.unittests(testerrors);
  xpath3_tests.unittests(testerrors, false); //}
  xquery3_tests.unittests(testerrors);
  extendedhtmlparser_tests.unitTests(testerrors);
  simpleinternet_tests.unittests();
  writeln('OK  (time: ', (now-start)*24*60*60*1000:5:5,')');
  writeln(globalTestCount, ' tests successful');
end.

