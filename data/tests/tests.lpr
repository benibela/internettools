program tests;

{$mode objfpc}{$H+}

uses
  //bbheaptrc,
  heaptrc,
  Classes, xpath2_tests, extendedhtmlparser_tests, bbutils_tests, extendedhtmlparser, sysutils, xquery1_tests

  , xquery_utf8, internetaccess_tests, xpath3_tests, xquery3_tests, utf8tools, bigdecimal_tests, parsertests;

var
  start: TDateTime;
  testerrors: boolean = false;
begin

  start := now;
 // bigdecimal_tests.unittests;
  bbutils_tests.unitTests;
  internetaccess_tests.unittests;
  testerrors := true; //better disable since they drive the lazarus debugger mad
  parsertests.unittests(testerrors);
 // xpath2_tests.unittests(testerrors);
  xquery1_tests.unittests(testerrors);
  xpath3_tests.unittests(testerrors);
  xquery3_tests.unittests(testerrors);
  extendedhtmlparser_tests.unitTests(testerrors);
  writeln('OK  (time: ', (now-start)*24*60*60*1000:5:5,')');
end.

