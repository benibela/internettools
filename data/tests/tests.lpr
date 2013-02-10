program tests;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, xpath2_tests, extendedhtmlparser_tests, bbutils_tests, int65math, extendedhtmlparser, bbutils, xquery, sysutils, xquery1_tests

  , xquery_utf8, xquery_json

  ;

var
  start: TDateTime;
  testerrors: boolean;
begin
  start := now;
  bbutils_tests.unitTests;
  testerrors := false; //disabled since they drive the lazarus debugger mad
  xpath2_tests.unittests(testerrors);
  xquery1_tests.unittests(testerrors);
  extendedhtmlparser_tests.unitTests();
  writeln('OK  (time: ', (now-start)*24*60*60*1000:5:5,')');
end.

