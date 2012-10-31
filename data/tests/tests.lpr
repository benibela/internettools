program tests;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, xpath2_tests, extendedhtmlparser_tests, bbutils_tests, int65math, extendedhtmlparser, bbutils, xquery, sysutils, xquery1_tests

  , xquery_utf8

  ;

var
  start: TDateTime;
begin
  start := now;
  bbutils_tests.unitTests;
  xpath2_tests.unittests;
  xquery1_tests.unittests;
  extendedhtmlparser_tests.unitTests();
  writeln('OK  (time: ', (now-start)*24*60*60*1000:5:5,')');
end.

