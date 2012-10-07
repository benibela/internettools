program tests;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, pseudoxpath_tests, extendedhtmlparser_tests, bbutils_tests, int65math, pseudoxpath, extendedhtmlparser, bbutils, sysutils;

var
  start: TDateTime;
begin
  start := now;
  bbutils_tests.unitTests;
  pseudoxpath_tests.unittests;
  extendedhtmlparser_tests.unitTests();
  writeln('OK  (time: ', (now-start)*24*60*60*1000:5:5,')');
end.

