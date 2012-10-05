program tests;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  Classes, pseudoxpath_tests, extendedhtmlparser_tests, bbutils_tests, int65math, pseudoxpath, extendedhtmlparser, bbutils
  { you can add units after this };

begin
  bbutils_tests.unitTests;
  pseudoxpath_tests.unittests;
  extendedhtmlparser_tests.unitTests();
  writeln('OK');
end.

