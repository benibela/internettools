program tests;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  Classes, pseudoxpath_tests, extendedhtmlparser_tests
  { you can add units after this };

begin
  pseudoxpath_tests.unittests;
  extendedhtmlparser_tests.unitTests();
  writeln('OK');
end.

