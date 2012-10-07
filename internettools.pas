{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools;

interface

uses
  bbutils, extendedhtmlparser, dregexpr, autoupdate, simpleinternet, internetaccess, simplehtmlparser, simplehtmltreeparser, 
  simplexmlparser, progressdialog, int65math, xquery, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools', @Register);
end.
