{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools;

interface

uses
  bbutils, extendedhtmlparser, dregexpr, simpleinternet, internetaccess, simplehtmlparser, simplehtmltreeparser, simplexmlparser, 
  int65math, xquery, synapseinternetaccess, w32internetaccess, simplexmltreeparserfpdom, xquery_json, mockinternetaccess, xquery__regex, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools', @Register);
end.
