{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools_utf8;

{$warn 5023 off : no warning about unused units}
interface

uses
  xquery_utf8, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools_utf8', @Register);
end.
