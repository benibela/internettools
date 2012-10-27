{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools_autoupdate;

interface

uses
  autoupdate, progressdialog, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools_autoupdate', @Register);
end.
