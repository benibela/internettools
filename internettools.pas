{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools;

{$warn 5023 off : no warning about unused units}
interface

uses
  bbutils, extendedhtmlparser, simpleinternet, internetaccess, 
  simplehtmlparser, simplehtmltreeparser, simplexmlparser, xquery, 
  synapseinternetaccess, w32internetaccess, simplexmltreeparserfpdom, 
  xquery_json, mockinternetaccess, xquery__regex, xquery__parse, 
  xquery_module_math, xquery__functions, multipagetemplate, 
  xquery.internals.common, xquery.namespaces, 
  xquery.internals.protectionbreakers, xquery.internals.lclexcerpt, 
  xquery.internals.rng, htmlInformation, xquery__serialization, 
  xquery.internals.floathelpers, xquery.internals.collations, 
  xquery_module_uca_icu, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools', @Register);
end.
