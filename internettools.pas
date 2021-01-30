{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit internettools;

{$warn 5023 off : no warning about unused units}
interface

uses
  bbutils, extendedhtmlparser, htmlInformation, simplehtmlparser, simplehtmltreeparser, simplexmlparser, simplexmltreeparserfpdom, xquery, 
  xquery.internals.collations, xquery.internals.common, xquery.internals.floathelpers, xquery.internals.lclexcerpt, 
  xquery.internals.protectionbreakers, xquery.internals.rng, xquery.namespaces, xquery__functions, xquery__parse, xquery__regex, 
  xquery__serialization, xquery_json, xquery_module_binary, xquery_module_file, xquery_module_math, xquery_module_uca_icu, internetaccess, 
  mockinternetaccess, multipagetemplate, simpleinternet, synapseinternetaccess, w32internetaccess, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('internettools', @Register);
end.
