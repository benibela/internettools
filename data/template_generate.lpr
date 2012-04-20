program template_generate;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, bbutils, pastemplate
  { you can add units after this };

begin
  if paramstr(1) = '' then exit;
  WriteLn(convertTemplate(strLoadFromFile(paramstr(1)), nil));
end.

