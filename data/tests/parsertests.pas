unit parsertests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser;

procedure unittests(TestErrors:boolean);

implementation

procedure unittests(TestErrors:boolean);
var
  tp: TTreeParser;
  count: Integer;


  procedure t(const i, o: string; ct: string = '');
  begin
    count += 1;
    tp.parseTree(i, '', ct);
    if tp.getLastTree.outerXML() <> o then
      raise Exception.Create('Parsing result invalid: '+tp.getLastTree.outerXML()+ ' != '+o);
  end;

begin
    //most parser tests are in xpath2_tests
  count := 0;

  tp := TTreeParser.Create;
  try
  tp.readComments:=true;
  tp.readProcessingInstructions:=true;

  tp.parsingModel := pmHTML;
  t('<html></html>', '<html/>');
  t('<html>'#$C3#$BC'</html>', '<html>'#$C3#$BC'</html>'); //utf-8 -> utf-8
  t('<html>'#$FC'x</html>', '<html>'#$C3#$BC'x</html>');     //latin1 -> utf-8
  t('<html>'#$FC#$C3#$BC'y</html>', '<html>'#$C3#$BC#$C3#$83#$C2#$BC'y</html>'); //murks -> utf-8

  t('<html>'#$C3#$BC'</html>', '<html>'#$C3#$BC'</html>', 'text/html; charset=utf-8');
  t('<html>'#$FC'x</html>', '<html>'#$C3#$BC'x</html>', 'text/html; charset=latin1');
  t('<html>'#$C3#$BC'</html>', '<html>'#$C3#$83#$C2#$BC'</html>',  'text/html; charset=latin1');

  t('<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"/></head>'#$C3#$BC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"/></head>'#$C3#$BC'</html>'); //utf-8 -> utf-8
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$FC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$C3#$BC'</html>');
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=UTF-8"/></head>'#$FC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=UTF-8"/></head>'#$FC'</html>'); //is this correct? #$FC is invalid utf-8
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$C3#$BC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$C3#$83#$C2#$BC'</html>');


  t('<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$FC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$FC'</html>', 'text/html; charset=utf-8'); //invalid utf-8 again
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$C3#$BC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"/></head>'#$C3#$BC'</html>', 'text/html; charset=utf-8');
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"/></head>'#$C3#$BC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"/></head>'#$C3#$83#$C2#$BC'</html>', 'text/html; charset=latin1');
  t('<html><head><meta http-equiv="content-type" content="text/html; charset=UTF-8"/></head>'#$FC'</html>', '<html><head><meta http-equiv="content-type" content="text/html; charset=UTF-8"/></head>'#$C3#$BC'</html>', 'text/html; charset=latin1');


  t('<'#0'h'#0't'#0'm'#0'l'#0'>'#0#$FC#$0'<'#0'/'#0'h'#0't'#0'm'#0'l'#0'>'#0, '<html>'#$C3#$BC'</html>',  'text/html; charset=utf-16le');
  t(#0'<'#0'h'#0't'#0'm'#0'l'#0'>'#0#$FC#$0'<'#0'/'#0'h'#0't'#0'm'#0'l'#0'>', '<html>'#$C3#$BC'</html>',  'text/html; charset=utf-16be');

  finally
      tp.free;
  end;


  writeln('parser tested');

end;

end.
