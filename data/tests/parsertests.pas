unit parsertests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser,bbutils;

procedure unittests(TestErrors:boolean);

implementation

function debugDump(t: TTreeNode): string;
begin
  result := '';
  while t <> nil do begin
    result += t.toString() + ' @' + inttostr(t.offset);
    if t.next <> nil then result += ' :next: ' + t.next.toString() ;
    if t.previous <> nil then result += ' :prev: ' + t.previous.toString() ;
    if t.reverse <> nil then result += ' :rev: ' + t.reverse.toString() ;
    if t.parent <> nil then result += ' :pa: ' + t.parent.toString() ;
    result += LineEnding;
    t := t.next;
  end;
end;

procedure unittests(TestErrors:boolean);
var
  tp: TTreeParser;
  count: Integer;


  procedure t(const i, o: string; ct: string = '');
  var
    tn: TTreeNode;
    parents: TList;
  begin
    count += 1;
    tp.parseTree(i, '', ct);
    tn := tp.getLastTree;
    parents := tlist.Create;
    while tn <> nil do begin
      if (tn.next <> nil) and (tn.next.previous <> tn) then
        raise Exception.Create('next/previous invalid');
      if (tn.previous <> nil) and (tn.previous.next <> tn) then raise Exception.Create('previous/next invalid');
      if (tn.reverse <> nil) and (tn.reverse.reverse <> tn) then raise Exception.Create('reverse invalid');
      if tn.typ = tetClose then parents.Count := parents.Count - 1;
      if (parents.Count > 1) and (tn.parent <> TTreeNode(parents[parents.Count-1])) then raise Exception.Create('parent invalid: '+tn.toString());
      if tn.typ in [tetOpen, tetDocument] then parents.Add(tn);
      if tn.next <> nil then
        if tn.offset >= tn.next.offset then
          raise Exception.Create('Offset invalid ' + inttostr(tn.offset) + ' >= ' + IntToStr(tn.next.offset));        ;
      tn := tn.next;
    end;
    parents.free;
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

  tp.repairMissingStartTags := true;
  tp.repairMissingEndTags := true;
  t('<html><body></body></html>aaa', '<html><head/><body>aaa</body></html>');
  t('<html><body></body>u</html> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '<html><head/><body>u aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</body></html>');
  t('<html><body></body>  </html> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '<html><head/><body>   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</body></html>');


  t('<table>', '<html><head/><body><table/></body></html>');
  t('<table><tr><td>1</td></tr></table>', '<html><head/><body><table><tbody><tr><td>1</td></tr></tbody></table></body></html>');
  t('<table><tr><td>1</td><td>1b</td></tr><tr><td>2</td><td>2b</td></tr></table>', '<html><head/><body><table><tbody><tr><td>1</td><td>1b</td></tr><tr><td>2</td><td>2b</td></tr></tbody></table></body></html>');

  tp.parsingModel := pmStrict;
  t('<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;23&apos;&amp;amp;=y">93.7</ex:b>', '<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;23&apos;&amp;amp;=y">93.7</ex:b>');
  t('<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;&#x20;&apos;&#x20;&#x20;&#x20;&#x20;&#x20;&#x20;">93.7</ex:b>', '<ex:b xmlns:ex="http://www.example.com/ns?p=&apos; &apos;">93.7</ex:b>');
  t('<r> <?foobar?> </r>', '<r> <?foobar?> </r>');
  t('<r> <?php echo "<span class=''cap''>".$row[''ID''].". "; ?> </r>', '<r> <?php echo "<span class=''cap''>".$row[''ID''].". "; ?> </r>');

  finally
      tp.free;
  end;


  writeln('parser tested');

end;

end.
