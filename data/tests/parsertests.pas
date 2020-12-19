unit parsertests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser,bbutils;

procedure unittests(TestErrors:boolean);

implementation

uses commontestutils;

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

procedure testCSS;
begin
  test( CSSHasHiddenStyle('display: none') );
  test( CSSHasHiddenStyle(';;display  : none') );
  test( not CSSHasHiddenStyle('xdisplay: none'));
  test( not CSSHasHiddenStyle('displayx: none') );
  test( not CSSHasHiddenStyle('display: xnone') );
  test( not CSSHasHiddenStyle('display: nonex') );
  test( CSSHasHiddenStyle('visibility: hidden') );
  test( CSSHasHiddenStyle('a; b; c; display: none') );
  test( not CSSHasHiddenStyle('ddsdas:::Aas; c; display:') );
  test( not CSSHasHiddenStyle(';; c; display') );
  test( CSSHasHiddenStyle('display: none; position: absolute; border: 1px solid rgb(17, 17, 17); background-color: rgb(224, 234, 204); padding: 5px; opacity: 0.85; margin-top: 0.5em; margin-left: -2em;'));
end;


procedure unittests(TestErrors:boolean);
var
  tp: TTreeParser;
  count: Integer;


  procedure t(i, o: RawByteString; ct: string = '');
  var
    tn: TTreeNode;
    parents: TList;
    oxml: rawbyteString;
  begin
    if strActualEncoding(DefaultSystemCodePage) <> CP_UTF8 then
      o := strConvertFromUtf8(o, CP_ACP);
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
    oxml := tp.getLastTree.outerXML();
    //todo: it is parsed as CP_ACP, but outerXML always returns CP_UTF8 ??
    //writeln(StringCodePage(o));
    //writeln(StringCodePage(oxml));
    //SetCodePage(oxml, CP_ACP, false);
    if oxml <> o then
      raise Exception.Create('Parsing result invalid: '+oxml+ ' != '+o + LineEnding + 'DefaultEncoding: ' + IntToStr(DefaultSystemCodePage));
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



  t('<html><head><meta charset="utf-8"/></head>'#$C3#$BC'</html>', '<html><head><meta charset="utf-8"/></head>'#$C3#$BC'</html>'); //utf-8 -> utf-8
  t('<html><head><meta charset="ISO-8859-1"/></head>'#$FC'</html>', '<html><head><meta charset="ISO-8859-1"/></head>'#$C3#$BC'</html>');
  t('<html><head><meta charset="UTF-8"/></head>'#$FC'</html>', '<html><head><meta charset="UTF-8"/></head>'#$FC'</html>'); //is this correct? #$FC is invalid utf-8
  t('<html><head><meta charset="ISO-8859-1"/></head>'#$C3#$BC'</html>', '<html><head><meta charset="ISO-8859-1"/></head>'#$C3#$83#$C2#$BC'</html>');

  t('<html><head><meta charset="ISO-8859-1"/></head>'#$FC'</html>', '<html><head><meta charset="ISO-8859-1"/></head>'#$FC'</html>', 'text/html; charset=utf-8'); //invalid utf-8 again
  t('<html><head><meta charset="ISO-8859-1"/></head>'#$C3#$BC'</html>', '<html><head><meta charset="ISO-8859-1"/></head>'#$C3#$BC'</html>', 'text/html; charset=utf-8');
  t('<html><head><meta charset="utf-8"/></head>'#$C3#$BC'</html>', '<html><head><meta charset="utf-8"/></head>'#$C3#$83#$C2#$BC'</html>', 'text/html; charset=latin1');
  t('<html><head><meta charset="UTF-8"/></head>'#$FC'</html>', '<html><head><meta charset="UTF-8"/></head>'#$C3#$BC'</html>', 'text/html; charset=latin1');

  if (DefaultSystemCodePage <> CP_ASCII) {$ifndef windows}or (true){$endif} then begin
    t('<'#0'h'#0't'#0'm'#0'l'#0'>'#0#$FC#$0'<'#0'/'#0'h'#0't'#0'm'#0'l'#0'>'#0, '<html>'#$C3#$BC'</html>',  'text/html; charset=utf-16le');
    t(#0'<'#0'h'#0't'#0'm'#0'l'#0'>'#0#$FC#$0'<'#0'/'#0'h'#0't'#0'm'#0'l'#0'>', '<html>'#$C3#$BC'</html>',  'text/html; charset=utf-16be');
  end;

  tp.repairMissingStartTags := true;
  tp.repairMissingEndTags := true;
  t('<html><body></body></html>aaa', '<html><head/><body>aaa</body></html>');
  t('<html><body></body>u</html> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '<html><head/><body>u aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</body></html>');
  t('<html><body></body>  </html> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '<html><head/><body>   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</body></html>');
  if DefaultSystemCodePage <> CP_WINDOWS1252 then
    t('<html><a href="&{x}&#x85;"/>', '<html><head/><body><a href="&amp;{x}'#$e2#$80#$a6'"/></body></html>');

  t('<table>', '<html><head/><body><table/></body></html>');
  t('<table><tr><td>1</td></tr></table>', '<html><head/><body><table><tbody><tr><td>1</td></tr></tbody></table></body></html>');
  t('<table><tr><td>1</td><td>1b</td></tr><tr><td>2</td><td>2b</td></tr></table>', '<html><head/><body><table><tbody><tr><td>1</td><td>1b</td></tr><tr><td>2</td><td>2b</td></tr></tbody></table></body></html>');

  tp.parsingModel := pmUnstrictXML;
  tp.repairMissingStartTags := false;
  tp.repairMissingEndTags := false;
  //valid xml
  t('<?xml version="1.0"?><!DOCTYPE xyz><a/>', '<a/>');
  t('<?xml version="1.0"?><!DOCTYPE xyz SYSTEM "foo"><a/>', '<a/>');
  t('<?xml version="1.0"?><!DOCTYPE abc [<?abc >>>?>]><a/>', '<a/>');
  t('<?xml version="1.0"?><!DOCTYPE abc PUBLIC "y" "t" [<?abc >>>?><!---->]><a/>', '<a/>');

  //invalid to repair
  t('<!doctype xml [<!ENTITY abc>]><a>', '<a/>');
  t('<!doctype xml [<!ENTITY>><a>', '<a/>');
  t('<!doctype xml [<!ENTITY foo bar><!NOTATION ass PUBLIC "->>>--">]><a>', '<a/>');
  if (DefaultSystemCodePage <> CP_LATIN1) and (DefaultSystemCodePage <> CP_WINDOWS1252) and (DefaultSystemCodePage <> CP_ASCII) then
    t('<xml><a href="&{x}&#x85;"/>', '<xml><a href="&amp;{x}&#x85;"/></xml>');


  tp.parsingModel := pmStrict;
  t('<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;23&apos;&amp;amp;=y">93.7</ex:b>', '<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;23&apos;&amp;amp;=y">93.7</ex:b>');
  t('<ex:b xmlns:ex="http://www.example.com/ns?p=&apos;&#x20;&apos;&#x20;&#x20;&#x20;&#x20;&#x20;&#x20;">93.7</ex:b>', '<ex:b xmlns:ex="http://www.example.com/ns?p=&apos; &apos;">93.7</ex:b>');
  t('<r> <?foobar?> </r>', '<r> <?foobar?> </r>');
  t('<r> <?php echo "<span class=''cap''>".$row[''ID''].". "; ?> </r>', '<r> <?php echo "<span class=''cap''>".$row[''ID''].". "; ?> </r>');

  finally
      tp.free;
  end;

  testCSS;

  writeln('parser tested, cp: ',DefaultSystemCodePage);

end;


end.
