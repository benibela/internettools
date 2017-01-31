unit internetaccess_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetaccess, commontestutils;

procedure unitTests();

implementation

procedure testcookies;
var cm: TCookieManager;
  sl: THTTPHeaderList;
  example, examplesub1, examplesub2, other: TDecodedUrl;

  procedure parseheaders(const url: TDecodedUrl; const t: string);
  begin
    sl.text := t; cm.parseHeadersForCookies(url, sl);
  end;
  procedure testcookies(const url: TDecodedUrl; const c: string);
  begin
    test(cm.makeCookieHeader(url), 'Cookie: ' + c);
  end;
  procedure testcookiedump(const c: string);
  begin
    test(cm.serializeCookies, c);
  end;

begin
  example := decodeURL('http://example.org/abvc');
  examplesub1 := decodeURL('http://sub1.example.org/def');
  examplesub2 := decodeURL('http://sub2.example.org/ghi');
  other := decodeURL('http://other.com');
  sl := THTTPHeaderList.Create;

  parseheaders(example, 'Set-Cookie: foo=bar');
  parseheaders(examplesub1, 'Set-Cookie: foo=t1');
  parseheaders(examplesub2, 'Set-Cookie: foo=t2');
  testcookies(example, 'foo=bar');
  testcookies(examplesub1, 'foo=t1');
  testcookies(examplesub2, 'foo=t2');
  testcookies(other, '');

  parseheaders(example ,'Set-Cookie: FOO=Xyz; '+LineEnding+'Set-Cookie: whitespace="a b%c";');
  parseheaders(examplesub1 ,'Set-Cookie: FOO===;;;');
  testcookies(example, 'foo=bar; FOO=Xyz; whitespace="a b%c"');
  testcookies(examplesub1, 'foo=t1; FOO===');
  testcookies(examplesub2, 'foo=t2');
  testcookies(other, '');

  testcookies(example, 'foo=bar; FOO=Xyz; whitespace="a b%c"');
  testcookies(examplesub1, 'foo=t1; FOO===');
  testcookies(examplesub2, 'foo=t2');
  testcookies(other, '');

  parseheaders(other,
    'Set-Cookie: colon1=;;' + LineEnding+
    'Set-Cookie: colon2=";";' + LineEnding+
    'Set-Cookie: colon3="\;";' + LineEnding
  );
  testcookies(example, 'foo=bar; FOO=Xyz; whitespace="a b%c"');
  testcookies(examplesub1, 'foo=t1; FOO===');
  testcookies(examplesub2, 'foo=t2');
  testcookies(other, 'colon1=; colon2="; colon3="\');

  testcookiedump(
   'Set-Cookie: foo=bar; Domain=example.org; HostOnly'#13#10+
   'Set-Cookie: foo=t1; Domain=sub1.example.org; HostOnly'#13#10+
   'Set-Cookie: foo=t2; Domain=sub2.example.org; HostOnly'#13#10+
   'Set-Cookie: FOO=Xyz; Domain=example.org; HostOnly'#13#10+
   'Set-Cookie: whitespace="a b%c"; Domain=example.org; HostOnly'#13#10+
   'Set-Cookie: FOO===; Domain=sub1.example.org; HostOnly'#13#10+
   'Set-Cookie: colon1=; Domain=other.com; HostOnly'#13#10+
   'Set-Cookie: colon2="; Domain=other.com; HostOnly'#13#10+
   'Set-Cookie: colon3="\; Domain=other.com; HostOnly'#13#10
  );

  cm.clear;
  parseheaders(example ,'Set-Cookie: ws1=a b c%d;');
  parseheaders(example ,'Set-Cookie: ws2="a b c%d"');
  parseheaders(other,'Set-Cookie: fo'#9'o'#9'=bar; xyz; asas=x<yasd; as');
  testcookies(example, 'ws1=a b c%d; ws2="a b c%d"');
  testcookies(other, 'fo'#9'o=bar');
  parseheaders(example ,'Set-Cookie:     w   s 3  =  "a b c%d"   ');
  testcookies(example, 'ws1=a b c%d; ws2="a b c%d"; w   s 3="a b c%d"');
  testcookies(other, 'fo'#9'o=bar');

  testcookiedump('Set-Cookie: ws1=a b c%d; Domain=example.org; HostOnly'#13#10+
                 'Set-Cookie: ws2="a b c%d"; Domain=example.org; HostOnly'#13#10+
                 'Set-Cookie: fo'#9'o=bar; Domain=other.com; HostOnly'#13#10+
                 'Set-Cookie: w   s 3="a b c%d"; Domain=example.org; HostOnly'#13#10);

  cm.clear;
  parseheaders(example ,'Set-Cookie: empty=');
  parseheaders(example ,'Set-Cookie: ignoreeasdasdfas');
  parseheaders(example ,'Set-Cookie: empty2=;');
  parseheaders(example ,'Set-Cookie: empty3="";');
  testcookies(example, 'empty=; empty2=; empty3=""');

  cm.clear;
  parseheaders(example ,'Set-Cookie: a=1; Domain=example.org');
  parseheaders(example,'Set-Cookie: b=2; Domain=sub1.example.org');
  parseheaders(examplesub1,'Set-Cookie: c=3; Domain=example.org; HttpOnly');
  parseheaders(examplesub1 ,'Set-Cookie: d=4; Domain=sub1.example.org');
  parseheaders(examplesub2 ,'Set-Cookie: hey=hu; Domain=sub1.example.org');
  testcookies(example, 'a=1; c=3');
  testcookies(examplesub1, 'a=1; c=3; d=4');
  testcookies(examplesub2, 'a=1; c=3');

  parseheaders(examplesub2,'Set-Cookie: c=3a; Domain=example.org;;;;;;');
  parseheaders(examplesub2,'Set-Cookie: c=3b; Path=/; HttpOnly;;; Domain   =   sub2.example.org;');
  parseheaders(examplesub2,'Set-Cookie: x=y; Domain=org');
  parseheaders(examplesub2,'Set-Cookie: x=y; Domain=.org');
  testcookies(example, 'a=1; c=3a');
  testcookies(examplesub1, 'a=1; c=3a; d=4');
  testcookies(examplesub2, 'a=1; c=3a; c=3b');

  testcookiedump(
    'Set-Cookie: a=1; Domain=example.org'#13#10+
    'Set-Cookie: c=3a; Domain=example.org'#13#10+
    'Set-Cookie: d=4; Domain=sub1.example.org'#13#10+
    'Set-Cookie: c=3b; Domain=sub2.example.org; HostOnly'#13#10
  );

  sl.free;
end;

procedure testurl(fullUrl: string; protocol, username, password, host, port, path, params, linktarget: string; combined: string = '');
var
  decoded: TDecodedUrl;
begin
  decoded := decodeURL(fullUrl);
  if combined = '' then test(decoded.combined, fullUrl)
  else test(decoded.combined, combined);
  test(decoded.protocol, protocol, fullUrl);
  test(decoded.username, username, fullUrl);
  test(decoded.password, password, fullUrl);
  test(decoded.host, host, fullUrl);
  test(decoded.port, port, fullUrl);
  test(decoded.path, path, fullUrl);
  test(decoded.params, params, fullUrl);
  test(decoded.linktarget, linktarget, fullUrl);
end;


procedure unitTests();
begin
  //http
  testurl('http://example.org', 'http', '', '', 'example.org', '', '', '', '');
  testurl('http://example.org/', 'http', '', '', 'example.org', '', '/', '', '');
  testurl('http://example.org?foobar', 'http', '', '', 'example.org', '', '', '?foobar', '');
  testurl('http://example.org/?foobar', 'http', '', '', 'example.org', '', '/', '?foobar', '');
  testurl('http://example.org?foobar#123', 'http', '', '', 'example.org', '', '', '?foobar', '#123');
  testurl('http://example.org/?foobar#123', 'http', '', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('http://example.org#123', 'http', '', '', 'example.org', '', '', '', '#123');
  testurl('http://example.org/#123', 'http', '', '', 'example.org', '', '/', '', '#123');
  testurl('http://example.org?foobar#123/abc', 'http', '', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('http://example.org/?foobar#123/abc', 'http', '', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('http://example.org?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://example.org/abc', 'http', '', '', 'example.org', '', '/abc', '', '');
  testurl('http://example.org/abc/', 'http', '', '', 'example.org', '', '/abc/', '', '');
  testurl('http://example.org/abc?foobar', 'http', '', '', 'example.org', '', '/abc',  '?foobar', '');
  testurl('http://example.org/abc/?foobar', 'http', '', '', 'example.org', '', '/abc/', '?foobar', '');
  testurl('http://example.org/abc?foobar#123', 'http', '', '', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('http://example.org/abc/?foobar#123', 'http', '', '', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('http://example.org/abc#123', 'http', '', '', 'example.org', '', '/abc', '',  '#123');
  testurl('http://example.org/abc/#123', 'http', '', '', 'example.org', '', '/abc/',  '', '#123');
  testurl('http://example.org/abc?foobar#123/abc', 'http', '', '', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('http://example.org/abc/?foobar#123/abc', 'http', '', '', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://example.org/abc?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org/abc/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://example.org/abc/def', 'http', '', '', 'example.org', '', '/abc/def', '', '');
  testurl('http://example.org/abc/def/', 'http', '', '', 'example.org', '', '/abc/def/', '', '');
  testurl('http://example.org/abc/def?foobar', 'http', '', '', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('http://example.org/abc/def/?foobar', 'http', '', '', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('http://example.org/abc/def?foobar#123', 'http', '', '', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('http://example.org/abc/def/?foobar#123', 'http', '', '', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://example.org/abc/def#123', 'http', '', '', 'example.org', '', '/abc/def', '',  '#123');
  testurl('http://example.org/abc/def/#123', 'http', '', '', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('http://example.org/abc/def?foobar#123/abc', 'http', '', '', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://example.org/abc/def/?foobar#123/abc', 'http', '', '', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://example.org/abc/def?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org/abc/def/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://example.org:77', 'http', '', '', 'example.org', '77', '', '', '');
  testurl('http://example.org:77/', 'http', '', '', 'example.org', '77', '/', '', '');
  testurl('http://example.org:77?foobar', 'http', '', '', 'example.org', '77', '', '?foobar', '');
  testurl('http://example.org:77/?foobar', 'http', '', '', 'example.org', '77', '/', '?foobar', '');
  testurl('http://example.org:77?foobar#123', 'http', '', '', 'example.org', '77', '', '?foobar', '#123');
  testurl('http://example.org:77/?foobar#123', 'http', '', '', 'example.org', '77', '/', '?foobar', '#123');
  testurl('http://example.org:77#123', 'http', '', '', 'example.org', '77', '', '', '#123');
  testurl('http://example.org:77/#123', 'http', '', '', 'example.org', '77', '/', '', '#123');
  testurl('http://example.org:77?foobar#123/abc', 'http', '', '', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('http://example.org:77/?foobar#123/abc', 'http', '', '', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('http://example.org:77?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org:77/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://example.org:77/abc', 'http', '', '', 'example.org', '77', '/abc', '', '');
  testurl('http://example.org:77/abc/', 'http', '', '', 'example.org', '77', '/abc/', '', '');
  testurl('http://example.org:77/abc?foobar', 'http', '', '', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('http://example.org:77/abc/?foobar', 'http', '', '', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('http://example.org:77/abc?foobar#123', 'http', '', '', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('http://example.org:77/abc/?foobar#123', 'http', '', '', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('http://example.org:77/abc#123', 'http', '', '', 'example.org', '77', '/abc', '',  '#123');
  testurl('http://example.org:77/abc/#123', 'http', '', '', 'example.org', '77', '/abc/',  '', '#123');
  testurl('http://example.org:77/abc?foobar#123/abc', 'http', '', '', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://example.org:77/abc/?foobar#123/abc', 'http', '', '', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://example.org:77/abc?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org:77/abc/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://example.org:77/abc/def', 'http', '', '', 'example.org', '77', '/abc/def', '', '');
  testurl('http://example.org:77/abc/def/', 'http', '', '', 'example.org', '77', '/abc/def/', '', '');
  testurl('http://example.org:77/abc/def?foobar', 'http', '', '', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('http://example.org:77/abc/def/?foobar', 'http', '', '', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('http://example.org:77/abc/def?foobar#123', 'http', '', '', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('http://example.org:77/abc/def/?foobar#123', 'http', '', '', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://example.org:77/abc/def#123', 'http', '', '', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('http://example.org:77/abc/def/#123', 'http', '', '', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('http://example.org:77/abc/def?foobar#123/abc', 'http', '', '', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://example.org:77/abc/def/?foobar#123/abc', 'http', '', '', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://example.org:77/abc/def?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://example.org:77/abc/def/?foobar/asasa/has#123/abc', 'http', '', '', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');




  testurl('http://username@example.org', 'http', 'username', '', 'example.org', '', '', '', '');
  testurl('http://username@example.org/', 'http', 'username', '', 'example.org', '', '/', '', '');
  testurl('http://username@example.org?foobar', 'http', 'username', '', 'example.org', '', '', '?foobar', '');
  testurl('http://username@example.org/?foobar', 'http', 'username', '', 'example.org', '', '/', '?foobar', '');
  testurl('http://username@example.org?foobar#123', 'http', 'username', '', 'example.org', '', '', '?foobar', '#123');
  testurl('http://username@example.org/?foobar#123', 'http', 'username', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('http://username@example.org#123', 'http', 'username', '', 'example.org', '', '', '', '#123');
  testurl('http://username@example.org/#123', 'http', 'username', '', 'example.org', '', '/', '', '#123');
  testurl('http://username@example.org?foobar#123/abc', 'http', 'username', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('http://username@example.org/?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('http://username@example.org?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');


  testurl('http://username@example.org/abc', 'http', 'username', '', 'example.org', '', '/abc', '', '');
  testurl('http://username@example.org/abc/', 'http', 'username', '', 'example.org', '', '/abc/', '', '');
  testurl('http://username@example.org/abc?foobar', 'http', 'username', '', 'example.org', '', '/abc',  '?foobar', '');
  testurl('http://username@example.org/abc/?foobar', 'http', 'username', '', 'example.org', '', '/abc/', '?foobar', '');
  testurl('http://username@example.org/abc?foobar#123', 'http', 'username', '', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('http://username@example.org/abc/?foobar#123', 'http', 'username', '', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('http://username@example.org/abc#123', 'http', 'username', '', 'example.org', '', '/abc', '',  '#123');
  testurl('http://username@example.org/abc/#123', 'http', 'username', '', 'example.org', '', '/abc/',  '', '#123');
  testurl('http://username@example.org/abc?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('http://username@example.org/abc/?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://username@example.org/abc?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org/abc/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@example.org/abc/def', 'http', 'username', '', 'example.org', '', '/abc/def', '', '');
  testurl('http://username@example.org/abc/def/', 'http', 'username', '', 'example.org', '', '/abc/def/', '', '');
  testurl('http://username@example.org/abc/def?foobar', 'http', 'username', '', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('http://username@example.org/abc/def/?foobar', 'http', 'username', '', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('http://username@example.org/abc/def?foobar#123', 'http', 'username', '', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('http://username@example.org/abc/def/?foobar#123', 'http', 'username', '', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://username@example.org/abc/def#123', 'http', 'username', '', 'example.org', '', '/abc/def', '',  '#123');
  testurl('http://username@example.org/abc/def/#123', 'http', 'username', '', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('http://username@example.org/abc/def?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username@example.org/abc/def/?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username@example.org/abc/def?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@example.org:77', 'http', 'username', '', 'example.org', '77', '', '', '');
  testurl('http://username@example.org:77/', 'http', 'username', '', 'example.org', '77', '/', '', '');
  testurl('http://username@example.org:77?foobar', 'http', 'username', '', 'example.org', '77', '', '?foobar', '');
  testurl('http://username@example.org:77/?foobar', 'http', 'username', '', 'example.org', '77', '/', '?foobar', '');
  testurl('http://username@example.org:77?foobar#123', 'http', 'username', '', 'example.org', '77', '', '?foobar', '#123');
  testurl('http://username@example.org:77/?foobar#123', 'http', 'username', '', 'example.org', '77', '/', '?foobar', '#123');
  testurl('http://username@example.org:77#123', 'http', 'username', '', 'example.org', '77', '', '', '#123');
  testurl('http://username@example.org:77/#123', 'http', 'username', '', 'example.org', '77', '/', '', '#123');
  testurl('http://username@example.org:77?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('http://username@example.org:77/?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('http://username@example.org:77?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org:77/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://username@example.org:77/abc', 'http', 'username', '', 'example.org', '77', '/abc', '', '');
  testurl('http://username@example.org:77/abc/', 'http', 'username', '', 'example.org', '77', '/abc/', '', '');
  testurl('http://username@example.org:77/abc?foobar', 'http', 'username', '', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('http://username@example.org:77/abc/?foobar', 'http', 'username', '', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('http://username@example.org:77/abc?foobar#123', 'http', 'username', '', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('http://username@example.org:77/abc/?foobar#123', 'http', 'username', '', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('http://username@example.org:77/abc#123', 'http', 'username', '', 'example.org', '77', '/abc', '',  '#123');
  testurl('http://username@example.org:77/abc/#123', 'http', 'username', '', 'example.org', '77', '/abc/',  '', '#123');
  testurl('http://username@example.org:77/abc?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://username@example.org:77/abc/?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://username@example.org:77/abc?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org:77/abc/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@example.org:77/abc/def', 'http', 'username', '', 'example.org', '77', '/abc/def', '', '');
  testurl('http://username@example.org:77/abc/def/', 'http', 'username', '', 'example.org', '77', '/abc/def/', '', '');
  testurl('http://username@example.org:77/abc/def?foobar', 'http', 'username', '', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('http://username@example.org:77/abc/def/?foobar', 'http', 'username', '', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('http://username@example.org:77/abc/def?foobar#123', 'http', 'username', '', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('http://username@example.org:77/abc/def/?foobar#123', 'http', 'username', '', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://username@example.org:77/abc/def#123', 'http', 'username', '', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('http://username@example.org:77/abc/def/#123', 'http', 'username', '', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('http://username@example.org:77/abc/def?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username@example.org:77/abc/def/?foobar#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username@example.org:77/abc/def?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org:77/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');


  testurl('http://username@example.org', 'http', 'username', '', 'example.org', '', '', '', '');
  testurl('http://username@example.org/', 'http', 'username', '', 'example.org', '', '/', '', '');
  testurl('http://username@example.org?foobar', 'http', 'username', '', 'example.org', '', '', '?foobar', '');
  testurl('http://username@example.org/?foobar', 'http', 'username', '', 'example.org', '', '/', '?foobar', '');
  testurl('http://username@example.org?foobar#123', 'http', 'username', '', 'example.org', '', '', '?foobar', '#123');
  testurl('http://username@example.org/?foobar#123', 'http', 'username', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('http://username@example.org#123', 'http', 'username', '', 'example.org', '', '', '', '#123');
  testurl('http://username@example.org/#123', 'http', 'username', '', 'example.org', '', '/', '', '#123');
  testurl('http://username@example.org?foobar#123/abc', 'http', 'username', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('http://username@example.org/?foobar#123/abc', 'http', 'username', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('http://username@example.org?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@example.org/?foobar/asasa/has#123/abc', 'http', 'username', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');




  testurl('http://username:password@example.org/abc', 'http', 'username', 'password', 'example.org', '', '/abc', '', '');
  testurl('http://username:password@example.org/abc/', 'http', 'username', 'password', 'example.org', '', '/abc/', '', '');
  testurl('http://username:password@example.org/abc?foobar', 'http', 'username', 'password', 'example.org', '', '/abc',  '?foobar', '');
  testurl('http://username:password@example.org/abc/?foobar', 'http', 'username', 'password', 'example.org', '', '/abc/', '?foobar', '');
  testurl('http://username:password@example.org/abc?foobar#123', 'http', 'username', 'password', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('http://username:password@example.org/abc/?foobar#123', 'http', 'username', 'password', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('http://username:password@example.org/abc#123', 'http', 'username', 'password', 'example.org', '', '/abc', '',  '#123');
  testurl('http://username:password@example.org/abc/#123', 'http', 'username', 'password', 'example.org', '', '/abc/',  '', '#123');
  testurl('http://username:password@example.org/abc?foobar#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('http://username:password@example.org/abc/?foobar#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://username:password@example.org/abc?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@example.org/abc/?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@example.org/abc/def', 'http', 'username', 'password', 'example.org', '', '/abc/def', '', '');
  testurl('http://username:password@example.org/abc/def/', 'http', 'username', 'password', 'example.org', '', '/abc/def/', '', '');
  testurl('http://username:password@example.org/abc/def?foobar', 'http', 'username', 'password', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('http://username:password@example.org/abc/def/?foobar', 'http', 'username', 'password', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('http://username:password@example.org/abc/def?foobar#123', 'http', 'username', 'password', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('http://username:password@example.org/abc/def/?foobar#123', 'http', 'username', 'password', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://username:password@example.org/abc/def#123', 'http', 'username', 'password', 'example.org', '', '/abc/def', '',  '#123');
  testurl('http://username:password@example.org/abc/def/#123', 'http', 'username', 'password', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('http://username:password@example.org/abc/def?foobar#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username:password@example.org/abc/def/?foobar#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username:password@example.org/abc/def?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@example.org/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@example.org:77', 'http', 'username', 'password', 'example.org', '77', '', '', '');
  testurl('http://username:password@example.org:77/', 'http', 'username', 'password', 'example.org', '77', '/', '', '');
  testurl('http://username:password@example.org:77?foobar', 'http', 'username', 'password', 'example.org', '77', '', '?foobar', '');
  testurl('http://username:password@example.org:77/?foobar', 'http', 'username', 'password', 'example.org', '77', '/', '?foobar', '');
  testurl('http://username:password@example.org:77?foobar#123', 'http', 'username', 'password', 'example.org', '77', '', '?foobar', '#123');
  testurl('http://username:password@example.org:77/?foobar#123', 'http', 'username', 'password', 'example.org', '77', '/', '?foobar', '#123');
  testurl('http://username:password@example.org:77#123', 'http', 'username', 'password', 'example.org', '77', '', '', '#123');
  testurl('http://username:password@example.org:77/#123', 'http', 'username', 'password', 'example.org', '77', '/', '', '#123');
  testurl('http://username:password@example.org:77?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77/?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@example.org:77/?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@example.org:77/abc', 'http', 'username', 'password', 'example.org', '77', '/abc', '', '');
  testurl('http://username:password@example.org:77/abc/', 'http', 'username', 'password', 'example.org', '77', '/abc/', '', '');
  testurl('http://username:password@example.org:77/abc?foobar', 'http', 'username', 'password', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('http://username:password@example.org:77/abc/?foobar', 'http', 'username', 'password', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('http://username:password@example.org:77/abc?foobar#123', 'http', 'username', 'password', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('http://username:password@example.org:77/abc/?foobar#123', 'http', 'username', 'password', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('http://username:password@example.org:77/abc#123', 'http', 'username', 'password', 'example.org', '77', '/abc', '',  '#123');
  testurl('http://username:password@example.org:77/abc/#123', 'http', 'username', 'password', 'example.org', '77', '/abc/',  '', '#123');
  testurl('http://username:password@example.org:77/abc?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77/abc/?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77/abc?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@example.org:77/abc/?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@example.org:77/abc/def', 'http', 'username', 'password', 'example.org', '77', '/abc/def', '', '');
  testurl('http://username:password@example.org:77/abc/def/', 'http', 'username', 'password', 'example.org', '77', '/abc/def/', '', '');
  testurl('http://username:password@example.org:77/abc/def?foobar', 'http', 'username', 'password', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('http://username:password@example.org:77/abc/def/?foobar', 'http', 'username', 'password', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('http://username:password@example.org:77/abc/def?foobar#123', 'http', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('http://username:password@example.org:77/abc/def/?foobar#123', 'http', 'username', 'password', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://username:password@example.org:77/abc/def#123', 'http', 'username', 'password', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('http://username:password@example.org:77/abc/def/#123', 'http', 'username', 'password', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('http://username:password@example.org:77/abc/def?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77/abc/def/?foobar#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username:password@example.org:77/abc/def?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@example.org:77/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', 'password', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');















  testurl('http://[::1]', 'http', '', '', '::1', '', '', '', '');
  testurl('http://[::1]/', 'http', '', '', '::1', '', '/', '', '');
  testurl('http://[::1]?foobar', 'http', '', '', '::1', '', '', '?foobar', '');
  testurl('http://[::1]/?foobar', 'http', '', '', '::1', '', '/', '?foobar', '');
  testurl('http://[::1]?foobar#123', 'http', '', '', '::1', '', '', '?foobar', '#123');
  testurl('http://[::1]/?foobar#123', 'http', '', '', '::1', '', '/', '?foobar', '#123');
  testurl('http://[::1]#123', 'http', '', '', '::1', '', '', '', '#123');
  testurl('http://[::1]/#123', 'http', '', '', '::1', '', '/', '', '#123');
  testurl('http://[::1]?foobar#123/abc', 'http', '', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('http://[::1]/?foobar#123/abc', 'http', '', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('http://[::1]?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://[::1]/abc', 'http', '', '', '::1', '', '/abc', '', '');
  testurl('http://[::1]/abc/', 'http', '', '', '::1', '', '/abc/', '', '');
  testurl('http://[::1]/abc?foobar', 'http', '', '', '::1', '', '/abc',  '?foobar', '');
  testurl('http://[::1]/abc/?foobar', 'http', '', '', '::1', '', '/abc/', '?foobar', '');
  testurl('http://[::1]/abc?foobar#123', 'http', '', '', '::1', '', '/abc', '?foobar', '#123');
  testurl('http://[::1]/abc/?foobar#123', 'http', '', '', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('http://[::1]/abc#123', 'http', '', '', '::1', '', '/abc', '',  '#123');
  testurl('http://[::1]/abc/#123', 'http', '', '', '::1', '', '/abc/',  '', '#123');
  testurl('http://[::1]/abc?foobar#123/abc', 'http', '', '', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('http://[::1]/abc/?foobar#123/abc', 'http', '', '', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://[::1]/abc?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]/abc/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://[::1]/abc/def', 'http', '', '', '::1', '', '/abc/def', '', '');
  testurl('http://[::1]/abc/def/', 'http', '', '', '::1', '', '/abc/def/', '', '');
  testurl('http://[::1]/abc/def?foobar', 'http', '', '', '::1', '', '/abc/def',  '?foobar', '');
  testurl('http://[::1]/abc/def/?foobar', 'http', '', '', '::1', '', '/abc/def/', '?foobar', '');
  testurl('http://[::1]/abc/def?foobar#123', 'http', '', '', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('http://[::1]/abc/def/?foobar#123', 'http', '', '', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://[::1]/abc/def#123', 'http', '', '', '::1', '', '/abc/def', '',  '#123');
  testurl('http://[::1]/abc/def/#123', 'http', '', '', '::1', '', '/abc/def/',  '', '#123');
  testurl('http://[::1]/abc/def?foobar#123/abc', 'http', '', '', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://[::1]/abc/def/?foobar#123/abc', 'http', '', '', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://[::1]/abc/def?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]/abc/def/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://[::1]:77', 'http', '', '', '::1', '77', '', '', '');
  testurl('http://[::1]:77/', 'http', '', '', '::1', '77', '/', '', '');
  testurl('http://[::1]:77?foobar', 'http', '', '', '::1', '77', '', '?foobar', '');
  testurl('http://[::1]:77/?foobar', 'http', '', '', '::1', '77', '/', '?foobar', '');
  testurl('http://[::1]:77?foobar#123', 'http', '', '', '::1', '77', '', '?foobar', '#123');
  testurl('http://[::1]:77/?foobar#123', 'http', '', '', '::1', '77', '/', '?foobar', '#123');
  testurl('http://[::1]:77#123', 'http', '', '', '::1', '77', '', '', '#123');
  testurl('http://[::1]:77/#123', 'http', '', '', '::1', '77', '/', '', '#123');
  testurl('http://[::1]:77?foobar#123/abc', 'http', '', '', '::1', '77', '', '?foobar', '#123/abc');
  testurl('http://[::1]:77/?foobar#123/abc', 'http', '', '', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('http://[::1]:77?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]:77/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://[::1]:77/abc', 'http', '', '', '::1', '77', '/abc', '', '');
  testurl('http://[::1]:77/abc/', 'http', '', '', '::1', '77', '/abc/', '', '');
  testurl('http://[::1]:77/abc?foobar', 'http', '', '', '::1', '77', '/abc',  '?foobar', '');
  testurl('http://[::1]:77/abc/?foobar', 'http', '', '', '::1', '77', '/abc/', '?foobar', '');
  testurl('http://[::1]:77/abc?foobar#123', 'http', '', '', '::1', '77', '/abc', '?foobar', '#123');
  testurl('http://[::1]:77/abc/?foobar#123', 'http', '', '', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('http://[::1]:77/abc#123', 'http', '', '', '::1', '77', '/abc', '',  '#123');
  testurl('http://[::1]:77/abc/#123', 'http', '', '', '::1', '77', '/abc/',  '', '#123');
  testurl('http://[::1]:77/abc?foobar#123/abc', 'http', '', '', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://[::1]:77/abc/?foobar#123/abc', 'http', '', '', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://[::1]:77/abc?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]:77/abc/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://[::1]:77/abc/def', 'http', '', '', '::1', '77', '/abc/def', '', '');
  testurl('http://[::1]:77/abc/def/', 'http', '', '', '::1', '77', '/abc/def/', '', '');
  testurl('http://[::1]:77/abc/def?foobar', 'http', '', '', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('http://[::1]:77/abc/def/?foobar', 'http', '', '', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('http://[::1]:77/abc/def?foobar#123', 'http', '', '', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('http://[::1]:77/abc/def/?foobar#123', 'http', '', '', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://[::1]:77/abc/def#123', 'http', '', '', '::1', '77', '/abc/def', '',  '#123');
  testurl('http://[::1]:77/abc/def/#123', 'http', '', '', '::1', '77', '/abc/def/',  '', '#123');
  testurl('http://[::1]:77/abc/def?foobar#123/abc', 'http', '', '', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://[::1]:77/abc/def/?foobar#123/abc', 'http', '', '', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://[::1]:77/abc/def?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'http', '', '', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');




  testurl('http://username@[::1]', 'http', 'username', '', '::1', '', '', '', '');
  testurl('http://username@[::1]/', 'http', 'username', '', '::1', '', '/', '', '');
  testurl('http://username@[::1]?foobar', 'http', 'username', '', '::1', '', '', '?foobar', '');
  testurl('http://username@[::1]/?foobar', 'http', 'username', '', '::1', '', '/', '?foobar', '');
  testurl('http://username@[::1]?foobar#123', 'http', 'username', '', '::1', '', '', '?foobar', '#123');
  testurl('http://username@[::1]/?foobar#123', 'http', 'username', '', '::1', '', '/', '?foobar', '#123');
  testurl('http://username@[::1]#123', 'http', 'username', '', '::1', '', '', '', '#123');
  testurl('http://username@[::1]/#123', 'http', 'username', '', '::1', '', '/', '', '#123');
  testurl('http://username@[::1]?foobar#123/abc', 'http', 'username', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('http://username@[::1]/?foobar#123/abc', 'http', 'username', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('http://username@[::1]?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');


  testurl('http://username@[::1]/abc', 'http', 'username', '', '::1', '', '/abc', '', '');
  testurl('http://username@[::1]/abc/', 'http', 'username', '', '::1', '', '/abc/', '', '');
  testurl('http://username@[::1]/abc?foobar', 'http', 'username', '', '::1', '', '/abc',  '?foobar', '');
  testurl('http://username@[::1]/abc/?foobar', 'http', 'username', '', '::1', '', '/abc/', '?foobar', '');
  testurl('http://username@[::1]/abc?foobar#123', 'http', 'username', '', '::1', '', '/abc', '?foobar', '#123');
  testurl('http://username@[::1]/abc/?foobar#123', 'http', 'username', '', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('http://username@[::1]/abc#123', 'http', 'username', '', '::1', '', '/abc', '',  '#123');
  testurl('http://username@[::1]/abc/#123', 'http', 'username', '', '::1', '', '/abc/',  '', '#123');
  testurl('http://username@[::1]/abc?foobar#123/abc', 'http', 'username', '', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('http://username@[::1]/abc/?foobar#123/abc', 'http', 'username', '', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://username@[::1]/abc?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]/abc/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@[::1]/abc/def', 'http', 'username', '', '::1', '', '/abc/def', '', '');
  testurl('http://username@[::1]/abc/def/', 'http', 'username', '', '::1', '', '/abc/def/', '', '');
  testurl('http://username@[::1]/abc/def?foobar', 'http', 'username', '', '::1', '', '/abc/def',  '?foobar', '');
  testurl('http://username@[::1]/abc/def/?foobar', 'http', 'username', '', '::1', '', '/abc/def/', '?foobar', '');
  testurl('http://username@[::1]/abc/def?foobar#123', 'http', 'username', '', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('http://username@[::1]/abc/def/?foobar#123', 'http', 'username', '', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://username@[::1]/abc/def#123', 'http', 'username', '', '::1', '', '/abc/def', '',  '#123');
  testurl('http://username@[::1]/abc/def/#123', 'http', 'username', '', '::1', '', '/abc/def/',  '', '#123');
  testurl('http://username@[::1]/abc/def?foobar#123/abc', 'http', 'username', '', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username@[::1]/abc/def/?foobar#123/abc', 'http', 'username', '', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username@[::1]/abc/def?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@[::1]:77', 'http', 'username', '', '::1', '77', '', '', '');
  testurl('http://username@[::1]:77/', 'http', 'username', '', '::1', '77', '/', '', '');
  testurl('http://username@[::1]:77?foobar', 'http', 'username', '', '::1', '77', '', '?foobar', '');
  testurl('http://username@[::1]:77/?foobar', 'http', 'username', '', '::1', '77', '/', '?foobar', '');
  testurl('http://username@[::1]:77?foobar#123', 'http', 'username', '', '::1', '77', '', '?foobar', '#123');
  testurl('http://username@[::1]:77/?foobar#123', 'http', 'username', '', '::1', '77', '/', '?foobar', '#123');
  testurl('http://username@[::1]:77#123', 'http', 'username', '', '::1', '77', '', '', '#123');
  testurl('http://username@[::1]:77/#123', 'http', 'username', '', '::1', '77', '/', '', '#123');
  testurl('http://username@[::1]:77?foobar#123/abc', 'http', 'username', '', '::1', '77', '', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77/?foobar#123/abc', 'http', 'username', '', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]:77/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://username@[::1]:77/abc', 'http', 'username', '', '::1', '77', '/abc', '', '');
  testurl('http://username@[::1]:77/abc/', 'http', 'username', '', '::1', '77', '/abc/', '', '');
  testurl('http://username@[::1]:77/abc?foobar', 'http', 'username', '', '::1', '77', '/abc',  '?foobar', '');
  testurl('http://username@[::1]:77/abc/?foobar', 'http', 'username', '', '::1', '77', '/abc/', '?foobar', '');
  testurl('http://username@[::1]:77/abc?foobar#123', 'http', 'username', '', '::1', '77', '/abc', '?foobar', '#123');
  testurl('http://username@[::1]:77/abc/?foobar#123', 'http', 'username', '', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('http://username@[::1]:77/abc#123', 'http', 'username', '', '::1', '77', '/abc', '',  '#123');
  testurl('http://username@[::1]:77/abc/#123', 'http', 'username', '', '::1', '77', '/abc/',  '', '#123');
  testurl('http://username@[::1]:77/abc?foobar#123/abc', 'http', 'username', '', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77/abc/?foobar#123/abc', 'http', 'username', '', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77/abc?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]:77/abc/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username@[::1]:77/abc/def', 'http', 'username', '', '::1', '77', '/abc/def', '', '');
  testurl('http://username@[::1]:77/abc/def/', 'http', 'username', '', '::1', '77', '/abc/def/', '', '');
  testurl('http://username@[::1]:77/abc/def?foobar', 'http', 'username', '', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('http://username@[::1]:77/abc/def/?foobar', 'http', 'username', '', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('http://username@[::1]:77/abc/def?foobar#123', 'http', 'username', '', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('http://username@[::1]:77/abc/def/?foobar#123', 'http', 'username', '', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://username@[::1]:77/abc/def#123', 'http', 'username', '', '::1', '77', '/abc/def', '',  '#123');
  testurl('http://username@[::1]:77/abc/def/#123', 'http', 'username', '', '::1', '77', '/abc/def/',  '', '#123');
  testurl('http://username@[::1]:77/abc/def?foobar#123/abc', 'http', 'username', '', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77/abc/def/?foobar#123/abc', 'http', 'username', '', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username@[::1]:77/abc/def?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');


  testurl('http://username@[::1]', 'http', 'username', '', '::1', '', '', '', '');
  testurl('http://username@[::1]/', 'http', 'username', '', '::1', '', '/', '', '');
  testurl('http://username@[::1]?foobar', 'http', 'username', '', '::1', '', '', '?foobar', '');
  testurl('http://username@[::1]/?foobar', 'http', 'username', '', '::1', '', '/', '?foobar', '');
  testurl('http://username@[::1]?foobar#123', 'http', 'username', '', '::1', '', '', '?foobar', '#123');
  testurl('http://username@[::1]/?foobar#123', 'http', 'username', '', '::1', '', '/', '?foobar', '#123');
  testurl('http://username@[::1]#123', 'http', 'username', '', '::1', '', '', '', '#123');
  testurl('http://username@[::1]/#123', 'http', 'username', '', '::1', '', '/', '', '#123');
  testurl('http://username@[::1]?foobar#123/abc', 'http', 'username', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('http://username@[::1]/?foobar#123/abc', 'http', 'username', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('http://username@[::1]?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username@[::1]/?foobar/asasa/has#123/abc', 'http', 'username', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');




  testurl('http://username:password@[::1]/abc', 'http', 'username', 'password', '::1', '', '/abc', '', '');
  testurl('http://username:password@[::1]/abc/', 'http', 'username', 'password', '::1', '', '/abc/', '', '');
  testurl('http://username:password@[::1]/abc?foobar', 'http', 'username', 'password', '::1', '', '/abc',  '?foobar', '');
  testurl('http://username:password@[::1]/abc/?foobar', 'http', 'username', 'password', '::1', '', '/abc/', '?foobar', '');
  testurl('http://username:password@[::1]/abc?foobar#123', 'http', 'username', 'password', '::1', '', '/abc', '?foobar', '#123');
  testurl('http://username:password@[::1]/abc/?foobar#123', 'http', 'username', 'password', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('http://username:password@[::1]/abc#123', 'http', 'username', 'password', '::1', '', '/abc', '',  '#123');
  testurl('http://username:password@[::1]/abc/#123', 'http', 'username', 'password', '::1', '', '/abc/',  '', '#123');
  testurl('http://username:password@[::1]/abc?foobar#123/abc', 'http', 'username', 'password', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]/abc/?foobar#123/abc', 'http', 'username', 'password', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]/abc?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@[::1]/abc/?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@[::1]/abc/def', 'http', 'username', 'password', '::1', '', '/abc/def', '', '');
  testurl('http://username:password@[::1]/abc/def/', 'http', 'username', 'password', '::1', '', '/abc/def/', '', '');
  testurl('http://username:password@[::1]/abc/def?foobar', 'http', 'username', 'password', '::1', '', '/abc/def',  '?foobar', '');
  testurl('http://username:password@[::1]/abc/def/?foobar', 'http', 'username', 'password', '::1', '', '/abc/def/', '?foobar', '');
  testurl('http://username:password@[::1]/abc/def?foobar#123', 'http', 'username', 'password', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('http://username:password@[::1]/abc/def/?foobar#123', 'http', 'username', 'password', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('http://username:password@[::1]/abc/def#123', 'http', 'username', 'password', '::1', '', '/abc/def', '',  '#123');
  testurl('http://username:password@[::1]/abc/def/#123', 'http', 'username', 'password', '::1', '', '/abc/def/',  '', '#123');
  testurl('http://username:password@[::1]/abc/def?foobar#123/abc', 'http', 'username', 'password', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]/abc/def/?foobar#123/abc', 'http', 'username', 'password', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]/abc/def?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@[::1]/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@[::1]:77', 'http', 'username', 'password', '::1', '77', '', '', '');
  testurl('http://username:password@[::1]:77/', 'http', 'username', 'password', '::1', '77', '/', '', '');
  testurl('http://username:password@[::1]:77?foobar', 'http', 'username', 'password', '::1', '77', '', '?foobar', '');
  testurl('http://username:password@[::1]:77/?foobar', 'http', 'username', 'password', '::1', '77', '/', '?foobar', '');
  testurl('http://username:password@[::1]:77?foobar#123', 'http', 'username', 'password', '::1', '77', '', '?foobar', '#123');
  testurl('http://username:password@[::1]:77/?foobar#123', 'http', 'username', 'password', '::1', '77', '/', '?foobar', '#123');
  testurl('http://username:password@[::1]:77#123', 'http', 'username', 'password', '::1', '77', '', '', '#123');
  testurl('http://username:password@[::1]:77/#123', 'http', 'username', 'password', '::1', '77', '/', '', '#123');
  testurl('http://username:password@[::1]:77?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77/?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@[::1]:77/?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@[::1]:77/abc', 'http', 'username', 'password', '::1', '77', '/abc', '', '');
  testurl('http://username:password@[::1]:77/abc/', 'http', 'username', 'password', '::1', '77', '/abc/', '', '');
  testurl('http://username:password@[::1]:77/abc?foobar', 'http', 'username', 'password', '::1', '77', '/abc',  '?foobar', '');
  testurl('http://username:password@[::1]:77/abc/?foobar', 'http', 'username', 'password', '::1', '77', '/abc/', '?foobar', '');
  testurl('http://username:password@[::1]:77/abc?foobar#123', 'http', 'username', 'password', '::1', '77', '/abc', '?foobar', '#123');
  testurl('http://username:password@[::1]:77/abc/?foobar#123', 'http', 'username', 'password', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('http://username:password@[::1]:77/abc#123', 'http', 'username', 'password', '::1', '77', '/abc', '',  '#123');
  testurl('http://username:password@[::1]:77/abc/#123', 'http', 'username', 'password', '::1', '77', '/abc/',  '', '#123');
  testurl('http://username:password@[::1]:77/abc?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77/abc/?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77/abc?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@[::1]:77/abc/?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('http://username:password@[::1]:77/abc/def', 'http', 'username', 'password', '::1', '77', '/abc/def', '', '');
  testurl('http://username:password@[::1]:77/abc/def/', 'http', 'username', 'password', '::1', '77', '/abc/def/', '', '');
  testurl('http://username:password@[::1]:77/abc/def?foobar', 'http', 'username', 'password', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('http://username:password@[::1]:77/abc/def/?foobar', 'http', 'username', 'password', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('http://username:password@[::1]:77/abc/def?foobar#123', 'http', 'username', 'password', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('http://username:password@[::1]:77/abc/def/?foobar#123', 'http', 'username', 'password', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('http://username:password@[::1]:77/abc/def#123', 'http', 'username', 'password', '::1', '77', '/abc/def', '',  '#123');
  testurl('http://username:password@[::1]:77/abc/def/#123', 'http', 'username', 'password', '::1', '77', '/abc/def/',  '', '#123');
  testurl('http://username:password@[::1]:77/abc/def?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77/abc/def/?foobar#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('http://username:password@[::1]:77/abc/def?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('http://username:password@[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'http', 'username', 'password', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');







































  //https
  testurl('https://example.org', 'https', '', '', 'example.org', '', '', '', '');
  testurl('https://example.org/', 'https', '', '', 'example.org', '', '/', '', '');
  testurl('https://example.org?foobar', 'https', '', '', 'example.org', '', '', '?foobar', '');
  testurl('https://example.org/?foobar', 'https', '', '', 'example.org', '', '/', '?foobar', '');
  testurl('https://example.org?foobar#123', 'https', '', '', 'example.org', '', '', '?foobar', '#123');
  testurl('https://example.org/?foobar#123', 'https', '', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('https://example.org#123', 'https', '', '', 'example.org', '', '', '', '#123');
  testurl('https://example.org/#123', 'https', '', '', 'example.org', '', '/', '', '#123');
  testurl('https://example.org?foobar#123/abc', 'https', '', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('https://example.org/?foobar#123/abc', 'https', '', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('https://example.org?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://example.org/abc', 'https', '', '', 'example.org', '', '/abc', '', '');
  testurl('https://example.org/abc/', 'https', '', '', 'example.org', '', '/abc/', '', '');
  testurl('https://example.org/abc?foobar', 'https', '', '', 'example.org', '', '/abc',  '?foobar', '');
  testurl('https://example.org/abc/?foobar', 'https', '', '', 'example.org', '', '/abc/', '?foobar', '');
  testurl('https://example.org/abc?foobar#123', 'https', '', '', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('https://example.org/abc/?foobar#123', 'https', '', '', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('https://example.org/abc#123', 'https', '', '', 'example.org', '', '/abc', '',  '#123');
  testurl('https://example.org/abc/#123', 'https', '', '', 'example.org', '', '/abc/',  '', '#123');
  testurl('https://example.org/abc?foobar#123/abc', 'https', '', '', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('https://example.org/abc/?foobar#123/abc', 'https', '', '', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://example.org/abc?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org/abc/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://example.org/abc/def', 'https', '', '', 'example.org', '', '/abc/def', '', '');
  testurl('https://example.org/abc/def/', 'https', '', '', 'example.org', '', '/abc/def/', '', '');
  testurl('https://example.org/abc/def?foobar', 'https', '', '', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('https://example.org/abc/def/?foobar', 'https', '', '', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('https://example.org/abc/def?foobar#123', 'https', '', '', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('https://example.org/abc/def/?foobar#123', 'https', '', '', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://example.org/abc/def#123', 'https', '', '', 'example.org', '', '/abc/def', '',  '#123');
  testurl('https://example.org/abc/def/#123', 'https', '', '', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('https://example.org/abc/def?foobar#123/abc', 'https', '', '', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://example.org/abc/def/?foobar#123/abc', 'https', '', '', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://example.org/abc/def?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org/abc/def/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://example.org:77', 'https', '', '', 'example.org', '77', '', '', '');
  testurl('https://example.org:77/', 'https', '', '', 'example.org', '77', '/', '', '');
  testurl('https://example.org:77?foobar', 'https', '', '', 'example.org', '77', '', '?foobar', '');
  testurl('https://example.org:77/?foobar', 'https', '', '', 'example.org', '77', '/', '?foobar', '');
  testurl('https://example.org:77?foobar#123', 'https', '', '', 'example.org', '77', '', '?foobar', '#123');
  testurl('https://example.org:77/?foobar#123', 'https', '', '', 'example.org', '77', '/', '?foobar', '#123');
  testurl('https://example.org:77#123', 'https', '', '', 'example.org', '77', '', '', '#123');
  testurl('https://example.org:77/#123', 'https', '', '', 'example.org', '77', '/', '', '#123');
  testurl('https://example.org:77?foobar#123/abc', 'https', '', '', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('https://example.org:77/?foobar#123/abc', 'https', '', '', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('https://example.org:77?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org:77/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://example.org:77/abc', 'https', '', '', 'example.org', '77', '/abc', '', '');
  testurl('https://example.org:77/abc/', 'https', '', '', 'example.org', '77', '/abc/', '', '');
  testurl('https://example.org:77/abc?foobar', 'https', '', '', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('https://example.org:77/abc/?foobar', 'https', '', '', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('https://example.org:77/abc?foobar#123', 'https', '', '', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('https://example.org:77/abc/?foobar#123', 'https', '', '', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('https://example.org:77/abc#123', 'https', '', '', 'example.org', '77', '/abc', '',  '#123');
  testurl('https://example.org:77/abc/#123', 'https', '', '', 'example.org', '77', '/abc/',  '', '#123');
  testurl('https://example.org:77/abc?foobar#123/abc', 'https', '', '', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://example.org:77/abc/?foobar#123/abc', 'https', '', '', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://example.org:77/abc?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org:77/abc/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://example.org:77/abc/def', 'https', '', '', 'example.org', '77', '/abc/def', '', '');
  testurl('https://example.org:77/abc/def/', 'https', '', '', 'example.org', '77', '/abc/def/', '', '');
  testurl('https://example.org:77/abc/def?foobar', 'https', '', '', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('https://example.org:77/abc/def/?foobar', 'https', '', '', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('https://example.org:77/abc/def?foobar#123', 'https', '', '', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('https://example.org:77/abc/def/?foobar#123', 'https', '', '', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://example.org:77/abc/def#123', 'https', '', '', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('https://example.org:77/abc/def/#123', 'https', '', '', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('https://example.org:77/abc/def?foobar#123/abc', 'https', '', '', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://example.org:77/abc/def/?foobar#123/abc', 'https', '', '', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://example.org:77/abc/def?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://example.org:77/abc/def/?foobar/asasa/has#123/abc', 'https', '', '', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');




  testurl('https://username@example.org', 'https', 'username', '', 'example.org', '', '', '', '');
  testurl('https://username@example.org/', 'https', 'username', '', 'example.org', '', '/', '', '');
  testurl('https://username@example.org?foobar', 'https', 'username', '', 'example.org', '', '', '?foobar', '');
  testurl('https://username@example.org/?foobar', 'https', 'username', '', 'example.org', '', '/', '?foobar', '');
  testurl('https://username@example.org?foobar#123', 'https', 'username', '', 'example.org', '', '', '?foobar', '#123');
  testurl('https://username@example.org/?foobar#123', 'https', 'username', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('https://username@example.org#123', 'https', 'username', '', 'example.org', '', '', '', '#123');
  testurl('https://username@example.org/#123', 'https', 'username', '', 'example.org', '', '/', '', '#123');
  testurl('https://username@example.org?foobar#123/abc', 'https', 'username', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('https://username@example.org/?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('https://username@example.org?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');


  testurl('https://username@example.org/abc', 'https', 'username', '', 'example.org', '', '/abc', '', '');
  testurl('https://username@example.org/abc/', 'https', 'username', '', 'example.org', '', '/abc/', '', '');
  testurl('https://username@example.org/abc?foobar', 'https', 'username', '', 'example.org', '', '/abc',  '?foobar', '');
  testurl('https://username@example.org/abc/?foobar', 'https', 'username', '', 'example.org', '', '/abc/', '?foobar', '');
  testurl('https://username@example.org/abc?foobar#123', 'https', 'username', '', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('https://username@example.org/abc/?foobar#123', 'https', 'username', '', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('https://username@example.org/abc#123', 'https', 'username', '', 'example.org', '', '/abc', '',  '#123');
  testurl('https://username@example.org/abc/#123', 'https', 'username', '', 'example.org', '', '/abc/',  '', '#123');
  testurl('https://username@example.org/abc?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('https://username@example.org/abc/?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://username@example.org/abc?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org/abc/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@example.org/abc/def', 'https', 'username', '', 'example.org', '', '/abc/def', '', '');
  testurl('https://username@example.org/abc/def/', 'https', 'username', '', 'example.org', '', '/abc/def/', '', '');
  testurl('https://username@example.org/abc/def?foobar', 'https', 'username', '', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('https://username@example.org/abc/def/?foobar', 'https', 'username', '', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('https://username@example.org/abc/def?foobar#123', 'https', 'username', '', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('https://username@example.org/abc/def/?foobar#123', 'https', 'username', '', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://username@example.org/abc/def#123', 'https', 'username', '', 'example.org', '', '/abc/def', '',  '#123');
  testurl('https://username@example.org/abc/def/#123', 'https', 'username', '', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('https://username@example.org/abc/def?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username@example.org/abc/def/?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username@example.org/abc/def?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@example.org:77', 'https', 'username', '', 'example.org', '77', '', '', '');
  testurl('https://username@example.org:77/', 'https', 'username', '', 'example.org', '77', '/', '', '');
  testurl('https://username@example.org:77?foobar', 'https', 'username', '', 'example.org', '77', '', '?foobar', '');
  testurl('https://username@example.org:77/?foobar', 'https', 'username', '', 'example.org', '77', '/', '?foobar', '');
  testurl('https://username@example.org:77?foobar#123', 'https', 'username', '', 'example.org', '77', '', '?foobar', '#123');
  testurl('https://username@example.org:77/?foobar#123', 'https', 'username', '', 'example.org', '77', '/', '?foobar', '#123');
  testurl('https://username@example.org:77#123', 'https', 'username', '', 'example.org', '77', '', '', '#123');
  testurl('https://username@example.org:77/#123', 'https', 'username', '', 'example.org', '77', '/', '', '#123');
  testurl('https://username@example.org:77?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('https://username@example.org:77/?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('https://username@example.org:77?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org:77/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://username@example.org:77/abc', 'https', 'username', '', 'example.org', '77', '/abc', '', '');
  testurl('https://username@example.org:77/abc/', 'https', 'username', '', 'example.org', '77', '/abc/', '', '');
  testurl('https://username@example.org:77/abc?foobar', 'https', 'username', '', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('https://username@example.org:77/abc/?foobar', 'https', 'username', '', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('https://username@example.org:77/abc?foobar#123', 'https', 'username', '', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('https://username@example.org:77/abc/?foobar#123', 'https', 'username', '', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('https://username@example.org:77/abc#123', 'https', 'username', '', 'example.org', '77', '/abc', '',  '#123');
  testurl('https://username@example.org:77/abc/#123', 'https', 'username', '', 'example.org', '77', '/abc/',  '', '#123');
  testurl('https://username@example.org:77/abc?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://username@example.org:77/abc/?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://username@example.org:77/abc?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org:77/abc/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@example.org:77/abc/def', 'https', 'username', '', 'example.org', '77', '/abc/def', '', '');
  testurl('https://username@example.org:77/abc/def/', 'https', 'username', '', 'example.org', '77', '/abc/def/', '', '');
  testurl('https://username@example.org:77/abc/def?foobar', 'https', 'username', '', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('https://username@example.org:77/abc/def/?foobar', 'https', 'username', '', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('https://username@example.org:77/abc/def?foobar#123', 'https', 'username', '', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('https://username@example.org:77/abc/def/?foobar#123', 'https', 'username', '', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://username@example.org:77/abc/def#123', 'https', 'username', '', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('https://username@example.org:77/abc/def/#123', 'https', 'username', '', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('https://username@example.org:77/abc/def?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username@example.org:77/abc/def/?foobar#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username@example.org:77/abc/def?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org:77/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');


  testurl('https://username@example.org', 'https', 'username', '', 'example.org', '', '', '', '');
  testurl('https://username@example.org/', 'https', 'username', '', 'example.org', '', '/', '', '');
  testurl('https://username@example.org?foobar', 'https', 'username', '', 'example.org', '', '', '?foobar', '');
  testurl('https://username@example.org/?foobar', 'https', 'username', '', 'example.org', '', '/', '?foobar', '');
  testurl('https://username@example.org?foobar#123', 'https', 'username', '', 'example.org', '', '', '?foobar', '#123');
  testurl('https://username@example.org/?foobar#123', 'https', 'username', '', 'example.org', '', '/', '?foobar', '#123');
  testurl('https://username@example.org#123', 'https', 'username', '', 'example.org', '', '', '', '#123');
  testurl('https://username@example.org/#123', 'https', 'username', '', 'example.org', '', '/', '', '#123');
  testurl('https://username@example.org?foobar#123/abc', 'https', 'username', '', 'example.org', '', '', '?foobar', '#123/abc');
  testurl('https://username@example.org/?foobar#123/abc', 'https', 'username', '', 'example.org', '', '/', '?foobar', '#123/abc');
  testurl('https://username@example.org?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@example.org/?foobar/asasa/has#123/abc', 'https', 'username', '', 'example.org', '', '/', '?foobar/asasa/has', '#123/abc');




  testurl('https://username:password@example.org/abc', 'https', 'username', 'password', 'example.org', '', '/abc', '', '');
  testurl('https://username:password@example.org/abc/', 'https', 'username', 'password', 'example.org', '', '/abc/', '', '');
  testurl('https://username:password@example.org/abc?foobar', 'https', 'username', 'password', 'example.org', '', '/abc',  '?foobar', '');
  testurl('https://username:password@example.org/abc/?foobar', 'https', 'username', 'password', 'example.org', '', '/abc/', '?foobar', '');
  testurl('https://username:password@example.org/abc?foobar#123', 'https', 'username', 'password', 'example.org', '', '/abc', '?foobar', '#123');
  testurl('https://username:password@example.org/abc/?foobar#123', 'https', 'username', 'password', 'example.org', '', '/abc/',  '?foobar', '#123');
  testurl('https://username:password@example.org/abc#123', 'https', 'username', 'password', 'example.org', '', '/abc', '',  '#123');
  testurl('https://username:password@example.org/abc/#123', 'https', 'username', 'password', 'example.org', '', '/abc/',  '', '#123');
  testurl('https://username:password@example.org/abc?foobar#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc', '?foobar', '#123/abc');
  testurl('https://username:password@example.org/abc/?foobar#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://username:password@example.org/abc?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@example.org/abc/?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@example.org/abc/def', 'https', 'username', 'password', 'example.org', '', '/abc/def', '', '');
  testurl('https://username:password@example.org/abc/def/', 'https', 'username', 'password', 'example.org', '', '/abc/def/', '', '');
  testurl('https://username:password@example.org/abc/def?foobar', 'https', 'username', 'password', 'example.org', '', '/abc/def',  '?foobar', '');
  testurl('https://username:password@example.org/abc/def/?foobar', 'https', 'username', 'password', 'example.org', '', '/abc/def/', '?foobar', '');
  testurl('https://username:password@example.org/abc/def?foobar#123', 'https', 'username', 'password', 'example.org', '', '/abc/def', '?foobar', '#123');
  testurl('https://username:password@example.org/abc/def/?foobar#123', 'https', 'username', 'password', 'example.org', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://username:password@example.org/abc/def#123', 'https', 'username', 'password', 'example.org', '', '/abc/def', '',  '#123');
  testurl('https://username:password@example.org/abc/def/#123', 'https', 'username', 'password', 'example.org', '', '/abc/def/',  '', '#123');
  testurl('https://username:password@example.org/abc/def?foobar#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username:password@example.org/abc/def/?foobar#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username:password@example.org/abc/def?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@example.org/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@example.org:77', 'https', 'username', 'password', 'example.org', '77', '', '', '');
  testurl('https://username:password@example.org:77/', 'https', 'username', 'password', 'example.org', '77', '/', '', '');
  testurl('https://username:password@example.org:77?foobar', 'https', 'username', 'password', 'example.org', '77', '', '?foobar', '');
  testurl('https://username:password@example.org:77/?foobar', 'https', 'username', 'password', 'example.org', '77', '/', '?foobar', '');
  testurl('https://username:password@example.org:77?foobar#123', 'https', 'username', 'password', 'example.org', '77', '', '?foobar', '#123');
  testurl('https://username:password@example.org:77/?foobar#123', 'https', 'username', 'password', 'example.org', '77', '/', '?foobar', '#123');
  testurl('https://username:password@example.org:77#123', 'https', 'username', 'password', 'example.org', '77', '', '', '#123');
  testurl('https://username:password@example.org:77/#123', 'https', 'username', 'password', 'example.org', '77', '/', '', '#123');
  testurl('https://username:password@example.org:77?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77/?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '/', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@example.org:77/?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@example.org:77/abc', 'https', 'username', 'password', 'example.org', '77', '/abc', '', '');
  testurl('https://username:password@example.org:77/abc/', 'https', 'username', 'password', 'example.org', '77', '/abc/', '', '');
  testurl('https://username:password@example.org:77/abc?foobar', 'https', 'username', 'password', 'example.org', '77', '/abc',  '?foobar', '');
  testurl('https://username:password@example.org:77/abc/?foobar', 'https', 'username', 'password', 'example.org', '77', '/abc/', '?foobar', '');
  testurl('https://username:password@example.org:77/abc?foobar#123', 'https', 'username', 'password', 'example.org', '77', '/abc', '?foobar', '#123');
  testurl('https://username:password@example.org:77/abc/?foobar#123', 'https', 'username', 'password', 'example.org', '77', '/abc/',  '?foobar', '#123');
  testurl('https://username:password@example.org:77/abc#123', 'https', 'username', 'password', 'example.org', '77', '/abc', '',  '#123');
  testurl('https://username:password@example.org:77/abc/#123', 'https', 'username', 'password', 'example.org', '77', '/abc/',  '', '#123');
  testurl('https://username:password@example.org:77/abc?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77/abc/?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77/abc?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@example.org:77/abc/?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@example.org:77/abc/def', 'https', 'username', 'password', 'example.org', '77', '/abc/def', '', '');
  testurl('https://username:password@example.org:77/abc/def/', 'https', 'username', 'password', 'example.org', '77', '/abc/def/', '', '');
  testurl('https://username:password@example.org:77/abc/def?foobar', 'https', 'username', 'password', 'example.org', '77', '/abc/def',  '?foobar', '');
  testurl('https://username:password@example.org:77/abc/def/?foobar', 'https', 'username', 'password', 'example.org', '77', '/abc/def/', '?foobar', '');
  testurl('https://username:password@example.org:77/abc/def?foobar#123', 'https', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar', '#123');
  testurl('https://username:password@example.org:77/abc/def/?foobar#123', 'https', 'username', 'password', 'example.org', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://username:password@example.org:77/abc/def#123', 'https', 'username', 'password', 'example.org', '77', '/abc/def', '',  '#123');
  testurl('https://username:password@example.org:77/abc/def/#123', 'https', 'username', 'password', 'example.org', '77', '/abc/def/',  '', '#123');
  testurl('https://username:password@example.org:77/abc/def?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77/abc/def/?foobar#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username:password@example.org:77/abc/def?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@example.org:77/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', 'password', 'example.org', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');















  testurl('https://[::1]', 'https', '', '', '::1', '', '', '', '');
  testurl('https://[::1]/', 'https', '', '', '::1', '', '/', '', '');
  testurl('https://[::1]?foobar', 'https', '', '', '::1', '', '', '?foobar', '');
  testurl('https://[::1]/?foobar', 'https', '', '', '::1', '', '/', '?foobar', '');
  testurl('https://[::1]?foobar#123', 'https', '', '', '::1', '', '', '?foobar', '#123');
  testurl('https://[::1]/?foobar#123', 'https', '', '', '::1', '', '/', '?foobar', '#123');
  testurl('https://[::1]#123', 'https', '', '', '::1', '', '', '', '#123');
  testurl('https://[::1]/#123', 'https', '', '', '::1', '', '/', '', '#123');
  testurl('https://[::1]?foobar#123/abc', 'https', '', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('https://[::1]/?foobar#123/abc', 'https', '', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('https://[::1]?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://[::1]/abc', 'https', '', '', '::1', '', '/abc', '', '');
  testurl('https://[::1]/abc/', 'https', '', '', '::1', '', '/abc/', '', '');
  testurl('https://[::1]/abc?foobar', 'https', '', '', '::1', '', '/abc',  '?foobar', '');
  testurl('https://[::1]/abc/?foobar', 'https', '', '', '::1', '', '/abc/', '?foobar', '');
  testurl('https://[::1]/abc?foobar#123', 'https', '', '', '::1', '', '/abc', '?foobar', '#123');
  testurl('https://[::1]/abc/?foobar#123', 'https', '', '', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('https://[::1]/abc#123', 'https', '', '', '::1', '', '/abc', '',  '#123');
  testurl('https://[::1]/abc/#123', 'https', '', '', '::1', '', '/abc/',  '', '#123');
  testurl('https://[::1]/abc?foobar#123/abc', 'https', '', '', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('https://[::1]/abc/?foobar#123/abc', 'https', '', '', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://[::1]/abc?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]/abc/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://[::1]/abc/def', 'https', '', '', '::1', '', '/abc/def', '', '');
  testurl('https://[::1]/abc/def/', 'https', '', '', '::1', '', '/abc/def/', '', '');
  testurl('https://[::1]/abc/def?foobar', 'https', '', '', '::1', '', '/abc/def',  '?foobar', '');
  testurl('https://[::1]/abc/def/?foobar', 'https', '', '', '::1', '', '/abc/def/', '?foobar', '');
  testurl('https://[::1]/abc/def?foobar#123', 'https', '', '', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('https://[::1]/abc/def/?foobar#123', 'https', '', '', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://[::1]/abc/def#123', 'https', '', '', '::1', '', '/abc/def', '',  '#123');
  testurl('https://[::1]/abc/def/#123', 'https', '', '', '::1', '', '/abc/def/',  '', '#123');
  testurl('https://[::1]/abc/def?foobar#123/abc', 'https', '', '', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://[::1]/abc/def/?foobar#123/abc', 'https', '', '', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://[::1]/abc/def?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]/abc/def/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://[::1]:77', 'https', '', '', '::1', '77', '', '', '');
  testurl('https://[::1]:77/', 'https', '', '', '::1', '77', '/', '', '');
  testurl('https://[::1]:77?foobar', 'https', '', '', '::1', '77', '', '?foobar', '');
  testurl('https://[::1]:77/?foobar', 'https', '', '', '::1', '77', '/', '?foobar', '');
  testurl('https://[::1]:77?foobar#123', 'https', '', '', '::1', '77', '', '?foobar', '#123');
  testurl('https://[::1]:77/?foobar#123', 'https', '', '', '::1', '77', '/', '?foobar', '#123');
  testurl('https://[::1]:77#123', 'https', '', '', '::1', '77', '', '', '#123');
  testurl('https://[::1]:77/#123', 'https', '', '', '::1', '77', '/', '', '#123');
  testurl('https://[::1]:77?foobar#123/abc', 'https', '', '', '::1', '77', '', '?foobar', '#123/abc');
  testurl('https://[::1]:77/?foobar#123/abc', 'https', '', '', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('https://[::1]:77?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]:77/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://[::1]:77/abc', 'https', '', '', '::1', '77', '/abc', '', '');
  testurl('https://[::1]:77/abc/', 'https', '', '', '::1', '77', '/abc/', '', '');
  testurl('https://[::1]:77/abc?foobar', 'https', '', '', '::1', '77', '/abc',  '?foobar', '');
  testurl('https://[::1]:77/abc/?foobar', 'https', '', '', '::1', '77', '/abc/', '?foobar', '');
  testurl('https://[::1]:77/abc?foobar#123', 'https', '', '', '::1', '77', '/abc', '?foobar', '#123');
  testurl('https://[::1]:77/abc/?foobar#123', 'https', '', '', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('https://[::1]:77/abc#123', 'https', '', '', '::1', '77', '/abc', '',  '#123');
  testurl('https://[::1]:77/abc/#123', 'https', '', '', '::1', '77', '/abc/',  '', '#123');
  testurl('https://[::1]:77/abc?foobar#123/abc', 'https', '', '', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://[::1]:77/abc/?foobar#123/abc', 'https', '', '', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://[::1]:77/abc?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]:77/abc/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://[::1]:77/abc/def', 'https', '', '', '::1', '77', '/abc/def', '', '');
  testurl('https://[::1]:77/abc/def/', 'https', '', '', '::1', '77', '/abc/def/', '', '');
  testurl('https://[::1]:77/abc/def?foobar', 'https', '', '', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('https://[::1]:77/abc/def/?foobar', 'https', '', '', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('https://[::1]:77/abc/def?foobar#123', 'https', '', '', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('https://[::1]:77/abc/def/?foobar#123', 'https', '', '', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://[::1]:77/abc/def#123', 'https', '', '', '::1', '77', '/abc/def', '',  '#123');
  testurl('https://[::1]:77/abc/def/#123', 'https', '', '', '::1', '77', '/abc/def/',  '', '#123');
  testurl('https://[::1]:77/abc/def?foobar#123/abc', 'https', '', '', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://[::1]:77/abc/def/?foobar#123/abc', 'https', '', '', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://[::1]:77/abc/def?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'https', '', '', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');




  testurl('https://username@[::1]', 'https', 'username', '', '::1', '', '', '', '');
  testurl('https://username@[::1]/', 'https', 'username', '', '::1', '', '/', '', '');
  testurl('https://username@[::1]?foobar', 'https', 'username', '', '::1', '', '', '?foobar', '');
  testurl('https://username@[::1]/?foobar', 'https', 'username', '', '::1', '', '/', '?foobar', '');
  testurl('https://username@[::1]?foobar#123', 'https', 'username', '', '::1', '', '', '?foobar', '#123');
  testurl('https://username@[::1]/?foobar#123', 'https', 'username', '', '::1', '', '/', '?foobar', '#123');
  testurl('https://username@[::1]#123', 'https', 'username', '', '::1', '', '', '', '#123');
  testurl('https://username@[::1]/#123', 'https', 'username', '', '::1', '', '/', '', '#123');
  testurl('https://username@[::1]?foobar#123/abc', 'https', 'username', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('https://username@[::1]/?foobar#123/abc', 'https', 'username', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('https://username@[::1]?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');


  testurl('https://username@[::1]/abc', 'https', 'username', '', '::1', '', '/abc', '', '');
  testurl('https://username@[::1]/abc/', 'https', 'username', '', '::1', '', '/abc/', '', '');
  testurl('https://username@[::1]/abc?foobar', 'https', 'username', '', '::1', '', '/abc',  '?foobar', '');
  testurl('https://username@[::1]/abc/?foobar', 'https', 'username', '', '::1', '', '/abc/', '?foobar', '');
  testurl('https://username@[::1]/abc?foobar#123', 'https', 'username', '', '::1', '', '/abc', '?foobar', '#123');
  testurl('https://username@[::1]/abc/?foobar#123', 'https', 'username', '', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('https://username@[::1]/abc#123', 'https', 'username', '', '::1', '', '/abc', '',  '#123');
  testurl('https://username@[::1]/abc/#123', 'https', 'username', '', '::1', '', '/abc/',  '', '#123');
  testurl('https://username@[::1]/abc?foobar#123/abc', 'https', 'username', '', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('https://username@[::1]/abc/?foobar#123/abc', 'https', 'username', '', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://username@[::1]/abc?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]/abc/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@[::1]/abc/def', 'https', 'username', '', '::1', '', '/abc/def', '', '');
  testurl('https://username@[::1]/abc/def/', 'https', 'username', '', '::1', '', '/abc/def/', '', '');
  testurl('https://username@[::1]/abc/def?foobar', 'https', 'username', '', '::1', '', '/abc/def',  '?foobar', '');
  testurl('https://username@[::1]/abc/def/?foobar', 'https', 'username', '', '::1', '', '/abc/def/', '?foobar', '');
  testurl('https://username@[::1]/abc/def?foobar#123', 'https', 'username', '', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('https://username@[::1]/abc/def/?foobar#123', 'https', 'username', '', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://username@[::1]/abc/def#123', 'https', 'username', '', '::1', '', '/abc/def', '',  '#123');
  testurl('https://username@[::1]/abc/def/#123', 'https', 'username', '', '::1', '', '/abc/def/',  '', '#123');
  testurl('https://username@[::1]/abc/def?foobar#123/abc', 'https', 'username', '', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username@[::1]/abc/def/?foobar#123/abc', 'https', 'username', '', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username@[::1]/abc/def?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@[::1]:77', 'https', 'username', '', '::1', '77', '', '', '');
  testurl('https://username@[::1]:77/', 'https', 'username', '', '::1', '77', '/', '', '');
  testurl('https://username@[::1]:77?foobar', 'https', 'username', '', '::1', '77', '', '?foobar', '');
  testurl('https://username@[::1]:77/?foobar', 'https', 'username', '', '::1', '77', '/', '?foobar', '');
  testurl('https://username@[::1]:77?foobar#123', 'https', 'username', '', '::1', '77', '', '?foobar', '#123');
  testurl('https://username@[::1]:77/?foobar#123', 'https', 'username', '', '::1', '77', '/', '?foobar', '#123');
  testurl('https://username@[::1]:77#123', 'https', 'username', '', '::1', '77', '', '', '#123');
  testurl('https://username@[::1]:77/#123', 'https', 'username', '', '::1', '77', '/', '', '#123');
  testurl('https://username@[::1]:77?foobar#123/abc', 'https', 'username', '', '::1', '77', '', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77/?foobar#123/abc', 'https', 'username', '', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]:77/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://username@[::1]:77/abc', 'https', 'username', '', '::1', '77', '/abc', '', '');
  testurl('https://username@[::1]:77/abc/', 'https', 'username', '', '::1', '77', '/abc/', '', '');
  testurl('https://username@[::1]:77/abc?foobar', 'https', 'username', '', '::1', '77', '/abc',  '?foobar', '');
  testurl('https://username@[::1]:77/abc/?foobar', 'https', 'username', '', '::1', '77', '/abc/', '?foobar', '');
  testurl('https://username@[::1]:77/abc?foobar#123', 'https', 'username', '', '::1', '77', '/abc', '?foobar', '#123');
  testurl('https://username@[::1]:77/abc/?foobar#123', 'https', 'username', '', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('https://username@[::1]:77/abc#123', 'https', 'username', '', '::1', '77', '/abc', '',  '#123');
  testurl('https://username@[::1]:77/abc/#123', 'https', 'username', '', '::1', '77', '/abc/',  '', '#123');
  testurl('https://username@[::1]:77/abc?foobar#123/abc', 'https', 'username', '', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77/abc/?foobar#123/abc', 'https', 'username', '', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77/abc?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]:77/abc/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username@[::1]:77/abc/def', 'https', 'username', '', '::1', '77', '/abc/def', '', '');
  testurl('https://username@[::1]:77/abc/def/', 'https', 'username', '', '::1', '77', '/abc/def/', '', '');
  testurl('https://username@[::1]:77/abc/def?foobar', 'https', 'username', '', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('https://username@[::1]:77/abc/def/?foobar', 'https', 'username', '', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('https://username@[::1]:77/abc/def?foobar#123', 'https', 'username', '', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('https://username@[::1]:77/abc/def/?foobar#123', 'https', 'username', '', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://username@[::1]:77/abc/def#123', 'https', 'username', '', '::1', '77', '/abc/def', '',  '#123');
  testurl('https://username@[::1]:77/abc/def/#123', 'https', 'username', '', '::1', '77', '/abc/def/',  '', '#123');
  testurl('https://username@[::1]:77/abc/def?foobar#123/abc', 'https', 'username', '', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77/abc/def/?foobar#123/abc', 'https', 'username', '', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username@[::1]:77/abc/def?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');


  testurl('https://username@[::1]', 'https', 'username', '', '::1', '', '', '', '');
  testurl('https://username@[::1]/', 'https', 'username', '', '::1', '', '/', '', '');
  testurl('https://username@[::1]?foobar', 'https', 'username', '', '::1', '', '', '?foobar', '');
  testurl('https://username@[::1]/?foobar', 'https', 'username', '', '::1', '', '/', '?foobar', '');
  testurl('https://username@[::1]?foobar#123', 'https', 'username', '', '::1', '', '', '?foobar', '#123');
  testurl('https://username@[::1]/?foobar#123', 'https', 'username', '', '::1', '', '/', '?foobar', '#123');
  testurl('https://username@[::1]#123', 'https', 'username', '', '::1', '', '', '', '#123');
  testurl('https://username@[::1]/#123', 'https', 'username', '', '::1', '', '/', '', '#123');
  testurl('https://username@[::1]?foobar#123/abc', 'https', 'username', '', '::1', '', '', '?foobar', '#123/abc');
  testurl('https://username@[::1]/?foobar#123/abc', 'https', 'username', '', '::1', '', '/', '?foobar', '#123/abc');
  testurl('https://username@[::1]?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username@[::1]/?foobar/asasa/has#123/abc', 'https', 'username', '', '::1', '', '/', '?foobar/asasa/has', '#123/abc');




  testurl('https://username:password@[::1]/abc', 'https', 'username', 'password', '::1', '', '/abc', '', '');
  testurl('https://username:password@[::1]/abc/', 'https', 'username', 'password', '::1', '', '/abc/', '', '');
  testurl('https://username:password@[::1]/abc?foobar', 'https', 'username', 'password', '::1', '', '/abc',  '?foobar', '');
  testurl('https://username:password@[::1]/abc/?foobar', 'https', 'username', 'password', '::1', '', '/abc/', '?foobar', '');
  testurl('https://username:password@[::1]/abc?foobar#123', 'https', 'username', 'password', '::1', '', '/abc', '?foobar', '#123');
  testurl('https://username:password@[::1]/abc/?foobar#123', 'https', 'username', 'password', '::1', '', '/abc/',  '?foobar', '#123');
  testurl('https://username:password@[::1]/abc#123', 'https', 'username', 'password', '::1', '', '/abc', '',  '#123');
  testurl('https://username:password@[::1]/abc/#123', 'https', 'username', 'password', '::1', '', '/abc/',  '', '#123');
  testurl('https://username:password@[::1]/abc?foobar#123/abc', 'https', 'username', 'password', '::1', '', '/abc', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]/abc/?foobar#123/abc', 'https', 'username', 'password', '::1', '', '/abc/', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]/abc?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@[::1]/abc/?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@[::1]/abc/def', 'https', 'username', 'password', '::1', '', '/abc/def', '', '');
  testurl('https://username:password@[::1]/abc/def/', 'https', 'username', 'password', '::1', '', '/abc/def/', '', '');
  testurl('https://username:password@[::1]/abc/def?foobar', 'https', 'username', 'password', '::1', '', '/abc/def',  '?foobar', '');
  testurl('https://username:password@[::1]/abc/def/?foobar', 'https', 'username', 'password', '::1', '', '/abc/def/', '?foobar', '');
  testurl('https://username:password@[::1]/abc/def?foobar#123', 'https', 'username', 'password', '::1', '', '/abc/def', '?foobar', '#123');
  testurl('https://username:password@[::1]/abc/def/?foobar#123', 'https', 'username', 'password', '::1', '', '/abc/def/',  '?foobar', '#123');
  testurl('https://username:password@[::1]/abc/def#123', 'https', 'username', 'password', '::1', '', '/abc/def', '',  '#123');
  testurl('https://username:password@[::1]/abc/def/#123', 'https', 'username', 'password', '::1', '', '/abc/def/',  '', '#123');
  testurl('https://username:password@[::1]/abc/def?foobar#123/abc', 'https', 'username', 'password', '::1', '', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]/abc/def/?foobar#123/abc', 'https', 'username', 'password', '::1', '', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]/abc/def?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@[::1]/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '', '/abc/def/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@[::1]:77', 'https', 'username', 'password', '::1', '77', '', '', '');
  testurl('https://username:password@[::1]:77/', 'https', 'username', 'password', '::1', '77', '/', '', '');
  testurl('https://username:password@[::1]:77?foobar', 'https', 'username', 'password', '::1', '77', '', '?foobar', '');
  testurl('https://username:password@[::1]:77/?foobar', 'https', 'username', 'password', '::1', '77', '/', '?foobar', '');
  testurl('https://username:password@[::1]:77?foobar#123', 'https', 'username', 'password', '::1', '77', '', '?foobar', '#123');
  testurl('https://username:password@[::1]:77/?foobar#123', 'https', 'username', 'password', '::1', '77', '/', '?foobar', '#123');
  testurl('https://username:password@[::1]:77#123', 'https', 'username', 'password', '::1', '77', '', '', '#123');
  testurl('https://username:password@[::1]:77/#123', 'https', 'username', 'password', '::1', '77', '/', '', '#123');
  testurl('https://username:password@[::1]:77?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77/?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '/', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@[::1]:77/?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '/', '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@[::1]:77/abc', 'https', 'username', 'password', '::1', '77', '/abc', '', '');
  testurl('https://username:password@[::1]:77/abc/', 'https', 'username', 'password', '::1', '77', '/abc/', '', '');
  testurl('https://username:password@[::1]:77/abc?foobar', 'https', 'username', 'password', '::1', '77', '/abc',  '?foobar', '');
  testurl('https://username:password@[::1]:77/abc/?foobar', 'https', 'username', 'password', '::1', '77', '/abc/', '?foobar', '');
  testurl('https://username:password@[::1]:77/abc?foobar#123', 'https', 'username', 'password', '::1', '77', '/abc', '?foobar', '#123');
  testurl('https://username:password@[::1]:77/abc/?foobar#123', 'https', 'username', 'password', '::1', '77', '/abc/',  '?foobar', '#123');
  testurl('https://username:password@[::1]:77/abc#123', 'https', 'username', 'password', '::1', '77', '/abc', '',  '#123');
  testurl('https://username:password@[::1]:77/abc/#123', 'https', 'username', 'password', '::1', '77', '/abc/',  '', '#123');
  testurl('https://username:password@[::1]:77/abc?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '/abc', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77/abc/?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77/abc?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '/abc', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@[::1]:77/abc/?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/',  '?foobar/asasa/has', '#123/abc');

  testurl('https://username:password@[::1]:77/abc/def', 'https', 'username', 'password', '::1', '77', '/abc/def', '', '');
  testurl('https://username:password@[::1]:77/abc/def/', 'https', 'username', 'password', '::1', '77', '/abc/def/', '', '');
  testurl('https://username:password@[::1]:77/abc/def?foobar', 'https', 'username', 'password', '::1', '77', '/abc/def',  '?foobar', '');
  testurl('https://username:password@[::1]:77/abc/def/?foobar', 'https', 'username', 'password', '::1', '77', '/abc/def/', '?foobar', '');
  testurl('https://username:password@[::1]:77/abc/def?foobar#123', 'https', 'username', 'password', '::1', '77', '/abc/def', '?foobar', '#123');
  testurl('https://username:password@[::1]:77/abc/def/?foobar#123', 'https', 'username', 'password', '::1', '77', '/abc/def/',  '?foobar', '#123');
  testurl('https://username:password@[::1]:77/abc/def#123', 'https', 'username', 'password', '::1', '77', '/abc/def', '',  '#123');
  testurl('https://username:password@[::1]:77/abc/def/#123', 'https', 'username', 'password', '::1', '77', '/abc/def/',  '', '#123');
  testurl('https://username:password@[::1]:77/abc/def?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/def', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77/abc/def/?foobar#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/def/', '?foobar', '#123/abc');
  testurl('https://username:password@[::1]:77/abc/def?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/def', '?foobar/asasa/has', '#123/abc');
  testurl('https://username:password@[::1]:77/abc/def/?foobar/asasa/has#123/abc', 'https', 'username', 'password', '::1', '77', '/abc/def/',  '?foobar/asasa/has', '#123/abc');


  testurl('https://ssl.muenchen.de/aDISWeb/app?service=direct/0/Home/$DirectLink&sp=SOPAC', 'https', '', '', 'ssl.muenchen.de', '', '/aDISWeb/app', '?service=direct/0/Home/$DirectLink&sp=SOPAC', '');
  testurl('https://ssl.muenchen.de//////aDISWeb///////app?service=direct//0///Home////$DirectLink&sp=SOPAC', 'https', '', '', 'ssl.muenchen.de', '', '/aDISWeb/app', '?service=direct//0///Home////$DirectLink&sp=SOPAC', '', 'https://ssl.muenchen.de/aDISWeb/app?service=direct//0///Home////$DirectLink&sp=SOPAC');


  test(decodeURL('http://example.org').resolved('http://foo.bar').combined(), 'http://foo.bar');
  test(decodeURL('http://example.org').resolved('https://foo.bar/xyz').combined(), 'https://foo.bar/xyz');
  test(decodeURL('http://example.org').resolved('redirect.php?target=http://whatwg.com/abc').combined(), 'http://example.org/redirect.php?target=http://whatwg.com/abc');
  test(decodeURL('http://example.org').resolved('/redirect.php?target=http://whatwg.com/abc').combined(), 'http://example.org/redirect.php?target=http://whatwg.com/abc');

  testcookies;
end;

end.
