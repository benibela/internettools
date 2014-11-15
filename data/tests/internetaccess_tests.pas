unit internetaccess_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetaccess;

procedure unitTests();

implementation

procedure test(condition: boolean; name: string='');
begin
  if not condition then raise Exception.Create('test: '+name);
end;
procedure test(a, b: string; name: string = '');
begin
  if a <> b then
    raise Exception.Create('test: '+name+': '+a+' <> '+b);
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

end;

end.
