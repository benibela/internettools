unit htmlInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser;

function htmlElementIsImplicitCDATA(const marker: pchar; const tempLen: integer): boolean;
function htmlElementIsImplicitCDATA(const name: string): boolean;
function htmlElementIsChildless(const s: string): boolean;
function htmlElementIsPhrasing(n: TTreeNode): boolean;
function htmlElementIsExpectedEmpty(n: TTreeNode; isHTML5: boolean): boolean;
function htmlElementIsExpectedEmptyHTML4(n: TTreeNode): boolean;
function htmlElementIsExpectedVoidHTML5(n: TTreeNode): boolean;
function htmlElementIsHead(n: TTreeNode): boolean;
function htmlElementIsMetaContentType(n: TTreeNode): boolean;
function htmlElementIsFormattedWhitespace(const s: string): boolean;
function htmlAttributeIsURI(n: TTreeAttribute): boolean;

type HTMLNodeNameHashs = object
  const a = $820103F0;
  const img = $4FACAFC2;
  const br = $2CF50F7A;
  const td = $A664EDFC;
  const tr = $B93B93AD;
  const th = $1483CA3C;
  const table = $57CFB523;
  const thead = $2D298F5C;
  const tbody = $BB316BB0;
  const p = $B7656EB4;

  //invisible
  const area = $B61A9737;
  const base = $36BAA821;
  const basefont = $C997A27A;
  const datalist = $41BB801A;
  const head = $FB1A74A6;
  const link = $21E329D3;
  const meta = $53F6A414;
  const noembed = $35DC71D8;
  const noframes = $8EF9275D;
  const param = $EA036F5E;
  const rp = $065D2F8B;
  const script = $75469FD3;
  const source = $B04BAA1E;
  const style = $244E4D3D;
  const template = $08F14C20;
  const track = $AB8D6A26;
  const title = $FE8D4719;
end;

implementation

uses bbutils;

function htmlElementIsImplicitCDATA(const marker: pchar; const tempLen: integer): boolean;
begin
  //    If the parent of current node is a style, script, xmp, iframe, noembed, noframes, or plaintext element, or if the parent of current node is noscript element
  result := strliequal(marker,'style',tempLen) or strliequal(marker,'script',tempLen)
            or strliequal(marker,'xmp',tempLen) or strliequal(marker,'iframe',tempLen)
            or strliequal(marker,'noembed',tempLen) or strliequal(marker,'noframes',tempLen)
            or strliequal(marker,'plaintext',tempLen);
end;

function htmlElementIsImplicitCDATA(const name: string): boolean;
begin
  result := htmlElementIsImplicitCDATA(pchar(name), length(name));
end;

function htmlElementIsChildless(const s: string): boolean;
begin
  //elements that should/must not have children
  //area, base, basefont, bgsound, br, col, command, embed, frame, hr, img, input, keygen, link, meta, param, source, track or wbr
  if s = '' then exit(false);
  case s[1] of
    'a', 'A': result := striequal(s,'area');
    'b', 'B': result := striequal(s,'base') or striequal(s,'basefont') or striequal(s,'bgsound') or striequal(s,'br') ;
    'c', 'C': result := striequal(s,'col') or striequal(s,'command');
    'e', 'E': result := striequal(s,'embed');
    'f', 'F': result := striequal(s,'frame');
    'h', 'H': result := striequal(s,'hr') ;
    'i', 'I': result := striequal(s,'img') or striequal(s,'input') or striequal(s,'isindex');
    'k', 'K': result := striequal(s,'keygen') ;
    'l', 'L': result := striequal(s,'link') ;
    'm', 'M': result := striequal(s,'meta') ;
    'p', 'P': result := striequal(s,'param') ;
    's', 'S': result := striequal(s,'source') ;
    't', 'T': result := striequal(s,'track');
    'w', 'W': result := striequal(s,'wbr');
    else result := false;
  end;
  //elements listed above, not being void are probably (??) deprecated?
  //void elements: area, base, br, col, command, embed, hr, img, input, keygen, link, meta, param, source, track, wbr

end;


function htmlElementIsPhrasing(n: TTreeNode): boolean;
begin
  case lowercase(n.value) of
    'del', 'ins': if not n.hasChildren() then exit(true);
    'a': exit(true);
    'abbr': exit(true);
    'area': exit(true); //only if a descendant of a map element
    'audio': exit(true);
    'b': exit(true);
    'bdi': exit(true);
    'bdo': exit(true);
    'br': exit(true);
    'button': exit(true);
    'canvas': exit(true);
    'cite': exit(true);
    'code': exit(true);
    'data': exit(true);
    'datalist': exit(true);
//      'del': exit(true);
    'dfn': exit(true);
    'em': exit(true);
    'embed': exit(true);
    'i': exit(true);
    'iframe': exit(true);
    'img': exit(true);
    'input': exit(true);
//      'ins': exit(true);
    'kbd': exit(true);
    'label': exit(true);
    'link': if n.getFirstChild() <> nil then exit(true);
    'map': exit(true);
    'mark': exit(true);
    'meta': exit(n.hasAttribute('itemprop'));
    'meter': exit(true);
    'noscript': exit(true);
    'object': exit(true);
    'output': exit(true);
    'picture': exit(true);
    'progress': exit(true);
    'q': exit(true);
    'ruby': exit(true);
    's': exit(true);
    'samp': exit(true);
    'script': exit(true);
    'select': exit(true);
    'slot': exit(true);
    'small': exit(true);
    'span': exit(true);
    'strong': exit(true);
    'sub': exit(true);
    'sup': exit(true);
    'SVG svg': exit(true);
    'template': exit(true);
    'textarea': exit(true);
    'time': exit(true);
    'u': exit(true);
    'var': exit(true);
    'video': exit(true);
    'wbr': exit(true);
//      'autonomous custom elements': exit(true);
//      'text': exit(true);
  end;
  exit(false);
end;


function htmlElementIsExpectedEmpty(n: TTreeNode; isHTML5: boolean): boolean;
begin
  case lowercase(n.value) of
    'area': exit(true);
    'base': exit(true);
    'br': exit(true);
    'col': exit(true);
    'embed': exit(true);
    'hr': exit(true);
    'img': exit(true);
    'input': exit(true);
    'link': exit(true);
    'meta': exit(true);
    'param': exit(true);

    'isindex': exit(not isHTML5);
    'basefont': exit(not isHTML5);
    'frame': exit(not isHTML5);

    'keygen': exit(isHTML5);
    'source': exit(isHTML5);
    'track': exit(isHTML5);
    'wbr': exit(isHTML5);
  end;
  result := false;
end;

function htmlElementIsExpectedEmptyHTML4(n: TTreeNode): boolean;
begin
  result := htmlElementIsExpectedEmpty(n, false);
end;

function htmlElementIsExpectedVoidHTML5(n: TTreeNode): boolean;
begin
  result := htmlElementIsExpectedEmpty(n, true);
end;


function htmlElementIsHead(n: TTreeNode): boolean;
begin
  result := striEqual(n.value, 'head');
end;

function htmlElementIsMetaContentType(n: TTreeNode): boolean;
begin
  result := striEqual(n.value, 'meta') and striEqual(n.getAttribute('http-equiv'), 'content-type');
end;

function htmlElementIsFormattedWhitespace(const s: string): boolean;
begin
  case lowercase(s) of
    'pre', 'script', 'style', 'title', 'textarea': exit(true);
  end;
  result := false;
end;

function htmlAttributeIsURI(n: TTreeAttribute): boolean;
begin
  if n.namespace = nil then
    case lowercase(n.value) of
      'action': case lowercase(n.getParent().value) of
        'form': exit(true);
                end;
      'archive': case lowercase(n.getParent().value) of
        'object': exit(true);
                end;
      'background': case lowercase(n.getParent().value) of
        'body': exit(true);
                end;
      'cite': case lowercase(n.getParent().value) of
        'blockquote', 'del', 'ins', 'q': exit(true);
                end;
      'classid': case lowercase(n.getParent().value) of
        'object': exit(true);
                end;
      'codebase': case lowercase(n.getParent().value) of
        'applet', 'object': exit(true);
                end;
      'data': case lowercase(n.getParent().value) of
        'object': exit(true);
                end;
      'datasrc': case lowercase(n.getParent().value) of
        'button', 'div', 'input', 'object', 'select', 'span', 'table', 'textarea': exit(true);
                end;
      'for': case lowercase(n.getParent().value) of
        'script': exit(true);
                end;
      'formaction': case lowercase(n.getParent().value) of
        'button', 'input': exit(true);
                end;
      'href': case lowercase(n.getParent().value) of
        'a', 'area', 'base', 'link': exit(true);
                end;
      'icon': case lowercase(n.getParent().value) of
        'command': exit(true);
                end;
      'longdesc': case lowercase(n.getParent().value) of
        'frame', 'iframe', 'img': exit(true);
                end;
      'manifest': case lowercase(n.getParent().value) of
        'html': exit(true);
                end;
      'name': case lowercase(n.getParent().value) of
        'a': exit(true);
                end;
      'poster': case lowercase(n.getParent().value) of
        'video': exit(true);
                end;
      'profile': case lowercase(n.getParent().value) of
        'head': exit(true);
                end;
      'src': case lowercase(n.getParent().value) of
        'audio', 'embed', 'frame', 'iframe', 'img', 'input', 'script', 'source', 'track', 'video': exit(true);
                end;
      'usemap': case lowercase(n.getParent().value) of
        'img', 'input', 'object': exit(true);
                end;
{        'value': case lowercase(n.getParent().value) of
        'input': exit(true);
               end;}
    end;
  result := false;
end;

end.

