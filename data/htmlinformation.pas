unit htmlInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser;

function htmlElementIsImplicitCDATA(const marker: pchar; const tempLen: integer): boolean; inline;
function htmlElementIsImplicitCDATA(hash: cardinal; const marker: pchar; const tempLen: integer): boolean;
//function htmlElementIsImplicitCDATA(const name: string): boolean;
function htmlElementIsImplicitCDATA(hash: cardinal; const name: string): boolean;
function htmlElementIsChildless(hash: cardinal; const s: string): boolean;
function htmlElementIsPhrasing(n: TTreeNode): boolean;
function htmlElementIsExpectedEmpty(n: TTreeNode; isHTML5: boolean): boolean;
function htmlElementIsExpectedEmptyHTML4(n: TTreeNode): boolean;
function htmlElementIsExpectedVoidHTML5(n: TTreeNode): boolean;
function htmlElementIsHead(n: TTreeNode): boolean;
function htmlElementIsMetaContentType(n: TTreeNode): boolean;
function htmlElementIsFormattedWhitespace(n: TTreeNode): boolean;
function htmlAttributeIsURI(n: TTreeAttribute): boolean;

type HTMLNodeNameHashs = object
  const a = $820103F0;
  const abbr = $4F9E14C1;
  const action = $80934BDB;
  const applet = $05E2ACFE;
  const archive = $4B622EB4;
  const area = $B61A9737;
  const audio = $56F37910;
  const b = $B7346E56;
  const background = $1BFC86A9;
  const base = $36BAA821;
  const basefont = $C997A27A;
  const bdi = $0DF6B456;
  const bdo = $3C861178;
  const bgsound = $9BA52A55;
  const blockquote = $E017206C;
  const body = $815CBD1A;
  const br = $2CF50F7A;
  const button = $CA7C1BA4;
  const canvas = $16B3637C;
  const cite = $87DC46C0;
  const classid = $AD3010FA;
  const code = $0EA6D335;
  const codebase = $B59E7ECC;
  const col = $C52DFF23;
  const command = $F9FE87AB;
  const data = $A77F762D;
  const datalist = $41BB801A;
  const datasrc = $8D44B614;
  const del = $1693E292;
  const dfn = $136283CB;
  const &div = $E550A8D4;
  const em = $4FC4D1A1;
  const embed = $D054CDAA;
  const &for = $FCAB20A3;
  const form = $4D706AFE;
  const formaction = $DF8C1BDC;
  const frame = $A45C5C57;
  const head = $FB1A74A6;
  const hr = $B60E9A97;
  const href = $8D043786;
  const html = $AEB82870;
  const i = $137826DC;
  const icon = $B1662169;
  const iframe = $1D7C0931;
  const img = $4FACAFC2;
  const input = $34286C01;
  const ins = $53AE343D;
  const isindex = $F2F96899;
  const itemprop = $7BD8D34A;
  const kbd = $BD8BF137;
  const keygen = $A54D33BB;
  const &label = $CDE39663;
  const link = $21E329D3;
  const longdesc = $62E556AF;
  const manifest = $10754E89;
  const map = $BE0D93C6;
  const mark = $7861CBEE;
  const meta = $53F6A414;
  const meter = $6D5A5336;
  const name = $1DAD61B1;
  const noembed = $35DC71D8;
  const noframes = $8EF9275D;
  const noscript = $997BA521;
  const &object = $FB129865;
  const output = $75DCAEF3;
  const param = $EA036F5E;
  const picture = $AB918FDD;
  const plaintext = $7DA5BF41;
  const poster = $4373E9B6;
  const pre = $781F9978;
  const profile = $F2E597AF;
  const progress = $0D2309F5;
  const q = $A51FCA29;
  const ruby = $747940C8;
  const s = $CA8B9500;
  const samp = $7350DA45;
  const script = $75469FD3;
  const select = $8DCBDF5D;
  const slot = $8E0AD6CD;
  const small = $EC0538D9;
  const source = $B04BAA1E;
  const span = $5BF01007;
  const src = $84D555D8;
  const strong = $6A82CE7B;
  const style = $244E4D3D;
  const sub = $1AA20312;
  const sup = $AA1E2214;
  const table = $57CFB523;
  const template = $08F14C20;
  const textarea = $674075DC;
  const time = $89EC1332;
  const title = $FE8D4719;
  const track = $AB8D6A26;
  const u = $EC3B585F;
  const usemap = $8D879431;
  const value = $86F1F211;
  const &var = $DB6AE6DB;
  const video = $42DA8BB9;
  const wbr = $3BDCB90E;
  const xmp = $66DD983F;

  const td = $A664EDFC;
  const tr = $B93B93AD;
  const th = $1483CA3C;
  const thead = $2D298F5C;
  const tbody = $BB316BB0;
  const p = $B7656EB4;

  const rp = $065D2F8B;
end;

XMLAttributeNameHashs = object
  const space = $219D3C61;
end;

implementation

uses bbutils, xquery.internals.common;

{$ImplicitExceptions off}

function htmlElementIsImplicitCDATA(const marker: pchar; const tempLen: integer): boolean;
begin
  result :=  htmlElementIsImplicitCDATA(nodeNameHash(marker, tempLen), marker, tempLen);
end;

function htmlElementIsImplicitCDATA(hash: cardinal; const marker: pchar; const tempLen: integer): boolean;
begin
  //    If the parent of current node is a style, script, xmp, iframe, noembed, noframes, or plaintext element, or if the parent of current node is noscript element
  case hash of
    HTMLNodeNameHashs.style: result := strliequal(marker,'style',tempLen);
    HTMLNodeNameHashs.script: result := strliequal(marker,'script',tempLen);
    HTMLNodeNameHashs.xmp: result := strliequal(marker,'xmp',tempLen);
    HTMLNodeNameHashs.iframe: result := strliequal(marker,'iframe',tempLen);
    HTMLNodeNameHashs.noembed: result := strliequal(marker,'noembed',tempLen);
    HTMLNodeNameHashs.noframes: result := strliequal(marker,'noframes',tempLen);
    HTMLNodeNameHashs.plaintext: result := strliequal(marker,'plaintext',tempLen);
    else result := false;
  end;
end;

function htmlElementIsImplicitCDATA(hash: cardinal; const name: string): boolean;
begin
  result := htmlElementIsImplicitCDATA(hash, pchar(name), length(name));
end;

function htmlElementIsChildless(hash: cardinal; const s: string): boolean;
begin
  //elements that should/must not have children
  //area, base, basefont, bgsound, br, col, command, embed, frame, hr, img, input, keygen, link, meta, param, source, track or wbr
  result := false;
  case hash of
    HTMLNodeNameHashs.area:  result := striequal(s,'area');
    HTMLNodeNameHashs.base:  result := striequal(s,'base');
    HTMLNodeNameHashs.basefont:  result := striequal(s,'basefont');
    HTMLNodeNameHashs.bgsound:  result := striequal(s,'bgsound');
    HTMLNodeNameHashs.br:  result := striequal(s,'br') ;
    HTMLNodeNameHashs.col:  result := striequal(s,'col');
    HTMLNodeNameHashs.command:  result := striequal(s,'command');
    HTMLNodeNameHashs.embed:  result := striequal(s,'embed');
    HTMLNodeNameHashs.frame:  result := striequal(s,'frame');
    HTMLNodeNameHashs.hr:  result := striequal(s,'hr') ;
    HTMLNodeNameHashs.img:  result := striequal(s,'img');
    HTMLNodeNameHashs.input:  result := striequal(s,'input');
    HTMLNodeNameHashs.isindex:  result := striequal(s,'isindex');
    HTMLNodeNameHashs.keygen:  result := striequal(s,'keygen') ;
    HTMLNodeNameHashs.link:  result := striequal(s,'link') ;
    HTMLNodeNameHashs.meta:  result := striequal(s,'meta') ;
    HTMLNodeNameHashs.param:  result := striequal(s,'param') ;
    HTMLNodeNameHashs.source:  result := striequal(s,'source') ;
    HTMLNodeNameHashs.track:  result := striequal(s,'track');
    HTMLNodeNameHashs.wbr:  result := striequal(s,'wbr');
  end;
  //elements listed above, not being void are probably (??) deprecated?
  //void elements: area, base, br, col, command, embed, hr, img, input, keygen, link, meta, param, source, track, wbr

end;

function htmlElementIsPhrasing(n: TTreeNode): boolean;
begin
  result := false;
  case n.hash of
    HTMLNodeNameHashs.a: result := striequal(n.value,'a');
    HTMLNodeNameHashs.abbr: result := striequal(n.value,'abbr');
    HTMLNodeNameHashs.area: result := striequal(n.value,'area'); //only if a descendant of a map element
    HTMLNodeNameHashs.audio: result := striequal(n.value,'audio');
    HTMLNodeNameHashs.b: result := striequal(n.value,'b');
    HTMLNodeNameHashs.bdi: result := striequal(n.value,'bdi');
    HTMLNodeNameHashs.bdo: result := striequal(n.value,'bdo');
    HTMLNodeNameHashs.br: result := striequal(n.value,'br');
    HTMLNodeNameHashs.button: result := striequal(n.value,'button');
    HTMLNodeNameHashs.canvas: result := striequal(n.value,'canvas');
    HTMLNodeNameHashs.cite: result := striequal(n.value,'cite');
    HTMLNodeNameHashs.code: result := striequal(n.value,'code');
    HTMLNodeNameHashs.data: result := striequal(n.value,'data');
    HTMLNodeNameHashs.datalist: result := striequal(n.value,'datalist');
    HTMLNodeNameHashs.dfn: result := striequal(n.value,'dfn');
    HTMLNodeNameHashs.em: result := striequal(n.value,'em');
    HTMLNodeNameHashs.embed: result := striequal(n.value,'embed');
    HTMLNodeNameHashs.i: result := striequal(n.value,'i');
    HTMLNodeNameHashs.iframe: result := striequal(n.value,'iframe');
    HTMLNodeNameHashs.img: result := striequal(n.value,'img');
    HTMLNodeNameHashs.input: result := striequal(n.value,'input');
    HTMLNodeNameHashs.kbd: result := striequal(n.value,'kbd');
    HTMLNodeNameHashs.&label: result := striequal(n.value,'label');
    HTMLNodeNameHashs.link: if striequal(n.value,'link')  and assigned(n.getFirstChild()) then result := true;
    HTMLNodeNameHashs.map: result := striequal(n.value,'map');
    HTMLNodeNameHashs.mark: result := striequal(n.value,'mark');
    HTMLNodeNameHashs.meta:  result := striequal(n.value,'meta') and n.hasAttribute('itemprop');
    HTMLNodeNameHashs.meter: result := striequal(n.value,'meter');
    HTMLNodeNameHashs.noscript: result := striequal(n.value,'noscript');
    HTMLNodeNameHashs.&object: result := striequal(n.value,'object');
    HTMLNodeNameHashs.output: result := striequal(n.value,'output');
    HTMLNodeNameHashs.picture: result := striequal(n.value,'picture');
    HTMLNodeNameHashs.progress: result := striequal(n.value,'progress');
    HTMLNodeNameHashs.q: result := striequal(n.value,'q');
    HTMLNodeNameHashs.ruby: result := striequal(n.value,'ruby');
    HTMLNodeNameHashs.s: result := striequal(n.value,'s');
    HTMLNodeNameHashs.samp: result := striequal(n.value,'samp');
    HTMLNodeNameHashs.script: result := striequal(n.value,'script');
    HTMLNodeNameHashs.select: result := striequal(n.value,'select');
    HTMLNodeNameHashs.slot: result := striequal(n.value,'slot');
    HTMLNodeNameHashs.small: result := striequal(n.value,'small');
    HTMLNodeNameHashs.span: result := striequal(n.value,'span');
    HTMLNodeNameHashs.strong: result := striequal(n.value,'strong');
    HTMLNodeNameHashs.sub: result := striequal(n.value,'sub');
    HTMLNodeNameHashs.sup: result := striequal(n.value,'sup');
    HTMLNodeNameHashs.template: result := striequal(n.value,'template');
    HTMLNodeNameHashs.textarea: result := striequal(n.value,'textarea');
    HTMLNodeNameHashs.time: result := striequal(n.value,'time');
    HTMLNodeNameHashs.u: result := striequal(n.value,'u');
    HTMLNodeNameHashs.&var: result := striequal(n.value,'var');
    HTMLNodeNameHashs.video: result := striequal(n.value,'video');
    HTMLNodeNameHashs.wbr: result := striequal(n.value,'wbr');
    //      "autonomous custom elements": exit(true);
//      "text": exit(true);
  end;
end;


function htmlElementIsExpectedEmpty(n: TTreeNode; isHTML5: boolean): boolean;
begin
  result := false;
  case n.hash of
    HTMLNodeNameHashs.area: result := striequal(n.value,'area');
    HTMLNodeNameHashs.base: result := striequal(n.value,'base');
    HTMLNodeNameHashs.br: result := striequal(n.value,'br');
    HTMLNodeNameHashs.col: result := striequal(n.value,'col');
    HTMLNodeNameHashs.embed: result := striequal(n.value,'embed');
    HTMLNodeNameHashs.hr: result := striequal(n.value,'hr');
    HTMLNodeNameHashs.img: result := striequal(n.value,'img');
    HTMLNodeNameHashs.input: result := striequal(n.value,'input');
    HTMLNodeNameHashs.link: result := striequal(n.value,'link');
    HTMLNodeNameHashs.meta: result := striequal(n.value,'meta');
    HTMLNodeNameHashs.param: result := striequal(n.value,'param');

    HTMLNodeNameHashs.isindex: result := striequal(n.value,'isindex') and not isHTML5;
    HTMLNodeNameHashs.basefont: result := striequal(n.value,'basefont') and not isHTML5;
    HTMLNodeNameHashs.frame: result := striequal(n.value,'frame') and not isHTML5;

    HTMLNodeNameHashs.keygen: result := isHTML5 and striequal(n.value,'keygen');
    HTMLNodeNameHashs.source: result := isHTML5 and striequal(n.value,'source');
    HTMLNodeNameHashs.track: result := isHTML5 and striequal(n.value,'track');
    HTMLNodeNameHashs.wbr: result := isHTML5 and striequal(n.value,'wbr') ;
  end;
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
  result := (n.hash = HTMLNodeNameHashs.head) and striEqual(n.value, 'head');
end;

function htmlElementIsMetaContentType(n: TTreeNode): boolean;
begin
  result := (n.hash = HTMLNodeNameHashs.meta) and striEqual(n.value, 'meta') and striEqual(n.getAttribute('http-equiv'), 'content-type');
end;

function htmlElementIsFormattedWhitespace(n: TTreeNode): boolean;
begin
  result := false;
  case n.hash of
    HTMLNodeNameHashs.pre: result := striEqual(n.value, 'pre');
    HTMLNodeNameHashs.script: result := striEqual(n.value, 'script');
    HTMLNodeNameHashs.style: result := striEqual(n.value, 'style');
    HTMLNodeNameHashs.title: result := striEqual(n.value, 'title');
    HTMLNodeNameHashs.textarea: result := striEqual(n.value, 'textarea');
  end;
end;

function htmlAttributeIsURI(n: TTreeAttribute): boolean;
begin
  result := false;
  if n.namespace <> nil then exit;

  case n.hash of
    HTMLNodeNameHashs.action: if striequal(n.value, 'action') then case n.getParent().hash  of
      HTMLNodeNameHashs.form: result := striequal(n.getParent().value, 'form');
    end;
    HTMLNodeNameHashs.archive: if striequal(n.value, 'archive') then case n.getParent().hash  of
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
    end;
    HTMLNodeNameHashs.background: if striequal(n.value, 'background') then case n.getParent().hash  of
      HTMLNodeNameHashs.body: result := striequal(n.getParent().value, 'body');
    end;
    HTMLNodeNameHashs.cite: if striequal(n.value, 'cite') then case n.getParent().hash  of
      HTMLNodeNameHashs.blockquote: result := striequal(n.getParent().value, 'blockquote');
      HTMLNodeNameHashs.del: result := striequal(n.getParent().value, 'del');
      HTMLNodeNameHashs.ins: result := striequal(n.getParent().value, 'ins');
      HTMLNodeNameHashs.q: result := striequal(n.getParent().value, 'q');
    end;
    HTMLNodeNameHashs.classid: if striequal(n.value, 'classid') then case n.getParent().hash  of
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
    end;
    HTMLNodeNameHashs.codebase: if striequal(n.value, 'codebase') then case n.getParent().hash  of
      HTMLNodeNameHashs.applet: result := striequal(n.getParent().value, 'applet');
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
    end;
    HTMLNodeNameHashs.data: if striequal(n.value, 'data') then case n.getParent().hash  of
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
    end;
    HTMLNodeNameHashs.datasrc: if striequal(n.value, 'datasrc') then case n.getParent().hash  of
      HTMLNodeNameHashs.button: result := striequal(n.getParent().value, 'button');
      HTMLNodeNameHashs.&div: result := striequal(n.getParent().value, 'div');
      HTMLNodeNameHashs.input: result := striequal(n.getParent().value, 'input');
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
      HTMLNodeNameHashs.select: result := striequal(n.getParent().value, 'select');
      HTMLNodeNameHashs.span: result := striequal(n.getParent().value, 'span');
      HTMLNodeNameHashs.table: result := striequal(n.getParent().value, 'table');
      HTMLNodeNameHashs.textarea: result := striequal(n.getParent().value, 'textarea');
    end;
    HTMLNodeNameHashs.&for: if striequal(n.value, 'for') then case n.getParent().hash  of
      HTMLNodeNameHashs.script: result := striequal(n.getParent().value, 'script');
    end;
    HTMLNodeNameHashs.formaction: if striequal(n.value, 'formaction') then case n.getParent().hash  of
      HTMLNodeNameHashs.button: result := striequal(n.getParent().value, 'button');
      HTMLNodeNameHashs.input: result := striequal(n.getParent().value, 'input');
    end;
    HTMLNodeNameHashs.href: if striequal(n.value, 'href') then case n.getParent().hash  of
      HTMLNodeNameHashs.a: result := striequal(n.getParent().value, 'a');
      HTMLNodeNameHashs.area: result := striequal(n.getParent().value, 'area');
      HTMLNodeNameHashs.base: result := striequal(n.getParent().value, 'base');
      HTMLNodeNameHashs.link: result := striequal(n.getParent().value, 'link');
    end;
    HTMLNodeNameHashs.longdesc: if striequal(n.value, 'longdesc') then case n.getParent().hash  of
      HTMLNodeNameHashs.frame: result := striequal(n.getParent().value, 'frame');
      HTMLNodeNameHashs.iframe: result := striequal(n.getParent().value, 'iframe');
      HTMLNodeNameHashs.img: result := striequal(n.getParent().value, 'img');
    end;
    HTMLNodeNameHashs.manifest: if striequal(n.value, 'manifest') then case n.getParent().hash  of
      HTMLNodeNameHashs.html: result := striequal(n.getParent().value, 'html');
    end;
    HTMLNodeNameHashs.name: if striequal(n.value, 'name') then case n.getParent().hash  of
      HTMLNodeNameHashs.a: result := striequal(n.getParent().value, 'a');
    end;
    HTMLNodeNameHashs.poster: if striequal(n.value, 'poster') then case n.getParent().hash  of
      HTMLNodeNameHashs.video: result := striequal(n.getParent().value, 'video');
    end;
    HTMLNodeNameHashs.profile: if striequal(n.value, 'profile') then case n.getParent().hash  of
      HTMLNodeNameHashs.head: result := striequal(n.getParent().value, 'head');
    end;
    HTMLNodeNameHashs.src: if striequal(n.value, 'src') then case n.getParent().hash  of
      HTMLNodeNameHashs.audio: result := striequal(n.getParent().value, 'audio');
      HTMLNodeNameHashs.embed: result := striequal(n.getParent().value, 'embed');
      HTMLNodeNameHashs.frame: result := striequal(n.getParent().value, 'frame');
      HTMLNodeNameHashs.iframe: result := striequal(n.getParent().value, 'iframe');
      HTMLNodeNameHashs.img: result := striequal(n.getParent().value, 'img');
      HTMLNodeNameHashs.input: result := striequal(n.getParent().value, 'input');
      HTMLNodeNameHashs.script: result := striequal(n.getParent().value, 'script');
      HTMLNodeNameHashs.source: result := striequal(n.getParent().value, 'source');
      HTMLNodeNameHashs.track: result := striequal(n.getParent().value, 'track');
      HTMLNodeNameHashs.video: result := striequal(n.getParent().value, 'video');
    end;
    HTMLNodeNameHashs.usemap: if striequal(n.value, 'usemap') then case n.getParent().hash  of
      HTMLNodeNameHashs.img: result := striequal(n.getParent().value, 'img');
      HTMLNodeNameHashs.input: result := striequal(n.getParent().value, 'input');
      HTMLNodeNameHashs.&object: result := striequal(n.getParent().value, 'object');
    end;
    HTMLNodeNameHashs.value: if striequal(n.value, 'value') then case n.getParent().hash  of
      HTMLNodeNameHashs.input: result := striequal(n.getParent().value, 'input') and striequal(n.getParent().getAttribute('type'), 'url');
    end;
   end;
end;

end.

