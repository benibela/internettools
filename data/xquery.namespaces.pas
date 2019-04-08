unit xquery.namespaces;
{
Copyright (C) 2008 - 2019 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery.internals.common;

type
TXQHashmapStrOwningInterface = specialize TXQHashmapStrOwning<IUnknown, TInterfaceList>;

TNamespace = class;

//** Namespace interface, storing url and prefix. (Interface, so it is ref-counted)
INamespace = interface
['{5F6DF5F2-548C-4F13-9BEA-CE59EBAE4CAB}']
  function getPrefix: string; //**< Returns the prefix
  function getURL: string; //**< Returns the url
  function serialize: string; //**< Returns a xmlns attribute declaring this namespace with url and prefix
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
end;


{ TNamespace }

//** Class implementing the INamespace interface
TNamespace = class(TInterfacedObject, INamespace)
public
  url: string;
  prefix: string;
  //** Creates a new namespace with url and prefix. (watch the argument order. It follows the XPath fn:QName function)
  constructor create(const aurl: string; aprefix: string);

  class function make(const aurl: string; const aprefix: string): TNamespace; static;
  class function uniqueUrl(const aurl: string): string; static;
  class procedure freeCache; static;

  function getPrefix: string;
  function getURL: string;
  function serialize: string;
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
  destructor Destroy; override;
end;

{ TNamespaceList }

//** List of namespaces
TNamespaceList = class(TInterfaceList)
private
  function getNamespace(const prefix: string): INamespace;
  function getNamespace(i: integer): INamespace;
public
  function hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
  function hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
  function hasNamespacePrefix(const prefix: string): boolean;
  function hasNamespace(const n: INamespace): boolean;

  function lastIndexOfNamespacePrefix(const prefix: string): integer;

  procedure add(const ns: TNamespace);
  procedure add(const ns: INamespace);
  procedure addIfNewPrefix(const ns: TNamespace);
  procedure addIfNewPrefix(const ns: INamespace);
  procedure addIfNewPrefixUrl(const ns: TNamespace);
  procedure addIfNewPrefixUrl(const ns: INamespace);

  procedure deleteFrom(i: integer);

  function clone: TNamespaceList;

  property namespaces[prefix: string]: INamespace read getNamespace;
  property items[i: integer]: INamespace read getNamespace;
end;

const XMLNamespaceUrl_XML = 'http://www.w3.org/XML/1998/namespace';
      XMLNamespaceUrl_XMLNS = 'http://www.w3.org/2000/xmlns/';

var
   XMLNamespace_XMLNS, XMLNamespace_XML: INamespace;

function equalNamespaces(const ans, bns: INamespace): boolean; inline;
function equalNamespaces(const ans, bns: string): boolean; inline;
function namespaceGetURL(const n: INamespace): string; inline;


implementation

uses bbutils;



function equalNamespaces(const ans, bns: INamespace): boolean;
begin
  result := (ans = bns) or ((ans <> nil) and (bns <> nil) and strEqual(ans.getURL, bns.getURL));
end;

function equalNamespaces(const ans, bns: string): boolean;
begin
  result := strEqual(ans, bns);
end;

function namespaceGetURL(const n: INamespace): string;
begin
  if n = nil then result := ''
  else result := n.getURL;
end;



function TNamespaceList.getNamespace(const prefix: string): INamespace;
begin
  hasNamespacePrefix(prefix, result);
end;

function TNamespaceList.getNamespace(i: integer): INamespace;
begin
  result := INamespace(pointer(inherited get(i)));
end;

function TNamespaceList.hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
var
  i: Integer;
begin
  for i := c - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then exit(true);
  exit(false);
end;

function TNamespaceList.hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then begin
      ns := items[i];
      exit(true);
    end;
  ns := nil;
  exit(false);
end;

function TNamespaceList.hasNamespacePrefix(const prefix: string): boolean;
var temp: INamespace;
begin
  result := hasNamespacePrefix(prefix, temp);
end;

function TNamespaceList.hasNamespace(const n: INamespace): boolean;
var
  temp: INamespace;
begin
  if not hasNamespacePrefix(n.getPrefix, temp) then exit(false);
  if temp.getURL <> n.getURL then exit(false);
  result := true;
end;

function TNamespaceList.lastIndexOfNamespacePrefix(const prefix: string): integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then
      exit(i);
  exit(-1);
end;

procedure TNamespaceList.add(const ns: TNamespace);
begin
  inherited add(INamespace(ns)); //hide ancestor method to prevent crash when tnamespace is treated as inamespace instead being cast
end;

procedure TNamespaceList.add(const ns: INamespace);
begin
  inherited add(ns);
end;

procedure TNamespaceList.addIfNewPrefix(const ns: TNamespace);
begin
  addIfNewPrefix(INamespace(ns));
end;

procedure TNamespaceList.addIfNewPrefix(const ns: INamespace);
var
  temp: INamespace;
begin
  if (ns = nil) or (ns.getURL = XMLNamespaceUrl_XMLNS) or (ns.getURL = XMLNamespaceUrl_XML) then exit;
  if not hasNamespacePrefix(ns.getPrefix, temp) then
    add(ns);
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: TNamespace);
begin
  addIfNewPrefixUrl(INamespace(ns));
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: INamespace);
var
  temp: INamespace;
begin
  if (ns = nil) or (ns.getURL = XMLNamespaceUrl_XMLNS) or (ns.getURL = XMLNamespaceUrl_XML) then exit;
  if not hasNamespacePrefix(ns.getPrefix, temp) then
    add(ns)
  else if temp.getURL <> ns.getURL then
    add(ns);
end;

procedure TNamespaceList.deleteFrom(i: integer);
begin
  if i < 0 then i := 0;
  while count > i do
    delete(count - 1);
end;


function TNamespaceList.clone: TNamespaceList;
var
  i: Integer;
begin
  result := TNamespaceList.Create;
  for i := 0 to count - 1 do
    result.Add(items[i]);
end;

type TNamespaceCache = class
  uniqueUrl: string;
  prefixes: TXQHashmapStrOwningInterface;
  constructor Create;
  destructor Destroy; override;
end;

constructor TNamespaceCache.Create;
begin
  prefixes := TXQHashmapStrOwningInterface.Create;
end;

destructor TNamespaceCache.Destroy;
begin
  prefixes.free;
  inherited Destroy;
end;

threadvar globalNamespaceCache: TXQHashmapStrOwningObject;

function TNamespace.getSelf: TNamespace;
begin
  result := self;
end;

function TNamespace.equal(const ns: string): boolean;
begin
  result := strEqual(url, ns);
end;

constructor TNamespace.create(const aurl: string; aprefix: string);
begin
  url := aurl;
  prefix := aprefix;
end;

function namespaceCache(const aurl: string): TNamespaceCache;
begin
  if globalNamespaceCache = nil then globalNamespaceCache := TXQHashmapStrOwningObject.Create();
  result := TNamespaceCache(globalNamespaceCache[aurl]);
  if result = nil then begin
    result := TNamespaceCache.Create;
    result.uniqueUrl := aurl;
    globalNamespaceCache.Add(aurl, result);
    //writeln(strFromPtr(pointer(aurl)), ' ',aurl);
  end;
end;

class function TNamespace.make(const aurl: string; const aprefix: string): TNamespace;
var cache : TNamespaceCache;
  tempptr: Pointer;
begin
  cache := namespaceCache(aurl);
  tempptr := pointer(cache.prefixes[aprefix]);
  if tempptr = nil then begin
    result := TNamespace.create(cache.uniqueUrl, aprefix);
    cache.prefixes.Add(aprefix, result);
  end else result := (IUnknown(tempptr) as INamespace).getSelf;
end;

class function TNamespace.uniqueUrl(const aurl: string): string;
begin
  result := namespaceCache(aurl).uniqueUrl;
end;

class procedure TNamespace.freeCache;
begin
  FreeAndNil(globalNamespaceCache);
end;

function TNamespace.getPrefix: string;
begin
  if self = nil then exit('');
  result := prefix;
end;

function TNamespace.getURL: string;
begin
  if self = nil then exit('');
  result := url;
end;

function TNamespace.serialize: string;
begin
  if prefix = '' then result := 'xmlns="'+xmlStrEscape(url, true)+'"'
  else result := 'xmlns:'+prefix+'="'+xmlStrEscape(url, true)+'"'
end;

destructor TNamespace.Destroy;
begin
  inherited Destroy;
end;

initialization
  XMLNamespace_XML := TNamespace.Make(XMLNamespaceUrl_XML, 'xml');
  XMLNamespace_XMLNS := TNamespace.Make(XMLNamespaceUrl_XMLNS, 'xmlns');

end.

