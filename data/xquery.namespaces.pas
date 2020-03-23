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
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils, xquery.internals.common;

type

TNamespace = class;

//** Namespace interface, storing url and prefix. (Interface, so it is ref-counted)
INamespace = interface
['{5F6DF5F2-548C-4F13-9BEA-CE59EBAE4CAB}']
  function getPrefix: string; //**< Returns the prefix
  function getURL: string; //**< Returns the url
  function serialize: string; //**< Returns an xmlns attribute declaring this namespace with url and prefix
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
end;


{ TNamespace }

//** Class implementing the INamespace interface
TNamespace = class(TFastInterfacedObject, INamespace)
public
  url: string;
  prefix: string;
  //** Creates a new namespace with url and prefix. (watch the argument order. It follows the XPath fn:QName function)
  constructor create(const aurl: string; aprefix: string);

  class function make(const aurl: string; const aprefix: string): TNamespace; static;
  class function makeWithRC1(const aurl: string; const aprefix: string): TNamespace; static;
  class function uniqueUrl(const aurl: string): string; static;
  class procedure freeCache; static;
  class procedure assignNonNil(var old: TNamespace; new: TNamespace); static; inline;
  class procedure assignRC(var old: TNamespace; new: TNamespace); static; inline;
  class procedure releaseIfNonNil(var old: TNamespace); static; inline;

  function getPrefix: string;
  function getURL: string;
  function serialize: string;
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
  destructor Destroy; override;
end;

{ TNamespaceList }

//** List of namespaces
TNamespaceList = class
private
  list: TFPList;
  function getNamespace(const prefix: string): TNamespace;
  function getNamespace(i: integer): TNamespace; inline;
  procedure remove(ns: TNamespace);
  procedure remove(ns: INamespace);
public
  function hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
  function hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
  function hasNamespacePrefix(const prefix: string; out ns: TNamespace): boolean;
  function hasNamespacePrefix(const prefix: string): boolean;
  function hasNamespace(const n: INamespace): boolean;
  function hasNamespace(const n: TNamespace): boolean;

  function lastIndexOfNamespacePrefix(const prefix: string): integer;

  procedure add(const ns: TNamespace); reintroduce;
  procedure add(const ns: INamespace); reintroduce;
  procedure addIfNewPrefix(const ns: TNamespace);
  procedure addIfNewPrefix(const ns: INamespace);
  procedure addIfNewPrefixUrl(const ns: TNamespace);
  procedure addIfNewPrefixUrl(const ns: INamespace);

  procedure delete(i: Integer);
  procedure deleteFrom(i: integer);

  constructor Create;
  destructor Destroy; override;
  procedure clear;
  function clone: TNamespaceList;

  function count: integer;

  property namespaces[prefix: string]: TNamespace read getNamespace;
  property items[i: integer]: TNamespace read getNamespace;
end;

const XMLNamespaceUrl_XML = 'http://www.w3.org/XML/1998/namespace';
      XMLNamespaceUrl_XMLNS = 'http://www.w3.org/2000/xmlns/';

var
   XMLNamespace_XMLNS, XMLNamespace_XML: TNamespace;

function equalNamespaces(const ans, bns: TNamespace): boolean; inline;
function equalNamespaces(const ans, bns: string): boolean; inline;
function namespaceGetURL(const n: INamespace): string; inline;


implementation

uses bbutils;



function equalNamespaces(const ans, bns: TNamespace): boolean;
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



function TNamespaceList.getNamespace(const prefix: string): TNamespace;
begin
  hasNamespacePrefix(prefix, result);
end;

function TNamespaceList.getNamespace(i: integer): TNamespace;
begin
  result := TNamespace((list.Items[i]));
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
var temp: TNamespace;
begin
  result := hasNamespacePrefix(prefix, temp);
  if result then ns := temp
  else ns := nil;
end;

function TNamespaceList.hasNamespacePrefix(const prefix: string; out ns: TNamespace): boolean;
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
var temp: TNamespace;
begin
  result := hasNamespacePrefix(prefix, temp);
end;

function TNamespaceList.hasNamespace(const n: INamespace): boolean;
begin
  result := hasNamespace(n.getSelf);
end;

function TNamespaceList.hasNamespace(const n: TNamespace): boolean;
var
  temp: TNamespace;
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
  ns._AddRef;
  list.add(ns);
end;

procedure TNamespaceList.add(const ns: INamespace);
begin
  add(ns.getSelf);
end;

procedure TNamespaceList.addIfNewPrefix(const ns: INamespace);
begin
  addIfNewPrefix(ns.getSelf);
end;

procedure TNamespaceList.addIfNewPrefix(const ns: TNamespace);
var
  temp: TNamespace;
begin
  if (ns = nil) or (ns.getURL = XMLNamespaceUrl_XMLNS) or (ns.getURL = XMLNamespaceUrl_XML) then exit;
  if not hasNamespacePrefix(ns.getPrefix, temp) then
    add(ns);
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: INamespace);
begin
  addIfNewPrefixUrl(ns.getSelf);
end;

procedure TNamespaceList.remove(ns: TNamespace);
begin
  if list.Remove(ns) >= 0 then ns._Release;
end;

procedure TNamespaceList.remove(ns: INamespace);
begin
  remove(ns.getSelf)
end;

procedure TNamespaceList.delete(i: Integer);
begin
  TFastInterfacedObject(list[i])._Release;
  list.Delete(i);
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: TNamespace);
var
  temp: TNamespace;
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

constructor TNamespaceList.Create;
begin
  list := TFPList.Create;
end;

destructor TNamespaceList.Destroy;
begin
  clear;
  list.Free;
  inherited Destroy;
end;

procedure TNamespaceList.clear;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    TFastInterfacedObject(list.items[i])._Release;
  list.Clear;
end;


function TNamespaceList.clone: TNamespaceList;
var
  i: Integer;
begin
  result := TNamespaceList.Create;
  for i := 0 to count - 1 do
    result.Add(items[i]);
end;

function TNamespaceList.count: integer;
begin
  result := list.Count;
end;

type
INamespaceTracker = record
  class procedure addref(const ns: INamespace); static;
  class procedure release(const ns: INamespace); static;
end;

TXQHashmapStrOwningNamespace = specialize TXQHashmapStrOwning<INamespace, INamespaceTracker>;
TNamespaceCache = class
  uniqueUrl: string;
  prefixes: TXQHashmapStrOwningNamespace;
  constructor Create;
  destructor Destroy; override;
end;

class procedure INamespaceTracker.addref(const ns: INamespace);
begin
  ns._AddRef;
end;

class procedure INamespaceTracker.release(const ns: INamespace);
begin
  ns._Release;
end;

constructor TNamespaceCache.Create;
begin
  prefixes.init();
end;

destructor TNamespaceCache.Destroy;
begin
  prefixes.done;
  inherited Destroy;
end;

threadvar globalNamespaceCache: ^TXQHashmapStrOwningObject;

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
  if globalNamespaceCache = nil then new(globalNamespaceCache,init);
  result := TNamespaceCache(globalNamespaceCache^[aurl]);
  if result = nil then begin
    result := TNamespaceCache.Create;
    result.uniqueUrl := aurl;
    globalNamespaceCache^[aurl] := result;
    //writeln(strFromPtr(pointer(aurl)), ' ',aurl);
  end;
end;

{$ImplicitExceptions off}
class function TNamespace.make(const aurl: string; const aprefix: string): TNamespace;
var cache : TNamespaceCache;
  old: INamespace;
begin
  cache := namespaceCache(aurl);
  old := cache.prefixes[aprefix];
  if old = nil then begin
    result := TNamespace.create(cache.uniqueUrl, aprefix);
    cache.prefixes[aprefix] := result;
  end else result := old.getSelf;
end;

class function TNamespace.makeWithRC1(const aurl: string; const aprefix: string): TNamespace;
begin
  result := make(aurl, aprefix);
  result._AddRef;
end;

{$ImplicitExceptions on}

class function TNamespace.uniqueUrl(const aurl: string): string;
begin
  result := namespaceCache(aurl).uniqueUrl;
end;

class procedure TNamespace.freeCache;
begin
  if globalNamespaceCache <> nil then begin
    dispose(globalNamespaceCache,done);
    globalNamespaceCache := nil;
  end;
end;

class procedure TNamespace.assignNonNil(var old: TNamespace; new: TNamespace);
begin
  old._Release;
  new._AddRef;
  old := new;
end;

class procedure TNamespace.assignRC(var old: TNamespace; new: TNamespace);
begin
  if old <> nil then old._Release;
  if new <> nil then new._AddRef;
  old := new;
end;

class procedure TNamespace.releaseIfNonNil(var old: TNamespace);
begin
  if old <> nil then old._Release;
  old := nil;
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
  //writeln(stderr, 'destroy: ', url);
  inherited Destroy;
end;

initialization
  XMLNamespace_XML := TNamespace.makeWithRC1(XMLNamespaceUrl_XML, 'xml');
  XMLNamespace_XMLNS := TNamespace.makeWithRC1(XMLNamespaceUrl_XMLNS, 'xmlns');
finalization
  XMLNamespace_XML._Release;
  XMLNamespace_XMLNS._Release;
end.

