{**
  @abstract(This unit provides a wrapper around the standard fpc dom unit)

  see TTreeParserDOM
*}
unit simplexmltreeparserfpdom;

{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
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
  Classes, SysUtils, simplehtmltreeparser, DOM, XMLRead,xmlutils;

type

{ TTreeParserDOM }

{** Base class for TTreeParserDOM *}
TTreeParserDOMBase = class(TTreeParser)
private
  fparser: TDOMParser;
public
  constructor Create;
  destructor destroy; override;

  //** Create a tree document from a standard fpc dom document
  function import(dom: TDOMDocument; fragment: boolean): TTreeDocument;
  //** Reads a tree document from a string, using the standard fpc dom functions to parse it
  function parseDOM(document: String; const uri: string = ''; const contenttype: string = ''): TTreeDocument;

  property Parser: TDOMParser read fparser;
end;

{** This class provides wrapper methods around the standard fpc DOM functions,
    to convert the TDOMDocument class created by fpc to the TTreeDocument class used by the XQuery engine.
*}
TTreeParserDOM = class(TTreeParserDOMBase)
  //** Reads a tree document from a string, using the standard fpc dom functions to parse it
  function parseTree(html: string; uri: string = ''; contentType: string = ''): TTreeDocument; override;
end;

implementation
uses bbutils;

{ TTreeParserDOM }

constructor TTreeParserDOMBase.Create;
begin
  inherited;
  fparser := TDOMParser.Create;
  fparser.Options.ExpandEntities := True;
  fparser.Options.CDSectionsAsText := True;
  fparser.Options.Namespaces := True;
  fparser.Options.PreserveWhitespace := True;
  fparser.Options.Validate := true;
  if not assigned(widestringmanager.Unicode2AnsiMoveProc) then
    raise Exception.Create('Need widestringmanager.Unicode2AnsiMoveProc');
end;

destructor TTreeParserDOMBase.destroy;
begin
  fparser.free;
  inherited destroy;
end;

function TTreeParserDOMBase.import(dom: TDOMDocument; fragment: boolean): TTreeDocument;
var doc: TTreeDocument;
  namespaces: TNamespaceList;
  encoding: TSystemCodePage;

  function convert(const u: UnicodeString): string;
  begin
    if encoding = CP_UTF8 then result := UTF8Encode(u)
    else widestringmanager.Unicode2AnsiMoveProc(PWideChar(u), result, TargetEncoding, length(u));
  end;

  function getNamespace(const url, prefix: string): INamespace;
  begin
    if namespaces.hasNamespacePrefix(prefix, result) then
      if result.getURL = url then exit;
    result := TNamespace.make(url, prefix);
    namespaces.add(result);
  end;

  procedure importNode(parent: TTreeNode; node: TDOMNode);
    function name(n: TDOMNode): string;
    begin
      if n.LocalName <>'' then result := convert(n.LocalName)
      else result := convert(n.NodeName);
    end;

  var
    i: Integer;
    new: TTreeNode;
    nscount: Integer;
  begin
    nscount := namespaces.Count;
    if node is TDOMElement then begin
      new := TTreeNode.createElementPair(name(node));
      new.document := parent.document; //if we do not set it now, further children do not know their document
      if node.HasAttributes then
        for i := 0 to node.Attributes.Length - 1 do begin
          new.addAttribute(name(node.Attributes[i]), convert(node.Attributes[i].NodeValue));
          if (node.Attributes[i].NamespaceURI <> '') and (node.Attributes[i].NodeName <> 'xmlns') then
            new.attributes.Items[new.attributes.count - 1].namespace := getNamespace(convert(node.Attributes[i].NamespaceURI), convert(node.Attributes[i].Prefix));
          case TDOMAttr(node.Attributes[i]).DataType of
          dtId:  new.attributes.Items[new.attributes.count - 1].setDataTypeHack(1);
          dtIdRef, dtIdRefs: new.attributes.Items[new.attributes.count - 1].setDataTypeHack(2);
          end;

        end;
      for i := 0 to node.ChildNodes.Count - 1 do
        importNode(new, node.ChildNodes[i]);
    end else begin
      if (node is TDOMText) or (node is TDOMCDATASection) then new := TTreeNode.create(tetText, convert(node.NodeValue))
      else if node is TDOMComment then new := TTreeNode.create(tetComment, convert(node.NodeValue))
      else if node is TDOMProcessingInstruction then begin
        new := TTreeNode.create(tetProcessingInstruction, convert(node.NodeName));
        new.addAttribute('', convert(node.NodeValue));
      end else exit;
    end;

    if node.NamespaceURI <> '' then
      new.namespace := getNamespace(convert(node.NamespaceURI), convert(node.Prefix));


    parent.addChild(new);
    namespaces.DeleteFrom(nscount);
  end;
var i: Integer;
  temp: TTreeNode;
  offset: Integer;
  a: TTreeAttribute;
  root: TDOMNode_WithChildren;
begin
  encoding := strActualEncoding(TargetEncoding);

  namespaces:= TNamespaceList.Create;
  doc := TTreeDocument.create(self);
  doc.baseURI:=dom.baseURI;
  doc.documentURI:=dom.baseURI;
  doc.document := doc;

  doc.reverse := TTreeNode.create(tetClose);
  doc.reverse.reverse := doc;
  doc.next := doc.reverse;
  doc.next.previous := doc;
  doc.reverse.document := doc;

  if fragment then root := dom.FirstChild as TDOMNode_WithChildren
  else root := dom;
  for i := 0 to root.ChildNodes.Count - 1 do
    importNode(doc, root.ChildNodes[i]);

  temp := doc;
  offset := 1;
  while temp <> nil do begin
    temp.offset := offset;
    offset += 1;
    if temp.attributes <> nil then begin
      for a in temp.attributes do begin
        a.offset := offset;
        offset += 1;
      end;
    end;
    temp := temp.next;
  end;

  FTrees.Add(doc);
  FCurrentTree := doc;

  namespaces.free;
  result := doc;
end;

function TTreeParserDOMBase.parseDOM(document: String; const uri: string; const contenttype: string): TTreeDocument;
var
  temp: TXMLDocument;
  source: TXMLInputSource;
  fragment: Boolean;
begin
  Parser.Options.ExpandEntities := true;
  Parser.Options.PreserveWhitespace := not trimText;
  Parser.Options.IgnoreComments := not readComments;

  fragment := strBeginsWith(contenttype, 'text/xml-external-parsed-entity');
  if fragment then document := '<wrapper>' + document + '</wrapper>';
  source:=TXMLInputSource.Create(document);
  try
    try
      Parser.Parse(source,temp);// TStringStream.Create(document)), temp)
      result := import(temp, fragment);
      result.baseURI:=uri;
      result.documentURI:=uri;
      temp.Free;
    finally
      source.free;
    end;
  except
    on e: EXMLReadError do raise ETreeParseException.Create(e.Message);
  end;
end;

function TTreeParserDOM.parseTree(html: string; uri: string; contentType: string): TTreeDocument;
begin
  Result:=parseDOM(html, uri, contentType);
end;
end.

