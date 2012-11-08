unit simplexmltreeparserfpdom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser, DOM, XMLRead;

type

{ TTreeParserDOM }

TTreeParserDOM = class(TTreeParser)
  function import(dom: TDOMDocument): TTreeDocument;
  function parseDOM(const document: String; const uri: string = ''): TTreeDocument;
  function parseDOMFromFile(const filename: string): TTreeDocument;
end;

implementation


{ TTreeParserDOM }

function TTreeParserDOM.import(dom: TDOMDocument): TTreeDocument;
var doc: TTreeDocument;
  namespaces: TNamespaceList;
  function getNamespace(const url, prefix: string): INamespace;
  begin
    if namespaces.hasNamespacePrefix(prefix, result) then  //not tested, but should work like this
      if result.getURL = url then exit;
    result := TNamespace.create(url, prefix);
    namespaces.add(result);
  end;

  procedure importNode(parent: TTreeNode; node: TDOMNode);
  var
    i: Integer;
    new: TTreeNode;
    nscount: Integer;
  begin
    nscount := namespaces.Count;
    if node is TDOMElement then begin
      new := TTreeNode.createElementPair(node.NodeName);
      if node.HasAttributes then
        for i := 0 to node.Attributes.Length - 1 do begin
          new.addAttribute(node.Attributes[i].NodeName, node.Attributes[i].NodeValue);
          if node.Attributes[i].NamespaceURI <> '' then
            new.attributes.Items[new.attributes.count - 1].namespace := getNamespace(node.Attributes[i].NamespaceURI, node.Attributes[i].Prefix);
        end;
      for i := 0 to node.ChildNodes.Count - 1 do
        importNode(new, node.ChildNodes[i]);
    end else begin
      if (node is TDOMText) or (node is TDOMCDATASection) then new := TTreeNode.create(tetText, node.NodeValue)
      else if node is TDOMComment then new := TTreeNode.create(tetComment, node.NodeValue)
      else if node is TDOMProcessingInstruction then begin
        new := TTreeNode.create(tetProcessingInstruction, node.NodeName);
        new.addAttribute('', node.NodeValue);
      end else exit;
    end;

    if node.NamespaceURI <> '' then
      new.namespace := getNamespace(node.NamespaceURI, node.Prefix);


    parent.addChild(new);
    namespaces.DeleteFrom(nscount);
  end;
var i: Integer;
  temp: TTreeNode;
  offset: Integer;
  a: TTreeAttribute;
begin
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

  for i := 0 to dom.ChildNodes.Count - 1 do
    importNode(doc, dom.ChildNodes[i]);

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

function TTreeParserDOM.parseDOM(const document: String; const uri: string): TTreeDocument;
var
  temp: TXMLDocument;
  stringStream: TStringStream;
begin
  stringStream := TStringStream.Create(document);
  ReadXMLFile(temp, stringStream);
  result := import(temp);
  result.baseURI:=uri;
  result.documentURI:=uri;
  stringStream.Free;
  temp.Free;
end;

function TTreeParserDOM.parseDOMFromFile(const filename: string): TTreeDocument;
var
  temp: TXMLDocument;
begin
  ReadXMLFile(temp, filename);
  result := import(temp);
  temp.Free;
end;

end.

