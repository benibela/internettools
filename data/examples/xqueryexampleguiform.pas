unit xqueryExampleGUIForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, extendedhtmlparser,simplehtmltreeparser,xquery;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBoxJSON: TCheckBox;
    CheckBoxTextOnly: TCheckBox;
    CheckBoxverbose: TCheckBox;
    CheckBoxOptions: TCheckBox;
    CheckBoxObjects: TCheckBox;
    CheckBoxShortnotatin: TCheckBox;
    CheckBoxVarsInStrs: TCheckBox;
    options: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    trimming: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBoxJSONChange(Sender: TObject);
    procedure CheckBoxOptionsChange(Sender: TObject);
    procedure htmlparserVariableRead(variable: string; value: string);
  private
    { private declarations }
    function mypxptostring(const v: IXQValue): string;
    procedure parseHTML(tp: TTreeParser);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses bbutils, xquery_json;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var htmlparser: THtmlTemplateParser;
  i: Integer;
begin
  htmlparser := THtmlTemplateParser.create;
  try
    htmlparser.AllowVeryShortNotation:=CheckBoxShortnotatin.Checked;
    if CheckBoxObjects.Checked then htmlparser.QueryEngine.ParsingOptions.AllowPropertyDotNotation := xqpdnAllowFullDotNotation
    else htmlparser.QueryEngine.ParsingOptions.AllowPropertyDotNotation := xqpdnDisallowDotNotation;
    htmlparser.QueryEngine.ParsingOptions.AllowJSON:=CheckBoxJSON.Checked;
    htmlparser.parseTemplate(memo1.Lines.Text);
    htmlparser.trimTextNodes:=TTrimTextNodes(trimming.ItemIndex);
    memo3.Clear;
    //htmlparser.onVariableRead:=@htmlparserVariableRead;
    try
      htmlparser.parseHTML(memo2.Lines.Text);
    except on e: EHTMLParseException do begin
      Memo3.Lines.text:='Parser-Exception: ' + e.Message;
      Memo3.Lines.add('Partial matches: ');
      Memo3.Lines.add(htmlparser.debugMatchings(50));
      raise;
    end
    end;

    for i:=0 to htmlparser.variableChangeLog.count-1 do
      memo3.Lines.add(htmlparser.variableChangeLog.getName(i)+'='+mypxptostring(htmlparser.variableChangeLog.get(i)));
//    memo3.Lines.Text:=htmlparser.variableChangeLog.debugTextRepresentation;
  finally
    htmlparser.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  tp: TTreeParser;
begin
  tp := TTreeParser.Create;
  parseHTML(tp);
  memo3.Lines.Text := tp.getLastTree.outerXML(true);
  tp.free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var ppath: TXQueryEngine;
    tp: TTreeParser;
    temp: IXQValue;
begin
  ppath := TXQueryEngine.Create;
  tp := TTreeParser.Create;
  try
    ppath.ParsingOptions.AllowExtendedStrings:=CheckBoxVarsInStrs.Checked;
    if CheckBoxObjects.Checked then ppath.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation
    else ppath.ParsingOptions.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
    ppath.ParsingOptions.AllowJSON:=CheckBoxJSON.Checked;

    if (sender as tbutton).tag = 1 then ppath.parseXQuery3(memo1.lines.Text)
    else ppath.parseXPath3(memo1.Lines.text);

    parseHTML(tp);

    temp := ppath.evaluate(tp.getLastTree);
    memo3.Lines.Text:=mypxptostring(temp);
  finally
    tp.Free;
    ppath.Free;
  end;
end;

procedure TForm1.CheckBoxJSONChange(Sender: TObject);
begin

end;

procedure TForm1.CheckBoxOptionsChange(Sender: TObject);
begin
  options.visible:=CheckBoxOptions.Checked;
end;

procedure TForm1.htmlparserVariableRead(variable: string; value: string);
begin
  memo3.Lines.Add(variable+ ' = '+value);
end;

function TForm1.mypxptostring(const v: IXQValue): string;
var
  i: Integer;
begin
  if not CheckBoxverbose.Checked then begin
    if (CheckBoxTextOnly.Checked) or not (v is TXQValueNode) then result := v.toString
    else result := v.toNode.outerXML()
  end else
    result := v.debugAsStringWithTypeAnnotation(CheckBoxTextOnly.Checked);
end;

procedure TForm1.parseHTML(tp: TTreeParser);
begin
  tp.readComments:=true;
  tp.readProcessingInstructions:=true;
  tp.parsingModel:=pmHTML;
  tp.trimText := trimming.ItemIndex = 3;
  tp.autoDetectHTMLEncoding:=false;
  tp.parseTree(memo2.Lines.Text);
 // if CheckBoxEntities.Checked and assigned(tp.getLastTree) then tp.getLastTree.setEncoding(CP_UTF8, true, true);
  if trimming.ItemIndex = 2 then tp.removeEmptyTextNodes(true);
end;

end.


