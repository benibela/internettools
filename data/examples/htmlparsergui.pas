unit htmlparsergui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, extendedhtmlparser,simplehtmltreeparser,pseudoxpath;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBoxTextOnly: TCheckBox;
    CheckBoxverbose: TCheckBox;
    CheckBoxOptions: TCheckBox;
    CheckBoxEntities: TCheckBox;
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
    procedure CheckBoxOptionsChange(Sender: TObject);
    procedure htmlparserVariableRead(variable: string; value: string);
  private
    { private declarations }
    function mypxptostring(v: TPXPValue): string;
    procedure parseHTML(tp: TTreeParser);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses bbutils;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var htmlparser: THtmlTemplateParser;
  i: Integer;
begin
  htmlparser := THtmlTemplateParser.create;
  if not CheckBoxEntities.Checked then htmlparser.OutputEncoding:=eUnknown;
  try
    htmlparser.AllowVeryShortNotation:=CheckBoxShortnotatin.Checked;
    htmlparser.AllowObjects:=CheckBoxObjects.Checked;
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
      memo3.Lines.add(htmlparser.variableChangeLog.getVariableName(i)+'='+mypxptostring(htmlparser.variableChangeLog.getVariableValue(i)));
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
  memo3.Lines.Text := tp.getLastTree.outerXML();
  tp.free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var ppath: TPseudoXPathParser;
    tp: TTreeParser;
    vars: TPXPVariableChangeLog;
    temp: TPXPValue;
begin
  ppath := TPseudoXPathParser.Create;
  vars := TPXPVariableChangeLog.create();
  tp := TTreeParser.Create;
  try
    ppath.OnEvaluateVariable:=@vars.evaluateVariable;
    ppath.OnDefineVariable:=@vars.defineVariable;
    ppath.AllowVariableUseInStringLiterals:=CheckBoxVarsInStrs.Checked;
    vars.allowObjects:=CheckBoxObjects.Checked;
    ppath.parse(memo1.Lines.text);

    parseHTML(tp);

    ppath.ParentElement := tp.getLastTree;
    ppath.RootElement := tp.getLastTree;
    temp := ppath.evaluate();
    memo3.Lines.Text:=mypxptostring(temp);
    temp.free;
  finally
    tp.Free;
    ppath.Free;
    vars.Free;
  end;
end;

procedure TForm1.CheckBoxOptionsChange(Sender: TObject);
begin
  options.visible:=CheckBoxOptions.Checked;
end;

procedure TForm1.htmlparserVariableRead(variable: string; value: string);
begin
  memo3.Lines.Add(variable+ ' = '+value);
end;

function TForm1.mypxptostring(v: TPXPValue): string;
var
  temp: TPXPValueObject;
  i: Integer;
begin
  if not CheckBoxverbose.Checked then begin
    if (CheckBoxTextOnly.Checked) or not (v is TPXPValueNode) then result := v.asString
    else result := v.asNode.outerXML()
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
  if CheckBoxEntities.Checked and assigned(tp.getLastTree) then tp.getLastTree.setEncoding(eUTF8, true, true);
  if trimming.ItemIndex = 2 then tp.removeEmptyTextNodes(true);
end;

end.

