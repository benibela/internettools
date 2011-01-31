unit htmlparsergui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, extendedhtmlparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure htmlparserVariableRead(variable: string; value: string);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses simplehtmltreeparser,pseudoxpath,bbutils;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var htmlparser: THtmlTemplateParser;
  i: Integer;
begin
  htmlparser := THtmlTemplateParser.create;
  if not CheckBox1.Checked then htmlparser.OutputEncoding:=eUnknown;
  try
    htmlparser.parseTemplate(memo1.Lines.Text);
    memo3.Clear;
    //htmlparser.onVariableRead:=@htmlparserVariableRead;
    htmlparser.parseHTML(memo2.Lines.Text);
    memo3.Lines.Clear;
    for i:=0 to htmlparser.variableChangeLog.Count-1 do
      memo3.lines.Add(htmlparser.variableChangeLog[i]);
  finally
    htmlparser.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  tp: TTreeParser;
  cur: TTreeElement;
begin
  tp := TTreeParser.Create;
  tp.parsingModel:=pmHTML;
  tp.parseTree(memo2.Lines.Text);

  cur := tp.getTree;
  memo3.Lines.Clear;
  while cur <> nil do begin
    memo3.Lines.Add(cur.toString());
    cur:=cur.next;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var ppath: TPseudoXPathParser;
    tp: TTreeParser;
begin
  ppath := TPseudoXPathParser.Create;
  try
    ppath.parse(memo1.Lines.text);
    tp := TTreeParser.Create;
    tp.readComments:=true;
    tp.parsingModel:=pmHTML;
    tp.parseTree(memo2.Lines.Text);
    ppath.ParentElement := tp.getTree;
    ppath.RootElement := tp.getTree;
    memo3.Lines.Text:=ppath.evaluate();
  finally
    ppath.Free;
  end;
end;

procedure TForm1.htmlparserVariableRead(variable: string; value: string);
begin
  memo3.Lines.Add(variable+ ' = '+value);
end;

end.

