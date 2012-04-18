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
    trimming: TComboBox;
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
    memo3.Lines.Text:=htmlparser.variableChangeLog.debugTextRepresentation;
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
  tp.readComments:=true;
  tp.parsingModel:=pmHTML;
  tp.trimText := trimming.ItemIndex = 3;
  tp.parseTree(memo2.Lines.Text);
  if trimming.ItemIndex = 2 then tp.removeEmptyTextNodes(true);

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
    vars: TPXPVariableChangeLog;
begin
  ppath := TPseudoXPathParser.Create;
  vars := TPXPVariableChangeLog.create();
  tp := TTreeParser.Create;
  try
    ppath.OnEvaluateVariable:=@vars.evaluateVariable;
    ppath.OnDefineVariable:=@vars.defineVariable;
    ppath.parse(memo1.Lines.text);

    tp.readComments:=true;
    tp.parsingModel:=pmHTML;
    tp.trimText := trimming.ItemIndex = 3;
    tp.parseTree(memo2.Lines.Text);
    if trimming.ItemIndex = 2 then tp.removeEmptyTextNodes(true);


    ppath.ParentElement := tp.getTree;
    ppath.RootElement := tp.getTree;
    memo3.Lines.Text:=ppath.evaluate().toString;
  finally
    tp.Free;
    ppath.Free;
    vars.Free;
  end;
end;

procedure TForm1.htmlparserVariableRead(variable: string; value: string);
begin
  memo3.Lines.Add(variable+ ' = '+value);
end;

end.

