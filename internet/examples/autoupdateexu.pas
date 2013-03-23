unit autoupdateexu; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,autoupdate;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure updateEnabledButtons;
  public
    { public declarations }
    au: TAutoUpdater;
  end; 

var
  Form1: TForm1; 

implementation

uses internetAccess, {$IFDEF WINDOWS}w32InternetAccess{$ELSE}synapseinternetaccess{$ENDIF};

{ TForm1 }


procedure TForm1.Button4Click(Sender: TObject);
begin
  au:=TAutoUpdater.create(StrToInt(Edit1.Text),'',Edit2.Text,Edit3.Text);
  updateEnabledButtons();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if au.existsUpdate then memo1.Lines.Add('update exists:'+IntToStr(au.newestVersion));
    updateEnabledButtons();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add(au.listChanges);
    updateEnabledButtons();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  au.downloadUpdate();
  updateEnabledButtons();
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FreeAndNil(au);
    updateEnabledButtons();
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  au.installUpdate;
  updateEnabledButtons();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  defaultInternetAccessClass:=TW32InternetAccess;
  {$ELSE}
  defaultInternetAccessClass:=TSynapseInternetAccess;
  {$ENDIF}
end;

procedure TForm1.updateEnabledButtons;
begin
  Button4.Enabled:=au=nil;
  Button1.Enabled:=(au<>nil);
  Button2.Enabled:=(au<>nil);
  Button3.Enabled:=(au<>nil);
  Button6.Enabled:=(au<>nil)and(au.installerCmd<>'')and(FileExists(Utf8ToAnsi(au.downloadedFileName)));
  button5.Enabled:=au<>nil;
end;

initialization
  {$I autoupdateexu.lrs}

end.
