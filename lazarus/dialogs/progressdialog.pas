unit progressdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { ProgressBarDialog }

  { TProgressBarDialog }

  TProgressBarDialog = class(TForm)
    Label1: TLabel;
    barLabel: TLabel;
    ProgressBar1: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
    addUnit: string;
    constructor create(title,description:string;maxprogress:longint=100);
    procedure setProgress(progress:longint);
    procedure progressEvent(sender:TObject;progress,maxprogress:longint);
  end;

implementation

{$R *.lfm}

constructor TProgressBarDialog.create(title,description: string; maxprogress: longint=100);
begin
  inherited Create(Application);
  Caption:=title;
  Label1.Caption:=description;
  ProgressBar1.Max:=maxprogress;
  setProgress(0);
  //BorderIcons:=[];
  //FormStyle:=fsStayOnTop;
  Show;
  Application.ProcessMessages;
end;

procedure TProgressBarDialog.setProgress(progress: longint);
var
  s: String;
begin
  ProgressBar1.Position:=progress;
  s:=IntToStr(progress)+addUnit+'/'+inttostr(ProgressBar1.max)+addUnit;
  if ProgressBar1.max<>0 then s:=s+' ('+IntToStr(100*progress div ProgressBar1.max)+'%)';
  barLabel.Caption:=s;
  Application.ProcessMessages;
end;

procedure TProgressBarDialog.progressEvent(sender: TObject; progress,
  maxprogress: longint);
begin
  if ProgressBar1.Max<>maxprogress then
    ProgressBar1.Max:=maxprogress;

  setProgress(progress);
end;


end.

