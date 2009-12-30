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
    constructor create(title,description:string;maxprogress:longint=100);
    procedure setProgress(progress:longint);
    procedure progressEvent(sender:TObject;progress,maxprogress:longint);
  end; 

var
  ProgressBarDialog: TProgressBarDialog;

implementation

{ ProgressBarDialog }

constructor TProgressBarDialog.create(title,description: string; maxprogress: longint=100);
begin
  inherited Create(Application);
  Caption:=title;
  Label1.Caption:=description;
  ProgressBar1.Max:=maxprogress;
  //BorderIcons:=[];
  //FormStyle:=fsStayOnTop;
  Show;
  Application.ProcessMessages;
end;

procedure TProgressBarDialog.setProgress(progress: longint);
begin
  ProgressBar1.Position:=progress;
  barLabel.Caption:=IntToStr(progress)+'/'+inttostr(ProgressBar1.max);
  //barLabel.Left:=(ProgressBar1.Width-barLabel.Width) div 2 + ProgressBar1.Left;
  //barLabel.left:=ClientWidth-barLabel.Width-10;
  Application.ProcessMessages;
end;

procedure TProgressBarDialog.progressEvent(sender: TObject; progress,
  maxprogress: longint);
begin
  if ProgressBar1.Max<>maxprogress then begin
    barLabel.Width:=Canvas.TextWidth(IntToStr(maxprogress)+'/')*2;
    barLabel.Left:=width-barLabel.Width;
    ProgressBar1.Width:=barLabel.Left-ProgressBar1.Left-10;
    ProgressBar1.Max:=maxprogress;
  end;
  setProgress(progress);
end;

initialization
  {$I progressdialog.lrs}

end.
