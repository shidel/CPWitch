{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uMain;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  Version, PasExt, Icons, MultiApp, LogView, Updater
  { other forms }
  ;

type

  { TfMain }

  TfMain = class(TMultiAppForm)
    btnLogWindow: TButton;
    btnUpdater: TButton;
    ctrlBar: TControlBar;
    statBar: TStatusBar;
    ToolBar1: TToolBar;
    procedure btnLogWindowClick(Sender: TObject);
    procedure btnUpdaterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    private
      TBS : Array of TToolButton;
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
    public
    published
  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.btnLogWindowClick(Sender: TObject);
begin
  LogShow;
end;

procedure TfMain.btnUpdaterClick(Sender: TObject);
begin
  UpdateCheck(True);
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  I : integer;
begin
  TBS:=[];
  SetLength(TBS, 10);
  for I := Low(TBS) to High(TBS) do begin
      TBS[I]:=TToolButton.Create(Self);
      TBS[I].Parent:=ToolBar1;
      TBS[I].ImageIndex:=I;
  end;
  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;
  ToolBar1.Images:=ilButtonEnabled;
  ToolBar1.DisabledImages:=ilButtonDisabled;
  ToolBar1.HotImages:=ilButtonHover;
  ToolBar1.Width:=ToolBar1.Images.Width * (Length(TBS) + 1);
end;


procedure TfMain.FormSettingsLoad(Sender: TObject);
begin
end;

procedure TfMain.FormSettingsSave(Sender: TObject);
begin
  SetConfig('Nothing', '1234');
end;


end.

