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
  ExtCtrls, ComCtrls, ActnList, Menus,
  Version, PasExt, Icons, MultiApp, LogView, Updater
  { other forms }
  ;

type

  { TfMain }

  TfMain = class(TMultiAppForm)
      actFileOpen: TAction;
      actFileExport: TAction;
      actDebugLog: TAction;
      actOnlineUpdate: TAction;
      actPreferences: TAction;
      alMain: TActionList;
      ctrlBar: TControlBar;
      mmMain: TMainMenu;
      statBar: TStatusBar;
      tbMain: TToolBar;
    procedure actDebugLogExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    private
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

procedure TfMain.actDebugLogExecute(Sender: TObject);
begin
  LogShow;
end;

procedure TfMain.actOnlineUpdateExecute(Sender: TObject);
begin
  UpdateCheck(True);
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;
  tbMain.Images:=ilButtonEnabled;
  tbMain.DisabledImages:=ilButtonDisabled;
  tbMain.HotImages:=ilButtonHover;
  // tbMain.Width:=tbMain.Images.Width * (Length(TBS) + 1);

  // Assign Images
  actFileOpen.ImageIndex:=idxButtonFileOpen;
  actFileExport.ImageIndex:=idxButtonFileExport;
  actPreferences.ImageIndex:=-1;

  CreateToolButton(tbMain, actFileOpen);
  CreateToolButton(tbMain, actFileExport);
  CreateToolButton(tbMain, actPreferences);
  CreateToolButton(tbMain, actOnlineUpdate);
  CreateToolButton(tbMain, actDebugLog);
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
begin
end;

procedure TfMain.FormSettingsSave(Sender: TObject);
begin
  SetConfig('Nothing', '1234');
end;


end.

