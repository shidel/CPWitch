{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uMain;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus,
  Version, PasExt, Icons, MultiApp, LogView, Updater,
  { other forms }
  uPreferences;

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
      lbCodepageLabel: TLabel;
      lbFileList: TLabel;
      lvCodepageList: TListView;
      lvFileList: TListView;
      mmMain: TMainMenu;
      pViewers: TPanel;
      pCodepageListLabel: TPanel;
      pCodepageList: TPanel;
      pFileListLabel: TPanel;
      pFileList: TPanel;
      spFilesCPs: TSplitter;
      spCPsViewers: TSplitter;
      statBar: TStatusBar;
      tbMain: TToolBar;
    procedure actDebugLogExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    private
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure SetButtonIcons;
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

procedure TfMain.actPreferencesExecute(Sender: TObject);
begin
  if not Assigned(fPreferences) then
      Application.CreateForm(TfPreferences, fPreferences);
  fPreferences.Show;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;

  SetButtonIcons;

  // Assign Images to Actions
  actFileOpen.ImageIndex:=idxButtonFileOpen;
  actFileExport.ImageIndex:=idxButtonFileExport;
  actPreferences.ImageIndex:=idxButtonPreferences;
  actOnlineUpdate.ImageIndex:=idxButtonUpdateCheck;
  actDebugLog.ImageIndex:=idxButtonDebugLog;

  // Add Main ToolBar Buttons
  CreateToolButton(tbMain, actFileOpen);
  CreateToolButton(tbMain, actFileExport);
  CreateToolButton(tbMain, tbsDivider);
  CreateToolButton(tbMain, actPreferences);
  CreateToolButton(tbMain, actOnlineUpdate);
  CreateToolButton(tbMain, actDebugLog);

  // Set Toolbar width
  tbMain.Width:=(tbMain.Images.Width + 4) * 10 + tbMain.Indent * 2;
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
begin
  SetButtonIcons;
end;

procedure TfMain.FormSettingsSave(Sender: TObject);
begin
  SetConfig('Nothing', '1234');
end;

procedure TfMain.SetButtonIcons;
begin
  tbMain.Images:=IconTheme.ButtonEnabled;
  tbMain.DisabledImages:=IconTheme.ButtonDisabled;
  tbMain.HotImages:=IconTheme.ButtonHover;
end;



end.

