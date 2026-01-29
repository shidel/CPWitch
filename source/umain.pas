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
  Version, PasExt, Icons, MultiApp, LogView, Updater, Preferences, Witch;

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
      imgCodepage: TImage;
      lbUnicodeViewLabel: TLabel;
      lbCodepageLabel: TLabel;
      lbFileList: TLabel;
      lvCodepageList: TListView;
      lvFileList: TListView;
      mUnicodeText: TMemo;
      mmMain: TMainMenu;
      dlgOpenFile: TOpenDialog;
      pViewCodepageLabel: TPanel;
      pViewUnicodeLabel: TPanel;
      pCodepage: TPanel;
      pViewUnicode: TPanel;
      pViewers: TPanel;
      pCodepageListLabel: TPanel;
      pCodepageList: TPanel;
      pFileListLabel: TPanel;
      pFileList: TPanel;
      sbCodepage: TScrollBox;
      spFilesCPs: TSplitter;
      spCPsViewers: TSplitter;
      spUnicodeCP: TSplitter;
      statBar: TStatusBar;
      tbMain: TToolBar;
    procedure actDebugLogExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    private
      lbViewCodePageLabel : TLabel;
      fWitch : TWitch;
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure SetApplicationIcons;
      procedure SetCodepageViewLabel;
    public
      procedure ApplyUserLanguage; override;
      procedure OpenFile(FileName : String; Select : boolean = False);
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

procedure TfMain.actFileOpenExecute(Sender: TObject);
var
  I : integer;
begin
  dlgOpenFile.InitialDir:=UserWorkPath;
  if dlgOpenFile.Execute then begin
     for I := 0 to dlgOpenFile.Files.Count - 1 do
       OpenFile(dlgOpenFile.Files[I], I=0); // I = dlgOpenFile.Files.Count - 1);
  end;
end;

procedure TfMain.actOnlineUpdateExecute(Sender: TObject);
begin
  UpdateCheck(True);
end;

procedure TfMain.actPreferencesExecute(Sender: TObject);
begin
  PreferencesShow;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  fWitch := TWitch.Create;
  fWitch.OnAnalyzed:=@WitchOnAnalyzed;
  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;

  SetApplicationIcons;

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

  // Create at runtime without name to not save Caption iduring NLS generation.
  lbViewCodepageLabel :=TLabel.Create(Self);
  lbViewCodepageLabel.Parent:=pViewCodepageLabel;
  lbViewCodepageLabel.Align:=alTop;
  lbViewCodepageLabel.AutoSize:=True;
  lbViewCodepageLabel.BorderSpacing.Around:=8;

  mUnicodeText.Clear;
  imgCodepage.Height:=1;
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
begin
  SetApplicationIcons;
end;

procedure TfMain.FormSettingsSave(Sender: TObject);
begin
  SetConfig('Nothing', '1234');
end;

procedure TfMain.WitchOnAnalyzed(Sender: TObject);
var
  W : TWitchItem;
begin
  if not (Sender is TWitchItem) then Exit;

  W:=Sender as TWitchItem;
  case W.Encoding of
    weNone : W.ListItem.ImageIndex:=idxFileTypeFilePlainGray;
    weCodePage : W.ListItem.ImageIndex:=idxFileTypeFilePlainBlue;
    weUnicode : W.ListItem.ImageIndex:=idxFileTypeFilePlainGreen;
  end;
end;

procedure TfMain.SetApplicationIcons;
begin
  tbMain.Images:=IconTheme.ButtonEnabled;
  tbMain.DisabledImages:=IconTheme.ButtonDisabled;
  tbMain.HotImages:=IconTheme.ButtonHover;
  lvFileList.SmallImages:=ilFileTypeColor;
  lvCodePageList.SmallImages:=ilPercentageColor;
end;

procedure TfMain.SetCodepageViewLabel;
begin

  lbViewCodepageLabel.Caption:=GetFormat(ComponentNamePath(pViewCodepageLabel,
    Self, True) + 'lbViewCodepageLabel/Value' , ['437'],
    'Viewed as Codepage %s');
end;

procedure TfMain.ApplyUserLanguage;
begin
  inherited ApplyUserLanguage;
  SetCodepageViewlabel;
end;

procedure TfMain.OpenFile(FileName: String; Select: boolean);
var
  I : integer;
begin
  UserWorkPath:=IncludeTrailingPathDelimiter(ExtractFilePath(FileName));

  I:=fWitch.Find(FileName);
  if I <> -1 then begin
    fWitch.Select(I);
    Exit;
  end;

  I := fWitch.Add(FileName, lvFileList.Items.Add);
  if I = -1 then Exit;
  if Select then
    fWitch.Select(I);

end;



end.

