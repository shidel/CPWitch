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
      actFileClose: TAction;
      actCodepageFilter: TAction;
      actListGood: TAction;
      actListPotential: TAction;
      actListAll: TAction;
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
      miListGood: TMenuItem;
      miListPotential: TMenuItem;
      miListAll: TMenuItem;
      mUnicodeText: TMemo;
      mmMain: TMainMenu;
      dlgOpenFile: TOpenDialog;
      pmListMode: TPopupMenu;
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
      tAnimate: TTimer;
    procedure actCodepageFilterExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCloseUpdate(Sender: TObject);
    procedure actFileExportUpdate(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvFileListClick(Sender: TObject);
    procedure tAnimateTimer(Sender: TObject);
    private
      lbViewCodePageLabel : TLabel;
      btnCodepageFilter : TToolButton;
      fWitch : TWitch;
      procedure PopulateCodePageList(Item : TWitchItem);
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure SetApplicationIcons;
      procedure SetCodepageViewLabel;
      procedure UpdateMetaData;
      procedure UpdateStatusBar;
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

procedure TfMain.actCodepageFilterExecute(Sender: TObject);
begin
  if Assigned(btnCodepageFilter.DropdownMenu) then
    btnCodepageFilter.CheckMenuDropdown;
end;

procedure TfMain.actFileCloseExecute(Sender: TObject);
var
  N : Integer;
begin
  if not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)
  and TWitchItem(lvFileList.Selected.Data).Analyzed) then Exit;
  N:=lvFileList.Selected.Index;
  if N>=lvFileList.Items.Count - 1 then
    N:=lvFileList.Items.Count - 2;
  fWitch.Delete(TWitchItem(lvFileList.Selected.Data));
  lvFileList.Selected.Delete;
  if N >= 0 then
    lvFileList.Items[N].Selected:=True;
  UpdateMetaData;
end;

procedure TfMain.actFileCloseUpdate(Sender: TObject);
begin
 // actFileClose.Enabled:=Assigned(lvFileList.Selected);
  actFileClose.Enabled:=
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
end;

procedure TfMain.actFileExportUpdate(Sender: TObject);
begin
  actFileExport.Enabled:=Assigned(lvCodePageList.Selected) and
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
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
  actFileClose.ImageIndex:=idxButtonFileClose;
  actPreferences.ImageIndex:=idxButtonPreferences;
  actOnlineUpdate.ImageIndex:=idxButtonUpdateCheck;
  actDebugLog.ImageIndex:=idxButtonDebugLog;
  actListGood.ImageIndex:=idxButtonListViewFinished;
  actListPotential.ImageIndex:=idxButtonListViewPartial;
  actListAll.ImageIndex:=idxButtonListViewEmpty;
  actCodePageFilter.ImageIndex:=idxButtonListView;

  // Add Main ToolBar Buttons
  CreateToolButton(tbMain, actFileOpen);
  CreateToolButton(tbMain, actFileExport);
  CreateToolButton(tbMain, actFileClose);
  CreateToolButton(tbMain, tbsDivider, 'btnDivider1');
  btnCodepageFilter:=CreateToolButton(tbMain, actCodepageFilter);
  btnCodepageFilter.Style:=tbsButtonDrop;
  btnCodepageFilter.DropdownMenu:=pmListMode;

  //btnCodepageFilter:=CreateToolButton(tbMain, tbsButtonDrop, 'btnCodepageFilter');
  //btnCodepageFilter.Style:=tbsDropDown;
  //btnCodepageFilter.DropdownMenu:=pmListMode;
  //btnCodePageFilter.ImageIndex:=idxButtonListView;

  CreateToolButton(tbMain, tbsDivider, 'btnDivider2');
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

procedure TfMain.lvFileListClick(Sender: TObject);
begin
  UpdateMetaData;
end;

procedure TfMain.tAnimateTimer(Sender: TObject);
var
  I : integer;
begin
  if lvCodePageList.Items.Count <> 1 then Exit;
  if not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)
  and (TWitchItem(lvFileList.Selected.Data).Analyzed=False)) then begin
     UpdateMetaData;
     Exit;
  end;
  I:=lvCodePageList.Items[0].ImageIndex+1;
  if I >= lvCodePageList.SmallImages.Count then
    I := 0;
  lvCodePageList.Items[0].ImageIndex:=I;
end;

procedure TfMain.PopulateCodePageList(Item: TWitchItem);
var
  L : TListItem;
  K : String;
begin
  if not Assigned(Item) then Exit;
  if Item.Analyzed then begin
    lvCodePageList.Enabled:=True;
    lvCodePageList.SmallImages:=ilPercentageColor;
    L:=lvCodePageList.Items.Add;
    K:=ComponentNamePath(lvCodePageList, Self, True);
    L.Caption:=GetTranslation(K+'Analyzed/Caption', 'Analyzed');
    tAnimate.Enabled:=False;
  end else begin
    lvCodePageList.Enabled:=False;
    lvCodePageList.SmallImages:=ilWorkingColor;
    L:=lvCodePageList.Items.Add;
    K:=ComponentNamePath(lvCodePageList, Self, True);
    L.Caption:=GetTranslation(K+'Analyzing/Caption', 'Analyzing');
    L.ImageIndex:=0;
    tAnimate.Enabled:=True;
  end;
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

  if W.ListItem = lvFileList.Selected then
    UpdateMetaData;
end;

procedure TfMain.SetApplicationIcons;
begin
  tbMain.Images:=IconTheme.ButtonEnabled;
  tbMain.DisabledImages:=IconTheme.ButtonDisabled;
  tbMain.HotImages:=IconTheme.ButtonHover;
  pmListMode.Images:=IconTheme.ButtonEnabled;

  lvFileList.SmallImages:=ilFileTypeColor;
  lvCodePageList.SmallImages:=ilPercentageColor;
end;

procedure TfMain.SetCodepageViewLabel;
begin

  lbViewCodepageLabel.Caption:=GetFormat(ComponentNamePath(pViewCodepageLabel,
    Self, True) + 'lbViewCodepageLabel/Value' , ['437'],
    'Viewed as Codepage %s');
end;

procedure TfMain.UpdateMetaData;
begin
  lvCodePageList.BeginUpdate;
  lvCodePageList.Clear;
  if Assigned(lvFileList.Selected) then begin
    PopulateCodePageList(TWitchItem(lvFileList.Selected.Data));
  end;
  lvCodePageList.EndUpdate;
  UpdateStatusBar;
end;

procedure TfMain.UpdateStatusBar;
var
  W : TWitchItem;
  K : String;
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data))then begin
    statBar.Panels[0].Text:='';
    statBar.Panels[1].Text:='';
    statBar.Panels[2].Text:='';
    statBar.Panels[3].Text:='';
    Exit;
  end;
  W:=TWitchItem(lvFileList.Selected.Data);
  statBar.Panels[3].Text:=SPACE2+W.FileName;
  K:=ComponentNamePath(statBar, Self, True);
  if W.Analyzed then begin
    case W.Encoding of
      weNone : statBar.Panels[0].Text:=GetTranslation(K+'NoEncoding/Caption', 'ASCII');
      weCodePage : statBar.Panels[0].Text:=GetTranslation(K+'Codepage/Caption', 'Codepage');
      weUnicode : statBar.Panels[0].Text:=GetTranslation(K+'Unicode/Caption', 'Unicode');
    end;
  end else begin
    statBar.Panels[0].Text:=GetTranslation(K+'Processing/Caption', 'Processing');
  end;
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
  if Select then begin
    fWitch.Select(I);
    UpdateMetaData;
  end;

end;



end.

