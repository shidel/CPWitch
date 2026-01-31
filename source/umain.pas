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

  TCodepageFilter = (cpfAll, cpfPartial, cpfComplete);

  { TfMain }

  TfMain = class(TMultiAppForm)
      actFileOpen: TAction;
      actFileExport: TAction;
      actDebugLog: TAction;
      actFileClose: TAction;
      actCodepageFilter: TAction;
      actListCompatible: TAction;
      actListPartial: TAction;
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
    procedure actListAllExecute(Sender: TObject);
    procedure actListCompatibleExecute(Sender: TObject);
    procedure actListPartialExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lvFileListClick(Sender: TObject);
    procedure tAnimateTimer(Sender: TObject);
    private
      FCodepageFilter: TCodepageFilter;
      lbViewCodePageLabel : TLabel;
      btnCodepageFilter : TToolButton;
      fWitch : TWitch;
      procedure PopulateCodePageList(Item : TWitchItem);
      procedure SetCodepageFilter(AValue: TCodepageFilter);
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure SetApplicationIcons;
      procedure SetCodepageViewLabel;
      procedure UpdateMetaData;
      procedure UpdateStatusBar;
      procedure UpdateFilterCheck;
    public
      procedure ApplyUserLanguage; override;
      procedure OpenFile(FileName : String; Select : boolean = False);
    published
      property CodepageFilter : TCodepageFilter read FCodepageFilter write SetCodepageFilter;

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

procedure TfMain.actListAllExecute(Sender: TObject);
begin
  CodepageFilter:=cpfAll;
  UpdateFilterCheck;
  UpdateMetaData;
end;

procedure TfMain.actListCompatibleExecute(Sender: TObject);
begin
  CodepageFilter:=cpfComplete;
  UpdateFilterCheck;
  UpdateMetaData;
end;

procedure TfMain.actListPartialExecute(Sender: TObject);
begin
  CodepageFilter:=cpfPartial;
  UpdateFilterCheck;
  UpdateMetaData;
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
  actListCompatible.ImageIndex:=idxButtonListViewFinished;
  actListPartial.ImageIndex:=idxButtonListViewPartial;
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

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
var
  I : Integer;
begin
  for I := 0 to High(FileNames) do
    OpenFile(FileNames[I], I=0);
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

procedure TfMain.SetCodepageFilter(AValue: TCodepageFilter);
begin
  if FCodepageFilter=AValue then Exit;
  FCodepageFilter:=AValue;
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
var
  S : String;
begin
  S:=Trim(Lowercase(GetConfig('Codepage_Filter', '')));
  case S of
    'complete' : FCodepageFilter:=cpfComplete;
    'partial' : FCodepageFilter:=cpfPartial;
  else
    FCodepageFilter:=cpfAll;
  end;
  UpdateFilterCheck;
  SetApplicationIcons;
end;

procedure TfMain.FormSettingsSave(Sender: TObject);
var
  S: String;
begin
  case FCodepageFilter of
    cpfComplete : S := 'Complete';
    cpfPartial : S := 'Partial';
    cpfAll : S := 'All';
  end;
  SetConfig('Codepage_Filter', S);
end;

procedure TfMain.WitchOnAnalyzed(Sender: TObject);
var
  W : TWitchItem;
  M : String;
begin
  if not (Sender is TWitchItem) then Exit;
  W:=Sender as TWitchItem;
  M:='Analyzed ';
  case W.Encoding of
    weNone : begin
      Cat(M, 'ASCII');
      W.ListItem.ImageIndex:=idxFileTypeFilePlainGray;
    end;
    weCodePage : begin
      Cat(M, 'Codepage');
      W.ListItem.ImageIndex:=idxFileTypeFilePlainBlue;
    end;
    weUnicode : begin
      Cat(M, 'Unicode');
      W.ListItem.ImageIndex:=idxFileTypeFilePlainGreen;
    end;
  end;
  Cat(M, ' file "'+W.DisplayName+'"');
  LogMessage(vbVerbose, M);

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

procedure TfMain.UpdateFilterCheck;
begin
  actListCompatible.Checked:=FCodepageFilter=cpfComplete;
  actListPartial.Checked:=FCodepageFilter=cpfPartial;
  actListAll.Checked:=FCodepageFilter=cpfAll;
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
    LogMessage(vbVerbose, 'Open file "' + FileName + '" already open.');
    fWitch.Select(I);
    Exit;
  end;

  I := fWitch.Add(FileName, lvFileList.Items.Add);
  if I = -1 then begin
    LogMessage(vbVerbose, 'Open file "' + FileName + '" Failed!');
    Exit;
  end;
  LogMessage(vbVerbose, 'Opened file "' + FileName + '"');
  if Select then begin
    fWitch.Select(I);
    UpdateMetaData;
  end;

end;


end.

