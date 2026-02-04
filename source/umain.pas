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
  ExtCtrls, ComCtrls, ActnList, Menus, IpHtml,
  Version, PasExt, Icons, MultiApp, LogView, Updater, Preferences,
  DosCRT, Witch;

type

  TCodepageFilter = (cpfAll, cpfPartial, cpfComplete);

  { TfMain }

  TfMain = class(TMultiAppForm)
      actExportUnicode: TAction;
      actOpen: TAction;
      actExportCodepage: TAction;
      actDebugLog: TAction;
      actClose: TAction;
      actCodepageFilter: TAction;
      actListCompatible: TAction;
      actListPartial: TAction;
      actListAll: TAction;
      actOnlineUpdate: TAction;
      actPreferences: TAction;
      alMain: TActionList;
      ctrlBar: TControlBar;
      hpUnicodeText: TIpHtmlPanel;
      lbUnicodeViewLabel: TLabel;
      lbCodepageLabel: TLabel;
      lbFileList: TLabel;
      lvCodepageList: TListView;
      lvFileList: TListView;
      miListGood: TMenuItem;
      miListPotential: TMenuItem;
      miListAll: TMenuItem;
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
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure actExportCodepageUpdate(Sender: TObject);
    procedure actExportUnicodeUpdate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actListAllExecute(Sender: TObject);
    procedure actListCompatibleExecute(Sender: TObject);
    procedure actListPartialExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lvCodepageListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvFileListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure tAnimateTimer(Sender: TObject);
    private
      fCodepageFilter: TCodepageFilter;
      lbViewCodePageLabel : TLabel;
      btnExportFile : TToolButton;
      btnCodepageFilter : TToolButton;
      fWitch : TWitch;
      fCodepageText : TDosCrt;
      procedure PopulateCodePageList(Item : TWitchItem);
      procedure SetCodepageFilter(AValue: TCodepageFilter);
      procedure SetUnicodeView( S : String );
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure SetApplicationIcons;
      procedure SetCodepageViewLabel;
      procedure UpdateMetaData;
      procedure UpdateCodepageList;
      procedure UpdateStatusBar;
      procedure UpdateUnicodeView;
      procedure UpdateCodepageView;
      procedure UpdateButtons;
      procedure UpdateFilterCheck;
    public
      procedure ApplyUserLanguage; override;
      procedure OpenFile(FileName : String; Select : boolean = False); overload;
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

procedure TfMain.actCloseExecute(Sender: TObject);
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

procedure TfMain.actCloseUpdate(Sender: TObject);
begin
 // actClose.Enabled:=Assigned(lvFileList.Selected);
  actClose.Enabled:=
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
end;

procedure TfMain.actExportCodepageUpdate(Sender: TObject);
begin
  actExportCodepage.Enabled:=Assigned(lvCodePageList.Selected) and
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
end;

procedure TfMain.actExportUnicodeUpdate(Sender: TObject);
begin
  actExportUnicode.Enabled:=Assigned(lvCodePageList.Selected) and
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
end;

procedure TfMain.actOpenExecute(Sender: TObject);
var
  I : integer;
begin
  dlgOpenFile.InitialDir:=UserWorkPath;
  if dlgOpenFile.Execute then begin
    for I := dlgOpenFile.Files.Count - 1 downto 0 do
      OpenFile(dlgOpenFile.Files[I], I=0);
  end;
end;

procedure TfMain.actListAllExecute(Sender: TObject);
begin
  CodepageFilter:=cpfAll;
end;

procedure TfMain.actListCompatibleExecute(Sender: TObject);
begin
  CodepageFilter:=cpfComplete;
end;

procedure TfMain.actListPartialExecute(Sender: TObject);
begin
  CodepageFilter:=cpfPartial;
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

  fCodePageText := TDosCrt.Create(Self);
  // fCodePageText.Name:='fCodePageText';
  fCodePageText.Parent:=sbCodePage;

  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;

  SetApplicationIcons;

  // Assign Images to Actions
  actOpen.ImageIndex:=idxButtonFileOpen;
  actExportUnicode.ImageIndex:=idxButtonFileExportGreen;
  actExportCodepage.ImageIndex:=idxButtonFileExport;
  actClose.ImageIndex:=idxButtonFileClose;
  actPreferences.ImageIndex:=idxButtonPreferences;
  actOnlineUpdate.ImageIndex:=idxButtonUpdateCheck;
  actDebugLog.ImageIndex:=idxButtonDebugLog;
  actListCompatible.ImageIndex:=idxButtonListViewFinished;
  actListPartial.ImageIndex:=idxButtonListViewPartial;
  actListAll.ImageIndex:=idxButtonListViewEmpty;
  actCodePageFilter.ImageIndex:=idxButtonListView;

  // Add Main ToolBar Buttons
  CreateToolButton(tbMain, actOpen);
  btnExportFile:=CreateToolButton(tbMain, actExportCodepage);
  CreateToolButton(tbMain, actClose);
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

  SetUnicodeView('');
end;

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
var
  I : Integer;
begin
  for I := High(FileNames) downto 0 do
    OpenFile(FileNames[I], I=0);
end;

procedure TfMain.lvCodepageListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  IgnoreParameter(Sender);
  IgnoreParameter(Item);
  IgnoreParameter(Change);
  UpdateStatusBar;
end;

procedure TfMain.lvFileListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  IgnoreParameter(Sender);
  IgnoreParameter(Item);
  IgnoreParameter(Change);
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
  I, P : integer;
begin
  if not Assigned(Item) then Exit;
  if Item.Analyzed then begin
    // Processing complete
    tAnimate.Enabled:=False;
    lvCodePageList.Enabled:=True;
    lvCodePageList.SmallImages:=ilPercentageColor;
    K:=ComponentNamePath(lvCodePageList, Self, True);
    case Item.Encoding of
      weNone : begin
        // Only Liwer 7-Bit ASCII characters, compatible with any codepage
        L:=lvCodePageList.Items.Add;
        L.Caption:=GetTranslation(K+'Any_Codepage/Caption', 'Any Codepage');
        L.ImageIndex:=High(iconPercentageNames);
      end;
      weUnicode : begin
        // UTF-8/Unicode encoded file
        for I := 0 to High(Item.Results) do begin
          P := High(iconPercentageNames) * Item.Results[I].Compatible div 100;
          if (P = 0) and (Item.Results[I].Compatible <> 0) then
            P := 1
          else if (P = High(iconPercentageNames)) and (Item.Results[I].Compatible <> 100) then
            P := High(iconPercentageNames) - 1;

          if (Item.Results[I].Compatible<>100) and (CodePageFilter=cpfComplete) then
            Continue
          else
          if (Item.Results[I].Compatible = 0) and (CodePageFilter=cpfPartial) then
            Continue;
          { permit incompatible codepages }

          L:=lvCodePageList.Items.Add;
          L.Caption:=IntToStr(Item.Results[I].Codepage);
          // L.Caption:=GetTranslation(K+'Analyzed/Caption', 'Analyzed');
          L.ImageIndex:=P;
          end;
      end;
      weCodepage : begin
        // File is not UTF-8 so must be Codepage encoded
        L:=lvCodePageList.Items.Add;
        L.ImageIndex:=0;
        L.Caption:=GetTranslation(K+'Not_implemented/Caption', 'Not implemented');
      end;
    end;
  end else begin
    // Still srocessing text file in background thread
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
  UpdateFilterCheck;
  UpdateMetaData;
end;

procedure TfMain.SetUnicodeView(S: String);
var
  C : TColor;
begin
  // This does not scroll a TMemo to the top on macOS
  // mUnicodeText.VertScrollBar.Position:=0;
  // This does work on macOS
  //mUnicodeText.SelStart := 0;
  //mUnicodeText.SelLength := 0;
  C:=ColorToRGB(clWindow);
  hpUnicodeText.SetHtmlFromStr('<html><body style="background-color:' +
    IntToHex(Red(C), 2) + IntToHex(Green(C), 2) + IntToHex(Blue(C), 2) + '; '+
    'margin:0; font-weight:light; font-size:100%;">' +
    '<pre>' + S + '</pre>' +
    { StringReplace(S, CR, '<br>',[rfReplaceAll]) + }
    '<br></body></html>');
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
var
  S : String;
begin
  S:=Trim(Lowercase(GetConfig('Codepage_Filter', 'partial')));
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
  UpdateCodePagelist;
  UpdateButtons;
  UpdateStatusBar;
  UpdateUnicodeView;
  UpdateCodepageView;
end;

procedure TfMain.UpdateCodepageList;
begin
  lvCodePageList.BeginUpdate;
  lvCodePageList.Clear;
  if Assigned(lvFileList.Selected) then begin
    PopulateCodePageList(TWitchItem(lvFileList.Selected.Data));
  end;
  lvCodePageList.EndUpdate;
end;

procedure TfMain.UpdateStatusBar;
const
  spiEncoding = 0;               // displayed when file is selected
  spiCompatiblity = 1;           // displayed when codepage is selected
  // spiLanguage = 2;            // not yet implemented
  // spiPrefered = 3;            // not yet implemented
  spiFileName = 2;               // displayed when file is selected
var
  W : TWitchItem;
  K : String;
  I, V, E : integer;
  Compat : String;
begin
  if not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data))then begin
    statBar.Panels[spiEncoding].Text:='';
    statBar.Panels[spiCompatiblity].Text:='';
    // statBar.Panels[spiLanguage].Text:='';
    // statBar.Panels[spiPrefered].Text:='';
    statBar.Panels[spiFileName].Text:='';
    Exit;
  end;
  Compat:='';
  W:=TWitchItem(lvFileList.Selected.Data);
  statBar.Panels[spiFileName].Text:=SPACE2+W.FileName;
  K:=ComponentNamePath(statBar, Self, True);
  if W.Analyzed then begin
    case W.Encoding of
      weNone : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'NoEncoding/Caption', 'ASCII');
      weCodePage : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Codepage/Caption', 'Codepage');
      weUnicode : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Unicode/Caption', 'Unicode');
    end;
    if Assigned(lvCodePageList.Selected) then begin
      Val(lvCodePageList.Selected.Caption, V, E);
      if E = 0 then begin
        for I := 0 to High(W.Results) do
          if W.Results[I].Codepage = V then begin
            Compat:=IntToStr(W.Results[I].Compatible);
            Break;
          end;
      end;
    end;
  end else begin
    statBar.Panels[spiEncoding].Text:=GetTranslation(K+'Processing/Caption', 'Processing');
  end;
  if Compat <> '' then Compat:=Compat+'%';
  statBar.Panels[spiCompatiblity].Text:=Compat;
end;

procedure TfMain.UpdateUnicodeView;
var
  W : TWitchItem;
  H : String;
begin
  if (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then begin
    W:=TWitchItem(lvFileList.Selected.Data);
    if W.Analyzed then begin
      case W.Encoding of
        weNone, weUnicode : begin
          H:=EscapeHTML(PasExt.ToString(W.FileData));
        end;
        weCodePage : begin
          H:= '';
        end;
      end;

    end;
  end;
  SetUnicodeView(H);
end;

procedure TfMain.UpdateCodepageView;
var
  W : TWitchItem;
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then begin
    // Clear
    Exit;
  end;
  W:=TWitchItem(lvFileList.Selected.Data);
  if not W.Analyzed then begin
    // Clear
    Exit;
  end else begin
    case W.Encoding of
      weNone : begin
      end;
      weCodePage : begin
      end;
      weUnicode : begin
      end;
    end;
  end;
end;

procedure TfMain.UpdateButtons;
begin
  if Assigned(lvFileList.Selected) then begin
    case TWitchItem(lvFileList.Selected.Data).Encoding of
      weNone, weCodepage : begin
        actClose.ImageIndex:=idxButtonFileClose;
        btnExportFile.Action:=actExportUnicode;
      end;
      weUnicode: begin
        actClose.ImageIndex:=idxButtonFileCloseGreen;
        btnExportFile.Action:=actExportCodepage;
      end;
    end;
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
  F : TStringList;
begin
  if DirectoryExists(FileName) then begin
    F := TStringList.Create;
    DirScan(IncludeTrailingPathDelimiter(FileName) + WildCard, F,
      [dsFiles, dsRecursive]);
    for I := F.Count - 1 downto 0 do
      OpenFile(IncludeTrailingPathDelimiter(FileName) + F[I], I=0);
    FreeAndNil(F);
    Exit;
  end;

  UserWorkPath:=IncludeTrailingPathDelimiter(ExtractFilePath(FileName));

  I:=fWitch.Find(FileName);
  if I <> -1 then begin
    LogMessage(vbVerbose, 'Open file "' + FileName + '" already open.');
    fWitch.Select(I);
    Exit;
  end;

  try
    I := fWitch.Add(FileName, lvFileList.Items.Add);
  except
    LogMessage(vbVerbose, 'Open file "' + FileName + '" Failed!');
    Exit;
  end;

  LogMessage(vbVerbose, 'Opened file "' + FileName + '"');
  if Select then begin
    lvFileList.Sort;
    fWitch.Select(I);
    fWitch.Items[I].ListItem.MakeVisible(False);
    lvFileList.SetFocus;
    UpdateMetaData;
  end;

end;


end.

