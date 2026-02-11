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
  DosCRT, DosFont, Codepages, Witch;

type

  TCodepageFilter = (cpfAll, cpfPartial, cpfComplete);

  { TfMain }

  TfMain = class(TMultiAppForm)
      actExportUnicode: TAction;
      actCloseAll: TAction;
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
      dlgFileSave: TSaveDialog;
      sbCodepage: TScrollBox;
      spFilesCPs: TSplitter;
      spCPsViewers: TSplitter;
      spUnicodeCP: TSplitter;
      statBar: TStatusBar;
      tbMain: TToolBar;
      tAnimate: TTimer;
    procedure actCloseAllExecute(Sender: TObject);
    procedure actCloseAllUpdate(Sender: TObject);
    procedure actCodepageFilterExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure actExportCodepageExecute(Sender: TObject);
    procedure actExportCodepageUpdate(Sender: TObject);
    procedure actExportUnicodeUpdate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actListAllExecute(Sender: TObject);
    procedure actListCompatibleExecute(Sender: TObject);
    procedure actListPartialExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lvCodepageListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tAnimateTimer(Sender: TObject);
    private
      FActiveCodepage: integer;
      fCodepageFilter: TCodepageFilter;
      lbViewCodepageLabel : TLabel;
      btnExportFile : TToolButton;
      btnCodepageFilter : TToolButton;
      fWitch : TWitch;
      fCodepageText : TDosCrt;
      fUFF : TUnicodeDosFont;
      procedure PopulateCodepageList(Item : TWitchItem);
      procedure SetCodepageFilter(AValue: TCodepageFilter);
      procedure SetUnicodeView( S : String );
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure SetApplicationIcons;
      procedure UpdateCodepageViewLabel;
      procedure UpdateMetaData;
      procedure UpdateCodepageList;
      procedure UpdateStatusBar;
      procedure UpdateUnicodeView;
      procedure UpdateCodepageView;
      procedure UpdateButtons;
      procedure UpdateFilterCheck;
      procedure SelectCodepage(Sender : TObject);
      procedure SelectFile(Sender : TObject);
    public
      procedure ApplyUserLanguage; override;
      procedure OpenFile(FileName : String; Select : boolean = False); overload;
      property ActiveCodepage : integer read FActiveCodepage;
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

procedure TfMain.actCloseAllUpdate(Sender: TObject);
begin
  actCloseAll.Enabled := lvFileList.Items.Count > 0;
end;

procedure TfMain.actCloseAllExecute(Sender: TObject);
var
  I : Integer;
  L : TListItem;
begin
  I := 0;
  while I < lvFileList.Items.Count do begin
    L := lvFileList.Items[I];
    if not (Assigned(L) and Assigned(L.Data) and TWitchItem(L.Data).Analyzed) then begin
      Inc(I);
      Continue;
    end;
    fWitch.Delete(TWitchItem(L.Data));
    lvFileList.Items[I].Delete;
  end;
  UpdateMetaData;
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

procedure TfMain.actExportCodepageExecute(Sender: TObject);
var
  W : TWitchItem;
  TCP : TUTF8ToCodepage;
  R : integer;
  N : String;
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then
    Exit;
  W:=TWitchItem(lvFileList.Selected.Data);
  TCP:=W.AsCodePage(FActiveCodepage);
  if not Assigned(TCP) then Exit;
  dlgFileSave.InitialDir:=UserWorkPath;
  N := ExcludeTrailing(W.DisplayName, '.UTF-8', false);
  dlgFileSave.FileName:=FileIterative(UserWorkPath + N);
  repeat
    if dlgFileSave.Execute then begin
      R:=FileSave(dlgFileSave.FileName, PasExt.ToBytes(TCP.Converted));
      if R <> 0 then
        if FileErrorDialog(dlgFileSave.FileName, R, True) <> mrRetry then
          R:=0;
    end else R:=0;
  until R=0;

  FreeAndNil(TCP);
end;

procedure TfMain.actExportCodepageUpdate(Sender: TObject);
begin
  actExportCodepage.Enabled:=Assigned(lvCodepageList.Selected) and
  Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) and
  TWitchItem(lvFileList.Selected.Data).Analyzed;
end;

procedure TfMain.actExportUnicodeUpdate(Sender: TObject);
begin
  actExportUnicode.Enabled:=Assigned(lvCodepageList.Selected) and
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
  FActiveCodepage := 437;
  fWitch := TWitch.Create;
  fWitch.OnAnalyzed:=@WitchOnAnalyzed;

  fCodepageText := TDosCrt.Create(Self);
  // fCodepageText.Name:='fCodepageText';
  fCodepageText.Parent:=sbCodepage;
  fCodepageText.Color:=clBlue;
  fCodepageText.Foreground := clWindowText;
  fCodepageText.Background := clWindow;
  fCodepageText.ErrorForeground := clRed;
  fCodepageText.ErrorBackground := clBlack;
  fCodepageText.ControlCodes:=False;
  fCodepageText.Wrapping:=False;
  { TODO 0 -cDevel Convert to BorderSpacing when supported by TCustomDosCRT }
  fCodepageText.Left:=8;
  fCodepageText.Top:=8;
  {$IFNDEF DARWIN}
  fCodepageText.Scale:=Point(2,2);
  {$ENDIF}

  fUFF:=TUnicodeDosFont.Create;
  if fUFF.LoadFromFile(AppDataPath + '0816norm.uff') = 0 then
    fCodepageText.Font:=fUFF
  else
    FreeAndNil(fUFF);

  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;

  SetApplicationIcons;

  fCodepageText.ClrScr;

  // Assign Images to Actions
  actOpen.ImageIndex:=idxButtonFileOpen;
  actExportUnicode.ImageIndex:=idxButtonFileExport;
  actExportCodepage.ImageIndex:=idxButtonFileExportGreen;
  actClose.ImageIndex:=idxButtonFileClose;
  actCloseAll.ImageIndex:=idxButtonFileCloseAll;
  actPreferences.ImageIndex:=idxButtonPreferences;
  actOnlineUpdate.ImageIndex:=idxButtonUpdateCheck;
  actDebugLog.ImageIndex:=idxButtonDebugLog;
  actListCompatible.ImageIndex:=idxButtonListViewFinished;
  actListPartial.ImageIndex:=idxButtonListViewPartial;
  actListAll.ImageIndex:=idxButtonListViewEmpty;
  actCodepageFilter.ImageIndex:=idxButtonListView;

  // Add Main ToolBar Buttons
  CreateToolButton(tbMain, actOpen);
  btnExportFile:=CreateToolButton(tbMain, actExportCodepage);
  CreateToolButton(tbMain, actClose);
  CreateToolButton(tbMain, actCloseAll);
  CreateToolButton(tbMain, tbsDivider, 'btnDivider1');
  btnCodepageFilter:=CreateToolButton(tbMain, actCodepageFilter);
  btnCodepageFilter.Style:=tbsButtonDrop;
  btnCodepageFilter.DropdownMenu:=pmListMode;
  CreateToolButton(tbMain, tbsDivider, 'btnDivider2');
  CreateToolButton(tbMain, actPreferences);
  CreateToolButton(tbMain, actOnlineUpdate);
  CreateToolButton(tbMain, actDebugLog);

  // Make controls "flat", UI looks weird without doing thins on Windows and
  // Linux. Has no effect on macOS.
  FlattenControl(pFileList);
  FlattenControl(pCodepageList);
  FlattenControl(pViewUnicode);
  FlattenControl(pCodepage);
  // Disable HideSelection for Windows. Or, when selecting things like the
  // codepage, The current File Selected becomes no longer highlighted.
  lvFileList.HideSelection:=False;
  lvCodepageList.HideSelection:=False;

  // Adjust stepping for codepage view parent ScrollBox.
  sbCodePage.HorzScrollBar.Increment:=16;
  sbCodePage.HorzScrollBar.Tracking:=True;
  sbCodePage.VertScrollBar.Increment:=32;
  sbCodePage.VertScrollBar.Tracking:=True;

  // Make toolbar the width of the visible buttons
  AdjustToolBarWidth(tbMain);

  // Create at runtime without name to not save Caption iduring NLS generation.
  lbViewCodepageLabel :=TLabel.Create(Self);
  lbViewCodepageLabel.Parent:=pViewCodepageLabel;
  lbViewCodepageLabel.Align:=alTop;
  lbViewCodepageLabel.AutoSize:=True;
  lbViewCodepageLabel.BorderSpacing.Around:=8;

  SetUnicodeView('');
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if Assigned(fUFF) then
    FreeAndNil(fUff);
end;

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
var
  I : Integer;
begin
  for I := High(FileNames) downto 0 do
    OpenFile(FileNames[I], I=0);
end;

procedure TfMain.lvCodepageListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  IgnoreParameter(Item);
  if Selected then SelectCodepage(Sender);
end;

procedure TfMain.lvFileListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  IgnoreParameter(Item);
  if Selected then SelectFile(Sender);
end;

procedure TfMain.tAnimateTimer(Sender: TObject);
var
  I : integer;
begin
  if lvCodepageList.Items.Count <> 1 then Exit;
  if not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)
  and (TWitchItem(lvFileList.Selected.Data).Analyzed=False)) then begin
     UpdateMetaData;
     Exit;
  end;
  I:=lvCodepageList.Items[0].ImageIndex+1;
  if I >= lvCodepageList.SmallImages.Count then
    I := 0;
  lvCodepageList.Items[0].ImageIndex:=I;
end;

procedure TfMain.PopulateCodepageList(Item: TWitchItem);
var
  L : TListItem;
  K : String;
  I, P : integer;
begin
  if not Assigned(Item) then Exit;
  if Item.Analyzed then begin
    // Processing complete
    tAnimate.Enabled:=False;
    lvCodepageList.Enabled:=True;
    lvCodepageList.SmallImages:=ilPercentageColor;
    K:=ComponentNamePath(lvCodepageList, Self, True);
    case Item.Encoding of
      weNone : begin
        // Only Liwer 7-Bit ASCII characters, compatible with any Codepage
        L:=lvCodepageList.Items.Add;
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

          if (Item.Results[I].Compatible<>100) and (CodepageFilter=cpfComplete) then
            Continue
          else
          if (Item.Results[I].Compatible = 0) and (CodepageFilter=cpfPartial) then
            Continue;
          { permit incompatible Codepages }

          L:=lvCodepageList.Items.Add;
          L.Caption:=IntToStr(Item.Results[I].Codepage);
          // L.Caption:=GetTranslation(K+'Analyzed/Caption', 'Analyzed');
          L.ImageIndex:=P;
          end;
      end;
      weCodepage : begin
       // File is not UTF-8 so must be Codepage encoded
        { TODO 9 -cDevel Implement Codepage List for Codepage encoded files. }
        L:=lvCodepageList.Items.Add;
        L.ImageIndex:=0;
        L.Caption:=GetTranslation(K+'Not_implemented/Caption', 'Not implemented');
      end;
    end;
  end else begin
    // Still srocessing text file in background thread
    lvCodepageList.Enabled:=False;
    lvCodepageList.SmallImages:=ilWorkingColor;
    L:=lvCodepageList.Items.Add;
    K:=ComponentNamePath(lvCodepageList, Self, True);
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
  B, F, EB, EF : String;
begin
  { TODO 0 -cLazarus_Bug On macOS, setting the VertScrollBar.Position does
    not scroll a TMemo }

  // This does not scroll a TMemo to the top on macOS
  // mUnicodeText.VertScrollBar.Position:=0;
  // This does work on macOS
  //
  // mUnicodeText.SelStart := 0;
  // mUnicodeText.SelLength := 0;

  C:=ColorToRGB(fCodepageText.Foreground);
  F:=IntToHex(Red(C), 2) + IntToHex(Green(C), 2) + IntToHex(Blue(C), 2);
  C:=ColorToRGB(fCodepageText.Background);
  B:=IntToHex(Red(C), 2) + IntToHex(Green(C), 2) + IntToHex(Blue(C), 2);
  C:=ColorToRGB(fCodepageText.ErrorForeground);
  EF:=IntToHex(Red(C), 2) + IntToHex(Green(C), 2) + IntToHex(Blue(C), 2);
  C:=ColorToRGB(fCodepageText.ErrorBackground);
  EB:=IntToHex(Red(C), 2) + IntToHex(Green(C), 2) + IntToHex(Blue(C), 2);
  { TODO 0 -cLazarus_Bug TIpHtmlPanel does not honor whites-space pre, pre-wrap or nowrap and wraps text anyway. }
  { TODO 0 -cLazarus_Bug TIpHtmlPanel displays HTML entities "as-is" inside PRE tags. }

  S:=NormalizeLineEndings(S, '<br>'+LF);
  S:=StringReplace(S, SPACE, '&nbsp;', [rfReplaceAll]);
  // TIpHtmlPanel incorrectly displays Named HTML Entities inside of PRE tags
  // displaying them as-is. For example, "&gt;" should be displayed as ">".
  // But, it is displayed as "&gt;". Also, pre, pre-wrap and nowrap will still
  // break lines at the edge of a window. Therefore, SPACE needs converted to
  // &nbsp and CR/LF need converted to a <br> tag.
  { TODO 6 -cDevel Add support to view unmapped characters as errors in UnicodeView }
  IgnoreParameter([EF, EB]);
  S:='<html><body style="' +
    'color:' + F + '; background-color:' + B + '; '+
    'margin:0; font-weight:light; font-size:90%;">' + CR +
    '<div style="font-family: monospace; ">'+ S + '</div>' +
    '<br></body></html>';
  hpUnicodeText.SetHtmlFromStr(S);
  // FileSave(AppBasePath + '/test.html',PasExt.ToBytes(S));
end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
var
  S : String;
  SX, SY : Integer;
begin
  S:=Trim(Lowercase(GetConfig('Codepage_List/Filter', 'partial')));
  case S of
    'complete' : FCodepageFilter:=cpfComplete;
    'partial' : FCodepageFilter:=cpfPartial;
  else
    FCodepageFilter:=cpfAll;
  end;
  SX:=GetConfig('Codepage_Scale/Horizontal', fCodepageText.Scale.X);
  SY:=GetConfig('Codepage_Scale/Vertical', fCodepageText.Scale.Y);
  fCodepageText.Scale:=Point(SX,SY);

  UpdateFilterCheck;
  SetApplicationIcons;
  { TODO 5 -cDevel Add load color settings for Good/Bad mappings }
  sbCodepage.Color:=fCodepageText.Background;
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
  SetConfig('Codepage_List/Filter', S);
  SetConfig('Codepage_Scale/Horizontal', fCodepageText.Scale.X);
  SetConfig('Codepage_Scale/Vertical', fCodepageText.Scale.Y);
  { TODO 5 -cDevel Add save color settings for Good/Bad mappings }
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
    weCodepage : begin
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
  lvCodepageList.SmallImages:=ilPercentageColor;
end;

procedure TfMain.UpdateCodepageViewLabel;
begin
  lbViewCodepageLabel.Caption:=GetFormat(ComponentNamePath(pViewCodepageLabel,
    Self, True) + 'lbViewCodepageLabel/Value' , [IntToStr(ActiveCodepage)],
    'Viewed as Codepage %s');
end;

procedure TfMain.UpdateMetaData;
begin
  UpdateCodepagelist;
  UpdateButtons;
  UpdateStatusBar;
  UpdateUnicodeView;
  UpdateCodepageView;
  UpdateCodepageViewLabel;
end;

procedure TfMain.UpdateCodepageList;
var
  I : integer;
  T : String;
  HCP, CP, E : integer;
  Item : TListItem;
begin
  lvCodepageList.BeginUpdate;
  lvCodepageList.Clear;
  if Assigned(lvFileList.Selected) then begin
    PopulateCodepageList(TWitchItem(lvFileList.Selected.Data));
  end;
  HCP:=FActiveCodepage;
  // Reselect Active Codepage if Possible;
  Item:=nil;
  for I := 0 to lvCodePageList.Items.Count -1 do begin
    T:=CutDelim(lvCodepageList.Items[I].Caption, SPACE, 1,1);
    Val(T, CP, E);
    if (E=0) and (CP=FActiveCodepage) then begin
      Item:=lvCodepageList.Items[I];
      Break;
    end;
  end;
  // If Active Codepage is not in list and If first item is not a Codepage
  // Number then select it instead.
  if (Not Assigned(Item)) and (lvCodePageList.Items.Count > 0) then begin
    T:=CutDelim(lvCodepageList.Items[0].Caption, SPACE, 1,1);
    Val(T, CP, E);
    if (E=1) then begin
      Item:=lvCodepageList.Items[0];
    end;
  end;
  if Assigned(Item) then begin
    Item.Selected:=True;
    Item.MakeVisible(False);
  end;
  fActiveCodepage:=HCP;
  lvCodepageList.EndUpdate;
end;

procedure TfMain.UpdateStatusBar;
const
  spiEncoding = 0;               // displayed when file is selected
  spiCompatiblity = 1;           // displayed when Codepage is selected
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
      weCodepage : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Codepage/Caption', 'Codepage');
      weUnicode : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Unicode/Caption', 'Unicode');
    end;
    if Assigned(lvCodepageList.Selected) then begin
      Val(lvCodepageList.Selected.Caption, V, E);
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
        weCodepage : begin
{ TODO 9 -cDevel Implement Unicode View for Codepage encoded files. }
          H:= GetTranslation(ComponentNamePath(lvCodepageList, Self, True)
          +'Not_implemented/Caption', 'Not implemented');
        end;
      end;

    end;
  end;
  SetUnicodeView(H);
end;

procedure TfMain.UpdateCodepageView;
var
  W : TWitchItem;
  SL : TStringList;
  I, Y, TW, TH : Integer;
  S : String;
  TCP : TUTF8ToCodepage;
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then begin
    fCodepageText.Resolution:=Point(1,1);
    fCodepageText.ClrScr;
    Exit;
  end;
  W:=TWitchItem(lvFileList.Selected.Data);
  if not W.Analyzed then begin
    fCodepageText.Resolution:=Point(1,1);
    fCodepageText.ClrScr;
    Exit;
  end else begin
    fCodepageText.BeginUpdate;
    SL := TStringList.Create;
    SL.AddText(PasExt.ToString(W.FileData));
    TW:=Longest(SL);
    if TW < 1 then TW:=1;
    TH:=SL.Count;
    if TH < 1 then TH:=1;
    Inc(TW);
    Inc(TH);
    case W.Encoding of
      weNone : begin
        fCodepageText.Codepage:=-1;
        fCodepageText.Resolution:=Point(TW,TH);
        fCodepageText.ClrScr;
        for Y := 0 to SL.Count - 1 do begin
          fCodepageText.GotoXY(1, Y + 1);
          fCodepageText.WriteCRT(SL[Y]);
        end;
      end;
      weCodepage : begin
{ TODO 9 -cDevel Implement Codepage View for Codepage encoded files. }
        S:=GetTranslation(ComponentNamePath(lvCodepageList, Self, True)
         +'Not_implemented/Caption', 'Not implemented');
        fCodepageText.Codepage:=-1;
        fCodepageText.Resolution:=Point(Length(S),1);
        fCodepageText.ClrScr;
        fCodepageText.WriteCRT(S);
        fCodepageText.WriteError(S);
      end;
      weUnicode : begin
        fCodepageText.Codepage:=-1;
        fCodepageText.Resolution:=Point(TW,TH);
        fCodepageText.ClrScr;
        TCP:=W.AsCodePage(FActiveCodepage);
        Y := 1;
        for I := Low(TCP.Values) to High(TCP.Values) do begin
          if TCP.Chars[I] = Byte(LF) then begin
            Inc(Y);
            fCodepageText.GotoXY(1, Y);
          end else
          if TCP.Values[I] < 0 then
              // if $bf not defined in the Unicode Font, DosCRT will have its
              // own charcter error and use $3f.
              fCodepageText.WriteError($bf)
            else
              fCodepageText.WriteCRT(TCP.Values[I]);
        end;
        FreeAndNil(TCP);
      end;
    end;
    SL.Free;
    fCodepageText.EndUpdate;
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

procedure TfMain.SelectCodepage(Sender: TObject);
var
  E: Integer;
begin
  IgnoreParameter(Sender);
  FActiveCodepage:= 437;
  if Assigned(lvCodepageList.Selected) then begin
    Val(CutDelim(lvCodepageList.Selected.Caption,SPACE,1,1),FActiveCodepage, E);
    if E <> 0 then FActiveCodepage:=437;
  end;
  LogMessage(vbVerbose, 'Selected Codepage ' + IntToStr(FActiveCodepage));
  UpdateStatusBar;
  UpdateCodepageViewlabel;
  UpdateCodePageView;
end;

procedure TfMain.SelectFile(Sender: TObject);
var
  S : String;
begin
  if Assigned(lvFileList.Selected) then
    S:=lvFileList.Selected.Caption
  else
    S:='(null)';
  LogMessage(vbVerbose, 'Select File: ' + S);
  IgnoreParameter(Sender);
  UpdateMetaData;
end;

procedure TfMain.ApplyUserLanguage;
begin
  inherited ApplyUserLanguage;
  UpdateStatusBar;
  UpdateCodepageViewlabel;
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

