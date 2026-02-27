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
  ExtCtrls, ComCtrls, ActnList, Menus, IpHtml, XMLConf,
  Version, PasExt, Icons, MultiApp, LogView, Updater, Preferences,
  DosView, DosFont, Codepages, Witch, uPrefs, uLostFile, uFixEnding,
  uEditor, uDictEdit;

type

  TCodepageFilter = (cpfAll, cpfPartial, cpfComplete);

  { TfMain }

  TfMain = class(TMultiAppForm)
      actExportUnicode: TAction;
      actCloseAll: TAction;
      actExportASCII: TAction;
      actEditASCII: TAction;
      actEditCodepage: TAction;
      actEditUnicode: TAction;
      actExportNone: TAction;
      actEditNone: TAction;
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
      tiFileWatch: TIdleTimer;
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
      spFilesCPs: TSplitter;
      spCPsViewers: TSplitter;
      spUnicodeCP: TSplitter;
      statBar: TStatusBar;
      tbMain: TToolBar;
      ttAnimate: TTimer;
    procedure actCloseAllExecute(Sender: TObject);
    procedure actCloseAllUpdate(Sender: TObject);
    procedure actCodepageFilterExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actEditASCIIExecute(Sender: TObject);
    procedure actEditCodepageExecute(Sender: TObject);
    procedure actEditUnicodeExecute(Sender: TObject);
    procedure actExportASCIIExecute(Sender: TObject);
    procedure actExportCodepageExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actListAllExecute(Sender: TObject);
    procedure actListCompatibleExecute(Sender: TObject);
    procedure actListPartialExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure tiFileWatchTimer(Sender: TObject);
    procedure lvCodepageListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ttAnimateTimer(Sender: TObject);
    private
      FActiveCodepage: integer;
      FViewedCodepage : integer;
      fCodepageFilter: TCodepageFilter;
      lbViewCodepageLabel : TLabel;
      btnEditFile : TToolButton;
      btnExportFile : TToolButton;
      btnCodepageFilter : TToolButton;
      fWitch : TWitch;
      fCodePageText:TDosView;
      fUFF : TUnicodeDosFont;
      fFileReopen:boolean;
      fFileWarn:boolean;
      fUnicodeScale : integer;
      fDOSScale: integer;
      fEndBlankOnInput : boolean;
      fEndBlankOnExport : boolean;
      fOpenExported : boolean;
      fWatchIndex : integer;
      procedure PopulateCodepageList(Item : TWitchItem);
      procedure SetCodepageFilter(AValue: TCodepageFilter);
      procedure SetUnicodeView( S : String );
      function CanExport:boolean;
      function CanEdit:boolean;
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure FileWasEdited(Sender : TObject);
      procedure FirstShow(Sender : TObject);
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
      procedure SessionSave;
      procedure SessionLoad;
      procedure RefreshFileEndsOnBlank;
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

procedure TfMain.actEditASCIIExecute(Sender: TObject);
begin
  if CanEdit then
    FileEditor(TWitchItem(lvFileList.Selected.Data).FileName, @FileWasEdited);
end;

procedure TfMain.actEditCodepageExecute(Sender: TObject);
begin
  if CanEdit then
    FileEditor(TWitchItem(lvFileList.Selected.Data).FileName, @FileWasEdited);
end;

procedure TfMain.actEditUnicodeExecute(Sender: TObject);
begin
  if CanEdit then
    FileEditor(TWitchItem(lvFileList.Selected.Data).FileName, @FileWasEdited);
end;

procedure TfMain.actExportASCIIExecute(Sender: TObject);
begin
  actExportCodepageExecute(Sender);
end;

procedure TfMain.actExportCodepageExecute(Sender: TObject);
var
  W : TWitchItem;
  TCP : TUTF8ToCodepage;
  R : integer;
  N : String;
  D : RawByteString;
  CM, AO : Boolean;
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then
    Exit;
  W:=TWitchItem(lvFileList.Selected.Data);
  TCP:=W.AsCodePage(FActiveCodepage);
  if not Assigned(TCP) then Exit;
  dlgFileSave.InitialDir:=UserWorkPath;
  N := ExcludeTrailing(W.DisplayName, '.UTF-8', false);
  dlgFileSave.FileName:=FileIterative(UserWorkPath + N);
  if fEndBlankOnExport then begin
    D:=NormalizeLineEndings(TCP.Converted, W.LineEndings);
    case W.LineEndings of
      leCRLF : D:=IncludeTrailing(D, CRLF);
      leLF   : D:=IncludeTrailing(D, LF);
      leCR   : D:=IncludeTrailing(D, CR);
    end;
    D:=NormalizeLineEndings(D, W.LineEndings);
  end else begin
    D:=TCP.Converted;
  end;
  CM:=False;
  AO:=False;
  repeat
    if dlgFileSave.Execute then begin
      R:=FileSave(dlgFileSave.FileName, D);
      CM:=True;
      AO:=R=0;
      if R <> 0 then
        if FileErrorDialog(dlgFileSave.FileName, R, True) <> mrRetry then
          R:=0;
    end else
      R:=0;
  until R=0;
  FreeAndNil(TCP);
  if CM then
    FWitch.FileModified(dlgFileSave.FileName);
  if AO and fOpenExported then begin
    OpenFile(dlgFileSave.FileName, false);
    lvFileList.Sort;
  end;
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
  // PreferencesShow;
  if not Assigned(fOptionsDialog) then
    Application.CreateForm(TfOptionsDialog, fOptionsDialog);
  fOptionsDialog.Show;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  OnFirstShow:=@FirstShow;
  FActiveCodepage := 437;
  FViewedCodePage :=-2;
  FUnicodeScale:=100;
  FDOSScale:=1;
  fEndBlankOnInput:=True;
  fEndBlankOnExport:=True;
  fOpenExported:=False;
  fWatchIndex:=0;

  fWitch := TWitch.Create;
  fWitch.OnAnalyzed:=@WitchOnAnalyzed;

  fCodepageText := TDosView.Create(Self);
  fCodepageText.Parent:=pCodepage;
  fCodepageText.Align:=alClient;

  fCodepageText.Color:=clWindow;
  fCodepageText.Foreground := clWindowText;
  fCodepageText.Background := clWindow;
  fCodepageText.ErrorForeground := clRed;
  fCodepageText.ErrorBackground := clBlack;
  fCodepageText.ControlCodes:=False;

  fUFF:=TUnicodeDosFont.Create;
  if fUFF.LoadFromFile(AppDataPath + '0816norm.uff') = 0 then begin
    fCodepageText.Font:=fUFF;
    fCodepageText.ErrorChar:=$bf;
  end else
    FreeAndNil(fUFF);
  OnSettingsLoad:=@FormSettingsLoad;
  OnSettingsSave:=@FormSettingsSave;

  SetApplicationIcons;

  // Assign Images to Actions
  actOpen.ImageIndex:=idxButtonFileOpen;
  actExportUnicode.ImageIndex:=idxButtonFileExport;
  actExportCodepage.ImageIndex:=idxButtonFileExportGreen;
  actExportASCII.ImageIndex:=idxButtonFileExportGray;
  actExportNone.ImageIndex:=idxButtonFileExportRed;
  actEditUnicode.ImageIndex:=idxButtonFileEditGreen;
  actEditCodepage.ImageIndex:=idxButtonFileEdit;
  actEditASCII.ImageIndex:=idxButtonFileEditGray;
  actEditNone.ImageIndex:=idxButtonFileEditRed;
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
  CreateToolButton(tbMain, tbsDivider, 'btnDivider0');
  btnEditFile:=CreateToolButton(tbMain, actEditCodepage);
  CreateToolButton(tbMain, tbsDivider, 'btnDivider1');
  btnExportFile:=CreateToolButton(tbMain, actExportCodepage);
  CreateToolButton(tbMain, actClose);
  CreateToolButton(tbMain, actCloseAll);
  CreateToolButton(tbMain, tbsDivider, 'btnDivider2');
  btnCodepageFilter:=CreateToolButton(tbMain, actCodepageFilter);
  btnCodepageFilter.Style:=tbsButtonDrop;
  btnCodepageFilter.DropdownMenu:=pmListMode;
  CreateToolButton(tbMain, tbsDivider, 'btnDivider3');
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

procedure TfMain.tiFileWatchTimer(Sender: TObject);
var
  I : integer;
begin
  if (not Assigned(FWitch)) or (FWitch.Count= 0) then Exit;
  I:=50;
  while I > 0 do begin
    if FWatchIndex >= FWitch.Count then begin
      FWatchIndex:=0;
      Break;
    end;
    FWitch.Modified[FWatchIndex]; // Ignore Result
    Inc(FWatchIndex);
  end;
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

procedure TfMain.ttAnimateTimer(Sender: TObject);
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
    ttAnimate.Enabled:=False;
    lvCodepageList.Enabled:=True;
    lvCodepageList.SmallImages:=ilCompatibleColor;
    K:=ComponentNamePath(lvCodepageList, Self, True);
    case Item.Encoding of
      weNone : begin
        // Only Liwer 7-Bit ASCII characters, compatible with any Codepage
        L:=lvCodepageList.Items.Add;
        L.Caption:=GetTranslation(K+'Any_Codepage/Caption', 'Any Codepage');
        L.ImageIndex:=High(iconCompatibleNames);
      end;
      weBinary : begin
         // Binary Data FIle , not supported
         lvCodepageList.SmallImages:=ilGeneralColor;
         L:=lvCodepageList.Items.Add;
         L.Caption:=GetTranslation(K+'Binary_data/Caption', 'Binary File');
         L.ImageIndex:=idxGeneralError;
       end;
      weUnicode : begin
        // UTF-8/Unicode encoded file
        for I := 0 to High(Item.Results) do begin
          P := High(iconCompatibleNames) * Item.Results[I].Compatible div 100;
          if (P = 0) and (Item.Results[I].Compatible <> 0) then
            P := 1
          else if (P = High(iconCompatibleNames)) and (Item.Results[I].Compatible <> 100) then
            P := High(iconCompatibleNames) - 1;

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
    ttAnimate.Enabled:=True;
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
    'color:' + F + '; background-color:' + B + '; '+ 'margin:0; ">' + CR+
    '<div style="font-family: monospace; font-weight:light; font-size:' +
    IntToStr(FUnicodeScale) + '%;" >' + CR +
    S + '</div>' +
    '<br></body></html>';
  hpUnicodeText.SetHtmlFromStr(''); // Clear Cache, apparently this may be needed sometimes.
  hpUnicodeText.SetHtmlFromStr(S);
  // FileSave(AppBasePath + '/test.html',PasExt.ToBytes(S));
end;

function TfMain.CanExport: boolean;
begin
  Result:=False;
  if Not Assigned(lvFileList.Selected) then Exit;
  if Not Assigned(lvFileList.Selected.Data) then Exit;
  if Not TWitchItem(lvFileList.Selected.Data).Analyzed then Exit;
  case TWitchItem(lvFileList.Selected.Data).Encoding of
    weBinary : Exit;
    weNone : Result:=True;
    weCodepage : Result:=False;
    weUnicode : Result:=Assigned(lvCodepageList.Selected);
  end;
end;

function TfMain.CanEdit: boolean;
begin
  Result:=False;
  if Not Assigned(lvFileList.Selected) then Exit;
  if Not Assigned(lvFileList.Selected.Data) then Exit;
  if Not TWitchItem(lvFileList.Selected.Data).Analyzed then Exit;
  case TWitchItem(lvFileList.Selected.Data).Encoding of
    weBinary : Exit;
    weNone : Result:=True;
    weCodepage : Result:=False;
    weUnicode : Result:=True;
  end;

end;

procedure TfMain.FormSettingsLoad(Sender: TObject);
var
  S : String;
  V, E : Integer;
  B : Boolean;
begin
  S:=Trim(Lowercase(GetConfig('Codepage_List/Filter', 'partial')));
  case S of
    'complete' : FCodepageFilter:=cpfComplete;
    'partial' : FCodepageFilter:=cpfPartial;
  else
    FCodepageFilter:=cpfAll;
  end;
  Val(RawByteString(UserConfig.GetValue(
    'Preferences/tsViewer/tbUnicodeScale/Value',
    UnicodeString(IntToStr(FUnicodeScale div 10)))),
    V, E);
  if (E = 0) and (V>1) and (V<101) and (FUnicodeScale <> V*10) then begin
    FUnicodeScale:=V*10;
    // RescaleUnicodeView;
  end;
  Val(RawByteString(UserConfig.GetValue(
    'Preferences/tsViewer/tbDOSScale/Value',
    UnicodeString(IntToStr(FDOSScale)))),
    V, E);
  if (E = 0) and (V>0) and (V<11) then begin
    FDOSScale:=V;
    fCodepageText.Scale:=Point(V,V);
  end;

  UpdateFilterCheck;
  SetApplicationIcons;
  { TODO 5 -cDevel Add load color settings for Good/Bad mappings }

  // Values set and managed through OptionsDialog preferences.
  fFileReopen:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbReopenFiles/State', 'Unchecked')) = cbChecked;
  fFileWarn:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbWarnMissing/State', 'Unchecked')) = cbChecked;
  fOpenExported:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbOpenExport/State', 'Unchecked')) = cbChecked;
  B:=StrToBool(UserConfig.GetValue(
    'Preferences/tsEncoding/rbFileEndAll/Checked', ''), fEndBlankOnInput);
  fEndBlankOnExport:=B or StrToBool(UserConfig.GetValue(
    'Preferences/tsEncoding/rbFileEndExport/Checked', ''), fEndBlankOnExport);
  if B <> fEndBlankOnInput then begin
    fEndBlankOnInput:=B;
    RefreshFileEndsOnBlank;
  end;
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
  { TODO 5 -cDevel Add save color settings for Good/Bad mappings }
  SessionSave;
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
      if fEndBlankOnInput and (not W.EndsWithBlank) then
        W.ListItem.ImageIndex:=idxFileTypeFilePlainYellow
      else
        W.ListItem.ImageIndex:=idxFileTypeFilePlainGray;
    end;
    weBinary : begin
      Cat(M, 'Binary');
      W.ListItem.ImageIndex:=idxFileTypeFilePlainRed;
    end;
    weCodepage : begin
      Cat(M, 'Codepage');
      if fEndBlankOnInput and (not W.EndsWithBlank) then
        W.ListItem.ImageIndex:=idxFileTypeFilePlainYellow
      else
        W.ListItem.ImageIndex:=idxFileTypeFilePlainBlue;
    end;
    weUnicode : begin
      Cat(M, 'Unicode');
      if fEndBlankOnInput and (not W.EndsWithBlank) then
        W.ListItem.ImageIndex:=idxFileTypeFilePlainYellow
      else
        W.ListItem.ImageIndex:=idxFileTypeFilePlainGreen;
    end;
  end;
  Cat(M, ' file "'+W.DisplayName+'"');
  LogMessage(vbVerbose, M);

  if W.ListItem = lvFileList.Selected then begin
    UpdateMetaData;
    SelectFile(Self);
  end;
end;

procedure TfMain.FileWasEdited(Sender: TObject);
var
  I : Integer;
begin
  if not Assigned(fWitch) then Exit;
  if Sender is TEditorForm then begin
    I:=fWitch.Find(TEditorForm(Sender).FileName);
    if I >= 0 then
      fWitch.Modified[I]:=True
    else if fOpenExported then begin
      OpenFile(TEditorForm(Sender).FileName,false);
      lvFileList.Sort;
    end;
  end;
end;

procedure TfMain.FirstShow(Sender: TObject);
begin
  // LogMessage(vbNormal, 'First Show: ' + BoolStr(fFileReopen));
  SessionLoad;
end;

procedure TfMain.SetApplicationIcons;
begin
  tbMain.Images:=IconTheme.ButtonEnabled;
  // tbMain.DisabledImages:=IconTheme.ButtonDisabled;
  tbMain.DisabledImages:=ilButtonDisabled;
  tbMain.HotImages:=IconTheme.ButtonHover;
  pmListMode.Images:=IconTheme.ButtonEnabled;

  lvFileList.SmallImages:=ilFileTypeColor;
  lvCodepageList.SmallImages:=ilCompatibleColor;
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
  spiLineEndings = 1;            // displayed when file is selected
  spiCompatiblity = 2;           // displayed when Codepage is selected
  // spiLanguage = 3;            // not yet implemented
  // spiPrefered = 4;            // not yet implemented
  spiFileName = 3;               // displayed when file is selected
var
  W : TWitchItem;
  K : String;
  I, V, E : integer;
  Compat : String;
begin
  if not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data))then begin
    statBar.Panels[spiEncoding].Text:='';
    statBar.Panels[spiLineEndings].Text:='';
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
    if W.Encoding = weBinary then
      statBar.Panels[spiLineEndings].Text:=''
    else case W.LineEndings of
      leCRLF : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/CRLF', 'CRLF');
      leLF : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/LF', 'LF');
      leCR : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/CR', 'CR');
    end;
    case W.Encoding of
      weNone : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'NoEncoding/Caption', 'ASCII');
      weBinary : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Binary/Caption', 'Binary');
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
        weBinary : begin
         H:= GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
          +'Unsupported_file/Binary/Text', 'Binary data files are not supported.');
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
begin
  if Not (Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data)) then begin
    FViewedCodepage:=-2;
    fCodepageText.Clear;
    Exit;
  end;
  W:=TWitchItem(lvFileList.Selected.Data);
  if not W.Analyzed then begin
    FViewedCodepage:=-2;
    fCodepageText.Clear;
    Exit;
  end else begin
    if FViewedCodePage=FActiveCodePage then Exit;
    FViewedCodePage:=FActiveCodepage;
    fCodepageText.BeginUpdate;
    fCodePageText.Clear;
    case W.Encoding of
      weNone : begin
        LogMessage(vbVerbose, 'ASCII Item: ' + W.DisplayName + ' (Any Codepage, using ' +
         IntToStr(FActiveCodepage) + ')');
        fCodepageText.Codepage:=FActiveCodepage;
        fCodepageText.AddText(PasExt.ToString(W.FileData));
      end;
      weBinary : begin
        fCodepageText.Codepage:=-1;
        LogMessage(vbVerbose, 'Binary Item: ' + W.DisplayName + ' (Not supported)');
        fCodepageText.AddError(GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
          +'Unsupported_file/Binary/Text', 'Binary data files are not supported.'));
       end;
      weCodepage : begin
        fCodepageText.Codepage:=-1;
        LogMessage(vbVerbose, 'Codepage Item: ' + W.DisplayName + ' (Codepage ' +
          IntToStr(FActiveCodepage) + ')');
  { TODO 9 -cDevel Implement Codepage View for Codepage encoded files. }

        fCodepageText.AddError(
        GetTranslation(ComponentNamePath(lvCodepageList, Self, True)
          +'Not_implemented/Caption', 'Not implemented'));
      end;
      weUnicode : begin
        LogMessage(vbVerbose, 'Unicode Item: ' + W.DisplayName + ' (Codepage ' +
          IntToStr(FActiveCodepage) + ')');
        fCodepageText.Codepage:=FActiveCodepage;
        fCodepageText.AddText(PasExt.ToString(W.FileData));
      end;
    end;
    fCodepageText.EndUpdate;
  end;
end;

procedure TfMain.UpdateButtons;
var
  E : TWitchEncoding;
begin
  if Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) then begin
    actClose.Enabled:=TWitchItem(lvFileList.Selected.Data).Analyzed;
    E:=TWitchItem(lvFileList.Selected.Data).Encoding;
  end else begin
    actClose.Enabled:=False;
    E:=weBinary;
  end;
  case E of
    weNone : begin
      actClose.ImageIndex:=idxButtonFileCloseGray;
      actExportASCII.Enabled:=CanExport;
      btnExportFile.Action:=actExportASCII;
      actEditASCII.Enabled:=CanEdit;
      btnEditFile.Action:=actEditASCII;
    end;
    weCodepage : begin
      actClose.ImageIndex:=idxButtonFileClose;
      actExportUnicode.Enabled:=CanExport;
      btnExportFile.Action:=actExportUnicode;
      actEditCodepage.Enabled:=CanEdit;
      btnEditFile.Action:=actEditCodepage;
    end;
    weUnicode: begin
      actClose.ImageIndex:=idxButtonFileCloseGreen;
      actExportCodepage.Enabled:=CanExport;
      btnExportFile.Action:=actExportCodepage;
      actEditUnicode.Enabled:=CanEdit;
      btnEditFile.Action:=actEditUnicode;
    end;
  else
    actClose.ImageIndex:=idxButtonFileCloseRed;
    btnExportFile.Action:=actExportNone;
    btnEditFile.Action:=actEditNone;
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
    if E <> 0 then begin
      FActiveCodepage:=437;
    end;
  end;
  LogMessage(vbVerbose, 'Selected Codepage ' + IntToStr(FActiveCodepage));
  UpdateStatusBar;
  UpdateCodepageViewlabel;
  UpdateCodePageView;
end;

procedure TfMain.SelectFile(Sender: TObject);
const
  SuspendCheck : boolean = false;
var
  S : String;
  W : TWitchItem;
begin
  FViewedCodepage:=-2;
  if Assigned(FDictEditForm) then begin
    FDictEditForm.WitchItem:=nil;
    {$IFDEF BUILD_SPECIAL}
       if fDictEditForm.Visible = False then
         fDictEditForm.Show;
    {$ENDIF}
  end;
  if Assigned(lvFileList.Selected) then begin
    S:=lvFileList.Selected.Caption;
    if Assigned(FDictEditForm) then
      FDictEditForm.WitchItem:=TWitchItem(lvFileList.Selected.Data);
  end else
    S:='(null)';
  LogMessage(vbVerbose, 'Select File: ' + S);
  IgnoreParameter(Sender);
  UpdateMetaData;
  if SuspendCheck then Exit;
  if fEndBlankOnInput and Assigned(lvFileList.Selected) then begin
    W:=TWitchItem(lvFileList.Selected.Data);
    if W.Encoding = weBinary then Exit;
    if Assigned(W) and (W.Analyzed) and (W.EndsWithBlank = false) then begin
      S:=W.FileName;
      if FixFileLineEnding(S) = mrOK then begin
        SuspendCheck:=True;
        actCloseExecute(Self);
        SuspendCheck:=False;
        OpenFile(S, True);
      end;
    end;
  end;
end;

procedure TfMain.SessionSave;
var
  XML : TXMLConfig;
  I, X : integer;
begin
  if not fFileReopen then Exit;
  XML:=TXMLConfig.Create(nil);
  try
    XML.SetValue('Files/Count', UnicodeString(IntToStr(fWitch.Count)));
    X:=-1;
    if Assigned(lvFileList.Selected) and Assigned(lvFileList.Selected.Data) then begin
      X:=TWitchItem(lvFileList.Selected.Data).Index;
    end;
    XML.SetValue('Files/Selected', UnicodeString(IntToStr(X)));
    for I := 0 to fWitch.Count - 1 do
      XML.SetValue(UnicodeString('Files/File_' + IntToStr(I)) + '/Name',
        UnicodeString(fWitch.Items[I].FileName));
    XML.SaveToFile(UserDataPath + 'session.xml');
  finally
    XML.Free;
  end;
end;

procedure TfMain.SessionLoad;
var
  XML : TXMLConfig;
  MFL : TStringList;
  C, I, X : integer;
  S, F : String;

begin
  if not fFileReopen then Exit;
  if not FileExists(UserDataPath + 'session.xml') then Exit;
  LogMessage(vbNormal,'Reopen previous session files.');
  if fFileWarn then
    MFL := TStringList.Create
  else
    MFL := nil;
  F:='';
  XML:=TXMLConfig.Create(nil);
  try
    XML.LoadFromFile(UserDataPath + 'session.xml');
    C:=StrToInt(RawByteString(XML.GetValue('Files/Count','0')));
    X:=StrToInt(RawByteString(XML.GetValue('Files/Selected', '-1')));
    for I := 0 to C - 1 do begin
      S:=Trim(RawByteString(XML.GetValue(UnicodeString('Files/File_' + IntToStr(I)) +
        '/Name', '')));
      if S = '' then Continue;
      if FileExists(S) then begin
        if I=X then F:=S;
        OpenFile(S, False);
      end
      else if Assigned(MFL) then begin
        MFL.Add(S);
        LogMessage(vbNormal, 'Unable to locate previously open file: ' + S);
      end;
    end;
  except
    FreeAndNil(XML);
  end;
  if F <> '' then
    OpenFile(F, True);
  if Assigned(XML) then
    FreeAndNil(XML);
  if Assigned(MFL) then begin
    if MFL.Count <> 0 then
      ShowMissingFiles(MFL);
    FreeAndNil(MFL);
  end;

end;

procedure TfMain.RefreshFileEndsOnBlank;
var
  I : Integer;
begin
  for I := 0 to fWitch.Count - 1 do
    if fWitch.Items[I].Analyzed then
      WitchOnAnalyzed(fWitch.Items[I]);
  SelectFile(Self);
end;

procedure TfMain.ApplyUserLanguage;
begin
  inherited ApplyUserLanguage;
  { UpdateStatusBar;
  UpdateCodepageViewlabel; }
  UpdateMetaData;
end;

procedure TfMain.OpenFile(FileName: String; Select: boolean);
var
  I : integer;
  F : TStringList;
  LI : TListItem;
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

  if Not FileExists(FileName) then Exit;
  UserWorkPath:=IncludeTrailingPathDelimiter(ExtractFilePath(FileName));

  I:=fWitch.Find(FileName);
  if I <> -1 then begin
    LogMessage(vbVerbose, 'Open file "' + FileName + '" already open.');
    fWitch.Modified[I];
  end else begin
    try
      LI:=lvFileList.Items.Add;
      I := fWitch.Add(FileName, LI);
    except
      LI.Delete;
      LogMessage(vbVerbose, 'Open file "' + FileName + '" Failed!');
      Exit;
    end;
    LogMessage(vbVerbose, 'Opened file "' + FileName + '"');
  end;

  if Select then begin
    lvFileList.Sort;
    fWitch.Select(I);
    fWitch.Items[I].ListItem.MakeVisible(False);
    lvFileList.SetFocus;
    UpdateMetaData;
  end;

end;


end.

