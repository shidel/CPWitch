{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uMain;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.


{$DEFINE STICKYLISTITEMS}

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus, IpHtml, XMLConf,
  Version, PasExt, Icons, MultiApp, LogView, Updater, Preferences,
  DosView, DosFont, Codepages, Witch, uPrefs, uLostFile, uFixEnding,
  uEditor, uDictEdit, Dictionary;

type

  TCodepageFilter = (cpfAll, cpfPartial, cpfComplete);
  TLocaleItem = record
    ID : String;
    Name : RawByteString;
  end;
  TLocaleItems = array of TLocaleItem;

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
      actDictionary: TAction;
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
      hpUnicodeText: TIpHtmlPanel;
      pTop: TPanel;
      pBody: TPanel;
      spFileCP: TSplitter;
      spCPViewers: TSplitter;
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
      spUnicodeCP: TSplitter;
      statBar: TStatusBar;
      tbMain: TToolBar;
      ttAnimate: TTimer;
    procedure actCloseAllExecute(Sender: TObject);
    procedure actCodepageFilterExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDictionaryExecute(Sender: TObject);
    procedure actEditASCIIExecute(Sender: TObject);
    procedure actEditCodepageExecute(Sender: TObject);
    procedure actEditUnicodeExecute(Sender: TObject);
    procedure actExportASCIIExecute(Sender: TObject);
    procedure actExportCodepageExecute(Sender: TObject);
    procedure actExportUnicodeExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actListAllExecute(Sender: TObject);
    procedure actListCompatibleExecute(Sender: TObject);
    procedure actListPartialExecute(Sender: TObject);
    procedure actOnlineUpdateExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure tiFileWatchTimer(Sender: TObject);
    procedure lvCodepageListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ttAnimateTimer(Sender: TObject);
    private
      { Witch and codepage related variables }
      fWitch : TWitch;
      fWitchItem : TWitchItem;
      fLastWitch : TWitchItem;
      fCodepage : integer;
      { User interface Controls }
      lbViewCodepageLabel : TLabel;
      lbUnicodeLanguage : TLabel;
      lbCodepageLanguage : TLabel;
      btnEditFile : TToolButton;
      btnExportFile : TToolButton;
      btnCodepageFilter : TToolButton;
      fCodePageText:TDosView;
      fUFF : TUnicodeDosFont;
      { User Application Settings }
      fCodepageFilter: TCodepageFilter;
      fFileReopen:boolean;
      fFileWarn:boolean;
      fUnicodeScale : integer;
      fDOSScale: integer;
      fEndBlankOnInput : boolean;
      fEndBlankOnExport : boolean;
      fOpenExported : boolean;
      fWatchIndex : integer;
      fLocales : TLocaleItems;
      fAutoSelectCP : boolean;
      fSingleViewer : boolean;
      fUseExternalEditor : boolean;
      fExternalEditor : String;
      { More Lazarus UI Glitch fixing!! }
      {$IFDEF STICKYLISTITEMS}
        fLastFileItem : TListItem;
        fLastCodeItem : TListItem;
      {$ENDIF}
      procedure SetCodepageFilter(AValue: TCodepageFilter);
      procedure SetUnicodeView( S : String );
      function CanExport:boolean;
      function CanEdit:boolean;
      function GetWitchErrorMessage : String;
    protected
      procedure FormSettingsLoad(Sender: TObject);
      procedure FormSettingsSave(Sender: TObject);
      procedure WitchOnAnalyzed(Sender : TObject);
      procedure EditFile(Sender : TObject);
      procedure FileWasEdited(Sender : TObject);
      procedure FirstShow(Sender : TObject);
      procedure SetApplicationIcons;
      procedure UpdateCodepageList;
      procedure UpdateCodepageViewLabel;
      procedure UpdateMetaData;
      procedure UpdateStatusBarFileName;
      procedure UpdateStatusBar;
      procedure UpdateUnicodeView;
      procedure UpdateCodepageView;
      procedure UpdateButtons;
      procedure UpdateFilterCheck;
      procedure UpdateLocale;
      procedure UpdateLocaleList;
      procedure UpdateDictionary;
      procedure SelectCodepageListItem;
      procedure SelectCodepage(Value : Integer);
      procedure SelectFile(Item : TWitchItem);
      procedure SessionSave;
      procedure SessionLoad;
      procedure RefreshFileStatus;
      procedure EnforceLayout; override;
      procedure SetSaveDialogText(Encoding : TWitchEncoding);
      procedure ReAnalyzeFiles;
    public
      procedure ApplyUserLanguage; override;
      procedure CloseAllFiles;
      procedure CloseFile;
      procedure OpenFile(FileName : String; Select : boolean = False); overload;
    published
     property CodepageFilter : TCodepageFilter read FCodepageFilter write SetCodepageFilter;
  end;

var
  fMain: TfMain;

implementation

{$IFDEF Darwin}
uses Process;
{$ENDIF}

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

procedure TfMain.actCloseAllExecute(Sender: TObject);
begin
  CloseAllFiles;
end;

procedure TfMain.actCloseExecute(Sender: TObject);
begin
  CloseFile;
end;

procedure TfMain.actDictionaryExecute(Sender: TObject);
begin
  if Assigned(FDictEditForm)  then begin
    fDictEditForm.Show;
    fDictEditForm.BringToFront;
    fDictEditForm.SetFocus;
    FDictEditForm.WitchItem:=nil;
    FDictEditForm.WitchList:=FWitch;
    UpdateDictionary;
  end;
end;

procedure TfMain.actEditASCIIExecute(Sender: TObject);
begin
  EditFile(Sender);
end;

procedure TfMain.actEditCodepageExecute(Sender: TObject);
begin
  EditFile(Sender);
end;

procedure TfMain.actEditUnicodeExecute(Sender: TObject);
begin
  EditFile(Sender);
end;

procedure TfMain.actExportASCIIExecute(Sender: TObject);
begin
  actExportCodepageExecute(Sender);
end;

procedure TfMain.actExportCodepageExecute(Sender: TObject);
var
  TCP : TUTF8ToCodepage;
  R : integer;
  N : String;
  D : RawByteString;
  CM, AO : Boolean;
begin
  if Not Assigned(fWitchItem) then Exit;
  if fCodepage < 0 then Exit;
  TCP:=fWitchItem.AsCodePage(fCodepage);
  if not Assigned(TCP) then Exit;
  dlgFileSave.InitialDir:=UserWorkPath;
  N := ExcludeTrailing(fWitchItem.DisplayName, '.UTF-8', false);
  dlgFileSave.FileName:=FileIterative(UserWorkPath + N);
  SetSaveDialogText(fWitchItem.Encoding);
  if fEndBlankOnExport then begin
    D:=NormalizeLineEndings(TCP.Converted, fWitchItem.LineEndings);
    case fWitchItem.LineEndings of
      leCRLF : D:=IncludeTrailing(D, CRLF);
      leLF   : D:=IncludeTrailing(D, LF);
      leCR   : D:=IncludeTrailing(D, CR);
    end;
    D:=NormalizeLineEndings(D, fWitchItem.LineEndings);
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
    fWitch.FileModified(dlgFileSave.FileName);
  if AO and fOpenExported then
    OpenFile(dlgFileSave.FileName, false);
end;

procedure TfMain.actExportUnicodeExecute(Sender: TObject);
var
  TUC : TCodepageToUTF8;
  R : integer;
  N : String;
  D : RawByteString;
  CM, AO : Boolean;
begin
  if Not Assigned(fWitchItem) then Exit;
  if fCodepage < 0 then Exit;
  TUC:=fWitchItem.AsUnicode(fCodepage);
  if not Assigned(TUC) then Exit;
  dlgFileSave.InitialDir:=UserWorkPath;
  N := IncludeTrailing(fWitchItem.DisplayName, '.UTF-8', false);
  dlgFileSave.FileName:=FileIterative(UserWorkPath + N);
  SetSaveDialogText(fWitchItem.Encoding);
  if fEndBlankOnExport then begin
    D:=NormalizeLineEndings(RawByteString(TUC.Converted), fWitchItem.LineEndings);
    case fWitchItem.LineEndings of
      leCRLF : D:=IncludeTrailing(D, CRLF);
      leLF   : D:=IncludeTrailing(D, LF);
      leCR   : D:=IncludeTrailing(D, CR);
    end;
    D:=NormalizeLineEndings(D, fWitchItem.LineEndings);
  end else begin
    D:=RawByteString(TUC.Converted);
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
  FreeAndNil(TUC);
  if CM then
    FWitch.FileModified(dlgFileSave.FileName);
  if AO and fOpenExported then
    OpenFile(dlgFileSave.FileName, false);
end;

procedure TfMain.actOpenExecute(Sender: TObject);
var
  I : integer;
begin
  dlgOpenFile.InitialDir:=UserWorkPath;
  { TODO 0 -cLazarus_Bug Setting Title or Filter for TOpenDialog does nothing on macOS }
  dlgOpenFile.Title:=GetTranslation('OpenFilesDialog/Title/Caption', 'Open existing file(s)');
  dlgOpenFile.Filter:=GetTranslation('OpenFilesDialog/File/Filters', 'All Files (*.*)|*.*|Unicode Files (*.UTF-8)|*.UTF-8');
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
  UpdateCheck(True); // True means not a background check and show dialog.
end;

procedure TfMain.actPreferencesExecute(Sender: TObject);
begin
  FormSettingsSave(Self);
  if not Assigned(fOptionsDialog) then
    Application.CreateForm(TfOptionsDialog, fOptionsDialog);
  fOptionsDialog.Show;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  {$IFDEF STICKYLISTITEMS}
  fLastFileItem:=nil;
  fLastCodeItem:=nil;
  {$ENDIF}
  fWitchItem:=nil;
  fLastWitch:=nil;
  fCodepage:=-1;
  OnFirstShow:=@FirstShow;
  fUnicodeScale:=100;
  fDOSScale:=1;
  fEndBlankOnInput:=True;
  fEndBlankOnExport:=True;
  fOpenExported:=False;
  fAutoSelectCP:=True;
  fWatchIndex:=0;
  fLocales:=[];
  fSingleViewer:=False;
  fUseExternalEditor:=False;
  fExternalEditor:='';

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
  actDictionary.ImageIndex:=idxButtonDictionary;

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
  CreateToolButton(tbMain, actDictionary);
  CreateToolButton(tbMain, tbsDivider, 'btnDivider4');
  CreateToolButton(tbMain, actPreferences);
  CreateToolButton(tbMain, actOnlineUpdate);
  CreateToolButton(tbMain, actDebugLog);

  // Make controls "flat", UI looks weird without doing thins on Windows and
  // Linux. Has no effect on macOS.
  FlattenControl(pFileList);
  FlattenControl(pCodepageList);
  FlattenControl(pViewUnicode);
  FlattenControl(pCodepage);
  FlattenControl(pTop);
  FlattenControl(pBody, false);

  // Disable HideSelection for Windows. Or, when selecting things like the
  // codepage, The current File Selected becomes no longer highlighted.
  lvFileList.HideSelection:=False;
  lvCodepageList.HideSelection:=False;

  // Make toolbar the width of the visible buttons, not needed without controlbar
  // AdjustToolBarWidth(tbMain);

  // Controls that are Created at runtime, without a Name so they will not
  // save their Caption properties at runtime during their NLS generation
  // when the BUILD_PRERELEASE version flag is set.

  // Displays "Viewed as Codepage NNN"
  lbViewCodepageLabel :=TLabel.Create(Self);
  lbViewCodepageLabel.Parent:=pViewCodepageLabel;
  lbViewCodepageLabel.Align:=alLeft;
  lbViewCodepageLabel.AutoSize:=True;
  lbViewCodepageLabel.BorderSpacing.Around:=6;

  // Displays language/local in Codepage pane like "German (Germany)"
  lbCodepageLanguage :=TLabel.Create(Self);
  lbCodepageLanguage.Parent:=pViewCodepageLabel;
  lbCodepageLanguage.Align:=alRight;
  lbCodepageLanguage.AutoSize:=True;
  lbCodepageLanguage.BorderSpacing.Around:=6;
  // lbCodepageLanguage.Caption:='Language';

  // Displays language/local in Unicode pane like "German (Germany)"
  lbUnicodeLanguage :=TLabel.Create(Self);
  lbUnicodeLanguage.Parent:=pViewUnicodeLabel;
  lbUnicodeLanguage.Align:=alRight;
  lbUnicodeLanguage.AutoSize:=True;
  lbUnicodeLanguage.BorderSpacing.Around:=6;
  // lbUnicodeLanguage.Caption:='Language';

  SetUnicodeView('');

  EnforceLayout;
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
  lvFileList.Items.BeginUpdate;
  try
  for I := High(FileNames) downto 0 do
    OpenFile(FileNames[I], I=0);

  finally
    lvFileList.Items.EndUpdate;
    lvFileList.Sort;
  end;
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  UpdateStatusBarFileName;
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
const
  Locked : Boolean = false;
var
  V, E : integer;
begin
  if Locked then Exit;
  Locked:=True;
  try
    IgnoreParameter(Sender);
    if Selected then begin
      {$IFDEF STICKYLISTITEMS}
      {$if defined(windows)}
      if Assigned(fLastCodeItem) and (fLastCodeItem <> Item) then begin
        fLastCodeItem.Selected := False;
        fLastCodeItem.Focused := False;
      end;
      {$endif}

      fLastCodeItem := Item;
      {$ENDIF}
      if Assigned(Item) then begin
        Val(Item.Caption, V, E);
        if E <> 0 then begin
          if fCodepage<>437 then SelectCodepage(-1);
        end else
        if fCodepage <> V then
          SelectCodepage(V);
      end;
    end else
    if not Assigned(lvCodepageList.Selected) then begin
      SelectCodepageListItem;
      {$IFDEF STICKYLISTITEMS}
      fLastCodeItem:=lvCodepageList.Selected;
      {$ENDIF}

    end;
  finally
    Locked:=False;
  end;
end;

procedure TfMain.lvFileListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
const
  Locked : Boolean = false;
begin
  if Locked then Exit;
  Locked:=True;
  try
    IgnoreParameter(Sender);
    if Selected then begin
      {$IFDEF STICKYLISTITEMS}
      {$if defined(windows)}
      if Assigned(fLastFileItem) and (fLastFileItem <> Item) then begin
        fLastFileItem.Selected := False;
        fLastFileItem.Focused := False;
      end;
      {$endif}

      fLastFileItem := Item;
      {$ENDIF}
      if Assigned(Item) and Assigned(Item.Data) then
        SelectFile(TWitchItem(Item.Data));
    end else
    if not Assigned(lvFileList.Selected) then
      if Assigned(fWitchItem) and Assigned(fWitchItem.ListItem) then begin
        fWitchItem.ListItem.Selected := True;
        fWitchItem.ListItem.Focused := True;
        {$IFDEF STICKYLISTITEMS}
        fLastFileItem:=fWitchItem.ListItem;
        {$ENDIF}
      end;
  finally
    Locked:=False;
  end;
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

procedure TfMain.UpdateCodepageList;
const
  Locked : boolean = false;
var
  L : TListItem;
  K : String;
  I, P : integer;
begin
  if Locked then Exit;
  Locked:=True;
  try
    if Assigned(fWitchItem) then begin
      if (fWitchItem = fLastWitch) and (lvCodepageList.Items.Count > 0) then begin
        SelectCodepageListItem;
        Locked:=False;
        Exit;
      end;
    end;

    {$IFDEF STICKYLISTITEMS}
    fLastCodeItem:=nil;
    {$ENDIF}

    lvCodepageList.BeginUpdate;
    lvCodepageList.Clear;
    if Assigned(fWitchItem) then begin
      if fWitchItem.Analyzed then begin
        // Processing complete
        ttAnimate.Enabled:=False;
        K:=ComponentNamePath(lvCodepageList, Self, True);
        case fWitchItem.Encoding of
          weNone : begin
            lvCodepageList.SmallImages:=ilCompatGreenColor;
            // Only Liwer 7-Bit ASCII characters, compatible with any Codepage
            L:=lvCodepageList.Items.Add;
            L.Caption:=GetTranslation(K+'Any_Codepage/Caption', 'Any Codepage');
            L.ImageIndex:=High(iconCompatGreenNames);
          end;
          weError : begin
              // Binary Data FIle , not supported
              lvCodepageList.SmallImages:=ilGeneralColor;
              L:=lvCodepageList.Items.Add;
              L.Caption:=GetTranslation(K+'Error/Caption', 'Error');
              L.ImageIndex:=idxGeneralError;
            end;
          weBinary : begin
             // Binary Data FIle , not supported
             lvCodepageList.SmallImages:=ilGeneralColor;
             L:=lvCodepageList.Items.Add;
             L.Caption:=GetTranslation(K+'Binary_data/Caption', 'Binary File');
             L.ImageIndex:=idxGeneralError;
           end;
          weUnicode, weCodepage : begin
            if fWitchItem.Encoding = weUnicode then
              lvCodepageList.SmallImages:=ilCompatGreenColor
            else
              lvCodepageList.SmallImages:=ilCompatBlueColor;
            // UTF-8/Unicode encoded file
            for I := 0 to High(fWitchItem.Results) do begin
              P := High(iconCompatGreenNames) * fWitchItem.Results[I].Compatible div 100;
              if (P = 0) and (fWitchItem.Results[I].Compatible <> 0) then
                P := 1
              else if (P = High(iconCompatGreenNames)) and (fWitchItem.Results[I].Compatible <> 100) then
                P := High(iconCompatGreenNames) - 1;

              // Always ensure Preferred Codepage are in list regardless of filter
              if (fWitchItem.Results[I].Codepage <> fWitchItem.Preferred) then begin
                if (fWitchItem.Results[I].Compatible<>100) and (CodepageFilter=cpfComplete) then
                  Continue
                else
                if (fWitchItem.Results[I].Compatible = 0) and (CodepageFilter=cpfPartial) then
                  Continue;
              end;
              { permit incompatible Codepages }

              L:=lvCodepageList.Items.Add;
              L.Caption:=IntToStr(fWitchItem.Results[I].Codepage);
              // L.Caption:=GetTranslation(K+'Analyzed/Caption', 'Analyzed');
              L.ImageIndex:=P;
              end;
          end;
        end;
        SelectCodepageListItem;
        lvCodepageList.Enabled:=True;
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
  finally
    lvCodePageList.EndUpdate;
    Locked:=False;
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
  if not Assigned(fWitchItem) then Exit;
  if not fWitchItem.Analyzed then Exit;
  if not fCodepage >= 0 then Exit;
  case FWitchItem.Encoding of
    weNone,
    weCodepage,
    weUnicode : Result:=True;
  else
    { weBinary, weError or maybe some future type not deffined }
    Exit;
  end;
end;

function TfMain.CanEdit: boolean;
begin
  Result:=False;
  if not Assigned(fWitchItem) then Exit;
  if not fWitchItem.Analyzed then Exit;
  if not fCodepage >= 0 then Exit;
  case FWitchItem.Encoding of
    weCodepage : Result := False; // Have not decided on this one yet.
    weNone,
    weUnicode : Result:=True;
  else
    { weBinary, weError or maybe some future type not deffined }
    Exit;
  end;
end;

function TfMain.GetWitchErrorMessage: String;
var
  Key : UnicodeString;
  Msg : UnicodeString;
  Err : UnicodeString;
begin
  if not Assigned(fWitchItem) then Exit('');
  Msg:=Translations.GetValue('Application/Error/File/Read/Message/Value',
  'Error #%1:s (%2:s) occured while reading from file "%0:s".');
  Key := '/Application/Error/Codes/';
  Err:=Translations.GetValue(Key + 'ec' +
  UnicodeString(IntToStr(fWitchItem.ErrorCode) + '/Value'), '');
  if Err='' then
    Err:=Translations.GetValue(Key + 'ecUnknown/Value', 'Unknown error');
  try
    Result:=Format(AnsiString(Msg), [fWitchItem.DisplayName,
    IntToStr(fWitchItem.ErrorCode), AnsiString(Err)]);
  except
    Result:=RawByteString(Translations.GetValue('/Application/Error/File/Read/Caption', 'File read error'));
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
  fSingleViewer:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsViewer/cbSingleViewer/State', 'Unchecked')) = cbChecked;

  UpdateFilterCheck;
  SetApplicationIcons;
  { TODO 5 -cDevel Add load color settings for Good/Bad mappings }

  // Values set and managed through OptionsDialog preferences.
  fUseExternalEditor:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsTextEditor/cbUseExternalEditor/State', 'Unchecked')) = cbChecked;
  fExternalEditor:=RawByteString(UserConfig.GetValue(
    'Preferences/tsTextEditor/edExternalEditor/Value', ''));
  {$IFDEF Darwin}
  // if DirectoryExists(fExternalEditor) and HasTrailing(fExternalEditor, '.app', false) then
  //  fExternalEditor:=GetAppBundleExec(fExternalEditor);
  {$ENDIF}
  if fUseExternalEditor and (fExternalEditor <> '') then
    LogMessage(vbNormal, 'External editor: ' + fExternalEditor);

  fFileReopen:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbReopenFiles/State', 'Unchecked')) = cbChecked;
  fFileWarn:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbWarnMissing/State', 'Unchecked')) = cbChecked;
  fOpenExported:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbOpenExport/State', 'Unchecked')) = cbChecked;
  fAutoSelectCP:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbAutoSelectCP/State', 'Checked')) = cbChecked;
  B:=StrToBool(UserConfig.GetValue(
    'Preferences/tsEncoding/rbFileEndAll/Checked', ''), fEndBlankOnInput);
  fEndBlankOnExport:=B or StrToBool(UserConfig.GetValue(
    'Preferences/tsEncoding/rbFileEndExport/Checked', ''), fEndBlankOnExport);

  if B <> fEndBlankOnInput then begin
    fEndBlankOnInput:=B;
    RefreshFileStatus;
  end;

  if fSingleViewer then begin
     pViewUnicode.Visible:=False;
     spUnicodeCP.Visible:=False;
  end else begin
    pViewUnicode.Visible:=True;
    spUnicodeCP.Visible:=True;
  end;
  EnforceLayout;
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
  if not Assigned(Sender) then Exit;
  if not (Sender is TWitchItem) then Exit;
  if fWitch.Count = 0 then Exit;
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
    weError : begin
      Cat(M, 'Error' + SPACE + '#');
      Cat(M, IntToStr(W.ErrorCode));
      Cat(M, SPACE+'with');
      W.ListItem.ImageIndex:=idxFileTypeFilePlainRed;
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

  if W = fWitchItem then begin
    fWitchItem:=nil;
    FDictEditForm.WitchItem:=nil;
    SelectFile(W);
  end;
end;

procedure TfMain.EditFile(Sender: TObject);
begin
  if CanEdit then begin
    if fUseExternalEditor and (fExternalEditor<>'') then begin // and FileExists(fExternalEditor) then begin
      try
        {$if defined(Darwin)}
          RunAsync('open', ['-a', fExternalEditor, fWitchItem.FileName]);
        {$elseif defined(Windows)}
          RunAsync('cmd', ['/c', 'start', '""', fExternalEditor, fWitchItem.FileName]);
        {$elseif defined(Linux)}
        { maybe xdg-open, gnome-open, kfmclient, etc }
          RunAsync(fExternalEditor, [fWitchItem.FileName]);
        {$else}
          raise Exception.Create('unknown host OS');
        {$endif}

        LogMessage(vbNormal, 'Open external editor with: ' + fWitchItem.FileName);
        Exit;
      except
        LogMessage(vbMinimal, 'Failed to launch external editor: ' + fExternalEditor);
      end;
    end;
    FileEditor(fWitchItem.FileName, @FileWasEdited);
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
  lvCodepageList.SmallImages:=ilCompatGreenColor;
end;

procedure TfMain.UpdateCodepageViewLabel;
begin
  if fCodepage < 0 then
    lbViewCodepageLabel.Caption:=' '
  else
    lbViewCodepageLabel.Caption:=GetFormat(ComponentNamePath(pViewCodepageLabel,
      Self, True) + 'lbViewCodepageLabel/Value' , [IntToStr(fCodepage)],
      'Viewed as Codepage %s');
end;

procedure TfMain.UpdateMetaData;
const
  Locked : boolean = false;
begin
  if Locked then Exit;
  Locked:=True;
  try
    UpdateCodepagelist;
    UpdateStatusBar;
    UpdateUnicodeView;
    UpdateCodepageView;
    UpdateCodepageViewLabel;
    UpdateLocale;
    UpdateButtons;
    UpdateDictionary;
  finally
    Locked :=False;
  end;
end;

procedure TfMain.UpdateStatusBarFileName;
var
  I, P, PW, M, Gutter : integer;
  S : String;
begin
  PW := statBar.Canvas.Handle; // Make sure Canvas is ready for textwidth.
  P := statBar.Panels.Count - 1;
  if not Assigned(fWitchItem) then begin
    statBar.Panels[P].Text:='';
    Exit;
  end;
  PW := 0;
  for I := 0 to P - 1 do
    Inc(PW, statBar.Panels[I].Width + 1);
  {$if defined(darwin)}
    Gutter := -25;
  {$elseif defined(windows)}
    Gutter := 10;
  {$else}
    Gutter := (15 * Screen.PixelsPerInch) div 96;
  {$endif}
  PW := statBar.ClientWidth - PW - Gutter;
  statBar.Canvas.Font := statBar.Font;
  M:=100;
  repeat
    S:=SPACE2+FriendlyPath(AppBasePath, fWitchItem.FileName, 1, M);
    if (statBar.Canvas.TextWidth(S) < PW) then Break;
    Dec(M,2);
  until (M < 10);
  statBar.Panels[P].Text:=S;
end;

procedure TfMain.UpdateStatusBar;
const
  spiEncoding = 0;               // displayed when file is selected
  spiLanguage = 1;            // not yet implemented
  spiPrefered = 2;            // not yet implemented
  spiLineEndings = 3;            // displayed when file is selected
  spiCompatiblity = 4;           // displayed when Codepage is selected
  spiFileName = 5;               // displayed when file is selected
var
  K : String;
  I : integer;
  Compat : String;
begin
  if not Assigned(fWitchItem) then begin
    statBar.Panels[spiEncoding].Text:='';
    statBar.Panels[spiLineEndings].Text:='';
    statBar.Panels[spiCompatiblity].Text:='';
    statBar.Panels[spiLanguage].Text:='';
    statBar.Panels[spiPrefered].Text:='';
    statBar.Panels[spiFileName].Text:='';
    Exit;
  end;
  Compat:='';
  UpdateStatusBarFileName;
  K:=ComponentNamePath(statBar, Self, True);
  if fWitchItem.Analyzed then begin
    if (fWitchItem.Encoding = weBinary) or (fWitchItem.Encoding = weError) then
      statBar.Panels[spiLineEndings].Text:=''
    else begin
      case fWitchItem.LineEndings of
      leCRLF : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/CRLF', 'CRLF');
      leLF : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/LF', 'LF');
      leCR : statBar.Panels[spiLineEndings].Text:=
        GetTranslation(K+'LineEnding/CR', 'CR');
      end;
      statBar.Panels[spiLanguage].Text:=fWitchItem.Locale;
      if fWitchItem.Preferred <> -1 then
        statBar.Panels[spiPrefered].Text:=IntToStr(fWitchItem.Preferred)
      else
        statBar.Panels[spiPrefered].Text:='';
    end;
    case fWitchItem.Encoding of
      weNone : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'NoEncoding/Caption', 'ASCII');
      weError : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Error/Caption', 'Error');
      weBinary : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Binary/Caption', 'Binary');
      weCodepage : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Codepage/Caption', 'Codepage');
      weUnicode : statBar.Panels[spiEncoding].Text:=
        GetTranslation(K+'Unicode/Caption', 'Unicode');
    end;
    if Assigned(fWitch) then begin
      for I := 0 to High(fWitchItem.Results) do
        if fWitchItem.Results[I].Codepage = fCodepage then begin
          Compat:=IntToStr(fWitchItem.Results[I].Compatible);
          Break;
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
  C : TCodepageToUTF8;
  H : String;
begin
  if fSingleViewer then Exit;
  if Assigned(fWitchItem) then begin
    if fWitchItem.Analyzed then begin
      case fWitchItem.Encoding of
        weNone, weUnicode : begin
          H:=EscapeHTML(PasExt.ToString(fWitchItem.FileData));
        end;
        weBinary : begin
         H:= GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
          +'Unsupported_file/Binary/Text', 'Binary data files are not supported.');
        end;
        weError : begin
         H:= GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
          +'File_Error/Error/Text', 'A file or processing error occured.') + LF + LF +
          GetWitchErrorMessage;
        end;
        weCodepage : begin
           C:=fWitchItem.AsUnicode(fCodepage, True);
           H:=EscapeHTML(RawByteString(C.Converted));
           C.Free;
        end;
      end;
    end;
  end;
  SetUnicodeView(H);
end;

procedure TfMain.UpdateCodepageView;
var
  C : TCodepageToUTF8;
begin
  if Not Assigned(fWitchItem) then begin
    fCodepage:=-2;
    fCodepageText.Clear;
    Exit;
  end;
  if not fWitchItem.Analyzed then begin
    fCodepage:=-2;
    fCodepageText.Clear;
    Exit;
  end else begin
    fCodepageText.BeginUpdate;
    fCodePageText.Clear;
    if (fCodepage <> -1) or (fWitchItem.Encoding=weBinary) or
    (fWitchItem.Encoding=weError) then begin
      case fWitchItem.Encoding of
        weNone : begin
          LogMessage(vbVerbose, 'ASCII Item: ' + fWitchItem.DisplayName + ' (Any Codepage, using ' +
           IntToStr(fCodepage) + ')');
          fCodepageText.Codepage:=fCodepage;
          fCodepageText.AddText(PasExt.ToString(fWitchItem.FileData));
        end;
        weError : begin
          fCodepageText.Codepage:=-1;
          LogMessage(vbVerbose, 'Error Item: ' + fWitchItem.DisplayName + ' (Error)');
          fCodepageText.AddError(GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
           +'File_Error/Error/Text', 'A file or processing error occured.') + LF + LF +
           GetWitchErrorMessage);
         end;
        weBinary : begin
          fCodepageText.Codepage:=-1;
          LogMessage(vbVerbose, 'Binary Item: ' + fWitchItem.DisplayName + ' (Not supported)');
          fCodepageText.AddError(GetTranslation(ComponentNamePath(pViewUnicode, Self, True)
            +'Unsupported_file/Binary/Text', 'Binary data files are not supported.'));
         end;
        weCodepage : begin
          LogMessage(vbVerbose, 'Codepage Item: ' + fWitchItem.DisplayName + ' (Codepage ' +
            IntToStr(fCodepage) + ')');
          // This is a tough choice. If using the active codepage, all characters
          // are displayed, Although some can be wrong because the codepage may
          // be wrong. However, using the Preferred Codepage, any characters
          // outside that codepage are  represented as an upside-down wuation
          // mark. This may be best as a toggle on the toolbar.
          fCodepageText.Codepage:=fCodepage;
          // fCodepageText.Codepage:=W.Preferred;
          C:=fWitchItem.AsUnicode(fCodepage, True);
          { TODO 6 -cDevel Decide on of codepage view of codepage files should be restricted to the preferred codepage. }
          fCodepageText.AddText(RawByteString(C.Converted));
          C.Free;
        end;
        weUnicode : begin
          LogMessage(vbVerbose, 'Unicode Item: ' + fWitchItem.DisplayName + ' (Codepage ' +
            IntToStr(fCodepage) + ')');
          fCodepageText.Codepage:=fCodepage;
          fCodepageText.AddText(PasExt.ToString(fWitchItem.FileData));
        end;
      end;
    end;
    fCodepageText.EndUpdate;
  end;
end;

procedure TfMain.UpdateButtons;
var
  E : TWitchEncoding;
begin
  actCloseAll.Enabled := fWitch.Count > 0;
  if Assigned(fWitchItem) then begin
    actClose.Enabled:=fWitchItem.Analyzed;
    E:=fWitchItem.Encoding;
  end else begin
    actClose.Enabled:=False;
    E:=weBinary;
  end;
  case E of
    weNone : begin
      actClose.ImageIndex:=idxButtonFileCloseGray;
      actExportASCII.Enabled:=CanExport;
      btnExportFile.Action:=actExportASCII;
      { TODO 0 -cLazarus_Bug In Lazarus 4.4, on Linux (and probably windows)
      simply switching from one Action to another will not cause the
      button to re-enable. It seems to get stuck for a little while as disabled
      until another Codepage list item is selected. }
      btnExportFile.Enabled:=CanExport;
      actEditASCII.Enabled:=CanEdit;
      btnEditFile.Action:=actEditASCII;
    end;
    weCodepage : begin
      actClose.ImageIndex:=idxButtonFileClose;
      actExportUnicode.Enabled:=CanExport;
      btnExportFile.Action:=actExportUnicode;
      { TODO 0 -cLazarus_Bug Without forcing it to Enable, the Button
      state can get "stuck" as disabled for a while. }
      btnExportFile.Enabled:=CanExport;
      actEditCodepage.Enabled:=CanEdit;
      btnEditFile.Action:=actEditCodepage;
    end;
    weUnicode: begin
      actClose.ImageIndex:=idxButtonFileCloseGreen;
      actExportCodepage.Enabled:=CanExport;
      btnExportFile.Action:=actExportCodepage;
      { TODO 0 -cLazarus_Bug Without forcing it to Enable, the Button
      state can get "stuck" as disabled for a while. }
      btnExportFile.Enabled:=CanExport;
      actEditUnicode.Enabled:=CanEdit;
      btnEditFile.Action:=actEditUnicode;
    end;
  else
    { weBinary or weError }
    actClose.ImageIndex:=idxButtonFileCloseRed;
    btnExportFile.Action:=actExportNone;
    { TODO 0 -cLazarus_Bug Without forcing it to Enable, the Button
    state can get "stuck" as disabled for a while. }
    btnExportFile.Enabled:=CanEdit;
    btnEditFile.Action:=actEditNone;
  end;
end;

procedure TfMain.UpdateFilterCheck;
begin
  actListCompatible.Checked:=FCodepageFilter=cpfComplete;
  actListPartial.Checked:=FCodepageFilter=cpfPartial;
  actListAll.Checked:=FCodepageFilter=cpfAll;
  case FCodepageFilter of
    cpfComplete : actCodepageFilter.ImageIndex:=actListCompatible.ImageIndex;
    cpfPartial  : actCodepageFilter.ImageIndex:=actListPartial.ImageIndex;
    cpfAll      : actCodepageFilter.ImageIndex:=actListAll.ImageIndex;
  end;
end;

procedure TfMain.UpdateLocale;
var
  S, L : String;
  I : Integer;
begin
  if not Assigned(fWitchItem) then begin
    lbCodepageLanguage.Caption:='';
    lbUnicodeLanguage.Caption:='';
    Exit;
  end;
  if fWitchItem.Analyzed then begin
    L:='';
    S:=LowerCase(fWitchItem.Locale);
    for I := 0 to High(fLocales) do
      if S=LowerCase(fLocales[I].ID) then begin
        L:=fLocales[I].Name;
        Break;
      end;
    if L='' then L:=S;
    if fSingleViewer then
      case fWitchItem.Encoding of
       weNone, weUnicode, weCodepage : begin
         lbCodepageLanguage.Caption:=L;
         lbUnicodeLanguage.Caption:='';
       end;
       else
         lbCodepageLanguage.Caption:='';
         lbUnicodeLanguage.Caption:='';
       end
    else
      case fWitchItem.Encoding of
       weNone, weUnicode : begin
         lbCodepageLanguage.Caption:='';
         lbUnicodeLanguage.Caption:=L;
       end;
       weCodepage : begin
         lbCodepageLanguage.Caption:=L;
         lbUnicodeLanguage.Caption:='';
       end;
       else
         lbCodepageLanguage.Caption:='';
         lbUnicodeLanguage.Caption:='';
       end;
  end else begin
    lbCodepageLanguage.Caption:='';
    lbUnicodeLanguage.Caption:='';
  end;
end;

procedure TfMain.UpdateLocaleList;
var
  I : integer;
  S, T : String;
  L : TArrayOfString;
  X : TLocale;
begin
  FLocales:=[];
  L:=LocaleList;
  S:='';
  if Assigned(MasterDictionary) then Cat(S, MasterDictionary.Locales);
  while S <> '' do begin
    T:=Trim(PopDelim(S, COMMA));
    if T = '' then Continue;
    if InArray(L, T, 0, False) = -1 then
      Cat(L, T);
  end;
  LogMessage(vbVerbose, IntToStr(Length(L)) + ' known locales: ' + Implode(L, COMMA + SPACE));
  SetLength(FLocales, Length(L));
  for I:=0 to High(L) do begin
    if Locale(L[I], X) then begin
      FLocales[I].ID:=X.Identifier;
      FLocales[I].Name:=RawByteString(X.Language);
    end else begin
      FLocales[I].ID:=L[I];
      FLocales[I].Name:=L[I];
    end;
    try
      FLocales[I].Name:=GetTranslation('Locales/' + FLocales[I].ID + '/Text',
        FLocales[I].Name);
    except
      LogMessage(vbMinimal, 'Exception for bad locale code: ' + FLocales[I].ID);
    end;
  end;
end;

procedure TfMain.UpdateDictionary;
begin
  if Assigned(FDictEditForm) and (fDictEditForm.Visible = True) then begin
    if Assigned(fWitchItem) then
      FDictEditForm.WitchItem:=fWitchItem
    else
      FDictEditForm.WitchItem:=nil;
  end;
end;

procedure TfMain.SelectCodepageListItem;
var
  I : Integer;
  L : TListItem;
begin
  try
    L:=nil;
    if lvCodepageList.Items.Count = 1 then
      L:=lvCodepageList.Items[0]
    else
    for I := 0 to lvCodepageList.Items.Count - 1 do
      if lvCodepageList.Items[I].Caption = IntToStr(fCodepage) then begin
        L:=lvCodepageList.Items[I];
        Break;
      end;

    if (not Assigned(L)) and Assigned(fWitchItem) then begin
      if (lvCodepageList.Items.Count > 0) and (fWitchItem.Preferred <> -1)
      and (fCodepage <> fWitchItem.Preferred) then begin
        SelectCodePage(fWitchItem.Preferred);
        SelectCodepageListItem;
        Exit;
      end;
    end;

    if Assigned(L) then begin
      L.Selected := True;
      L.Focused := True;
      L.MakeVisible(False);
    end;

  finally
  end;
end;

procedure TfMain.SelectCodepage(Value : Integer);
begin
  if (Value = -1) then Value:=437;
  if Value = fCodepage then Exit;
  fCodepage:=Value;
  LogMessage(vbVerbose, 'Selected Codepage ' + IntToStr(fCodePage));
  // Commented out items should never change when simply switching codepages.
  // UpdateCodepagelist;
  UpdateStatusBar;
  if Assigned(fWitchItem) and (fWitchItem.Encoding = weCodepage) then
    UpdateUnicodeView;
  UpdateCodepageView;
  UpdateCodepageViewLabel;
  // UpdateLocale;
  UpdateButtons;
  // UpdateDictionary;
end;

procedure TfMain.SelectFile(Item:TWitchItem);
const
  SuspendCheck : boolean = false;
var
  S : String;
  LI : TListItem;
begin
  fWitchItem:=Item;
  fCodepage:=-1;
  if Assigned(fWitchItem) then begin
    S:=fWitchItem.FileName;
    fCodepage:=fWitchItem.Preferred;
    LI:=fWitchItem.ListItem;
    if Assigned(LI) and (lvFileList.Selected <> LI) then begin
      lvFileList.Selected:=LI;
      LI.MakeVisible(False);
    end;

  end else
    S:='(null)';
  LogMessage(vbVerbose, 'Select File: ' + FriendlyPath(AppBasePath, S));
  UpdateMetaData;

  if not Assigned(fWitchItem) then Exit;
  if SuspendCheck then Exit;
  if not fWitchItem.Analyzed then Exit;
  if fWitchItem.Encoding = weError then Exit;
  if fWitchItem.Encoding = weBinary then Exit;

  if fEndBlankOnInput and (fWitchItem.EndsWithBlank = false) then begin
    S:=fWitchItem.FileName;
    if FixFileLineEnding(S) = mrOK then begin
      SuspendCheck:=True;
      actCloseExecute(Self);
      SuspendCheck:=False;
      OpenFile(S, True);
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
    if Assigned(fWitchItem) then begin
      X:=fWitchItem.Index;
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
    lvFileList.Items.BeginUpdate;
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
  lvFileList.Items.EndUpdate;
  lvFileList.Sort;
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

procedure TfMain.RefreshFileStatus;
var
  I : Integer;
begin
  for I := 0 to fWitch.Count - 1 do
    if fWitch.Items[I].Analyzed then
      WitchOnAnalyzed(fWitch.Items[I]);
end;

procedure TfMain.EnforceLayout;
begin
  inherited EnforceLayout;
  {$IFNDEF darwin}
  { TODO 0 -cLazarus_Bug In Lazarus 4.4, Minimizing and restoring a window on
  Windows causes all but the pViewers panel (alClient) to move around and change
  positions. This will force them back into the correcto order. }
  if not (
  (pFileList.Left < spFileCP.Left) and
  (spFileCP.Left < pCodepageList.Left) and
  (pCodepageList.Left < spCPViewers.Left) and
  (spCPViewers.Left < pViewers.Left)
  ) then begin
     pViewers.Left:=10000;
     spCPViewers.Left:=9000;
     pCodepageList.Left:=8000;
     spFileCP.Left:=7000;
     pFileList.Left:=0;
   end;
   {$ENDIF}
   // Since we chan hide/unhide the viewer for unicode, We might as well
   // check on the Viewer Panels on all platforms. With how the Left + Left
   // panels behave, who knows what order these will be in when restoring their
   // properties of minimizing the app.
   if (Not fSingleViewer) and (not (
   (pViewUnicode.Top < spUnicodeCP.Top) and
   (spUnicodeCP.Top < pCodepage.Top)
   )) then begin
     pCodepage.Top:=10000;
     spUnicodeCP.Top := 9000;
     pViewUnicode.Top:=0;
   end;
end;

procedure TfMain.SetSaveDialogText(Encoding : TWitchEncoding);
begin
  case Encoding of
    weNone: begin
      dlgFileSave.Title:=GetTranslation('ExportASCIIDialog/Title/Caption',
       'Save file as simple 7-bit ASCII');
      dlgFileSave.Filter:=GetTranslation('ExportASCIIDialog/File/Filters', 'All Files (*.*)|*.*');
    end;
    weUnicode: begin
      dlgFileSave.Title:=GetFormat('ExportCodepageDialog/Title/Caption',
       [IntToStr(fCodepage)], 'Export file as Codepage %s');
      dlgFileSave.Filter:=GetTranslation('ExportCodepageDialog/File/Filters', 'All Files (*.*)|*.*');
    end;
    weCodepage: begin
      dlgFileSave.Title:=GetFormat('ExportUnicodeDialog/Title/Caption',
       [IntToStr(fCodepage)], 'Export file as UTF-8');
      dlgFileSave.Filter:=GetTranslation('ExportUnicodeDialog/File/Filters', 'All Files (*.*)|*.*|Unicode Files (*.UTF-8)|*.UTF-8');
    end;
  else
    dlgFileSave.Title:=GetTranslation('ExportFileDialog/Title/Caption',
    'Save file as');
    dlgFileSave.Filter:=GetTranslation('ExportFileDialog/File/Filters', 'All Files (*.*)|*.*');
  end;
end;

procedure TfMain.ReAnalyzeFiles;
var
  I : Integer;
begin
  if Not Assigned(FWitch) then Exit;
  for I := 0 to FWitch.Count - 1 do
    FWitch.Modified[I]:=True;
end;

procedure TfMain.ApplyUserLanguage;
var
  X:Boolean;
begin
  inherited ApplyUserLanguage;
  UpdateLocaleList;
  UpdateMetaData;
  if Assigned(fWitch) then begin
    X:=StringToCheckBoxState(UserConfig.GetValue(
    'Preferences/tsSession/cbUseFileExt/State', 'Checked')) = cbChecked;
    if X <> fWitch.UseFileExt then begin
      fWitch.UseFileExt:=X;
      ReAnalyzeFiles;

    end;
  end;

end;

procedure TfMain.CloseAllFiles;
var
  I, LIDX : Integer;
begin
  {$IFDEF STICKYLISTITEMS}
  fLastFileItem:=nil;
  fLastCodeItem:=nil;
  {$ENDIF}
  fWitchItem:=nil;
  fCodepage:=-1;
  lvFileList.Items.BeginUpdate;
  try
    for I := fWitch.Count - 1 downto 0 do begin
      fWitchItem:=fWitch.Items[I];
      if fWitchItem.Analyzed then begin
        LIDX:=fWitchItem.ListItem.Index;
        fWitchItem.ListItem:=nil;
        lvFileList.Items.Delete(LIDX);
        lvFileList.Invalidate;
        fWitch.Delete(fWitchItem);
      end;
    end;

    { Still has problems on Linux, probably on Windows as well
    LogMessage(vbVerbose, 'Clear All Files');
    fWitch.AbortAll;
    lvFileList.Items.Clear;
    fWitch.Clear;}
  finally
    lvFileList.Items.EndUpdate;
    fWitchItem:=nil;
  end;
  UpdateMetaData;
end;

procedure TfMain.CloseFile;
var
  LIDX : Integer;
begin
  {$IFDEF STICKYLISTITEMS}
  fLastFileItem:=nil;
  fLastCodeItem:=nil;
  {$ENDIF}
  { TODO 1 -cLazarus_Bug Removing a list item leaves a clickable null item at
  the end of the list. This creates a phantom null TListItem at the end. While
  it does not seem to hurt anything, it does cause a flicker to that phatom
  item and back to the current selection. Clearing and rebuilding the list,
  along with preserving the TopItem should fix it. But for now, I'll just let
  the LCL bug stand. }
  if not Assigned(fWitchItem) then Exit;
  fWitchItem.Abort;
  lvFileList.Items.BeginUpdate;
  try
    lvFileList.ItemIndex := -1;
    if Assigned(fWitchItem.ListItem) then begin
      LIDX:=fWitchItem.ListItem.Index;
      fWitchItem.ListItem:=nil;
      lvFileList.Items.Delete(LIDX);
      lvFileList.Invalidate;
    end else
      LIDX:=-1;
   {$IFDEF STICKYLISTITEMS}
   fLastFileItem:=nil;
   fLastCodeItem:=nil;
   {$ENDIF}
   fWitch.Delete(fWitchItem);
    fWitchItem:=nil;
    fCodepage:=-1;
    if (fWitch.Count = 0) or (LIDX <0) then
      UpdateMetadata
    else begin
      if LIDX >= lvFileList.Items.Count then
        LIDX:=lvFileList.Items.Count - 1;
      SelectFile(TWitchItem(lvFileList.Items[LIDX].Data));
    end;
  finally
    lvFileList.Items.EndUpdate;
  end;
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
    LogMessage(vbVerbose, 'File "' + FriendlyPath(AppBasePath, FileName) +
      '" is already open.');
    fWitch.Modified[I];
  end else begin
    try
      LI:=lvFileList.Items.Add;
      I := fWitch.Add(FileName, LI);
      lvFileList.Sort;
    except
      LI.Delete;
      LogMessage(vbVerbose, 'Open file "' + FriendlyPath(AppBasePath, FileName) +
        '" Failed!');
      Exit;
    end;
    LogMessage(vbVerbose, 'Opened file "' + FriendlyPath(AppBasePath, FileName) + '"');
  end;

  if Select then begin
    fWitch.Select(I);
    fWitch.Items[I].ListItem.MakeVisible(False);
    lvFileList.SetFocus;
    UpdateMetaData;
  end;

end;


end.

