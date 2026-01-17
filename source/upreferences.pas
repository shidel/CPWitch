{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uPreferences;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus,
  Version, PasExt, Icons, MultiApp, Updater;

type

  { TfPreferences }

  { TODO 5 -cDevel Figure out the best way to migrate Prefenences Dialog to the
    MPLA as a generic reusable dialog. }
  TfPreferences = class(TMultiAppForm)
    btnCancel: TButton;
    btnOkay: TButton;
    btnUpdateCheck: TButton;
    cbAutoCheck: TCheckBox;
    cbColorButtons: TCheckBox;
    cbHints: TCheckBox;
    gbOnlineUpdates: TGroupBox;
    gbUserLanguage: TGroupBox;
    gbMiscellaneous: TGroupBox;
    imgAppImage: TImage;
    lbPreferred: TLabel;
    lbSections: TLabel;
    pPrefOpts: TPanel;
    pUsrLang: TPanel;
    pSysLang: TPanel;
    pUpdateButton: TPanel;
    pVersionInfo: TPanel;
    pAppImage: TPanel;
    pcTabs: TPageControl;
    pSectionLabel: TPanel;
    pPagePanel: TPanel;
    pSectionPanel: TPanel;
    pButtons: TPanel;
    pUpperPanel: TPanel;
    pMiddlePanel: TPanel;
    pLowerPanel: TPanel;
    sSectionsPage: TSplitter;
    tsGeneral: TTabSheet;
    tsAbout: TTabSheet;
    tvSections: TTreeView;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkayClick(Sender: TObject);
    procedure btnUpdateCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvSectionsChange(Sender: TObject; Node: TTreeNode);
  private
  protected
    lblApp, lblVer, lblCopy, lblSysLang : TLabel;
    cbPrefLang : TComboBox;
    fDefaultLanguage : String;
    function CreateLabel(AParent : TWinControl; ACaption : String;
      AAlignment : TAlignment=taCenter; ASpacing : integer=4) : TLabel;
    procedure CreateSectionTree;
    procedure CreateLanguageSelectBox;
  public
    procedure ApplyUserLanguage; override;
    procedure ReadConfiguration; virtual;
    procedure WriteConfiguration; virtual;

  end;

var
  fPreferences: TfPreferences;

implementation

{$R *.lfm}

{ TfPreferences }

procedure TfPreferences.FormCreate(Sender: TObject);
var
  Key : String;
begin
  pcTabs.ShowTabs:=False;
  try
    imgAppImage.Picture.LoadFromResourceName(HINSTANCE, 'MAINICON');
  except
    // ignore
  end;

// Labels added to About page in reverse order and aligned with top of panel
  Key:=ComponentNamePath(tsAbout, Self, True);
  lblCopy := CreateLabel(pVersionInfo,GetFormat(Key + 'Copyright',
    [APP_LEGALCOPYRIGHT], 'Copyright %s'));
  lblVer := CreateLabel(pVersionInfo,GetFormat(Key + 'Version',
    [APP_VERSION, APP_BUILD], 'Version %0:s (build %1:s)')
  );
  lblApp := CreateLabel(pVersionInfo,GetFormat(Key + 'Application',
    [APP_PRODUCTNAME], 'The "%s"')
  );
  // User System Language
  Key:=ComponentNamePath(gbUserLanguage, Self, True);
  lblSysLang := CreateLabel(pSysLang,GetFormat(Key + 'System',
    [UserLanguage], 'System Language:  %s'), taLeftJustify);
  lblSysLang.BorderSpacing.Left:=4;

  // String for Default preferred language
  Key:=ComponentNamePath(lbPreferred, Self, True);
  fDefaultLanguage := GetTranslation(Key + 'Default', '(default)');

  CreateLanguageSelectBox;
  pSysLang.AutoSize:=True;
  lbPreferred.FocusControl:=cbPrefLang;
  ControlOnRight(pPrefOpts, lbPreferred);

  CreateSectionTree;
  pcTabs.ActivePage:=tsAbout;
end;

procedure TfPreferences.btnUpdateCheckClick(Sender: TObject);
begin
  UpdateCheck(True);
end;

procedure TfPreferences.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfPreferences.btnOkayClick(Sender: TObject);
begin
  WriteConfiguration;
  Close;
end;

procedure TfPreferences.FormResize(Sender: TObject);
begin
  { TODO 0 -cBug Force position to prevent controls from swapping places when
    the form is resized vertically. However, they do still flicker back and
    forth on occasion when being resized. This was improved a lot by placing
    the TComboBox inside it's own TPanel. Instead of aligning both controls
    to tle left, Aligning the panel with the TComboBox as alClient has seemed
    to fix the issue. However, leaving the forced position in place anyway. }
  ControlOnRight(pPrefOpts, lbPreferred);
end;

procedure TfPreferences.FormShow(Sender: TObject);
begin
  ReadConfiguration;
end;

procedure TfPreferences.tvSectionsChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and Assigned(Node.Data) then
    pcTabs.ActivePage:=TTabSheet(Node.Data)
  else
    pcTabs.ActivePage:=tsAbout;
end;

function TfPreferences.CreateLabel(AParent:TWinControl; ACaption: String;
    AAlignment : TAlignment=taCenter; ASpacing : integer=4): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent:=AParent;
  Result.Align:=alTop;
  Result.Alignment:=AAlignment;
  Result.AutoSize:=True;
  Result.WordWrap:=True;
  Result.BorderSpacing.Around:=ASpacing;
  Result.Caption:=ACaption;
end;

procedure TfPreferences.CreateSectionTree;
var
  I : Integer;
  P : TTabSheet;
  L : TTreeNode;
  C : String;
begin
  { TODO 5 -cDevel Have preference section tree automatically make sub-nodes for
    pages }
  for I := 0 to pcTabs.PageCount - 1 do begin
    P:=pcTabs.Pages[I];
    if not (P.TabVisible) then Continue;
    C:=P.Caption;
    L:=tvSections.Items.Add(nil, C);
    L.Data:=P;
  end;
  L:=tvSections.Items.GetFirstNode;
  if Assigned(L) then L.Selected:=True;
end;

procedure TfPreferences.CreateLanguageSelectBox;
var
  Pref, L : String;
  Langs : TArrayOfRawByteString;
  I : Integer;
begin
  Pref:=Trim(RawByteString(UserConfig.GetValue('Application/Language/Value', '')));

  cbPrefLang:=TComboBox.Create(Self);
  cbPrefLang.Parent:=pPrefOpts;
  // cbPrefLang.Left:=1;
  // cbPrefLang.Width:=75;
  cbPrefLang.Align:=alClient;
  cbPrefLang.AutoSize:=True;
  cbPrefLang.Constraints.MaxWidth:=200;
  cbPrefLang.BorderSpacing.Top:=1;
  cbPrefLang.BorderSpacing.Left:=8;
  cbPrefLang.BorderSpacing.Bottom:=7;
  cbPrefLang.BorderSpacing.Right:=8;
  cbPrefLang.Items.Add(fDefaultLanguage);
  // cbPrefLang.Text:=fDefaultLanguage;
  cbPrefLang.ReadOnly:=True;
  // Add Other Available Languages
  { TODO 0 -cDevel Improve Language NLS scan to not be case specific }
  cbPrefLang.Items.Add('en_US'); // Built-in
  DirScan(AppDataPath + PathDelimiter + '*.nls', Langs, [dsFiles]);
  for I := 0 to Length(Langs) - 1 do begin
    L :=Trim(ExcludeTrailing(Langs[I], '.NLS', false));
    if cbPrefLang.Items.IndexOf(L) <> -1 then Continue;
    cbPrefLang.Items.Add(L);
  end;

end;

procedure TfPreferences.ApplyUserLanguage;

  procedure UpdateNode(Node : TTreeNode);
  var
    C : String;
  begin
    While Assigned(Node) do begin
      UpdateNode(Node.GetFirstChild);
      if Assigned(Node.Data) then begin
        C := TTabSheet(Node.Data).Caption;
        Node.Text:=C;
      end;
      Node:=Node.GetNext;
    end;
  end;

begin
  inherited ApplyUserLanguage;
  UpdateNode(tvSections.Items.GetFirstNode);
end;

procedure TfPreferences.ReadConfiguration;
begin
  cbAutoCheck.Checked:=GetAutoUpdate;
  cbColorButtons.Checked:=DefaultIconThemeInColor;
  if Assigned(IconTheme) then
    cbColorButtons.Checked := IconTheme.InColor;
end;

procedure TfPreferences.WriteConfiguration;
begin
  SetAutoUpdate(cbAutoCheck.Checked);
  if Assigned(UserConfig) then begin
    if Assigned(IconTheme) and (DefaultIconThemeInColor<>cbColorButtons.Checked) then
      UserConfig.SetValue('Application/Theme/ColorIcons', cbColorButtons.Checked);
  end;
  if Assigned(IconTheme) then IconTheme.InColor:=cbColorButtons.Checked;
  try
    if Assigned(UserConfig) then
      UserConfig.Flush;
  except
    on E : Exception do
      LogMessage(vbCritical, 'error writing config file: ' + E.Message);
  end;
end;

initialization

  fPreferences:=nil;
end.

