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
  Version, PasExt, Icons, MultiApp;

type

  { TfPreferences }

  { TODO 5 -cDevel Figure out the best way to migrate Prefenences Dialog to the
    MPLA as a generic reusable dialog. }
  TfPreferences = class(TMultiAppForm)
    btnCancel: TButton;
    btnOkay: TButton;
    btnUpdateCheck: TButton;
    cbAutoCheck: TCheckBox;
    gbOnlineUpdates: TGroupBox;
    gbUserLanguage: TGroupBox;
    imgAppImage: TImage;
    lbSections: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure tvSectionsChange(Sender: TObject; Node: TTreeNode);
  private
    lblApp, lblVer, lblCopy, lblSysLang : TLabel;
    function CreateLabel(AParent : TWinControl; ACaption : String;
      AAlignment : TAlignment=taCenter; ASpacing : integer=4) : TLabel;
    procedure CreateSectionTree;
  public
    procedure ApplyUserLanguage; override;

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
    [UserLanguage], 'System Language: %s'), taLeftJustify, 8
  );

  CreateSectionTree;
  pcTabs.ActivePage:=tsAbout;
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

initialization

  fPreferences:=nil;
end.

