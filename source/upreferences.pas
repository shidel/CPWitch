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

  TfPreferences = class(TMultiAppForm)
    btnCancel: TButton;
    btnOkay: TButton;
    imgAppImage: TImage;
    lbSections: TLabel;
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
    Splitter1: TSplitter;
    tsAbout: TTabSheet;
    tvSections: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    lblApp, lblVer, lblCopy : TLabel;
    function CreateAboutComment(ACaption : String) : TLabel;
  public

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
  Key:=ComponentNamePath(tsAbout, Self);
  if Key <> '' then Key:=IncludeTrailing(Key, '/');
  lblCopy := CreateAboutComment(GetFormat(Key + 'Copyright',
    [APP_LEGALCOPYRIGHT], 'Copyright %s'));
  lblVer := CreateAboutComment(GetFormat(Key + 'Version',
    [APP_VERSION, APP_BUILD], 'Version %0:s (build %1:s)')
  );
  lblApp := CreateAboutComment(GetFormat(Key + 'Application',
    [APP_PRODUCTNAME], 'The "%s"')
  );

end;

function TfPreferences.CreateAboutComment(ACaption: String): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent:=pVersionInfo;
  Result.Align:=alTop;
  Result.Alignment:=taCenter;
  Result.AutoSize:=True;
  Result.WordWrap:=True;
  Result.BorderSpacing.Around:=4;
  Result.Caption:=ACaption;
end;

initialization

  fPreferences:=nil;
end.

