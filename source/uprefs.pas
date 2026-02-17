{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uPrefs;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  MultiApp, Preferences;

type

  { TfOptionsDialog }

  TfOptionsDialog = class(TMultiAppPreferences)
    cbReopenFiles: TCheckBox;
    cbWarnMissing: TCheckBox;
    gbSessionFiles: TGroupBox;
    gbUnicodeView: TGroupBox;
    gbDOSView: TGroupBox;
    lbDOSScale: TLabel;
    lbUnicodeScale: TLabel;
    pPages: TPageControl;
    rbFileEndWhatever: TRadioButton;
    rbFileEndExport: TRadioButton;
    rbFileEndAll: TRadioButton;
    rgFileEnding: TRadioGroup;
    tbUnicodeScale: TTrackBar;
    tbDOSScale: TTrackBar;
    tsEncoding: TTabSheet;
    tsViewer: TTabSheet;
    tsSession: TTabSheet;
    procedure cbReopenFilesChange(Sender: TObject);
    procedure tbDOSScaleChange(Sender: TObject);
    procedure tbUnicodeScaleChange(Sender: TObject);
  private

  protected
    procedure UpdateUnicodeScale;
    procedure UpdateDOSScale;

  public
    procedure ApplyUserLanguage; override;

  end;

var
  fOptionsDialog: TfOptionsDialog;

implementation

{$R *.lfm}

{ TfOptionsDialog }

procedure TfOptionsDialog.cbReopenFilesChange(Sender: TObject);
begin
  cbWarnMissing.Enabled:=cbReopenFiles.Checked;
end;

procedure TfOptionsDialog.tbDOSScaleChange(Sender: TObject);
begin
  UpdateDOSScale;
end;

procedure TfOptionsDialog.tbUnicodeScaleChange(Sender: TObject);
begin
  UpdateUnicodeScale;
end;

procedure TfOptionsDialog.UpdateUnicodeScale;
var
  S : String;
begin
  S:=IntToStr(tbUnicodeScale.Position * 10);
  lbUnicodeScale.Caption:=GetFormat(ComponentNamePath(tsViewer, Self, True) +
    'lbUnicodeScale/Caption', [S], 'Font scale %s%%');
end;

procedure TfOptionsDialog.UpdateDOSScale;
var
  S : String;
begin
  S:=IntToStr(tbDOSScale.Position * 100);
  lbDOSScale.Caption:=GetFormat(ComponentNamePath(tsViewer, Self, True) +
    'lbDOSScale/Caption', [S], 'Font scale %s%%');
end;

procedure TfOptionsDialog.ApplyUserLanguage;
begin
  inherited ApplyUserLanguage;
  UpdateUnicodeScale;
  UpdateDOSScale;
end;

initialization

  fOptionsDialog:=nil;

end.

