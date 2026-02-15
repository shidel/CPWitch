unit uPrefs;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  Preferences;

type

  { TfOptionsDialog }

  TfOptionsDialog = class(TMultiAppPreferences)
    cbReopenFiles: TCheckBox;
    cbWarnMissing: TCheckBox;
    gbSessionFiles: TGroupBox;
    pPages: TPageControl;
    tsEncoding: TTabSheet;
    tsViewer: TTabSheet;
    tsSession: TTabSheet;
    procedure cbReopenFilesChange(Sender: TObject);
  private

  public

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

initialization

  fOptionsDialog:=nil;

end.

