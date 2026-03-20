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
  ExtCtrls, ComCtrls, PasExt, MultiApp, Preferences, Icons;

type

  { TfOptionsDialog }

  TfOptionsDialog = class(TMultiAppPreferences)
    btnBrowseEditor: TButton;
    cbReopenFiles: TCheckBox;
    cbWarnMissing: TCheckBox;
    cbOpenExport: TCheckBox;
    cbAutoSelectCP: TCheckBox;
    cbSingleViewer: TCheckBox;
    cbUseExternalEditor: TCheckBox;
    cbUseFileExt: TCheckBox;
    edExternalEditor: TEdit;
    gbSessionFiles: TGroupBox;
    gbUnicodeView: TGroupBox;
    gbDOSView: TGroupBox;
    gbCodepage: TGroupBox;
    gbViewers: TGroupBox;
    gbExternalEditor: TGroupBox;
    gbLocaleDetection: TGroupBox;
    lbDOSScale: TLabel;
    lbUnicodeScale: TLabel;
    DlgBrowseEditor: TOpenDialog;
    pEditor: TPanel;
    pPages: TPageControl;
    rbFileEndWhatever: TRadioButton;
    rbFileEndExport: TRadioButton;
    rbFileEndAll: TRadioButton;
    rgFileEnding: TRadioGroup;
    tsTextEditor: TTabSheet;
    tbUnicodeScale: TTrackBar;
    tbDOSScale: TTrackBar;
    tsEncoding: TTabSheet;
    tsViewer: TTabSheet;
    tsSession: TTabSheet;
    procedure btnBrowseEditorClick(Sender: TObject);
    procedure cbReopenFilesChange(Sender: TObject);
    procedure cbUseExternalEditorChange(Sender: TObject);
    procedure edExternalEditorEditingDone(Sender: TObject);
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

procedure TfOptionsDialog.btnBrowseEditorClick(Sender: TObject);
var
  FU, FW : String;
begin
  dlgBrowseEditor.Title:=GetTranslation(
   'pPages/tsTextEditor/gbExternalEditor/BrowseDialog/Title/Caption',
   'Browser for external text editor');
  FU:=GetTranslation(
   'pPages/tsTextEditor/gbExternalEditor/BrowseDialog/File/Unix/Exec',
   'All files|*');
  FW:=GetTranslation(
   'pPages/tsTextEditor/gbExternalEditor/BrowseDialog/File/Windows/Exec',
   'Executable files|*.EXE|All files|*.*');
  IgnoreParameter([FU, FW]);
  {$if defined(windows)}
    dlgBrowseEditor.Filter:=FW;
  {$else}
    dlgBrowseEditor.Filter:=FU;
  {$endif}
  dlgBrowseEditor.FileName:=edExternalEditor.Text;
  if dlgBrowseEditor.Execute then
    edExternalEditor.Text:=dlgBrowseEditor.FileName;
end;

procedure TfOptionsDialog.cbUseExternalEditorChange(Sender: TObject);
begin
  edExternalEditor.Enabled:=cbUseExternalEditor.Checked;
  btnBrowseEditor.Enabled:=edExternalEditor.Enabled;
end;

{$IFDEF Darwin}
function GetMacApp(S : String):String;
  function AppSearch(P : String) : String;
  var
    I : Integer;
    L : TArrayOfRawByteString;
  begin
    Result:='';
    Cat(P, '/Applications/');
    DirScan(P+'*.app', L, [dsDirectories]);
    for I := Low(L) to High(L) do begin
      if (LowerCase(L[I]) = S) or (LowerCase(L[I]) = S+'.app') then begin
        Result:=P+L[I];
        Exit;
      end;
    end;
  end;
begin
  S:=Lowercase(S);
  Result:=AppSearch('/Users/' + ExtractFileName(ExcludeTrailing(UserHomePath, PathDelimiter)));
  if Result='' then
    Result:=AppSearch('');
end;
{$ENDIF}

procedure TfOptionsDialog.edExternalEditorEditingDone(Sender: TObject);
var
  S : String;
begin
  S := Trim(edExternalEditor.Text);
  if S <> edExternalEditor.Text then
    edExternalEditor.Text:=S;
  if S <> '' then begin
    S := ExeSearch(edExternalEditor.Text);
    {$IFDEF Darwin}
    if S = '' then
      S:=GetMacApp(edExternalEditor.Text);
    {$ENDIF}
    if (S <> '') and (S <> edExternalEditor.Text) then
      edExternalEditor.Text:=S;
  end;
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
//  lbUnicodeScale.Caption:=GetFormat(ComponentNamePath(tsViewer, Self, True) +
//    'lbUnicodeScale/Caption', [S], 'Font scale %s%%');
  lbUnicodeScale.Caption:=GetFormat(
    'pPages/tsViewer/gbUnicodeView/lbScaleOfUnicode/Caption', [S], 'Font scale %s%%');
end;

procedure TfOptionsDialog.UpdateDOSScale;
var
  S : String;
begin
  S:=IntToStr(tbDOSScale.Position * 100);
//  lbDOSScale.Caption:=GetFormat(ComponentNamePath(tsViewer, Self, True) +
//    'lbDOSScale/Caption', [S], 'Font scale %s%%');
  lbDOSScale.Caption:=GetFormat(
    'pPages/tsViewer/gbDOSView/lbScaleOfDOS/Caption', [S], 'Font scale %s%%');
end;

procedure TfOptionsDialog.ApplyUserLanguage;
begin
  FlattenControl(pEditor,False);
  inherited ApplyUserLanguage;
  UpdateUnicodeScale;
  UpdateDOSScale;
  cbReopenFilesChange(Self);
  cbUseExternalEditorChange(Self);
end;

initialization

  fOptionsDialog:=nil;

end.

