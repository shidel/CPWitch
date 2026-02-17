{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uLostFile;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  PasExt, MultiApp, Icons;

procedure ShowMissingFiles(List : TStringList);

implementation

type

  { TfMissingFiles }

  TfMissingFiles = class(TMultiAppDialog)
    mFileList: TMemo;
  private
  protected
    procedure DoCreate; override;
  public
    procedure FileList(List : TStringList);

  end;

var
  fMissingFiles: TfMissingFiles;

procedure ShowMissingFiles(List: TStringList);
begin
  if not Assigned(fMissingFiles) then begin
    fMissingFiles:=TfMissingFiles.CreateNew(Application);
  end;
  fMissingFiles.FileList(List);
  fMissingFiles.Show;
  fMissingFiles.Activate;
end;

{ TfMissingFiles }

procedure TfMissingFiles.DoCreate;
begin
  Name:='fMissingFiles';
  Caption:='Missing document files';
  Images:=ilDialogColor;
  ImageIndex:=idxDialogFileError;
  ImageVisible:=True;
  mFileList:=TMemo.Create(Self);
  inherited DoCreate;
  mFilelist.Parent:=BodyPanel;
  mFileList.Align:=alClient;
  mFileList.ReadOnly:=True;
  mFileList.ScrollBars:=ssAutoBoth;
  mFileList.WordWrap:=False;
  mFileList.Clear;
end;

procedure TfMissingFiles.FileList(List: TStringList);
begin
  mFileList.Lines.AddStrings(List, True);
end;

initialization

  fMissingFiles:=nil;

end.

