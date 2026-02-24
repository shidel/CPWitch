{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uEditor;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

(* Application conditional defines *)
{$IFDEF BUILD_PRERELEASE}
// Include support for the possible creation of a 'no_language.nls' by the
// MultiApp unit. Strings are only written into ths file when the program
// requests a string for the first time. Subsequent requests will not update
// the text stored in the file.
  {$DEFINE SUPPORT_NOLANG}
{$ENDIF}

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  PasExt, MultiApp, Preferences;

type

  { TEditorForm }

  TEditorForm = class(TMultiAppForm)
  private
    FFileName: String;
    FOnSave: TNotifyEvent;
    procedure SetFileName(AValue: String);
    procedure SetOnSave(AValue: TNotifyEvent);
  protected
    pTop : TPanel;
    pBody : TPanel;
    pBottom : TPanel;
    pStatus : TStatusBar;
    mEdit : TMemo;
    btnClose : TButton;
    btnSave : TButton;
    procedure DoCreate; override;
    procedure DoClose(var CloseAction : TCloseAction); override;
    procedure OnCloseButton(Sender : TObject);
    procedure OnSaveButton(Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateNew(TheOwner: TComponent;Num:Integer=0); override;
    destructor Destroy; override;
    procedure ApplyUserLanguage; override;
    property FileName : String read FFileName write SetFileName;
    property OnSave : TNotifyEvent read FOnSave write SetOnSave;
  end;

var
  FileEditors : array of TEditorForm;

procedure FileEditor(FileName : String; OnSaveNotify : TNotifyEvent = nil);

implementation


function AddEditor(Sender : TEditorForm):integer;
var
  I : integer;
begin
  try
    for I := 0 to High(FileEditors) do
      if not Assigned(FileEditors[I]) then begin
        Result:=I;
        FileEditors[I]:=Sender;
        Exit;
      end;
    SetLength(FileEditors, Length(FileEditors) + 1);
    Result:=High(FileEditors);
    FileEditors[Result]:=Sender;
  except
    Result:=-1;
  end;
end;

procedure DeleteEditor(Sender : TEditorForm);
var
  I : Integer;
begin
  try
    for I := 0 to High(FileEditors) do
      if FileEditors[I] = Sender then begin
        FileEditors[I]:=nil;
        Break;
      end;
    while (Length(FileEditors) > 0) and (FileEditors[High(FileEditors)] = nil) do
      SetLength(FileEditors, Length(FileEditors) - 1);
  except
  end;
end;

procedure FileEditor(FileName: String; OnSaveNotify : TNotifyEvent = nil);
var
  Editor : TEditorForm;
  I : Integer;
begin
  LogMessage(vbNormal, 'Edit file: ' + FileName);
  Editor:=nil;
  for I := 0 to High(FileEditors) do
    if Assigned(FileEditors[I]) and (FileEditors[I].FileName=FileName) then begin
      Editor:=FileEditors[I];
      Break;
    end;
  if Not Assigned(Editor) then begin
    Editor:=TEditorForm.CreateNew(Application);
    Editor.OnSave:=OnSaveNotify;
    Editor.FileName:=FileName;
  end;
  Editor.ApplyUserLanguage;
  Editor.Show;
  Editor.BringToFront;
  Editor.SetFocus;
end;

{ TEditorForm }

procedure TEditorForm.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  try
    mEdit.Lines.LoadFromFile(FFileName);
  except
    mEdit.Clear;
    mEdit.Lines.Add('File Read Error');
    mEdit.Enabled:=False;
    mEdit.ReadOnly:=True;
    btnSave.Enabled:=False;
  end;
end;

procedure TEditorForm.SetOnSave(AValue: TNotifyEvent);
begin
  if FOnSave=AValue then Exit;
  FOnSave:=AValue;
end;

procedure TEditorForm.DoCreate;
begin
  inherited DoCreate;
  BorderStyle:=bsSizeToolWin;
  SetBounds(0,0,480,320);
  Position:=poScreenCenter;
  Constraints.MinWidth:=480;
  Constraints.MinHeight:=320;
  DesignTimePPI:=96;

  pStatus:=TStatusBar.Create(Self);
  pStatus.Parent:=Self;
  pStatus.SizeGrip:=True;
  pStatus.SimplePanel:=True;
  // Cludge to put Status Bar at the bottom of the form
  pStatus.Top:=100000;

  pBottom:=TPanel.Create(Self);
  FlattenControl(pBottom);
  //pBottom.BorderStyle:=bsNone;
  pBottom.Parent:=Self;
  pBottom.BorderStyle:=bsNone;
  pBottom.BorderSpacing.Around:=8;
  pBottom.AutoSize:=True;
  // Cludge to put Bottom Panel above the Status Bar.
  pStatus.Top:=99999;

  pTop:=TPanel.Create(Self);
  FlattenControl(pTop);
  pTop.BorderStyle:=bsNone;
  pTop.Parent:=Self;
  pTop.BorderSpacing.Around:=8;
  pTop.Visible:=False;

  pBody:=TPanel.Create(Self);
  FlattenControl(pBody);
  pBody.BorderStyle:=bsNone;
  pBody.Parent:=Self;
  pBody.BorderSpacing.Around:=8;

  pStatus.Align:=alBottom;
  pBottom.Align:=alBottom;
  pTop.Align:=alTop;
  pBody.Align:=alClient;

  mEdit:=TMemo.Create(Self);
  mEdit.Parent:=pBody;
  mEdit.Clear;
  mEdit.Align:=alClient;
  mEdit.ScrollBars:=ssAutoBoth;
  mEdit.WordWrap:=False;
  mEdit.Font.Name:='Monospace';

  btnSave:=TButton.Create(Self);
  btnSave.Parent:=pBottom;
  btnSave.ModalResult:=mrCancel;
  btnSave.AutoSize:=True;
  btnSave.Align:=alRight;
  btnSave.BorderSpacing.Around:=8;

  btnClose:=TButton.Create(Self);
  btnClose.Parent:=pBottom;
  btnClose.ModalResult:=mrCancel;
  btnClose.AutoSize:=True;
  btnClose.Align:=alRight;
  btnClose.BorderSpacing.Around:=8;

  btnSave.OnClick:=@OnSaveButton;
  btnClose.OnClick:=@OnCloseButton;

end;

procedure TEditorForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caFree;
end;

procedure TEditorForm.OnCloseButton(Sender: TObject);
begin
  Close;
end;

procedure TEditorForm.OnSaveButton(Sender: TObject);
begin

end;

constructor TEditorForm.Create(TheOwner: TComponent);
var
  Idx : Integer;
begin
  inherited Create(TheOwner);
  Idx:=AddEditor(Self);
  Name:='fFileEditor' + WhenTrue(IDX > 0, '_'+IntToStr(Idx));
end;

constructor TEditorForm.CreateNew(TheOwner: TComponent; Num: Integer);
var
  Idx : Integer;
begin
  inherited CreateNew(TheOwner, Num);
  Idx:=AddEditor(Self);
  Name:='fFileEditor' + WhenTrue(IDX > 0, '_'+IntToStr(Idx));
end;

destructor TEditorForm.Destroy;
begin
  DeleteEditor(Self);
  inherited Destroy;
end;

procedure TEditorForm.ApplyUserLanguage;
var
  K, S, V, BS, BC : String;
begin
  // Adjust Language Strings Based on first Editor Form, not current index
  // inherited ApplyUserLanguage;
  K:='/Controls/fFileEditor/';
  S:='Codepage Witch - %s';
  BC:='Cancel';
  BS:='Save';
  if Assigned(Translations) then begin
    {$IFDEF SUPPORT_NOLANG}
    No_Language.SetValue(UnicodeString(K+'Caption'), UnicodeString(S));
    No_Language.SetValue(UnicodeString(K+'btnCancel/Caption'), UnicodeString(BC));
    No_Language.SetValue(UnicodeString(K+'btnSave/Caption'), UnicodeString(BS));
    {$ENDIF}
    V:=RawByteString(Translations.GetValue(UnicodeString(K+'Caption'), UnicodeString(S)));
    BC:=RawByteString(Translations.GetValue(UnicodeString(K+'btnCancel/Caption'), UnicodeString(BC)));
    BS:=RawByteString(Translations.GetValue(UnicodeString(K+'btnSave/Caption'), UnicodeString(BS)));
  end;
  try
    S:=Format(V, [ExtractFileName(FileName)]);
  finally
  end;
  Caption:=S;
  pStatus.SimpleText:=SPACE2 + FileName;
  btnClose.Caption:=BC;
  btnSave.Caption:=BS;
end;

initialization

  FileEditors:=[];

end.

