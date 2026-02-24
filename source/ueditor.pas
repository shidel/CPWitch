{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uEditor;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

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
    procedure DoCreate; override;
    procedure DoClose(var CloseAction : TCloseAction); override;
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
begin
  LogMessage(vbNormal, 'Edit file: ' + FileName);
  Editor:=TEditorForm.CreateNew(Application);
  Editor.OnSave:=OnSaveNotify;
  Editor.FileName:=FileName;
  Editor.ApplyUserLanguage;
  Editor.Show;
end;

{ TEditorForm }

procedure TEditorForm.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
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

end;

procedure TEditorForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caFree;
end;

constructor TEditorForm.Create(TheOwner: TComponent);
var
  Idx : Integer;
begin
  inherited Create(TheOwner);
  Idx:=AddEditor(Self);
  Name:='fFileEditor_'+IntToStr(Idx);
end;

constructor TEditorForm.CreateNew(TheOwner: TComponent; Num: Integer);
var
  Idx : Integer;
begin
  inherited CreateNew(TheOwner, Num);
  Idx:=AddEditor(Self);
  Name:='fFileEditor_'+IntToStr(Idx);
end;

destructor TEditorForm.Destroy;
begin
  DeleteEditor(Self);
  inherited Destroy;
end;

procedure TEditorForm.ApplyUserLanguage;
var
  K, S : String;
begin
  inherited ApplyUserLanguage;
  K:='Controls/fFileEditor/';
  S:=GetTranslation(K+'Caption', 'Codepage Witch - %s');
  try
    S:=Format(S, [ExtractFileName(FileName)]);
  except
    S:='resource error';
  end;
  Caption:=S;
end;

initialization

  FileEditors:=[];

end.

