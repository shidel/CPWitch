{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uDiffLocale;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  PasExt, MultiApp, Icons;

type

  { TfDiffLocaleDialog }

  TfDiffLocaleDialog = class(TMultiAppDialog)
  private
    lbMessage : TLabel;
    FExpected: String;
    FRequested: String;
    procedure SetExpected(AValue: String);
    procedure SetRequested(AValue: String);

  protected
    procedure DoCreate; override;
    procedure DoButton; override;
    procedure DoShow; override;
  public
    property Expected : String read FExpected write SetExpected;
    property Requested : String read FRequested write SetRequested;

  end;

var
  fDiffLocaleDialog: TfDiffLocaleDialog;

function DifferentLocale(Expected, Requested : String): TModalResult;

implementation

function DifferentLocale(Expected, Requested : String): TModalResult;
begin
  if not Assigned(fDiffLocaleDialog) then begin
    fDiffLocaleDialog:=TfDiffLocaleDialog.CreateNew(Application);
  end;
  fDiffLocaleDialog.Expected := Expected;
  fDiffLocaleDialog.Requested := Requested;
  fDiffLocaleDialog.ShowModal;
  Result:=fDiffLocaleDialog.ClickResult;
end;


{$R *.lfm}

{ TfDiffLocaleDialog }

procedure TfDiffLocaleDialog.SetExpected(AValue: String);
begin
  if FExpected=AValue then Exit;
  FExpected:=AValue;
end;

procedure TfDiffLocaleDialog.SetRequested(AValue: String);
begin
  if FRequested=AValue then Exit;
  FRequested:=AValue;
end;

procedure TfDiffLocaleDialog.DoCreate;
begin
  Name:='fDiffLocaleDialog';
  Caption:='Conflicting Locale';
  Images:=ilDialogColor;
  ImageIndex:=idxDialogQuestion;
  ImageVisible:=True;
  Buttons:=['Cancel', 'Proceed'];
  inherited DoCreate;
  lbMessage:=TLabel.Create(Self);
  lbMessage.Parent:=BodyPanel;
  lbMessage.AutoSize:=False;
  lbMessage.Align:=alClient;
  lbMessage.Layout:=tlCenter;
  lbMessage.WordWrap:=True;
end;

procedure TfDiffLocaleDialog.DoButton;
begin
  inherited DoButton;
  if ClickResult = 1 then
    ClickResult:=mrOk // Proceed
  else
    ClickResult:=mrCancel;
end;

procedure TfDiffLocaleDialog.DoShow;
begin
  inherited DoShow;
  lbMessage.Caption:=GetFormat('Message/Text', [Expected,Requested],
  'The locale for this file is expected to be %s.'+ LF + LF +
  'Are you certian you wish to add to the dictionary under the %s locale?');
end;

initialization

 fDiffLocaleDialog:=nil;

end.

