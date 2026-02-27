{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit uDictEdit;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Version, PasExt, Icons, MultiApp, Witch, Codepages, Dictionary;

type

  { TfDictEditForm }

  TfDictEditForm = class(TMultiAppForm)
    cbLocale: TComboBox;
    lbLocale: TLabel;
    pTop: TPanel;
    pStatusBar: TStatusBar;
  private
    FWitchItem: TWitchItem;
    procedure SetWitchItem(AValue: TWitchItem);
  protected
    procedure DoUpdateLocales;
    procedure DoCreate; override;
    procedure DoShow; override;
  public
    property WitchItem : TWitchItem read FWitchItem write SetWitchItem;


  end;

var
  fDictEditForm: TfDictEditForm;

implementation

{$R *.lfm}

{ TfDictEditForm }

procedure TfDictEditForm.SetWitchItem(AValue: TWitchItem);
begin
  if FWitchItem=AValue then Exit;
  FWitchItem:=AValue;
end;

procedure TfDictEditForm.DoUpdateLocales;
var
  I : Integer;
  L, LL, LLL : String;
  LA : TArrayOfString;
begin
  cbLocale.Clear;
  if not Assigned(Dictionaries) then Exit;
  LL := Dictionaries.Languages;
  LLL:='';
  while LL <> '' do begin
    L:=Trim(PopDelim(LL, COMMA));
    if L = '' then Continue;
    cbLocale.Items.Add(L);
    Cat(LLL, ';' + UpperCase(L) + ';');
  end;
  LA:=LocaleList;
  for I := 0 to High(LA) do begin
    if Pos(';' + UpperCase(LA[I]) + ';', LLL) > 0 then Continue;
    cbLocale.Items.Add(LA[I]);
    Cat(LLL, ';' + UpperCase(LA[I]) + ';');
  end;
end;

procedure TfDictEditForm.DoCreate;
begin
  inherited DoCreate;
  FWitchItem:=nil;
end;

procedure TfDictEditForm.DoShow;
begin
  inherited DoShow;
  DoUpdateLocales;
end;

initialization

  fDictEditForm:=nil;

end.

