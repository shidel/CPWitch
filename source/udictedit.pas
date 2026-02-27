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
  ComCtrls, CheckLst, Version, PasExt, Icons, MultiApp, Witch, Codepages,
  BinTree, Dictionary;

type

  { TfDictEditForm }

  TfDictEditForm = class(TMultiAppForm)
    btnSave: TButton;
    btnReload: TButton;
    btnInvert: TButton;
    btnAdd: TButton;
    cbLocale: TComboBox;
    clWords: TCheckListBox;
    lbLocale: TLabel;
    pBody: TPanel;
    pButtons: TPanel;
    pTop: TPanel;
    pStatusBar: TStatusBar;
    procedure btnAddClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbLocaleChange(Sender: TObject);
  private
    FWitchItem: TWitchItem;
    FWitchList: TWitch;
    FWords : TBinaryTree;
    procedure SetWitchItem(AValue: TWitchItem);
    procedure SetWitchList(AValue: TWitch);
  protected
    procedure DoUpdateLocales;
    procedure DoUpdateWords;
    procedure DoUpdateWordList;
    procedure DoUpdateButtons;
    procedure DoWitchReanalyze;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoShow; override;
  public
    property WitchItem : TWitchItem read FWitchItem write SetWitchItem;
    property WitchList : TWitch read FWitchList write SetWitchList;

  end;

var
  fDictEditForm: TfDictEditForm;

implementation

{$R *.lfm}

{ TfDictEditForm }

procedure TfDictEditForm.cbLocaleChange(Sender: TObject);
var
  LC : TLocale;
begin
  if Locale(Trim(cbLocale.Text), LC) then begin
    pStatusBar.Panels[0].Text:=LC.Identifier;
    pStatusBar.Panels[1].Text:=LC.LetterCode;
    if LC.Codepage = -1 then
      pStatusBar.Panels[2].Text:='n/a'
    else
      pStatusBar.Panels[2].Text:=IntToStr(LC.Codepage);
    pStatusBar.Panels[3].Text:=SPACE2+RawByteString(LC.Language);
  end else begin
    pStatusBar.Panels[0].Text:=Trim(cbLocale.Text);
    pStatusBar.Panels[1].Text:='n/a';
    pStatusBar.Panels[2].Text:='n/a';
    pStatusBar.Panels[3].Text:=SPACE2+'(unknown locale)';
  end;
  if FWords.Count < 10 then
    DoUpdateWords;
  DoUpdateWordList;
end;

procedure TfDictEditForm.btnInvertClick(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to clWords.Count - 1 do
    clWords.Checked[I]:= not clWords.Checked[I];
end;

procedure TfDictEditForm.btnReloadClick(Sender: TObject);
begin
  if not Assigned(Dictionaries) then Exit;
  Dictionaries.Reload;
  DoUpdateWordList;
  DoWitchReanalyze;
end;

procedure TfDictEditForm.btnSaveClick(Sender: TObject);
begin
  if not Assigned(Dictionaries) then Exit;
  Dictionaries.Save;
  DoUpdateWordList;
end;

procedure TfDictEditForm.btnAddClick(Sender: TObject);
var
  I : Integer;
  LCA, LCB, S : String;
  DN : TBinaryTreeNode;
begin
  if not Assigned(Dictionaries) then Exit;
  LCB:=Trim(cbLocale.Text);
  if LCB = '' then Exit;
  Dictionaries.AddLocale(LCB);
  LCB:=LCB + ';';
  LCA:=';'+LCB;
  for I := 0 to clWords.Count - 1 do
    if clWords.Checked[I] then begin
      S:=LowerCase(clWords.Items[I]);
      DN:=Dictionaries.Find(S);
      if not Assigned(DN) then
        DN:=Dictionaries.Add(S, PasExt.ToBytes(LCA))
      else if Pos(LCA, PasExt.ToString(DN.Data))=0 then
        DN.Data:=PasExt.ToBytes(PasExt.ToString(DN.Data) + LCB);
    end;
  Dictionaries.Modified:=True;
  DoUpdateWordList;
  DoWitchReanalyze;
end;

procedure TfDictEditForm.SetWitchItem(AValue: TWitchItem);
begin
  if FWitchItem=AValue then Exit;
  FWitchItem:=AValue;
  DoUpdateWords;
  DoUpdateWordList;
end;

procedure TfDictEditForm.SetWitchList(AValue: TWitch);
begin
  if FWitchList=AValue then Exit;
  FWitchList:=AValue;
end;

procedure TfDictEditForm.DoUpdateLocales;
var
  I : Integer;
  L, LL, LLL : String;
  LA : TArrayOfString;
begin
  cbLocale.Clear;
  if not Assigned(Dictionaries) then Exit;
  LL := Dictionaries.Locales;
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

procedure TfDictEditForm.DoUpdateWords;
var
  S, Line : RawByteString;
  P : integer;
  W : TStringList;
  I : Integer;
begin
  FWords.Clear;
  if not Assigned(FWitchItem) then Exit;
  LogMessage(vbVerbose, 'Detecting words in file data...');
  W:=TStringList.Create;
  S:=NormalizeLineEndings(PasExt.ToString(FWitchItem.FileData), LF) + LF;
  while Length(S) > 0 do begin
    Line:=Trim(PopDelim(S, LF));
    if HasLeading(Line, ';') or HasLeading(Line, '#') then Continue;
    P := Pos(EQUAL, Line);
    if P <> 0 then begin
      if Pos(SPACE, Copy(Line, 1, P)) = 0 then
        Line:=Trim(Copy(Line, P+1));
    end;
    if Line = '' then Continue;
    W.Clear;
    WordsOfString(Line, W);
    for I := 0 to W.Count - 1 do begin
      if (Length(W[I]) > 1) and (Not Assigned(FWords.Find(W[I], false))) then
        FWords.Add(W[I]);
    end;
  end;
  W.Free;
  LogMessage(vbVerbose, 'File contains ' + IntToStr(FWords.Count) + ' unique usable words.');
end;

procedure TfDictEditForm.DoUpdateWordList;
var
  LC : String;
  FN, DN : TBinaryTreeNode;
  SL : Array of String;
  SC : integer;
begin
  clWords.Clear;
  DoUpdateButtons;
  if FWords.Count = 0 then Exit;
  if Not Assigned(Dictionaries) then Exit;
  SL:=[];
  SC:=0;
  SetLength(SL, FWords.Count);
  LC:=Trim(cbLocale.Text);
  if LC = '' then Exit;
  LC:=';' + LC + ';';
  FN:=FWords.First;
  while Assigned(FN) do begin
    DN:=Dictionaries.Find(Lowercase(FN.UniqueID));
    if (not Assigned(DN)) or (Pos(LC, PasExt.ToString(DN.Data)) = 0) then begin
      SL[SC]:=LowerCase(FN.UniqueID);
      Inc(SC);
    end;
    FN:=FN.Next;
  end;
  SetLength(SL, SC);
  if SC > 0 then begin
    clWords.Items.AddStrings(SL);
  end;
  DoUpdateButtons;
end;

procedure TfDictEditForm.DoUpdateButtons;
begin
  btnAdd.Enabled:=(clWords.Items.Count > 0) and (Trim(cbLocale.Text) <> '');
  btnInvert.Enabled:=btnAdd.Enabled;
  btnSave.Enabled:=Assigned(Dictionaries) and (Dictionaries.Modified);
  btnReload.Enabled:=btnSave.Enabled;
end;

procedure TfDictEditForm.DoWitchReanalyze;
var
  I : Integer;
begin
  if Not Assigned(FWitchList) then Exit;
  for I := 0 to FWitchList.Count - 1 do
    FWitchList.Modified[I]:=True;
end;

procedure TfDictEditForm.DoCreate;
begin
  inherited DoCreate;
  FWitchItem:=nil;
  FWords:=TBinaryTree.Create;
end;

procedure TfDictEditForm.DoDestroy;
begin
  if Assigned(FWords) then
    FreeAndNil(FWords);
  inherited DoDestroy;
end;

procedure TfDictEditForm.DoShow;
begin
  inherited DoShow;
  DoUpdateLocales;
  DoUpdateWords;
  DoUpdateWordList;
  DoUpdateButtons;
end;

initialization

  fDictEditForm:=nil;

end.

