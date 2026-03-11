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

{ TODO 8 -cDevel Add Dictionary Locking for when threads are analyzing data. }

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
    btnNone: TButton;
    btnMerge: TButton;
    cbLocale: TComboBox;
    clWords: TCheckListBox;
    lbLocale: TLabel;
    pBody: TPanel;
    pButtons: TPanel;
    pTop: TPanel;
    pStatusBar: TStatusBar;
    procedure btnAddClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
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
  S, L, R: String;
  N, X : Integer;
begin
  S:=Trim(cbLocale.Text);
  R :=UpperCase(S);
  N:=Pos(UNDERSCORE, R);
  L:=LowerCase(PopDelim(R, UNDERSCORE));
  if N <> 0 then Cat(L, UNDERSCORE);
  if R <> '' then Cat(L, R);
  if S <> L then begin
    X := cbLocale.SelStart;
    cbLocale.Text:=L;
    cbLocale.SelStart:=X;
    S:=L;
  end;
  if Locale(L, LC) then begin
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

procedure TfDictEditForm.btnMergeClick(Sender: TObject);
begin
  MergeDictionaries;
  DoUpdateButtons;
end;

procedure TfDictEditForm.btnNoneClick(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to clWords.Count - 1 do
    clWords.Checked[I]:=False;
end;

procedure TfDictEditForm.btnReloadClick(Sender: TObject);
begin
  ReloadDictionaries;
  DoUpdateWordList;
  DoWitchReanalyze;
end;

procedure TfDictEditForm.btnSaveClick(Sender: TObject);
begin
  if not Assigned(UserDictionary) then Exit;
  UserDictionary.Save;
  if Assigned(MasterDictionary) and (MasterDictionary.Modified) then
    MasterDictionary.Save;
  DoUpdateWordList;
end;

procedure TfDictEditForm.btnAddClick(Sender: TObject);
var
  LC : Int32;
  I : Integer;
  S : String;
  SS, TS : TArrayOfInt32;
  DN : TBinaryTreeNode;
begin
  if not Assigned(UserDictionary) then Exit;
  if Trim(cbLocale.Text) = '' then Exit;
  LC:=UserDictionary.AddLocale(Trim(cbLocale.Text));
  SS:=[LC];
  for I := 0 to clWords.Count - 1 do
    if clWords.State[I] = cbChecked then begin
      S:=LowerCase(clWords.Items[I]);
      DN:=UserDictionary.Find(S);
      if not Assigned(DN) then
        DN:=UserDictionary.Add(S, SS)
      else if InArray(DN.Data32, LC) = -1 then begin
        TS:=DN.Data32;
        Cat(TS, LC);
        DN.Data32:=TS;
      end;
    end;
  UserDictionary.Modified:=True;
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
  if not Assigned(UserDictionary) then Exit;
  LL := UserDictionary.Locales;
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
  if not FWitchItem.Analyzed then Exit;
  if FWitchItem.Encoding = weBinary then Exit;
  if FWitchItem.Encoding = weCodepage then Exit;
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
      if (Length(W[I]) > 1) and (Not Assigned(FWords.Find(LowerCase(W[I])))) then
        FWords.Add(W[I]);
    end;
  end;
  W.Free;
  LogMessage(vbVerbose, 'File contains ' + IntToStr(FWords.Count) + ' unique usable words.');
end;

procedure TfDictEditForm.DoUpdateWordList;
var
  LC : Int32;
  N, M, U : TBinaryTreeNode;
  SL : Array of String;
  SC, I : integer;
  L : String;
begin
  clWords.Clear;
  DoUpdateButtons;
  if FWords.Count = 0 then Exit;
  if Not Assigned(MasterDictionary) then Exit;
  if Not Assigned(UserDictionary) then Exit;
  SL:=[];
  SC:=0;
  SetLength(SL, FWords.Count);
  LC:=UserDictionary.IndexOfLocale(Trim(cbLocale.Text));
  N:=FWords.First;
  if LC = -1 then begin
    // Unknown (New) locale, only add words in neither dictionary.
    // Once the user actually adds a word, then the locale will be known and
    // more words will be presented.
    while Assigned(N) do begin
      L:=LowerCase(N.UniqueID);
      if (not Assigned(MasterDictionary.Find(L)))
      and (not Assigned(UserDictionary.Find(L))) then begin
        SL[SC]:=N.UniqueID; // Added to selection list in actual case of word.
        Inc(SC);
      end;
      N:=N.Next;
    end;
  end else begin
    while Assigned(N) do begin
      // Known locale, only add word if it is not already listed for this
      // locale in either dictionary.
      L:=LowerCase(N.UniqueID);
      M:=MasterDictionary.Find(L);
      if (not Assigned(M)) or (InArray(M.Data32, LC) = -1) then begin
        U:=UserDictionary.Find(L);
        if (not Assigned(U)) or (InArray(U.Data32, LC) = -1) then begin
          SL[SC]:=N.UniqueID;
          Inc(SC);
        end;
      end;
      N:=N.Next;
    end;
  end;
  SetLength(SL, SC);
  if SC > 0 then begin
    clWords.Items.AddStrings(SL);
    // Of the words added to the Selection List. Only preselect words,
    // that are not in any other local, over 4 letters long, not all uppercase
    // and only if it is a "known" locale.
    for I := 0 to clWords.Items.Count - 1 do begin
      L:=LowerCase(clWords.Items[I]);
      M:=MasterDictionary.Find(L);
      U:=UserDictionary.Find(L);
      if (LC <> -1) and (Not Assigned(M)) and (Not Assigned(U)) then begin
        if (Length(clWords.Items[I]) > 5) and
        ((Uppercase(clWords.Items[I]) <> clWords.Items[I]) or
        (Uppercase(clWords.Items[I]) = LowerCase(clWords.Items[I]))) then
          clWords.Checked[I]:=True
      end;
    end;
  end;
  DoUpdateButtons;
end;

procedure TfDictEditForm.DoUpdateButtons;
var
  X : Boolean;
  L, R : String;
begin
  R:=Trim(cbLocale.Text);
  L:=PopDelim(R, UNDERSCORE);
  X:=((Length(L)=2) and ((Length(R) = 0) or (Length(R) = 2) or (Length(R) = 3)))
  or ((Length(L)=3) and ((Length(R) = 0) or (Length(R) = 2)))

  btnAdd.Enabled:=X and (clWords.Items.Count > 0) and (Trim(cbLocale.Text) <> '');
  btnInvert.Enabled:=btnAdd.Enabled;
  btnNone.Enabled:=btnAdd.Enabled;
  btnSave.Enabled:=Assigned(UserDictionary) and (UserDictionary.Modified);
  btnReload.Enabled:=btnSave.Enabled;
  {$IFDEF BUILD_PRIVATE}
    btnMerge.Visible:=True;
    btnMerge.Enabled:=Assigned(MasterDictionary) and Assigned(UserDictionary) and
      (UserDictionary.Count > 0);
  {$ELSE}
    btnMerge.Visible:=False;
    btnMerge.Enabled:=False;
  {$ENDIF}
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

