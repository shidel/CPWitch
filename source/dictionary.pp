{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

{ TODO 1 -cDevel Remove need for an external codepages.ini file. }

{ This unit requires an external data file called codepages.ini to be stored
  in the "AppDataPath" set by the PasExt Unit. Eventually, that data will likely
  be embedded into this unit to remove the requirement of the external file. }

unit Dictionary;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, PasExt, BinTree;

type

  { TDictionaries }

  TDictionaries = class(TBinaryTree)
    FLocales : TArrayOfRawByteString;

    FLoaded : boolean;
    FModified : boolean;
  private
    FFileName: String;
    function GetLocalCount: integer;
    function GetLocale(Index : integer): RawByteString;
    function GetLocales: RawByteString;
    procedure SetFileName(AValue: String);
    procedure Load;
    procedure SetModified(AValue: boolean);
  public
    constructor Create; override;
    procedure Clear; override;
    property FileName : String read FFileName write SetFileName;
    procedure Reload;
    procedure Save;
    property Locales : RawByteString read GetLocales;
    property LocaleCount : integer read GetLocalCount;
    property Locale[Index : integer] : RawByteString read GetLocale;
    property Modified : boolean read FModified write SetModified;
    function AddLocale(LocaleID : String) : integer;
    function IndexOfLocale(LocaleID : String) : integer;
  end;

var
  Dictionaries : TDictionaries;
  UserDictionary : TDictionaries;

  function DetectLocale(S : RawByteString; out Stats : TArrayOfInt32) : Int32; overload;
  function DetectLocale(S : RawByteString) : String; overload;

implementation

{$UNDEF CaseSpecific}
{$UNDEF StressTest}

procedure Initialize;
begin
  Dictionaries:=TDictionaries.Create;
  try
    Dictionaries.FileName:=AppDataPath+'dictionary.cpw';
  except
    FreeAndNil(Dictionaries);
  end;
  UserDictionary:=TDictionaries.Create;
  try
    UserDictionary.FileName:=UserDataPath+'user_words.cpw';
  except
    FreeAndNil(UserDictionary);
  end;
end;

procedure Finalize;
begin
  if Assigned(Dictionaries) then begin
    // if Dictionaries.FModified then
    //  Dictionaries.Save;
    FreeAndNil(Dictionaries);
  end;
end;

{ TDictionaries }

procedure TDictionaries.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  if not FLoaded then
    Reload;
end;

function TDictionaries.GetLocalCount: integer;
begin
  Result:=Length(FLocales);
end;

function TDictionaries.GetLocale(Index : integer): RawByteString;
begin
  if (Index < 0) or (Index > High(FLocales)) then Exit('');
  Result:=FLocales[Index];
end;

function TDictionaries.GetLocales: RawByteString;
var
  SL : TStringList;
  I : Integer;
begin
  SL := TStringList.Create;
  try
    for I := 0 to High(FLocales) do
      SL.Add(FLocales[I]);
    SL.Sort;
    Result:=Implode(SL, COMMA + SPACE);
  except
    Result:='';
  end;
  SL.Free;
end;

{$IFDEF StressTest}
procedure StressTest;
var
  N, T : TBinaryTreeNode;
  C, I : Integer;
  B : Boolean;
begin

  N:=Dictionaries.First;
  C:=Dictionaries.Count;
  B:=True;
  try
    While C > 0 do begin
      T:=N;
      for I := 0 to Random(100) do begin
        While (T=N) and (C>1) do begin
          if B then T:=T.Next else T:=T.Previous;
          if not Assigned(T) then begin
            if not Dictionaries.CheckIntegrity then
              raise Exception.Create('failed integrity');
            if B then
              T:=Dictionaries.Last
            else
              T:=Dictionaries.First;
            B:=Not B;
            LogMessage(vbVerbose, 'Direction Change');
          end;
        end;
      end;
      if not assigned(N) then
        raise Exception.Create('Current Node is nil');
      if not assigned(T) then
        raise Exception.Create('Next Node is nil');
      Dictionaries.Delete(N);
      Dec(C);
      if N <> T then
        N:=T
      else
        N:=Dictionaries.Root;
    end;
    LogMessage(vbVerbose,'Tree Should be empty');
    if C <> 0 then
      raise Exception.Create ('Did not remove all items');
    if Dictionaries.Count <> 0 then
      raise Exception.Create ('Item count is off');
    if Dictionaries.First <> nil then
      raise Exception.Create ('Tree not empty');
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Stress test failed at ' +  IntToStr(C) +
      '. ' + E.Message);
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure StressTest2;
var
  C : Integer;
  N : TBinaryTreeNode;
begin
  C:=Dictionaries.Count;
  try
    While C > 0 do begin
      N:=Dictionaries.Root;
      Dictionaries.Delete(N);
      Dec(C);
    end;
    LogMessage(vbVerbose,'Tree Should be empty');
    if C <> 0 then
      raise Exception.Create ('Did not remove all items');
    if Dictionaries.Count <> 0 then
      raise Exception.Create ('Item count is off');
    if Dictionaries.First <> nil then
      raise Exception.Create ('Tree not empty');
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Stress test failed at ' +  IntToStr(C) +
      '. ' + E.Message);
      raise Exception.Create(E.Message);
    end;
  end;
end;

{$ENDIF}

procedure TDictionaries.Load;
var
  Sect, E, J : integer;
  ST : TArrayOfInt32;
  FileText : RawByteString;
  Line : RawByteString;
  U : UnicodeString;
  N : TBinaryTreeNode;
  S : String;
  WC:Integer;
begin
  WC:=0;
  if not FileExists(FileName) then begin
    LogMessage(vbMinimal, 'Cannot find dictionary file: ' + FriendlyPath(AppBasePath, FileName));
    Exit;
  end;

  E :=FileLoad(FileName, FileText);
  if E <> 0 then
    raise Exception.Create('reading file: ' + FriendlyPath(AppBasePath, FileName));
  try
    LogMessage(vbVerbose, 'Processing dictionary file: '+ FriendlyPath(AppBasePath, FileName));
    Sect:=-1;
    FileText:=StringReplace(FileText, CR, LF, [rfReplaceAll]);
    while Length(FileText) > 0 do begin
      Line:=Trim(PopDelim(FileText, LF));
      if Length(Line) = 0 then Continue;
      if HasEnds(Line, '[', ']') then begin
        Line:=StringReplace(Trim(ExcludeEnds(Line, '[', ']')), '-', '_', [rfReplaceAll]);
        if Line = '' then
          raise Exception.Create('null section name');
        if IndexOfLocale(Line) <> -1 then
          raise Exception.Create('duplicate section name: ' + Line);
        Sect:=AddLocale(Line);
        LogMessage(vbExcessive, TAB + 'Language: ' + Locale[Sect]);
        Continue;
      end;
      if HasLeading(Line, ';') then Continue;
      if HasLeading(Line, '#') then Continue;
      while Length(Line) > 0 do begin
        U:=UnicodeString(Trim(PopDelim(Line, COMMA)));
        if Length(U) = 0 then Continue;
        {$IFNDEF CaseSpecific}
        U:=Lowercase(U);
        {$ENDIF}
        N:=Find(U);
        if not Assigned(N) then begin
          N:=Add(U, [Sect]);
          Inc(WC);
        end else begin
          if InArray(N.Data32, Sect) <> -1 then begin
            S:='';
            for J:= 0 to Length(N.Data32) - 1 do
              S:=S+Locale[J] + SPACE;
            LogMessage(vbVerbose, WhenTrue(VerboseLevel=vbExcessive,
              TAB + 'duplicate word: ', 'duplicate dictionary word: ') +
              RawByteString(U) + ' [' + Locale[Sect] + '] ' + S + IntToStr(InArray(N.Data32, Sect)));
            Continue;
          end;
          ST:=N.Data32;
          Cat(ST, Sect);
          N.Data32:=ST;
        end;
      end;
    end;
    LogMessage(vbExcessive, 'Balancing dictionary');
    Optimize;
    LogMessage(vbExcessive, 'Verifying tree integrity');
    CheckIntegrity;
  finally
    LogMessage(vbNormal, 'Dictionary contains ' + IntToStr(WC) + ' unique words and ' +
      IntToStr(Length(FLocales)) + ' locales.' + LF + TAB+Locales);
  end;
  fModified:=False;

end;

procedure TDictionaries.SetModified(AValue: boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
end;

procedure TDictionaries.Reload;
begin
  FModified:=False;
  try
    {$IFDEF StressTest}
    Clear;
    Load;
    LogMessage(vbVerbose, 'Stress Test #1');
    StressTest;
    FLocales:=[];
    FLoaded:=False;
    Load;
    LogMessage(vbVerbose, 'Stress Test #2');
    StressTest;
    {$ENDIF}
    Clear;
    Load;
    FLoaded:=True;
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Exception opening dictionary. ' + E.Message);
      Clear;
     end;
  end;
end;

procedure TDictionaries.Save;
var
  I : Integer;
  SS : TStringStream;
  N : TBinaryTreeNode;
  LW : integer;
begin
  FModified:=False;
  if FFileName = '' then Exit;
  LogMessage(vbVerbose, 'Saving dictionary file: '+ ExtractRelativepath(AppBasePath, FileName));
  SS:=TStringStream.Create('');
  try
    SS.Position:=0;
    for I := 0 to High(FLocales) do begin
      SS.WriteString('[' + FLocales[I] + ']' + LF + LF);
      N:=First;
      LW:=0;
      While Assigned(N) do begin
        if InArray(N.Data32, I) <> -1 then begin
          if (LW > 0) then begin
            if (LW + Length(N.UniqueID) + 2 > 79) then begin
              LW:=0;
              SS.WriteString(LF)
            end else begin
              Inc(LW, 2);
              SS.WriteString(COMMA + SPACE)
            end;
          end;
          Inc(LW, Length(N.UniqueID));
          SS.WriteString(N.UniqueID);
        end;
        N:=N.Next;
      end;
      if LW <> 0 then
        SS.WriteString(LF);
      if I < High(FLocales) then
        SS.WriteString(LF);
    end;
    SS.SaveToFile(FFileName);
    LogMessage(vbVerbose, 'saved dictionary file. ' + IntToStr(Dictionaries.Count) +
     ' unique words.');
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Exception saving dictionary. ' + E.Message);
     end;
  end;
  SS.Free;
end;

function TDictionaries.AddLocale(LocaleID: String) : integer;
begin
  Result:=IndexOfLocale(LocaleID);
  if Result <> -1 then Exit;
  SetLength(FLocales, Length(FLocales) + 1);
  FLocales[High(FLocales)]:=LocaleID;
  FModified:=True;
  Result:=High(FLocales);
end;

function TDictionaries.IndexOfLocale(LocaleID: String): integer;
var
  I : Integer;
begin
  for I := 0 to Length(FLocales) -1 do
    if UpperCase(LocaleID) = UpperCase(FLocales[I]) then Exit(I);
  Result:=-1;
end;

constructor TDictionaries.Create;
begin
  inherited Create;
  FLoaded:=False;
  FLocales:=[];
  FModified:=False;
end;

procedure TDictionaries.Clear;
begin
  inherited Clear;
  FLocales:=[];
  FLoaded:=False;
  FModified:=False;
end;

{ Unit functions }

function DetectDict(D : TDictionaries; S: RawByteString; out Stats: TArrayOfInt32
  ): Int32;
var
  I, J, P, L : integer;
  W : TStringList;
  N : TBinaryTreeNode;
begin
  Result:=0;
  Stats:=[];
  SetLength(Stats, Length(D.FLocales));
  for I := 0 to High(Stats) do
    Stats[I]:=0;
  W := TStringList.Create;
  try
    WordsOfString(S, W);
    for I := 0 to W.Count -1 do begin
      N:=D.Find(LowerCase(W[I]));
      if Assigned(N) then begin
        L := Length(W[I]);
        if L > 11 then L:=10;
        P:=High(N.Data32);
        P:=L-(P*P);
        if P < 1 then P:=1;
        for J := 0 to High(N.Data32) do
          Inc(Stats[N.Data32[J]], P);
      end;
    end;
    Result:=W.Count;
  finally
    W.Free;
  end;
end;

function DetectLocale(S: RawByteString; out Stats: TArrayOfInt32
  ): Int32;
begin
  Stats:=[];
  if Not Assigned(Dictionaries) then Exit(0);
  try
    Result:=DetectDict(Dictionaries, S, Stats);
  except
    Stats:=[];
    Result:=0;
  end;
end;

function DetectLocale(S: RawByteString): String;
var
  I, P : integer;
  Stats:TArrayOfInt32;
begin
  if Not Assigned(Dictionaries) then Exit('');
  Result:='';
  DetectLocale(S, Stats);
  P:=-1;
  for I := 0 to High(Stats) do
    if (P<0) or (Stats[I]>Stats[P]) then P:=I;
  if P<0 then Exit;
  Result:=Dictionaries.FLocales[P];
end;


initialization

  Initialize;

finalization

  Finalize;

end.

