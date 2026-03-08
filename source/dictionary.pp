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

{$UNDEF CaseSpecific}

type

  { TCustomDictionary }

  TCustomDictionary = class(TBinaryTree)
    FLoaded : boolean;
    FLocaleRemap : TArrayOfInt32;
  private
    FFileName: String;
    function GetLocaleCount: integer; virtual;
    function GetLocale(Index : integer): RawByteString; virtual;
    function GetLocalesList(AllLocales : boolean = True): RawByteString; virtual;
    function GetLocales: RawByteString; virtual;
    procedure SetFileName(AValue: String);
  protected
    function  StreamID : String; override;
    procedure WriteHead(Stream : TStream); override;
    procedure ReadHead(Stream: TStream; out ExpectedCount : integer); override;
    procedure WriteNodeData(Stream : TStream; Node : TBinaryTreeNode); override;
    procedure ReadNodeData(Stream : TStream; Node : TBinaryTreeNode); override;
    procedure Load; virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    property FileName : String read FFileName write SetFileName;
    procedure Save; virtual;
    property Locales : RawByteString read GetLocales;
    property LocaleCount : integer read GetLocaleCount;
    property Locale[Index : integer] : RawByteString read GetLocale;
    function AddLocale(LocaleID : String) : integer;
    function IndexOfLocale(LocaleID : String) : integer;
  end;

  { TMasterDictionary }

  TMasterDictionary = class(TCustomDictionary)
  protected
  public
  end;

  { TUserDictionary }

  TUserDictionary = class(TCustomDictionary)
  protected
  public
  end;

var
  MasterDictionary : TMasterDictionary;
  UserDictionary : TUserDictionary;

  procedure ReloadDictionaries;
  procedure MergeDictionaries;
  function DetectLocale(S : RawByteString; out Stats : TArrayOfInt32) : Int32; overload;
  function DetectLocale(S : RawByteString) : String; overload;

implementation

const
  MasterDictFile='master.cpw';
  UserDictFile='user.cpw';
  LocaleOffset=5000; // Should be good unless we join "The Federation" or a
                     // Galactic Empire and all of the Aliens want to use DOS
                     // programs in their own native language. But, that would
                     // not be supported by the current UTF-8 encoding or the
                     // present Unicode specification anyway. So, SMH.

var
  MasterLocales,
  UserLocales : TArrayOfRawByteString;

{ TCustomDictionary }

procedure TCustomDictionary.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  if not FLoaded then
    Load;
end;

function TCustomDictionary.GetLocaleCount: integer;
begin
  Result:=Length(MasterLocales) + Length(UserLocales);
end;

function TCustomDictionary.GetLocale(Index : integer): RawByteString;
begin
  Result:='';
  if Index < 0 then Exit;
  if (Index < Length(MasterLocales)) then
    Result:=MasterLocales[Index]
  else if (Index >= LocaleOffset) and (Index < LocaleOffset+Length(UserLocales)) then
    Result:=UserLocales[Index - LocaleOffset];
end;

function TCustomDictionary.GetLocalesList(AllLocales: boolean): RawByteString;
var
  SL : TStringList;
  I : Integer;
begin
  SL := TStringList.Create;
  try
    for I := 0 to High(MasterLocales) do
      SL.Add(MasterLocales[I]);
    if AllLocales then
      for I := 0 to High(UserLocales) do
        if SL.IndexOf(UserLocales[I]) < 0 then
          SL.Add(UserLocales[I]);
    SL.Sort;
    Result:=Implode(SL, COMMA + SPACE);
  except
    Result:='';
  end;
  SL.Free;
end;

function TCustomDictionary.GetLocales: RawByteString;
begin
  Result:=GetLocalesList(True);
end;

function TCustomDictionary.AddLocale(LocaleID: String) : integer;
begin
  Result:=IndexOfLocale(LocaleID);
  if Result <> -1 then Exit;
  if Self is TMasterDictionary then begin
    SetLength(MasterLocales, Length(MasterLocales) + 1);
    MasterLocales[High(MasterLocales)]:=LocaleID;
    Result:=High(MasterLocales);
  end else begin
    SetLength(UserLocales, Length(UserLocales) + 1);
    UserLocales[High(UserLocales)]:=LocaleID;
    Result:=High(UserLocales) + LocaleOffset;
  end;
end;

function TCustomDictionary.IndexOfLocale(LocaleID: String): integer;
var
  I : Integer;
begin
  for I := 0 to Length(MasterLocales) -1 do
    if UpperCase(LocaleID) = UpperCase(MasterLocales[I]) then Exit(I);
  for I := 0 to Length(UserLocales) -1 do
    if UpperCase(LocaleID) = UpperCase(UserLocales[I]) then Exit(I + LocaleOffset);
  Result:=-1;
end;

function TCustomDictionary.StreamID: String;
begin
  Result:='CPWD1';
end;

procedure TCustomDictionary.WriteHead(Stream: TStream);
var
  I, C : integer;
  M : TArrayOfRawByteString;
begin
  inherited WriteHead(Stream);
  M:=Copy(MasterLocales, 0, Length(MasterLocales));
  if Self is TUserDictionary then
    Cat(M, UserLocales);
  C:=Length(M);
  Stream.Write(C, Sizeof(C));
  FLocaleRemap:=[];
  SetLength(FLocaleRemap, C);
  for I:= 0 to High(M) do begin
    C:=Length(M[I]);
    Stream.Write(C, Sizeof(C));
    Stream.Write(Pointer(M[I])^, C);
    FLocaleRemap[I]:=IndexOfLocale(M[I]);
  end;
  if Self is TMasterDictionary then FLocaleRemap:=[];
end;

procedure TCustomDictionary.ReadHead(Stream: TStream; out ExpectedCount: integer
  );
var
  I, C : integer;
  S : String;
  DL : TArrayOfString;
begin
  inherited ReadHead(Stream, ExpectedCount);
  LogMessage(vbExcessive, 'Expected Count: ' + IntToStr(ExpectedCount));
  C:=0;
  S:='';
  Stream.Read(C, Sizeof(C));
  FLocaleRemap:=[];
  DL:=[];
  SetLength(FLocaleRemap, C);
  for I := 0 to High(FLocaleRemap) do begin
    Stream.Read(C, Sizeof(C));
    SetLength(S, C);
    Stream.Read(Pointer(S)^, C);
    Cat(DL, S);
    FLocaleRemap[I] := IndexOfLocale(S);
    if FLocaleRemap[I] = -1 then
      FLocaleRemap[I] := AddLocale(S);
  end;
  if Self is TMasterDictionary then begin
    FLocaleRemap:=[];
    LogMessage(vbExcessive, 'Dictionary Locales: ' + Implode(DL, COMMA + SPACE));
    LogMessage(vbExcessive, 'No locale remapping.');
  end else begin
    LogMessage(vbExcessive, 'Dictionary Locales: ' + Implode(DL, COMMA + SPACE));
    LogMessage(vbExcessive, 'Remap table: ', FLocaleRemap);
  end;
end;

procedure TCustomDictionary.WriteNodeData(Stream: TStream; Node: TBinaryTreeNode);
var
  C, I : integer;
  V : Int32;
begin
  // Only Data32 is used from the TBinaryTreeNode data in Dictionary file.
  C:=Length(Node.Data32);
  Stream.Write(C, Sizeof(C));
  if Length(FLocaleRemap) = 0 then begin
    for I := 0 to C - 1 do
      Stream.Write(Node.Data32[I], Sizeof(Int32));
  end else begin
    for I := 0 to C - 1 do begin
      V:=Node.Data32[I];
      if V >= LocaleOffset then
        V:=InArray(FLocaleRemap, V);
      Stream.Write(V, Sizeof(V));
    end;
  end;
end;

procedure TCustomDictionary.ReadNodeData(Stream: TStream; Node: TBinaryTreeNode);
var
  D : TArrayOfInt32;
  C, I : integer;
  M : TBinaryTreeNode;
begin
  // Other data types of TBinaryTreeNode are not used, no point in saving them
  // in Dinctionary file.
  C:=0;
  D:=[];
  Stream.Read(C, Sizeof(C));
  SetLength(D, C);
  for I := 0 to C - 1 do
    Stream.Read(D[I], Sizeof(Int32));
  if Length(FLocaleRemap) <> 0 then begin
    for I := 0 to C - 1 do begin
      // LogMessage(vbExcessive, 'Word: ' + Node.UniqueID);
      // LogMessage(vbExcessive, 'File Locales: ', D);
      if (D[I] >= 0) and (D[I] < Length(FLocaleRemap)) then
        D[I] := FLocaleRemap[D[I]];
      // LogMessage(vbExcessive, 'Data Locales: ', D);
    end;
    if Assigned(MasterDictionary) then begin
      // Prune Locale from Users Dictionary if it was added to Master.
      M := MasterDictionary.Find(Node.UniqueID);
      if Assigned(M) then begin
        for I := High(D) downto 0 do begin
          if InArray(M.Data32, D[I]) <> -1 then begin
            System.Delete(D, I, 1);
          end;
        end;
      end;
    end;
  end;
  if Length(D) = 0 then
    Delete(Node)
  else
    Node.Data32:=Copy(D, 0, Length(D));
end;

procedure TCustomDictionary.Load;
begin
  if not FileExists(FileName) then begin
    LogMessage(vbMinimal, 'Cannot find dictionary file: ' + FriendlyPath(AppBasePath, FileName));
    Exit;
  end;
  try
    LoadFromFile(FFileName);
    if not CheckIntegrity then
      raise Exception.Create('dictionary failed integrity check.');
    LogMessage(vbNormal, ExtractFileName(FileName) +
    ' dictionary contains ' + IntToStr(Count) + ' unique words');
    FLoaded:=True;
  except
    on E : Exception do begin
      Clear;
      LogMessage(vbCritical, 'Exception saving dictionary. ' + E.Message);
     end;
  end;
end;

procedure TCustomDictionary.Save;
begin
  Modified:=False;
  if FFileName = '' then Exit;
  LogMessage(vbVerbose, 'Saving dictionary file: '+ FriendlyPath(AppBasePath, FileName));
  try
    SaveToFile(FFileName);
    LogMessage(vbVerbose, 'saved dictionary file. ' + IntToStr(Count) +
     ' unique words.');
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Exception saving dictionary. ' + E.Message);
     end;
  end;
end;

constructor TCustomDictionary.Create;
begin
  inherited Create;
  FLoaded:=False;
end;

procedure TCustomDictionary.Clear;
begin
  inherited Clear;
  FLoaded:=False;
end;

{ TUserDictionary }

{ Unit functions }

procedure Initialize;
begin
  MasterDictionary:=nil;
  UserDictionary:=nil;
  ReloadDictionaries;
end;

procedure Finalize;
begin
  if Assigned(MasterDictionary) then begin
    if MasterDictionary.Modified then
      MasterDictionary.Save;
    FreeAndNil(MasterDictionary);
  end;
  if Assigned(UserDictionary) then begin
    if UserDictionary.Modified then
      UserDictionary.Save;
    FreeAndNil(UserDictionary);
  end;
end;

procedure ReloadDictionaries;
begin
  if Assigned(MasterDictionary) then FreeAndNil(MasterDictionary);
  if Assigned(UserDictionary) then FreeAndNil(UserDictionary);
  MasterLocales:=[];
  UserLocales:=[];
  MasterDictionary:=TMasterDictionary.Create;
  try
    LogMessage(vbExcessive, 'Load Master dictionary.');
    MasterDictionary.FileName:=AppDataPath+MasterDictFile;
    LogMessage(vbVerbose, 'Master dictionary is ready.');
  except
    FreeAndNil(MasterDictionary);
  end;
  UserDictionary:=TUserDictionary.Create;
  try
    LogMessage(vbExcessive, 'Load User dictionary.');
    UserDictionary.FileName:=UserDataPath+UserDictFile;
    LogMessage(vbVerbose, 'User dictionary is ready.');
  except
    FreeAndNil(UserDictionary);
  end;
  if Assigned(MasterDictionary) then begin
    LogMessage(vbVerbose, IntToStr(MasterDictionary.LocaleCount) +
    ' total locales.' + WhenTrue(MasterDictionary.LocaleCount > 0,
    LF + TAB+ MasterDictionary.Locales));
  end;
end;

type
  TNode=class(TBinaryTreeNode);

procedure MergeDictionaries;
var
  LazyRemap : TArrayOfInt32;

  procedure AddEntry(Node:TBinaryTreeNode);
  var
    N : TBinaryTreeNode;
    D, T : TArrayOfInt32;
    I : Integer;
  begin
    if Not Assigned(Node) then Exit;
    D:=Copy(Node.Data32, 0, Length(Node.Data32));
    for I := 0 to High(D) do
      if D[I] >= LocaleOffset then
        D[I]:=LazyRemap[D[I] - LocaleOffset];
    N:=MasterDictionary.Find(Node.UniqueID);
    if Not Assigned(N) then
      N:=MasterDictionary.Add(Node.UniqueID, D)
    else begin
      T:=N.Data32;
      for I := 0 to High(D) do
        if InArray(T, D[I]) < 0 then Cat(T, D[I]);
      N.Data32:=Copy(T, 0, Length(T));
    end;

    if Assigned(TNode(Node).Lesser) then
      AddEntry(TNode(Node).Lesser);
    if Assigned(TNode(Node).Greater) then
      AddEntry(TNode(Node).Greater);
  end;

var
  TempLocales: TArrayOfString;
  I : Integer;

begin
  if Not Assigned(MasterDictionary) then Exit;
  if Not Assigned(UserDictionary) then Exit;
  try
    TempLocales:=Copy(UserLocales,0,Length(UserLocales));
    UserLocales:=[];
    LazyRemap:=[];
    SetLength(LazyRemap, Length(TempLocales));
    for I := 0 to High(LazyRemap) do
      LazyRemap[I]:=MasterDictionary.AddLocale(TempLocales[I]);
    AddEntry(UserDictionary.Root);
    UserDictionary.Clear;
    UserDictionary.Modified:=True;
    MasterDictionary.Optimize;
  except
    ReloadDictionaries;
  end;

end;

function DetectDict(D : TCustomDictionary; S: RawByteString; var Stats: TArrayOfInt32
  ): Int32;
var
  I, J, P, L, X : integer;
  W : TStringList;
  N : TBinaryTreeNode;
begin
  Result:=0;
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
        for J := 0 to High(N.Data32) do begin
          X:=N.Data32[J];
          if X >= LocaleOffset then
            X:=X - LocaleOffset + Length(MasterLocales);
          Inc(Stats[X], P);
        end;
      end;
    end;
    Result:=W.Count;
  finally
    W.Free;
  end;
end;

function DetectLocale(S: RawByteString; out Stats: TArrayOfInt32
  ): Int32;
var
  I : integer;
  HoldStats : TArrayOfInt32;
  HoldCount : integer;
begin
  Stats:=[];
  SetLength(Stats, Length(MasterLocales) + Length(UserLocales));
  HoldStats:=[];
  SetLength(HoldStats, Length(Stats));
  for I := 0 to High(Stats) do
    Stats[I]:=0;
  HoldStats:=Copy(Stats,0,Length(Stats));

  if Assigned(MasterDictionary) then begin
    try
      Result:=DetectDict(MasterDictionary, S, Stats);
      HoldStats:=Copy(Stats,0,Length(Stats));
    except
      Stats:=Copy(HoldStats,0,Length(HoldStats));;
      Result:=0;
    end;
  end;

  if Assigned(UserDictionary) then begin
    try
      HoldCount:=Result;
      Result:=DetectDict(UserDictionary, S, Stats);
      HoldStats:=Copy(Stats,0,Length(Stats));
    except
      Stats:=Copy(HoldStats,0,Length(HoldStats));;
      Result:=HoldCount;
    end;
  end;

  if Result = 0 then
    Stats:=[];

end;

function DetectLocale(S: RawByteString): String;
var
  I, P : integer;
  Stats:TArrayOfInt32;
begin
  Result:='';
  if Not Assigned(MasterDictionary) then Exit;
  DetectLocale(S, Stats);
  P:=-1;
  for I := 0 to High(Stats) do
    if (Stats[I]>0) then
      if (P<0) or (Stats[I]>Stats[P]) then P:=I;
  if P<0 then Exit;
  if P >= Length(MasterLocales) then
    P:=P- Length(MasterLocales) + LocaleOffset;
  Result:=MasterDictionary.Locale[P];
end;

initialization

  Initialize;

finalization

  Finalize;

end.

