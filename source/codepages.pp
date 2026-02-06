{
   Copyright (c) 2025-2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

{ TODO 0 -cDevel Remove need for an external codepages.ini file. }

{ This unit requires an external data file called codepages.ini to be stored
  in the "AppDataPath" set by the PasExt Unit. Eventually, that data will likely
  be embedded into this unit to remove the requirement of the external file. }

unit Codepages;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, PasExt, IniFiles, BinTree;

type
  TCodepageMap = record
    Codepage : integer;
    Map : TArrayOfInt32;
  end;
  TCodepageResult = record
    Codepage : integer;       // Specific Codepage.
    Characters : integer;     // total number of characters
    ASCII : integer;          // ASCII < 0x80
    Unicode : integer;        // Unicode character count
    Converted : integer;      // Number of characters supported in conversion
    Compatible : integer;     // percentage of compatibility
  end;
  TCodepageResults = array of TCodepageResult;

  function CodepageList : TArrayOfInteger;
  function CodepageMap(Codepage : integer) : TCodepageMap;

type

  { TCodepageConverter }

  TCodepageConverter = class
  private
    FCodepage: integer;
    FControlCodes: boolean;
    FExpanded: boolean;
    FInvalid: Int32;
    FResults: TCodepageResult;
    procedure SetCodepage(AValue: integer);
    procedure SetControlCodes(AValue: boolean);
    procedure SetExpanded(AValue: boolean);
  protected
    procedure SetInvalid(AValue: Int32); virtual; abstract;
  public
    constructor Create; virtual;
    { Clear all data. Does not reset Expanded or ControlCodes! }
    procedure Clear; virtual;
    { Perform the conversion }
    function Convert : boolean; virtual; abstract;
    { DOS Codepage number }
    property Codepage : integer read FCodepage write SetCodepage;
    { Expand non-existing unicode characters to multi-character sequence }
    property Expanded : boolean read FExpanded write SetExpanded;
    { Convert ASCII Control Codes }
    property ControlCodes : boolean read FControlCodes write SetControlCodes;
    { Invalid or Un-mappable characters get assigned this Value. For conversion
      to Codepage, it should be the ASCII value 0-255. For conversion to Unicode,
      it should be the Value of a Unicode Character. }
    property Invalid : Int32 read FInvalid write SetInvalid;
    { Conversion success results }
    property Results : TCodepageResult read FResults;
  end;

  { TCodepageToUTF8 }

  TCodepageToUTF8 = class(TCodepageConverter)
  private
    FConverted: UnicodeString;
    FSource: RawByteString;
    procedure SetSource(AValue: RawByteString);
  protected
    procedure SetInvalid(AValue: Int32); override;
  public
    constructor Create; override;
    procedure Clear; override;
    function Convert : boolean; override;
    property Source : RawByteString read FSource write SetSource;
    property Converted : UnicodeString read FConverted;
  end;

  { TUTF8ToCodepage }

  TUTF8ToCodepage = class(TCodepageConverter)
  private
    FConverted: RawByteString;
    FSource: UnicodeString;
    procedure SetSource(AValue: UnicodeString);
  protected
    procedure SetInvalid(AValue: Int32); override;
  public
    constructor Create; override;
    procedure Clear; override;
    function Convert : boolean; override;
    property Source : UnicodeString read FSource write SetSource;
    property Converted : RawByteString read FConverted;
  end;

  { TUTF8Anaylize }

  TUTF8Analyze = class
  private
    FValues : TArrayOfInt32;
    FResults: TCodepageResults;
  protected
    procedure Analyze; overload;
  public
    constructor Create(const Data : TArrayOfByte); virtual;
    property Results : TCodepageResults read FResults;
  end;

implementation

{ DEFINE DEBUGGING}

const
// cpeExcluded = -1; // Character specifically excluded from Codepage map.
   cpeInherited = -2; // Characer inherited from Codepage 437. If Codepage 437,
                     // then is same as ASCII value.
// cpeUnknown = -3; // Character Unicode to Codepage mapping not know at present
   cpeError = -4; // Mostly for functions that need to know to ignore the result.

type
  TCodepageMaps = array of TCodepageMap;
  TUnmappableChar = record
    Value : Int32;
    Data : TArrayOfInt32;
  end;
  TUnmappable = array of TUnmappableChar;

var
  EnMap : Integer;         // Default English Codepage to Unicode Map
  Maps : TCodepageMaps;    // All Codepage Maps
  UnMap : TUnmappable;     // Codepage character which do not exist in Unicode
  AnalyzeMap : TBinaryTree;    // Binary Tree for Unicode to Codepage Lookup

function FindMap(Codepage : Integer) : Integer;
var
  I : Integer;
begin
  Result:=-1;
   for I := Low(Maps) to High(Maps) do
    if Maps[I].Codepage = Codepage then begin
      Result:=I;
      Break;
    end;
end;

function CodepageMap(Codepage: Integer): TCodepageMap;
var
  I : Integer;
begin
  Result.Codepage:=-1;
  Result.Map:=[];
  for I := Low(Maps) to High(Maps) do
    if Maps[I].Codepage = Codepage then begin
      Result.Codepage:=Codepage;
      Result.Map:=Copy(Maps[I].Map,0,Length(Maps[I].Map));
      Break;
    end;
end;

procedure ProcessNoMap(L : TStringList);
var
  C, I, E : Integer;
  V : Int32;
  TS : RawByteString;
begin
  C := 0;
  SetLength(UnMap, L.Count);
  for I := 0 to L.Count - 1 do begin
    UnMap[C].Data:=[];
    TS:=L[I];
    Val(PopDelim(TS, EQUAL), UnMap[C].Value, E);
    if E <> 0 then Continue;
    While TS <> '' do begin
      Val(Trim(PopDelim(TS, COMMA)), V, E);
      if E <> 0 then begin
        SetLength(UnMap[C].Data, 0);
        Break;
      end;
      TS:=Trim(TS);
      Cat(UnMap[C].Data, V);
    end;
    if Length(UnMap[C].Data) = 0 then Continue;
    Inc(C);
  end;
  SetLength(UnMap, C);
  LogMessage(vbExcessive, TAB+'Unmappable character data, ' + IntToStr(C) + ' entries.');
end;

procedure LoadMappingData;
var
  INI : TInifile;
  S, L : TStringList;
  I, J, E, C, T, K, D, CC : integer;
  V : Int32;
  TS, CN : RawByteString;
begin
  Maps:=[];
  UnMap:=[];
  INI:=nil;
  S:=nil;
  L:=nil;
  D:=0;
  try
    LogMessage(vbExcessive, 'Codepage data: ' + AppDataPath+'Codepages.ini');
    INI:=TIniFile.Create(AppDataPath+'Codepages.ini');
    S:=TStringList.Create;
    L:=TStringList.Create;
    INI.ReadSections(S);
    for I := 0 to S.Count - 1 do begin
      TS:=S[I];
      if Uppercase(TS) = 'NOMAP' then begin
        INI.ReadSectionValues(S[I], L);
        ProcessNoMap(L);
        Continue;
      end;
      PopDelim(TS,UNDERSCORE);
      Val(TS,T,E);
      if E <> 0 then Continue;
      SetLength(Maps, Length(Maps) + 1);
      C:=High(Maps);
      Maps[C].Codepage:=T;
      if T = 437 then D:=C;
      Maps[C].Map:=[];
      SetLength(Maps[C].Map,256);
      for J:=Low(Maps[C].Map) to High(Maps[C].Map) do
        Maps[C].Map[J]:=cpeInherited; { Same as CP437 }
      CC:=0;
      CN:=TS;
      INI.ReadSectionValues(S[I], L);
      for J := 0 to L.Count - 1 do begin
        TS:=L[J];
        Val(PopDelim(TS, EQUAL), K, E);
        if E <> 0 then Continue;
        if (K < 0) or (K > 255) then Continue;
        Val(TS, V, E);
        if E <> 0 then Continue;
        if (V <= cpeError) or (V > $10ffff) then Continue;
        Maps[C].Map[K]:=V;
        Inc(CC);
        // LogMessage(vbNormal, TAB + IntToStr(K) + '=' + IntToStr(V));
      end;
      LogMessage(vbExcessive, TAB+'Codepage: ' + CN + ', ' + IntToStr(CC) + ' entries.');
    end;
    for I := 0 to Length(Maps) - 1 do begin
      if I = D then Continue;
      for J := 0 to 255 do
        if Maps[D].Map[J] = Maps[I].Map[J] then begin
          if J > 127 then
            LogMessage(vbExcessive, 'Codepage ' + IntToStr(Maps[I].Codepage) +
             ', Map entry + 0x' + HexStr(J, 2) + ' should be removed.');
          Maps[I].Map[J]:=cpeInherited;
        end;
    end;
    LogMessage(vbExcessive, IntToStr(Length(Maps)) + ' Codepage maps.');
  except
    LogMessage(vbCritical, 'Exception raised loading Codepage mapping data.');
    Maps:=[];
  end;
  if Assigned(L) then FreeAndNil(L);
  if Assigned(S) then FreeAndNil(S);
  if Assigned(INI) then FreeAndNil(INI);
end;

function SubTree(S : String) : TBinaryTree;
var
  SL : TStringList;
  B : TBinaryTree;

  procedure AddNode(L, H : integer);
  var
    M : Integer;
    S, T : String;
    V, E : Integer;
  begin
    if H < L then Exit;
    M := L + (H - L) div 2;

    S:=SL[M];
    T:=PopDelim(S, COMMA);

    Val(S, V, E);
    IgnoreParameter(E);

    B.Add(T, V);

    if L = H then Exit;
    if L < M then AddNode(L, M-1);
    if M < H then AddNode(M + 1, H);
  end;

begin
  SL :=TStringList.Create;
  Explode(S, SL, SEMICOLON);
  SL.Sort;
  B:=TBinaryTree.Create;
  AddNode(0, SL.Count - 1);
  SL.Free;
  Result:=B;
end;

function CreateMapper(Inclusive : boolean = true) : TBinaryTree;
var
  R : TBinaryTree;
  SL : TStringList;
  I, J : integer;
  V, N : Int32;
  T, S : String;

  procedure AddNode(L, H : Integer);
  var
    M : Integer;
    X : TBinaryTreeNode;
  begin
    if H < L then Exit;
    M := L + (H - L) div 2;
    S := SL[M];
    T:=PopDelim(S, EQUAL);
    X:=R.Add(T);
    X.Item:=SubTree(S);
    if L = H then Exit;
    if L < M then AddNode(L, M-1);
    if M < H then AddNode(M + 1, H);
  end;

{$IFDEF DEBUGGING}
var
  TN : TBinaryTreeNode;
{$ENDIF}
var
  FC : integer;
begin
  if Inclusive then FC:=0 else FC:=$80;
  SL := TStringList.Create;
  // Add all chars from all Codepage maps as separate entries in TStringList
  for I := 0 to High(Maps) do
    for J := FC to High(Maps[I].Map) do begin
      V:=Maps[I].Map[J];
      if V < 0 then begin
        if not Inclusive then Continue;
        // Potentially, this should only be done for cpeInherited and other
        // values skipped.
        V:=Maps[EnMap].Map[J];
        if V < 0 then V:=J;
      end;
      SL.Add(IntToStr(V) + // Unicode Value 0 - 1,114,111
        EQUAL + IntToStr(Maps[I].Codepage) + COMMA + // Codepage Number
        IntToStr(J) ); // ASCI Value
    end;
  SL.Sort;
  // Combine Unicode character data
  I := 0;
  V := -1;
  While I < SL.Count do begin
    S := SL[I];
    T:=PopDelim(S, EQUAL);
    Val(T, N, J);
    if N = V then begin
      SL[I-1]:=SL[I-1] + SEMICOLON + S;
      SL.Delete(I);
    end else begin
      V:=N;
      Inc(I);
    end;
  end;
 {$IFDEF DEBUGGING}
  WriteLn(StringOf('-', 80));
  for I := 0 to SL.Count -1 do
    WriteLn(ZeroPad(I, 3) , ' - ', SL[I]);
  WriteLn(StringOf('-', 80));
  {$ENDIF}
  // Create BinaryTree from TStringist
  R := TBinaryTree.Create;
  AddNode(0, SL.Count - 1);
  SL.Free;
  Result:=R;
  {$IFDEF DEBUGGING}
  TN:=R.First;
  V:=0;
  I:=0;
  while Assigned(TN) do begin
    if Assigned(TN.Item) then
      WriteLn(ZeroPad(I, 3), ': ', TN.UniqueID, ' - ',TBinaryTree(TN.Item).Count)
    else
      WriteLn(ZeroPad(I, 3), ': ', TN.UniqueID, ' - nil');
    TN:=TN.Next;
  end;
  {$ENDIF}
end;

procedure Initialize;
begin
  AnalyzeMap:=nil;
  LoadMappingData;
  EnMap:=FindMap(437); // Default CP437 (en_US) index in Maps array.
  if EnMap <> -1 then begin
    AnalyzeMap:=CreateMapper;
  end;
end;

procedure Finalize;
begin
  if Assigned(AnalyzeMap) then FreeAndNil(AnalyzeMap);
end;

function CodepageList: TArrayOfInteger;
var
  I : Integer;
begin
  Result:=[];
  SetLength(Result, Length(Maps));
  for I := 0 to High(Result) do
    Result[I]:=Maps[I].Codepage;
end;

{ TCodepageConverter }

procedure TCodepageConverter.SetCodepage(AValue: integer);
begin
  if FCodepage=AValue then Exit;
  FCodepage:=AValue;
end;

procedure TCodepageConverter.SetControlCodes(AValue: boolean);
begin
  if FControlCodes=AValue then Exit;
  FControlCodes:=AValue;
end;

procedure TCodepageConverter.SetExpanded(AValue: boolean);
begin
  if FExpanded=AValue then Exit;
  FExpanded:=AValue;
end;

constructor TCodepageConverter.Create;
begin
  inherited Create;
  FExpanded:=False;
  FControlCodes:=False;
  FInvalid:=0;
  Clear;
end;

procedure TCodepageConverter.Clear;
begin
  FCodepage:=-1;
  FResults.Codepage:=FCodepage;
  FResults.Characters:=0;
  FResults.ASCII:=0;
  FResults.Converted:=0;
  FResults.Unicode:=0;
  FResults.Compatible:=0;
end;

{ TCodepageToUTF8 }

procedure TCodepageToUTF8.SetSource(AValue: RawByteString);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

procedure TCodepageToUTF8.SetInvalid(AValue: Int32);
begin
  if AValue = FInvalid then Exit;
  FInvalid:=AValue;
end;

constructor TCodepageToUTF8.Create;
begin
  inherited Create;
  FSource:='';
  FConverted:='';
end;

procedure TCodepageToUTF8.Clear;
begin
  inherited Clear;
  FSource:='';
  FConverted:='';
end;

function TCodepageToUTF8.Convert : boolean;
var
  M, I, C : Integer;
begin
  Result:=False;
  FConverted := '';
  M:=FindMap(FCodepage);
  if M < 0 then Exit;
  if EnMap < 0 then Exit;
  for I := 1 to Length(FSource) do begin
     C:=Byte(FSource[I]);
  end;
end;

{ TUTF8ToCodepage }

procedure TUTF8ToCodepage.SetSource(AValue: UnicodeString);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

procedure TUTF8ToCodepage.SetInvalid(AValue: Int32);
begin
  if (AValue < 0) or (AValue > 255) then
    AValue:=0;
  if AValue = FInvalid then Exit;
  FInvalid:=AValue;
end;

constructor TUTF8ToCodepage.Create;
begin
  inherited Create;
end;

procedure TUTF8ToCodepage.Clear;
begin
  inherited Clear;
  FSource:='';
  FConverted:='';
end;

function TUTF8ToCodepage.Convert : boolean;
begin
  Result:=False;
end;

{ TUTF8Analyze }

procedure TUTF8Analyze.Analyze;
var
  R : TBinaryTree;
  N, X : TBinaryTreeNode;
  I : integer;
  AC, UC, EC : integer;
begin
  R := TBinaryTree.Create;
  AC:=0;
  UC:=0;
  EC:=0;
  for I := 0 to High(FValues) do begin
    if FValues[I] < $80 then
      Inc(AC)
    else begin
      Inc(UC);
      N := AnalyzeMap.Find(IntToStr(FValues[I]));
      if not (Assigned(N) and Assigned(N.Item)) then begin
        {$IFDEF DEBUGGING}
        WriteLn(I, ' error1 ', HexStr(FValues[I],8), ' ', BoolStr(Assigned(N)));
        {$ENDIF}
        Inc(EC)
      end
      else begin
        N:=TBinaryTree(N.Item).First;
        if Not Assigned(N) then begin
          {$IFDEF DEBUGGING}
          WriteLn(I, ' error2 ', BoolStr(Assigned(N)));
          {$ENDIF}
          Inc(EC)
        end
        else
          while Assigned(N) do begin
            X:=R.Find(N.UniqueID);
            if Assigned(X) then
              X.Value:=X.Value+1
            else begin
              {$IFDEF DEBUGGING}
              WriteLn(I, ' Add ', N.UniqueID);
              {$ENDIF}
              R.Add(N.UniqueID,1);
            end;
            N:=N.Next;

          end;
      end;
    end;
  end;

// Populate Result Array
  SetLength(FResults, Length(Maps));
  for I := 0 to High(FResults) do begin
    FResults[I].Codepage:=Maps[I].Codepage;
    FResults[I].Characters := Length(FValues);
    FResults[I].ASCII := AC;
    FResults[I].Unicode := UC;
    X:=R.Find(IntToStr(Maps[I].Codepage));
    if Assigned(X) then begin
      FResults[I].Converted:=X.Value;
      if (UC = 0) or  (FResults[I].Converted = UC) then
        FResults[I].Compatible:=100
      else begin
        FResults[I].Compatible := (FResults[I].Converted) * 100 div UC;
        // Cap unperfect match at 99%
        if FResults[I].Compatible > 99 then
          FResults[I].Compatible:=99
        else
        // Minimum umperfect match of 1%
        if FResults[I].Compatible < 1 then
          FResults[I].Compatible:=1;
      end;
    end else begin
      FResults[I].Converted := 0;
      FResults[I].Compatible := 0;
    end;
    {$IFDEF DEBUGGING}
    with FResults[I] do
      WriteLn(ZeroPad(I, 2), ': CP', Codepage, ', Converted ', Converted,
        ', Compatible ', Compatible, '%');
    {$ENDIF}
  end;
  R.Free;
end;

constructor TUTF8Analyze.Create(const Data: TArrayOfByte);
begin
  inherited Create;
  UTF8ToValues(Data, FValues);
  FResults:=[];
  SetLength(FResults, Length(Maps));
  if Assigned(AnalyzeMap) then
    Analyze;
end;

initialization

  Initialize;

finalization

  Finalize;

end.

