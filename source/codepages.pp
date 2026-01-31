{
   Copyright (c) 2025-2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit CodePages;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, PasExt, IniFiles, BinTree;

  function CodePageList : TArrayOfInteger;

type
  TCodePageResult = record
    Bytes : integer;
    Unicode : integer;
    Converted : integer;
  end;

  { TCodepageConverter }

  TCodepageConverter = class
  private
    FCodepage: integer;
    FControlCodes: boolean;
    FExpanded: boolean;
    FResults: TCodePageResult;
    procedure SetCodepage(AValue: integer);
    procedure SetControlCodes(AValue: boolean);
    procedure SetExpanded(AValue: boolean);
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
    { Conversion success results }
    property Results : TCodePageResult read FResults;
  end;

  { TCodepageToUTF8 }

  TCodepageToUTF8 = class(TCodePageConverter)
  private
    FConverted: UnicodeString;
    FSource: RawByteString;
    procedure SetSource(AValue: RawByteString);
  public
    constructor Create; override;
    procedure Clear; override;
    function Convert : boolean; override;
    property Source : RawByteString read FSource write SetSource;
    property Converted : UnicodeString read FConverted;
  end;

  { TUTF8ToCodePage }

  TUTF8ToCodePage = class(TCodePageConverter)
  private
    FConverted: RawByteString;
    FSource: UnicodeString;
    procedure SetSource(AValue: UnicodeString);
  public
    constructor Create; override;
    procedure Clear; override;
    function Convert : boolean; override;
    property Source : UnicodeString read FSource write SetSource;
    property Converted : RawByteString read FConverted;
  end;

implementation

const
  cpeExcluded = -1; // Character specifically excluded from Codepage map.
  cpeInherited = -2; // Characer inherited from Codepage 437. If Codepage 437,
                     // then is same as ASCII value.
  cpeUnknown = -3; // Character Unicode to Codepage mapping not know at present
  cpeError = -4; // Mostly for functions that need to know to ignore the result.

type
  TCodePageMap = record
    CodePage : integer;
    Map : TArrayOfInt64;
  end;
  TCodePageMaps = array of TCodePageMap;
  TUnmappableChar = record
    Value : Int64;
    Data : TArrayOfInt64;
  end;
  TUnmappableChars = array of TUnmappableChar;

var
  EnMap : Integer;
  Maps : TCodePageMaps;
  UnMap : TUnmappableChars;

function FindMap(CodePage : Integer) : Integer;
var
  I : Integer;
begin
  Result:=-1;
   for I := Low(Maps) to High(Maps) do
    if Maps[I].CodePage = CodePage then begin
      Result:=I;
      Break;
    end;
end;

function CodePageMap(CodePage: Integer): TCodePageMap;
var
  I : Integer;
begin
  Result.CodePage:=-1;
  Result.Map:=[];
  for I := Low(Maps) to High(Maps) do
    if Maps[I].CodePage = CodePage then begin
      Result.CodePage:=CodePage;
      Result.Map:=Copy(Maps[I].Map,0,Length(Maps[I].Map));
      Break;
    end;
end;

procedure ProcessNoMap(L : TStringList);
var
  C, I, E : Integer;
  V : Int64;
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
  V : Int64;
  TS, CN : RawByteString;
begin
  Maps:=[];
  UnMap:=[];
  INI:=nil;
  S:=nil;
  L:=nil;
  D:=0;
  try
    LogMessage(vbExcessive, 'Codepage data: ' + AppDataPath+'codepages.ini');
    INI:=TIniFile.Create(AppDataPath+'codepages.ini');
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
      Maps[C].CodePage:=T;
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
            LogMessage(vbExcessive, 'Codepage ' + IntToStr(Maps[I].CodePage) +
             ', Map entry + 0x' + HexStr(J, 2) + ' should be removed.');
          Maps[I].Map[J]:=cpeInherited;
        end;
    end;
    LogMessage(vbExcessive, IntToStr(Length(Maps)) + ' Codepage maps.');
  except
    LogMessage(vbCritical, 'Exception raised loading codepage mapping data.');
    Maps:=[];
  end;
  if Assigned(L) then FreeAndNil(L);
  if Assigned(S) then FreeAndNil(S);
  if Assigned(INI) then FreeAndNil(INI);
end;

procedure Initialize;
begin
  LoadMappingData;
  EnMap:=FindMap(437); // en_US
end;

function CodePageList: TArrayOfInteger;
var
  I : Integer;
begin
  Result:=[];
  SetLength(Result, Length(Maps));
  for I := 0 to High(Result) do
    Result[I]:=Maps[I].CodePage;
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
  Clear;
end;

procedure TCodepageConverter.Clear;
begin
  FCodePage:=-1;
  FResults.Bytes:=0;
  FResults.Converted:=0;
  FResults.Unicode:=0;
end;

{ TCodepageToUTF8 }

procedure TCodepageToUTF8.SetSource(AValue: RawByteString);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
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
begin
  Result:=False;
end;

{ TUTF8ToCodePage }

procedure TUTF8ToCodePage.SetSource(AValue: UnicodeString);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

constructor TUTF8ToCodePage.Create;
begin
  inherited Create;
end;

procedure TUTF8ToCodePage.Clear;
begin
  inherited Clear;
  FSource:='';
  FConverted:='';
end;

function TUTF8ToCodePage.Convert : boolean;
begin
  Result:=False;
end;

initialization

  Initialize;

end.

