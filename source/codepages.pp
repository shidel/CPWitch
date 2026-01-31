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
  CodePageMaps : TCodePageMaps;
  UnmappableChars : TUnmappableChars;

  function CodePageMap ( CodePage : Integer ) : TCodePageMap;

implementation

procedure ProcessNoMap(L : TStringList);
var
  C, I, E : Integer;
  V : Int64;
  TS : RawByteString;
begin
  C := 0;
  SetLength(UnmappableChars, L.Count);
  for I := 0 to L.Count - 1 do begin
    UnmappableChars[C].Data:=[];
    TS:=L[I];
    Val(PopDelim(TS, EQUAL), UnmappableChars[C].Value, E);
    if E <> 0 then Continue;
    While TS <> '' do begin
      Val(Trim(PopDelim(TS, COMMA)), V, E);
      if E <> 0 then begin
        SetLength(UnmappableChars[C].Data, 0);
        Break;
      end;
      TS:=Trim(TS);
      Cat(UnmappableChars[C].Data, V);
    end;
    if Length(UnmappableChars[C].Data) = 0 then Continue;
    Inc(C);
  end;
  SetLength(UnmappableChars, C);
  LogMessage(vbExcessive, TAB+'Unmappable character data, ' + IntToStr(C) + ' entries.');
end;

procedure Initialize;
var
  INI : TInifile;
  S, L : TStringList;
  I, J, E, C, T, K, D, CC : integer;
  V : Int64;
  TS, CN : RawByteString;
begin
  CodePageMaps:=[];
  UnmappableChars:=[];
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
      SetLength(CodePageMaps, Length(CodePageMaps) + 1);
      C:=High(CodePageMaps);
      CodePageMaps[C].CodePage:=T;
      if T = 437 then D:=C;
      CodePageMaps[C].Map:=[];
      SetLength(CodePageMaps[C].Map,256);
      for J:=Low(CodePageMaps[C].Map) to High(CodePageMaps[C].Map) do
        CodePageMaps[C].Map[J]:=cpeInherited; { Same as CP437 }
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
        if (V < -3) or (V > $10ffff) then Continue;
        CodePageMaps[C].Map[K]:=V;
        Inc(CC);
        // LogMessage(vbNormal, TAB + IntToStr(K) + '=' + IntToStr(V));
      end;
      LogMessage(vbExcessive, TAB+'Codepage: ' + CN + ', ' + IntToStr(CC) + ' entries.');
    end;
    for I := 0 to Length(CodePageMaps) - 1 do begin
      if I = D then Continue;
      for J := 0 to 255 do
        if CodePageMaps[D].Map[J] = CodePageMaps[I].Map[J] then begin
          if J > 127 then
            LogMessage(vbExcessive, 'Codepage ' + IntToStr(CodePageMaps[I].CodePage) +
             ', Map entry + 0x' + HexStr(J, 2) + ' should be removed.');
          CodePageMaps[I].Map[J]:=cpeInherited;
        end;
    end;
    LogMessage(vbExcessive, IntToStr(Length(CodepageMaps)) + ' Codepage maps.');
  except
    CodePageMaps:=[];
  end;
  if Assigned(L) then FreeAndNil(L);
  if Assigned(S) then FreeAndNil(S);
  if Assigned(INI) then FreeAndNil(INI);
end;

function CodePageMap(CodePage: Integer): TCodePageMap;
var
  I : Integer;
begin
  Result.CodePage:=-1;
  Result.Map:=[];
  for I := Low(CodePageMaps) to High(CodePageMaps) do
    if CodePageMaps[I].CodePage = CodePage then begin
      Result.CodePage:=CodePage;
      Result.Map:=Copy(CodePageMaps[I].Map,0,Length(CodePageMaps[I].Map));
      Break;
    end;
end;

initialization

  Initialize;

end.

