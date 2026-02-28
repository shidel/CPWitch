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

implementation

{$UNDEF CaseSpecific}

procedure Initialize;
begin
  Dictionaries:=TDictionaries.Create;
  try
    Dictionaries.FileName:=AppDataPath+'dictionary.cpw';
  except
    FreeAndNil(Dictionaries);
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

procedure TDictionaries.Load;
var
  Sect, E : integer;
  SS, ST : TArrayOfInt32;
  FileText : RawByteString;
  Line : RawByteString;
  U : UnicodeString;
  N : TBinaryTreeNode;
  WC:Integer;
begin
  WC:=0;
  if not FileExists(FileName) then begin
    LogMessage(vbMinimal, 'Cannot find dictionary file: ' + ExtractRelativepath(AppBasePath, FileName));
    Exit;
  end;
  E :=FileLoad(FileName, FileText);
  if E <> 0 then
    raise Exception.Create('reading file: ' + ExtractRelativepath(AppBasePath, FileName));
  try
    LogMessage(vbVerbose, 'Processing dictionary file: '+ ExtractRelativepath(AppBasePath, FileName));
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
        SS:=[Sect];
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
          N:=Add(U, SS);
          Inc(WC);
        end else begin
          if InArray(N.Data32, Sect) <> -1 then begin
            LogMessage(vbVerbose, WhenTrue(VerboseLevel=vbExcessive,
              TAB + 'duplicate word: ', 'duplicate dictionary word: ') +
              RawByteString(U) + ' [' + Locale[Sect] + ']');
            Continue;
          end;
          ST:=N.Data32;
          Cat(ST, Sect);
          N.Data32:=ST;
        end;
      end;
    end;
    Balance;
    {$IFDEF BUILD_DEBUG}
    if VerboseLevel = vbExcessive then begin
      N:=First;
      While Assigned(N) do begin
        LogMessage(VerboseLevel, N.UniqueID + ' ' + PasExt.ToString(N.Data));
        N:=N.Next;
      end;
    end;
    {$ENDIF}
  finally
    LogMessage(vbVerbose, 'Dictionary contains ' + IntToStr(WC) + ' unique words and ' +
      IntToStr(Length(FLocales)) + ' languages.');
    LogMessage(vbVerbose, TAB+Locales);
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


initialization

  Initialize;

finalization

  Finalize;

end.

