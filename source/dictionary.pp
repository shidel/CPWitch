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
    FLanguages : RawByteString;
    FLoaded : boolean;
    FModified : boolean;
  private
    FFileName: String;
    procedure SetFileName(AValue: String);
    procedure Load;
  public
    constructor Create; override;
    procedure Clear; override;
    property FileName : String read FFileName write SetFileName;
    procedure Reload;
    procedure Save;
    property Languages : RawByteString read FLanguages;
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
    DIctionaries.FModified:=True;
  except
    FreeAndNil(Dictionaries);
  end;
end;

procedure Finalize;
begin
  if Assigned(Dictionaries) then begin
    if Dictionaries.FModified then
      Dictionaries.Save;
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

procedure TDictionaries.Load;
var
  Sects : TBinaryTree;
  E : integer;
  FileText : RawByteString;
  Sect, Line, S : RawByteString;
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
  Sects:=TBinaryTree.Create;
  try
    LogMessage(vbVerbose, 'Processing dictionary file: '+ ExtractRelativepath(AppBasePath, FileName));
    Sect:='';
    FileText:=StringReplace(FileText, CR, LF, [rfReplaceAll]);
    while Length(FileText) > 0 do begin
      Line:=Trim(PopDelim(FileText, LF));
      if Length(Line) = 0 then Continue;
      if HasEnds(Line, '[', ']') then begin
        Line:=StringReplace(Trim(ExcludeEnds(Line, '[', ']')), '-', '_', [rfReplaceAll]);
        if Line = '' then
          raise Exception.Create('null section name');
        if Assigned(Sects.Find(Line, false)) then
          raise Exception.Create('duplicate section name: ' + Line);
        Sects.Add(Line);
        Sect:=Line;
        if FLanguages <> '' then
          Cat(FLanguages, COMMA + SPACE);
        Cat(FLanguages, Sect);
        LogMessage(vbExcessive, TAB + 'Language: ' + Sect);
        Sect:=Sect + ';';
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
          S:=';' + Sect;
          N:=Add(U, PasExt.ToBytes(S));
          Inc(WC);
        end else begin
          S:=PasExt.ToString(N.Data);
          if Pos(';' + Sect, S) > 0 then begin
            LogMessage(vbVerbose, WhenTrue(VerboseLevel=vbExcessive,
              TAB + 'duplicate word: ', 'duplicate dictionary word: ') +
              RawByteString(U) + ' [' + ExcludeTrailing(Sect, ';') + ']');
            Continue;
          end;
          N.Data:=PasExt.ToBytes(S+Sect);
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
      IntToStr(Sects.Count) + ' languages.');
    LogMessage(vbVerbose, TAB+FLanguages);
    Sects.Free;
  end;
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
  SS : TStringStream;
  Lang, Langs : String;
  N : TBinaryTreeNode;
  LW : integer;
begin
  if FFileName = '' then Exit;
  SS:=TStringStream.Create('');
  try
    SS.Position:=0;
    Langs:=FLanguages;
    while Langs <> '' do begin
      Lang:=Trim(PopDelim(Langs, COMMA));
      if Lang='' then Continue;
      SS.WriteString('[' + Lang + ']' + LF + LF);
      Lang:=';'+Lang+';';
      N:=First;
      LW:=0;
      While Assigned(N) do begin
        if Pos(Lang, PasExt.ToString(N.Data)) > 0 then begin
          if (LW > 0) then begin
            if (LW + Length(N.UniqueID) + 2 > 79) then begin
              LW:=0;
              SS.WriteString(LF)
            end else begin
              Inc(LW, 2);
              SS.WriteString(COMMA + SPACE)
            end;
          end;
          INc(LW, Length(N.UniqueID));
          SS.WriteString(N.UniqueID);
        end;
        N:=N.Next;
      end;
      if LW <> 0 then
        SS.WriteString(LF);
      if Langs <> '' then
        SS.WriteString(LF);
    end;
    SS.SaveToFile(FFileName);
    FModified:=False;
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Exception saving dictionary. ' + E.Message);
     end;
  end;
  SS.Free;
end;

constructor TDictionaries.Create;
begin
  inherited Create;
  FLoaded:=False;
  FLanguages:='';
  FModified:=False;
end;

procedure TDictionaries.Clear;
begin
  inherited Clear;
  FLanguages:='';
  FLoaded:=False;
  FModified:=False;
end;


initialization

  Initialize;

finalization

  Finalize;

end.

