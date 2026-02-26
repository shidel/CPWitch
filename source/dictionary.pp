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

var
  DictionaryWords : TBinaryTree;

implementation

{$UNDEF CaseSpecific}

procedure LoadDictionary;
var
  E : integer;
  FileName: String;
  FileText : RawByteString;
  Sects : TStringList;
  Sect, Line, S : RawByteString;
  U : UnicodeString;
  N : TBinaryTreeNode;
  WC:Integer;
begin
  WC:=0;
  FileName:=AppDataPath+'dictionary.cpw';
  if not FileExists(FileName) then begin
    LogMessage(vbMinimal, 'Cannot find dictionary file: ' + ExtractRelativepath(AppBasePath, FileName));
    Exit;
  end;
  E :=FileLoad(FileName, FileText);
  if E <> 0 then
    raise Exception.Create('reading file: ' + ExtractRelativepath(AppBasePath, FileName));
  try
    LogMessage(vbExcessive, 'Processing dictionary file');
    Sect:='';
    Sects:=TStringList.Create;
    FileText:=StringReplace(FileText, CR, LF, [rfReplaceAll]);
    while Length(FileText) > 0 do begin
      Line:=Trim(PopDelim(FileText, LF));
      if Length(Line) = 0 then Continue;
      if HasEnds(Line, '[', ']') then begin
        Line:=Trim(ExcludeEnds(Line, '[', ']'));
        if Line = '' then
          raise Exception.Create('null section name');
        if Sects.IndexOf(UpperCase(Line)) <> -1 then
          raise Exception.Create('duplicate section name: ' + Line);
        Sects.Add(UpperCase(Line));
        Sect:=Line;
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
        N:=DictionaryWords.Find(U);
        if not Assigned(N) then begin
          S:=';' + Sect;
          N:=DictionaryWords.Add(U, PasExt.ToBytes(S));
        end else begin
          S:=PasExt.ToString(N.Data);
          if Pos(';' + Sect, S) > 0 then begin
            LogMessage(vbExcessive, TAB2 + 'duplicate word: ' + RawByteString(U));
            Continue;
          end;
          N.Data:=PasExt.ToBytes(S+Sect);
        end;
        Inc(WC);
      end;
    end;
    DictionaryWords.Balance;
    {$IFDEF BUILD_DEBUG}
    N:=DictionaryWords.First;
    While Assigned(N) do begin
      LogMessage(VerboseLevel, N.UniqueID + ' ' + PasExt.ToString(N.Data));
      N:=N.Next;
    end;
    {$ENDIF}
  finally
    LogMessage(vbVerbose, 'Dictionary contains ' + IntToStr(WC) + ' and ' +
      IntToStr(Sects.Count) + ' languages.');
    Sects.Free;
  end;
end;

procedure Initialize;
begin
  try
    DictionaryWords:=TBinaryTree.Create;
    LoadDictionary;
  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Exception opening dictionary. ' + E.Message);
      DictionaryWords:=nil;
    end;
  end;
end;

procedure Finalize;
begin
  if Assigned(DictionaryWords) then
    FreeAndNil(DictionaryWords);
end;


initialization

  Initialize;

finalization

  Finalize;

end.

