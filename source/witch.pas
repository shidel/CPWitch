{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit Witch;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

{ DEFINE Slow_Analyze}

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Controls, ComCtrls,
  Version, PasExt, Icons, MultiApp, Codepages, Dictionary;


 { TODO 2 -cDevel Improve TWitchItem to load files in Analize Thread. Report
   errors back to UI. UI show in Red and prompt when selected. }

type

  TWitchEncoding = (weNone, weCodepage, weUnicode, weBinary, weError);

  TWitch = class;

  { TWitchItem }

  TWitchItem = class
  private
    FErrorCode: integer;
    FThread: TThread;
    FAnalyzed: boolean;
    FAnalyzing: boolean;
    FDetected: integer;
    FLocale: String;
    FEncoding: TWitchEncoding;
    FEndsWithBlank: boolean;
    FFileName: String;
    FData : TArrayOfByte;
    FLineEndings: TLineEndings;
    FListItem: TListItem;
    FOwner: TWitch;
    FIndex: Integer;
    FPreferred: integer;
    FResults: TCodepageResults;
    FDateTime : TDateTime;
    function GetDisplayName: String;
    function GetIndex: integer;
    procedure SetFileName(AValue: String);
  protected
    procedure SetIndex(AIndex : Integer);
    procedure ClearData;
    procedure AnalyzeStart;
    procedure AnalyzeDone(Sender : TObject);
    function FileChanged : boolean;
  public
    constructor Create(AOwner: TWitch);
    destructor Destroy; override;
    procedure Abort;
    property Owner : TWitch read FOwner;
    property ListItem : TListItem read FListItem write FListItem;
    property FileName : String read FFileName write SetFileName;
    property LineEndings : TLineEndings read FLineEndings;
    property FileData: TArrayOfByte read FData;
    property DisplayName : String read GetDisplayName;
    property Index : integer read GetIndex;
    property Analyzed : boolean read FAnalyzed;
    property Encoding : TWitchEncoding read FEncoding;
    property Results : TCodepageResults read FResults;
    property Locale : String read FLocale; // Detected Language
    property Preferred : integer read FPreferred; // Preferred Codepage
    property Detected: integer read FDetected; // Detected probable Codepage
    property EndsWithBlank : boolean read FEndsWithBlank;
    property ErrorCode : integer read FErrorCode;
    function AsCodePage(Codepage : integer; Convert : boolean = true) : TUTF8ToCodepage;
    function AsUnicode(Codepage : integer; Convert : boolean = true) : TCodepageToUTF8;
  published
  end;

  { TWitchItems }

  TWitchItems = array of TWitchItem;

  { TWitch }

  TWitch = class
  private
    FItems: TWitchItems;
    FOnAnalyzed: TNotifyEvent;
    FOnModified: TNotifyEvent;
    function GetCount: integer;
    function GetModified(Index : integer): boolean;
    procedure SetItems(AValue: TWitchItems);
    procedure SetModified(Index : integer; AValue: boolean);
    procedure SetOnAnalyzed(AValue: TNotifyEvent);
    procedure SetOnModified(AValue: TNotifyEvent);
  protected
    procedure ValidIndex(Index : integer);
    procedure Remove(Index:integer);
    procedure ThreadComplete(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Items : TWitchItems read FItems write SetItems;
    property Count : integer read GetCount;
    function IndexOf(Item : TWitchItem) : integer;
    function Find(FileName : String; CaseSensitive : boolean = true) : integer;
    function Add(FileName : String; ListItem : TListItem = nil) : integer; overload;
    procedure AbortAll;
    procedure Abort(Index : Integer); overload;
    procedure Abort(Item : TWitchItem); overload;
    procedure Delete(Index : Integer); overload;
    procedure Delete(Item : TWitchItem); overload;
    procedure Select(Index : Integer); overload;
    procedure Select(Item : TWitchItem); overload;
    function FileModified(Index : integer) : boolean; overload;
    function FileModified(FileName : String) : boolean; overload;
    property Modified[Index : integer] : boolean read GetModified write SetModified;
  published
    property OnAnalyzed : TNotifyEvent read FOnAnalyzed write SetOnAnalyzed;
    property OnModified : TNotifyEvent read FOnModified write SetOnModified;
  end;

implementation

uses TaskMngr;

{ DEFINE Slow_Analyze}

procedure dlg(Message : String); overload;
begin
  LogMessage(vbCritical, Message);
end;

procedure dlg(Message : String; I : Int64); overload;
begin
  LogMessage(vbCritical, Message, I);
end;

type

  { TWitchAnalyzeThread }

  TWitchAnalyzeThread=class(TThread)
  private
    FErrorCode : integer;
    FDetected: integer;
    FEncoding: TWitchEncoding;
    FEndsWithBlank: boolean;
    FFileName: String;
    FLineEndings: TLineEndings;
    FLocale: RawByteString;
    FPreferred: integer;
    FResults: TCodepageResults;
    FText: RawByteString;
    FWitch: TWitch;
    FWitchItem: TWitchItem;
    procedure SetFileName(AValue: String);
    procedure SetText(AValue: RawByteString);
    procedure SetWitch(AValue: TWitch);
    procedure SetWitchItem(AValue: TWitchItem);
  protected
    procedure Execute; override;
    procedure Completed;
    function NoComments : RawByteString;
    procedure AnalyzeLineEndings;
    procedure AnalyzeUTF8;
    procedure AnalyzeCP;
    procedure AnalyzeASCII;
  public
    procedure AfterConstruction; override;
    // constructor Create(CreateSuspended:Boolean); override;
    property Witch : TWitch read FWitch write SetWitch;
    property WitchItem : TWitchItem read FWitchItem write SetWitchItem;
    property FileName : String read FFileName write SetFileName;
    property Text : RawByteString read FText write SetText;
    property Encoding : TWitchEncoding read FEncoding;
    property Results : TCodepageResults read FResults;
    property LineEndings : TLineEndings read FLineEndings;
    property EndsWithBlank : boolean read FEndsWithBlank;
    property Locale : RawByteString read FLocale;
    property Preferred : integer read FPreferred;
    property Detected: integer read FDetected;
  end;

{ TWitchAnalyzeThread }

procedure TWitchAnalyzeThread.SetWitchItem(AValue: TWitchItem);
begin
  if FWitchItem=AValue then Exit;
  FWitchItem:=AValue;
end;

procedure TWitchAnalyzeThread.SetText(AValue: RawByteString);
begin
  if FText=AValue then Exit;
  FText:=AValue;
end;

procedure TWitchAnalyzeThread.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TWitchAnalyzeThread.SetWitch(AValue: TWitch);
begin
  if FWitch=AValue then Exit;
  FWitch:=AValue;
end;

procedure TWitchAnalyzeThread.Execute;
var
  I : integer;
  V : TArrayOfInt32;
  L : TLocale;
begin
  If Not (Assigned(FWitch) and Assigned(FWitchItem)) then Exit;

  // Initial "Unknown" state
  FLineEndings:=leCRLF;
  FEndsWithBlank:=False;
  FEncoding:=weNone;
  FLocale:='';
  FPreferred:=-1;
  FDetected:=-1;
  FResults:=[];
  FErrorCode:=0;

  if Terminated then Exit;
  {$IFDEF Slow_Analyze}
    Sleep(200);
  {$ENDIF}

  try
    FErrorCode:=FileLoad(FFileName, FText);
    if FErrorCode <> 0 then
      raise Exception.Create('error loading file');
  except
    FText:='';
    FErrorCode:=5;
    FEncoding:=weError;
    Synchronize(@Completed);
    Exit;
  end;

  // Test for characters above ASCII 127
  for I := 1 to Length(FText) do
    if Byte(FText[I]) = 0 then begin
      FEncoding:=weBinary;
      Break;
    end else if Byte(FText[I]) > 127 then begin
      FEncoding:=weCodepage;
    end;

  if Terminated then Exit;

  // Now if there are, see if it is Codepage or UTF-8.
  if (FEncoding = weCodepage) then begin
    if UTF8ToValues(FText, V) then begin
      if Length(V) <> Length(FText) then
        FEncoding:=weUnicode;
    end;
  end;

  if Terminated then Exit;

  case FEncoding of
    weBinary:begin end;
    weNone : AnalyzeASCII;
    weCodepage:AnalyzeCP;
    weUnicode:AnalyzeUTF8;
  end;

  if Terminated then Exit;

  if Codepages.Locale(FLocale, L) then
    FPreferred:=L.Codepage;

  Synchronize(@Completed);
end;

procedure TWitchAnalyzeThread.Completed;
var
  I : Integer;
  S : String;
begin
  if Terminated then Exit;
  If Not (Assigned(FWitch) and Assigned(FWitchItem)) then Exit;
  FWitchItem.FEncoding:=FEncoding;
  FWitchItem.FErrorCode:=FErrorCode;
  try
    if (FEncoding = weError) or (FEncoding = weBinary) then
      FWitchItem.FData:=[]
    else
      FWitchItem.FData:=PasExt.ToBytes(FText);
  except
    FWitchItem.FData:=[];
  end;
  FWitchItem.FLineEndings:=FLineEndings;
  FWitchItem.FEndsWithBlank:=FEndsWithBlank;
  FWitchItem.FResults:=Copy(FResults);
  FWitchItem.FLocale:=Flocale;
  FWitchItem.FPreferred:=FPreferred;
  FWitchItem.FDetected:=FDetected;
  if FEncoding = weCodePage then
  if VerboseLevel > vbVerbose then begin
    S:=TAB+'[Locale: ' + FLocale + ', Preferred: ' + IntToStr(FPreferred) +
    ', Detected: '+IntToStr(FDetected) + ']' + LF;
    for I := 0 to High(FResults) do begin
        Cat(S, Tab+'Codepage: ' + IntToStr(FResults[I].Codepage) + LF);
        Cat(S, Tab2+'Characters: ' + IntToStr(FResults[I].Characters) + LF);
        Cat(S, Tab2+'ASCII: ' + IntToStr(FResults[I].ASCII) + LF);
        Cat(S, Tab2+'Unicode: ' + IntToStr(FResults[I].Unicode) + LF);
        Cat(S, Tab2+'Converted: ' + IntToStr(FResults[I].Converted) + LF);
        Cat(S, Tab2+'Compatible: ' + IntToStr(FResults[I].Compatible) + LF);
      end;
    LogMessage(vbNormal,'Analysis Results for: ' + FriendlyPath(AppBasePath, FFileName) +  LF + S);
  end else
    LogMessage(vbVerbose, 'Analyzed: ' + FriendlyPath(AppBasePath, FFileName));

  FWitch.ThreadComplete(Self);
end;

function TWitchAnalyzeThread.NoComments: RawByteString;
var
  S : RawByteString;
  I, C : integer;
begin
  Result:='';
  try
    S:=NormalizeLineEndings(FText, LF) + LF;
    SetLength(Result, Length(S));
    I:=1;
    C:=1;
    While I <= Length(S) do begin
     // Trim Indentation
     While ((S[I] = SPACE) or (S[I]=TAB)) do Inc(I);
     // Check for Comment, Skip to EOL
     if (S[I] = ';') or (S[I] = '#') then
       While (S[I] <> LF) do Inc(I);
     // Add Line, could be blank, don't care
     repeat
       Result[C]:=S[I];
       Inc(C);
       Inc(I);
     until S[I-1]=LF;
    end;
  except
    Result:='';
  end;
end;

procedure TWitchAnalyzeThread.AnalyzeLineEndings;
begin
  FLineEndings:=leCRLF;
  if (FEncoding = weBinary) then Exit;
  FLineEndings:=DetectLineEndings(FText, FLineEndings);
  case FLineEndings of
    leCRLF : if (Length(FText) > 1) then
      FEndsWithBlank:=Copy(FText, Length(FText) -1, 2) = CRLF;
    leLF : if Length(FText) > 0 then
      FEndsWithBlank:=Copy(FText, Length(FText), 1) = LF;
    leCR : if Length(FText) > 0 then
      FEndsWithBlank:=Copy(FText, Length(FText), 1) = CR;
  end;
end;

procedure TWitchAnalyzeThread.AnalyzeUTF8;
var
  A : TUTF8Analyze;
  S : RawByteString;
begin
  AnalyzeLineEndings;
  if Terminated then Exit;
  S:=NoComments;
  if Terminated then Exit;
  A:=TUTF8Analyze.Create(ToBytes(S));
  FResults:=A.Results;
  A.Free;
  if Terminated then Exit;
  FLocale:=DetectLocale(S);
end;

procedure TWitchAnalyzeThread.AnalyzeCP;
var
  I : Integer;
  A : TCodepageToUTF8;
  S : RawByteString;
  CPL : TArrayOfInteger;
  LC : TLocale;
  BestCP,
  BestLocale,
  BestScore : Integer;
  Stats: TArrayOfInt32;
begin
  try
    AnalyzeLineEndings;
    if Terminated then Exit;
    S:=NoComments;
    if Terminated then Exit;
    if Length(S) > 8192 then // Cap size to process for detection. It will likely
      SetLength(S, 8192);    // cut a word. But, that should not skew results.
    CPL:=CodepageList;

    SetLength(FResults, Length(CPL));
    BestLocale:=-1;
    BestScore:=-1;
    BestCP:=-1;
    FLocale:='';

    try
      A :=TCodepageToUTF8.Create;
      for I := 0 to  High(CPL) do begin
        if Terminated then begin
          A.Free;
          Exit;
        end;
        A.Clear;
        A.Codepage:=CPL[I];
        A.ControlCodes:=False; // Don't worry about ASCII control codes
        A.Expanded:=False;     // Don't bother expanding TABS
        A.Source:=S;
        A.Convert;
        // Just setting it, going to repurpose a couple fields.
        // Going to use Unicode for Word Count, Converted for Most Words found.
        // Then later, adjust compatibility for how compatible we think this one is.
        FResults[I]:=A.Results;
        FResults[I].Unicode:=DetectLocale(RawByteString(A.Converted), Stats); // total probable words
        if FResults[I].Unicode > 0 then begin
          FResults[I].Converted:=PasExt.Maximum(Stats); // Highest value in array
          if FResults[I].Converted>BestScore then begin
            BestCP:=CPL[I];
            BestScore:=FResults[I].Converted;
            BestLocale:=PasExt.Maximum(Stats, True); // Index of Highest value in array.
          end;
        end else begin
          FResults[I].Converted:=0;
          FResults[I].Compatible:=0;
        end;
      end;
      if Terminated then begin
        A.Free;
        Exit;
      end;
      if BestLocale < 0 then begin
        FDetected:=-1;
        FPreferred:=-1;
      end else begin
       FDetected:=BestCP;
       // convert BestLocale Index into Locale String
        if BestLocale >= MasterLocaleCount then
          BestLocale:=BestLocale-MasterLocaleCount+UserLocaleOffset;
        if Assigned(MasterDictionary) then
          FLocale:=MasterDictionary.Locale[BestLocale];
        // Lookup the appropriate codepage for that locale
        if Codepages.Locale(FLocale, LC) then
          FPreferred:=LC.Codepage;
      end;
      for I := 0 to High(FResults) do
        if FResults[I].Unicode > 0 then begin
          // With math rounding errors and such only show 100 for perfect match
          if BestScore = FResults[I].Converted then
            FResults[I].Compatible:=100
          else begin
            FResults[I].Compatible := (FResults[I].Converted * 100) div BestScore;
            // Cap unperfect match at 99%
            if FResults[I].Compatible > 99 then
              FResults[I].Compatible:=99
            else
            // Minimum umperfect match of 1%
            if FResults[I].Compatible < 1 then
              FResults[I].Compatible:=1;
          end;
        end;
    finally
      A.Free;
    end;
  except
    FEncoding:=weBinary; // Not supported, error, etc.
    FLocale:='';
    FDetected:=-1;
    FPreferred:=-1;
    FLineEndings:=leCRLF;
    FResults:=[];
  end;

end;

procedure TWitchAnalyzeThread.AnalyzeASCII;
var
  S : RawByteString;
begin
  AnalyzeLineEndings;
  if Terminated then Exit;
  S:=NoComments;
  if Terminated then Exit;
  FLocale:=DetectLocale(S);
end;

procedure TWitchAnalyzeThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FreeOnTerminate:=True;
  Priority:=tpLowest;
  FText:='';
  FWitch:=nil;
  FWitchItem:=nil;
  FResults:=[];
  FLocale:='';
  FPreferred:=-1;
end;

{ TWitchItem }

procedure TWitchItem.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  ClearData;
  if AValue = '' then Exit;
  FFileName:=AValue;
  if not FileAge(FFileName, FDateTime) then
    FDateTime:=-1;
//   E:=FileLoad(AFileName, FData);
//    if E <> 0 then begin
//      ClearData;
//      if FileErrorDialog(AFileName, E, false) <> mrRetry then
//        raise Exception.Create('file read error');
//    end;
//  until E=0;
end;

function TWitchItem.GetIndex: integer;
begin
  Result:=FIndex;
end;

function TWitchItem.GetDisplayName: String;
begin
  Result:=ExtractFileName(FFileName);
end;

procedure TWitchItem.SetIndex(AIndex: Integer);
begin
  FIndex:=AIndex;
end;

procedure TWitchItem.ClearData;
begin
  FFileName:='';
  FEndsWithBlank:=False;
  FLineEndings:=leCRLF;
  FEncoding:=weNone;
  FAnalyzed:=False;
  FAnalyzing:=False;
  FLocale:='';
  FPreferred:=-1;
  FDetected:=-1;
  FDateTime:=-1;
  FErrorCode:=0;
  SetLength(FResults, 0);
  SetLength(FData, 0);
end;

procedure TWitchItem.AnalyzeStart;
begin
  if not (Assigned(FOwner) and Assigned(FOwner.FOnAnalyzed)) then Exit;
  if FAnalyzed then begin
    if Assigned(FListItem) then
      FOwner.FOnAnalyzed(Self);
  end else
  if not FAnalyzing then begin
    if FFileName = '' then begin
      FAnalyzed:=True;
      FEncoding:=weError;
      FErrorCode:=2;
      AnalyzeDOne(Self);
      Exit;
    end;
    FAnalyzing:=True;
    FThread := TWitchAnalyzeThread.Create(True);
    TWitchAnalyzeThread(FThread).Witch:=FOwner;
    TWitchAnalyzeThread(FThread).WitchItem:=Self;
    TWitchAnalyzeThread(FThread).FileName:=FFileName;
    // PasExt.ToString(FData);
    QueueTask(FThread);
  end;
end;

procedure TWitchItem.AnalyzeDone(Sender: TObject);
begin
  if FileChanged then Exit;
  FAnalyzing:=False;
  FAnalyzed:=True;
  if Assigned(FListItem) and Assigned(FOwner) and Assigned(FOwner.FOnAnalyzed) then
    FOwner.FOnAnalyzed(Self);
end;

function TWitchItem.FileChanged: boolean;
var
  DT : TDateTime;
  FN : String;
begin
  Result:=False;
  if FileExists(FileName) then begin
    if FileAge(FFileName, DT) then
       if DT = FDateTime then Exit;
    LogMessage(vbVerbose, 'File modified: ' + FriendlyPath(AppBasePath, FileName));
    FN:=FFileName;
    FFileName:='';
    FileName:=FN;
    Result:=True;
    if Assigned(FListItem) then begin
      ListItem.ImageIndex:=idxFileTypeFilePlainOrange;
      if Assigned(FOwner) and Assigned(FOwner.FOnModified) then
        FOwner.FOnModified(Self);
    end;
    if Assigned(FListItem) and Assigned(FOwner) and Assigned(FOwner.FOnAnalyzed) then
      AnalyzeStart;
  end else begin
    if FDateTime = -1 then Exit;
    FDateTime:=-1;
    LogMessage(vbVerbose, 'File deleted: ' + FriendlyPath(AppBasePath, FileName));
    FAnalyzed:=True;
    FEncoding:=weError;
    FErrorCode:=2;
    if Assigned(FListItem) then begin
      ListItem.ImageIndex:=idxFileTypeFilePlainRed;
      if Assigned(FOwner) then begin
        if Assigned(FOwner.FOnModified) then
          FOwner.FOnModified(Self);
        if Assigned(FOwner.FOnAnalyzed) then
          FOwner.FOnAnalyzed(Self);
      end;
    end;
  end;
end;

constructor TWitchItem.Create(AOwner: TWitch);
begin
  inherited Create;
  FOwner:=AOwner;
  FListItem:=nil;
  FThread:=nil;
  FIndex:=-1;
  FResults:=[];
  FData:=[];
  ClearData;
end;

destructor TWitchItem.Destroy;
begin
  Abort;
  FListItem:=nil;
  if Assigned(FOwner) then
    FOwner.Remove(FOwner.IndexOf(Self));
  inherited Destroy;
end;

procedure TWitchItem.Abort;
begin
  if Assigned(FOwner) then
    FOwner.Abort(FOwner.IndexOf(Self));
end;

function TWitchItem.AsCodePage(Codepage: integer; Convert : boolean): TUTF8ToCodepage;
begin
  Result := TUTF8ToCodepage.Create;
  Result.Codepage:=Codepage;
  Result.ControlCodes:=False;
  Result.Expanded:=True;
  Result.Invalid:=$3f; { Question Mark }
  Result.Source:=UnicodeString(NormalizeLineEndings(PasExt.ToString(FData)));
  if Convert then
    Result.Convert;
end;

function TWitchItem.AsUnicode(Codepage: integer; Convert : boolean): TCodepageToUTF8;
begin
  Result := TCodepageToUTF8.Create;
  Result.Codepage:=Codepage;
  Result.ControlCodes:=False;
  Result.Expanded:=True;
  Result.Invalid:=$3f; { Question Mark }
  Result.Source:=NormalizeLineEndings(PasExt.ToString(FData));
  if Convert then
    Result.Convert;
end;

{ TWitch }

function TWitch.GetCount: integer;
begin
  Result:=Length(FItems);
end;

function TWitch.GetModified(Index : integer): boolean;
begin
  ValidIndex(Index);
  if FItems[Index].FDateTime = -1 then
    Result:=False
  else
    Result:= FItems[Index].FileChanged;
end;

procedure TWitch.SetItems(AValue: TWitchItems);
begin
  if FItems=AValue then Exit;
  FItems:=AValue;
end;

procedure TWitch.SetModified(Index : integer; AValue: boolean);
begin
  ValidIndex(Index);
  if AValue=True then
    FItems[Index].FDateTime:=FItems[Index].FDateTime - 1;
end;

procedure TWitch.SetOnAnalyzed(AValue: TNotifyEvent);
var
  I : integer;
begin
  if FOnAnalyzed=AValue then Exit;
  FOnAnalyzed:=AValue;
  if Assigned(FOnAnalyzed) then
    for I := Low(FItems) to High(FItems) do
      FItems[I].AnalyzeStart;
end;

procedure TWitch.SetOnModified(AValue: TNotifyEvent);
begin
  if FOnModified=AValue then Exit;
  FOnModified:=AValue;
end;

procedure TWitch.ValidIndex(Index: integer);
begin
  if (Index < Low(FItems)) or (Index > High(FItems)) then begin
    LogMessage(vbCritical, 'TWitch item index out of range');
    raise Exception.Create('TWitch item index out of range');
  end;
end;

procedure TWitch.Remove(Index:integer);
var
  I : integer;
begin
  ValidIndex(Index);
  Abort(Index);
  FItems[Index].FOwner:=nil;
  FItems[Index].SetIndex(-1);
  for I := Index to High(FItems) - 1 do  begin
    FItems[I]:=FItems[I+1];
    FItems[I].SetIndex(I);
  end;
  SetLength(FItems, Length(FItems) - 1);
end;

procedure TWitch.ThreadComplete(Sender: TObject);
var
  I : Integer;
begin
  if not (Sender is TWitchAnalyzeThread) then Exit;
  if not Assigned(TWitchAnalyzeThread(Sender).Witch) then Exit;
  if not Assigned(TWitchAnalyzeThread(Sender).WitchItem) then Exit;
  I := IndexOf(TWitchAnalyzeThread(Sender).WitchItem);
  // Item no longer exists.
  if I = -1 then Exit;
  FItems[I].FThread:=nil;
  FItems[I].AnalyzeDone(Sender);
end;

constructor TWitch.Create;
begin
  inherited Create;
  FItems:=[];
  FOnAnalyzed:=nil;
  FOnModified:=nil;
end;

destructor TWitch.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TWitch.Clear;
var
  I : Integer;
begin
  AbortAll;
  for I := Low(FItems) to High(FItems) do begin
    FItems[I].FOwner:=nil;
    FItems[I].FListItem:=nil;
    FItems[I].FThread:=nil;
    FreeAndNil(FItems[I]);
  end;
  FItems:=[];
end;

function TWitch.Find(FileName: String; CaseSensitive : boolean): integer;
var
  I : integer;
begin
  Result:=-1;
  if FileName = '' then Exit;
  if CaseSensitive then begin
    for I := Low(FItems) to High(FItems) do
      if FileName = FItems[I].FileName then begin
        Result:=I;
        Exit;
      end;
  end else begin
    FileName:=LowerCase(FileName);
    for I := Low(FItems) to High(FItems) do
      if FileName = LowerCase(FItems[I].FileName) then begin
        Result:=I;
        Exit;
      end;
  end;
end;

function TWitch.Add(FileName: String; ListItem : TListItem): integer;
var
  W : TWitchItem;
begin
  // Create WitchItem for File
  W := TWitchItem.Create(Self);
  try
    W.FileName := FileName;
  except
    W.FOwner:=nil;
    FreeAndNil(W);
  end;
  if not Assigned(W) then begin
    Result:=-1;
    LogMessage(vbCritical, 'unable to create TWitchItem for ' + FileName);
    raise Exception.Create('unable to create TWitchItem for ' + FileName);
  end;

  // Add Entry to Witch List
  SetLength(FItems, Length(FItems) + 1);
  Result:=High(FItems);
  FItems[Result] := W;
  W.SetIndex(Result);

  // Set ListView Item data
  ListItem.ImageIndex:=idxFileTypeFilePlainOrange;
  ListItem.Caption:=W.DisplayName;
  ListItem.Data:=Pointer(W);
  W.FListItem:=ListItem;

  if Assigned(FOnAnalyzed) then
    W.AnalyzeStart;

end;

procedure TWitch.AbortAll;
var
  I : Integer;
begin
  LogMessage(vbVerbose, 'Abort all tasks');
  for I := Low(FItems) to High(FItems) do begin
     FItems[I].FThread:=nil;
     FItems[I].ListItem:=nil;;
  end;
  CancelAllTasks;
end;

procedure TWitch.Abort(Index: Integer);
begin
  ValidIndex(Index);
  Abort(Items[Index]);
end;

procedure TWitch.Abort(Item: TWitchItem);
begin
  if (not Assigned(Item)) or (Item.Analyzed) then Exit;
  Item.ListItem:=nil;
  //LogMessage(vbVerbose, 'Item: ' + Item.FileName);
  if Assigned(Item.FThread) then begin
    //LogMessage(vbVerbose, 'Thread is Assigned');
    if Item.FThread is TWitchAnalyzeThread then begin
      LogMessage(vbVerbose, 'Abort task for ' + Item.DisplayName);
      TWitchAnalyzeThread(Item.FThread).Witch := nil;
      TWitchAnalyzeThread(Item.FThread).WitchItem := nil;
      //LogMessage(vbVerbose, 'Thread callback nullified');
    end;
    CancelTask(Item.FThread);
    // LogMessage(vbVerbose, 'Aborted Thread');
    Item.FThread:=nil;
  end;
  // LogMessage(vbVerbose, 'Abort Thread Completed');
  Item.FAnalyzing:=False;
end;

procedure TWitch.Delete(Index: Integer);
var
  W : TWitchItem;
begin
  ValidIndex(Index);
  W:=FItems[Index];
  Remove(Index);
  FreeAndNil(W);
end;

procedure TWitch.Delete(Item: TWitchItem);
begin
  if not Assigned(Item) then Exit;
  Delete(IndexOf(Item));
end;

procedure TWitch.Select(Index: Integer);
begin
  ValidIndex(Index);
  if Assigned(FItems[Index].ListItem) then
    FItems[Index].ListItem.Selected:=True;
end;

procedure TWitch.Select(Item: TWitchItem);
begin
  Select(IndexOf(Item));
end;

function TWitch.FileModified(Index: integer): boolean;
begin
  Result:=GetModified(Index);
end;

function TWitch.FileModified(FileName: String): boolean;
var
  Index:Integer;
begin
  Index:=Find(FileName);
  if Index = -1 then
    Result:=False
  else
    Result:=GetModified(Index);
end;

function TWitch.IndexOf(Item: TWitchItem): integer;
var
  I : integer;
begin
  Result:=-1;
  if not Assigned(Item) then Exit;
  for I := Low(FItems) to High(FItems) do
    if Item = FItems[I] then begin
      Result:=I;
      Exit;
    end;
end;

end.
