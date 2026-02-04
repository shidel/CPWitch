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

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Controls, ComCtrls,
  Version, PasExt, Icons, MultiApp, Codepages;
  { other forms }

type

  TWitchEncoding = (weNone, weCodePage, weUnicode);

  TWitch = class;

  { TWitchItem }

  TWitchItem = class
  private
    FAnalyzed: boolean;
    FAnalyzing: boolean;
    FDetected: String;
    FEncoding: TWitchEncoding;
    FFileName: String;
    FData : TArrayOfByte;
    FListItem: TListItem;
    FOwner: TWitch;
    FIndex: Integer;
    FPreferred: integer;
    FResults: TCodePageResults;
    function GetDisplayName: String;
    function GetIndex: integer;
    procedure SetFileName(AValue: String);
  protected
    procedure SetIndex(AIndex : Integer);
    procedure ClearData;
    procedure AnalyzeStart;
    procedure AnalyzeDone(Sender : TObject);
    procedure LoadFile(AFileName : String);
  public
    constructor Create(AOwner: TWitch);
    destructor Destroy; override;
    property Owner : TWitch read FOwner;
    property ListItem : TListItem read FListItem;
    property FileName : String read FFileName write SetFileName;
    property FileData: TArrayOfByte read FData;
    property DisplayName : String read GetDisplayName;
    property Index : integer read GetIndex;
    property Analyzed : boolean read FAnalyzed;
    property Encoding : TWitchEncoding read FEncoding;
    property Results : TCodePageResults read FResults;
    property Detected : String read FDetected; // Detected Language
    property Preferred : integer read FPreferred; // Preferred Codepage
  published
  end;

  { TWitchItems }

  TWitchItems = array of TWitchItem;

  { TWitch }

  TWitch = class
  private
    FItems: TWitchItems;
    FOnAnalyzed: TNotifyEvent;
    function GetCount: integer;
    procedure SetItems(AValue: TWitchItems);
    procedure SetOnAnalyzed(AValue: TNotifyEvent);
  protected
    procedure ValidIndex(Index : integer);
    procedure Remove(Index:integer);
    procedure ThreadComplete(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Items : TWitchItems read FItems write SetItems;
    property Count : integer read GetCount;
    function IndexOf(Item : TWitchItem) : integer;
    function Find(FileName : String; CaseSensitive : boolean = true) : integer;
    function Add(FileName : String; ListItem : TListItem = nil) : integer; overload;
    procedure Delete(Index : Integer); overload;
    procedure Delete(Item : TWitchItem); overload;
    procedure Select(Index : Integer); overload;
    procedure Select(Item : TWitchItem); overload;
  published
    property OnAnalyzed : TNotifyEvent read FOnAnalyzed write SetOnAnalyzed;
  end;

implementation

uses TaskMngr;

{$DEFINE Slow_Analyze}

type

  { TWitchAnalyzeThread }

  TWitchAnalyzeThread=class(TThreadTask)
  private
    FEncoding: TWitchEncoding;
    FResults: TCodePageResults;
    FText: RawByteString;
    FWitch: TWitch;
    FWitchItem: TWitchItem;
    procedure SetText(AValue: RawByteString);
    procedure SetWitch(AValue: TWitch);
    procedure SetWitchItem(AValue: TWitchItem);
  protected
    procedure Execute; override;
    procedure Completed;
    procedure AnalyzeUTF8;
    procedure AnalyzeCP;
  public
    constructor Create(CreateSuspended:Boolean);
    property Witch : TWitch read FWitch write SetWitch;
    property WitchItem : TWitchItem read FWitchItem write SetWitchItem;
    property Text : RawByteString read FText write SetText;
    property Encoding : TWitchEncoding read FEncoding;
    property Results : TCodePageResults read FResults;
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

procedure TWitchAnalyzeThread.SetWitch(AValue: TWitch);
begin
  if FWitch=AValue then Exit;
  FWitch:=AValue;
end;

procedure TWitchAnalyzeThread.Execute;
var
  I : integer;
  V : TArrayOfInt32;
begin
  If Not (Assigned(FWitch) and Assigned(FWitchItem)) then Exit;

  FEncoding:=weNone;
  // Test for characters above ASCII 127
  for I := 1 to Length(FText) do
    if Byte(FText[I]) > 127 then begin
      FEncoding:=weCodepage;
      Break;
  {$IFDEF Slow_Analyze}
    end else Sleep(3);
  {$ELSE}
    end;
  {$ENDIF}
  // Now if there are, see if it is Codepage or UTF-8.
  if FEncoding <> weNone then begin
    if UTF8ToValues(FText, V) then begin
      if Length(V) <> Length(FText) then
        FEncoding:=weUnicode;
    end;
  end;

  case FEncoding of
    weCodePage:AnalyzeCP;
    weUnicode:AnalyzeUTF8;
  end;

  Synchronize(@Completed);
end;

procedure TWitchAnalyzeThread.Completed;
begin
  FWitchItem.FResults:=Results;
  FWitch.ThreadComplete(Self);
end;

procedure TWitchAnalyzeThread.AnalyzeUTF8;
var
  A : TUTF8Analyze;
begin
  A:=TUTF8Analyze.Create(ToBytes(FText));
  FResults:=A.Results;
  A.Free;
end;

procedure TWitchAnalyzeThread.AnalyzeCP;
begin

end;

constructor TWitchAnalyzeThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:=True;
  FText:='';
  FWitch:=nil;
  FWitchItem:=nil;
  FResults:=[];
end;

{ TWitchItem }

procedure TWitchItem.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  LoadFile(AValue);
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
  FEncoding:=weNone;
  FAnalyzed:=False;
  FAnalyzing:=False;
  FDetected:='';
  FPreferred:=-1;
  SetLength(FResults, 0);
  SetLength(FData, 0);
end;

procedure TWitchItem.AnalyzeStart;
var
  T : TWitchAnalyzeThread;
begin
  if not (Assigned(FOwner) and Assigned(FOwner.FOnAnalyzed)) then Exit;
  if FAnalyzed then begin
    if Assigned(FListItem) then
      FOwner.FOnAnalyzed(Self);
  end else
  if not FAnalyzing then begin
    FAnalyzing:=True;
    T := TWitchAnalyzeThread.Create(True);
    T.Witch:=FOwner;
    T.WitchItem:=Self;
    T.Text:=PasExt.ToString(FData);
    QueueTask(T);
  end;

end;

procedure TWitchItem.AnalyzeDone(Sender: TObject);
begin
  FAnalyzing:=False;
  FAnalyzed:=True;
  if Sender is TWitchAnalyzeThread then begin
    FEncoding:=TWitchAnalyzeThread(Sender).Encoding;
  end;
  if Assigned(FListItem) and Assigned(FOwner) and Assigned(FOwner.FOnAnalyzed) then
    FOwner.FOnAnalyzed(Self);
end;

procedure TWitchItem.LoadFile(AFileName : String);
var
  E : integer;
begin
  ClearData;
  if AFileName = '' then Exit;
  repeat
    E:=FileLoad(AFileName, FData);
    if E <> 0 then begin
      ClearData;
      if FileErrorDialog(AFileName, E, false) <> mrRetry then
        raise Exception.Create('file read error');
    end;
  until E=0;
  FFileName:=AFileName;
end;

constructor TWitchItem.Create(AOwner: TWitch);
begin
  inherited Create;
  FOwner:=AOwner;
  FListItem:=nil;
  FIndex:=-1;
  FResults:=[];
  FData:=[];
  ClearData;
end;

destructor TWitchItem.Destroy;
begin
  FListItem:=nil;
  if Assigned(FOwner) then
    FOwner.Remove(FOwner.IndexOf(Self));
  inherited Destroy;
end;

{ TWitch }

function TWitch.GetCount: integer;
begin
  Result:=Length(FItems);
end;

procedure TWitch.SetItems(AValue: TWitchItems);
begin
  if FItems=AValue then Exit;
  FItems:=AValue;
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
  I := IndexOf(TWitchAnalyzeThread(Sender).WitchItem);
  // Item no longer exists.
  if I = -1 then Exit;
  FItems[I].AnalyzeDone(Sender);
end;

constructor TWitch.Create;
begin
  inherited Create;
  FItems:=[];
  FOnAnalyzed:=nil;
end;

destructor TWitch.Destroy;
var
  I : Integer;
begin
  for I := Low(FItems) to High(FItems) do begin
    FItems[I].FOwner:=nil;
    FItems[I].FListItem:=nil;
    FreeAndNil(FItems[I]);
  end;
  inherited Destroy;
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
