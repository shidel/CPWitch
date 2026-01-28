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
  Classes, SysUtils,
  Version, PasExt, Icons, MultiApp;
  { other forms }

type

  TWitchEncoding = (weNone, weCodePage, weUnicode);

  TWitch = class;

  { TWitchItem }

  TWitchItem = class
  private
    FEncoding: TWitchEncoding;
    FFileName: String;
    FData : TArrayOfByte;
    FOwner: TWitch;
    FIndex: Integer;
    function GetDisplayName: String;
    function GetIndex: integer;
    procedure SetFileName(AValue: String);
    procedure SetOwner(AValue: TWitch);
  protected
    procedure SetIndex(AIndex : Integer);
    procedure ClearData;
    procedure AnalyzeData;
    procedure LoadFile(AFileName : String);
  public
    constructor Create(AOwner: TWitch);
    destructor Destroy; override;
    property Owner : TWitch read FOwner write SetOwner;
    property FileName : String read FFileName write SetFileName;
    property DisplayName : String read GetDisplayName;
    property Index : integer read GetIndex;
    property Encoding : TWitchEncoding read FEncoding;
  published
  end;

  { TWitchItems }

  TWitchItems = array of TWitchItem;

  { TWitch }

  TWitch = class
  private
    FItems: TWitchItems;
    function GetCount: integer;
    procedure SetItems(AValue: TWitchItems);
  protected
    procedure ValidIndex(Index : integer);
    procedure Remove(Index:integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Items : TWitchItems read FItems write SetItems;
    property Count : integer read GetCount;
    function Find(FileName : String; CaseSensitive : boolean = true) : integer;
    function Add(FileName : String) : integer;
    procedure Delete(Index : Integer); overload;
    procedure Delete(Item : TWitchItem); overload;
    function IndexOf(Item : TWitchItem) : integer;
  published
  end;

implementation

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
  SetLength(FData, 0);
end;

procedure TWitchItem.AnalyzeData;
var
  I : integer;
begin
  // First check if any characters > 127 exist
  for I := Low(FData) to High(FData) do
    if FData[I] > 127 then begin
      FEncoding:=weCodepage;
      Break;
    end;
  // Now if there are, see if it is Codepage or Unicode
  if FEncoding <> weNone then begin
    if IsUnicode(PasExt.ToString(FData)) then
      FEncoding:=weUnicode;
  end;
end;

procedure TWitchItem.LoadFile(AFileName : String);
var
  E : integer;
begin
  ClearData;
  if AFileName = '' then Exit;
  E:=FileLoad(AFileName, FData);
  if E <> 0 then begin
    ClearData;
    FileErrorDialog(AFileName, E, false);
    raise Exception.Create('file read error');
  end;
  FFileName:=AFileName;
  AnalyzeData;
end;

procedure TWitchItem.SetOwner(AValue: TWitch);
begin
  if FOwner=AValue then Exit;
  FOwner:=AValue;
end;

constructor TWitchItem.Create(AOwner: TWitch);
begin
  inherited Create;
  FOwner:=AOwner;
  FIndex:=-1;
  FData:=[];
  ClearData;
end;

destructor TWitchItem.Destroy;
begin
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
  FItems[Index].Owner:=nil;
  FItems[Index].SetIndex(-1);
  for I := Index to High(FItems) - 1 do  begin
    FItems[I]:=FItems[I+1];
    FItems[I].SetIndex(I);
  end;
  SetLength(FItems, Length(FItems) - 1);
end;

constructor TWitch.Create;
begin
  inherited Create;
  FItems:=[];
end;

destructor TWitch.Destroy;
var
  I : Integer;
begin
  for I := Low(FItems) to High(FItems) do begin
    FItems[I].Owner:=nil;
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

function TWitch.Add(FileName: String): integer;
var
  W : TWitchItem;
begin
  W := TWitchItem.Create(Self);
  try
    W.FileName := FileName;
  except
    W.Owner:=nil;
    FreeAndNil(W);
  end;
  if not Assigned(W) then begin
    Result:=-1;
    LogMessage(vbCritical, 'unable to create TWitchItem for ' + FileName);
    raise Exception.Create('unable to create TWitchItem for ' + FileName);
  end;
  SetLength(FItems, Length(FItems) + 1);
  Result:=High(FItems);
  FItems[Result] := W;
  W.SetIndex(Result);
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
