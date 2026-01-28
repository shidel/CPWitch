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

  TWitch = class;

  { TWitchItem }

  TWitchItem = class
  private
    FFileName: String;
    FOwner: TWitch;
    FIndex: Integer;
    function GetIndex: integer;
    procedure SetFileName(AValue: String);
    procedure SetOwner(AValue: TWitch);
  protected
    procedure SetIndex(AIndex : Integer);
  public
    constructor Create(AOwner: TWitch);
    destructor Destroy; override;
    property Owner : TWitch read FOwner write SetOwner;
    property FileName : String read FFileName write SetFileName;
    property Index : integer read GetIndex;
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
  FFileName:=AValue;
end;

function TWitchItem.GetIndex: integer;
begin
  Result:=FIndex;
end;

procedure TWitchItem.SetIndex(AIndex: Integer);
begin
  FIndex:=AIndex;
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
