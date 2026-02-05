{
   Copyright (c) 2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit DosCRT;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Controls, Graphics, PasExt, CodePages, DosFont;

type

  TDosCRTScreenItem = record
    Character : Byte;
    Foreground : TColor;
    BackGround : TColor;
  end;

  { TCustomDosCRT }

  TCustomDosCRT = class ( TGraphicControl )
  private
    FFont: TCustomDosFont;
    FScale: TPoint;
    FResolution: TPoint;
    FScreen : array of TDosCRTScreenItem;
    procedure SetFont(AValue: TCustomDosFont);
    procedure SetResolution(AValue: TPoint);
    procedure SetScale(AValue: TPoint);
  protected
    FTopLeft : TPoint;
    FBottomRight : TPoint;
    FForeground : TColor;
    FBackground : TColor;
    procedure ValidateCoordinates(var P : TPoint); overload;
    procedure ValidateCoordinates(var X, Y : LongInt); overload;
    procedure DoSizeChange; virtual;
    procedure Paint; override;
    procedure PaintCharAttr(X, Y : LongInt; C : TDosCRTScreenItem);
    procedure SetScreenBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Resolution : TPoint read FResolution write SetResolution;
    property Scale : TPoint read FScale write SetScale;
    property Font : TCustomDosFont read FFont write SetFont;
  published
  end;

  { TDosCRT }

  TDosCRT = class ( TCustomDosCRT )
  private
    function GetWindMax: TPoint;
    function GetWindMin: TPoint;
    procedure SetWindMax(AValue: TPoint);
    procedure SetWindWin(AValue: TPoint);
  protected
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    { CRT Style Stuff }
    // property TextAttr : byte;
    property WindMin : TPoint read GetWindMin write SetWindWin;
    property WindMax : TPoint read GetWindMax write SetWindMax;
    // procedure Window(X1, Y1, X2, Y2 : byte);
    // procedure GotoXY(X, Y : byte);
    //function WhereX: byte;
    //function WhereY: byte;
    procedure ClrScr;
    procedure ClrEol;
    procedure InsLine;
    procedure DelLine;
    //procedure TextColor(Color : byte);
    //procedure TextBackground(Color : byte);
  published
  end;

implementation

{ TCustomDosCRT }

procedure TCustomDosCRT.SetResolution(AValue: TPoint);
begin
  if FResolution=AValue then Exit;
  FResolution:=AValue;
  DoSizeChange;
end;

procedure TCustomDosCRT.SetFont(AValue: TCustomDosFont);
begin
  if FFont=AValue then Exit;
  FFont:=AValue;
  DoSizeChange;
end;

procedure TCustomDosCRT.SetScale(AValue: TPoint);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  DoSizeChange;
end;

procedure TCustomDosCRT.ValidateCoordinates(var P: TPoint);
begin
  ValidateCoordinates(P.X, P.Y);
end;

procedure TCustomDosCRT.ValidateCoordinates(var X, Y: LongInt);
begin
  if X < 0 then X := 0;
  if Y < 0 then Y := 0;
  if X >= FResolution.X then X:=FResolution.X - 1;
  if Y >= FResolution.Y then Y:=FResolution.Y - 1;
end;

procedure TCustomDosCRT.DoSizeChange;
begin
  if Assigned(FFont) then begin
    Width:=FResolution.X * FFont.Width * FScale.X;
    Height:=FResolution.X * FFont.Height * FScale.Y;
  end else begin
    Width:=FResolution.X * 8 * FScale.X;
    Height:=FResolution.X * 8 * FScale.Y;
  end;
  SetScreenBuffer;
  Invalidate;
end;

procedure TCustomDosCRT.Paint;
var
  X, Y, I : LongInt;
  CLX, CLY, CXX,
  SLX, SLY, SHX, SHY : LongInt;
begin
  inherited Paint;
  if not Assigned(FFont) then begin
    Canvas.Brush.Color:=FBackground;
    Canvas.FillRect(0,0,Width,Height);
    Exit;
  end;
  // Calculate what Screen characters need drawn.
  SLX := Canvas.ClipRect.TopLeft.X div (FFont.Width * FScale.X);
  SLY := Canvas.ClipRect.TopLeft.Y div (FFont.Height * FScale.Y);
  SHX := Canvas.ClipRect.BottomRight.X div (FFont.Width * FScale.X) + 1;
  SHY := Canvas.ClipRect.BottomRight.Y div (FFont.Height * FScale.Y) + 1;
  ValidateCoordinates(SLX, SLY);
  ValidateCoordinates(SHX, SHY);
  CXX := SLX * (FFont.Width * FScale.X);
  CLY := SLY * (FFont.Height * FScale.Y);
  for Y := SLY to SHY do begin
    CLX:=CXX;
    I := FResolution.Y * Y;
    for X := SLX to SHX do begin
      // PaintCharAttr(CLX, CLY, FScreen[I]);
      Inc(I);
      Inc(CLX);
    end;
    Inc(CLY);
  end;

end;

procedure TCustomDosCRT.PaintCharAttr(X, Y: LongInt; C: TDosCRTScreenItem);
begin
end;

procedure TCustomDosCRT.SetScreenBuffer;
var
  I, J : Integer;
begin
  J := 0;
  SetLength(FScreen, FResolution.X * FResolution.Y);
  for I := 0 to High(FScreen) do begin
      FScreen[I].Background:=FBackGround;
      FScreen[I].Foreground:=FForeGround;
      // FScreen[I].Character:=Byte(SPACE);
      FScreen[I].Character:=J;
      Inc(J);
      if J > 255 then J := 0;
  end;
end;

constructor TCustomDosCRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont:=nil;
  FScreen:=[];
  FForeground:=clGray;
  FBackGround:=clBlack;
  FTopLeft:=Point(0,0);
  FBottomRight:=Point(79,24);
  FScale:=Point(1,1);
  FResolution.X:=FBottomRight.X + 1;
  FResolution.Y:=FBottomRight.Y + 1;
  DoSizeChange;
end;

destructor TCustomDosCRT.Destroy;
begin
  inherited Destroy;
end;

{ TDosCRT }

function TDosCRT.GetWindMax: TPoint;
begin
  Result:=FBottomRight;
end;

function TDosCRT.GetWindMin: TPoint;
begin
  Result:=FTopLeft;
end;

procedure TDosCRT.SetWindMax(AValue: TPoint);
var
  A : TPoint;
begin
  ValidateCoordinates(AValue);
  A := FTopLeft;
  if AValue.X < A.X then Exchange(AValue.X, A.X);
  if AValue.Y < A.Y then Exchange(AValue.Y, A.Y);
  if (A.X = FTopLeft.X) and (A.Y = FTopLeft.Y) and (AValue.X = FBottomRight.X)
    and (AValue.Y = FBottomRight.Y) then Exit;
  FTopLeft:=A;
  FBottomRight:=AValue;
end;

procedure TDosCRT.SetWindWin(AValue: TPoint);
var
  B : TPoint;
begin
  ValidateCoordinates(AValue);
  B := FBottomRight;
  if AValue.X > B.X then Exchange(AValue.X, B.X);
  if AValue.Y > B.Y then Exchange(AValue.Y, B.Y);
  if (B.X = FBottomRight.X) and (B.Y = FBottomRight.Y) and
    (AValue.X = FTopLeft.X) and (AValue.Y = FTopLeft.Y) then Exit;
  FTopLeft:=AValue;
  FBottomRight:=B;
end;

procedure TDosCRT.ClrScr;
begin

end;

procedure TDosCRT.ClrEol;
begin

end;

procedure TDosCRT.InsLine;
begin

end;

procedure TDosCRT.DelLine;
begin

end;

initialization

finalization

end.

