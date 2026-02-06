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

{ DEFINE OLDDRAW}


uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFNDEF OLDDRAW}
  IntfGraphics, FPCanvas, LCLType, LCLIntf, GraphType, FPImage,
  {$ENDIF}
  Classes, SysUtils, Controls, Graphics, PasExt, CodePages, DosFont;

type

  TDosCRTScreenItem = record
    Character : Byte;
    Foreground : TColor;
    BackGround : TColor;
  end;
  TDosCRTScreen = array of TDosCRTScreenItem;

  { TCustomDosCRT }

  TCustomDosCRT = class ( TGraphicControl )
  private
    FCodepage: integer;
    FControlCodes: boolean;
    FFont, FNoFont: TCustomDosFont;
    FInvalidBackground: TColor;
    FInvalidChar: Int32;
    FInvalidTextColor: TColor;
    FScale: TPoint;
    FResolution: TPoint;
    FScreen : TDosCRTScreen;
    FUpdate : integer;
    FWrapping: boolean;
    procedure SetCodepage(AValue: integer);
    procedure SetControlCodes(AValue: boolean);
    procedure SetFont(AValue: TCustomDosFont);
    procedure SetInvalidBackground(AValue: TColor);
    procedure SetInvalidChar(AValue: Int32);
    procedure SetInvalidTextColor(AValue: TColor);
    procedure SetResolution(AValue: TPoint);
    procedure SetScale(AValue: TPoint);
    procedure SetWindMin(AValue : TPoint);
    procedure SetWindMax(AValue : TPoint);
    procedure SetWrapping(AValue: boolean);
  protected
    FPosition : TPoint;
    FTopLeft : TPoint;
    FBottomRight : TPoint;
    FForeground : TColor;
    FBackground : TColor;
    function CurrentFont : TCustomDosFont;
    procedure ValidateCoordinates(var P : TPoint); overload;
    procedure ValidateCoordinates(var X, Y : LongInt); overload;
    procedure DoSizeChange; virtual;
    procedure Paint; override;
    {$IFNDEF OLDDRAW}
    procedure Render(IntfImg: TLazIntfImage);
    {$ELSE}
    procedure PaintCharAttr(X, Y : LongInt; C : TDosCRTScreenItem);
    {$ENDIF}
    procedure SetScreenBuffer;
    procedure ScrollUp(AFromPosition : boolean = false);
    procedure ScrollDown(AFromPosition : boolean = false);
    procedure ScrollLeft(AFromPosition : boolean = false);
    procedure ScrollRight(AFromPosition : boolean = false);
    procedure NextLine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property Resolution : TPoint read FResolution write SetResolution;
    property Scale : TPoint read FScale write SetScale;
    property Font : TCustomDosFont read FFont write SetFont;
    property Screen : TDosCRTScreen read FScreen write FScreen;
    property ControlCodes : boolean read FControlCodes write SetControlCodes;
    property Codepage : integer read FCodepage write SetCodepage;
    property InvalidChar : Int32 read FInvalidChar write SetInvalidChar;
    property InvalidTextColor : TColor read FInvalidTextColor write SetInvalidTextColor;
    property InvalidBackground : TColor read FInvalidBackground write SetInvalidBackground;
    property Wrapping : boolean read FWrapping write SetWrapping;
    procedure WriteCRT(const S : RawByteString); overload;
  published
  end;

  { TDosCRT }

  TDosCRT = class ( TCustomDosCRT )
  private
  protected
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    { CRT Style Stuff }
    // property TextAttr : byte;
    // property WindMin : TPoint read GetWindMin write SetWindWin;
    // property WindMax : TPoint read GetWindMax write SetWindMax;
    // procedure Window(X1, Y1, X2, Y2 : byte);
    procedure GotoXY(X, Y : LongInt); overload;
    procedure GotoXY(XY : TPoint); overload;
    //function WhereX: byte;
    //function WhereY: byte;
    procedure ClrScr;
    procedure ClrEol;
    procedure TextColor(AValue : TColor);
    procedure TextBackground(AValue : TColor);
  published
  end;

  TLittleDosCRT = class ( TCustomDosCRT )
  private
  protected
  public
  //  property CheckBreak : boolean;
  //  property CheckEOF : boolean;
  //  property DirectVideo : boolean;
  //  property CheckSnow : boolean;
  //  property LastMode : word;
  //  property TextAttr : byte;
  //  property WindMin : word;
  //  property WindMax : word;
  //  procedure AssignCrt(var F : text);
  //  function KeyPressed : boolean;
  //  function ReadKey : char;
  //  procedure TextMode(Mode : word);
  //  procedure Window(X1, Y1, X2, Y2 : byte);
  //  procedure GotoXY(X, Y : byte);
  //  function WhereX: byte;
  //  function WhereY: byte;
  //  procedure ClrScr;
  //  procedure ClrEol;
  //  procedure InsLine;
  //  procedure DelLine;
  //  procedure TextColor(Color : byte);
  //  procedure TextBackground(Color : byte);
  //  procedure LowVideo;
  //  procedure HighVideo;
  //  procedure NormVideo;
  //  procedure Delay(MS : word);
  //  procedure Sound(Hz : word);
  // procedure NoSound;
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

procedure TCustomDosCRT.SetInvalidBackground(AValue: TColor);
begin
  if FInvalidBackground=AValue then Exit;
  FInvalidBackground:=AValue;
end;

procedure TCustomDosCRT.SetInvalidChar(AValue: Int32);
begin
  if FInvalidChar=AValue then Exit;
  FInvalidChar:=AValue;
end;

procedure TCustomDosCRT.SetInvalidTextColor(AValue: TColor);
begin
  if FInvalidTextColor=AValue then Exit;
  FInvalidTextColor:=AValue;
end;

procedure TCustomDosCRT.SetControlCodes(AValue: boolean);
begin
  if FControlCodes=AValue then Exit;
  FControlCodes:=AValue;
end;

procedure TCustomDosCRT.SetCodepage(AValue: integer);
begin
  if FCodepage=AValue then Exit;
  FCodepage:=AValue;
end;

procedure TCustomDosCRT.SetScale(AValue: TPoint);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  DoSizeChange;
end;

procedure TCustomDosCRT.SetWindMin(AValue: TPoint);
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

procedure TCustomDosCRT.SetWindMax(AValue: TPoint);
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

procedure TCustomDosCRT.SetWrapping(AValue: boolean);
begin
  if FWrapping=AValue then Exit;
  FWrapping:=AValue;
end;

function TCustomDosCRT.CurrentFont: TCustomDosFont;
begin
  if Assigned(FFont) and (FFont.Width > 0) then
    Result:=FFont
  else
    Result:=FNoFont;
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
var
   W, H : Integer;
begin
  W:=FResolution.X * CurrentFont.Width * FScale.X;
  H:=FResolution.Y * CurrentFont.Height * FScale.Y;
  { TODO 1 -cDevel Add support for BorderSpacing to TCustomDosCRT }
  // Inc(W,BorderSpacing.Inner * 2);
  // Inc(H,BorderSpacing.Inner * 2);
  Width:=W;
  Height:=H;
  SetScreenBuffer;
  Invalidate;
end;

{$IFNDEF OLDDRAW}
procedure TCustomDosCRT.Paint;
var
  I: TLazIntfImage;
  B : TBitmap;
begin
  if FUpdate > 0 then Exit;
  B := TBitmap.Create;
  try
    B.SetSize(Width, Height);
    I := TLazIntfImage.Create(0, 0);
    try
      I.LoadFromBitmap(B.Handle, B.MaskHandle);
      Render(I);
      B.LoadFromIntfImage(I);
      Canvas.Draw(0, 0, B);
    finally
      I.Free;
    end;
  finally
    B.Free;
  end;
end;

function ColorToFP(C: TColor): TFPColor;
var
  RGB: LongInt;
  R, G, B: Word;
begin
  RGB := ColorToRGB(C);
  R := RGB and $FF;
  G := (RGB shr 8) and $FF;
  B := (RGB shr 16) and $FF;
  Result.Red   := (R shl 8) or R;  // Bit math version of R * 257
  Result.Green := (G shl 8) or G;
  Result.Blue  := (B shl 8) or B;
  Result.Alpha := $FFFF;           // Fully opaque
end;

procedure TCustomDosCRT.Render(IntfImg: TLazIntfImage);
var
  F : TCustomDosFont;
  X, Y, XX, YY, SX, SY: Integer;
  SLX, SLY, SHX, SHY : LongInt;
  BitMask: TArrayOfByte;
  FG, BG, DrawColor: TFPColor;
  ScreenItem: TDosCRTScreenItem;
  CW, CH: Integer;
  BaseX, BaseY, PixelY, PixelX: Integer;
  B, I: Integer;
begin
  F:=CurrentFont;
  CW := F.Width * FScale.X;
  CH := F.Height * FScale.Y;

  // Calculate what Screen characters need drawn.
  SLX := Canvas.ClipRect.TopLeft.X div (F.Width * FScale.X);
  SLY := Canvas.ClipRect.TopLeft.Y div (F.Height * FScale.Y);
  SHX := Canvas.ClipRect.BottomRight.X div (F.Width * FScale.X) + 1;
  SHY := Canvas.ClipRect.BottomRight.Y div (F.Height * FScale.Y) + 1;
  ValidateCoordinates(SLX, SLY);
  ValidateCoordinates(SHX, SHY);
  for Y := SLY to SHY do begin
    BaseY := Y * CH;
    for X := SLX to SHX do begin
      BaseX := X * CW;

      ScreenItem := FScreen[FResolution.X * Y + X];
      BitMask := F.BitMask[ScreenItem.Character];
      if Length(BitMask) = 0 then Continue;

      FG := ColorToFP(ScreenItem.Foreground);
      BG := ColorToFP(ScreenItem.Background);

      I := 0;
      for YY := 0 to F.Height - 1 do begin
        B := 0;
        PixelY := BaseY + (YY * FScale.Y);
        for XX := 0 to F.Width - 1 do begin
          if (BitMask[I] and (1 shl (7 - B))) <> 0 then
            DrawColor := FG
          else
            DrawColor := BG;
          PixelX := BaseX + (XX * FScale.X);
          for SY := 0 to FScale.Y - 1 do
            for SX := 0 to FScale.X - 1 do
              IntfImg.Colors[PixelX + SX, PixelY + SY] := DrawColor;
          Inc(B);
          if B > 7 then begin
            B := 0;
            Inc(I);
          end;
        end;
        if B <> 0 then Inc(I);
      end;
    end;
  end;
end;

{$ELSE}

procedure TCustomDosCRT.Paint;
var
  F : TCustomDosFont;
  X, Y, I : LongInt;
  CLX, CLY, CXX,
  SLX, SLY, SHX, SHY : LongInt;
begin
  if FUpdate > 0 then Exit;
  F:=CurrentFont;
  // Calculate what Screen characters need drawn.
  SLX := Canvas.ClipRect.TopLeft.X div (F.Width * FScale.X);
  SLY := Canvas.ClipRect.TopLeft.Y div (F.Height * FScale.Y);
  SHX := Canvas.ClipRect.BottomRight.X div (F.Width * FScale.X) + 1;
  SHY := Canvas.ClipRect.BottomRight.Y div (F.Height * FScale.Y) + 1;
  ValidateCoordinates(SLX, SLY);
  ValidateCoordinates(SHX, SHY);
  CXX := SLX * (F.Width * FScale.X);
  CLY := SLY * (F.Height * FScale.Y);
  for Y := SLY to SHY do begin
    CLX:=CXX;
    I := FResolution.X * Y + SLX;
    for X := SLX to SHX do begin
      PaintCharAttr(CLX, CLY, FScreen[I]);
      Inc(I);
      Inc(CLX, (F.Width * FScale.X));
    end;
    Inc(CLY, (F.Height * FScale.Y));
  end;
end;

procedure TCustomDosCRT.PaintCharAttr(X, Y: LongInt; C: TDosCRTScreenItem);
var
  F : TCustomDosFont;
  D : TArrayOfBoolean;
  I, PX, PY, XX, YY : LongInt;
begin
  F:=CurrentFont;
  D:=F.Pixels[C.Character];
  Canvas.Brush.Color:=C.BackGround;
  Canvas.FillRect(X, Y, X + F.Width * FScale.X, Y + F.Height * FScale.Y);
  Canvas.Brush.Color:=C.ForeGround;
  I:=0;
  for PY := 0 to F.Height - 1 do begin
    YY := Y + PY * FScale.Y;
    for PX := 0 to F.Width - 1 do begin
      XX := X + PX * FScale.X;
      if D[I] then
        Canvas.FillRect(XX, YY, XX + FScale.X, YY + FScale.Y);
      Inc(I);
    end;
  end;
end;
{$ENDIF}

procedure TCustomDosCRT.SetScreenBuffer;
var
  I : Integer;
begin
  FPosition:=Point(0,0);
  FTopLeft:=Point(0,0);
  FBottomRight:=Point(FResolution.X -1, FResolution.Y-1);
  SetLength(FScreen, FResolution.X * FResolution.Y);
  for I := 0 to High(FScreen) do begin
      FScreen[I].Background:=FBackGround;
      FScreen[I].Foreground:=FForeGround;
      FScreen[I].Character:=Byte(SPACE);
  end;
end;

procedure TCustomDosCRT.ScrollUp(AFromPosition: boolean);
var
  X, Y, YY : LongInt;
  Org : TPoint;
begin
  BeginUpdate;
  Org:=FTopLeft;
  if AFromPosition then
    Inc(Org.Y,FPosition.Y);
  for Y := Org.Y to FBottomRight.Y - 1 do begin
    YY:=Y * FResolution.Y;
    for X := Org.X to FBottomRight.X do
      FScreen[YY + X] := FScreen[YY + FResolution.Y + X];
  end;
  Y:=FBottomRight.Y * FResolution.Y;
  for X := Org.X to FBottomRight.X do begin
    FScreen[YY+X].Character := Byte(SPACE);
    FScreen[YY+X].Foreground := FForeground;
    FScreen[YY+X].BackGround := FBackground;
  end;
  EndUpdate;
end;

procedure TCustomDosCRT.ScrollDown(AFromPosition: boolean);
var
  X, Y, YY : LongInt;
  Org : TPoint;
begin
  BeginUpdate;
  Org:=FTopLeft;
  if AFromPosition then
    Inc(Org.Y,FPosition.Y);
  for Y := FBottomRight.Y - 1 downto Org.Y do begin
    YY:=Y * FResolution.Y;
    for X := Org.X to FBottomRight.X do
      FScreen[YY + X + FResolution.Y] := FScreen[YY + X];
  end;
  Y:=FTopLeft.Y * FResolution.Y;
  for X := Org.X to FBottomRight.X do begin
    FScreen[YY+X].Character := Byte(SPACE);
    FScreen[YY+X].Foreground := FForeground;
    FScreen[YY+X].BackGround := FBackground;
  end;
  EndUpdate;
end;

procedure TCustomDosCRT.ScrollLeft(AFromPosition: boolean);
var
  X, Y : LongInt;
  Org : TPoint;
begin
  BeginUpdate;
  Org:=FTopLeft;
  if AFromPosition then
    Inc(Org.X,FPosition.X);
  for X := Org.X to FBottomRight.X - 1 do
    for Y := Org.X to FBottomRight.X do
      FScreen[Y * FResolution.Y + X] := FScreen[Y * FResolution.Y + X + 1];
  for Y := Org.Y to FBottomRight.Y do begin
    X:=Y * FResolution.Y + FBOttomRight.X;
    FScreen[X].Character := Byte(SPACE);
    FScreen[X].Foreground := FForeground;
    FScreen[X].BackGround := FBackground;
  end;
  EndUpdate;
end;

procedure TCustomDosCRT.ScrollRight(AFromPosition: boolean);
var
  X, Y : LongInt;
  Org : TPoint;
begin
  BeginUpdate;
  Org:=FTopLeft;
  if AFromPosition then
    Inc(Org.X,FPosition.X);
  for X := FBottomRight.X - 1 downto Org.X do
    for Y := Org.X to FBottomRight.X do
      FScreen[Y * FResolution.Y + X + 1] := FScreen[Y * FResolution.Y + X];
  for Y := Org.Y to FBottomRight.Y do begin
    X:=Y * FResolution.Y + FTopLeft.X;
    FScreen[X].Character := Byte(SPACE);
    FScreen[X].Foreground := FForeground;
    FScreen[X].BackGround := FBackground;
  end;
  EndUpdate;
end;

procedure TCustomDosCRT.NextLine;
begin
  Inc(FPosition.Y);
  if (FTopLeft.Y + FPosition.Y) > FBottomRight.Y then begin
    if Wrapping then begin
      Dec(FPosition.Y);
      ScrollUp(False);
    end else begin
      FPosition.Y:=0;
    end;
  end;
end;

constructor TCustomDosCRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FUpdate:=0;
  FControlCodes:=True;
  FWrapping:=True;
  FFont:=nil;
  FNoFont:=TBitmapDosFont.Create;
  FScreen:=[];
  FForeground:=clGray;
  FBackGround:=clBlack;
  FScale:=Point(1,1);
  FResolution:=Point(80,25);
  FInvalidChar:=$3f; { or $bf }
  FCodePage:=-1;
  FInvalidTextColor:=clRed;
  FInvalidBackground:=clBlack;
  DoSizeChange;
end;

destructor TCustomDosCRT.Destroy;
begin
  FreeAndNil(FNoFont);
  inherited Destroy;
end;

procedure TCustomDosCRT.BeginUpdate;
begin
  Inc(FUpdate);
end;

procedure TCustomDosCRT.EndUpdate;
begin
  Dec(FUpdate);
  if FUpdate <= 0 then Invalidate;
end;

procedure TCustomDosCRT.WriteCRT(const S: RawByteString);
var
  I : Integer;
  P : LongInt;
begin
  BeginUpdate;
  for I := 1 to Length(S) do begin
  if FControlCodes and (S[I] < #32) or (S[I]=#127) then begin
      { propcess or ignore character control codes }
      case S[I] of
        CR  : FPosition.X := 0;
        LF  : NextLine;
        TAB : begin
          P := (8 - (FPosition.X + 1) mod 8);
          WriteCRT(StringOf(SPACE, P));
        end;
        { TODO 7 -cDevel Add support for BACKSPACE character in DosCRT }
        BACKSPACE  : begin end;
        { TODO 7 -cDevel Add support for DELETE character in DosCRT }
        DELETECHAR : begin end;
      end;
      Continue;
    end;
    P := (FTopLeft.Y + FPosition.Y) * FResolution.X + (FTopLeft.X + FPosition.X);
    FScreen[P].Character:=Byte(S[I]);
    FScreen[P].Foreground := FForeground;
    FScreen[P].BackGround := FBackground;
    Inc(FPosition.X);
    if (FTopLeft.X + FPosition.X) > FBottomRight.X then begin
      FPosition.X:=0;
      NextLine;
    end;
  end;
  EndUpdate;
end;

{ TDosCRT }

procedure TDosCRT.GotoXY(X, Y: LongInt);
begin
  Dec(X);
  Dec(Y);
  if X < 0 then X := 0;
  if Y < 0 then Y := 0;
  if FTopLeft.X + X > FBottomRight.X then X := FBottomRight.X - FTopLeft.X;
  if FTopLeft.Y + Y > FBottomRight.Y then Y := FBottomRight.Y - FTopLeft.Y;
  FPosition:=Point(X, Y);
end;

procedure TDosCRT.GotoXY(XY: TPoint);
begin
  GotoXY(XY.X, XY.Y);
end;

procedure TDosCRT.ClrScr;
var
  X, Y, P : Integer;
begin
  BeginUpdate;
  FPosition:=Point(0,0);
  for Y := FTopLeft.Y to FBottomRight.Y do
    for X := FTopLeft.X to FBottomRight.X do
      begin
        P:=Y * FResolution.X + X;
        FScreen[P].Background:=FBackGround;
        FScreen[P].Foreground:=FForeGround;
        FScreen[P].Character:=Byte(SPACE);
      end;
  EndUpdate;
end;

procedure TDosCRT.ClrEol;
var
  X, P : Integer;
begin
  BeginUpdate;
  for X := FTopLeft.X to FBottomRight.X do
    begin
      P:=FPosition.Y * FResolution.X + X;
      FScreen[P].Background:=FBackGround;
      FScreen[P].Foreground:=FForeGround;
      FScreen[P].Character:=Byte(SPACE);
    end;
  EndUpdate;
end;

procedure TDosCRT.TextColor(AValue: TColor);
begin
  FForeground:=AValue;
end;

procedure TDosCRT.TextBackground(AValue: TColor);
begin
  FBackground:=AValue;
end;

initialization

finalization

end.

