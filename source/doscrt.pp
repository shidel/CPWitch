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

  { TCustomDosCRT }

  TCustomDosCRT = class ( TGraphicControl )
  private
    FFont: TCustomDosFont;
    FScale: TPoint;
    FResolution: TPoint;
    procedure SetFont(AValue: TCustomDosFont);
    procedure SetResolution(AValue: TPoint);
    procedure SetScale(AValue: TPoint);
  protected
    procedure AdjustSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Resolution : TPoint read FResolution write SetResolution;
    property Scale : TPoint read FScale write SetScale;
    property Font : TCustomDosFont read FFont write SetFont;
  published
  end;

  TDosCRT = class ( TCustomDosCRT )
  private
  protected
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
  published
  end;

implementation

{ TCustomDosCRT }

procedure TCustomDosCRT.SetResolution(AValue: TPoint);
begin
  if FResolution=AValue then Exit;
  FResolution:=AValue;
  AdjustSize;
end;

procedure TCustomDosCRT.SetFont(AValue: TCustomDosFont);
begin
  if FFont=AValue then Exit;
  FFont:=AValue;
  AdjustSize;
end;

procedure TCustomDosCRT.SetScale(AValue: TPoint);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  AdjustSize;
end;

procedure TCustomDosCRT.AdjustSize;
begin
  if Assigned(FFont) then begin
    Width:=FResolution.X * FFont.Width * FScale.X;
    Height:=FResolution.X * FFont.Height * FScale.Y;
  end else begin
    Width:=FResolution.X * 8 * FScale.X;
    Height:=FResolution.X * 8 * FScale.Y;
  end;
end;

constructor TCustomDosCRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScale:=Point(1,1);
  Resolution:=Point(80,25);
end;

destructor TCustomDosCRT.Destroy;
begin
  inherited Destroy;
end;

initialization

finalization

end.

