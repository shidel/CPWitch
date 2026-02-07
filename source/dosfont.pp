{
   Copyright (c) 2025-2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.
}

unit DosFont;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, PasExt, BinTree, Codepages;

type

  { TCustomDosFont }

  TCustomDosFont = class
  private
    FHeight: integer;
    FWidth: integer;
    function GetPixels(Index : integer): TArrayOfBoolean;
  protected
    FData : TArrayOfByte;
    function GetBitMask(Index : integer): TArrayOfByte; virtual; abstract;
  public
    constructor Create; virtual;
    function LoadFromFile(FileName : String) : integer; virtual; abstract;
    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property BitMask[Index : integer] : TArrayOfByte read GetBitMask;
    property Pixels[Index : integer] : TArrayOfBoolean read GetPixels;
  published
  end;

  { TBitmapDosFont }

  { TODO 1 -cDevel Add Modification and Saving of a Dos Bitmap Font file. }

  TBitmapDosFont = class ( TCustomDosFont )
  private
  protected
    procedure Reset;
    function GetBitMask(Index : integer): TArrayOfByte; override;
  public
    constructor Create; override;
    function LoadFromFile(FileName : String) : integer; override;
  published
  end;

  { TUnicodeDosFont }

  { TODO 1 -cDevel Port Modification and Saving of a (UFF) DOS Unicode Font
    file from the Danger Engine Resource Editor. }

  TUnicodeDosFont = class ( TCustomDosFont )
  private
    FChars : TBinaryTree;
    FBaseLine: integer;
    FBoldness: Int8;
    FComment: String;
    FDisplacement: integer;
    FFontName: String;
    FInvalidChar: Int32;
    FItalics: Int8;
    FProportional: Boolean;
    FStrikeOut: integer;
    FTitle: String;
    FUnderLine: integer;
    FVersion: word;
    function GetCount: integer;
    function GetPadded(Index : integer): boolean;
    procedure Reset;
    procedure SetInvalidChar(AValue: Int32);
  protected
    procedure ReadData(var Position : integer; out V; C : Word);
    function ReadASCIIz(var Position : integer) : RawByteString;
    function ReadBytes(var Position : integer; C : word) : TArrayOfByte;
    function ProcessHeader : integer;
    function DecodeChars(var Position: integer; Current, Last: Int32; Expected: word): boolean;
    function DecodeChar(CData, CMask: TArrayOfByte): TArrayOfByte;
    procedure AddCharacter(Index : Int64; CData : TArrayOfByte; AValue: boolean);
    function GetBitMask(Index : integer): TArrayOfByte; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function LoadFromFile(FileName : String) : integer; override;
    property InvalidChar : Int32 read FInvalidChar write SetInvalidChar;
    property Count : integer read GetCount;
    property Version : word read FVersion;
    property Proportional : Boolean read FProportional;
    property Boldness : Int8 read FBoldness;
    property Italics : Int8 read FItalics;
    property FontName : String read FFontName;
    property Title : String read FTitle;
    property Comment : String read FComment;
    property BaseLine : integer read FBaseLine;
    property UnderLine : integer read FUnderLine;
    property StrikeOut : integer read FStrikeOut;
    property Displacement : integer read FDisplacement;
    property Padded[Index : integer] : boolean read GetPadded;
  published
  end;

implementation

function GetDefaultBitFont : TArrayOfByte;
begin
  Result:=[
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$7e,$81,$a5,$81,$81,$bd,$99,$81,$81,$7e,$00,$00,$00,$00,
    $00,$00,$7e,$ff,$db,$ff,$ff,$c3,$e7,$ff,$ff,$7e,$00,$00,$00,$00,
    $00,$00,$00,$00,$6c,$fe,$fe,$fe,$7c,$38,$10,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$10,$38,$7c,$fe,$7c,$38,$10,$00,$00,$00,$00,$00,
    $00,$00,$00,$18,$3c,$3c,$db,$ff,$db,$18,$18,$3c,$00,$00,$00,$00,
    $00,$00,$00,$18,$3c,$7e,$ff,$ff,$5a,$18,$18,$3c,$00,$00,$00,$00,
    $fc,$c6,$fc,$c6,$fc,$00,$fe,$c0,$f8,$c0,$fe,$00,$c0,$c0,$c0,$fe,
    $ff,$ff,$ff,$ff,$ff,$81,$bd,$bd,$bd,$bd,$81,$ff,$ff,$ff,$ff,$ff,
    $00,$00,$00,$00,$00,$7e,$42,$42,$42,$42,$7e,$00,$00,$00,$00,$00,
    $c0,$c0,$c0,$c0,$c0,$fe,$00,$00,$fe,$c0,$c0,$f8,$c0,$c0,$c0,$00,
    $00,$00,$1e,$0e,$1a,$30,$78,$cc,$cc,$cc,$cc,$78,$00,$00,$00,$00,
    $00,$00,$78,$cc,$cc,$cc,$cc,$78,$30,$fc,$30,$30,$00,$00,$00,$00,
    $ff,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$ff,
    $00,$00,$3f,$63,$63,$7f,$63,$63,$63,$67,$e6,$c0,$00,$00,$00,$00,
    $00,$00,$00,$18,$db,$db,$3c,$e7,$3c,$db,$db,$18,$00,$00,$00,$00,
    $00,$80,$c0,$e0,$f0,$f8,$fe,$f8,$f0,$e0,$c0,$80,$00,$00,$00,$00,
    $00,$02,$06,$0e,$1e,$3e,$fe,$3e,$1e,$0e,$06,$02,$00,$00,$00,$00,
    $00,$00,$30,$78,$fc,$30,$30,$30,$fc,$78,$30,$00,$00,$00,$00,$00,
    $00,$00,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$00,$cc,$cc,$00,$00,$00,$00,
    $00,$00,$7f,$db,$db,$db,$db,$7b,$1b,$1b,$1b,$1b,$00,$00,$00,$00,
    $00,$3c,$66,$30,$7c,$c6,$c6,$c6,$7c,$18,$cc,$78,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$fe,$fe,$fe,$fe,$00,$00,$00,
    $00,$00,$30,$78,$fc,$30,$30,$30,$fc,$78,$30,$fc,$00,$00,$00,$00,
    $00,$00,$30,$78,$fc,$30,$30,$30,$30,$30,$30,$30,$00,$00,$00,$00,
    $00,$00,$30,$30,$30,$30,$30,$30,$30,$fc,$78,$30,$00,$00,$00,$00,
    $00,$00,$00,$00,$30,$18,$0c,$fe,$0c,$18,$30,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$18,$30,$60,$fe,$60,$30,$18,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$c0,$c0,$c0,$fe,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$24,$42,$ff,$42,$24,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$18,$18,$3c,$3c,$7e,$7e,$ff,$ff,$00,$00,$00,$00,$00,
    $00,$00,$00,$ff,$ff,$7e,$7e,$3c,$3c,$18,$18,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$38,$7c,$7c,$7c,$7c,$38,$38,$38,$38,$00,$00,$38,$38,$00,$00,
    $00,$cc,$cc,$cc,$cc,$48,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$36,$6c,$6c,$6c,$fe,$6c,$6c,$fe,$6c,$6c,$6c,$d8,$00,$00,
    $00,$18,$18,$7e,$c6,$c0,$c0,$7c,$06,$06,$c6,$fc,$30,$30,$00,$00,
    $00,$c6,$c6,$0c,$0c,$18,$18,$30,$30,$60,$60,$c6,$c6,$00,$00,$00,
    $00,$38,$6c,$6c,$6c,$38,$70,$de,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,
    $00,$18,$18,$18,$18,$30,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$1c,$30,$30,$60,$60,$60,$60,$60,$60,$60,$30,$30,$1c,$00,$00,
    $00,$60,$30,$30,$18,$18,$18,$18,$18,$18,$18,$30,$30,$60,$00,$00,
    $00,$00,$00,$00,$66,$66,$3c,$ff,$3c,$66,$66,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$18,$18,$18,$7e,$18,$18,$18,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$30,$30,$30,$60,$00,
    $00,$00,$00,$00,$00,$00,$00,$7e,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$30,$30,$00,$00,
    $00,$03,$03,$06,$06,$0c,$0c,$18,$18,$30,$30,$60,$60,$c0,$c0,$00,
    $00,$7c,$c6,$c6,$ce,$ce,$de,$d6,$f6,$e6,$e6,$c6,$c6,$7c,$00,$00,
    $00,$30,$70,$f0,$30,$30,$30,$30,$30,$30,$30,$30,$30,$fc,$00,$00,
    $00,$7c,$c6,$c6,$06,$06,$0c,$18,$30,$60,$c0,$c0,$c6,$fe,$00,$00,
    $00,$7c,$c6,$c6,$06,$06,$1c,$06,$06,$06,$06,$c6,$c6,$7c,$00,$00,
    $00,$0c,$0c,$cc,$cc,$cc,$cc,$cc,$fe,$0c,$0c,$0c,$0c,$0c,$00,$00,
    $00,$fe,$c0,$c0,$c0,$c0,$fc,$06,$06,$06,$06,$c6,$c6,$7c,$00,$00,
    $00,$7c,$c6,$c6,$c0,$c0,$fc,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$fe,$c6,$c6,$06,$06,$0c,$18,$30,$30,$30,$30,$30,$30,$00,$00,
    $00,$7c,$c6,$c6,$c6,$c6,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$7c,$c6,$c6,$c6,$c6,$c6,$7e,$06,$06,$06,$06,$c6,$7c,$00,$00,
    $00,$00,$00,$00,$30,$30,$30,$00,$00,$00,$30,$30,$30,$00,$00,$00,
    $00,$00,$00,$00,$30,$30,$30,$00,$00,$00,$30,$30,$30,$30,$60,$00,
    $00,$00,$06,$0c,$18,$30,$60,$c0,$c0,$60,$30,$18,$0c,$06,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$00,$00,$fe,$00,$00,$00,$00,$00,$00,
    $00,$00,$c0,$60,$30,$18,$0c,$06,$06,$0c,$18,$30,$60,$c0,$00,$00,
    $00,$7c,$c6,$c6,$06,$0c,$18,$18,$18,$18,$00,$00,$18,$18,$00,$00,
    $00,$7c,$c6,$c6,$de,$de,$f6,$f6,$f6,$de,$dc,$c0,$c0,$7e,$00,$00,
    $00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$fe,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$fc,$c6,$c6,$c6,$c6,$fc,$c6,$c6,$c6,$c6,$c6,$c6,$fc,$00,$00,
    $00,$3c,$66,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$66,$3c,$00,$00,
    $00,$f8,$cc,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$cc,$f8,$00,$00,
    $00,$fe,$c0,$c0,$c0,$c0,$f8,$c0,$c0,$c0,$c0,$c0,$c0,$fe,$00,$00,
    $00,$fe,$c0,$c0,$c0,$c0,$f8,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$00,$00,
    $00,$3c,$66,$c0,$c0,$c0,$c0,$c0,$de,$c6,$c6,$c6,$66,$3e,$00,$00,
    $00,$c6,$c6,$c6,$c6,$c6,$c6,$fe,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$fc,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$fc,$00,$00,
    $00,$06,$06,$06,$06,$06,$06,$06,$06,$06,$c6,$c6,$c6,$7c,$00,$00,
    $00,$c6,$c6,$c6,$cc,$cc,$f8,$f8,$cc,$cc,$c6,$c6,$c6,$c6,$00,$00,
    $00,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$fe,$00,$00,
    $00,$c6,$c6,$ee,$fe,$fe,$d6,$d6,$d6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$c6,$c6,$c6,$e6,$f6,$fe,$fe,$de,$ce,$c6,$c6,$c6,$c6,$00,$00,
    $00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$fc,$c6,$c6,$c6,$c6,$c6,$c6,$fc,$c0,$c0,$c0,$c0,$c0,$00,$00,
    $00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$d6,$d6,$de,$7c,$0e,$00,
    $00,$fc,$c6,$c6,$c6,$c6,$c6,$fc,$d8,$cc,$c6,$c6,$c6,$c6,$00,$00,
    $00,$7c,$c6,$c0,$c0,$60,$38,$0c,$06,$06,$06,$c6,$c6,$7c,$00,$00,
    $00,$fc,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$00,$00,
    $00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$6c,$38,$10,$00,$00,
    $00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$d6,$d6,$fe,$fe,$ee,$c6,$00,$00,
    $00,$c6,$c6,$c6,$c6,$6c,$38,$38,$6c,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$cc,$cc,$cc,$cc,$cc,$78,$30,$30,$30,$30,$30,$30,$30,$00,$00,
    $00,$fe,$c6,$86,$0c,$0c,$18,$30,$30,$60,$60,$c2,$c6,$fe,$00,$00,
    $00,$7c,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$7c,$00,$00,
    $00,$c0,$c0,$60,$60,$30,$30,$18,$18,$0c,$0c,$06,$06,$03,$03,$00,
    $00,$7c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$7c,$00,$00,
    $00,$10,$38,$6c,$c6,$c6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,
    $00,$30,$30,$30,$30,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$c0,$c0,$c0,$f8,$cc,$c6,$c6,$c6,$c6,$c6,$e6,$bc,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c0,$c0,$c0,$c0,$c0,$c6,$7c,$00,$00,
    $00,$00,$06,$06,$06,$3e,$66,$c6,$c6,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c6,$c6,$fc,$c0,$c0,$c2,$7e,$00,$00,
    $00,$00,$7c,$c6,$c0,$c0,$c0,$f8,$c0,$c0,$c0,$c0,$c0,$c0,$00,$00,
    $00,$00,$00,$00,$00,$7a,$ce,$c6,$c6,$c6,$c6,$c6,$7e,$06,$c6,$7c,
    $00,$00,$c0,$c0,$c0,$dc,$f6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$00,$18,$18,$00,$18,$18,$18,$18,$18,$18,$18,$18,$1c,$00,$00,
    $00,$00,$06,$06,$00,$06,$06,$06,$06,$06,$06,$06,$06,$c6,$c6,$7c,
    $00,$00,$c0,$c0,$c0,$c0,$c6,$cc,$d8,$f0,$f0,$d8,$cc,$c6,$00,$00,
    $00,$00,$38,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$1c,$00,$00,
    $00,$00,$00,$00,$00,$44,$ee,$fe,$d6,$d6,$d6,$c6,$c6,$c6,$00,$00,
    $00,$00,$00,$00,$00,$bc,$e6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$00,$00,$00,$00,$bc,$c6,$c6,$c6,$c6,$c6,$c6,$fc,$c0,$c0,$c0,
    $00,$00,$00,$00,$00,$7a,$ce,$c6,$c6,$c6,$c6,$ce,$7e,$06,$06,$06,
    $00,$00,$00,$00,$00,$bc,$e6,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c0,$70,$38,$0c,$06,$c6,$7c,$00,$00,
    $00,$00,$20,$60,$60,$f8,$60,$60,$60,$60,$60,$60,$66,$3c,$00,$00,
    $00,$00,$00,$00,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$00,$00,$00,$c6,$c6,$c6,$c6,$c6,$c6,$6c,$38,$10,$00,$00,
    $00,$00,$00,$00,$00,$c6,$c6,$c6,$c6,$d6,$d6,$fe,$fe,$6c,$00,$00,
    $00,$00,$00,$00,$00,$82,$c6,$6c,$38,$38,$38,$6c,$c6,$82,$00,$00,
    $00,$00,$00,$00,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7e,$06,$c6,$7c,
    $00,$00,$00,$00,$00,$fe,$86,$0c,$18,$30,$60,$c2,$c2,$fe,$00,$00,
    $00,$1c,$30,$30,$30,$30,$30,$e0,$30,$30,$30,$30,$30,$1c,$00,$00,
    $00,$30,$30,$30,$30,$30,$30,$00,$30,$30,$30,$30,$30,$30,$00,$00,
    $00,$e0,$30,$30,$30,$30,$30,$1c,$30,$30,$30,$30,$30,$e0,$00,$00,
    $00,$00,$00,$76,$dc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$10,$38,$6c,$c6,$c6,$c6,$c6,$c6,$c6,$fe,$00,$00,$00,
    $00,$00,$3c,$66,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$66,$3c,$0c,$66,$3c,
    $00,$00,$cc,$00,$00,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,
    $00,$18,$30,$60,$00,$7c,$c6,$c6,$c6,$fc,$c0,$c0,$c2,$7e,$00,$00,
    $10,$38,$6c,$c6,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$c6,$00,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$30,$18,$0c,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $3c,$66,$3c,$00,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c0,$c0,$c0,$c6,$7c,$0c,$66,$3c,$00,
    $10,$38,$6c,$c6,$00,$7c,$c6,$c6,$c6,$fc,$c0,$c0,$c2,$7e,$00,$00,
    $00,$00,$c6,$00,$00,$7c,$c6,$c6,$c6,$fc,$c0,$c0,$c2,$7e,$00,$00,
    $00,$60,$30,$18,$00,$7c,$c6,$c6,$c6,$fc,$c0,$c0,$c2,$7e,$00,$00,
    $00,$00,$cc,$00,$00,$30,$30,$30,$30,$30,$30,$30,$30,$38,$00,$00,
    $10,$38,$6c,$c6,$00,$30,$30,$30,$30,$30,$30,$30,$30,$38,$00,$00,
    $00,$c0,$60,$30,$00,$30,$30,$30,$30,$30,$30,$30,$30,$38,$00,$00,
    $00,$00,$c6,$00,$38,$6c,$c6,$c6,$c6,$c6,$fe,$c6,$c6,$c6,$00,$00,
    $38,$6c,$38,$00,$38,$6c,$c6,$c6,$c6,$c6,$fe,$c6,$c6,$c6,$00,$00,
    $0c,$18,$30,$00,$fe,$c0,$c0,$c0,$f8,$c0,$c0,$c0,$c0,$fe,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$6e,$ba,$1a,$7e,$d8,$da,$6e,$00,$00,
    $00,$00,$00,$00,$3e,$6c,$cc,$cc,$ce,$fc,$cc,$cc,$cc,$ce,$00,$00,
    $10,$38,$6c,$c6,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$00,$c6,$00,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$c0,$60,$30,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $10,$38,$6c,$c6,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$60,$30,$18,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$c6,$00,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7e,$06,$c6,$7c,
    $00,$c6,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$c6,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$18,$18,$7c,$c6,$c0,$c0,$c0,$c0,$c0,$c6,$7c,$18,$18,$00,$00,
    $00,$38,$6c,$60,$60,$60,$f0,$60,$60,$60,$60,$60,$e6,$fe,$00,$00,
    $00,$00,$00,$cc,$cc,$78,$30,$fc,$30,$30,$fc,$30,$30,$30,$00,$00,
    $00,$f8,$cc,$cc,$cc,$f8,$c0,$cc,$cc,$de,$cc,$cc,$cc,$c6,$00,$00,
    $0e,$1b,$1b,$18,$18,$18,$7e,$18,$18,$18,$18,$18,$d8,$f8,$70,$00,
    $00,$0c,$18,$30,$00,$7c,$c6,$06,$7e,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$0c,$18,$30,$00,$30,$30,$30,$30,$30,$30,$30,$30,$38,$00,$00,
    $00,$0c,$18,$30,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,
    $00,$0c,$18,$30,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$ce,$7a,$00,$00,
    $00,$00,$76,$dc,$00,$bc,$e6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$76,$dc,$00,$c6,$c6,$e6,$f6,$fe,$de,$ce,$c6,$c6,$c6,$00,$00,
    $00,$78,$0c,$7c,$cc,$76,$00,$fe,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$78,$cc,$cc,$cc,$78,$00,$fc,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$30,$30,$00,$00,$30,$30,$30,$30,$60,$c0,$c6,$c6,$7c,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$c0,$c0,$c0,$c0,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$06,$06,$06,$06,$00,$00,$00,$00,$00,
    $00,$00,$c0,$c0,$c2,$c6,$cc,$18,$30,$7c,$c6,$8c,$18,$3e,$00,$00,
    $00,$00,$c0,$c0,$c2,$c6,$cc,$18,$36,$6e,$de,$be,$06,$06,$00,$00,
    $00,$38,$38,$00,$00,$38,$38,$38,$38,$7c,$7c,$7c,$7c,$38,$00,$00,
    $00,$00,$00,$00,$00,$1b,$36,$6c,$d8,$6c,$36,$1b,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$d8,$6c,$36,$1b,$36,$6c,$d8,$00,$00,$00,$00,
    $44,$11,$44,$11,$44,$11,$44,$11,$44,$11,$44,$11,$44,$11,$44,$11,
    $aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,
    $bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee,
    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$18,$f8,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$f8,$18,$f8,$18,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$36,$36,$36,$f6,$36,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$00,$00,$00,$fe,$36,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$00,$f8,$18,$f8,$18,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$36,$f6,$06,$f6,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$00,$fe,$06,$f6,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$f6,$06,$fe,$00,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$36,$36,$36,$fe,$00,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$f8,$18,$f8,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$f8,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$18,$1f,$00,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$18,$18,$ff,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$ff,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$18,$1f,$18,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$18,$18,$ff,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$1f,$18,$1f,$18,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$36,$36,$36,$37,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$37,$30,$3f,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$3f,$30,$37,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$f7,$00,$ff,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$ff,$00,$f7,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$37,$30,$37,$36,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$00,$ff,$00,$ff,$00,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$36,$f7,$00,$f7,$36,$36,$36,$36,$36,$36,$36,
    $18,$18,$18,$18,$18,$18,$ff,$00,$ff,$00,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$36,$36,$36,$ff,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$ff,$00,$ff,$18,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$00,$ff,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$36,$3f,$00,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$1f,$18,$1f,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$1f,$18,$1f,$18,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$00,$3f,$36,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$36,$ff,$36,$36,$36,$36,$36,$36,$36,
    $18,$18,$18,$18,$18,$18,$ff,$18,$ff,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$18,$f8,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$1f,$18,$18,$18,$18,$18,$18,$18,
    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
    $00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
    $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,
    $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,
    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$76,$dc,$d8,$d8,$d8,$dc,$76,$00,$00,$00,
    $00,$00,$00,$7c,$c6,$c6,$fc,$c6,$c6,$c6,$fc,$c0,$c0,$c0,$00,$00,
    $00,$00,$00,$00,$fe,$c6,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$6c,$6c,$6c,$6c,$6c,$6c,$ec,$00,$00,
    $00,$00,$00,$00,$00,$fe,$c6,$60,$30,$38,$30,$60,$c6,$fe,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$7e,$cc,$cc,$cc,$cc,$cc,$78,$00,$00,
    $00,$00,$00,$00,$00,$00,$6c,$6c,$6c,$6c,$6c,$76,$60,$60,$c0,$00,
    $00,$00,$00,$00,$00,$00,$76,$dc,$d8,$18,$18,$18,$18,$18,$00,$00,
    $00,$00,$00,$00,$00,$fc,$30,$78,$cc,$cc,$cc,$78,$30,$fc,$00,$00,
    $00,$00,$00,$00,$00,$38,$7c,$c6,$c6,$fe,$c6,$c6,$7c,$38,$00,$00,
    $00,$00,$00,$00,$38,$6c,$c6,$c6,$c6,$c6,$6c,$6c,$6c,$ee,$00,$00,
    $00,$00,$00,$00,$38,$60,$30,$18,$7c,$cc,$cc,$cc,$cc,$78,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$7e,$d8,$d8,$d8,$7e,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$06,$0c,$7e,$db,$db,$db,$7e,$30,$60,$00,$00,
    $00,$00,$00,$00,$1c,$30,$60,$60,$78,$60,$60,$60,$30,$1c,$00,$00,
    $00,$00,$00,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$00,$00,$fe,$00,$00,$fe,$00,$00,$00,
    $00,$00,$00,$00,$18,$18,$18,$7e,$18,$18,$18,$00,$00,$7e,$00,$00,
    $00,$00,$60,$30,$18,$0c,$06,$06,$0c,$18,$30,$60,$00,$7e,$00,$00,
    $00,$00,$00,$0c,$18,$30,$60,$c0,$60,$30,$18,$0c,$00,$fe,$00,$00,
    $00,$00,$38,$6c,$6c,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,
    $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$6c,$6c,$6c,$38,$00,$00,$00,$00,
    $00,$00,$00,$00,$18,$18,$00,$7e,$00,$18,$18,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$76,$dc,$00,$76,$dc,$00,$00,$00,$00,$00,$00,
    $00,$00,$38,$6c,$6c,$38,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$38,$38,$38,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$18,$18,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$0f,$0c,$0c,$0c,$0c,$0c,$0c,$cc,$6c,$6c,$3c,$1c,$00,$00,
    $00,$00,$bc,$ee,$c6,$c6,$c6,$c6,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$38,$6c,$18,$30,$64,$7c,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$7c,$7c,$7c,$7c,$7c,$7c,$7c,$7c,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    ];
end;

{ TODO 1 -Devel Included an embedded version og the basic DOS unicode font file. }

(* Unicode Font File for DOS, File Format Specification, Version 1.0 (draft) *)

const
  UFFFileID : array[0..5] of char = ('U', 'N', 'I', 'F', 'N', 'T');

type
  { Unicode Character Value 0 - 0x10ffff (21 bits) }
  TUFFCharacterIndex = array[0..2] of byte;

  { Unicode Font File Header Block }
  TUFFHeader = record
    ID : array[0..5] of char;        { Contains ‘UNIFNT’ }
    HeaderSize : word;               { Total size of file header, this is also
                                       the position in the file where the
                                       character data stream begins. }
    Width : byte;                    { Bit width of characters }
    Height : byte;                   { Bit height of characters }
    Count : word;                    { Total number of characters defined }
    BaseLine : Int8;                 { vertical offset to bottom of text. This value
                                       can be used to bottom align text using
                                       different size fonts. }
    UnderLine : Int8;                { vertical offset to generate underline text.
                                       if 0, do not use this value to underline text. }
    StrikeOut : Int8;                { vertical offset to generate strikethrough text.
                                       if 0, do not use this value to strike out text. }
    Displacement : Int8;             { Horizontal offset of characters. Primarily,
                                       for things like slanted italic text. Generally,
                                       this value is zero. }
    First : TUFFCharacterIndex;      { Lowest character value defined in font }
    Last : TUFFCharacterIndex;       { Highest character value defined in font }
    { General Informational fields for user }
    Version : word;                  { Font release version n.nn (High.Low) }
    Style : word;                    { Font style bit flags
                                       Low Byte:
                                        00000000b = Normal/Monospace
                                        00000001b = Proportional
                                        11111110b = Reserved
                                       High Byte:
                                        00000011b = Thin/Bold amount ( 0-3 )
                                        00000100b = 1=thin, 0=bold
                                        00011000b = Retalic/Italic amount ( 0-3 )
                                        00100000b = 1=Retailic, 0=Italic
                                        11000000b = Reserved }

    { The following fields can vary in length. While part of the Header,
      they are not included in this record. }

    // Font : ASCIIzString;             { Font name (Roman, Helvetica, etc) }
    // Title : ASCIIzString;            { Font title (FD-System, Scary Halloween, etc) }
    // Comment : ASCIIzString;          { Any other text (Author, Copyright, etc) }
    { There may be additional fields added in future revisions of this format.
      do not assume the size of the header block will fixed at its current size.
      Use the HeaderSize field to determine the length of the Header. The location
      of the first data block will begin at that offset position. }
  end;

const
  uffSkipCount        = $80; // Followed by byte, no other bits can be set.
  uffUserDefined      = $40; // Followed by word size of user data, no other bits.
  uffCombining        = $20; // Followed by TUFFCharacterIndex of combining character (like diacritical).
  uffDuplicate        = $10; // Followed by TUFFCharacterIndex of previously defined character.
  uffPadded           = $08; // No additional data.
  uffSpacing          = $04; // Character spacing word. (Two Int8s)
  uffMasking          = $02; // Used line mask word.
  uffCharacter        = $01; // Character data defined.
  uffTerminate        = $00; // End of font data stream.

function UFFToInt32(Index: TUFFCharacterIndex): Int32;
begin
  Result:=Index[2];
  Result :=Result shl 8 + Index[1];
  Result :=Result shl 8 + Index[0];
end;

{ TCustomDosFont }

function TCustomDosFont.GetPixels(Index: integer): TArrayOfBoolean;
var
  D : TArrayOfByte;
  I, P : Integer;
  Y, X : integer;
  B : integer;
begin
  Result:=[];
  D:=GetBitMask(Index);
  if Length(D) = 0 then Exit;
  SetLength(Result, FWidth * FHeight);
  for I := 0 to High(Result) do
    Result[I] := False;
  I := 0;
  P := 0;
  for Y := 0 to FHeight - 1 do begin
    B:=0;
    for X := 0 to FWidth - 1 do begin
      Result[P]:= (D[I] and (1 shl (7 - B))) <> 0;
      Inc(P);
      Inc(B);
      if B > 7 then begin
        B:=0;
        Inc(I);
      end;
    end;
    if B <> 0 then
      Inc(I);
  end;
end;

constructor TCustomDosFont.Create;
begin
  inherited Create;
  FWidth:=0;
  FHeight:=0;
  FData:=[];
end;

{ TBitmapDosFont }

procedure TBitmapDosFont.Reset;
begin
  FData:=GetDefaultBitFont;
  FWidth:=8;
  FHeight:=Length(FData) div 256;
end;

function TBitmapDosFont.GetBitMask(Index: integer): TArrayOfByte;
var
  I, P : Integer;
begin
  Result:=[];
  if FWidth or FHeight = 0 then Exit;
  if (Index < 0) or (Index > 255) then Exit;
  SetLength(Result, FHeight);
  P := Index * FHeight;
  for I := 0 to High(Result) do
    Result[I]:= FData[P+I];
end;

constructor TBitmapDosFont.Create;
begin
  inherited Create;
  FData:=[];
  Reset;
end;

function TBitmapDosFont.LoadFromFile(FileName: String): integer;
begin
  FWidth:=0;
  FHeight:=0;
  Result:=FileLoad(FileName, FData);
  if Result = 0 then begin
    LogMessage(vbVerbose, 'Loaded DOS bitmap font: ' + FileName);
    FHeight:=Length(FData) div 256;
    if FHeight * 256 <> Length(FData) then begin
      Reset;
      Result:=5;
    end else
      FWidth:=8;
  end else
    LogMessage(vbCritical, 'Error #' + IntToStr(Result) + ' loading DOS bitmap font: ' + FileName);

end;

{ TUnicodeDosFont }

procedure TUnicodeDosFont.Reset;
begin
  FInvalidChar:=-1;
  FWidth:=0;
  FHeight:=0;
  SetLength(FData, 0);
end;

function TUnicodeDosFont.GetPadded(Index : integer): boolean;
var
  N : TBinaryTreeNode;
begin
  N:=FChars.Find(Index);
  if Assigned(N) then
    Result:=N.Flags and uffPadded <> 0
  else
    Result:=False;
end;

function TUnicodeDosFont.GetCount: integer;
begin
  if Assigned(FChars) then
    Result:=FChars.Count
  else
    Result:=0;
end;

function TUnicodeDosFont.GetBitMask(Index: integer): TArrayOfByte;
var
  N : TBinaryTreeNode;
begin
  Result := [];
  N:=FChars.Find(Index);
  if Assigned(N) then
    Result:=N.Data
  else if FInvalidChar > 0 then begin
    N:=FChars.Find(FInvalidChar);
    if Assigned(N) then
      Result:=N.Data
  end;
end;

procedure TUnicodeDosFont.SetInvalidChar(AValue: Int32);
begin
  if FInvalidChar=AValue then Exit;
  FInvalidChar:=AValue;
end;

function TUnicodeDosFont.ProcessHeader: integer;
var
  I : integer;
  H : TUFFHeader;
  P : integer;
begin
  PseudoInit(H);
  Result:=5;
  // FCount:=0;
  P:=0;
  for I := 0 to 5 do
    if UFFFileID[I] <> Char(FData[I]) then begin
      LogMessage(vbVerbose, 'not a Unicode font file');
      Exit;
    end;
  if Length(FData) < Sizeof(TUFFHeader) + 3 then begin
    LogMessage(vbVerbose, 'font file is trucated');
    Exit;
  end;
  ReadData(P, H, SizeOf(H));
  if Length(FData) <  H.HeaderSize + 1 then begin
    LogMessage(vbVerbose, 'invalid header size');
    Exit;
  end;
  { Parse Header }
  FWidth:=H.Width;
  FHeight:=H.Height;
  FBaseLine:=H.BaseLine;
  FUnderLine:=H.UnderLine;
  FStrikeOut:=H.StrikeOut;
  FDisplacement:=H.Displacement;
  FVersion:=H.Version;
  if H.Style and $c0fe <> 0 then
    LogMessage(vbVerbose, TAB + 'ignore unrecognized style bits: ' + BinStr(H.Style and $c0fe, 16) );
  FProportional:=H.Style and 1 <> 0;
  FBoldness := (H.Style shr 8) and 3;
  if H.Style and $0400 <> 0 then FBoldness:=-FBoldness;
  FItalics := (H.Style shr 11) and 3;
  if H.Style and $0200 <> 0 then FItalics:=-FItalics;
  FFontName:=ReadASCIIz(P);
  FTitle:=ReadASCIIz(P);
  FComment:=ReadASCIIz(P);
  { Process Data }
  if P <> H.HeaderSize then begin
    LogMessage(vbVerbose, 'extra data in header (contains ' + IntToStr(P) +
      ' bytes, expected ' + IntToStr(H.HeaderSize) + ' bytes)' );
    P := H.HeaderSize;
  end;
  if not DecodeChars(P, UFFToInt32(H.First), UFFToInt32(H.Last), H.Count) then begin
    LogMessage(vbMinimal, TAB+'decoding error');
    Exit;
  end;
  if P <> Length(FData) then begin
    LogMessage(vbVerbose, TAB+'extraneous data after character stream');
  end;
  { Summary }
  Result:=0;
end;

function TUnicodeDosFont.DecodeChars(var Position: integer; Current,
  Last: Int32; Expected: word): boolean;
var
  Flags : byte;
  Entry, W : word;
  U : TUFFCharacterIndex;
  CCombine : Int32;
  CSpacing, CMaskSize : word;
  CMask, CMAll, CData, CChar : TArrayOfByte;
  CPad : Boolean;
  CBytes, I : integer;
begin
  Result:=False;
  Entry:=0;
  CMaskSize:=BitSize(FHeight);
  CMAll:=[];
  SetLength(CMAll, CMaskSize);
  for I := 0 to CMaskSize - 1 do
    CMAll[I]:=255;
  while true do try
    if Position > Length(FData) then raise Exception.Create('not terminated');
    ReadData(Position, Flags, Sizeof(Flags));
    { LogMessage(vbExcessive, TAB + 'Action Flags: ' + BinStr(Flags,8)); }

    if Flags = uffTerminate then Break; { terminated }

    if Flags and uffSkipCount <> 0 then begin { Skip Count }
      W := (Flags and $7f);
      W:=W shl 8;
      ReadData(Position, W, Sizeof(Byte));
      Current:=Current + W + 1;
      LogMessage(vbExcessive, TAB + 'skip ' + IntToStr(W+1) + ' character' +
       WhenTrue(W+1 <>1, 's'));
      Continue;
    end;

    if Flags and (uffUserDefined) <> 0 then begin { unknown data }
      ReadData(Position, W, SizeOf(W));
      LogMessage(vbExcessive, TAB + IntToStr(W) + ' byte' + WhenTrue(W <> 1, 's') +
      ' of unknown data');
      Position := Position + W;
      Continue;
    end;

    if Flags and uffCombining <> 0 then begin { Combine with character }
      ReadData(Position, U, Sizeof(U));
      CCombine:=UFFToInt32(U);
    end else
      CCombine:=-1;

    if Flags and uffDuplicate <> 0 then begin { Duplicate of existing }
      ReadData(Position, U, Sizeof(U));
      CMask:=[];
      CChar:=GetBitMask(UFFToInt32(U));
      CPad:=GetPadded(UFFToInt32(U));
      CSpacing:=0;
      LogMessage(vbExcessive, TAB+'Entry #' + IntToStr(Entry) + ' (U+' +
        HexStr(Current, 6) + ') duplicate of U+' + HexStr(UFFToInt32(U),6));
    end else begin
      if (Flags and uffCharacter = 0) then
        LogMessage(vbNormal, 'Probable corruption of UFF data.');
      CPad:=Flags and uffPadded <> 0;
      if Flags and uffSpacing <> 0 then  { Spacing Data }
        ReadData(Position, CSpacing, Sizeof(CSpacing))
      else
        CSpacing := 0;
      if Flags and uffMasking <> 0 then begin { Masking Data }
        CMask:=ReadBytes(Position, CMaskSize);
        CBytes:=BitSize(FWidth) * BitCount(CMask, FHeight);
      end else begin
        CBytes:=BitSize(FWidth) * FHeight;
        CMask:=Copy(CMAll, 0, CMaskSize);
      end;
      if CBytes <> 0 then
        CData:=ReadBytes(Position, CBytes);
      CChar := DecodeChar(CData, CMask);
    end;

    { TODO 0 -cDevel UFF Combine Character Support }
    if CCombine <> -1 then begin
      LogMessage(vbMinimal, 'UFF combining characters not yet implemented.');
    end;

    LogMessage(vbExcessive, TAB+'Entry #' + IntToStr(Entry) + ' (U+' +
      HexStr(Current, 6) + ') ' + IntToStr(CBytes) + ' byte' +
      WhenTrue(CBytes <> 1, 's'));
    if CSpacing <> 0 then
      LogMessage(vbExcessive, TAB2+'Spacing: ' + HexStr(CSpacing, 4));
    if Length(CMask) <> 0 then begin
      LogMessage(vbExcessive, TAB2+'Masking: ', CMask, FHeight);
      LogMessage(vbExcessive, TAB2+'Data: ', CData);
    end;
    LogMessage(vbExcessive, TAB2+'Character: ', CChar);
    AddCharacter(Current, CChar, CPad);

    { TODO 0 -cDevel UFF Character Spacing Support }

    Inc(Entry);
    Inc(Current);

  except
    on E : Exception do begin
      LogMessage(vbCritical, 'UFF, exception: ' + E.Message);
      LogMessage(vbCritical, 'UFF, corrupt unicode font file');
      exit;
    end;
  end;
  Dec(Current);
  if Last <> Current then
    LogMessage(vbMinimal, 'UFF, last expected character was U+' + HexStr(Last,6) +
    ', but was ' + HexStr(Current ,6));
  if Expected <> Count then
    LogMessage(vbMinimal, 'UFF, expected ' + IntToStr(Expected) + ' character' +
    WhenTrue(Expected <> 1, 's') + ', but ' + IntToStr(Count) + SPACE +
    WhenTrue(Count <> 1, 'were', 'was') + SPACE + 'defined');
  Result:=True;
end;

function TUnicodeDosFont.DecodeChar(CData, CMask: TArrayOfByte): TArrayOfByte;
var
  M : DWord;
  I, R, J, W : integer;
begin
  CData := BitUnpack(CData, FWidth);
  M := 0;
  for I := Length(CMask) - 1 downto 0 do
    M := (M shl 8) + CMask[I];
  // LogMessage(vbExcessive, TAB2 + 'Mask Value: ' + BinStr(M, FHeight));
  Result:=[];
  W:=BitSize(FWidth);
  SetLength(Result, W * FHeight);
  R:=0;
  for I := 0 to FHeight - 1 do
    if (M shr I) and 1 = 0 then begin
      for J := 0 to W - 1 do
        Result[I * W + J] := 0;
    end else begin
      for J := 0 to W - 1 do begin
        Result[I * W + J] := CData[R];
        Inc(R);
      end;
    end;
end;

procedure TUnicodeDosFont.AddCharacter(Index: Int64; CData: TArrayOfByte;
  AValue: boolean);
var
  N : TBinaryTreeNode;
begin
  N:=FChars.Find(Index);
  if Assigned(N) then begin
    LogMessage(vbCritical, 'Identical UniqueID ' + IntToStr(Index) + ' already present in tree.');
    Exit;
  end;
  try
    N:=FChars.Add(Index, CData);
    N.Value:=WhenTrue(AValue, uffPadded);
  except
    LogMessage(vbCritical, 'Failed to add UniqueID ' + IntToStr(Index) + ' to tree.');
  end;
end;

procedure TUnicodeDosFont.ReadData(var Position: integer; out V; C: Word);
type
  Bytes=array[0..65535] of byte;
var
  I : integer;
begin
  try
    for I := 0 to C - 1 do begin
      Bytes(V)[I]:=FData[Position];
      Inc(Position);
    end;
  finally
  end;
end;

function TUnicodeDosFont.ReadASCIIz(var Position: integer): RawByteString;
var
  B : Byte;
begin
  try
    Result:='';
    While Position < Length(FData) do begin
      ReadData(Position, B, Sizeof(B));
      if B = 0 then Break;
      Result:=Result+Char(B);
    end;
  except
    Result:='';
  end;
end;

function TUnicodeDosFont.ReadBytes(var Position: integer; C: word
  ): TArrayOfByte;
var
  I : integer;
  B : Byte;
begin
  Result:=[];
  SetLength(Result, C);
  for I := 0 to C - 1 do begin
    ReadData(Position, B, Sizeof(B));
    Result[I]:=B;
  end;
end;

constructor TUnicodeDosFont.Create;
begin
  inherited Create;
  FChars := TBinaryTree.Create;
  FData:=[];
  Reset;
end;

destructor TUnicodeDosFont.Destroy;
begin
  if Assigned(FChars) then FreeAndNil(FChars);
  inherited Destroy;
end;

function TUnicodeDosFont.LoadFromFile(FileName: String): integer;
begin
  Reset;
  Result:=FileLoad(FileName, FData);
  if Result = 0 then
    Result:=ProcessHeader;
  SetLength(FData, 0);
  if Result = 0 then begin
    LogMessage(vbVerbose, 'Loaded DOS Unicode font: ' + FileName);
  end else begin
    Reset;
    LogMessage(vbCritical, 'Error #' + IntToStr(Result) + ' loading DOS Unicode font: ' + FileName);
  end;
  FChars.Balance;
end;

initialization

finalization

end.

