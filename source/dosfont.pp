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
  Classes, SysUtils, PasExt, CodePages;

type

  { TCustomDosFont }

  TCustomDosFont = class
  private
    FHeight: integer;
    FWidth: integer;
  protected
    function GetCharacter(Index : integer): TArrayOfByte; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadFromFile(FileName : String) : integer; virtual; abstract;
    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property Character[Index : integer] : TArrayOfByte read GetCharacter;
  published
  end;

  { TBitmapDosFont }

  TBitmapDosFont = class ( TCustomDosFont )
  private
     FData : TArrayOfByte;
  protected
    function GetCharacter(Index : integer): TArrayOfByte; override;
  public
    constructor Create; override;
    function LoadFromFile(FileName : String) : integer; override;
  published
  end;

  { TUFFDosFont }

   TUFFDosFont = class ( TCustomDosFont )
  private
  protected
  public
  published
  end;

implementation

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

{ TCustomDosFont }

constructor TCustomDosFont.Create;
begin
  inherited Create;
  FWidth:=0;
  FHeight:=0;
end;

destructor TCustomDosFont.Destroy;
begin
  inherited Destroy;
end;

{ TBitmapDosFont }

function TBitmapDosFont.GetCharacter(Index: integer): TArrayOfByte;
begin
  Result:=[];
  if FWidth or FHeight = 0 then Exit;
end;

constructor TBitmapDosFont.Create;
begin
  inherited Create;
end;

function TBitmapDosFont.LoadFromFile(FileName: String): integer;
begin

end;


initialization

finalization

end.

