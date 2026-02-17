unit uFixEnding;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  PasExt, MultiApp, Icons;

type

  { TfFixEnding }

  TfFixEnding = class(TMultiAppDialog)
  private
    lbMessage : TLabel;
    FFileName: String;
    procedure SetFileName(AValue: String);
  protected
    procedure DoCreate; override;
    procedure DoButton; override;
    procedure DoShow; override;
    function CorrectFile : boolean;
  public
    property FileName : String read FFileName write SetFileName;
  end;

var
  fFixEnding: TfFixEnding;

function FixFileLineEnding(FileName : String) : TModalResult;

implementation

function FixFileLineEnding(FileName: String): TModalResult;
begin
  if not Assigned(fFixEnding) then begin
    fFixEnding:=TfFixEnding.CreateNew(Application);
  end;
  fFixEnding.FileName := FileName;
  fFixEnding.ShowModal;
  Result:=fFixEnding.ClickResult;
end;

{$R *.lfm}

{ TfFixEnding }

procedure TfFixEnding.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TfFixEnding.DoCreate;
begin
  Name:='fCorrectFileEndingDialog';
  Caption:='No final line ending';
  Images:=ilDialogColor;
  ImageIndex:=idxDialogAlert;
  ImageVisible:=True;
  FFileName:='';
  Buttons:=['Ignore', 'Correct'];
  inherited DoCreate;
  lbMessage:=TLabel.Create(Self);
  lbMessage.Parent:=BodyPanel;
  lbMessage.AutoSize:=False;
  lbMessage.Align:=alClient;
  lbMessage.Layout:=tlCenter;
  lbMessage.WordWrap:=True;
end;

procedure TfFixEnding.DoButton;
begin
  inherited DoButton;
  case ClickResult of
    0 : begin // Ignore
      ClickResult:=mrCancel;
    end;
    1 : begin // Correct
      if CorrectFile then
        ClickResult:=mrOk
      else
        ClickResult:=-1; // Do not Close;
    end;
  end;
end;

procedure TfFixEnding.DoShow;
begin
  inherited DoShow;
  lbMessage.Caption:=GetFormat('Message/Text', [ExtractFileName(FileName)],
  'The file "%s" does not end on a blank line.'+ LF + LF +
  'Do you wish to correct this by adding a new line to the end of the file?');
end;

function TfFixEnding.CorrectFile: boolean;
var
  Data : RawByteString;
  R : Integer;
begin
  Result:=True;
  if FileLoad(FileName, Data) <> 0 then begin
    // User delete file or something
    Exit;
  end;
  case DetectLineEndings(Data, leCRLF) of
    leCRLF : Data:=IncludeTrailing(Data, CRLF);
    leLF   : Data:=IncludeTrailing(Data, LF);
    leCR   : Data:=IncludeTrailing(Data, CR);
  end;
  repeat
    R:=FileSave(FileName, Data);
    if R <> 0 then begin
      if FileErrorDialog(FileName, R, True) <> mrRetry then begin
        R:=0;
        Result:=False;
      end;
    end;
  until R=0;
end;

initialization

  fFixEnding:=nil;

end.

