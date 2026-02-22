program CPWitch;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Interfaces, Forms, SysUtils, Classes, // this includes the LCL widgetset
  Version, PasExt, MultiApp, Icons, Preferences,
  uMain, uPrefs, uFixEnding;

{$R *.res}

begin
  try
    {$IFDEF BUILD_PRERELEASE}
    Create_No_Language:=True;
    {$ENDIF}
    {$IFDEF BUILD_DEBUG}
    LogMessage(vbNormal, 'Generating icon sets...');
    {$ENDIF}
    CreateIconSets;
    RequireDerivedFormResource:=True;
    Application.Scaled:=True;
    {$PUSH}{$WARN 5044 OFF}
    Application.MainFormOnTaskbar:=True;
    {$POP}
    {$IFDEF BUILD_DEBUG}
    LogMessage(vbNormal, 'Initializing application...');
    {$ENDIF}
    Application.Initialize;
    {$IFDEF BUILD_DEBUG}
    LogMessage(vbNormal, 'Creating main form...');
    {$ENDIF}
    Application.CreateForm(TfMain, fMain);
    {$IFDEF BUILD_DEBUG}
    LogMessage(vbNormal, 'Starting application...');
    {$ENDIF}
    Application.Run;
    {$IFDEF BUILD_DEBUG}
    LogMessage(vbNormal, 'Shut down...');
    {$ENDIF}
  except
    on E:Exception do begin
      WriteLn('Critical exception: ' + E.ClassName + ', ' + E.Message);
      LogMessage(vbCritical, 'Critical exception: ' + E.ClassName + ', ' + E.Message);
    end;
  end;
end.

