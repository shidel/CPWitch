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
  Interfaces, Forms, // this includes the LCL widgetset
  Version, PasExt, MultiApp, Icons, Preferences,
  uMain;

{$R *.res}

begin
  {$IFDEF BUILD_PRERELEASE}
  Create_No_Language:=True;
  {$ENDIF}
  CreateIconSets;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

