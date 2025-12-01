program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, aocgen, fileUtilities
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='aocgenerator';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TAocgenerator, Aocgenerator);
  Application.Run;
end.

