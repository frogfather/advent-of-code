program adventOfCode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, aoc_22, iAoc, day1, day2, aocPuzzle, visualise, anysort, aocUtils,
  arrayUtils, fileUtilities, day3, day4, day5, day6, day7, day8
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfVisualise, fVisualise);
  Application.Run;
end.

