program adventOfCode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, etpackage, aoc23, iAoc, aocPuzzle, visualise, arrayUtils, aocUtils,
  anysort, fileUtilities, cardData, rangeConvert, hand, pipenode, day16, day17,
  day18, day19, day20, day21, day22, day23, day24, day25, gridElement;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfVisualise, fVisualise);
  Application.Run;
end.

