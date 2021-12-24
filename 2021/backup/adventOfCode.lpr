program adventOfCode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, advent, bingo, ventMap, arrayUtils, anysort, paintbox, octopus,
  origami, polymer, chiton, packet, rangecalc, trickshot, snailfish;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TpaintboxForm, paintboxForm);
  Application.Run;
end.

