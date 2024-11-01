program adventOfCode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, advent, bingo, ventMap, anysort, paintbox, octopus,
  origami, polymer, chiton, packet, trickshot, snailfish, cavePassage, treeView,
  node;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TtreeForm, treeForm);
  Application.Run;
end.

