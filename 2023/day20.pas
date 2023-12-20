unit day20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,commModule;
type

  { TDayTwenty}
  TDayTwenty = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwenty }

constructor TDayTwenty.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwenty.runPartOne;
var
  bMod:TBroadcastCommsModule;
  fa,fb,fc:TFlipFlopCommsModule;
  finv,fcon:TConjunctionCommsModule;
  fOut:TOutputModule;
  commModules:TCommModules;
  index:integer;
  totalHigh,totalLow,loopcount:integer;
begin
  results.Clear;
  commModules:=TCommModules.create;

//broadcaster -> a, b, c
//%a -> b
//%b -> c
//%c -> inv
//&inv -> a

//  broadcaster -> a
//%a -> inv, con
//&inv -> b
//%b -> con
//&con -> output

  bMod:=TBroadcastCommsModule.Create('brd');
  commModules.push(bMod);
  fa:=TFlipFlopCommsModule.create('fa');
  commModules.push(fa);
  fb:=TFlipFlopCommsModule.create('fb');
  commModules.push(fb);
  fc:=TFlipFlopCommsModule.create('fc');
  commModules.push(fc);
  finv:=TConjunctionCommsModule.create('inv');
  commModules.push(finv);
  //fOut:=ToutputModule.create('out');
  //commModules.push(fOut);

  fa.subscribeTo(bMod);
  fb.subscribeTo(bMod);
  fc.subscribeTo(bMod);
  fb.subscribeTo(fa);
  fc.subscribeTo(fb);
  fInv.subscribeTo(fc);
  fa.subscribeTo(fInv);
  //fOut.subscribeTo();
  //need output module

  totalHigh:=0;
  totalLow:=0; //button sends a low to broadcast module
  for loopCount:=0 to 999 do
    begin
    bMod.pressButton;
    totalLow:=totalLow+1;
    sleep(2);
    end;

  //Set up the modules and connections
  //Work out how many presses cause the state to return to the original state

  for index:=0 to pred(commModules.size) do
    begin
    totalHigh:=totalHigh + commModules[index].highs;
    totalLow:= totalLow+ commModules[index].lows;
    end;
  results.Add('Highs '+totalHigh.ToString+' Lows '+totalLow.ToString);
end;

procedure TDayTwenty.runPartTwo;
begin
  results.Clear;
end;


end.


