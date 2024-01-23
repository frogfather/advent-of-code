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

var
  totalHigh,totalLow,loopCount:integer;
  bMod:TBroadcastCommsModule;
{ TDayTwenty }

constructor TDayTwenty.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwenty.runPartOne;
var
  fa,fb,fc:TFlipFlopCommsModule;
  finv,fcon:TConjunctionCommsModule;
  fOut:TOutputModule;
  commModules:TCommModules;
  index:integer;
begin
  results.Clear;
  commModules:=TCommModules.create;

  //example 2
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
  finv:=TConjunctionCommsModule.create('inv');
  commModules.push(finv);
  fCon:=TConjunctionCommsModule.create('con');
  commModules.push(fCon);
  fOut:=ToutputModule.create('out');
  commModules.push(fOut);

  fa.subscribeTo(bMod);
  fInv.subscribeTo(fa);
  fcon.subscribeTo(fa);
  fb.subscribeTo(fInv);
  fcon.subscribeTo(fb);
  fOut.subscribeTo(fCon);

  for loopCount:=0 to 0 do
    begin
    bMod.pressButton;
    totalLow:=totalLow+1;
    end;


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


