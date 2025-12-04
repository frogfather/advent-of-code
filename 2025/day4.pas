unit day4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFour}
  TDayFour = class(TAocPuzzle)
  private
  procedure loadRolls;
  function getFreeRolls:integer;
  procedure clearFlaggedRolls;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  rolls:T2DStringArray;

{ TDayFour }

procedure TDayFour.loadRolls;
var
  index:integer;
begin
  for index:=0 to pred(puzzleInputLines.size) do
    if puzzleInputLines[index]<>'' then
       rolls.push(puzzleInputLines[index]);
end;

function TDayFour.getFreeRolls: integer;
var
  xindex,yindex,surroundingRolls:integer;
  rollSymbols:TStringArray;
begin
  result:=0;
  rollSymbols:=TStringArray.create;
  rollSymbols.push('@');
  rollSymbols.push('X');
  for yindex:=0 to pred(rolls.rows) do
    for xindex:=0 to pred(rolls.size(0))do
      begin
      if rolls[yindex][xindex]='@' then
        begin
        surroundingRolls:=0;
        if (xindex > 0)and(rollSymbols.indexOf(rolls[yindex][xindex-1])>-1)then surroundingRolls:=surroundingRolls + 1;
        if (xindex < pred(rolls.size(yindex)))and(rollSymbols.indexOf(rolls[yindex][xindex+1])>-1)then surroundingRolls:=surroundingRolls + 1;
        if (yindex > 0) then
          begin
          if (xindex > 0)and(rollSymbols.indexOf(rolls[yindex-1][xindex-1])>-1)then surroundingRolls:=surroundingRolls + 1;
          if (rollSymbols.indexOf(rolls[yindex-1][xindex])>-1)then surroundingRolls:=surroundingRolls + 1;
          if (xindex < pred(rolls.size(yindex-1)))and(rollSymbols.indexOf(rolls[yindex-1][xindex+1])>-1)then surroundingRolls:=surroundingRolls + 1;
          end;
        if (yindex < pred(rolls.rows))then
          begin
          if (xindex > 0)and(rollSymbols.indexOf(rolls[yindex+1][xindex-1])>-1)then surroundingRolls:=surroundingRolls + 1;
          if (rollSymbols.indexOf(rolls[yindex+1][xindex])>-1)then surroundingRolls:=surroundingRolls + 1;
          if (xindex < pred(rolls.size(yindex+1)))and(rollSymbols.indexOf(rolls[yindex+1][xindex+1])>-1)then surroundingRolls:=surroundingRolls + 1;
          end;
        if (surroundingRolls < 4)then
          begin
          result:=result+1;
          rolls[yindex][xindex]:='X';
          end;
        end;
      end;
end;

procedure TDayFour.clearFlaggedRolls;
var
  xindex,yindex:integer;
begin
  for yindex:=0 to pred(rolls.rows) do
    for xindex:=0 to pred(rolls.size(0))do
      if rolls[yindex][xindex]='X' then rolls[yindex][xindex]:='.';
end;

constructor TDayFour.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 4',paintbox_);
rolls:=T2DStringArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayFour.runPartOne;
var
  freeRollCount:integer;
begin
  rolls.clear;
  results.Clear;
  loadRolls;
  freeRollCount:=getFreeRolls;
  results.add('free rolls '+freeRollCount.toString);
end;

procedure TDayFour.runPartTwo;
var
  freeRollCount,totalFreeRollCount:integer;
begin
  rolls.clear;
  results.Clear;
  loadRolls;
  totalFreeRollCount:=0;
    repeat
    freeRollCount:=getFreeRolls;
    totalFreeRollCount:=totalFreeRollCount + freeRollCount;
    clearFlaggedRolls;
    until freeRollCount = 0;
    results.add('free rolls '+totalfreeRollCount.toString);
end;


end.

                
