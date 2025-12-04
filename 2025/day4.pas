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
begin
  result:=0;
  for yindex:=0 to pred(rolls.rows) do
    for xindex:=0 to pred(rolls.size(0))do
      begin
      //is there a roll there?
      //is there a roll to the left or right?
      //if there is a row above, is there a roll above, above-left and above-right?
      //if there is a row below is there a roll below, below-left and below-right?
      if rolls[yindex][xindex]='@' then
        begin
        surroundingRolls:=0;
        if (xindex > 0)and(rolls[yindex][xindex-1]='@')then surroundingRolls:=surroundingRolls + 1;
        if (xindex < pred(rolls.size(yindex)))and(rolls[yindex][xindex+1]='@')then surroundingRolls:=surroundingRolls + 1;
        if (yindex > 0) then
          begin
          if (xindex > 0)and(rolls[yindex-1][xindex-1]='@')then surroundingRolls:=surroundingRolls + 1;
          if (rolls[yindex-1][xindex]='@')then surroundingRolls:=surroundingRolls + 1;
          if (xindex < pred(rolls.size(yindex-1)))and(rolls[yindex-1][xindex+1]='@')then surroundingRolls:=surroundingRolls + 1;
          end;
        if (yindex < pred(rolls.rows))then
          begin
          if (xindex > 0)and(rolls[yindex+1][xindex-1]='@')then surroundingRolls:=surroundingRolls + 1;
          if (rolls[yindex+1][xindex]='@')then surroundingRolls:=surroundingRolls + 1;
          if (xindex < pred(rolls.size(yindex+1)))and(rolls[yindex+1][xindex+1]='@')then surroundingRolls:=surroundingRolls + 1;
          end;
        if (surroundingRolls < 4)then
          result:=result+1;
        end;
      end;
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
begin
  results.Clear;
end;


end.

                
