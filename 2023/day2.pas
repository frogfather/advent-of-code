unit day2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwo }
  TDayTwo = class(TAocPuzzle)
  private
  function gamePossible(game_:string):boolean;
  function findGamePower(game_:string):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwo }

constructor TDayTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 2',paintbox_);
//parent loads the file as a string and converts to string array;
end;

//Find which games are possible with 12 red cubes, 13 green cubes, and 14 blue cubes.
//Add game Ids
procedure TDayTwo.runPartOne;
var
  gameIndex: integer;
  sum:integer;
begin
  results.Clear;
  sum:=0;
  for gameIndex:=0 to pred(puzzleinputlines.size) do
    if gamePossible(puzzleInputLines[gameIndex]) then sum:=sum+gameIndex+1;
  results.Add('The sum is '+sum.ToString);
end;

//Find min cubes required for each game and multiply
//Add above for each game
procedure TDayTwo.runPartTwo;
var
  gameIndex: integer;
  sum:integer;
begin
  results.Clear;
  sum:=0;
  for gameIndex:=0 to pred(puzzleinputlines.size) do
    sum:=sum + findGamePower(puzzleinputlines[gameIndex]);
  results.add('The sum is '+sum.toString);
end;

function TDayTwo.gamePossible(game_: string): boolean;
var
  index,quantity,partIndex:integer;
  numberColourPairs,quantityAndColour,gameParts: TStringArray;
  sColour:string;
begin
  result:=false;
  gameParts:=game_.Split([':',';']);
  for partIndex:=1 to pred(gameParts.size) do
    begin
    numberColourPairs:=gameParts[partIndex].Split([',']);
    for index:=0 to pred(numberColourPairs.size) do
      begin
      quantityAndColour:=numberColourPairs[index].Trim.Split([' ']);
      sColour:=quantityAndColour[1];
      quantity:=quantityAndColour[0].Trim.ToInteger;
        case sColour of
          'red': if (quantity > 12) then exit;
          'green': if (quantity > 13) then exit;
          'blue': if (quantity > 14) then exit;
        end;
      end;
    end;
  result:=true; //only reach this point if we don't exit in the case statement
end;

function TDayTwo.findGamePower(game_: string): integer;
var
  index,quantity:integer;
  numberColourPairs,quantityAndColour:TStringArray;
  sColour:string;
  minRed,minGreen,minBlue:integer;
begin
  numberColourPairs:=game_.Split([':',';',',']);
  minRed:=1;
  minGreen:=1;
  minBlue:=1;

  for index:=1 to pred(numberColourPairs.size) do
    begin
    quantityAndColour:=numberColourPairs[index].Trim.Split([' ']);
    sColour:=quantityAndColour[1];
    quantity:=quantityAndColour[0].Trim.ToInteger;
    case sColour of
        'red': if quantity > minRed then minRed:=quantity;
        'green': if quantity > minGreen then minGreen:= quantity;
        'blue': if quantity > minBlue then minBlue:=quantity;
      end;
    end;
  result:=minRed * minGreen * minBlue;
end;

end.


