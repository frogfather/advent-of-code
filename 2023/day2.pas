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
  lineIndex,gameIndex: integer;
  lineParts: TStringArray;
  sum:integer;
  allGamesPossible:boolean;
begin
  results.Clear;
  sum:=0;
  for lineIndex:=0 to pred(puzzleinputlines.size) do
    begin
    allGamesPossible:=true;
    //First split the line on colon and semicolon
    lineParts:=puzzleinputlines[lineIndex].Split([':',';']);
    for gameIndex:=1 to pred(lineparts.size) do
      begin
      if not gamePossible(lineparts[gameIndex]) then
        begin
          allGamesPossible:=false;
          break;
        end;
      end;
    if allGamesPossible then sum:=sum+lineIndex+1; //lineIndex is always 1 less than the game id
    end;
  results.Add('The sum is '+sum.ToString);
end;

function TDayTwo.gamePossible(game_: string): boolean;
var
  index,quantity:integer;
  numberColourPairs,quantityAndColour:TStringArray;
  sColour:string;
begin
  //the input will be in the form: number colour,
  result:=true;
  numberColourPairs:=game_.Split([',']);
  for index:=0 to pred(numberColourPairs.size) do
    //split further into quantity and colour
    begin
    quantityAndColour:=numberColourPairs[index].Trim.Split([' ']);
    //first element is number, second is colour
    sColour:=quantityAndColour[1];
    quantity:=quantityAndColour[0].Trim.ToInteger;
      case sColour of
        'red': if (quantity > 12) then
          begin
            result:=false;
            exit;
          end;
        'green': if (quantity > 13) then
          begin
            result:=false;
            exit;
          end;
        'blue': if (quantity > 14) then
          begin
            result:=false;
            exit;
          end;
      end;
    end;
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
  //initializing these with 1 avoids getting a zero result if
  //a game doesn't include that colour
  for index:=1 to pred(numberColourPairs.size) do
    //split further into quantity and colour
    begin
    quantityAndColour:=numberColourPairs[index].Trim.Split([' ']);
    //first element is number, second is colour
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


//Find the minimum numbers of cubes required for each game
//multiply these together to get the power for each game
//add all the powers together
procedure TDayTwo.runPartTwo;
var
  lineIndex: integer;
  sum:integer;
begin
  results.Clear;
  sum:=0;
  for lineIndex:=0 to pred(puzzleinputlines.size) do
    sum:=sum+findGamePower(puzzleinputlines[lineIndex]);
  results.add('The sum is '+sum.toString);
end;

end.


