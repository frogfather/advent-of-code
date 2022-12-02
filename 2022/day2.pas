unit day2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;
type

  { TDayTwo }
  TDayTwo = class(TAocPuzzle)
  private
  fName:string;
  function gamePoints(input,response:string):integer;
  function gamePoints2(input,outcome:string):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  scoring: TStringIntMap;
{ TDayOne }

//Note: William Feng's solution maps the inputs to 0,1,2 and then uses mod3 to determine the
//outcome. This is a cleaner solution than mine, but since I didn't think of it I'll leave mine as it is,
//https://www.youtube.com/watch?v=oNAChDCtk9Q
//Jonathan Paulsons approach is a little different
//https://www.youtube.com/watch?v=X1XH774hId0

function TDayTwo.gamePoints(input, response: string): integer;
  var
    theirInput,ourInput:integer;
begin
  //turn the letters into integers to make the calculation easier
  theirInput:=scoring.KeyData[input];
  ourInput:=scoring.KeyData[response];

  if (theirInput = ourInput)
       then result:=ourInput + 3 //draw
  else if (
       (theirInput = 2) and (ourInput = 3)
    or (theirInput = 1) and (ourInput = 2)
    or (theirInput = 3) and (ourInput = 1))
       then result:= ourInput + 6
  else result:=ourInput; //lose
end;

function TDayTwo.gamePoints2(input, outcome: string): integer;
var
  theirInput,ourChoice:integer;
begin
  theirInput:=scoring.KeyData[input];
  //based on their input we need to make our choice to achieve the required result
  //if outcome is X we need to lose
  //if outcome is Y we need to draw
  //if outcome is Z we need to win
  if (outcome = 'X') then //lose
    begin
    if theirInput = 1 then ourChoice:= 3 else
    if theirInput = 2 then ourChoice:= 1 else
    ourChoice:= 2;
    result:= ourChoice;
    end else
  if (outcome = 'Y') then //draw
    begin
    ourChoice:= theirInput;
    result:= ourChoice + 3;
    end else //win
    begin
    if theirInput = 1 then ourChoice:= 2 else
    if theirInput = 2 then ourChoice:= 3 else
    ourChoice:= 1;
    result:= ourChoice + 6;
    end;
end;

constructor TDayTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 2';
//Create a string int map to convert the items played to values
scoring:= TStringIntMap.Create;
scoring.Add('A',1);
scoring.Add('B',2);
scoring.Add('C',3);
scoring.Add('X',1);
scoring.Add('Y',2);
scoring.Add('Z',3);
end;


procedure TDayTwo.runPartOne;
var
  index,total:integer;
begin
  total:=0;
  for index:=0 to pred(length(puzzleInputLines)) do
      total:=total + gamePoints(puzzleInputLines[index].Substring(0,1),puzzleInputLines[index].Substring(2,1));
  results.Add('Total score is '+total.ToString);
end;

//Now we need to react to the input by choosing a response that results in the required output
procedure TDayTwo.runPartTwo;
var
  index,total:integer;
begin
  total:=0;
  for index:=0 to pred(length(puzzleInputLines)) do
      total:=total + gamePoints2(puzzleInputLines[index].Substring(0,1),puzzleInputLines[index].Substring(2,1));
  results.Add('Total score is '+total.ToString);
end;

end.

