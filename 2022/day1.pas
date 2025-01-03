unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  function processAndSort:TIntArray;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
//parent loads the file as a string and converts to string array;
end;

//We have a list of groups of numbers separated by blank lines
//We want to add each group together and then sort to find the largest
function TDayOne.processAndSort:TIntArray;
var
  index,currentElfFood:integer;
begin
  result:=TIntArray.Create;
  currentElfFood:=0;
  for index:= 0 to pred(length(puzzleInputLines)) do
    begin
    if (puzzleInputLines[index]<> '')
      then currentElfFood:= currentElfFood + puzzleInputLines[index].ToInteger
      else
        begin
          result.push(currentElfFood);
          currentElfFood:=0;
        end;
    end;
  sort(result,result.size,false);
end;

procedure TDayOne.runPartOne;
begin
  results.Clear;
  results.Add('Elf with most calories has '+processAndSort[0].ToString);
end;

//Astonishingly I've done part one in a way that makes part two easy
procedure TDayOne.runPartTwo;
var
  sortedElfFood:TIntArray;
  sumOfTopThree:integer;
begin
  results.Clear;
  sortedElfFood:=processAndSort;
  sumOfTopThree:= sortedElfFood[0]+sortedElfFood[1]+sortedElfFood[2];
  results.Add('Sum of three elves with most food is '+sumOfTopThree.ToString);
end;

end.

