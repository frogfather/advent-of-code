unit day3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type
{ TDayThree }
  TDayThree = class(TAocPuzzle)
  private
  fName:string;
  function asciiToIndex(input:char):integer;
  function commonItems(string1,string2:string):string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }

//Return the value of the letter indicated in the puzzle
function TDayThree.asciiToIndex(input: char): integer;
begin
  if (ord(input) > 96) then result:= ord(input) - 96
  else result:= ord(input) - 38;
end;

function TDayThree.commonItems(string1, string2: string): string;
var
  index: integer;
  element:char;
begin
  result:= '';
  //Strings are 1 indexed!
  for index:= 1 to length(string1) do
    begin
      element:=string1[index];
      //ignore duplicates of the common items
      if (string2.IndexOf(element)> -1)
      and (Result.IndexOf(element) = -1) then result:=result + element;
    end;
end;

constructor TDayThree.create(filename: string; paintbox_: TPaintbox);
var
  index:integer;
begin
inherited create(filename,paintbox_);
fName:= 'Day 3';
end;

procedure TDayThree.runPartOne;
var
  lineIndex,itemIndex:integer;
  compartment1,compartment2:string;
  common:string;
  itemPrioritySum:integer;
begin
  //for each line
  //divide in two
  //find common elements
  //calculate their value
  results.Clear;
  itemPrioritySum:=0;
  for lineIndex:= 0 to pred(puzzleInputLines.size) do
    begin
      compartment1:= puzzleInputLines[lineIndex].Substring(0,length(puzzleInputLines[lineIndex]) div 2);
      compartment2:= puzzleInputLines[lineIndex].Substring(length(puzzleInputLines[lineIndex]) div 2);
      common:= commonItems(compartment1,compartment2);
      //Your daily reminder that ANSI strings are 1 indexed!
      for itemIndex:=1 to common.Length do
        itemPrioritySum:=itemPrioritySum + asciiToIndex(common[itemIndex]);
    end;
    results.Add('Sum of priorities of duplicated items is '+itemPrioritySum.ToString);
end;

procedure TDayThree.runPartTwo;
begin
  DebugLn('Run '+fName+' part two');
end;

end.

