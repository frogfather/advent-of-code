unit day3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayThree}
  TDayThree = class(TAocPuzzle)
  private
  function findMaxNumberInRange(input:string;range:integer):string;
  function getMaxJoltage(range:integer):int64;
  function findLargestNumber(input:string;range:integer):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }

function TDayThree.getMaxJoltage(range:integer): int64;
var
  index:integer;
begin
  result:=0;
  for index:=0 to pred(puzzleinputlines.size) do
    begin
    result:=result+findLargestNumber(puzzleInputLines[index],range);
    end;
end;

function TDayThree.findLargestNumber(input: string;range:integer): int64;
var
  remainingRange:integer;
  remainingInput,maxNumber:string;
  sResult:string;
begin
remainingInput:=input;
remainingRange:=range;
sResult:='';
while (remainingRange > 0) do
  begin
  maxNumber:=findMaxNumberInRange(remainingInput,remainingRange);
  sResult:=sResult + maxNumber;
  remainingRange:=remainingRange - 1;
  remainingInput:=remainingInput.Substring(remainingInput.IndexOf(maxNumber)+1);
  end;
result:=sResult.ToInt64;
end;


function TDayThree.findMaxNumberInRange(input: string; range:integer):string;
var
  index,endAt,currentNumber,highestNumber:integer;
begin
   endAt:= input.Length - range;
   result:='';
   highestNumber:=0;
   for index:=0 to endAt do
    begin
    currentNumber:=input.Substring(index,1).ToInteger;
    if currentNumber > highestNumber then
      highestNumber:= currentNumber;
    end;
   result:=highestNumber.ToString;
end;

constructor TDayThree.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 3',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThree.runPartOne;
var
  max:int64;
begin
  results.Clear;
  max:=getMaxJoltage(2);
  results.add('Max is '+max.toString);
end;

procedure TDayThree.runPartTwo;
var
  max:int64;
begin
  results.Clear;
  max:=getMaxJoltage(12);
  results.add('Max is '+max.toString);
end;


end.

                
