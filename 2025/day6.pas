unit day6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySix}
  TDaySix = class(TAocPuzzle)
  private
  procedure loadDataAndOperators;
  function anyNumbersInColumn(columnIndex:integer):Boolean;
  function extractNumbersForSum(sumIndex:integer;cephalapodLogic:boolean=false):TInt64Array;
  function calculateSum(sumIndex:integer;cephalapodLogic:boolean=false):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  data:T2DCharArray;
  operators:TStringArray;

{ TDaySix }

procedure TDaySix.loadDataAndOperators;
var
  lineIndex:integer;
begin
  operators:=puzzleInputLines[pred(puzzleInputLines.size)].Split(' ',TstringSplitOptions.ExcludeEmpty);
  for lineIndex:= 0 to pred(pred(puzzleInputLines.size)) do
    data.push(puzzleInputLines[lineIndex].ToCharArray);
end;

//Is this column just spaces?
function TDaySix.anyNumbersInColumn(columnIndex: integer): Boolean;
var
  yIndex:integer;
begin
  result:=false;
  for yIndex:=0 to pred(data.rows)do
    if(data[yIndex][columnIndex] <> ' ') then
      begin
      result:=true;
      exit;
      end;
end;

function TDaySix.extractNumbersForSum(sumIndex: integer;
  cephalapodLogic: boolean): TInt64Array;
var
  blockIndex,blockStartsAt,blockEndsAt:integer;
  columnIndex,rowIndex:integer;
  previousColBlank,nextColBlank:boolean;
  currentNumber:string;
begin
  result:=TInt64Array.create;
  //If regular logic we want to find the nth entries horizontally
  //so for example
  //123 328  51 64
  // 45 64  387 23
  //  6 98  215 314
  // *   +   *   +
  //if sumIndex is 1 find the second group of numbers on each line
  //i.e. 328, 64, 98
  //if cephalapod logic find the nth entries vertically
  //so for sumindex 1
  //8, 248, 369
  //The number of sums is equal to the number of operators;
  blockIndex:=-1;
  columnIndex:=0;
  blockStartsAt:=-1;
  blockEndsAt:=-1;
    repeat
    previousColBlank:= (columnIndex = 0)or not anyNumbersInColumn(columnIndex - 1);
    nextColBlank:= (columnIndex = pred(length(data[0]))) or not (anyNumbersInColumn(columnIndex + 1));
    if anyNumbersInColumn(columnIndex) and previousColBlank then
      blockIndex:=blockIndex+1;
    if (blockIndex = sumIndex) then
      begin
      if (previousColBlank) and (blockStartsAt = -1) then blockStartsAt:=columnIndex;
      if (nextColBlank) and (blockEndsAt = -1) then blockEndsAt:=columnIndex;
      end;
    columnIndex:= columnIndex + 1;
    until (blockIndex = sumIndex) and (blockStartsAt > -1) and (blockEndsAt > -1);
  if (cephalapodLogic) then for columnIndex:= blockEndsAt downTo blockStartsAt do
    begin
    currentNumber:='';
    for rowIndex:=0 to pred(data.rows) do
      begin
      if (data[rowIndex][columnIndex] <> ' ') then
        currentNumber:=currentNumber + data[rowIndex][columnIndex];
      end;
    result.push(currentNumber.ToInt64);
    end else for rowIndex:=0 to pred(data.rows) do
    begin
    currentNumber:='';
    for columnIndex:=blockStartsAt to blockEndsAt do
      begin
      if (data[rowIndex][columnIndex] <> ' ') then
        currentNumber:=currentNumber + data[rowIndex][columnIndex];
      end;
    result.push(currentNumber.ToInt64);
    end;
end;
//The extractNumbersForSum method above will return the numbers in the correct
//format for either regular or cephalapod logic.
function TDaySix.calculateSum(sumIndex: integer;cephalapodLogic:boolean): int64;
var
  index:integer;
  numbersForSum:TInt64Array;
  op:string;
begin
  result:=0;
  op:=operators[sumIndex];
  numbersForSum:=extractNumbersForSum(sumIndex,cephalapodLogic);
  result:=numbersForSum[0];
  for index:=1 to pred(numbersForSum.size) do
    begin
    if op = '*' then result:=result * numbersForSum[index]
      else result:= result + numbersForSum[index];
    end;
end;

constructor TDaySix.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 6',paintbox_);
//parent loads the file as a string and converts to string array;
data:=T2DCharArray.create;
operators:=TStringArray.create;
end;

procedure TDaySix.runPartOne;
var
  totalResult:int64;
  sumId,totalSums:integer;
begin
  totalResult:=0;
  results.Clear;
  loadDataAndOperators;
  totalSums:= operators.size;
  for sumId:=0 to pred(totalSums) do
    totalResult:= totalResult + calculateSum(sumId);
  results.add('Total is '+totalResult.ToString);
end;

procedure TDaySix.runPartTwo;
var
   totalResult:int64;
   sumId,totalSums:integer;
begin
  totalResult:=0;
  results.Clear;
  loadDataAndOperators;
  totalSums:= operators.size;
  for sumId:=0 to pred(totalSums) do
    totalResult:= totalResult + calculateSum(sumId,true);
  results.add('Total is '+totalResult.ToString);
end;


end.

                
