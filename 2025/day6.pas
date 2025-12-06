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
  function calculateSum(sumIndex:integer):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  data:T2DInt64Map;
  operators:TStringArray;

{ TDaySix }

procedure TDaySix.loadDataAndOperators;
var
  lineIndex,elementIndex:integer;
  inputLine:string;
  inputElements:TStringArray;
begin
  operators:=puzzleInputLines[pred(puzzleInputLines.size)].Split(' ',TstringSplitOptions.ExcludeEmpty);
  for lineIndex:= 0 to pred(pred(puzzleInputLines.size)) do
    begin
    inputLine:=puzzleInputLines[lineIndex].Trim;
    inputElements:=inputLine.Split(' ',TstringSplitOptions.ExcludeEmpty);
    for elementIndex:=0 to pred(inputElements.size) do
      begin
      if inputElements[elementIndex].Trim <> '' then
      data.push(lineIndex,inputElements[elementIndex].ToInt64);
      end
    end;
    results.add('data has '+data.rows.toString+' rows and operators has '+operators.size.toString+' entries.');
end;

function TDaySix.calculateSum(sumIndex: integer): int64;
var
  yindex:integer;
  valuesForSum:TInt64Array;
  operatorForSum:string;

  testIndex:integer;
begin
  valuesForSum:=TInt64Array.create;
  for yindex:=0 to pred(data.rows) do
    if (sumIndex < data.size(yindex)) then
      valuesForSum.push(data[yindex][sumIndex]);
    operatorForSum:= operators[sumIndex];

  result:=valuesForSum[0];
    results.add(valuesForSum[0].toString);
  for testIndex:=1 to pred(valuesForSum.size) do
    begin
    results.add(valuesForSum[testIndex].toString);
    if (operatorForSum = '*') then
      result:=result * valuesForSum[testIndex]
    else result:=result + valuesForSum[testIndex];
    end;
  results.add('result of sum at index '+sumIndex.toString+' is '+result.toString);
end;

constructor TDaySix.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 6',paintbox_);
//parent loads the file as a string and converts to string array;
data:=T2DInt64Map.create;
operators:=TStringArray.create;
end;

procedure TDaySix.runPartOne;
var
  maxLineLength,yIndex,xIndex:integer;
  totalResult:int64;
begin
  results.Clear;
  loadDataAndOperators;
  maxLineLength:=0;
  for yindex:=0 to pred(data.rows)do
    if data.size(yindex)>maxLineLength then
      maxLineLength:=data.size(yindex);
  totalResult:=0;
  for xindex:=0 to pred(maxLineLength) do
    totalResult:=totalResult + calculateSum(xIndex);
  results.add('total result is '+totalResult.ToString);
end;

procedure TDaySix.runPartTwo;
begin
  results.Clear;
end;


end.

                
