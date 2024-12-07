unit day7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySeven}
  TDaySeven = class(TAocPuzzle)
  private
  function getOperatorCombinations(operands:TInt64Array):TStringArray;
  function sumCanBeReached(sum:int64;operands:TInt64Array):boolean;
  function sumCanBeReachedWithCombination(sum:int64; operands:TInt64Array; operatorCombinations:TStringArray):boolean;
  function operatorCombinationsLeft(combinations:TStringArray):boolean;
  function getNextCombination(combinations:TStringArray):TStringArray;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDaySeven }
var
  options: TStringArray;
  rejected: TStringArray;
  total:int64;

function TDaySeven.getOperatorCombinations(operands: TInt64Array): TStringArray;
var
  index:integer;
begin
  result:=TStringArray.Create;
  for index:=0 to pred(operands.size - 1) do
    result.push('+');
end;

function TDaySeven.sumCanBeReached(sum: int64; operands: TInt64Array): boolean;
var
  operatorCombinations:TStringArray;
begin
  result:=true;
  //get operands length for this sum
  operatorCombinations:= getOperatorCombinations(operands);
  if sumCanBeReachedWithCombination(sum, operands, operatorCombinations) then exit;
    repeat
    operatorCombinations:=getNextCombination(operatorCombinations);
    if sumCanBeReachedWithCombination(sum, operands, operatorCombinations) then exit;
    until not operatorCombinationsLeft(operatorCombinations);
  result:= false;
end;

function TDaySeven.sumCanBeReachedWithCombination(sum: int64;
  operands: TInt64Array; operatorCombinations: TStringArray): boolean;
var
  total:int64;
  index:integer;
begin
  total:=operands[0];
  for index:=0 to pred(operatorCombinations.size) do
    begin
    if (operatorCombinations[index] = '+')
      then total:=total + operands[index + 1]
    else if (operatorCombinations[index] = '*')
      then total:=total * operands[index + 1]
    else if (operatorCombinations[index] = '|')
      then
        total:=(total.ToString+operands[index+1].ToString).ToInt64;
    end;
  result:=total = sum;
end;

function TDaySeven.operatorCombinationsLeft(combinations: TStringArray): boolean;
var
  nextCombinations:TStringArray;
  index:integer;
begin
  result:=false;
  //if any of the next options is not the first one then there are still options
  nextCombinations:= getNextCombination(combinations);
  for index:=0 to pred(nextCombinations.Size) do
    begin
    if (nextCombinations[index] <> options[0]) then
      begin
      result:=true;
      exit;
      end;
    end;
end;

function TDaySeven.getNextCombination(combinations: TStringArray): TStringArray;
var
  index,optionIndex:integer;
  carry:boolean;
begin
  result:=TStringArray.Create;
  for index:=0 to pred(combinations.Size) do
    result.push(combinations[index]);
  carry:=true;
  for index:=0 to pred(result.Size) do
    begin
    optionIndex:=options.indexOf(result[index]);
    if carry then
      begin
      if (optionIndex < pred(options.size)) then
        begin
        carry:=false;
        optionIndex:=optionIndex + 1;
        end else optionIndex:=0;
      result[index]:=options[optionIndex];
      end;
    end;
end;

constructor TDaySeven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 7',paintbox_);
rejected:=TStringArray.create;
options:=TStringArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDaySeven.runPartOne;
var
  target: int64;
  operands: TInt64Array;
  index: integer;
begin
  results.Clear;
  total:=0;
  options.clear;
  options.push('+');
  options.push('*');
  rejected.clear;
  //for each line, separate into target and operands
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    target:=puzzleInputLines[index].Split(':',TStringSplitOptions.ExcludeEmpty)[0].ToInt64;
    operands:=puzzleInputLines[index]
      .Split(':',TStringSplitOptions.ExcludeEmpty)[1]
      .Split(' ',TStringSplitOptions.ExcludeEmpty).toInt64Array;
    if sumCanBeReached(target, operands) then total:=total + target
    else rejected.push(puzzleInputLines[index]);
    end;
  results.add('total '+total.toString);
end;

procedure TDaySeven.runPartTwo;
var
  index:integer;
  target: int64;
  operands: TInt64Array;
begin
  runPartOne;//populates the rejected list and sets the total for non-rejected items
  options.push('|'); //add the new operator
  for index:=0 to pred(rejected.size) do
    begin
    target:=rejected[index].Split(':',TStringSplitOptions.ExcludeEmpty)[0].ToInt64;
    operands:=rejected[index]
      .Split(':',TStringSplitOptions.ExcludeEmpty)[1]
      .Split(' ',TStringSplitOptions.ExcludeEmpty).toInt64Array;
    if sumCanBeReached(target, operands) then total:=total + target
    end;
  results.add('total '+total.ToString)
end;


end.

                
