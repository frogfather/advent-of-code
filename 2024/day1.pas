unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayOne}
  TDayOne = class(TAocPuzzle)
  private
  procedure separateAndSortLists(arr:TStringArray);
  function findIndexOfFirstMatching(arr:TIntArray; val:integer):integer;
  function findNumberOfMatchingEntries(arr: TIntArray; start, val: integer):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

  var left, right: TIntArray;

implementation

{ TDayOne }

procedure TDayOne.separateAndSortLists(arr: TStringArray);
var
  index:integer;
begin
  left.clear;
  right.clear;
  for index:= 0 to pred(arr.size) do
    begin
    left.push(arr[index].split(' ',TStringSplitOptions.ExcludeEmpty)[0].trim.ToInteger);
    right.push(arr[index].split(' ',TStringSplitOptions.ExcludeEmpty)[1].trim.ToInteger);
    end;
  sort(left,left.size);
  sort(right,right.size);
end;

//Finds first incidence of the required number in a sorted int array
function TDayOne.findIndexOfFirstMatching(arr:TIntArray; val:integer): integer;
var
  lower,upper,rangeCentre, mid:integer;
  done,firstFound:boolean;
begin
  result:=-1;
  done:=false;
  lower:=0;
  upper:=pred(arr.size);
  mid:=0;
  while not done do
    begin
    rangeCentre:= lower + ((upper-lower)div 2);
    if (mid <> rangeCentre) then mid:= rangeCentre else done:=true;
    if (arr[mid] = val) then
      begin
      result:=mid;
      done:=true;
      end else
      if (arr[mid] > val) then
      begin
      if (upper <> mid) then upper:=mid else done:=true;
      end else
      begin
      if (lower + 1 < pred(arr.size)) and (lower <> mid+1) then lower:=mid+1 else done:=true;
      end;
    end;
  if (result <> -1) then
    begin
    firstFound:=false;
    while not firstFound do
      begin
      if (result > 0) and (arr[result -1] = val)
         then result:=result - 1
         else firstFound:=true;
      end;
    end;
end;

function TDayOne.findNumberOfMatchingEntries(arr: TIntArray; start, val: integer
  ): integer;
var
  index:integer;
begin
  result:=0;
  index:=start;
  while arr[index]= val do
    begin
    result:=result+1;
    if (index < pred(arr.size)) then index:=index+1;
    end;
end;

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
left:=TIntArray.create;
right:=TIntArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayOne.runPartOne;
var
  index:integer;
  total:integer;
begin
  results.Clear;
  total:=0;
  separateAndSortLists(puzzleInputLines);
  for index:= 0 to pred(puzzleInputLines.size) do
    total:=total + abs(left[index] - right[index]);
  results.add('Total is '+total.toString);
end;

procedure TDayOne.runPartTwo;
var
  indexl:integer;
  value:integer;
  total:integer;
  matchingIndex,incidences:integer;
begin
  results.Clear;
  total:=0;
  separateAndSortLists(puzzleInputLines);
  for indexl:=0 to pred(left.size) do
    begin
    value:=left[indexl];
    matchingIndex:=findIndexOfFirstMatching(right, value);
    incidences:=findNumberOfMatchingEntries(right,matchingIndex,value);
    total:=total + (incidences * value);
    results.add('Total is '+total.toString);
    end;
end;


end.

                
