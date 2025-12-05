unit day5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFive}
  TDayFive = class(TAocPuzzle)
  private
  procedure loadRangesAndProducts;
  function combineRanges:TPoint64Array;
  function inRange(input:int64):boolean;
  function entryOverlap(input:TPoint64;arr:TPoint64Array):integer;
  function adjustEntry(input,entryToBeAdjusted:TPoint64):TPoint64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  ranges:TPoint64Array;
  products:TInt64Array;

{ TDayFive }

procedure TDayFive.loadRangesAndProducts;
var
  index:integer;
  onProducts:boolean;
  currentRange:TStringArray;
  newPoint:TPoint64;
begin
  ranges.Clear;
  products.clear;
  onProducts:=false;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    if puzzleInputLines[index].Trim = '' then onProducts:=true else
    if onProducts = false then
      begin
      currentRange:=puzzleInputLines[index].Trim.split('-');
      newPoint.X:=currentRange[0].toInt64;
      newPoint.Y:=currentRange[1].toInt64;
      ranges.push(newPoint);
      end else
      products.push(puzzleInputLines[index].Trim.ToInt64);
    end;
  sort(ranges,ranges.size);
end;

function TDayFive.inRange(input: int64): boolean;
var
  index:integer;
begin
  result:=false;
  for index:=0 to pred(ranges.size) do
    begin
    if (input >= ranges[index].X) and (input <= ranges[index].Y) then
      begin
      result:=true;
      exit;
      end;
    if (input < ranges[index].Y) then exit;
    end;
end;

//Method to check for overlapping ranges and combine these into
//a single range which is usually larger but
//not always because Eric Wastl is a sneaky fellow.

function TDayFive.combineRanges: TPoint64Array;
var
  index:integer;
  currentEntry:TPoint64;
  overlapIndex:integer;
begin
  result:=TPoint64Array.create;
  for index:=0 to pred(ranges.size) do
    begin
    currentEntry:=ranges[index];
    overlapIndex:= entryOverlap(currentEntry,result);
    if overlapIndex > -1 then
      result[overlapIndex]:= adjustEntry(currentEntry,result[overlapIndex])
    else result.push(currentEntry);
    end;
end;

//Does the supplied entry overlap in any way with an existing entry?
//If so return the index of the existing entry
function TDayFive.entryOverlap(input: TPoint64; arr: TPoint64Array): integer;
var
  index:integer;
begin
  result:=-1;
  //Does the supplied entry overlap with any existing entry
  if arr = nil then exit;
  for index:=0 to pred(arr.size) do
    if ((input.X >= arr[index].X)
      and(input.X <= arr[index].Y)
      and (input.Y >= arr[index].Y))
    or ((input.Y >= arr[index].X)
      and(input.Y <= arr[index].Y)
      and (input.X <= arr[index].Y))
    then
      begin
      result:=index;
      exit;
      end;
end;

//Combine the two entries to give a combined value which covers both
function TDayFive.adjustEntry(input, entryToBeAdjusted: TPoint64): TPoint64;
begin
  //Combine these two numbers to give the largest range
  if (input.X < entryToBeAdjusted.X)
    then result.X:=input.X
    else result.X:=entryToBeAdjusted.X;
  if (input.Y > entryToBeAdjusted.Y)
    then result.Y:=input.Y
    else result.Y:=entryToBeAdjusted.Y;
end;

constructor TDayFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 5',paintbox_);
//parent loads the file as a string and converts to string array;
ranges:=TPoint64Array.Create;
products:=TInt64Array.create;
end;

procedure TDayFive.runPartOne;
var
  index:integer;
  freshCount:integer;
begin
  results.Clear;
  loadRangesAndProducts;
  freshCount:=0;
  for index:=0 to pred(products.size) do
    if inRange(products[index]) then freshCount:= freshCount + 1;
  results.Add('Fresh count is '+freshCount.toString);
end;

procedure TDayFive.runPartTwo;
var
  index:integer;
  freshCount:int64;
  combinedRanges:TPoint64Array;
begin
  results.Clear;
  loadRangesAndProducts;
  freshCount:=0;
  combinedRanges:=combineRanges;
  for index:=0 to pred(combinedRanges.size) do
    freshCount:=freshCount + (combinedRanges[index].Y - combinedRanges[index].X + 1);
  results.add('fresh count is '+freshCount.ToString);
end;


end.

                
