unit day9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNine}
  TDayNine = class(TAocPuzzle)
  private
  function generateBlockArrangement(input:string):TInt64Array;
  function addBlocks(input:TInt64Array;value:string; index:int64):TInt64Array;
  function addSpaces(input:TInt64Array;value:string):TInt64Array;
  function defrag(input:TInt64Array):TInt64Array;
  function defragFiles(input: TInt64Array):TInt64Array;
  function spacesLeft(input:TInt64Array):boolean;
  function lastBlockPosition(input:TInt64Array):int64;
  function firstSpacePosition(input:TInt64Array):int64;
  function moveLastBlockToFirstSpace(input:TInt64Array):TInt64Array;
  function getCheckSum(input:TInt64Array):Int64;
  function getIndexOfHighestValueFile(input:TInt64Array;startAt:int64):int64;
  function getHighestValueFileLength(input:Tint64Array;startAt:int64):int64;
  function findIndexOfFirstLargeEnoughSpace(input:TInt64Array;size,endAt:int64):int64;
  function moveFile(value:TInt64Array;fileIndex,fileLength,spaceIndex:integer):TInt64Array;
  function noSpacesBeforeStartPoint(input:TInt64Array;startsAt:int64):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayNine }

function TDayNine.generateBlockArrangement(input:string):TInt64Array;
var
  index:int64;
begin
  result:=TInt64Array.create;
  for index:=0 to pred(input.Length) do
    if (index mod 2) = 0 then result:=addBlocks(result, input.Substring(index,1),index div 2)
      else result:=addSpaces(result,input.Substring(index,1));
end;

function TDayNine.addBlocks(input:TInt64Array;value:string; index:int64): TInt64Array;
var
  count:int64;
begin
  result:=input;
  for count:=0 to pred(value.toInt64) do
  result.push(index);
end;

function TDayNine.addSpaces(input:TInt64Array;value:string):TInt64Array;
var
  count:int64;
begin
  result:=input;
  for count:=0 to pred(value.toInt64) do
  result.push(-1);
end;

function TDayNine.defrag(input: TInt64Array): TInt64Array;
begin
  result:=input;
  while spacesLeft(result) do
    result:=moveLastBlockToFirstSpace(result)
end;

function TDayNine.defragFiles(input: TInt64Array): TInt64Array;
var
  highestValueFileIndex,highestValueFileLength:int64;
  startAt:int64;
  done:boolean;
  indexOfFirstLargeEnoughSpace:int64;
begin
  result:=input;
  startAt:=pred(input.size);
  done:=false;
  while not done do
    begin
    highestValueFileIndex:=getIndexOfHighestValueFile(result,startAt);
    highestValueFileLength:=getHighestValueFileLength(result,highestValueFileIndex);
    indexOfFirstLargeEnoughSpace:=findIndexOfFirstLargeEnoughSpace(result,highestValueFileLength,highestValueFileIndex);
    if (indexOfFirstLargeEnoughSpace > -1)
      then result:=moveFile(result,highestValueFileIndex,highestValueFileLength,indexOfFirstLargeEnoughSpace);
    startAt:=highestValueFileIndex - 1;
    done:=noSpacesBeforeStartPoint(result,startAt);
    end;
  //TODO - fix space issue
  //Find highest input unmoved file from position
end;

function TDayNine.spacesLeft(input: TInt64Array): boolean;
var
  index,lastSpaceIndex:int64;
begin
  result:=true;
  lastSpaceIndex:=-1;
  for index:=input.IndexOf(-1) to pred(input.size) do
  //if we have a dot(space) followed by a input we have spaces left
  if (input[index]= -1)
    then lastSpaceIndex:=index
    else if (lastSpaceIndex > -1) then exit;
  result:=false;
end;

function TDayNine.lastBlockPosition(input:TInt64Array): int64;
begin
  for result:=pred(input.size) downTo 0 do
    if input[result] <> -1 then exit;
end;

function TDayNine.firstSpacePosition(input: TInt64Array): int64;
begin
  result:= input.IndexOf(-1);
end;

function TDayNine.moveLastBlockToFirstSpace(input: TInt64Array): TInt64Array;
var
  firstSpacePos,lastBlockPos:int64;
begin
  result:=input;
  firstSpacePos:=firstSpacePosition(input);
  lastBlockPos:=lastBlockPosition(input);
  result[firstSpacePos]:=result[lastBlockPos];
  result[lastBlockPos]:= -1;
end;

function TDayNine.getCheckSum(input: TInt64Array): Int64;
var
  index:int64;
begin
  result:=0;
  for index:=0 to pred(input.size) do
    begin
    if (input[index] > -1) then
    result:=result+input[index] * index;
    end;
end;

function TDayNine.getIndexOfHighestValueFile(input: TInt64Array; startAt: int64
  ): int64;
var
  done:boolean;
  highestValue:int64;
begin
  result:=startAt;
  done:=false;
  highestValue:=-1;
  while not done do
    begin
    if (input[result]<> -1)and(highestValue = -1) then highestValue:= input[result];
    if (highestValue <> -1)and(result > 0) and (input[result-1] <> highestValue) then exit;
    result:=result -1;
    done:=(result = -1);
    end;
end;

function TDayNine.getHighestValueFileLength(input: Tint64Array; startAt: int64
  ): int64;
var
  index:int64;
  currentValue:int64;
  done:boolean;
begin
  result:=0;
  index:=startAt;
  done:=false;
  currentValue:=input[index];
  while not done do
    begin
    result:=result+1;
    done:=(index = pred(input.size)) or (input[index+1] <> currentValue);
    if not done then index:=index+1;
    end;
end;

function TDayNine.findIndexOfFirstLargeEnoughSpace(input: TInt64Array;
  size,endAt: int64): int64;
var
  index,currentSpaceLength:int64;
begin
  result:=-1;
  currentSpaceLength:=0;
  for index:=input.indexOf(-1) to endAt do
    begin
    if (input[index]= -1) then
      begin
      if (result = -1) then result:=index;
      currentSpaceLength:=currentSpaceLength + 1;
      if (currentSpaceLength = size) then exit;
      end else
      begin
      result:=-1;
      currentSpaceLength:=0;
      end;
    end;
end;

function TDayNine.moveFile(value: TInt64Array; fileIndex, fileLength,
  spaceIndex: integer): TInt64Array;
var
  index,offset:int64;
begin
  result:=value;
  offset:=0;
  for index:=fileIndex to (fileIndex+fileLength - 1) do
    begin
    result[spaceIndex+offset]:=result[index];
    result[index]:=-1;
    offset:=offset+1;
    end;
  results.add(result.toString(','));
end;

function TDayNine.noSpacesBeforeStartPoint(input: TInt64Array; startsAt: int64
  ): boolean;
begin
  result:= (input.indexOf(-1) = -1)or (input.indexOf(-1) > startsAt);
end;

constructor TDayNine.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 9',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayNine.runPartOne;
var
  blockArrangement:TInt64Array;
  defragged:TInt64Array;
begin
  results.Clear;
  //Here we want puzzleInput - a single string
  //lets generate another string with the actual positions
  blockArrangement:=generateBlockArrangement(puzzleInput.Trim);
  defragged:=defrag(blockArrangement);
  results.Add(getChecksum(defragged).ToString);
end;

procedure TDayNine.runPartTwo;
var
  blockArrangement:TInt64Array;
  defragged:TInt64Array;
begin
  results.Clear;
  //same but move whole files
  blockArrangement:=generateBlockArrangement(puzzleInput.trim);
  defragged:=defragFiles(blockArrangement);
  results.add('result '+getChecksum(defragged).ToString);
end;


end.

                
