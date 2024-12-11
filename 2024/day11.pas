unit day11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,int64Cache;
type

  { TDayEleven}
  TDayEleven = class(TAocPuzzle)
  private
  function countStones(stone,steps:int64):int64;
  function countStonesTimes(stones:TInt64Array;times:integer):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayEleven }
var
  cache:TInt64Cache;

function TDayEleven.countStones(stone, steps: int64): int64;
var
  str,strLeftStone,strRightStone:string;
  lengthOfString,leftStone,rightStone:int64;
  cacheEntry:TInt64CacheRecord;
begin
  result:=1;
  if (steps = 0) then exit;
  if (cache.indexOf(stone,steps) > -1) then
    begin
    result:=cache.outputOf(stone,steps);
    end else
    begin
    if (stone = 0) then
       result:= countStones(1, steps - 1)
    else
      begin
      str:=stone.ToString;
      lengthOfString:=str.Length;
      if (lengthOfString mod 2 = 0) then
        begin
        strLeftStone:=str.Substring(0, lengthOfString div 2);
        strRightStone:=str.Substring(lengthOfString div 2);
        leftStone:=strLeftStone.ToInt64;
        rightStone:=strRightStone.ToInt64;
        result:=countStones(leftStone,steps-1) + countStones(rightStone,steps-1);
        end
      else result:= countStones(stone * 2024, steps -1);
      end;
     //add the result to the cache
     cacheEntry.input1:=stone;
     cacheEntry.input2:=steps;
     cacheEntry.output:=result;
     cache.push(cacheEntry);
    end;

end;

function TDayEleven.countStonesTimes(stones: TInt64Array; times: integer
  ): int64;
var
  index:integer;
begin
  result:=0;
  for index:=0 to pred(stones.size) do
    result:=result+countStones(stones[index],times);
end;

constructor TDayEleven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 11',paintbox_);
cache:=TInt64Cache.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayEleven.runPartOne;
var
  stones:TInt64array;
begin
  results.Clear;
  cache.clear;
  stones:=puzzleInput.Substring(0,puzzleInput.IndexOf(#$0A)).Split(' ',TStringSplitOptions.ExcludeLastEmpty).toInt64Array;
  results.add('Total is '+countStonesTimes(stones,25).ToString);
end;

procedure TDayEleven.runPartTwo;
var
  stones:TInt64array;
begin
  results.Clear;
  cache.clear;
  stones:=puzzleInput.Substring(0,puzzleInput.IndexOf(#$0A)).Split(' ',TStringSplitOptions.ExcludeLastEmpty).toInt64Array;
  results.add('Total is '+countStonesTimes(stones,75).ToString);
end;


end.

                
