unit day9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNine}
  TDayNine = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  map:T2DInt64Map;
  history:TInt64Array;
{ TDayNine }

constructor TDayNine.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 9',paintbox_);
//parent loads the file as a string and converts to string array;
map:=T2DInt64Map.create;
history:=TInt64Array.create;
end;

procedure TDayNine.runPartOne;
var
  lineNo:integer;
  mapIndex,mapLineIndex,partIndex,historyIndex:integer;
  parts:TStringArray;
  allResultsZero:boolean;
  difference:Int64;
  historySum:int64;
begin
  results.Clear;
  setLength(history,0);
  //clear the map for each line. Record the results elsewhere
  for lineNo:= 0 to pred(puzzleInputLines.size) do
    begin
    map.clear;
    //Split into individual numbers and add to the array;
    mapIndex:=0;
    parts:=puzzleInputLines[lineNo].Split([' '],(TstringSplitOptions.ExcludeEmpty));
    for partIndex:=0 to pred(parts.size)do
      map.push(mapIndex,parts[partIndex].ToInt64);
    //Now calculate the differences until they are all zeros
    repeat
    allResultsZero:=true;
    for mapLineIndex:=1 to pred(map.size(mapIndex)) do
      begin
      difference:=map[mapIndex][mapLineIndex]-map[mapIndex][mapLineIndex - 1];
      if difference <> 0 then allresultsZero:=false;
      map.push(mapIndex+1,difference);
      end;
    mapIndex:=mapIndex+1;
    until allResultsZero;
    map.push(mapIndex,0);
    //Now calculate the history value
    historySum:=0;
    for mapIndex:=pred(map.rows) downTo 0 do
      historySum:=historySum+map.getLast(mapIndex);
    setLength(history,length(history)+1);
    history[length(history)-1]:=historySum
    end;
  //Now add all the entries in history
  historySum:=0;
  for historyIndex:=0 to pred(length(history)) do
    historySum:=historySum + history[historyIndex];
  results.add('The sum is '+historySum.ToString);
end;

procedure TDayNine.runPartTwo;
begin
  results.Clear;
end;

end.


