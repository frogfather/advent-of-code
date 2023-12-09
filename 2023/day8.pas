unit day8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils;
type

  { TDayEight}
  TDayEight = class(TAocPuzzle)
  private
  procedure loadMap;
  function getSteps(start_:string;directions:string;endOnLastz:boolean = false):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var map: TStringMap;
{ TDayEight }

constructor TDayEight.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 8',paintbox_);
//parent loads the file as a string and converts to string array;
map:=TStringMap.Create;
end;

procedure TDayEight.runPartOne;
var
  directions:string;
begin
  results.Clear;
  directions:=puzzleInputLines[0];
  loadMap;
  results.add('steps taken: '+getSteps('AAA',directions).ToString);
end;

procedure TDayEight.runPartTwo;
var
  stepsForPaths:TInt64Array;
  mapPosition:integer;
  directions:string;
  runningTotal:int64;
  index:integer;
begin
  stepsForPaths:=TInt64Array.create;
  results.Clear;
  directions:=puzzleInputLines[0];
  loadMap;
  //set up list of starting positions
  for mapPosition:=0 to pred(map.Count) do
    if (map.Keys[mapPosition].Substring(2,1) = 'A') then
      stepsForPaths.push(getSteps(map.Keys[mapPosition],directions,true));
  //Now find lowest common multiple for these entries
  //find lcd of first 2 - thats running total
  //the lcd of running total and next entry
  runningTotal:=0;
  for index:=0 to pred(stepsForPaths.size) do
    runningTotal:=lcm(runningtotal,stepsForPaths[index]);
  results.add('Steps taken is '+runningTotal.ToString);
end;

procedure TDayEight.loadMap;
var
  lineNo:integer;
  parts:TStringArray;
begin
  map.Clear;
  for lineNo:= 1 to pred(puzzleInputLines.size) do
    if (puzzleInputLines[lineNo].Trim.Length > 0) then
      begin
      parts:=puzzleInputLines[lineNo].Split(['='],(TstringSplitOptions.ExcludeEmpty));
      map.AddOrSetData(parts[0].Trim,parts[1].trim.Replace('(','').Replace(')',''));
      end;
end;

function TDayEight.getSteps(start_: string;directions:string;endOnLastz:boolean): int64;
var
  mapPosition:integer;
  direction:string;
  nextInstruction:string;
  stepCount:int64;
  destFound:boolean;
begin
  mapPosition:=map.IndexOf(start_);
  destFound:=false;
  stepCount:=0;
  while not destFound do
    begin
    direction:=directions.Substring(stepCount mod directions.Length,1);
    if (direction = 'L') then nextInstruction:= map.Data[mapPosition].Split(',')[0].Trim
    else nextInstruction:= map.Data[mapPosition].Split(',')[1].Trim;
    stepCount:=stepCount+1;
    if endonLastZ then destFound:= (nextInstruction.Substring(2,1) = 'Z')
      else destFound:= (nextInstruction = 'ZZZ');
    if destFound then break;
    mapPosition:= map.IndexOf(nextInstruction);
    end;
  result:=stepCount;
end;



end.


