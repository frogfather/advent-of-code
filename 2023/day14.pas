unit day14;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils;
type

  { TDayFourteen}
  TDayFourteen = class(TAocPuzzle)
  private
  procedure loadPlatformData;
  procedure runCycle;
  procedure rollRocks;
  procedure printRocks;
  function totalLoad:integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  platform: TStringArray;

{ TDayFourteen }

constructor TDayFourteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 14',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFourteen.runPartOne;
var
  columnId,total:integer;
begin
  results.Clear;
  platform:=TStringArray.create;
  loadPlatformData;
  rollRocks;
  results.add('Total is '+totalLoad.ToString);
end;

//Solution reached with a lot of help from HyperNeutrino's video solution
//https://youtu.be/WCVOBKUNc38?si=1_uKYqfncX4FIqXH
procedure TDayFourteen.runPartTwo;
var
  cycleNo: int64;
  stateOfMap:string;
  previousStates:TStringIntMap;
  previousMapStateFound,repeatFound,done:boolean;
  indexOfState:integer;
  cycleRepeat:integer;
begin
  results.Clear;
  previousStates:=TStringIntMap.Create;
  platform:=TStringArray.create;
  loadPlatformData;
  cycleNo:=0;
  previousMapStateFound:=false;
  done:=false;
  repeatFound:=false;
  while not done do
    begin
    runCycle;
    stateOfMap:=platform.toString('');
    previousMapStateFound:= previousStates.TryGetData(stateOfMap,indexOfState);
    if not previousMapStateFound then
      previousStates.Add(stateOfMap,cycleNo) else
    if not repeatFound then
      begin
      cycleRepeat:=cycleNo - previousStates.Data[indexOfState];
      repeatFound:=true;
      cycleNo:= cycleNo + ((1000000000 - (indexOfState + cycleRepeat)) div cycleRepeat) * cycleRepeat;
      end;
    cycleNo:=cycleNo+1;
    done:=(cycleNo >= 1000000000);
    end;
  results.add('Total is '+totalLoad.ToString);
end;

procedure TDayFourteen.loadPlatformData;
var
  lineNo:integer;
begin
  for lineNo:=0 to pred(puzzleInputLines.size) do
    platform.push(puzzleInputLines[lineNo]);
end;

procedure TDayFourteen.rollRocks;
var
  nextFreeSpace:integer;
  x,y:integer;
  element:string;
begin
  //move all rocks as far towards y=0 as possible
  for x:=0 to pred(platform[0].Length) do
    begin
    nextFreeSpace:=0;
    for y:=0 to pred(platform.size) do
      begin  //Strings are 1 indexed!
      element:=platform[y][x+1];
      if (element = 'O') then
        begin
        if (y > nextFreeSpace) then
          begin
          platform[nextFreeSpace][x+1]:=element[1];
          platform[y][x+1]:='.';
          end;
        nextFreeSpace:=nextFreeSpace+1;
        end else
      if (element = '#') then nextFreeSpace:=y+1;
      end;
    end;
end;

procedure TDayFourteen.printRocks;
var
    x:integer;
  begin
  for x:=0 to pred(platform.size) do
    results.add(platform[x]);
  end;

function TDayFourteen.totalLoad: integer;
var
columnId,rowId:integer;
begin
  result:=0;
  for rowId:=0 to pred(platform.size) do
    for columnId:=0 to pred(platform[0].Length) do
      begin
      if (platform[rowId].Substring(columnId,1) = 'O')
      then result:=result+(platform.size - rowId);
      end;
end;

procedure TDayFourteen.runCycle;
begin
  rollRocks;  //N
  platform:=platform.rotate(1,false);
  rollRocks; //W
  platform:=platform.rotate(1,false);
  rollRocks; //S
  platform:=platform.rotate(1,false);
  rollRocks; //E
  platform:=platform.rotate(1,false);
end;

end.


