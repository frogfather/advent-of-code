unit day6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySix}
  TDaySix = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDaySix }

constructor TDaySix.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 6',paintbox_);
//parent loads the file as a string and converts to string array;
end;

//Time     x x x
//distance y y y
procedure TDaySix.runPartOne;
var
  times,distances:TStringArray;
  raceCount,raceNo,availableTime, recordDistance,timeHeld,speed,distanceTravelled:integer;
  beatRecordCount,totalBeatRecordCount:integer;
begin
  results.Clear;
  //Total time = t
  //Time held = x
  //Time running = t - x
  //speed = x
  //get the puzzle input
  times:=puzzleInputLines[0].Split([' '],(TstringSplitOptions.ExcludeEmpty));
  distances:=puzzleInputLines[1].Split([' '],(TstringSplitOptions.ExcludeEmpty));
  raceCount:=times.size - 1;
  totalBeatRecordCount:=0;
  for raceNo:=1 to raceCount do
    begin
    beatRecordCount:=0;
    availableTime:=times[raceNo].ToInteger;
    recordDistance:=distances[raceno].ToInteger;
    for timeHeld:=1 to availableTime - 1 do
      begin
      speed:=timeHeld;
      distanceTravelled:= (availableTime - timeHeld) * speed;
      results.add('Race: '+raceNo.ToString+' - Speed: '+speed.ToString+', distance: '+distanceTravelled.ToString);
      if (distanceTravelled > recordDistance) then beatRecordCount:=beatRecordCount+1;
      end;
    if (beatRecordCount = 0) then break; //Don't multiply by zero!
    if (totalBeatRecordCount = 0) then totalBeatRecordCount:=beatRecordCount
    else totalBeatRecordCount:=totalBeatRecordCount * beatRecordCount;
    end;
  results.add('The answer is '+totalbeatrecordCount.ToString);
end;

procedure TDaySix.runPartTwo;
begin
  results.Clear;
end;

end.


