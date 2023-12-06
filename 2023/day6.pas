unit day6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySix}
  TDaySix = class(TAocPuzzle)
  private
  function getFirstWinningTime(availableTime,distance:int64;fromEnd:boolean=false):int64;
  function timeWins(availableTime,time,winningDistance:int64):boolean;
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
var
  availableTime,winningDistance:int64;
  firstTimeThatWins,lastTimeThatWins:int64;
begin
  results.Clear;
  availableTime:=puzzleInputlines[0].split([':'],(TstringSplitOptions.ExcludeEmpty))[1].Replace(' ','').Trim.ToInt64;
  winningDistance:=puzzleInputlines[1].split([':'],(TstringSplitOptions.ExcludeEmpty))[1].Replace(' ','').Trim.ToInt64;
  results.add('Available time '+availableTime.ToString);
  results.add('winning distance '+winningDistance.ToString);
  firstTimeThatWins:=getFirstWinningTime(availableTime,winningDistance);
  lastTimeThatwins:=getFirstWinningTime(availableTime,winningDistance,true);
  results.add('First winning time '+firstTimeThatWins.ToString);
  results.add('Last winning time '+lastTimeThatWins.toString);
  results.add('Winning times '+(lastTimeThatWins - firstTimeThatWins +1).toString);
end;

function TDaySix.getFirstWinningTime(availableTime, distance: int64;
  fromEnd: boolean): int64;
var
  direction,directionIfWon:integer;
  interval,currentTime:int64;
  adjusted:boolean;
begin
  //Start in the middle regardless
  currentTime:=availableTime div 2;
  interval:=availableTime div 2;
  if fromEnd then direction:=1 else direction:=-1;
    repeat
    if timeWins(availableTime,currentTime,distance) then directionIfWon:=1
    else directionIfWon:=-1;
    interval:=interval div 2;
    currentTime:=currentTime + (direction * directionIfWon * interval);
    until interval = 1;
  //This gets us pretty close, but need to adjust to find exact value
  adjusted:=false;
  repeat
  adjusted:= timeWins(availableTime,currentTime,distance);
  if not adjusted then currentTime:=currentTime - direction;
  until adjusted;

  result:=currentTime;
end;

function TDaySix.timeWins(availableTime,time,winningDistance:int64): boolean;
begin
  result:=((availableTime - time)*time > winningDistance);
end;

end.


