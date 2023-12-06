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
var
  availableTime,winningDistance:int64;
  firstTimeThatWins,lastTimeThatWins:int64;
  segmentStart,segmentEnd:int64;
  done,startWins,endWins:boolean;
  loopCount:integer;

   function timeWins(time:int64):boolean;
   begin
     result:=((availableTime - time)*time > winningDistance);
   end;

begin
  results.Clear;
  availableTime:=puzzleInputlines[0].split([':'],(TstringSplitOptions.ExcludeEmpty))[1].Replace(' ','').Trim.ToInt64;
  winningDistance:=puzzleInputlines[1].split([':'],(TstringSplitOptions.ExcludeEmpty))[1].Replace(' ','').Trim.ToInt64;
  results.add('Distance: '+winningDistance.ToString+' time: '+availableTime.ToString);
  firstTimeThatWins:=0;
  lastTimeThatWins:=0;
  //Find first winning availableTime
  done:=false;
  loopCount:=0;
  segmentStart:=0;
  segmentEnd:=availableTime div 2;
  repeat
  //keep dividing until both times win
  startWins:=timeWins(segmentStart);
  endWins:=timeWins(segmentEnd);

  results.Add('start: '+segmentStart.ToString+' win? '+startWins.ToString+' end: '+segmentEnd.ToString+' win? '+endWins.ToString);
  if (startWins and endWins) then
    begin
    //move to previous segment if possible otherwise we're done
    if (segmentEnd - segmentStart > 1) then
      begin
      segmentEnd:=segmentStart - 1;
      segmentStart:=segmentEnd div 2;
      end else
      begin
      firstTimeThatWins:=segmentStart;
      done:=true;
      end;
    end else
    if ((not startWins) and endWins )then
    begin
    //split in two and move to the higher segment
    if (segmentEnd - segmentStart > 1) then
      begin
      segmentStart:=segmentEnd - ((segmentEnd - segmentStart) div 2);
      end else
      begin
      firstTimeThatWins:=segmentEnd;
      done:=true;
      end;
    end else
    if ((not endWins) and startWins) then
      begin
      //move to previous segment if possible
      if (segmentStart > 0) then
        begin
        segmentEnd:=segmentStart - 1;
        segmentStart:=segmentEnd div 2;
        end else
        begin
        firstTimeThatWins:=segmentStart;
        done:=true;
        end;
      end;
    results.Add('Start now '+segmentStart.ToString+' end now '+segmentEnd.ToString);
    loopCount:=loopCount+1;
    if (loopCount = 1000) then
      begin
      results.add('loop count exceeded');
      exit;
      end;
  until done;

  //Now much the same for the end time
  done:=false;
  loopCount:=0;
  segmentStart:=availableTime div 2;
  segmentEnd:=availableTime;
  repeat
  //keep dividing until both times win
  startWins:=timeWins(segmentStart);
  endWins:=timeWins(segmentEnd);

  results.Add('start: '+segmentStart.ToString+' win? '+startWins.ToString+' end: '+segmentEnd.ToString+' win? '+endWins.ToString);
  if (startWins and endWins) then
    begin
    //Unless the segment width is very small we should move to the next segment
    if (segmentEnd - segmentStart > 1) then
      begin
      segmentStart:=segmentEnd+1;
      segmentEnd:= availableTime;
      end else
      begin
      lastTimeThatWins:=segmentEnd;
      done:=true;
      end;
    end else
    if ((not startWins) and endWins )then
    begin
    //move to the next segment if possible
    if (segmentEnd < availableTime) then
      begin
      segmentStart:=segmentEnd+1;
      segmentEnd:=segmentStart+(availableTime-segmentStart)div 2;
      end else
      begin
      lastTimeThatWins:=segmentEnd;
      done:=true;
      end;
    end else
    if ((not endWins) and startWins) then
      begin
      //half segment
      if (segmentEnd - SegmentStart > 1) then
        begin
        segmentEnd:=segmentStart+((SegmentEnd - segmentStart) div 2)
        end else
        begin
        lastTimeThatWins:=segmentStart;
        done:=true;
        end;
      end;
    results.Add('Start now '+segmentStart.ToString+' end now '+segmentEnd.ToString);
    loopCount:=loopCount+1;
    if (loopCount = 1000) then
      begin
      results.add('loop count exceeded');
      exit;
      end;
  until done;
end;

end.


