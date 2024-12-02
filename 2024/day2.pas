unit day2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwo}
  TDayTwo = class(TAocPuzzle)
  private
  function reportIsSafe(report: string):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwo }

function TDayTwo.reportIsSafe(report: string): boolean;
var
  index:integer;
  elements:TStringArray;
  asc:boolean;
  current,next,diff:integer;
begin
  result:=true;
  elements:=report.Split(' ',TStringSplitOptions.ExcludeEmpty);
  asc:=elements[1].toInteger > elements[0].toInteger;
  for index:= 0 to pred(elements.size) do
    begin
    current:=elements[index].toInteger;
    if (index < pred(elements.size)) then
      begin
      next:= elements[index+1].toInteger;
      diff:= next - current;
      if (asc and ((diff > 3) or (diff < 1)))
      or (not asc and ((diff < -3) or (diff > -1))) then
        begin
        result:=false;
        exit;
        end;
      end;
    end;
end;

constructor TDayTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 2',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwo.runPartOne;
var
  index, safeReports:integer;
begin
  results.Clear;
  safeReports:=0;
  for index:=0 to pred(puzzleInputLines.size) do
    if reportIsSafe(puzzleInputLines[index]) then safeReports:= safeReports + 1;
  results.add('Safe reports '+safeReports.toString);
end;

procedure TDayTwo.runPartTwo;
begin
  results.Clear;
end;


end.

                
