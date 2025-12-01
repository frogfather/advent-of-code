unit day19;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,math;
type

  { TDayNineteen}
  TDayNineteen = class(TAocPuzzle)
  private
  function canMakeDesign(design:String):boolean;
  procedure loadPatterns;
  function findPossibleDesigns:TStringArray;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  patterns:TStringArray;
  cache:TStringArray;
  maxPatternLength:integer;

{ TDayNineteen }

function TDayNineteen.canMakeDesign(design: String): boolean;
var
  index:integer;
  sUpTo,sAfter:string;
begin
  if (design = '') or (cache.IndexOf(design) > -1) then
    begin
    result:=true;
    exit;
    end;
  for index:=0 to pred(min(design.Length,maxPatternLength)) do
    begin
    sUpTo:=design.Substring(0,index+1)
    sAfter:=design.Substring(index+1);
    if (patterns.indexOf(sUpTo) > -1) and (canMakeDesign(sAfter)) then
      begin
      cache.push(sUpTo);
      result:=true;
      exit;
      end;
    end;
  result:=false;
end;

procedure TDayNineteen.loadPatterns;
var
  index:integer;
begin
  patterns:=puzzleInputLines[0].Split(',',TStringSplitOptions.ExcludeEmpty);
  maxPatternLength:=0;
  for index:=0 to pred(patterns.size) do
    if (patterns[index].Length > maxPatternLength)
      then maxPatternLength:=patterns[index].Length;
end;

function TDayNineteen.findPossibleDesigns: TStringArray;
var
  index:integer;
begin
  result:=TStringArray.create;
  for index:=2 to pred(puzzleInputLines.size) do
    if canMakeDesign(puzzleInputLines[index])
      then result.push(puzzleInputLines[index]);
end;

constructor TDayNineteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 19',paintbox_);
cache:=TStringArray.Create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayNineteen.runPartOne;
begin
  results.Clear;
  cache.clear;
  loadPatterns;
  findPossibleDesigns;


end;

procedure TDayNineteen.runPartTwo;
begin
  results.Clear;
  loadPatterns
end;


end.

                
