unit day3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils;
type

  { TDayThree}
  TDayThree = class(TAocPuzzle)
  private
  function findIndexOfFirstOp(input:string; start:integer=0):integer;
  function findNextValidEntry(input: string; start:integer):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }
var
  currentIndex:integer;
  runningTotal:integer;

function TDayThree.findIndexOfFirstOp(input: string; start:integer=0): integer;
begin
  result:=input.IndexOf('mul(',start);
end;

function TDayThree.findNextValidEntry(input: string; start: integer): boolean;
var
  indexOfOpeningBracket:integer;
  candidate:string;
  parts:TStringArray;
  closingBrIndex:integer;
begin
  //start at the specified index
  result:=false;
  indexOfOpeningBracket:=findIndexOfFirstOp(input,start);
  if (indexOfOpeningBracket = -1) or (indexOfOpeningBracket > (input.Length - 8)) then exit;
  currentIndex:=indexOfOpeningBracket;
  closingBrIndex:=input.IndexOf(')',indexOfOpeningBracket);
  if (closingBrIndex = -1) then exit;
  candidate:=input.Substring(indexOfOpeningBracket+4, closingBrIndex - (indexOfOpeningBracket+4));
  parts:=candidate.Split(',');
  if(parts.size <> 2) then
    begin
    results.add('parts '+parts.size.toString);
    currentIndex:=currentIndex + 1;
    exit;
    end;
  if isNumberString(parts[0]) and isNumberString(parts[1]) then
    begin
    runningTotal:=runningTotal + (parts[0].toInteger * parts[1].toInteger);
    currentIndex:=closingBrIndex + 1;
    end else currentIndex:= currentIndex + 1;
end;

constructor TDayThree.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 3',paintbox_);
currentIndex:=0;
runningTotal:=0;
//parent loads the file as a string and converts to string array;
end;

procedure TDayThree.runPartOne;
var
  done:boolean;
  refIndex:integer;
begin
  results.Clear;
  done:=false;
  while not done do
    begin
    refIndex:=currentIndex;
    if currentIndex < puzzleInput.Length then findNextValidEntry(puzzleInput,currentIndex);
    done:= (refIndex=currentIndex);
    end;
  results.add(runningTotal.toString);
end;

procedure TDayThree.runPartTwo;
begin
  results.Clear;
end;


end.

                
