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
  function findNextValidEntry(input: string; start:integer; includeSwitches:boolean=false):boolean;
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

function TDayThree.findNextValidEntry(input: string; start: integer; includeSwitches:boolean=false): boolean;
var
  indexOfMul:integer;
  candidate:string;
  parts:TStringArray;
  closingBrIndex:integer;
  distanceToDo,distanceToDont:integer;
  indexOfDo,indexOfDont:integer;
  doActive:boolean;
begin
  //start at the specified index
  result:=false;
  indexOfMul:=findIndexOfFirstOp(input,start);
  indexOfDo:=input.lastIndexOf('do()',indexOfMul);
  indexOfDont:=input.lastIndexOf('don''t()',indexOfMul);
  //How far are the nearest do() and don't() looking back from this sum?
  distanceToDo:=indexOfMul - indexOfDo;
  distanceToDont:= indexOfMul - indexOfDont;
  doActive:= (distanceToDo <= distanceToDont);

  if (indexOfMul = -1) or (indexOfMul > (input.Length - 8)) then exit;

  currentIndex:=indexOfMul;
  closingBrIndex:=input.IndexOf(')',indexOfMul);
  if (closingBrIndex = -1) then exit;

  candidate:=input.Substring(indexOfMul+4, closingBrIndex - (indexOfMul+4));
  parts:=candidate.Split(',');
  if(parts.size <> 2) then
    begin
    currentIndex:=currentIndex + 1;
    exit;
    end;
  if isNumberString(parts[0]) and isNumberString(parts[1]) then
    begin
    if (not includeSwitches) or doActive then
       runningTotal:=runningTotal + (parts[0].toInteger * parts[1].toInteger);
    currentIndex:=currentIndex + 1;
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
var
  done:boolean;
  refIndex:integer;
begin
  results.Clear;
  done:=false;
  while not done do
    begin
    refIndex:=currentIndex;
    if currentIndex < puzzleInput.Length then findNextValidEntry(puzzleInput,currentIndex, true);
    done:= (refIndex=currentIndex);
    end;
  results.add(runningTotal.toString);
end;


end.

                
