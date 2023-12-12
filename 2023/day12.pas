unit day12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwelve}
  TDayTwelve = class(TAocPuzzle)
  private
  procedure setupSpringData;
  procedure reset;
  function getPermutationsForEntry(data_,groups_:string):integer;
  function findFirstPositionForGroup(data_:string;groupSize,startPosition:integer):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  springData:TStringArray;
  brokenSpringGroups: TStringArray;

{ TDayTwelve }

constructor TDayTwelve.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 12',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwelve.runPartOne;
var
  lineNo:integer;
  springDataEntry:string;
  brokenSpringGroupEntry:string;
  totalPermutations:integer;
begin
  results.Clear;
  //Separate each line into data and blocks of broken springs
  //Start with all options as far left as they'll go
  //examine positions for rhs option
  //then move second rh option to next available space and repeat
  //until no more space
  setupSpringData;
  //now for each entry in spring data get all possible permutations
  totalPermutations:=0;
  for lineNo:= 0 to pred(springData.size) do
    begin
    springDataEntry:=springData[lineNo];
    brokenSpringGroupEntry:= brokenSpringGroups[lineNo];
    totalPermutations:= totalPermutations + getPermutationsForEntry(springDataEntry,brokenSpringGroupEntry)
    end;
end;

procedure TDayTwelve.runPartTwo;
begin
  results.Clear;
end;

procedure TDayTwelve.setupSpringData;
var
  index:integer;
  parts:TStringArray;
begin
springData:=TStringArray.create;
brokenSpringGroups:=TStringArray.create;
for index:= 0 to pred(puzzleInputLines.size) do
  begin
  parts:=puzzleInputLines[index].Split([' ']);
  springData.push(parts[0]);
  brokenSpringGroups.push(parts[1]);
  end;
end;

procedure TDayTwelve.reset;
begin
  setLength(springData,0);
  setLength(brokenSpringGroups,0);
end;

function TDayTwelve.getPermutationsForEntry(data_, groups_: string): integer;
var
  groups:TIntArray;
  layout,initialLayout:string;
  layoutIsLegal:boolean;
  groupOffsets:TIntArray;
  index,groupSize,startPosition,groupPosition:integer;
begin
  //.??..??...?##. 1,1,3 (for reference)
  groups:=groups_.Split([',']).toIntArray;
  groupOffsets:=TintArray.create;
  //Set up offsets for each group
  for index:=0 to pred(groups.size) do
    groupOffsets.push(0);
  //Set up legal arrangement
  //Starting at the left hand side, how far left can we fit the first item?
  startPosition:=0;
  for index:=0 to pred(groups.size)do
    begin
    groupSize:=groups[index];
    //we need enough space to fit this group
    //where the elements are ? or #
    groupPosition:=findFirstPositionForGroup(data_,groupSize,startPosition);
    startPosition:=groupPosition+groupSize+1;
    end;

  result:=0;
end;

function TDayTwelve.findFirstPositionForGroup(data_: string; groupSize,
  startPosition: integer): integer;
const legalBoundingChars: TStringArray = ('?','.');
var
   index:integer;
   done,enoughSpace,leftLegal,rightLegal,spaceLegal:boolean;
begin
  result:=-1;
  //given the input data what is the first position we can place a group
  //of this size?
  //start must be # or ?
  //end must be ? or .
  index:=startPosition;
  done:=false;
  results.add('Checking for legal position for space of size '+groupSize.toString+' in '+data_+' at position '+startPosition.ToString);
  while not done do
    begin
    enoughSpace:=index + groupSize <= data_.Length;
    leftLegal:=((index = 0) or (legalBoundingChars.indexOf(data_.Substring(index-1,1))> -1));
    rightLegal:=((index + groupSize = data_.Length)or(legalBoundingChars.indexOf(data_.Substring(index+groupSize,1))>-1));
    spaceLegal:=(data_.Substring(index,groupSize).IndexOf('.') = -1);
    results.add('at index '+index.ToString+' enough space '+enoughSpace.toString+' left legal '+leftlegal.ToString+' right legal '+rightLegal.toString+' space legal '+spacelegal.toString);
    done:= enoughSpace and leftLegal and rightLegal and spaceLegal;
    if not done then
      begin
      if (index < data_.Length) then index:=index + 1 else exit;
      end;
    end;
  result:=index;
  results.add('found legal position '+result.ToString);
end;

end.


