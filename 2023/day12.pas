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
  function arrangementIsLegal(data_:string;groupInfo,positions:TIntArray):boolean;
  function updateGroupOffsets(var offsets:TIntArray; groups: TIntArray;dataLength:integer):boolean;
  function outOfRange(offsets,groups:TIntArray;offsetId,dataLength:integer):boolean;
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
  groupOffsets,currentGroupPositions:TIntArray;
  index,groupSize,groupOffset,startPosition,groupPosition:integer;
  totalGroupSize,dataLength:integer;
  allPositionsTried:boolean;
begin
  result:=0;
  //.??..??...?##. 1,1,3 (for reference)
  groups:=groups_.Split([',']).toIntArray;
  groupOffsets:=TintArray.create;
  currentGroupPositions:=TIntArray.create;
  totalGroupSize:=0;
  dataLength:=data_.Length;
  //Set up offsets for each group
  for index:=0 to pred(groups.size) do
    begin
    groupOffsets.push(0);
    totalGroupSize:=totalGroupSize + groups[index];
    if (index < pred(groups.size)) then totalGroupSize:=totalGroupSize+1;
    end;
  allPositionsTried:=false;

  while not allPositionsTried do
  begin
  //Set up legal arrangement
  //Starting at the left hand side, how far left can we fit the first item?
  startPosition:=0;
  setLength(currentGroupPositions,0); //clear method would be nice here
  for index:=0 to pred(groups.size)do
    begin
    groupSize:=groups[index];
    groupOffset:=groupOffsets[index];
    groupPosition:=findFirstPositionForGroup(data_,groupSize,startPosition + groupOffset);
    //If we can't place the group then it's not valid
    if (groupPosition > -1) then
      begin
      currentGroupPositions.push(groupPosition);
      startPosition:=groupPosition+groupSize+1;
      end else break;
    end;
  if arrangementIsLegal(data_, groups,currentGroupPositions) then result:=result + 1;
  //how do we determine that we're done?
  //If the offset of the first item + the total length of the groups is > length of data
  allPositionsTried:= (groupOffsets[0] + totalGroupSize > dataLength);
  if not allPositionsTried then updateGroupOffsets(groupOffsets,groups,dataLength);
  end;

end;

function TDayTwelve.findFirstPositionForGroup(data_: string; groupSize,
  startPosition: integer): integer;
const legalBoundingChars: TStringArray = ('?','.');
var
   index:integer;
   done,enoughSpace,leftLegal,rightLegal,spaceLegal:boolean;
begin
  result:=-1;
  index:=startPosition;
  done:=false;
  while not done do
    begin
    enoughSpace:=index + groupSize <= data_.Length;
    leftLegal:=((index = 0) or (legalBoundingChars.indexOf(data_.Substring(index-1,1))> -1));
    rightLegal:=((index + groupSize = data_.Length)or(legalBoundingChars.indexOf(data_.Substring(index+groupSize,1))>-1));
    spaceLegal:=(data_.Substring(index,groupSize).IndexOf('.') = -1);
    done:= enoughSpace and leftLegal and rightLegal and spaceLegal;
    if not done then
      begin
      if (index < data_.Length) then index:=index + 1 else exit;
      end;
    end;
  result:=index;
end;

function TDayTwelve.arrangementIsLegal(data_: string; groupInfo,positions: TIntArray
  ): boolean;
begin
  result:=false;
  if (positions.size <> groupInfo.size) then exit;
  //Check where specified groupInfo would result in groups being
  //and see if it would result in an illegal arrangement
end;

function TDayTwelve.updateGroupOffsets(var offsets:TIntArray; groups: TIntArray;
  dataLength: integer):boolean;
var
   offsetId,startOfGroup:integer;
   index:integer;
   done,outRange:boolean;
begin
  offsetId:=pred(offsets.size);
  //Repeat this until we find an offset we can update
  //or until we can't move the first one
  done:=false;
  repeat
  index:=0;
  startOfGroup:=0;
  while index <= offsetId do
    begin
    startOfGroup:=startOfGroup + offsets[index];
    if index < offsetId then startOfGroup:=startOfGroup + groups[index]+ 1;
    index:=index + 1;
    end;
  outRange:=outOfRange(offsets, groups,offsetId,dataLength);
  //If incrementing the current offset would not make us out of range then we can do it
  If outRange then offsetId:= offsetId -1
    else offsets[offsetId]:=offsets[offsetId]+1;
  done:= (not outRange) or(offsetId < 0);
  until done;
  //If we're done then either offsetId is < 0 or we're able to update the offset
  result:= not outRange;
end;

function TDayTwelve.outOfRange(offsets, groups: TIntArray; offsetId, dataLength: integer
  ): boolean;
var
   index:integer;
   totalLength:integer;
begin
  //takes a set of offsets and works out if the total length is too long
  //add all the offsets and groups. If we're on the offsetId that is to be incremented then add one
  totalLength:=0;
  for index:=0 to pred(offsets.size) do
    begin
    totalLength:=totalLength + offsets[index] + groups[index] -1;
    if (index < pred(offsets.size)) then totalLength:=totalLength + 1;
    if index = offsetId then totalLength:=totalLength+1;
    end;
  result:=totalLength > dataLength;
end;

end.


