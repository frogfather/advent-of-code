unit day12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwelve}
  TDayTwelve = class(TAocPuzzle)
  protected
  procedure setupSpringData;
  procedure reset;
  function getPermutationsForEntry(data_,groups_:string):integer;
  function findFirstPositionForGroup(data_:string;groupSize,startPosition:integer):integer;
  function arrangementIsLegal(data_:string;groupInfo,positions,groupOffsets:TIntArray):boolean;
  function updateGroupOffsets(var offsets:TIntArray; groups: TIntArray;dataLength:integer):boolean;
  function sequenceIsTooLong(offsets,groups:TIntArray;offsetId,dataLength:integer):boolean;
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
  results.Add('Get permutations for '+data_);
  result:=0;
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
  //Set up start positions at start
  startPosition:=0;
  setLength(currentGroupPositions,0); //clear method would be nice here
  for index:=0 to pred(groups.size)do
    begin
    groupSize:=groups[index];
    groupOffset:=groupOffsets[index];//initially 0
    groupPosition:=findFirstPositionForGroup(data_,groupSize,startPosition + groupOffset);
    if (groupPosition > -1) then
      begin
      currentGroupPositions.push(groupPosition);
      startPosition:=groupPosition+groupSize+1;
      end else break;
    end;

  allPositionsTried:=false;

  while not allPositionsTried do
  begin
  if arrangementIsLegal(data_, groups,currentGroupPositions,groupOffsets) then result:=result + 1;
  allPositionsTried:= not updateGroupOffsets(groupOffsets,groups,dataLength);
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
      if (index < data_.Length) then
        begin
        index:=index + 1;
        end else exit;
      end;
    end;
  result:=index;
end;

function TDayTwelve.arrangementIsLegal(data_: string; groupInfo,positions,groupOffsets: TIntArray
  ): boolean;
var
   index,element,laterEntryIndex:integer;
   copyData:String;
   testStart,testSize:integer;
begin
  copyData:=data_;
  result:=false;
  if (positions.size <> groupInfo.size) then exit;
  //does the range of any group contain a dot?
  for index:=0 to pred(positions.size)do
    begin
    //Invalid if there are dots in the range selected
    testStart:=positions[index]+groupOffsets[index];
    testSize:=groupInfo[index];
    if (data_.Substring(testStart,testSize).IndexOf('.') > -1) then exit;

    //Overlap check - is a given position + offset > a later position
    if (index < pred(positions.size)) then
      for laterEntryIndex:=index + 1 to pred(positions.size) do
      begin
      if (testStart+testSize >= positions[laterEntryIndex]+groupOffsets[laterEntryIndex]) then
        begin
        results.add('Exclude sequence '+positions.toString(',')+' '+groupOffsets.toString(',')+' because entry '+index.toString+' collides with '+laterEntryIndex.ToString);
        exit;
        end;
      end;

    for element:=testStart to testStart+ pred(testSize) do
      copyData[element+1]:='?';
    end;
  if (copyData.IndexOf('#') > -1) then exit;




  results.add('Sequence '+positions.toString(',')+' '+groupOffsets.toString(',')+' is valid');
  result:=true;

end;

function TDayTwelve.updateGroupOffsets(var offsets:TIntArray; groups: TIntArray;
  dataLength: integer):boolean;
var
   offsetId:integer;
   index:integer;
   done,outOfRange:boolean;
begin
  offsetId:=pred(offsets.size);
  offsets[offsetId]:=offsets[offsetId]+1;
  //increment the specified offset and see if it's legal
  done:=false;
  index:=0;
  repeat
  outOfRange:=sequenceIsTooLong(offsets, groups,offsetId,dataLength);

  If outOfRange then
    begin
    offsetId:= offsetId -1; //move to previous offsetId
    if (offsetId > -1) then
      begin
      offsets[offsetId]:=offsets[offsetId]+1; //and increment it by 1
      for index:=offsetId+1 to pred(offsets.size) do
        offsets[index]:=0;
      end;
    end;
  done:= (not outOfRange) or(offsetId < 0);
  until done;
  result:= not outOfRange;
end;

function TDayTwelve.sequenceIsTooLong(offsets, groups: TIntArray; offsetId, dataLength: integer
  ): boolean;
var
   index:integer;
   totalLength:integer;
begin
  //Would incrementing the specified offset make the sequence length greater than the dataLength?
  totalLength:=groups.size - 1; //each group needs a minimum of one space between
  for index:=0 to pred(offsets.size) do
    begin
    totalLength:=totalLength + offsets[index] + groups[index];
    if index = offsetId then totalLength:=totalLength+1;
    end;
  result:=totalLength > dataLength;
end;

end.


