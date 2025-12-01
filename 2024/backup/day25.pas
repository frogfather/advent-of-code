unit day25;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  TLock = record
  heights: TIntArray;
  end;

  TKey = record
  heights:TIntArray;
  end;

  TLocks = array of TLock;
  TKeys = array of TKey;

  { TLocksHelper }

  TLocksHelper = type helper for TLocks
  function size:integer;
  function push(element:TLock):integer;
  procedure clear;
  end;

  { TKeysHelper }

  TKeysHelper = type helper for TKeys
  function size:integer;
  function push(element:TKey):integer;
  procedure clear;
  end;

  { TDayTwentyFive}
  TDayTwentyFive = class(TAocPuzzle)
  private
  procedure loadKeysAndLocks;
  function itemIsKey(item:TstringArray):boolean;
  function getHeights(item:TStringArray):TIntArray;
  function convertToKey(item:TStringArray):TKey;
  function convertToLock(item:TStringArray):TLock;
  function getFittingKeysCount:integer;
  function keyFits(lock:TLock;key:TKey):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  keys:TKeys;
  locks:TLocks;

{ TLocksHelper }

function TLocksHelper.size: integer;
begin
   result:=length(self);
end;

function TLocksHelper.push(element:TLock): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

procedure TLocksHelper.clear;
begin
  setLength(self,0);
end;

{ TKeysHelper }

function TKeysHelper.size: integer;
begin
  result:=length(self);
end;

function TKeysHelper.push(element:TKey): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

procedure TKeysHelper.clear;
begin
  setLength(self,0);
end;

{ TDayTwentyFive }

procedure TDayTwentyFive.loadKeysAndLocks;
var
  index:integer;
  lines:TStringArray;
begin
  lines:=TStringArray.create;
  for index:=0 to puzzleInputLines.size do
    begin
    //If the line is blank and the array has stuff in it...
    if ((index = puzzleInputLines.size)or(puzzleInputLines[index].Trim = ''))and(lines.size > 0) then
      begin
      if itemIsKey(lines) then keys.push(convertToKey(lines))
      else locks.push(convertToLock(lines));
      lines.clear;
      end else if (index < puzzleInputLines.size)and(puzzleInputLines[index].trim <> '') then lines.push(puzzleInputLines[index]);
    end;
end;

function TDayTwentyFive.itemIsKey(item: TstringArray): boolean;
begin
  result:=item[0].Substring(0,1) = '#';
end;

function TDayTwentyFive.getHeights(item: TStringArray): TIntArray;
var
  col,row:integer;
  height:integer;
begin
  result:=TIntArray.create;
  for col:=0 to pred(item[0].Length) do
    begin
    height:=0;
    for row:=0 to pred(item.size) do
      if (item[row].Substring(col,1) = '#') then height:=height + 1;
    result.push(height - 1);
    end;
end;

function TDayTwentyFive.convertToKey(item: TStringArray): TKey;
begin
  result.heights:=getHeights(item);
end;

function TDayTwentyFive.convertToLock(item: TStringArray): TLock;

begin
  result.heights:=getHeights(item);
end;

function TDayTwentyFive.getFittingKeysCount: integer;
var
  lockIndex,keyIndex:integer;
begin
  result:=0;
  for lockIndex:=0 to pred(locks.size) do
    for keyIndex:=0 to pred(keys.size) do
     if keyFits(locks[lockIndex],keys[keyIndex]) then result:=result+1;
end;

function TDayTwentyFive.keyFits(lock: TLock; key: TKey): boolean;
var
  index:integer;
begin
  //all sums must be less than 6
  result:=false;
  for index:=0 to pred(lock.heights.size)do
   if lock.heights[index]+key.heights[index] > 5 then exit;
  result:=true;
end;

constructor TDayTwentyFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 25',paintbox_);
keys:=TKeys.create;
locks:=TLocks.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyFive.runPartOne;
var
  index:integer;
begin
  results.Clear;
  loadKeysAndLocks;
  results.add('Fitting keys '+getFittingKeyCount.ToString);
end;

procedure TDayTwentyFive.runPartTwo;
begin
  results.Clear;
end;


end.

                
