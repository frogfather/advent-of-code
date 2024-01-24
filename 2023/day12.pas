unit day12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwelve}
  TDayTwelve = class(TAocPuzzle)
  private
  function count(config:string;nums:TIntArray):int64;
  function multiplyConfig(config_:string):string;
  function multiplyNums(nums_:string):string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
//This is entirely based on HyperNeutrino's recursive solution
//
{ TDayTwelve }

var
  cache: TStringInt64Map;
const
  workingOptions: TStringArray = ('.','?');
  brokenOptions:TStringArray = ('#','?');

constructor TDayTwelve.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 12',paintbox_);
cache:=TStringInt64Map.Create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwelve.runPartOne;
var
  lineTotal:integer;
  total,index:integer;
  parts:TStringArray;
begin
  results.Clear;
  total := 0;
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    cache.Clear;
    parts:=puzzleInputLines[index].Split([' '],TStringSplitOptions.ExcludeEmpty);
    lineTotal:=count(parts[0],parts[1].Split([','],TStringSplitOptions.ExcludeEmpty).toIntArray);
    results.add('total for '+puzzleInputLines[index]+' '+lineTotal.ToString);
    total:=total+ lineTotal;
    end;
  results.Add('Total is '+total.ToString);
end;

procedure TDayTwelve.runPartTwo;
var
  lineTotal,total:int64;
  index:integer;
  parts:TStringArray;
begin
  results.Clear;
  total := 0;
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    cache.Clear;
    parts:=puzzleInputLines[index].Split([' '],TStringSplitOptions.ExcludeEmpty);
    lineTotal:=count(multiplyConfig(parts[0]),multiplyNums(parts[1]).Split([','],TStringSplitOptions.ExcludeEmpty).toIntArray);
    writeln('line '+index.toString);
    total:=total+ lineTotal;
    end;
  results.Add('Total is '+total.ToString);
end;

function TDayTwelve.count(config: string; nums: TIntArray): int64;
var
  firstBlock,elementAfterFirstBlock:string;
  remainderOfNums:TIntArray;
  cachedResult:int64;
begin
  result:=0;

  if ((config = '')
  or(config.IndexOf('#') = -1))and(nums.size = 0 ) then
    begin
    result:= 1;
    exit;
    end;

  if (nums.size = 0) then exit;

  if cache.TryGetData(config+nums.toString(''),cachedResult) then
    begin
    result:=cachedResult;
    end else
    begin

    if (workingOptions.indexOf(config.Substring(0,1)) > -1) then
      result:=result + count(config.Substring(1),nums);

    if (brokenOptions.indexOf(config.Substring(0,1)) > -1) then
      begin
      firstBlock:= config.Substring(0,nums[0]);
      elementAfterFirstBlock:=config.Substring(nums[0],1);
      remainderOfNums:=nums.slice(1,nums.size);

      if (nums[0] <= config.Length)
      and (firstBlock.IndexOf('.') = -1)
      and ((nums[0] = config.Length)or(elementAfterFirstBlock <> '#')) then
        result:= result + count(config.Substring(nums[0]+1),remainderOfNums);
      end;
    cache.AddOrSetData(config+nums.toString(''),result);
    end;
end;

function TDayTwelve.multiplyConfig(config_: string): string;
var
  count_:integer;
begin
  //repeat 5x with ? between
  result:='';
  for count_:=1 to 5 do
    begin
    result:=result+config_;
    if (count_ < 5) then result:=result+'?';
    end;
end;

function TDayTwelve.multiplyNums(nums_: string): string;
var
  count_:integer;
begin
  result:='';
  for count_:=1 to 5 do
    begin
    result:=result+nums_;
    if (count_ < 5) then result:=result+',';
    end;
end;

end.


