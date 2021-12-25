unit cavePassage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,arrayUtils,fpJSON,fgl;
type
  
  { TCaveNavigator }

  TCaveNavigator = class(TInterfacedObject)
    private
    fPuzzleInput: TStringArray;
    fCaveMap: TJSONObject;
    fVisitedCaveMap: TStringIntMap;
    fPaths:integer;
    fVisitCount: integer;
    procedure setUpCaveMap;
    procedure setUpVisitedMap;
    function isSmallCave(cave: string):boolean;
    procedure addToDictionary(key,value: string);
    procedure adjustVisitedCount(cave:string;value:integer);
    public
    constructor create(puzzleInput:TStringArray);
    destructor destroy;
    procedure explore(cave: string; maxSmallCaveRepeatVisits:integer=0);
    property paths: integer read fPaths;
  end;

implementation

{ TCaveNavigator }

procedure TCaveNavigator.setUpCaveMap;
var
index:integer;
pathFrom,pathTo:string;
  begin
  for index:=0 to pred(length(fPuzzleInput))do
    begin
    pathFrom:=fPuzzleInput[index].Split('-')[0];
    pathTo:=fPuzzleInput[index].Split('-')[1];
    addToDictionary(pathFrom,pathTo);
    if (pathFrom <> 'start') then
      addToDictionary(pathTo,pathFrom);
    end;
  end;

procedure TCaveNavigator.setUpVisitedMap;
var
index:integer;
  begin
  for index:=0 to pred(fCaveMap.Count) do
  fVisitedCaveMap.Add(fCaveMap.Names[index],0);
  end;

function TCaveNavigator.isSmallCave(cave: string): boolean;
begin
  result:=ord(cave[1]) > 90;
end;

procedure TCaveNavigator.addToDictionary(key, value: string);
var
jValue:TJSONString;
  begin
  jValue:=TJSONString.Create(value);
  if fCaveMap.indexOfName(key) = -1
      then fCaveMap.Add(key,TJSONArray.Create);
  if fCaveMap.Arrays[key].IndexOf(jValue) = -1
    then fCaveMap.Arrays[key].Add(jValue);
  end;

procedure TCaveNavigator.adjustVisitedCount(cave: string; value: integer);
var
visitCount:integer;
  begin
  fVisitedCaveMap.TryGetData(cave,visitCount);
  visitCount:=visitCount + value;
  fVisitedCaveMap.AddOrSetData(cave,visitCount);
  end;

constructor TCaveNavigator.create(puzzleInput: TStringArray);
begin
fPuzzleInput:=puzzleInput;
fCaveMap:= TJSONObject.Create;
fVisitedCaveMap:=TStringIntMap.Create;
fPaths:=0;
setupCaveMap;
setUpVisitedMap;
end;

destructor TCaveNavigator.destroy;
begin
  fCaveMap.Free;
end;

procedure TCaveNavigator.explore(cave: string; maxSmallCaveRepeatVisits:integer=0);
var
  neighbourId:integer;
  neighbour:string;
  index:integer;
  smallCavesVisitedMoreThanOnce:integer;
  caveToCheck:String;
  currentCount:integer;
  begin
  if (cave = 'end') then
    begin
    fPaths:= fPaths+1;
    exit;
    end;
  if isSmallCave(cave) then adjustVisitedCount(cave,1);
  //now check how many small caves have been visited more than once
  smallCavesVisitedMoreThanOnce:=0;
  for index:= 0 to pred(fVisitedCaveMap.Count) do
    begin
    currentCount:=fVisitedCaveMap.Data[index];
    //in part one we can visit no small caves twice
    //in part two we can visit exactly one small cave twice
    if (currentCount > 1)
      then smallCavesVisitedMoreThanOnce:=smallCavesVisitedMoreThanOnce + 1;
    if (smallCavesVisitedMoreThanOnce > maxSmallCaveRepeatVisits)
      or (currentCount > succ(maxSmallCaveRepeatVisits))
      then
        begin
        adjustVisitedCount(cave,-1);
        exit;
        end;
    end;
  //explore all the neighbours
  for neighbourId:=0 to pred(fCaveMap.Arrays[cave].Count) do
    begin
    neighbour:=fCaveMap.Arrays[cave].Items[neighbourId].AsString;
    explore(neighbour);
    end;
  if isSmallCave(cave) then adjustVisitedCount(cave,-1);
  end;


end.

