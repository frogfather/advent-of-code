unit polymer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ArrayUtils;
 type
   
   { TPolymer }

   TPolymer = class(TInterfacedObject)
     private
     fTemplate:string;
     fInsertionRules:TStringMap;
     fpairMap:TStringInt64Map;
     fDistributionMap:TStringInt64Map;
     fAnswer:int64;
     procedure updateMap(map:TStringInt64Map; keyVal:string; adjustment:int64);
     procedure initializeInsertionRules(rules:TStringArray);
     procedure initializePairMap(template:string);
     procedure populateDistributionMap;
     procedure calculateResult;
     function copyMap(sourceMap:TStringInt64Map):TStringInt64Map;
     property template: string read fTemplate write fTemplate;
     public
     constructor create(puzzleInput:TStringArray);
     procedure run(times:integer);
     property answer:int64 read fAnswer;
   end;

implementation

{ TPolymer }

procedure TPolymer.updateMap(map: TStringInt64Map; keyVal:string; adjustment: int64);
var
  currentValue:int64;
    begin
    //find the key if it exists and increment or add
    if not map.TryGetData(keyVal,currentValue)
      then currentValue:=adjustment
    else currentValue:=currentValue+adjustment;
    //if the adjusted value is 0 we should remove the entry
    if currentValue = 0
      then map.Remove(keyVal)
    else map.AddOrSetData(keyVal,currentValue);
    end;

procedure TPolymer.initializeInsertionRules(rules: TStringArray);
var
  index:integer;
  begin
  fInsertionRules:=TStringMap.Create;
  for index:=0 to pred(length(rules)) do
    begin
    finsertionRules.Add(
      rules[index].Split(' -> ',TStringSplitOptions.ExcludeEmpty)[0],
      rules[index].Split(' -> ',TStringSplitOptions.ExcludeEmpty)[1]);
    end;
  end;

procedure TPolymer.initializePairMap(template: string);
var
  index:integer;
  key:string;
  value:int64;
begin
  fpairMap:=TStringInt64Map.Create;
  //Set up the pair map with initial pairs
  for index:=1 to pred(length(template)) do
    begin
    key:=template.Substring(index-1,2);
    if (fpairMap.TryGetData(key,value))
      then value:=value+1
    else value:=1;
    fpairMap.AddOrSetData(key,value);
    end;
end;

procedure TPolymer.populateDistributionMap;
var
  index:integer;
  key:string;
  value:int64;
begin
  for index:=0 to pred(fPairMap.Count) do
    begin
    key:=fPairMap.Keys[index];
    fPairMap.TryGetData(key,value);
    //add each element of the pair to the distribution map
    updateMap(fDistributionMap,key.Substring(0,1),value);
    updateMap(fDistributionMap,key.Substring(1,1),value);
    end;
  //increment first and last elements by 1 because all others are double counted
  updateMap(fDistributionMap,template.Substring(0,1),1);
  updateMap(fDistributionMap,template.Substring(pred(length(template)),1),1);

end;

procedure TPolymer.calculateResult;
var
  index:integer;
  min,max:int64;
  key:string;
  value:int64;
begin
  max:=0;
  //Get the max and min - remember to divide by 2!
  if fDistributionMap.count > 0 then
    begin
    //set min to the first value
    key:=fDistributionMap.keys[0];
    fDistributionMap.TryGetData(key,value);
    min:=value;
    for index:=0 to pred(fDistributionMap.Count) do
      begin
      key:=fDistributionMap.Keys[index];
      if fdistributionMap.TryGetData(key,value) then
        begin
        if value>max then max:=value;
        if value<min then min:=value;
        end;
      end;
    max:=max div 2;
    min:=min div 2;
    fAnswer:=max-min;
    end;
end;

function TPolymer.copyMap(sourceMap:TStringInt64Map):TStringInt64Map;
  var
  copiedMap:TStringInt64Map;
  copyIndex:integer;
  key:string;
  value:int64;
    begin
    copiedMap:=TStringInt64Map.Create;
    for copyIndex:=0 to pred(sourceMap.Count) do
      begin
      key:=sourceMap.Keys[copyIndex];
      if sourceMap.TryGetData(key,value)
        then copiedMap.Add(key,value);
      end;
    result:=copiedMap;
    end;

procedure TPolymer.run(times: integer);
var
  dMapIndex:integer;
  pairMapKey,insertionRuleValue,modifiedEntry,pair1,pair2:string;
  pairMapValue:int64;
  runCount:integer;
  copyPairMap:TStringInt64Map;
  begin
  for runCount:= 0 to times - 1 do
    begin
    copyPairMap:=copyMap(fPairMap);
    //go through the original and modify the copy
    //at the end copy back
    for dMapIndex:=0 to pred(fPairMap.Count) do
      begin
      pairMapKey:=fPairMap.Keys[dMapIndex];
      fPairMap.TryGetData(pairMapKey,pairMapValue);
      //find the pairMapKey in polymer map and insert the value
      if fInsertionRules.TryGetData(pairMapKey,insertionRuleValue) then
        begin
        modifiedEntry:=pairMapKey;
        modifiedEntry:=modifiedEntry.Insert(1,insertionRuleValue);
        pair1:=modifiedEntry.Substring(0,2);
        pair2:=modifiedEntry.Substring(1,2);
        //remove the pairMapKey key from the copy map
        updateMap(copyPairMap,pairMapKey,pairMapValue * -1);
        //update or add the new pairs
        updateMap(copyPairMap,pair1,pairMapValue);
        updateMap(copyPairMap,pair2,pairMapValue);
        end;
      end;
    //now copy back to the original distribution map
    fPairMap:=copyMap(copyPairMap);
    copyPairMap.Free;
    end;
  populateDistributionMap;
  calculateResult;
  end;

constructor TPolymer.create(puzzleInput: TStringArray);
begin
  fTemplate:=puzzleInput[0];
  deleteFromArray(puzzleInput,0);
  initializeInsertionRules(puzzleInput);
  initializePairMap(fTemplate);
  fDistributionMap:=TStringInt64Map.Create;
end;

end.

