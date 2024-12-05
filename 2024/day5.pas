unit day5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFive}
  TDayFive = class(TAocPuzzle)
  private
  function isValidData(entry:string):boolean;
  function rulePasses(entry, rule:string):boolean;
  function middleValue(entry:string):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  rules,data: TStringlist;
  validData,invalidData:TStringlist;

{ TDayFive }

function TDayFive.isValidData(entry: string): boolean;
var
  index:integer;
begin
  result:=false;
  for index:=0 to pred(rules.Count) do
    begin
    if not rulePasses(entry, rules[index]) then exit;
    end;
  result:=true;
end;

function TDayFive.rulePasses(entry, rule: string): boolean;
var
  firstNo,secondNo:integer;
  dataElements: TIntArray;
  lastIndexOfFirst,firstIndexOfSecond:integer;
begin
  result:=false;
  dataElements:=TIntArray.create;
  firstNo:=rule.Split('|')[0].toInteger;
  secondNo:= rule.Split('|')[1].toInteger;
  dataElements:=entry.Split(',').toIntArray;
  //if the dataElements doesn't include both numbers we can return true
  lastIndexOfFirst:= dataElements.lastIndexOf(firstNo);
  firstIndexOfSecond:= dataElements.IndexOf(secondNo);
  result:=(lastIndexOfFirst < firstIndexOfSecond) or (lastIndexOfFirst = -1) or (firstIndexOfSecond = -1);
end;

function TDayFive.middleValue(entry: string): integer;
var
  values:TIntArray;
begin
  values:=entry.Split(',').toIntArray;
  result:=values[values.size div 2];
end;

constructor TDayFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 5',paintbox_);
rules:=TStringlist.create;
data:=TStringlist.create;
validData:=TStringlist.create;
invalidData:=TStringlist.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayFive.runPartOne;
var
  index:integer;
  onData:boolean;
  answer:Integer;
begin
  results.Clear;
  onData:=false;
  answer:=0;
  validData.Clear;
  invalidData.Clear;
  rules.Clear;
  data.Clear;
  //rules are first, data second with blank line between
  //at this point we're identifying instructions that are correct
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    if (puzzleInputLines[index].Trim = '') then onData:=true
    else if onData then data.Add(puzzleInputLines[index])
    else rules.Add(puzzleInputLines[index]);
    end;
  //for each rule, check if any elements violate that rule
  //so for 46|57 do ANY 57s come before 46?
  for index:=0 to pred(data.Count) do
    begin
    if isValidData(data[index])
       then validData.Add(data[index])
       else invalidData.Add(data[index]);
    end;
  //Now find the middle number of each entry
  for index:=0 to pred(validData.Count) do
      answer := answer + middleValue(validData[index]);
  results.add('answer '+answer.toString);
end;

procedure TDayFive.runPartTwo;
begin
  results.Clear;
  //Now reorder incorrect entries
  runPartOne;
end;


end.

                
