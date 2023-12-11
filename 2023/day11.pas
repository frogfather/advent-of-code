unit day11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayEleven}
  TDayEleven = class(TAocPuzzle)
  private
  procedure loadMap;
  procedure adjustMap;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  map: TStringArray;

{ TDayEleven }

constructor TDayEleven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 11',paintbox_);
//parent loads the file as a string and converts to string array;
map:=TStringArray.create;
end;

procedure TDayEleven.runPartOne;
var
  index:integer;
begin
  results.Clear;
  loadMap;
  adjustMap;
  for index:=0 to pred(map.size) do
    results.add(map[index])
end;

procedure TDayEleven.runPartTwo;
begin
  results.Clear;
end;

procedure TDayEleven.loadMap;
var
  lineNo:integer;
begin
  //Array of strings
  for lineNo:=0 to pred(puzzleInputLines.size) do
    map.push(puzzleInputLines[lineNo]);
end;

procedure TDayEleven.adjustMap;
var
  lineNo,elementNo,emptyCol:integer;
  newLine:TStringArray;
  emptyColumns:TintArray;
  columnIsEmpty:boolean;
begin
  newLine:=TStringArray.create;
  emptyColumns:=TIntArray.create;
  for lineNo:=pred(map.size) downTo 0 do
    begin
    //Does this row have no # symbol?
    if (map[lineNo].IndexOf('#') = -1) then
      begin
      setLength(newLine,0);
      newLine.push(map[LineNo]);
      map.splice(lineNo,0,newLine);
      end;
    end;
  //Do the same for columns. A little trickier
  for elementNo:=0 to pred(map[0].Length) do
    begin
    columnIsEmpty:=true;
    for lineNo:=0 to pred(map.size) do
      begin
      if map[lineNo].Substring(elementNo,1) = '#' then columnIsEmpty:=false;
      end;
    if columnIsEmpty then emptyColumns.push(elementNo);
    end;
  //For each empty column we need to add an extra char at that point
  for lineNo:=0 to pred(map.size) do
    for emptyCol:=0 to pred(emptyColumns.size) do
      begin
      //the value of empty columns at that point tells us what index the space should be inserted
      map[lineNo].Insert(emptyColumns[emptyCol],'.');
      end;
end;

end.


