unit day8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayEight}
  TDayEight = class(TAocPuzzle)
  private
  procedure loadMap;
  procedure printMap;
  procedure findAllAntinodes(multi:boolean=false);
  procedure findAntinodesFromAntennaAt(symbol:string;x,y:integer);
  procedure findAntinodesFromAntennaWithHarmonicsAt(symbol:string;x,y:integer);
  procedure markAntinode(x,y:integer);
  function sameSymbol(x,y:integer):boolean;
  function offGrid(x,y:integer):boolean;
  function countAntinodes:integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayEight }
var
  map:T3DStringArray;

procedure TDayEight.loadMap;
var
  puzzleRow,puzzleCol:integer;
begin
  map.clear;
  //Put the puzzle info into layer 1
  for puzzleRow:=0 to pred(puzzleInputLines.size) do
    for puzzleCol:=0 to pred(puzzleInputLines[puzzleRow].Length) do
      map.setValue(puzzleCol,puzzleRow,1,puzzleInputLines[puzzleRow].Substring(puzzleCol,1));
end;

procedure TDayEight.printMap;
var
  y,z:integer;
begin
  for z:=0 to pred(map.layers) do
    for y:=0 to pred(map.rows(z))do
      results.Add(map.getValue(y,z).toString(''));
end;

procedure TDayEight.findAllAntinodes(multi:boolean=false);
var
  x,y:integer;
  symbol:string;
begin
  for y:=0 to pred(map.rows(1)) do
    for x:=0 to pred(map.size(1,y)) do
      begin
      symbol:=map.getValue(x,y,1);
      if (symbol <> '.') then
        begin
        if multi
          then findAntinodesFromAntennaWithHarmonicsAt(symbol,x,y)
          else findAntinodesFromAntennaAt(symbol,x,y);
        end;
      end;
end;
//Each antenna interacting with another will produce 2 nodes
//Calculate the x,y distance to the next node and also its reflection
procedure TDayEight.findAntinodesFromAntennaAt(symbol:string;x,y:integer);
var
  rowOffset,colOffset:integer;
begin
  for rowOffset:=0 to pred(map.rows(1)) - y do
    begin
    //Does the selected row contain the symbol?
    if map.getValue(y+rowOffset,1).indexOf(symbol) > -1 then
    for colOffset:= 0-x to pred(map.size(1,y+rowOffset)) do
      if (map.getValue(x+colOffset,y+rowOffset,1)=symbol)
      and not sameSymbol(colOffset,rowOffset) then
        begin
        //double the offset - are we off the map?
        if not offGrid(x+(colOffset * 2),y+(rowOffset * 2))
           then markAntinode(x+(colOffset * 2),y+(rowOffset * 2));
        if not offGrid(x-colOffset, y-rowOffset)
           then markAntinode(x-colOffset, y-rowOffset);
        end;
    end;
end;

procedure TDayEight.findAntinodesFromAntennaWithHarmonicsAt(symbol: string; x,
  y: integer);
var
  rowOffset,colOffset:integer;
  harmonic:integer;
  done:boolean;
begin
  for rowOffset:=0 to pred(map.rows(1)) - y do
    begin
    //Does the selected row contain the symbol?
    if map.getValue(y+rowOffset,1).indexOf(symbol) > -1 then
    for colOffset:= 0-x to pred(map.size(1,y+rowOffset)) do
      if (map.getValue(x+colOffset,y+rowOffset,1)=symbol) then
        begin
        //keep adding the offset until we're off the map
        harmonic:=0;
        done:=false;
        repeat
        if not offGrid(x+(colOffset * harmonic),y+(rowOffset * harmonic))
           then markAntinode(x+(colOffset * harmonic),y+(rowOffset * harmonic)) else done:=true;
        harmonic:=harmonic+1;
        until done;
        harmonic:=0;
        done:=false;
        repeat
        if not offGrid(x-(colOffset * harmonic), y-(rowOffset * harmonic))
           then markAntinode(x-(colOffset * harmonic), y-(rowOffset * harmonic)) else done:= true;
        harmonic:=harmonic+1;
        until done;
        end;
    end;
end;

procedure TDayEight.markAntinode(x, y:integer);
begin
  map.setValue(x,y,0,'#');
end;

function TDayEight.sameSymbol(x, y:integer): boolean;
begin
  result:= (x=0) and (y=0);
end;

function TDayEight.offGrid(x, y:integer): boolean;
begin
  result:= (y < 0)
           or (y > pred(map.rows(1)))
           or (x < 0)
           or (x > pred(map.size(1,y)));
end;

function TDayEight.countAntinodes: integer;
var
  x,y:integer;
begin
  result:=0;
  for y:=0 to pred(map.rows(0))do
    for x:=0 to pred(map.size(0,y)) do
      if (map.getValue(x,y,0) = '#') then result:=result+1;
end;

constructor TDayEight.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 8',paintbox_);
map:=T3DStringArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayEight.runPartOne;
begin
  results.Clear;
  loadMap;
  findAllAntinodes;
  results.add('There are '+countAntinodes.toString+' antinodes on the map');
end;

procedure TDayEight.runPartTwo;
begin
  results.Clear;
  loadMap;
  findAllAntinodes(true);
  results.add('There are '+countAntinodes.toString+' antinodes on the map');
end;


end.

                
