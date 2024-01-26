unit gridElement;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

//used in day 16
uses
  Classes, SysUtils;

type
  TGridElement = record
    mirror:char;
    visited:boolean;
    north:boolean;
    south:boolean;
    east:boolean;
    west:boolean;
  end;

  TGridElementMap = array of array of TGridElement;

  { TGridElementMapHelper }

  TGridElementMapHelper = type helper for TGridElementMap
  procedure clear;
  procedure reset;
  function size:integer;
  procedure addRow(rowNo:integer;rowData:string);
  function getMirror(row,col:integer):char;
  function getVisited(row,col:integer):boolean;
  function previouslyVisitedFromDirection(row,col,xdir,ydir:integer):boolean;
  procedure setVisited(row,col,xdir,ydir:integer);
  function outOfBounds(x,y:integer):boolean;
  function getAllVisited:integer;
  end;


implementation

{ TGridElementMapHelper }

procedure TGridElementMapHelper.clear;
begin
  setLength(self,0);
end;

procedure TGridElementMapHelper.reset;
var
  x,y:integer;
begin
  for x:=0 to pred(self.size) do
    for y:=0 to pred(length(self[x])) do
      begin
      self[x][y].visited:=false;
      self[x][y].north:=false;
      self[x][y].south:=false;
      self[x][y].west:=false;
      self[x][y].east:=false;
      end;
end;

function TGridElementMapHelper.size: integer;
begin
  result:=length(self);
end;

procedure TGridElementMapHelper.addRow(rowNo: integer; rowData: string);
var
  col:integer;
  newElement:TGridElement;
begin
  //length(self) is the number of columns which is the length of the string
  //length(self[col] is the number of rows in that column
  if (length(self) <= rowData.Length) then setLength(self,rowData.Length);
  for col:=0 to pred(length(rowData)) do
    begin
    newElement.mirror:=rowData[col+1];  //Remember strings are 1 indexed
    newElement.visited:=false;
    newElement.north:=false;
    newElement.south:= false;
    newElement.west:=false;
    newElement.east:=false;
    if (length(self[col]) <= rowNo) then setLength(self[col],rowNo + 1);
    self[col][rowNo]:=newElement;
    end;
end;

function TGridElementMapHelper.getMirror(row, col: integer): char;
begin
  result:=self[col][row].mirror;
end;

function TGridElementMapHelper.getVisited(row, col: integer): boolean;
begin
  result:=self[col][row].visited;
end;

function TGridElementMapHelper.previouslyVisitedFromDirection(row, col, xdir,
  ydir: integer): boolean;
var
  elem:TGridElement;
begin
  elem:=self[col][row];
  //if xdir is 1 west
  //if xdir is -1 east
  //if ydir is 1 north
  //if ydir is -1 south
  result:=((xdir = 1)and(elem.west))
  or ((xdir = -1) and (elem.east))
  or ((ydir = 1)and(elem.north))
  or ((ydir = -1)and(elem.south));
end;

procedure TGridElementMapHelper.setVisited(row, col,xdir,ydir: integer);
begin
  self[col][row].visited:=true;
  if (xdir = 1) then self[col][row].west:=true else
  if (xdir = -1)then self[col][row].east:=true else
  if (ydir = 1)then self[col][row].north:=true else
  if (ydir = -1) then self[col][row].south:=true;
end;

function TGridElementMapHelper.outOfBounds(x, y: integer): boolean;
begin
  result:= (x < 0)or(y < 0)or(x >= self.size)or(y>=length(self[x]))
end;

function TGridElementMapHelper.getAllVisited: integer;
var
  x,y:integer;
begin
  result:=0;
  for x:=0 to pred(self.size) do
    for y:=0 to pred(length(self[x])) do
      if self[y][x].visited then result:=result+1;
end;

end.

