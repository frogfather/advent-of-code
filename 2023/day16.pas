unit day16;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,gridElement;
type

  { TDaySixteen}
  TDaySixteen = class(TAocPuzzle)
  private
  procedure setUpMap;
  procedure traceBeam(startx,starty,dirx,diry:integer);
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
//This solution uses recursion. An alternative would be to put the starting coordinates
//and direction in a queue. Whenever a splitter is encountered the two new directions
//are added to the queue and the loop ends when the queue is empty.
//Hyperneutrino uses this approach. https://youtu.be/2td0PZRKkpQ?si=zK8wMnZIUXZrJ4Zq
var
  map_: TGridElementMap;

{ TDaySixteen }

constructor TDaySixteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
map_:=TGridElementMap.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDaySixteen.runPartOne;
var
  x,y:integer;
begin
  results.Clear;
  setUpMap;
  tracebeam(0,0,1,0);
  results.add('visited : '+map_.getAllVisited.tostring);
end;

procedure TDaySixteen.runPartTwo;
var
  x,y:integer;
  filledOnThisRun,maxTiles:integer;
begin
  results.Clear;
  setupMap;
  //Now we need to run the trace method from every edge location
  //and in two directions if starting on a corner
  maxTiles:=0;
  for x:= 0 to pred(map_.size) do
    begin                           //top row
    map_.reset;
    tracebeam(x,0,0,1);
    if (x=0) then tracebeam(x,0,1,0);
    filledOnThisRun:=map_.getAllVisited;
    if (filledOnThisRun > maxTiles) then
      maxTiles:= filledOnThisRun;

    map_.reset;                   //bottom row
    tracebeam(x,pred(length(map_[x])),0,-1);
    if (x=pred(map_.size)) then tracebeam(x,pred(length(map_[x])),-1,0);
    filledOnThisRun:=map_.getAllVisited;
    if (filledOnThisRun > maxTiles) then
      maxTiles:= filledOnThisRun;
    end;

  for y:= 0 to pred(length(map_[0])) do
    begin
    map_.reset;
    tracebeam(0,y,1,0);        //left side
    if (y=0)then tracebeam(0,y,0,1);
    filledOnThisRun:=map_.getAllVisited;
    if (filledOnThisRun > maxTiles) then
      maxTiles:= filledOnThisRun;

    map_.reset;
    tracebeam(pred(map_.size),y,-1,0);
    if (y=pred(length(map_[0])))then tracebeam(pred(map_.size),y,0,-1);
    filledOnThisRun:=map_.getAllVisited;
    if (filledOnThisRun > maxTiles) then
      maxTiles:= filledOnThisRun;
    end;
  results.Add('Max filled '+maxTiles.ToString);
end;

procedure TDaySixteen.setUpMap;
var
  index:integer;
begin
  for index := 0 to pred(puzzleInputLines.size) do
  map_.addRow(index,puzzleInputLines[index]);
end;

procedure TDaySixteen.traceBeam(startx, starty,dirx,diry: integer);
var
  row,col:integer;
  movex,movey,swap:integer;
  done:boolean;
  mirror:char;
begin
  done:=false;
  col:=startx;
  row:=starty;
  movex:=dirx;
  movey:=diry;
  repeat
  if (map_.outOfBounds(col,row))or(map_.previouslyVisitedFromDirection(row,col,movex,movey)) then
    begin
    exit;
    end;
  map_.setVisited(row,col,movex,movey);
  mirror:=map_.getMirror(row,col);
    case mirror of
    '/':
      begin
      swap:=movex;
      movex:=movey * -1;
      movey:=swap * -1;
      end;
    '\':
      begin
      swap:=movex;
      movex:=movey;
      movey:=swap;
      end;
    '-': if (movey <> 0) then
      begin
      //call this recursively twice - one for each direction
      tracebeam(col-1,row,-1,0);
      tracebeam(col+1,row,1,0);
      exit;
      end;
    '|': if (movex <> 0) then
      begin
      tracebeam(col,row-1,0,-1);
      tracebeam(col,row+1,0,1);
      exit;
      end;
    end;
  col:=col+movex;
  row:=row+movey;
  done:=map_.outOfBounds(col,row);
  until done;
end;

end.


