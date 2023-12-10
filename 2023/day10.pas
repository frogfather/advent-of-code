unit day10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,pipenode;
type

  { TDayTen}
  TDayTen = class(TAocPuzzle)
  private
  function loadMap:TPipeNode;
  procedure reset;
  procedure drawScreen(sender:TObject);
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  pipeNodeMap:TPipeNodeMap;
  queue:TPipeNodeQueue;
{ TDayTen }

constructor TDayTen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 10',paintbox_);
//parent loads the file as a string and converts to string array;
if (paintbox <> nil) then Paintbox.OnPaint := @drawScreen;
pipeNodeMap:=TPipeNodeMap.create;
queue:=TPipeNodeQueue.create;
end;

procedure TDayTen.runPartOne;
var
  x,y:integer;
  startNode,node:TPipeNode;
  neighbours:TPipeNodeQueue;
  distanceFromStart:integer;
  neighbourIndex:integer;
begin
  results.Clear;
  reset;
  startNode:=loadMap;

  //Add start node to queue
  queue.push(startNode);
  //Now for each node, get the neighbours
  while not (queue.size = 0) do
    begin
    node:=queue.shift;
    distanceFromStart:=node.distance;
    //set this node to visited
    pipeNodeMap.setVisited(node.x,node.y);
    //get its neighbours. This also updates the distance
    neighbours:=pipeNodeMap.getUnvisitedNeighbours(node,distanceFromStart);
    for neighbourIndex:=0 to pred(neighbours.size) do
      queue.push(neighbours[neighbourIndex]);
    end;
  //The distance from start should be the max distance
  results.Add('Distance is '+distanceFromStart.ToString);
end;

procedure TDayTen.runPartTwo;
begin
  runPartOne;//It's the same map
  //For each point with value 0
end;

function TDayTen.loadMap:TPipeNode;
var
  x,y:integer;
  newNode:TPipeNode;
begin
  setLength(pipeNodeMap,puzzleInputLines.size,puzzleInputLines[0].Length);
  for y:=0 to pred(puzzleInputLines.size) do
    for x:=0 to pred(puzzleInputLines[0].Length) do
      begin
      newNode.visited:= false;
      newNode.symbol:=puzzleInputLines[y].Substring(x,1);
      newNode.x:=x;
      newNode.y:=y;
      newNode.distance:=0;
      if (newNode.symbol = 'S') then result:=newNode;
      pipeNodeMap[y][x]:=newNode;
      end;
end;

procedure TDayTen.reset;
begin
  setLength(pipeNodeMap,0);
  setLength(queue,0);
end;

procedure TDayTen.drawScreen(sender: TObject);
var
  x,y,cellLeft,cellTop,cellWidth,cellHeight:integer;
  node:TPipeNode;
begin
  if Sender is TPaintbox then with Sender as TPaintbox do
    begin
    cellWidth:=canvas.Width div (pipeNodeMap.width div 2);
    cellHeight:= canvas.Height div (pipeNodeMap.height div 2);
    canvas.Font.Height:=cellHeight - 1;
    canvas.Brush.Color:=clBlue;
    for y:=0 to pred(pipeNodeMap.height div 2) do
      for x:=0 to pred(pipenodeMap.width div 2) do
        begin
        node:=pipeNodeMap[y][x];
        cellLeft:=(x * cellWidth);
        cellTop:=(y * cellHeight);
        if (node.symbol='S') then canvas.brush.Color:=clYellow else
          if (node.distance > 0) then canvas.Brush.Color:=clTeal else canvas.Brush.Color:=clBlue;
        canvas.Rectangle(cellLeft,cellTop,cellLeft+cellWidth,cellTop+cellHeight);
        canvas.TextOut(cellLeft,cellTop,node.distance.ToString);
        end;
    end;
end;



end.


