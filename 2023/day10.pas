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
  function crossings(x,y:integer):integer;
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
var
  outside,inside:integer;
  x,y:integer;
  nodeToTest:TPipeNode;
begin
  runPartOne;//It's the same map
  //For each point with value 0 check number of line crossings
  //If it's 0 try another direction
  outside:=0;
  inside:=0;
  for y:=0 to pred(pipeNodeMap.height) do
    begin
    for x:=0 to pred(pipeNodeMap.width) do
      begin
      nodeToTest:=pipeNodeMap.getNodeAt(x,y);
      if (not nodeToTest.visited) then
        begin
        if (crossings(x,y) mod 2 = 1) then inside:=inside+1
        else outside:=outside+1;
        end;
      end;
    end;
  results.Add('Nodes outside loop '+outside.ToString);
  results.Add('Nodes inside loop '+inside.ToString);
end;

function TDayTen.loadMap:TPipeNode;
var
  x,y:integer;
  newNode:TPipeNode;
begin
  setLength(pipeNodeMap,puzzleInputLines.size,puzzleInputLines[0].Length);
  for y:=0 to pred(puzzleInputLines.size) do
    begin
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
end;

procedure TDayTen.reset;
begin
  setLength(pipeNodeMap,0);
  setLength(queue,0);
end;

function TDayTen.crossings(x, y: integer): integer;
const allowedChars: TStringArray = ('J','L','|');
var
  xPos,yPos:integer;
  nodeToCheck:TPipeNode;

begin
  result:=0;
  if (x=0) then exit;
  xPos:=x;
  yPos:=y;

  for xPos:=x-1 downTo 0 do
  begin
  nodeToCheck:=pipeNodeMap.getNodeAt(xPos,yPos);
  if (nodeToCheck.visited) and (allowedChars.indexOf(nodeToCheck.symbol) > -1) then result:=result+1;
  end;
end;

procedure TDayTen.drawScreen(sender: TObject);
var
  x,y,cellLeft,cellTop,cellWidth,cellHeight:integer;
  node:TPipeNode;
  scale:integer;
begin
  if Sender is TPaintbox then with Sender as TPaintbox do
    begin
    scale:=4;
    cellWidth:=canvas.Width div (pipeNodeMap.width div scale);
    cellHeight:= canvas.Height div (pipeNodeMap.height div scale);
    canvas.Font.Height:=cellHeight div 2;
    canvas.Brush.Color:=clYellow;
    for y:=0 to pred(pipeNodeMap.height div scale) do
      for x:=0 to pred(pipenodeMap.width div scale) do
        begin
        node:=pipeNodeMap[y][x];
        cellLeft:=(x * cellWidth);
        cellTop:=(y * cellHeight);
        if (node.symbol='S') then canvas.brush.Color:=clYellow else
          if (node.visited) then canvas.Brush.Color:=clTeal else canvas.Brush.Color:=clYellow;
        canvas.Rectangle(cellLeft,cellTop,cellLeft+cellWidth,cellTop+cellHeight);
        canvas.TextOut(cellLeft+1,cellTop,node.symbol+' '+node.distance.toString);
        end;
    end;
end;

end.


