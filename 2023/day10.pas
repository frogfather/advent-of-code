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
  procedure clearUnvisitedPipes;
  procedure drawScreen(sender:TObject);
  procedure setStartPoint;
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
  clearUnvisitedPipes;
  setStartPoint; //Start point 'S' could be any type - we need to replace it with the correct type or it'll mess up the crossing detection
  outside:=0;
  inside:=0;
  for y:=0 to pred(pipeNodeMap.height) do
    begin
    for x:=0 to pred(pipeNodeMap.width) do
      begin
      nodeToTest:=pipeNodeMap.getNodeAt(x,y);
      if (not nodeToTest.visited) then
        begin
        if (crossings(x,y) mod 2 = 1)
          then inside:=inside+1 else outside:=outside+1;
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

//This was rewritten with a LOT of reference to hyperneutrino's solution.
//There is a very clear explanation of why it has to be this way on his video https://www.youtube.com/watch?v=r3i3XE9H4uw

function TDayTen.crossings(x, y: integer): integer;
const directionChanges: TStringArray = ('J','L','F','7');
const northFacing: TStringArray = ('J','L');
const southFacing: TStringArray = ('F','7');
var
  xPos,yPos:integer;
  nodeToCheck:TPipeNode;
  enteredFromNorth:boolean;
  directionChangeCount:integer;
  symbol:string;
begin
  result:=0;
  if (x=0) then exit;
  xPos:=x;
  yPos:=y;
  directionChangeCount:=0;
  for xPos:=x-1 downTo 0 do
  begin
  nodeToCheck:=pipeNodeMap.getNodeAt(xPos,yPos);
  symbol:=nodeToCheck.symbol;
  if nodeToCheck.visited then
    begin
    if (directionChanges.indexOf(nodeToCheck.symbol) > -1) then
      begin
      if (directionChangeCount = 0) then
        begin
        enteredFromNorth:= (northFacing.indexOf(symbol) > -1);
        directionChangeCount:=directionChangeCount+1;
        end else
        begin
        if (enteredFromNorth) and (southFacing.indexOf(symbol) > -1)
        or (not enteredFromNorth) and (northFacing.indexOf(symbol)> -1)
          then result:=result+1;
        directionChangeCount:=directionChangeCount-1;
        end;
      end else
    if (symbol = '|') then result:=result+1;
    end;
  end;
end;

procedure TDayTen.clearUnvisitedPipes;
var
x,y:integer;
node:TPipeNode;
begin
  for y:=0 to pred(pipeNodeMap.Height) do
    for x:=0 to pred(pipeNodeMap.Width) do
    begin
    node:=pipeNodeMap.getNodeAt(x,y);
    if (not node.visited) and (node.symbol <> '.') then
       pipeNodeMap.setSymbol(x,y,'.');
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
    scale:=1;
    cellWidth:=canvas.Width div (pipeNodeMap.width div scale);
    cellHeight:= canvas.Height  div (pipenodeMap.height div scale);
    canvas.Font.Height:=cellHeight div 2;
    canvas.Brush.Color:=clGray;
    for y:=0 to pred(pipenodeMap.height) do
      for x:=0 to pred(pipenodeMap.width) do
        begin
        node:=pipeNodeMap[y][x];
        cellLeft:=(x * cellWidth);
        cellTop:=(y * cellHeight);
        if (node.visited) then canvas.Brush.Color:=clTeal else
          canvas.Brush.Color:=clGray;
        canvas.Rectangle(cellLeft,cellTop,cellLeft+cellWidth,cellTop+cellHeight);
        canvas.TextOut(cellLeft+1,cellTop,node.symbol);
        end;
    end;
end;

procedure TDayTen.setStartPoint;
var
  startNode:TPipeNode;
  x,y:integer;
  nDist,sDist,wDist,eDist:integer;
  sValue:string;
begin
  //StartPoint is one of LJF7|-
  //Look at the nodes round it
  nDist:=-1;
  sDist:=-1;
  wDist:=-1;
  eDist:=-1;
  for y:=0 to pred(pipeNodeMap.height) do
    for x:=0 to pred(pipeNodeMap.width) do
      begin
      if (pipeNodeMap.getNodeAt(x,y).symbol = 'S') then
        begin
        startNode:=pipeNodeMap.getNodeAt(x,y);
        break;
        end;
      end;
  if (startNode.x > 0)
    then wDist:= pipeNodeMap.getNodeAt(startNode.x-1,startNode.y).distance;
  if (startNode.x < pred(pipeNodeMap.width)) then eDist:=pipeNodeMap.getNodeAt(startNode.x + 1,startNode.y).distance;
  if (startNode.y > 0) then nDist:=pipeNodeMap.getNodeAt(startNode.x,startNode.y-1).distance;
  if (startNode.y < pred(pipeNodeMap.height)) then sDist:=pipeNodeMap.getNodeAt(startNode.x,startNode.y+1).distance;
  if (nDist = 1) then
    begin
    if (sDist = 1) then sValue:= '|' else
    if (eDist = 1) then sValue:= 'L' else
    if (wDist = 1) then sValue:= 'J';
    end else
  if (sDist = 1) then
    begin
    if (eDist = 1) then sValue:= 'F' else
    if (wDist = 1) then sValue:= '7';
    end else
  if (eDist = 1) then sValue:= '-';
  pipeNodeMap.setSymbol(startNode.x,startNode.y,sValue);
end;

end.


