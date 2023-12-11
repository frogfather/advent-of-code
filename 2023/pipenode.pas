unit pipenode;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,arrayUtils;
Type

  TPipeNode = record
    x:integer;
    y:integer;
    distance:integer;
    symbol: string;
    visited: Boolean;
  end;

  TPipeNodeQueue = array of TPipeNode;

  { TPipeNodeQueueHelper }
  TPipeNodeQueueHelper = type helper for TPipeNodeQueue
  function size:integer;
  function shift:TPipeNode;
  function push(item_:TPipeNode):integer;
  procedure addNodes(newNodes:TPipeNodeQueue);
  end;

  TPipeNodeMap = array of array of TPipeNode;

  { TPipeNodeMapHelper }
  TPipeNodeMapHelper = type helper for TPipeNodeMap
  function getNodeAt(x,y:integer):TPipeNode;
  function getUnvisitedNeighbours(node:TPipeNode;distance_:integer):TPipeNodeQueue;
  function inRange(x,y:integer):boolean;
  function height:integer;
  function width:integer;
  procedure setDistance(x,y,distance_:integer);
  procedure setVisited(x,y:integer);
  procedure setSymbol(x,y:integer;symbol_:string);
  end;

implementation

{ TPipeNodeQueueHelper }

function TPipeNodeQueueHelper.size: integer;
begin
  result:=length(self);
end;

function TPipeNodeQueueHelper.shift: TPipeNode;
var
  index:integer;
begin
  if self.size > 0 then
    begin
    result:=self[0];
    for index:=1 to pred(self.size) do
    self[index-1]:=self[index];
    setLength(self,self.size - 1);
    end;
end;

function TPipeNodeQueueHelper.push(item_: TPipeNode): integer;
begin
  insert(item_,self,length(self));
  result:=self.size;
end;

procedure TPipeNodeQueueHelper.addNodes(newNodes: TPipeNodeQueue);
var
  index:integer;
begin
  for index:=0 to pred(newNodes.size) do
    self.push(newNodes[index]);
end;

{ TPipeNodeMapHelper }

function TPipeNodeMapHelper.getNodeAt(x, y: integer): TPipeNode;
begin
  if self.inRange(x,y) then result:=self[y][x];
end;

function TPipeNodeMapHelper.getUnvisitedNeighbours(node: TPipeNode;distance_:integer): TPipeNodeQueue;
const
  upSymbols : TStringArray = ('|','L','J','S');
  downSymbols: TStringArray = ('|','7','F','S');
  leftSymbols: TStringArray = ('-','7','J','S');
  rightSymbols: TStringArray = ('-','L','F','S');
var
  x,y:integer;
  symbol:string;

  //Can we move from this node in the direction indicated
  function validNode(x_,y_:integer;direction:string):boolean;
  var
    testNode:TPipeNode;
    allowedSymbols:TStringArray;
  begin
    result:=false;
    if not inrange(x_,y_) then exit;
    testNode:=self[y_][x_];
    if (testNode.symbol = '.') then exit;
    case direction of
     'R': allowedSymbols:=rightSymbols;
     'L': allowedSymbols:=leftSymbols;
     'U': allowedSymbols:=upSymbols;
     'D': allowedSymbols:=downSymbols;
     end;
  result:= not (self[y_][x_].visited)
  and (allowedSymbols.indexOf(self[y_][x_].symbol) > -1);
  end;

begin
  result:=TPipeNodeQueue.create;
  symbol:=node.symbol;
  x:=node.x;
  y:=node.y;
  if (leftSymbols.indexOf(symbol) > -1) and validNode(x-1,y,'R')
  then
    begin
    self[y][x-1].distance:=distance_+1;
    result.push(self[y][x-1]);
    end;
  if (rightSymbols.indexOf(symbol) > -1) and validNode(x+1,y,'L')
  then
    begin
    self[y][x+1].distance:=distance_+1;
    result.push(self[y][x+1]);
    end;
  if (upSymbols.indexOf(symbol) > -1) and validNode(x,y-1,'D')
  then
    begin
    self[y-1][x].distance:=distance_+1;
    result.push(self[y-1][x]);
    end;
  if (downSymbols.indexOf(symbol) > -1) and validNode(x,y+1,'U')
  then
    begin
    self[y+1][x].distance:=distance_+1;
    result.push(self[y+1][x]);
    end;
end;

function TPipeNodeMapHelper.inRange(x, y: integer): boolean;
begin
  result:= (x > -1)and(x < length(self[0]))and(y > -1)and(y < length(self));
end;

function TPipeNodeMapHelper.height: integer;
begin
  result:=length(self);
end;

function TPipeNodeMapHelper.width: integer;
begin
  result:=0;
  if (self=nil)or(self[0]=nil) then exit;
  result:=length(self[0]);
end;

procedure TPipeNodeMapHelper.setDistance(x, y, distance_: integer);
begin
  self[y][x].distance:=distance_;
end;

procedure TPipeNodeMapHelper.setVisited(x, y:integer);
begin
  self[y][x].visited:=true;
end;

procedure TPipeNodeMapHelper.setSymbol(x, y: integer; symbol_: string);
begin
  self[y][x].symbol:=symbol_;
end;

end.

