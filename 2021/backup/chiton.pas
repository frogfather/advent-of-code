unit chiton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ArrayUtils;
type

  { TQueueEntry }

  TQueueEntry = class(TInterfacedObject)
    private
    fNodeId:integer;
    fNodePos:TPoint;
    fRisk:integer;
    fSource:integer;
    fInfinity:boolean;
    fVisited:boolean;
    public
    constructor create(position,mapDimensions:TPoint;initialRisk:integer=0;source:integer=0);
    property nodeId: integer read fNodeId;
    property nodePos: TPoint read fNodePos;
    property risk: integer read fRisk write fRisk;
    property source: integer read fSource write fSource;
    property infinity: boolean read fInfinity write fInfinity;
    property visited:boolean read fVisited write fVisited;
  end;

  TQueue = array of TQueueEntry;

  { TRouteFinder }

  TRouteFinder = class(TInterfacedObject)
    private
    fMap:T3DIntMap;
    fQueue:TQueue;
    fShortest:integer;
    fFinishPoint:TPoint;
    function getQueueLength: integer;
    function getMapDimensions:TPoint;
    procedure initializeMap(puzzleInput:TStringArray);
    function processNode(startPoint,endPoint:TPoint):TPoint;
    function findQueueEntryPosition(queueEntry:TQueueEntry):integer;
    procedure updateRisk(queueEntry,sourceEntry:TQueueEntry);
    procedure removeFromQueue(queueEntry:TQueueEntry);
    procedure addToQueue(queueEntry:TQueueEntry);
    function visitable(point,refPoint:TPoint):boolean;
    function getLeastRiskyQueueEntry:TQueueEntry;
    function findQueueEntry(atPoint:TPoint):TQueueEntry;
    property queueLength:integer read getQueueLength;
    property leastRiskyQueueEntry: TQueueEntry read getLeastRiskyQueueEntry;
    property finishPoint:TPoint read fFinishPoint write fFinishPoint;
    public
    constructor create(puzzleInput:TStringArray);
    procedure findShortestPath(startPoint,endPoint:TPoint);
    property shortest:integer read fShortest;
    property mapDimensions: TPoint read getMapDimensions;
  end;

implementation

{ TRouteFinder }

constructor TRouteFinder.create(puzzleInput: TStringArray);
begin
  fMap:=T3DIntMap.create;
  fQueue:=TQueue.create;
  fNodePos:=TPoint.Create(0,0);
  initializeMap(puzzleInput);
end;

function TRouteFinder.getQueueLength: integer;
begin
  result:=length(fQueue);
end;

function TRouteFinder.getMapDimensions: TPoint;
begin
  result.X:=length(fMap);
  if (length(fMap)>0) then result.Y:=length(fMap[0]) else result.Y:=0;
end;

procedure TRouteFinder.initializeMap(puzzleInput: TStringArray);
var
  puzzleWidth,puzzleHeight,x,y:integer;
  sLine:string;
begin
  puzzleHeight:=length(puzzleInput);
  if puzzleHeight > 0 then puzzleWidth:=length(puzzleInput[0]);
  setLength(fMap,puzzleWidth,puzzleHeight,2);
  for y:=0 to pred(puzzleHeight) do
    begin
    sLine:=puzzleInput[y];
    for x:=0 to pred(puzzleWidth) do
      fMap[x][y][0]:=sLine.Substring(x,1).ToInteger;
      fMap[x][y][1]:=0;//unvisited
    end;
end;

function TRouteFinder.processNode(startPoint,endPoint: TPoint):TPoint;
var
  startEntry,visitedEntry:TQueueEntry;
  visitingPoint:TPoint;
  xOffset,yOffset:integer;
  updatedRisk:integer;
begin
  startEntry:=findQueueEntry(startPoint);
  if (startPoint = endPoint) and (startEntry <> nil)
    then fShortest:= startEntry.fRisk
  else
    begin
    fMap[startPoint.X][startPoint.Y][1]:=1;
    for xOffset:=-1 to 1 do
      for yOffset:= -1 to 1 do
        begin
        visitingPoint.X := startEntry.nodePos.X + xOffset;
        visitingPoint.Y := startEntry.nodePos.Y + yOffset;
        if visitable(visitingPoint,startEntry.nodePos) then
          begin
          //calculate the new risk value and add a queue entry
          updatedRisk:=startEntry.risk + fMap[visitingPoint.X][visitingPoint.Y][0];
          visitedEntry:= findQueueEntry(visitingPoint);
          if visitedEntry = nil
            then addToQueue(TQueueEntry.create(visitingPoint,mapDimensions,updatedRisk,startEntry.nodeId))
          else updateRisk(visitedEntry,startEntry);
          end;
        end;
    writeln('remove from queue ' +startPoint.X.ToString+':'+startPoint.Y.ToString);
    removeFromQueue(startEntry);
    result:=leastRiskyQueueEntry.fNodePos;
    end;
end;

procedure TRouteFinder.findShortestPath(startPoint, endPoint: TPoint);
var
  endEntry:TQueueEntry;
  leastRiskPoint:TPoint;
  done:boolean;
begin
  finishPoint:=endPoint;
  addToQueue(TQueueEntry.create(startPoint,mapDimensions));
  leastRiskPoint:= startPoint;
    repeat
    leastRiskPoint:=processNode(leastRiskPoint,endPoint);
    done:=leastRiskPoint = endPoint;
    until done;
  endEntry:= findQueueEntry(endPoint);
  if endEntry <> nil then
  fShortest:=endEntry.risk;

end;

function TRouteFinder.findQueueEntry(atPoint: TPoint): TQueueEntry;
var
  idOfEntry:integer;
  index:integer;
begin
  result:= nil;
  //get the id of the queue entry and then find it
  if (atPoint.X >= mapDimensions.X) or (atPoint.Y >= mapDimensions.Y)
  or (atPoint.X < 0) or (atPoint.Y < 0)
    then exit;
  idOfEntry:= (atPoint.Y * mapDimensions.X) + atPoint.X;
  for index:=0 to pred(length(fQueue)) do
    begin
    if fQueue[index].fNodeId = idOfEntry then
      begin
        result:=fQueue[index];
        exit;
      end;
    end;
end;

function TRouteFinder.findQueueEntryPosition(queueEntry: TQueueEntry): integer;
var
  index:integer;
begin
  result:=-1;
  for index:=0 to pred(queueLength) do
    begin
    if fQueue[index] = queueEntry then
      begin
      result:=index;
      exit;
      end;
    end;
end;

procedure TRouteFinder.updateRisk(queueEntry,sourceEntry: TQueueEntry);
var
  riskToVisitingPoint:integer;
begin
  riskToVisitingPoint:= sourceEntry.risk + fMap[queueEntry.nodePos.X][queueEntry.nodePos.Y][0];
  if queueEntry.infinity or (riskToVisitingPoint < queueEntry.risk)
    then
      begin
      queueEntry.infinity:=false;
      queueEntry.risk:=riskToVisitingPoint;
      queueEntry.source:=sourceEntry.nodeId;
      end;
end;

procedure TRouteFinder.removeFromQueue(queueEntry: TQueueEntry);
var
  index,deleteIndex:integer;
begin
  deleteIndex:=findQueueEntryPosition(queueEntry);
  if deleteIndex > -1 then
    begin
    if (deleteIndex < pred(queueLength)) then
      begin
      for index:=deleteIndex to pred(pred(queueLength)) do
        fQueue[index]:=fQueue[index + 1];
      end;
    setLength(fQueue, pred(queueLength));
    end;
end;

procedure TRouteFinder.addToQueue(queueEntry: TQueueEntry);
begin
  setLength(fQueue,queueLength+1);
  fQueue[queueLength - 1]:=queueEntry;
end;

function TRouteFinder.visitable(point, refPoint: TPoint): boolean;
begin
  result:= ((point.X = refPoint.X)
    or (point.Y = refPoint.Y))
    and not ((point.X = refPoint.X) and (point.Y = refPoint.Y))
    and (point.X > -1) and (point.Y > -1)
    and (point.X < mapDimensions.X)
    and (point.Y < mapDimensions.Y)
    and (fMap[point.X][point.Y][1] <> 1)
end;

function TRouteFinder.getLeastRiskyQueueEntry: TQueueEntry;
var
  index:integer;
  minRisk:integer;
  minRiskIndex:integer;
  firstFound:boolean;
  thisItem:TQueueEntry;
begin
  firstFound:=false;
  result:=nil;
  minRisk:=-1;
  minRiskIndex:=-1;
  if queueLength = 0 then exit;
  for index:=0 to pred(queueLength) do
    if (not fQueue[index].infinity)
    and ((not fQueue[index].visited) or (fQueue[index].nodePos = finishPoint))
    and ((firstFound = false) or (fQueue[index].risk < minRisk)) then
      begin
      firstFound:=true;
      minRisk:=fQueue[index].risk;
      minRiskIndex:=index;
      end;
  if minRiskIndex > -1 then result:=fQueue[minRiskIndex];
end;

{ TQueueEntry }

constructor TQueueEntry.create(position,mapDimensions:TPoint; initialRisk:integer=0;source:integer=0);
begin
  fInfinity:= initialRisk = 0;
  fNodeId:= (position.Y * mapDimensions.X) + position.X;
  fNodePos:= position;
  fVisited:=false;
  fSource:= source;
  fRisk:= initialRisk;
end;

end.

