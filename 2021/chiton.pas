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
    constructor create(position,mapDimensions:TPoint);
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
    fMap:T2DIntMap;
    fQueue:TQueue;
    fShortest:integer;
    procedure initializeMapAndQueue(puzzleInput:TStringArray);
    function getQueueLength: integer;
    function getMapDimensions:TPoint;
    procedure processNode(startEntry,endEntry:TQueueEntry);
    function findQueueEntryPosition(queueEntry:TQueueEntry):integer;
    procedure updateRisk(queueEntry,sourceEntry:TQueueEntry;position:TPoint);
    procedure removeFromQueue(queueEntry:TQueueEntry);
    function getLeastRiskyQueueEntry:TQueueEntry;
    function findQueueEntry(atPoint:TPoint):TQueueEntry;
    property queueLength:integer read getQueueLength;
    property leastRiskyQueueEntry: TQueueEntry read getLeastRiskyQueueEntry;
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
  fMap:=T2DIntMap.create;
  fQueue:=TQueue.create;
  initializeMapAndQueue(puzzleInput);
end;

procedure TRouteFinder.initializeMapAndQueue(puzzleInput: TStringArray);
var
  puzzleWidth,puzzleHeight,x,y:integer;
  sLine:string;
  nodePosition:TPoint;
begin
  puzzleHeight:=length(puzzleInput);
  if puzzleHeight > 0 then puzzleWidth:=length(puzzleInput[0]);
  setLength(fMap,puzzleWidth,puzzleHeight);
  for y:=0 to pred(puzzleHeight) do
    begin
    sLine:=puzzleInput[y];
    for x:=0 to pred(puzzleWidth) do
      begin
      fMap[x][y]:=sLine.Substring(x,1).ToInteger;
      nodePosition.X:=x;
      nodePosition.Y:=y;
      setLength(fQueue,length(fQueue)+1);
      fQueue[pred(length(fQueue))]:=TQueueEntry.create(nodePosition,mapDimensions);
      end;
    end;
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

procedure TRouteFinder.processNode(startEntry,endEntry: TQueueEntry);
var
  visitingQueueEntry,leastRQEntry:TQueueEntry;
  visitingPoint:TPoint;
  xOffset,yOffset:integer;
begin
  startEntry.visited:=true;
  if startEntry = endEntry
    then fShortest:= startEntry.fRisk
  else
    begin
    for xOffset:=-1 to 1 do
      for yOffset:= -1 to 1 do
        begin
        visitingPoint.X := startEntry.nodePos.X + xOffset;
        visitingPoint.Y := startEntry.nodePos.Y + yOffset;
        if ((visitingPoint.X = startEntry.nodePos.X)
          or (visitingPoint.Y = startEntry.nodePos.Y))
          and not ((visitingPoint.X = startEntry.nodePos.X) and (visitingPoint.Y = startEntry.nodePos.Y)) then
          begin
          visitingQueueEntry:= findQueueEntry(visitingPoint);
          if (visitingQueueEntry <> nil)
          and (visitingQueueEntry.visited = false)
            then updateRisk(visitingQueueEntry,startEntry,visitingPoint);
          end;
        end;
    removeFromQueue(startEntry);
    //now find the queue entry with the lowest risk
    //we really should sort the entries but that requires a bit
    //of refactoring
    leastRQEntry:=leastRiskyQueueEntry;
    processNode(leastRiskyQueueEntry, endEntry);
    end;
end;

procedure TRouteFinder.findShortestPath(startPoint,endPoint: TPoint);
var
  startEntry,endEntry:TQueueEntry;
begin
  startEntry:=findQueueEntry(startPoint);
  endEntry:=findQueueEntry(endPoint);
  if (startEntry = nil) or (endEntry = nil) then exit;
  processNode(startEntry,endEntry);
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

procedure TRouteFinder.updateRisk(queueEntry,sourceEntry: TQueueEntry; position: TPoint);
var
  riskToVisitingPoint:integer;
begin
  riskToVisitingPoint:= sourceEntry.risk + fMap[position.X][position.Y];
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
  index,deleteIndex,lengthOfQueue:integer;
begin
  deleteIndex:=findQueueEntryPosition(queueEntry);
  if deleteIndex > -1 then
    begin
    if (deleteIndex < pred(lengthOfQueue)) then
      begin
      for index:=deleteIndex to pred(pred(queueLength)) do
        fQueue[index]:=fQueue[index + 1];
      end;
    setLength(fQueue, length(fQueue) -1);
    end;
end;

function TRouteFinder.getLeastRiskyQueueEntry: TQueueEntry;
var
  index:integer;
  minRisk:integer;
  minRiskIndex:integer;
  firstFound:boolean;
begin
  firstFound:=false;
  result:=nil;
  minRisk:=-1;
  minRiskIndex:=-1;
  if queueLength = 0 then exit;
  for index:=0 to pred(queueLength) do
    if (not fQueue[index].infinity)
    and (not fQueue[index].visited)
    and ((firstFound = false) or (fQueue[index].risk < minRisk)) then
      begin
      firstFound:=true;
      minRisk:=fQueue[index].risk;
      minRiskIndex:=index;
      end;
  if minRiskIndex > -1 then result:=fQueue[minRiskIndex];
end;



{ TQueueEntry }

constructor TQueueEntry.create(position,mapDimensions:TPoint);
begin
  fInfinity:= true;
  fNodeId:= (position.Y * mapDimensions.X) + position.X;
  fNodePos:= position;
  fVisited:=false;
  fSource:= -1; //not set
  fRisk:= 0; //should be infinity but signal this with infinity flag
end;

end.

