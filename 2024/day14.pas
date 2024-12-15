unit day14;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TRobot }

  TRobot = class
  private
  fpos:TPoint;
  fvelocity:TPoint;
  flimits:TRect;
  public
  constructor create(position,velocity:TPoint;limits:TRect);
  procedure move;
  property position:TPoint read fPos;
  end;

  TRobots = array of TRobot;

  { TRobotsHelper }
  TRobotsHelper = type helper for TRobots
  function size: integer;
  function push(element:TRobot):integer;
  procedure clear;
  end;

  { TDayFourteen}
  TDayFourteen = class(TAocPuzzle)
  private
  procedure loadRobots(grid:TRect);
  procedure runRobots(times:integer);
  procedure printRobots;
  function getQuadrant(quadrantNo:integer;grid:TRect):TRect;
  function findRobotsInQuadrant(quadrantNo:integer;grid:TRect):integer;
  function noOverlaps:boolean;
  procedure paintgrid(sender:TObject);
  function robotAtPos(pos:TPoint):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
  var
    robots:TRobots;
    grid:TRect;
    testGrid:TRect;
{ TRobot }

constructor TRobot.create(position,velocity:TPoint;limits: TRect);
begin
  fPos:=position;
  fVelocity:=velocity;
  fLimits:=limits;
end;

procedure TRobot.move;
var
  newX,newY:integer;
begin
  newX:=fPos.X + fVelocity.X;
  newY:=fPos.Y + fVelocity.Y;
  if (newX < fLimits.Left) then newX:=fLimits.Right + newX + 1;
  if (newX > fLimits.Right) then newX:= newX - (fLimits.Right + 1);
  if (newY < fLimits.Top) then newY:=fLimits.Bottom + newY +1;
  if (newY > fLimits.Bottom) then newY:=newY - (fLimits.Bottom + 1);
  fPos.X:=newX;
  fPos.Y:=newY;
end;

{ TRobotsHelper }

function TRobotsHelper.size: integer;
begin
  result:=length(self);
end;

function TRobotsHelper.push(element: TRobot): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

procedure TRobotsHelper.clear;
begin
  setLength(self,0);
end;

{ TDayFourteen }

procedure TDayFourteen.loadRobots(grid:TRect);
var
  index:integer;
  pos,vel:string;
  posX,posY,velX,velY:integer;
begin
  robots.clear;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    if puzzleInputLines[index]='' then exit;
    //each line has position and velocity separated by space
    pos:= puzzleInputLines[index].Trim.Split(' ')[0].Trim.Split('=')[1].Trim;
    vel:= puzzleInputLines[index].Trim.Split(' ')[1].Trim.Split('=')[1].Trim;
    posX:=pos.Split(',')[0].Trim.ToInteger;
    posY:=pos.Split(',')[1].Trim.ToInteger;
    velX:=vel.Split(',')[0].Trim.ToInteger;
    vely:=vel.Split(',')[1].Trim.ToInteger;
    robots.push(TRobot.create(TPoint.Create(posX,posY),TPoint.Create(velX,velY),grid));
    end;
end;

procedure TDayFourteen.runRobots(times: integer);
var
  runCount,robotNo:integer;
begin
  for runCount:=0 to pred(times) do
    for robotNo:=0 to pred(robots.size) do
      robots[robotNo].move;
end;

procedure TDayFourteen.printRobots;
var
  robotNo:integer;
begin
  for robotNo:=0 to pred(robots.size)do
    begin
    results.Add('Robot '+robotNo.toString+' is at '+robots[robotNo].position.X.toString+':'+robots[robotNo].position.Y.toString);
    end;
end;

function TDayFourteen.getQuadrant(quadrantNo: integer; grid: TRect): TRect;
var
  gridMiddleH,gridMiddleV:integer;
begin
  result:=TRect.Create(0,0,0,0);
  //quadrant 1 is top left, 2 is bottom left, 3 is top right, 4 is bottom right
  gridMiddleH:=(grid.Right - grid.Left) div 2;
  gridMiddleV:=(grid.Bottom- grid.Top) div 2;
  case quadrantNo of
  1:
    begin
    result.Left:=grid.Left;
    result.Top:=grid.Top;
    result.Right:=gridMiddleH - 1;
    result.Bottom:=gridMiddleV - 1;
    end;
  2:
    begin
    result.Left:=grid.Left;
    result.Top:=gridMiddleV + 1;
    result.Right:=gridMiddleH - 1;
    result.Bottom:=grid.Bottom;
    end;
  3:
    begin
    result.Left:=gridMiddleH + 1;
    result.Top:=grid.Top;
    result.Right:=grid.Right;
    result.Bottom:=gridMiddleV - 1;
    end;
  4:
    begin
    result.Left:=gridMiddleH + 1;
    result.Top:=gridMiddleV + 1;
    result.Right:=grid.Right;
    result.Bottom:=grid.Bottom;
    end;
  end;
end;

function TDayFourteen.findRobotsInQuadrant(quadrantNo: integer; grid: TRect
  ): integer;
var
  quadrant:TRect;
  index:integer;
  robotPosition:TPoint;
begin
  result:=0;
  quadrant:=getQuadrant(quadrantNo,grid);
  for index:=0 to pred(robots.size) do
    begin
    robotPosition:=robots[index].position;
    if (robotPosition.X >= quadrant.Left)
    and(robotPosition.X <= quadrant.Right)
    and(robotPosition.Y >= quadrant.Top)
    and(robotPosition.Y <= quadrant.Bottom)
    then result:=result+1;
    end;
end;

function TDayFourteen.noOverlaps: boolean;
var
  index:integer;
  pointsSeen:TPointArray;
begin
  result:=false;
  pointsSeen:=TPointArray.create;
  for index:=0 to pred(robots.size) do
    begin
    if (pointsSeen.indexOf(robots[index].position) > -1) then exit;
    pointsSeen.push(robots[index].position);
    end;
  result:=true;
end;

procedure TDayFourteen.paintgrid(sender:TObject);
var
  cellWidth,cellheight:integer;
  col,row:integer;
begin
  if sender is TPaintbox then with sender as TPaintbox do
    begin
    canvas.Brush.Color:=clBlack;
    canvas.Rectangle(paintbox.ClientRect);
    cellHeight:=paintbox.ClientRect.Height div grid.Height;
    cellWidth:=cellheight;
    for row:=0 to pred(grid.Height) do
      for col:=0 to pred(grid.Width) do
        begin
        if robotAtPos(TPoint.Create(col,row)) then
          canvas.Brush.Color:=clGreen else canvas.brush.color:= clBlack;
        canvas.FillRect(cellWidth*col,cellHeight*row,cellWidth*(col+1),cellheight*(row+1));
        end;
    end;
end;

function TDayFourteen.robotAtPos(pos: TPoint): boolean;
var
  index:integer;
begin
  result:=true;
  for index:=0 to pred(robots.size) do
    if (robots[index].position = pos) then exit;
  result:=false;
end;



constructor TDayFourteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 14',paintbox_);
robots:=TRobots.create;
grid:=TRect.Create(0,0,100,102);
testGrid:=TRect.Create(0,0,10,6);
paintbox.OnPaint:=@paintGrid;
//parent loads the file as a string and converts to string array;
end;

procedure TDayFourteen.runPartOne;
var
  quadNo:integer;
  factor:integer;
  robotCount:integer;
begin
  results.Clear;
  factor:=1;
  loadRobots(grid);
  runRobots(100);
  for quadNo:= 1 to 4 do
    begin
    robotCount:=findRobotsInQuadrant(quadNo,grid);
    results.add('quadrant '+quadNo.toString+': '+robotCount.toString);
    factor:=factor * robotCount;
    end;
  results.add('Safety factor is '+factor.toString);
end;

procedure TDayFourteen.runPartTwo;
var
  steps:integer;
  done:boolean;
begin
  results.Clear;
  loadRobots(grid);
  done:=false;
  steps:=0;
  while not done do
    begin
    runRobots(1);
    steps:=steps+1;
    done:=noOverlaps or (steps > 10000);
    end;
  results.add('steps '+steps.ToString);
end;


end.

                
