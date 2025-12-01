unit day13;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  TDoublePoint = record
  X:double;
  Y:double;
  end;

  { TDayThirteen}
  TDayThirteen = class(TAocPuzzle)
  private
  function minCostOfPrize(machine:TStringArray;useOffset:boolean=false):double;
  function getButtonMoves(input:String):TDoublePoint;
  function getPrizeCoords(input:String):TDoublePoint;
  function getPresses(buttonA,buttonB,prize:TDoublePoint;offset:boolean=false):TDoublePoint;
  function getTokenCount(usingOffset:boolean=false):double;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThirteen }

function TDayThirteen.minCostOfPrize(machine: TStringArray;useOffset:boolean=false): double;
var
  buttonA,buttonB:TDoublePoint;
  prize,presses:TDoublePoint;
begin
  buttonA:=getButtonMoves(machine[0]);
  buttonB:=getButtonMoves(machine[1]);
  prize:=getPrizeCoords(machine[2]);
  presses:=getPresses(buttonA,buttonB,prize,useOffset);
  result:=(3*presses.X) + presses.Y;
end;

function TDayThirteen.getButtonMoves(input: String): TDoublePoint;
var
  x,y:double;
begin
  x:=input.Split(':')[1].Trim.Split(',')[0].Trim.Split('+')[1].Trim.ToDouble;
  y:=input.Split(':')[1].Trim.Split(',')[1].Trim.Split('+')[1].Trim.ToDouble;
  result.X:=x;
  result.Y:=y;
end;

function TDayThirteen.getPrizeCoords(input: String): TDoublePoint;
var
  x,y:double;
begin
  x:=input.Split(':')[1].Trim.Split(',')[0].Trim.Split('=')[1].Trim.ToDouble;
  y:=input.Split(':')[1].Trim.Split(',')[1].Trim.Split('=')[1].Trim.ToDouble;
  result.X:=x;
  result.Y:=y;
end;

function TDayThirteen.getPresses(buttonA, buttonB, prize: TDoublePoint;offset:boolean): TDoublePoint;
var
  a,b:double;
  modifiedPrizeX,modifiedPrizeY:double;
  underHundred:boolean;
begin
  result.X:=0;
  result.Y:=0;
  if offset then modifiedPrizeX := prize.X + 10000000000000 else modifiedPrizeX:=prize.X;
  if offset then modifiedPrizeY := prize.Y + 10000000000000 else modifiedPrizeY:=prize.Y;
  a:= ((modifiedPrizeX*buttonB.Y)-(buttonB.X*modifiedPrizeY))/((buttonB.Y*buttonA.X)-(buttonA.Y*buttonB.X));
  b:= (modifiedPrizeX-a*buttonA.X)/buttonB.X;
  underHundred:= (a <= 100) and (b <= 100);
  if ((frac(a) = 0))and (underHundred or offset) then
    begin
    result.X:=int(a);
    result.Y:=int(b);
    end;
end;

function TDayThirteen.getTokenCount(usingOffset: boolean): double;
var
  index:integer;
  machine:TStringArray;
  currentLine:string;
begin
  result:=0;
  machine:=TStringArray.create;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[index].Trim;
    if (currentLine <> '')
      then machine.push(puzzleInputLines[index]);
    if (currentLine = '') or (index = pred(puzzleInputLines.size))
      then
      begin
        result:=result + minCostOfPrize(machine,usingOffset);
        machine.clear;
      end;
    end;
end;

constructor TDayThirteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 13',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThirteen.runPartOne;

begin
  results.Clear;
  results.add('Total is '+getTokenCount.ToString)
end;

procedure TDayThirteen.runPartTwo;
begin
  results.Clear;
  results.add('Total is '+getTokenCount(true).ToString)
end;


end.

                
