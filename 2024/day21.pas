unit day21;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TKeypad }

  TKeypad = class(TInterfacedObject)
  private
  fButtons:TStringArray;
  fPosition:TPoint;
  fSequence:String;
  fIsKeypad:boolean;
  function getRow(button:String):integer;
  function getCol(button:String):integer;
  procedure moveXBy(x:integer);
  procedure moveYBy(y:integer);
  public
  constructor create(isKeypad:Boolean=true);
  procedure moveToAndPress(_end:string);
  property sequence: string read fSequence;
  property position: TPoint read fPosition;
  property isKeypad: boolean read fIsKeypad;
  end;

  { TDayTwentyOne}
  TDayTwentyOne = class(TAocPuzzle)
  private
  function getSequence(input:string):string;
  function getProduct(target,sequence:string):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TKeypad }

function TKeypad.getRow(button: String): integer;
begin
  case button of
  '7','8','9','^': result:=0;
  '4','5','6','<','v','>': result:=1;
  '1','2','3': result:=2;
      '0','A':result:=3;
  end;
  if not isKeypad and (button = 'A') then result:=0;
end;

function TKeypad.getCol(button: String): integer;
begin
  case button of
  '7','4','1','<':    result:=0;
  '8','5','2','0','^','v':result:=1;
  '9','6','3','A','>':result:=2;
  end;
end;

procedure TKeypad.moveXBy(x: integer);
var
  character:string;
  distance,increment:integer;
begin
  distance:=x;
  if x < 0 then
    begin
    character:='<';
    increment:=1;
    end else
    begin
    character:='>';
    increment:=-1;
    end;
  while distance <> 0 do
    begin
    fSequence:=fSequence+character;
    distance:=distance + increment;
    fPosition.X:=fPosition.X - increment;
    //check

    end;
end;

procedure TKeypad.moveYBy(y: integer);
var
  character:string;
  distance,increment:integer;
begin
  distance:=y;
  if y < 0 then
    begin
    character:='^';
    increment:=1;
    end else
    begin
    character:='v';
    increment:=-1;
    end;
  while distance <> 0 do
    begin
    fSequence:=fSequence+character;
    distance:=distance + increment;
    fPosition.Y:=fPosition.Y - increment;
    end;
end;

procedure TKeypad.moveToAndPress(_end: string);
var
  destination,offset:TPoint;
begin
  destination:=TPoint.create(getCol(_end),getRow(_end));
  offset:=TPoint.Create(destination.X - fPosition.X,destination.Y - fPosition.Y);
  //If we're on the top row and it's not a keypad or the bottom row and it is a keypad move y first
  //to avoid hitting the empty space
  if (isKeypad and (fPosition.Y = pred(fButtons.size)))
  or (not isKeypad and (fPosition.Y = 0)) then
    begin
    moveYBy(offset.Y);
    moveXBy(offset.X);
    end else
    begin
    moveXBy(offset.X);
    moveYBy(offset.Y);
    end;
  fSequence:=fSequence+'A';
end;

constructor TKeypad.create(isKeypad:Boolean);
begin
  if isKeypad then fPosition:=TPoint.Create(2,3)
  else fPosition:=TPoint.Create(2,0);
  fSequence:='';
  fisKeypad:=isKeypad;
  fButtons:=TStringArray.create;
  if isKeypad then
    begin
    fButtons.push('789');
    fButtons.push('456');
    fButtons.push('123');
    fButtons.push('_0A');
    end else
    begin
    fButtons.push('_^A');
    fButtons.push('<v>');
    end;
end;

{ TDayTwentyOne }

function TDayTwentyOne.getSequence(input: string): string;
var
  keypad:TKeypad;
  robot1,robot2:TKeypad;
  keypadSequence,robot1Sequence:string;
  index:integer;
begin
  keypad:=TKeypad.create;
  for index:=0 to pred(input.Length) do
    keypad.moveToAndPress(input.Substring(index,1));
  keypadSequence:=keypad.sequence;
  results.add('kp '+keypadSequence);
  robot1:=TKeypad.create(false);
  for index:= 0 to pred(keypadSequence.Length) do
    robot1.moveToAndPress(keypadSequence.Substring(index,1));
  robot1Sequence:=robot1.sequence;
  results.add('r1 '+robot1sequence);
  robot2:=TKeypad.create(false);
  for index:= 0 to pred(robot1Sequence.Length) do
    robot2.moveToAndPress(robot1Sequence.Substring(index,1));
  result:=robot2.sequence;
  results.add(input+': '+robot2.sequence);
end;

function TDayTwentyOne.getProduct(target, sequence: string): integer;
begin
  results.add(target.Substring(0,target.Length - 1)+' '+sequence.Length.toString);
  result:=target.Substring(0,target.Length - 1).ToInteger * sequence.Length;
end;

constructor TDayTwentyOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 21',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyOne.runPartOne;
var
  index:integer;
  total:integer;
begin
  results.Clear;
  total:=0;
  for index:=0 to pred(puzzleInputLines.size) do
    total:=total+getProduct(puzzleInputLines[index],getSequence(puzzleInputLines[index]));
  results.add('Total is '+total.ToString);
end;

procedure TDayTwentyOne.runPartTwo;
begin
  results.Clear;
end;


end.

                
