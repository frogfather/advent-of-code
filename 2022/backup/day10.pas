unit day10;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayTen }

TDayTen = class(TAocPuzzle)
  private
     fScreen:T2DStringArray;
     fTotalValue:integer;
     procedure drawScreen(sender:TObject);
     procedure runPuzzle;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;
implementation

{ TDayTen }

procedure TDayTen.drawScreen(sender: TObject);
var
  screenX,screenY:integer;
begin
  if sender is TPaintbox then with sender as TPaintbox do
  begin
  canvas.font.Size:=24;
  canvas.Brush.color:=clblack;
  canvas.Rectangle(0,0,canvas.Width,canvas.Height);
  canvas.Font.Color:=clLime;
  for screenY:=0 to pred(Length(fScreen[0])) do
    for screenX:=0 to pred(length(fScreen)) do
      canvas.TextOut(screenX*canvas.TextWidth('#'),screenY*canvas.TextHeight('#'),fScreen[screenX][screenY]);
  end;
end;

procedure TDayTen.runPuzzle;
  const cyclesToAdd:TIntArray = (20,60,100,140,180,220);
  var
  index:integer;
  instruction:TStringArray;
  X,signalStrength,cycle,addValue:integer;
  done,getNext:boolean;
  pixel,displayX,displayY:integer;

begin
  X:=1;
  cycle:=0;
  index:=0;
  addValue:=0;
  done:=false;
  getNext:=true;
  fTotalValue:=0;
  while not done do
    begin
    cycle:=cycle + 1;
    if (getNext = true) then
      begin
      X:=X+addValue;
      instruction:=puzzleInputLines[index].Split(' ');
      if (instruction[0] <> 'noop') then
        begin
        getNext:=false;
        addValue:=instruction[1].toInteger;
        end else addValue:=0;
      index:=index+1;
      end else getNext:= true;
    done:=(index = puzzleInputLines.size) and (getNext = true);
    //Used in part 1
    signalStrength:=X * cycle;
    if (cyclesToAdd.indexOf(cycle) > -1) then
       fTotalValue:=totalValue + signalStrength;

    //Used in part 2
    //if the current pixel ((cycle-1) mod 40) is in range (x-1, x+1) then we draw # otherwise we draw '.'
    pixel:=(cycle-1) mod 40;
    if (pixel >= x-1)and(pixel <= x+1) then
       fScreen[displayX][displayY]:='#'
    else fScreen[displayX][displayY]:='.';
    displayX:=displayX + 1;
    if displayX > 39 then
      begin
      displayY:=displayY + 1;
      displayX:=0;
      end;
    end;
end;

constructor TDayTen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 10',paintbox_);
  if (paintbox <> nil) then Paintbox.OnPaint:=@drawScreen;
  fScreen:=T2DStringArray.create;
  setLength(fScreen,40,6);
end;

procedure TDayTen.runPartOne;

begin
  runPuzzle;
  results.add('totalValue '+fTotalValue.toString);
end;

procedure TDayTen.runPartTwo;

begin
  runPuzzle;
  results.Add('Click Visualise to see the answer');
end;

end.

