unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, aocPuzzle,LazLogger,graphics;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  fPaintBox:TPaintbox;
  fName:string;
  procedure runPartOne; override;
  procedure runPartTwo; override;
  procedure paint(sender:TObject);
  public
  constructor create(filename:string; paintbox:TPaintbox = nil);
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox:TPaintbox);
begin
inherited create(filename);
fPaintbox:=paintbox;
fName:= 'Day 1';
if (fPaintbox <> nil) then fPaintbox.OnPaint:=@paint;
//parent loads file as string;
end;

//Private methods
procedure TDayOne.runPartOne;
begin
  fPaintbox.Repaint;
  DebugLn('run part one');
end;

procedure TDayOne.runPartTwo;
begin
  DebugLn('run part two');
end;

procedure TDayOne.paint(sender: TObject);
begin
  with fPaintbox do
    begin
    Canvas.Brush.Color:= clRed;
    Canvas.Rectangle(0,0,100,100);
    Canvas.TextOut(0,0,fName);
    end;
end;

end.

