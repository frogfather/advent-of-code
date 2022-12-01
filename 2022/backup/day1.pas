unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  fName:string;
  procedure runPartOne; override;
  procedure runPartTwo; override;
  procedure doPaint(sender:TObject);reintroduce;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,paintbox_);
//if paintbox <> nil then paintbox.OnPaint:=@doPaint;
fName:= 'Day 1';
//parent loads file as string;
end;

//Private methods
procedure TDayOne.runPartOne;
begin
  DebugLn('run part one');
end;

procedure TDayOne.runPartTwo;
begin
  DebugLn('run part two');
end;

procedure TDayOne.doPaint(sender: TObject);
var
  topLeft,topRight,bottomLeft,bottomRight:TPoint;
begin
  topLeft.X:= 0;
  topLeft.Y:= 0;
  topRight.X:=paintbox.Canvas.Width;
  topRight.Y:=0;
  bottomLeft.X:=0;
  bottomLeft.Y:=paintbox.Canvas.Height;
  bottomRight.X:= paintbox.Canvas.width;
  bottomRight.Y:= paintbox.Canvas.height;
  with paintbox.Canvas do
   begin
     pen.Width:=6;
     pen.Color:=clGreen;
     Line(topLeft,bottomRight);
     Line(bottomLeft,topRight);
   end;
end;


end.

