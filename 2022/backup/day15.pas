unit day15;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,math;
type

  { TDayFifteen }

  TDayFifteen = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayFifteen }

constructor TDayFifteen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 15', paintbox_);
end;

procedure TDayFifteen.runPartOne;
var
  pointArray:TPointArray;
  index:integer;
begin
  pointArray:=TPointArray.create;
  for index:=0 to 300 do
  pointArray.push(TPoint.Create(random(200),random(400)));
  sort(pointArray,pointArray.size);
end;

procedure TDayFifteen.runPartTwo;
begin

end;

end.

