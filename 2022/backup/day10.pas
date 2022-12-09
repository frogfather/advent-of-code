unit day10;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayTen }

TDayTen = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;
implementation

{ TDayTen }

function TDayTen.foo(const Data1, Data2: integer): Integer;
begin
  result:= 0;
end;

constructor TDayTen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 10',paintbox_);
end;

procedure TDayTen.runPartOne;
begin

end;

procedure TDayTen.runPartTwo;
begin

end;

end.

