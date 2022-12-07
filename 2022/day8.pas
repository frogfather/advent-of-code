unit day8;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayEight }

TDayEight = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;
implementation

{ TDayEight }

constructor TDayEight.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 8',paintbox_);
end;

procedure TDayEight.runPartOne;
begin

end;

procedure TDayEight.runPartTwo;
begin

end;

end.

