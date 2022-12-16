unit day15;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
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
begin

end;

procedure TDayFifteen.runPartTwo;
begin

end;

end.

