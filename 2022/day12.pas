unit day12;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;

type

  { TDayTwelve }

  TDayTwelve = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }

constructor TDayTwelve.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 12', paintbox_);
end;

procedure TDayTwelve.runPartOne;
begin

end;

procedure TDayTwelve.runPartTwo;
begin

end;

end.

