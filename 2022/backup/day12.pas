unit day12;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;

type

  { TDay Elevent }

  { TDayEleven }

  TDayEleven = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayEleven }

constructor TDayEleven.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 11', paintbox_);
end;

procedure TDayEleven.runPartOne;
begin

end;

procedure TDayEleven.runPartTwo;
begin

end;

end.

