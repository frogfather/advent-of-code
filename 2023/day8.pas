unit day8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayEight}
  TDayEight = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayEight }

constructor TDayEight.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 8',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayEight.runPartOne;
begin
  results.Clear;
end;

procedure TDayEight.runPartTwo;
begin
  results.Clear;
end;

end.


