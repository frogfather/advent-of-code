unit day11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayEleven}
  TDayEleven = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayEleven }

constructor TDayEleven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 11',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayEleven.runPartOne;
begin
  results.Clear;
end;

procedure TDayEleven.runPartTwo;
begin
  results.Clear;
end;

end.


