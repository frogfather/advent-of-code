unit day10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTen}
  TDayTen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTen }

constructor TDayTen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 10',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTen.runPartOne;
begin
  results.Clear;
end;

procedure TDayTen.runPartTwo;
begin
  results.Clear;
end;


end.

                
