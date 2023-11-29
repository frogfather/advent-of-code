unit day4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFour}
  TDayFour = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }

constructor TDayFour.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 4',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFour.runPartOne;
begin
  results.Clear;
end;

procedure TDayFour.runPartTwo;
begin
  results.Clear;
end;

end.


