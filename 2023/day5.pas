unit day5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFive}
  TDayFive = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFive }

constructor TDayFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 5',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFive.runPartOne;
begin
  results.Clear;
end;

procedure TDayFive.runPartTwo;
begin
  results.Clear;
end;

end.


