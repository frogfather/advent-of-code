unit day3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayThree}
  TDayThree = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }

constructor TDayThree.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 3',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThree.runPartOne;
begin
  results.Clear;
end;

procedure TDayThree.runPartTwo;
begin
  results.Clear;
end;

end.


