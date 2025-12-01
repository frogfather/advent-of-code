unit day9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNine}
  TDayNine = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayNine }

constructor TDayNine.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 9',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayNine.runPartOne;
begin
  results.Clear;
end;

procedure TDayNine.runPartTwo;
begin
  results.Clear;
end;


end.

                
