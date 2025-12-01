unit day25;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwentyFive}
  TDayTwentyFive = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwentyFive }

constructor TDayTwentyFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 25',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyFive.runPartOne;
begin
  results.Clear;
end;

procedure TDayTwentyFive.runPartTwo;
begin
  results.Clear;
end;


end.

                
