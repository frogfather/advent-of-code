unit day12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwelve}
  TDayTwelve = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }

constructor TDayTwelve.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 12',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwelve.runPartOne;
begin
  results.Clear;
end;

procedure TDayTwelve.runPartTwo;
begin
  results.Clear;
end;


end.

                
