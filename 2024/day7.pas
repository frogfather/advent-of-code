unit day7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySeven}
  TDaySeven = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDaySeven }

constructor TDaySeven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 7',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDaySeven.runPartOne;
begin
  results.Clear;
end;

procedure TDaySeven.runPartTwo;
begin
  results.Clear;
end;


end.

                
