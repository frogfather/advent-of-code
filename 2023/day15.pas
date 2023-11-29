unit day15;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFifteen}
  TDayFifteen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFifteen }

constructor TDayFifteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 15',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFifteen.runPartOne;
begin
  results.Clear;
end;

procedure TDayFifteen.runPartTwo;
begin
  results.Clear;
end;

end.


