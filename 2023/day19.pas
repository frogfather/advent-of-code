unit day19;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNineteen}
  TDayNineteen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayNineteen }

constructor TDayNineteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayNineteen.runPartOne;
begin
  results.Clear;
end;

procedure TDayNineteen.runPartTwo;
begin
  results.Clear;
end;


end.


