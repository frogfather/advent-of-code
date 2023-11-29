unit day13;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayThirteen}
  TDayThirteen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThirteen }

constructor TDayThirteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 13',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThirteen.runPartOne;
begin
  results.Clear;
end;

procedure TDayThirteen.runPartTwo;
begin
  results.Clear;
end;

end.


