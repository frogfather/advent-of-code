unit day14;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFourteen}
  TDayFourteen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFourteen }

constructor TDayFourteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 14',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFourteen.runPartOne;
begin
  results.Clear;
end;

procedure TDayFourteen.runPartTwo;
begin
  results.Clear;
end;


end.

                
