unit day4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type
{ TDay Four }
  
  { TDayFour }

  TDayFour = class(TAocPuzzle)
  private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }

constructor TDayFour.create(filename: string; paintbox_: TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 4';
end;

procedure TDayFour.runPartOne;
begin

end;

procedure TDayFour.runPartTwo;
begin

end;

end.

