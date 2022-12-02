unit day3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type
{ TDayThree }
  TDayThree = class(TAocPuzzle)
  private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }

constructor TDayThree.create(filename: string; paintbox_: TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 3';
end;

procedure TDayThree.runPartOne;
begin
  DebugLn('Run '+fName+' part one');
end;

procedure TDayThree.runPartTwo;
begin
  DebugLn('Run '+fName+' part two');
end;

end.

