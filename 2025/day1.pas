unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayOne}
  TDayOne = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

  { TSafe }
  TSafe = class(TInterfacedObject)
  private
  fDialValue:integer;
  fZeros:integer;
  public
  constructor create;
  procedure reset;
  procedure rotate(value:string);
  property zeros: integer read fZeros;
  property dialPosition: integer read fDialValue;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayOne.runPartOne;
var
  index:integer;
  safe:TSafe;
begin
  safe:=TSafe.create;
  results.Clear;
  for index:= 0 to pred(puzzleInputLines.size) do
    if (puzzleInputLines[index] <> '') then
      safe.rotate(puzzleInputLines[index]);
  results.add('Total is '+safe.zeros.toString);

end;

procedure TDayOne.runPartTwo;
begin
  results.Clear;
end;

{ TSafe }

constructor TSafe.create;
begin
  fZeros:=0;
  fDialValue:=50;
end;

procedure TSafe.reset;
begin
  fZeros:=0;
  fDialValue:=50;
end;

procedure TSafe.rotate(value: string);
var
  direction:char;
  clicks,hundreds:integer;
begin
  direction:=value[1];
  clicks:=value.Substring(1).ToInteger;
  //If it's over 100 remove the number of 100s
  hundreds:=clicks div 100;
  clicks:=clicks - (100 * hundreds);
  if (direction = 'L') then clicks:=clicks * -1;
  fDialValue:=fDialValue + clicks;
  if fDialValue < 0 then
    fDialValue:=100 + fDialValue
  else if fDialValue > 99 then
    fDialValue:=fDialValue - 100;
  if fDialValue = 0 then
    fZeros:=fZeros+1
end;


end.

                
