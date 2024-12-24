unit day22;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils,strUtils;
type

  { TDayTwentyTwo}
  TDayTwentyTwo = class(TAocPuzzle)
  private
  function getNextSecret(secret:int64):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwentyTwo }

function TDayTwentyTwo.getNextSecret(secret: int64): int64;
var
  step1,step2:int64;
begin
  step1:=((secret shl 6) xor secret) mod 16777216;
  step2:=((step1 shr 5) xor step1)  mod 16777216;
  result:=((step2 shl 11) xor step2)  mod 16777216;
end;

constructor TDayTwentyTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 22',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyTwo.runPartOne;
var
  output:integer;
  index:integer;
  lineNo:integer;
  total:int64;
begin
  results.Clear;
  total:=0;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
    writeln('Process line '+lineNo.toString);
    output:=puzzleInputLines[lineNo].ToInteger;
    for index:=0 to 1999 do
      output:=getNextSecret(output);
    total:=total+output;
    end;
  results.Add('Total is '+total.ToString);
end;

procedure TDayTwentyTwo.runPartTwo;
begin
  results.Clear;
end;


end.

                
