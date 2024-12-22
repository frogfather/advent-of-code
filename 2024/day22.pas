unit day22;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils,strUtils;
type

  { TDayTwentyTwo}
  TDayTwentyTwo = class(TAocPuzzle)
  private
  function mix(arg1,arg2:int64):int64;
  function prune(arg:int64):int64;
  function getNextSecret(secret:int64):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwentyTwo }

function TDayTwentyTwo.mix(arg1, arg2: int64): int64;
begin
  result:=stringOfBinaryToInt64(bitwiseXOR(intToBin(arg1,64),intToBin(arg2,64)));
end;

function TDayTwentyTwo.prune(arg: int64): int64;
begin
  result:=arg mod 16777216;
end;

function TDayTwentyTwo.getNextSecret(secret: int64): int64;
var
  step1,step2:integer;
begin
  step1:=prune(mix(secret,secret * 64));
  step2:=prune(mix(step1 div 32, step1));
  result:=prune(mix(step2 * 2048,step2));
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

                
