unit day6;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;
type
  
  { TDaySix }

  TDaySix = class(TAocPuzzle)
  private
  fName:string;
  function noRepeats(buffersize,index:integer):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDaySix }

constructor TDaySix.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 6';
end;

function TDaySix.noRepeats(buffersize, index: integer): boolean;
var
  x:integer;
  currentElement:String;
  occurrences:TStringArray;
begin
  result:=false;
  occurrences:=TStringArray.create;
  if (index < buffersize - 1) then exit;
  for x:= (index+1 - buffersize) to index do
    begin
      currentElement:=puzzleInput.Substring(x,1);
      //already found in string so duplicate
      if (occurrences.indexOf(currentElement) > -1) then exit
      else occurrences.push(currentElement);
    end;
  result:=true;
end;

procedure TDaySix.runPartOne;
var
  index:integer;
begin
  for index:=3 to Pred(length(puzzleInput)) do
    begin
      if noRepeats(4,index) then
        begin
        results.Add('Start of packet marker at '+(index+1).ToString);
        exit;
        end;
    end;
end;

procedure TDaySix.runPartTwo;
begin

end;

end.

