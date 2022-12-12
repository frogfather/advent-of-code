unit day12;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind;

  { TDayTwelve }
type
  TDayTwelve = class(TAocPuzzle)
  private
    fStartPoint,fEndPoint:TPoint;
    fShortestPath:integer;
    fRouteFinder:TRouteFinder;
    function convertMap:T3DIntMap;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }

function TDayTwelve.convertMap:T3DIntMap;
var
  lineNo,charNo:integer;
  currentLine:string;
  currentChar:char;
  charValue:integer;
begin
  setLength(result,length(puzzleInputLines[0]),length(puzzleInputLines),2);
  //convert the letters a-z into numbers
  //chr - 96 for small letters
  //special case for S and E call them 0 and 27
  for lineNo := 0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[lineNo];
    for charNo:= 0 to pred(currentLine.Length) do
      begin
      currentChar:= currentLine[charNo+1]; //strings 1 indexed!!
      if currentChar = 'S' then
        begin
        charValue:= 0;
        fStartPoint:= TPoint.Create(charNo,LineNo);
        end
      else if currentChar = 'E' then
        begin
        charValue:= 27;
        fEndPoint:= TPoint.Create(charNo, LineNo);
        end else
      charValue:=ord(currentChar) - 96;
      result[charNo][lineNo][0]:= charValue;
      result[charNo][lineNo][1]:= 0;
      end;
    end;
end;

constructor TDayTwelve.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 12', paintbox_);
end;

procedure TDayTwelve.runPartOne;
begin
  fRouteFinder:= TRouteFinder.create(convertMap);
end;

procedure TDayTwelve.runPartTwo;
begin

end;

end.

