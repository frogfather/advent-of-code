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
var
  total:integer;
  section:integer;
  sections:TStringArray;

  function hash(input:string):integer;
  var
    chrindex:integer;
    sectionSum:integer;
  begin
  sectionSum:=0;
  for chrIndex:= 1 to input.Length do
    sectionSum:=((sectionSum+ord(input[chrIndex]))*17) mod 256;
  result:=sectionSum;
  end;

begin
  results.Clear;
  sections:=puzzleInputLines[0].Split([',']);
  total:=0;
  for section:=0 to pred(sections.size) do
  total:=total+hash(sections[section]);
  results.add('Total is '+total.ToString);
end;

procedure TDayFourteen.runPartTwo;
begin
  results.Clear;
end;

end.


