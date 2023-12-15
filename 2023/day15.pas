unit day15;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFifteen}
  TDayFifteen = class(TAocPuzzle)
  private
  function getSectionParts(section_:string):TStringArray;
  function getLenses(boxContents:string):TStringArray;
  function codePosition(boxContents_,code_:string):integer;
  function addLens(boxContents_,lens_:string):string;
  function removeLens(boxContents_,lens_:string):string;
  function replaceLens(boxContents_,lens_:string):string;
  function hash(input:string):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFifteen }

constructor TDayFifteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 15',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFifteen.runPartOne;
var
  total:integer;
  section:integer;
  sections:TStringArray;

begin
  results.Clear;
  sections:=puzzleInputLines[0].Split([',']);
  total:=0;
  for section:=0 to pred(sections.size) do
  total:=total+hash(sections[section]);
  results.add('Total is '+total.ToString);
end;

procedure TDayFifteen.runPartTwo;
var
  boxes,sections,parts,lenses:TStringArray;
  boxId,sectionId:integer;
  lens:string;
  boxNo,lensNo,lensPower:integer;
  total:integer;
begin
  results.Clear;
  boxes:=TStringArray.create;
  setLength(boxes,256);
  sections:=puzzleInputLines[0].Split([',']);
  for sectionId:=0 to pred(sections.size) do
    begin
    parts:=getSectionParts(sections[sectionId]);
    lens:=parts.toString('|');
    boxId:=hash(parts[0]);
    if (parts[1] = '-') then boxes[boxId]:=removeLens(boxes[boxId],lens)
    else if (parts[1] = '=') then boxes[boxId]:=addLens(boxes[boxId],lens);
    end;
  total:=0;
  for boxNo:=0 to pred(boxes.size) do
    begin
    lenses:=getLenses(boxes[boxNo]);
    for lensNo:=0 to pred(lenses.size) do
      begin
      //focal length is lens[2]
      if (lenses[lensNo].Length > 0) then
        begin
        lensPower:=lenses[lensNo].Split(['|'])[2].ToInteger;
        total:=total+(boxNo+1)*(lensNo+1)*lensPower;
        end;
      end;
    end;
  results.add('Total is '+total.ToString);
end;

function TDayFifteen.getSectionParts(section_: string): TStringArray;
var
  opIndex:integer;
  operator_:string;
begin
  result:=TStringArray.create;
  opIndex:=section_.IndexOf('-');
  if (opIndex=-1) then opIndex:=section_.IndexOf('=');
  operator_:=section_.Substring(opIndex,1);
  result.push(section_.Substring(0,opIndex));
  result.push(operator_);
  if (operator_ = '=') then result.push(section_.Substring(opIndex+1));
end;

function TDayFifteen.getLenses(boxContents: string): TStringArray;
begin
  //a string consisting of comma separated entries
  result:=boxContents.Split([','],(TstringSplitOptions.ExcludeEmpty));
end;

function TDayFifteen.codePosition(boxContents_,code_: string):integer;
var
  entries,entryParts:TStringArray;
  entryId:integer;
begin
  result:=-1;
  entries:=boxContents_.Split(',');
  //each entry is code, op, value separated by |
  for entryId:= 0 to pred(entries.size) do
    begin
    //Split the entry on pipe
    entryParts:=entries[entryId].Split(['|']);
    if (entryParts[0] = code_) then
      begin
      result:=entryId;
      exit;
      end;
    end;
end;

function TDayFifteen.addLens(boxContents_,lens_: string): string;
begin
  result:='';
  if codePosition(boxContents_,lens_.Split(['|'])[0]) = -1 then
    begin
    if boxContents_.Length = 0 then result:=lens_
      else result:= boxContents_+','+lens_;
    end else result:= replaceLens(boxContents_,lens_);
end;

function TDayFifteen.removeLens(boxContents_,lens_: string): string;
var
  lensIndex:integer;
  lenses:TStringArray;
begin
  lensIndex:=codePosition(boxContents_,lens_.Split(['|'])[0]);
  if (lensIndex > -1) then
    begin
    lenses:=boxContents_.Split([',']);
    lenses[lensIndex]:='';
    result:=lenses.toString(',');
    end else result:=boxContents_;
end;

function TDayFifteen.replaceLens(boxContents_,lens_:string): string;
var
  lensIndex:integer;
  lenses:TStringArray;
begin
  result:='';
  lenses:=boxContents_.Split([',']);
  lensIndex:=codePosition(boxContents_,lens_.Split(['|'])[0]);
  lenses[lensIndex]:=lens_;
  result:=lenses.toString(',');
end;

function TDayFifteen.hash(input: string): integer;
var
chrindex:integer;
sectionSum:integer;
  begin
  sectionSum:=0;
  for chrIndex:= 1 to input.Length do
    sectionSum:=((sectionSum+ord(input[chrIndex]))*17) mod 256;
  result:=sectionSum;
end;

end.


