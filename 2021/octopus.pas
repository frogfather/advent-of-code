unit octopus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils;

type
  
  { TOctopus }

  TOctopus = class(TInterfacedObject)
    private
    fPosition:TPoint;
    fEnergy: integer;
    fNotifyFlash:TNotifyEvent;//Fired when the octopus flashes
    fHasFlashed:boolean;
    public
    constructor create(initialEnergy:integer;position:TPoint; eventHandler:TNotifyEvent);
    procedure addEnergy(units:integer);
    procedure resetFlash;
    property energy: integer read fEnergy write fEnergy;
    property position:TPoint read fPosition write fPosition;
    property hasFlashed:boolean read fHasFlashed;
  end;

  { TOctopusMap }
  TOctopusMap = array of array of TOctopus;

  { TOctopusHandler }
  TOctopusHandler = class(TInterfacedObject)
    private
    fPuzzleInput:TStringArray;
    fOctopusMap: TOctopusMap;
    fOctopusFlashCount:integer;
    fMapDimensions:TPoint;
    procedure setupOctopuses(puzzleInput:TStringArray);
    procedure octopusFlashHandler(sender:TObject);
    function inRange(position:TPoint; xPosition,yPosition:integer):boolean;
    function allHaveFlashed: boolean;
    public
    constructor create(puzzleInput:TStringArray);
    procedure runOctopuses;
    procedure resetOctopuses;
    property allFlashed: boolean read allHaveFlashed;
    property flashCount: integer read fOctopusFlashCount;
  end;

implementation

{ TOctopusHandler }

procedure TOctopusHandler.setupOctopuses(puzzleInput: TStringArray);
var
  mapPosition: TPoint;
  row, column, octopusEnergy: Integer;
begin
 fMapDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
 fOctopusMap:=TOctopusMap.create;
 setLength(fOctopusMap, fMapDimensions.X,fMapDimensions.Y);
 for row:=0 to pred(fMapDimensions.Y) do
   for column:=0 to pred(fMapDimensions.X) do
     begin
       with mapPosition do
         begin
         X:=column;
         Y:=row;
         end;
       octopusEnergy:=puzzleInput[row].Substring(column,1).ToInteger;
       fOctopusMap[column][row]:=TOctopus.create(octopusEnergy,mapPosition,@OctopusFlashHandler);
     end;
 fOctopusFlashCount:=0;
end;

procedure TOctopusHandler.octopusFlashHandler(sender: TObject);
var
  octoPosition:TPoint;
  xPosition,yPosition:integer;
begin
if sender is TOctopus then with sender as TOctopus do
  begin
  foctopusFlashCount:= foctopusFlashCount + 1;
  //get its position
  octoPosition:=position;
  for xPosition:= octoPosition.X - 1 to octoPosition.X + 1 do
    for yPosition:= octoPosition.Y -1 to octoPosition.Y + 1 do
      begin
      //check within range
      if inRange(octoPosition,xPosition,yPosition)
        then with fOctopusMap[xPosition][yPosition] as TOctopus do
          addEnergy(1);
      end;
    end;
  end;

function TOctopusHandler.inRange(position: TPoint;xPosition,yPosition:integer): boolean;
var
  notAtOwnPosition:boolean;
  mapWidth,mapHeight:integer;
begin
result:=false;
 mapWidth:=length(foctopusMap);
 if mapWidth = 0 then exit;
 mapHeight:= length(foctopusMap[0]);
notAtOwnPosition:= ((xPosition <> position.X) or (yPosition <> position.Y));
result:=(xPosition < mapWidth)
  and (xPosition > -1)
  and (yPosition < mapHeight)
  and (yPosition > -1)
  and notAtOwnPosition;
end;

function TOctopusHandler.allHaveFlashed: boolean;
var
  x,y,flashCounter :integer;
begin
  flashCounter:=0;
  for x:=0 to pred(fMapDimensions.X) do
    for y:=0 to pred(fMapDimensions.Y) do
      with fOctopusMap[x][y] as TOctopus do
        begin
        if hasFlashed then flashCounter:=flashCounter + 1;
        end;
    result:= flashCounter = fMapDimensions.X * fMapDimensions.Y;
  end;


constructor TOctopusHandler.create(puzzleInput: TStringArray);
begin
  fpuzzleInput:=puzzleInput;
  setupOctopuses(puzzleInput);
end;

procedure TOctopusHandler.runOctopuses;
var
row,column:integer;
begin
  for row:=0 to pred(fMapDimensions.Y) do
    for column:=0 to pred(fMapDimensions.X)do
      with fOctopusMap[column][row]as TOctopus do
        addEnergy(1);
end;
procedure TOctopusHandler.resetOctopuses;
var
row,column:integer;
begin
  for row:=0 to pred(fMapDimensions.Y) do
    for column:=0 to pred(fMapDimensions.X) do
      with fOctopusMap[column][row] as TOctopus do
        resetFlash;
end;

{ TOctopus }

constructor TOctopus.create(initialEnergy: integer; position: TPoint; eventHandler: TNotifyEvent);
begin
  fEnergy:=initialEnergy;
  fPosition:=position;
  fNotifyFlash:=eventHandler;
end;

procedure TOctopus.addEnergy(units: integer);
begin
  if not hasFlashed then energy:=energy+units;
  if energy > 9 then
    begin
    fhasFlashed:=true;
    energy:=energy - 10;
    fNotifyFlash(self);
    end;
end;

procedure TOctopus.resetFlash;
begin
  fhasFlashed:=false;
end;

end.

