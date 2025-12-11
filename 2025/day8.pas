unit day8;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TJunctionBox }

  TJunctionBox = class(TInterfacedObject)
  private
  fId:integer;
  fx:integer;
  fy:integer;
  fz:integer;
  fCircuitId:integer;
  public
  procedure setCircuit(id:integer);
  constructor create(xcoord,ycoord,zcoord,ident:integer);
  end;

  TBoxes = array of TJunctionBox;

  { TBoxesHelper }

  TBoxesHelper = type helper for TBoxes
  function size: int64;
  function push(element: TJunctionBox):int64;
  end;

  { TBoxDistance }

  TBoxDistance = class(TInterfacedObject)
  private
  fBox1:TJunctionBox;
  fBox2:TJunctionBox;
  fDistance:Int64;
  public
  constructor create(box1,box2:TJunctionBox;dist:int64);
  end;

  TBoxDistances = array of TBoxDistance;

  { TBoxDistancesHelper }

  TBoxDistancesHelper = type helper for TBoxDistances
  function size: int64;
  function push(element: TBoxDistance):int64;
  function findIndexOfSmallestAbove(limit:integer):Integer;
  procedure clear;
  function sortItems:TBoxDistances;
  end;

  { TDayEight}
  TDayEight = class(TAocPuzzle)
  private
  function root(item:integer):Integer;
  procedure merge(item1,item2:integer);
  function allOneCircuit:boolean;
  public
  function distanceBetween(box1,box2:TJunctionBox):int64;
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure loadBoxes;
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  boxDistances:TBoxDistances;
  parents:TIntArray;

{ TJunctionBox }

procedure TJunctionBox.setCircuit(id: integer);
begin
  fCircuitId:=id;
end;

constructor TJunctionBox.create(xcoord, ycoord, zcoord, ident: integer);
begin
  fx:=Xcoord;
  fy:=Ycoord;
  fZ:=ZCoord;
  fId:=ident;
  fCircuitId:=-1;
end;

{ TBoxesHelper }

function TBoxesHelper.size: int64;
begin
  result:=length(self);
end;

function TBoxesHelper.push(element: TJunctionBox): int64;
begin
  setLength(self,self.size + 1);
  self[self.size - 1]:=element;
  result:=self.size;
end;

{ TBoxDistance }

constructor TBoxDistance.create(box1, box2: TJunctionBox; dist: int64);
begin
  fBox1:=box1;
  fBox2:=box2;
  fDistance:=dist;
end;

{ TBoxDistancesHelper }

function TBoxDistancesHelper.size: int64;
begin
  result:=length(self);
end;

function TBoxDistancesHelper.push(element: TBoxDistance): int64;
begin
  setLength(self,self.size + 1);
  self[self.size - 1]:=element;
  result:=self.size;
end;

function TBoxDistancesHelper.findIndexOfSmallestAbove(limit: integer): Integer;
var
  index:integer;
  distance,smallest:integer;
begin
  if (self.size = 0) then exit;
  //Set to the first element for now
  smallest:=-1;
  result:=-1;
  for index:=0 to pred(self.size) do
    begin
    distance:=self[index].fDistance;
    if (distance > limit)and((distance < smallest)or(smallest = -1)) then
      begin
      smallest:=self[index].fDistance;
      result:=index;
      end;
    end;
end;

procedure TBoxDistancesHelper.clear;
begin
  setLength(self,0);
end;

//Annoyingly I can't get the anysort method to work with this data type
//Let's load the indexes and distances into a TPoint64 array and sort that way
function TBoxDistancesHelper.sortItems: TBoxDistances;
var
  IndexPoints:TPoint64Array;
  thisPoint:TPoint64;
  index:integer;
begin
result:=TBoxDistances.create;
indexPoints:=TPoint64Array.create;
for index:=0 to pred(self.size) do
  begin
  thisPoint.X:=index;
  thisPoint.Y:=self[index].fDistance;
  indexPoints.push(thisPoint);
  end;
sort(indexPoints,indexPoints.size,true,false);
//Now find each index in self and return them in that order
for index:=0 to pred(indexPoints.size) do
  result.push(self[indexPoints[index].X]);
end;

{ TDayEight }
//This approach is based on Hyperneutrino's solution
//https://youtu.be/Rd7c4Wx7QDg?si=X_JnonB0eORVZbDd

//We create an array of 'Parents' which is the same length
//as the number of boxes. Intially each entry has the same
//value as its index. So box 3 has parent 3 because nothing's
//been joined up yet.
//As boxes are joined in circuits these values get updated.
function TDayEight.root(item: integer): Integer;
begin
  if parents[item] = item then
    begin
    result:=item;
    exit;
    end else
    parents[item] := root(parents[item]);
    result:=parents[item];
end;

procedure TDayEight.merge(item1, item2: integer);
begin
  parents[root(item1)] := root(item2);
end;

//If every entry has the same root then they must all be part of the same circuit.
function TDayEight.allOneCircuit: boolean;
var
  index,currentValue:integer;
begin
  //If every entry has the same root then they must all be part of the same circuit.
  result:=false;
  currentValue:=root(parents[0]);
  for index:=0 to pred(parents.size)do
    if (root(parents[index]) <> currentValue) then exit;
  result:=true;
end;

function TDayEight.distanceBetween(box1, box2: TJunctionBox): int64;
begin
  result:=sqr(box2.fx - box1.fx) + sqr(box2.fy - box1.fy) + sqr(box2.fz - box1.fz) ;
end;


constructor TDayEight.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 8',paintbox_);
boxDistances:=TBoxDistances.create;
parents:=TIntArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayEight.loadBoxes;
var
  index,pairIndex:integer;
  parts:TStringArray;
  unsortedBoxes:TBoxes;
  unsortedBoxDistances:TBoxDistances;
begin
  unsortedBoxes:=TBoxes.create;
  unsortedBoxDistances:=TBoxDistances.create;
  for index:=0 to pred(puzzleInputLines.size) do
    if puzzleInputLines[index].Trim <> '' then
    begin
    parts:=puzzleInputLines[index].Split(',');
    unsortedBoxes.push(TJunctionBox.create(parts[0].ToInteger,parts[1].ToInteger,parts[2].ToInteger,index));
    end;

  for index:=0 to pred(unsortedboxes.size) do
    for pairIndex:=index + 1 to pred(unsortedBoxes.size) do
      unsortedboxDistances.push(TBoxDistance.create(unsortedBoxes[index],unsortedBoxes[pairIndex],distanceBetween(unsortedBoxes[index],unsortedBoxes[pairIndex])));

  boxDistances:=unsortedBoxDistances.sortItems;
end;

procedure TDayEight.runPartOne;
var
  index,rootIndex:integer;
  sizes:TIntArray;
  sum:Int64;
begin
  sizes:=TIntArray.create;
  results.Clear;
  loadBoxes;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    parents.push(index);
    sizes.push(0);
    end;
  for index:=0 to 999 do
    merge(boxDistances[index].fBox1.fId,boxDistances[index].fBox2.fId);
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    rootIndex:=root(index);
    sizes[rootIndex]:= sizes[rootIndex] + 1;
    end;
  //Now sort descending
  sort(sizes,sizes.size,false);
  sum:=sizes[0]*sizes[1]*sizes[2];
  results.Add('Product of largest 3 circuits is '+sum.ToString);
end;

procedure TDayEight.runPartTwo;
var
  index:integer;
  sizes:TIntArray;
  sum:Int64;
begin
  sizes:=TIntArray.create;
  results.Clear;
  loadBoxes;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    parents.push(index);
    sizes.push(0);
    end;
  index:=-1;
  while not allOneCircuit do
    begin
    index:=index+1;
    merge(boxDistances[index].fBox1.fId,boxDistances[index].fBox2.fId);
    end;
  sum:=boxDistances[index].fBox1.fx * boxDistances[index].fBox2.fx;
  results.Add('Product of x coords of last box in circuit is '+sum.ToString);
end;

end.

                
