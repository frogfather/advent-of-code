unit day23;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type
  
  { TComputer }

  TComputer = class(TInterfacedObject)
  private
  fConnections: TStringArray;
  fName:string;
  public
  procedure addConnection(conn:string);
  property connections: TStringArray read fConnections;
  property name:string read fName;
  constructor create(_name:string);
  end;

  TComputers = array of TComputer;

  { TComputersHelper }

  TComputersHelper = type helper for TComputers
  function size:integer;
  procedure clear;
  function push(comp:TComputer):integer;
  function indexOf(_name:string):integer;
  end;

  { TDayTwentyThree}
  TDayTwentyThree = class(TAocPuzzle)
  private
  procedure setupConnections;
  function findSetsOfThree:TStringArray;
  function atLeastOneStartsWithT(el1,el2,el3:string):boolean;
  function findLargestNetwork(from_:TComputer):TStringArray;
  function getMatches(list1,list2:TStringArray):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  computers: TComputers;
{ TComputer }

procedure TComputer.addConnection(conn: string);
begin
  fConnections.push(conn);
end;

constructor TComputer.create(_name:string);
begin
  fName:=_name;
  fConnections:=TStringArray.create;
end;

{ TComputersHelper }

function TComputersHelper.size: integer;
begin
  result:=length(self);
end;

procedure TComputersHelper.clear;
begin
  setLength(self,0);
end;

function TComputersHelper.push(comp: TComputer): integer;
begin
  insert(comp,self,length(self));
  result:=self.size;
end;

function TComputersHelper.indexOf(_name: string): integer;
begin
  for result:=0 to pred(self.size) do
    begin
    if self[result].name = _name then exit;
    end;
  result:=-1;
end;

{ TDayTwentyThree }

procedure TDayTwentyThree.setupConnections;
var
  index:integer;
  conns:TStringArray;
  compAIndex,compBIndex:integer;
  compA,compB:TComputer;
begin
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    //setup each computer and add its connection
    conns:=puzzleInputLines[index].Split('-');
    compAIndex:=computers.indexOf(conns[0]);
    compBIndex:=computers.indexOf(conns[1]);
    if (compAIndex = -1) then
      begin
      compA:=TComputer.create(conns[0]);
      computers.push(compA);
      end else compA:=computers[compAIndex];
    compA.addConnection(conns[1]);
    if (compBIndex = -1) then
      begin
      compB:=TComputer.create(conns[1]);
      computers.push(compB);
      end else compB:=computers[compBIndex];
    compB.addConnection(conns[0]);
    end;
end;

function TDayTwentyThree.findSetsOfThree: TStringArray;
var
  chain:TStringArray;
  chainString:String;
  compA,compB,compC:TComputer;
  index, indexa,indexb:integer;
begin
  result:=TStringArray.create;
  chain:=TStringArray.create;
  for index:=0 to pred(computers.size) do
    begin
    compA:=computers[index];
    for indexa:=0 to pred(compA.connections.size)do
      begin
      //find the connection and look at its connections
      compB:=computers[computers.indexOf(compA.connections[indexA])];
      for indexb:= 0 to pred(compB.connections.size)do
        begin
        compC:=computers[computers.indexOf(compB.connections[indexB])];
        if (compC.connections.indexOf(compA.name) > -1)
        and (atLeastOneStartsWithT(compA.name,compB.name,compC.name)) then
          begin
          chain.clear;
          chain.push(compA.name);
          chain.push(compB.name);
          chain.push(compC.name);
          sort(chain,chain.size);
          chainString:=chain.toString(',');
          if (result.indexOf(chainString) = -1) then result.push(chainString);
          end;
        end;
      end;
    end;
end;

function TDayTwentyThree.atLeastOneStartsWithT(el1, el2, el3:string): boolean;
begin
  result:=(el1.StartsWith('t')) or (el2.StartsWith('t')) or (el3.StartsWith('t'))
end;

function TDayTwentyThree.findLargestNetwork(from_: TComputer): TStringArray;
var
  initialSet:TStringArray;
  index:integer;
  scores:TStringIntMap;
  sResult:String;
begin
  result:=TStringArray.create;
  scores:=TStringIntMap.create;
  initialSet:=from_.connections;
  initialSet.push(from_.name);
  results.Add('For '+from_.name+' set '+initialSet.toString(','));
  //now for each item in this list, how many have items in common
  for index:=0 to pred(pred(initialSet.size)) do
    scores.AddOrSetData(initialSet[index],getMatches(initialSet,computers[computers.indexOf(initialSet[index])].connections));
  //how many entries do we have with the same number?
  sResult:=from_.name+' ';
  for index:=0 to pred(scores.Count) do
    sResult:=sResult+scores.Keys[index]+':'+scores.Data[index].ToString+',';
  results.add(sResult);
end;

function TDayTwentyThree.getMatches(list1, list2: TStringArray): integer;
var
  index:integer;
  matches:integer;
begin
  results.Add('comparing '+list1.toString(',')+' with '+list2.toString(','));
  matches:=0;
  for index:= 0 to pred(list1.size) do
    if (list2.indexOf(list1[index]) > -1) then matches:=matches+1;
  result:=matches - 1; //there will always be one link
end;

constructor TDayTwentyThree.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 23',paintbox_);
computers:=TComputers.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyThree.runPartOne;
var
  setsOfThree:TStringArray;
  index:integer;
begin
  results.Clear;
  computers.clear;
  setupConnections;
  setsOfThree:=findSetsOfThree;
  results.add('There are '+setsOfThree.size.toString+' results');
end;

procedure TDayTwentyThree.runPartTwo;
var
  index:integer;
  currConnections,maxConnections:TStringArray;
begin
  results.Clear;
  computers.clear;
  maxConnections:=TStringArray.create;
  setupConnections;
  for index:=0 to pred(computers.size) do
    begin
    currConnections:=findLargestNetwork(computers[index]);
    end;
  //all computers have 13 connections
  //start with first, devise set of all connections including itself
  //see how many of the set point to the others
end;


end.

                
