unit day19;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  TPart = record
  fx:integer;
  fm:integer;
  fa:integer;
  fs:integer;
  fWorkflowsSeen: TStringArray;
  end;

  TParts = array of TPart;

  { TPartsHelper }

  TPartsHelper = type helper for TParts
  function size: integer;
  function push(element:TPart):integer;
  end;

  TWorkflowRule = record
  element:string;
  op:string;
  value:integer;
  acceptIfTrue:boolean;
  rejectIfTrue:boolean;
  nextWorkflow:boolean;
  end;

  TWorkflowRules = array of TWorkflowRule;

  { TWorkflowRulesHelper }

  TWorkflowRulesHelper = type helper for TWorkflowRules
  function size: integer;
  function push(element:TWorkflowRule):integer;
  end;

  TWorkflow = class(TInterfacedObject)
  private
  fName:string;
  fRules: TWorkflowRules;
  public
  function processPart(part_:TPart):string;
  end;

  TWorkflows = array of TWorkflow;

  { TWorkflowHelper }

  TWorkflowHelper = type helper for TWorkflows
  function size: integer;
  function push(element:TWorkflow):integer;
  end;


  { TDayNineteen}
  TDayNineteen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TPartsHelper }

function TPartsHelper.size: integer;
begin
  result:=length(self);
end;

function TPartsHelper.push(element: TPart): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

{ TWorkflow }

function TWorkflow.processPart(part_: TPart): string;
begin
  //This will run the part through all the rules in this workflow and will return either A,R or the id of another workflow
  result:='';
end;

{ TWorkflowHelper }

function TWorkflowHelper.size: integer;
begin
  result:=length(self);
end;

function TWorkflowHelper.push(element: TWorkflow): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

{ TWorkflowRulesHelper }

function TWorkflowRulesHelper.size: integer;
begin
  result:=length(self);
end;

function TWorkflowRulesHelper.push(element: TWorkflowRule): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

{ TDayNineteen }

constructor TDayNineteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayNineteen.runPartOne;
begin
  results.Clear;
end;

procedure TDayNineteen.runPartTwo;
begin
  results.Clear;
end;


end.


