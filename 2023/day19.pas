unit day19;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TPart }

  TPart = Class(TInterfacedObject)
  private
  fx:integer;
  fm:integer;
  fa:integer;
  fs:integer;
  fWorkflowsSeen: TStringArray;
  function getSumOfAttributes:integer;
  public
  constructor create(input_:string);
  procedure addToSeenWorkflows(wfName:string);
  function alreadyProcessedBy(wfName:string):boolean;
  property sumOfAttributes: integer read getSumOfAttributes;
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
  nextWorkflow:string;
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
  fNextAction:string;
  public
  constructor create(input_:string);
  function processPart(part_:TPart):string;
  end;

  TWorkflows = array of TWorkflow;

  { TWorkflowsHelper }

  TWorkflowsHelper = type helper for TWorkflows
  function size: integer;
  function push(element:TWorkflow):integer;
  function findByName(name_:string):TWorkflow;
  end;

  { TWorkflowRunner }

  TWorkflowRunner = class(TInterfacedObject)
  private
  fUnprocessed:TParts;
  fAccepted:TParts;
  fRejected:TParts;
  fWorkflows:TWorkflows;
  public
  property workflows: TWorkflows read fWorkflows;
  constructor create;
  procedure setUp(input_:TStringArray);
  procedure processParts;
  function sumAcceptedParts:integer;
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
var
  workflowRunner:TWorkflowRunner;

{ TWorkflowRunner }

constructor TWorkflowRunner.create;
begin
  fUnprocessed:=TParts.create;
  fAccepted:=TParts.create;
  fRejected:=TParts.create;
  fWorkflows:=TWorkflows.create;
end;

procedure TWorkflowRunner.setUp(input_:TStringArray);
var
  index:integer;
  parts:boolean;
begin
  parts:=false;
  for index:=0 to pred(input_.size) do
    begin
    if input_[index].Trim = '' then
       begin
       parts:=true;
       continue;
       end;
    writeln('Index: '+index.ToString);
    if not parts then fWorkflows.push(TWorkflow.Create(input_[index]))
    else fUnprocessed.push(TPart.create(input_[index]));
    end;
end;

procedure TWorkflowRunner.processParts;
var
  partNo:integer;
  currentPart:TPart;
  currentWorkflow:TWorkflow;
  partProcessed:boolean;
  currentWfResult:string;
begin
//This will loop through the parts and call processPart for each
//workflow recording either the next workflow if there is one
//or adding the part to the accepted or rejected lists
for partNo:=0 to pred(fUnprocessed.size) do
  begin
  partProcessed:=false;
  currentPart:=fUnprocessed[partNo];
  currentWorkflow:=fWorkflows.findByName('in');
  while not partProcessed do
    begin
    currentWfResult:=currentWorkflow.processPart(currentPart);
    if (currentWfResult = 'A') then
       begin
       fAccepted.push(currentPart);
       //delete from unprocessed
       partProcessed:=true;
       end else
    if (currentWfResult = 'R') then
       begin
       fRejected.push(currentPart);
       //delete from unprocessed
       partProcessed:=true;
       end else
    currentWorkflow:=fWorkflows.findByName(currentWfresult);
    end;
  end;
end;

function TWorkflowRunner.sumAcceptedParts: integer;
var
  index:integer;
begin
  result:=0;
  for index:=0 to pred(fAccepted.size) do
    result:=result+fAccepted[index].sumOfAttributes;
end;

{ TPart }

function TPart.getSumOfAttributes: integer;
begin
  result:=fx+fm+fa+fs;
end;

constructor TPart.create(input_:string);
var
  parts:TStringArray;
begin
  //process the string
  //{x=1691,m=2832,a=1861,s=2722}
  //split on {}and ,
  fx:=0;
  fm:=0;
  fa:=0;
  fs:=0;
  parts:=input_.Split(['}','{',','],TStringSplitOptions.ExcludeEmpty);
  fx:=parts[0].Substring(2).ToInteger;
  fm:=parts[1].Substring(2).ToInteger;
  fa:=parts[2].Substring(2).ToInteger;
  fs:=parts[3].Substring(2).ToInteger;
  fWorkflowsSeen:=TStringArray.create;
end;

procedure TPart.addToSeenWorkflows(wfName: string);
begin
  fWorkflowsSeen.push(wfName);
end;

function TPart.alreadyProcessedBy(wfName: string): boolean;
begin
  result:= fWorkflowsSeen.indexOf(wfName) > -1;
end;

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

constructor TWorkflow.create(input_: string);
var
  parts,ruleParts:TStringArray;
  index:integer;
  newRule:TWorkflowRule;
begin
  parts:=input_.Split(['{','}',','],TStringSplitOptions.ExcludeEmpty);
  fName:=parts[0];
  fNextAction:=parts[pred(parts.size)];
  for index:=1 to pred(pred(parts.size))do
    begin
    ruleParts:=parts[index].Split([':']);
    newRule.element:=ruleParts[0].Substring(0,1);
    newRule.op:=ruleParts[0].Substring(1,1);
    newRule.value:=ruleParts[0].Substring(2).ToInteger;
    newRule.nextWorkflow:=ruleParts[1];
    fRules.push(newRule);
    end;
end;

function TWorkflow.processPart(part_: TPart): string;
var
  currentRule:TWorkflowRule;
  ruleId,elementValue:integer;
  ruleValid:boolean;
begin
  //This will run the part through all the rules in this workflow and will return either A,R or the id of another workflow
  part_.addToSeenWorkflows(fName);
  for ruleId:=0 to pred(fRules.size) do
    begin
    currentRule:=fRules[ruleId];
    //get the value of the elememt we're measuring;
    case currentRule.element of
      'x': elementValue:=part_.fx;
      'm': elementValue:=part_.fm;
      'a': elementValue:=part_.fa;
      's': elementValue:=part_.fs;
    end;
    case currentRule.op of
      '>': ruleValid:= elementValue > currentRule.value;
      '<': ruleValid:= elementValue < currentRule.value;
      '=': ruleValid:= elementValue = currentRule.value;
    end;
    if ruleValid then
       begin
       result:=currentRule.nextWorkflow;
       exit;
       end;
    end;
  result:=fNextAction;
end;

{ TWorkflowsHelper }

function TWorkflowsHelper.size: integer;
begin
  result:=length(self);
end;

function TWorkflowsHelper.push(element: TWorkflow): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TWorkflowsHelper.findByName(name_: string): TWorkflow;
var
  index:integer;
begin
  result:=nil;
  for index:=0 to pred(self.size) do
    if self[index].fName = name_ then
       begin
       result:=self[index];
       exit;
       end;
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
  workflowRunner:=TWorkflowRunner.create;
  workflowRunner.setUp(puzzleInputLines);
  workflowRunner.processParts;
  results.add('Sum: '+workflowRunner.sumAcceptedParts.ToString);
end;

procedure TDayNineteen.runPartTwo;
begin
  results.Clear;
end;


end.


