unit day20;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,lgQueue;
type

  TModuleType = (button,broadcaster,flipflop,conjunction,output);

  { TPulse }

  TPulse = class(TInterfacedObject)
  private
  fSource:string;
  fHigh:boolean;
  public
  property source:string read fSource;
  property high:boolean read fHigh;
  constructor create(high_:boolean; source_:string);
  end;

  { TModuleInput }

  TModuleInput = class(TInterfacedObject)
  private
  fname: string;
  fValue:boolean;
  public
  constructor create(name_:string);
  property name: string read fName;
  property value: boolean read fValue write fValue;
  end;

  TModuleInputs = array of TModuleInput;

  { TModuleInputsHelper }

  TModuleInputsHelper = type helper for TModuleInputs
  function size: integer;
  function push(element:TModuleInput):integer;
  function findByName(name_:string):TModuleInput;
  function allHigh:boolean;
  end;


  //flip flops just need a list of inputs
  //conjunction modules need to keep track of their previous value

  { TModule }

  TModule = class(TInterfacedObject)
  private
  fType: TModuleType;
  fName:string;
  fInputs:TModuleInputs;
  fOutput:boolean;
  fSendPulse:TNotifyEvent;
  fReceivePulse:TNotifyEvent;
  public
  constructor create(type_:TModuleType;name_:string;sendPulse_,receivePulse_:TNotifyEvent);
  property moduleType:TModuleType read fType;
  property output:boolean read fOutput;
  procedure addInput(input_:TModuleInput);
  procedure pulseFrom(pulse_:TPulse);
  end;

  { TButtonModule }

  TButtonModule = class(TModule)
  public
  procedure press;
  constructor create(sendPulse_,receivePulse_:TNotifyEvent);
  end;

  TModules = array of TModule;

  { TModulesHelper }

  TModulesHelper = type helper for TModules
  function size: integer;
  function push(element:TModule):integer;
  function findByName(name_:String):TModule;
  function getState:TBits;
  end;

  { TQueueManager }

  TQueueManager = class(TInterfacedObject)
  private
  fQueue: specialize TGObjectQueue<TPulse>;
  fModules:TModules;
  fLowCount:integer;
  fHighCount:integer;
  fInitialState:TBits;
  fCycles:integer;
  procedure moduleOutputChanged(sender:TObject);
  procedure moduleReceivedPulse(sender:TObject);
  procedure runSingle;
  public
  constructor create;
  procedure setUpModules(inputLines:TStringArray);
  procedure run(buttonPushes:integer);
  procedure push(entry_:TPulse);
  function pop: TPulse;
  property lowCount:integer read fLowCount;
  property highCount:integer read fHighCount;
  end;

  { TDayTwenty}
  TDayTwenty = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  queue_:TQueueManager;

{ TButtonModule }

procedure TButtonModule.press;
begin
if assigned(fSendPulse) then fSendPulse(TPulse.create(false,fname));
end;

constructor TButtonModule.create(sendPulse_, receivePulse_: TNotifyEvent);
begin
inherited create(TModuleType.button,'button',sendPulse_,receivePulse_);
end;

{ TModulesHelper }

function TModulesHelper.size: integer;
begin
  result:=length(self);
end;

function TModulesHelper.push(element: TModule): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TModulesHelper.findByName(name_: String): TModule;
var
  index:integer;
begin
  result:=nil;
  for index:=0 to pred(length(self)) do
    if (self[index].fName = name_) then
    begin
      result:=self[index];
      exit;
    end;
end;

function TModulesHelper.getState: TBits;
var
  index:integer;
begin
  result:=TBits.Create(self.size);
  for index:=0 to pred(self.size) do
    result[index]:=self[index].output;
end;

{ TModuleInput }

constructor TModuleInput.create(name_: string);
begin
  fName:=name_;
  fValue:=false;//remembered state only used in conjunction modules
end;

{ TModuleInputsHelper }

function TModuleInputsHelper.size: integer;
begin
  result:=length(self);
end;

function TModuleInputsHelper.push(element: TModuleInput): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TModuleInputsHelper.findByName(name_: string): TModuleInput;
var
  index:integer;
begin
  result:=nil;
  for index:=0 to pred(length(self)) do
    if (self[index].name = name_) then
    begin
      result:=self[index];
      exit;
    end;

end;

function TModuleInputsHelper.allHigh: boolean;
var
  index:integer;
begin
  result:=true;
  for index:=0 to pred(self.size) do
    if not self[index].value then
    begin
      result:=false;
      exit;
    end;
end;

{ TModule }

constructor TModule.create(type_:TModuleType;name_: string;sendPulse_,receivePulse_:TNotifyEvent);
begin
  fType:=type_;
  fName:=name_;
  fSendPulse:=sendPulse_;
  fReceivePulse:=receivePulse_;
  fInputs:=TModuleInputs.create;
  fOutput:=type_ = TModuleType.conjunction; //conjunction modules are high initially
end;

procedure TModule.addInput(input_: TModuleInput);
begin
  fInputs.push(input_);
end;

//This is the ONLY place we determine if the module should send a pulse
//Also the module should signal that it has received a valid pulse so the
//queue manager can increment its count
procedure TModule.pulseFrom(pulse_:TPulse);
var
  input:TModuleInput;
begin
  input:=fInputs.findByName(pulse_.fSource);
  //quit if no corresponding input
  if assigned(input) and (pulse_.fSource <> fName) then input.value:=pulse_.high else exit;
  //signal that a pulse was received to allow the queueManager to count them
  if assigned(fReceivePulse) then fReceivePulse(pulse_);
  if (moduleType = TModuleType.flipflop)and not pulse_.high then
    begin
    fOutput:=not fOutput;
    if assigned(fSendPulse) then fSendPulse(TPulse.create(fOutput,fName));
    end else
  if (moduleType = TModuleType.conjunction) then
    begin
    fOutput:= not fInputs.allHigh;
    if assigned(fSendPulse) then fSendPulse(TPulse.create(fOutput,fName));
    end else
  if (moduleType = TModuleType.broadcaster) then
    begin
    //just pass it on
    fOutput:=pulse_.fHigh;
    if assigned(fSendPulse) then fSendPulse(TPulse.create(fOutput,fName));
    end;
end;

{ TQueueManager }
//If a module sends a pulse this method adds it to the queue
procedure TQueueManager.moduleOutputChanged(sender: TObject);
begin
  if sender is TPulse then push(sender as TPulse);
end;

//Called when a module signals that it has received a pulse
procedure TQueueManager.moduleReceivedPulse(sender: TObject);
begin
  if sender is TPulse then with Sender as TPulse do
    begin
    if fHigh then fHighCount:=fHighCount + 1 else
      fLowCount:=fLowCount + 1;
    writeln('Pulse received from '+fSource + ' with value '+fHigh.ToString+' High count '+fHighCount.ToString+' low count '+fLowCount.ToString);
    end;
end;

procedure TQueueManager.runSingle;
var
  buttonModule:TButtonModule;
  currentPulse:TPulse;
  moduleId:integer;
begin
  //Push the button once
  buttonModule:=fModules.findByName('button') as TButtonModule;
  if not assigned(buttonModule) then exit;
  buttonModule.press;
  while not fQueue.IsEmpty do
    begin
    //get the item out of the queue
    currentPulse:=pop;
    //and send it to each module
    for moduleId:=0 to pred(fModules.size) do
      fModules[moduleId].pulseFrom(currentPulse);
    end;
end;

constructor TQueueManager.create;
begin
  fQueue:= specialize TGObjectQueue<TPulse>.Create;
  fModules:= TModules.create;
  fLowCount:=0;
  fHighCount:=0;
  fCycles:=0;
end;

procedure TQueueManager.setUpModules(inputLines: TStringArray);
var
  lineNo,destNo:integer;
  moduleId:string;
  moduleDestinations:TStringArray;
  moduleType:string;
  moduleName:string;
  currentModule:TModule;
begin
  //clear fModules and fQueue
  setLength(fModules,0);
  fQueue.Clear;
  fModules.push(TButtonModule.create(@moduleOutputChanged,@moduleReceivedPulse));
  fModules.push(TModule.create(TModuleType.broadcaster,'broadcaster',@moduleOutputChanged,@moduleReceivedPulse));
  fModules.push(TModule.create(TModuleType.output,'output',@moduleOutputChanged,@moduleReceivedPulse));
  for lineNo:=0 to pred(inputLines.size) do
    begin
    moduleId:=inputLines[lineNo].Split('->')[0].Trim;
    if (moduleId <> 'broadcaster') then
      begin
      moduleType:=moduleId.Substring(0,1);
      moduleName:=moduleId.Substring(1).Trim;
      if (moduleType = '%') then fModules.push(TModule.create(TModuleType.flipflop, moduleName,@moduleOutputChanged,@moduleReceivedPulse))
      else fModules.push(TModule.create(TModuleType.conjunction, moduleName,@moduleOutputChanged,@moduleReceivedPulse));
      end;
    end;
  fModules.findByName('broadcaster').addInput(TModuleInput.create('button'));
  for lineNo:=0 to pred(inputLines.size) do
    begin
    moduleName:=inputLines[lineNo].Split('->',TStringSplitOptions.ExcludeEmpty)[0].Trim;
    if (moduleName <> 'broadcaster') then moduleName:= moduleName.Substring(1);
    moduleDestinations:=inputLines[lineNo].Split('->',TStringSplitOptions.ExcludeEmpty)[1].Trim.Split(',',TStringSplitOptions.ExcludeEmpty);
    for destNo:=0 to pred(moduleDestinations.size) do
      begin
      //Look for the module. If it doesn't exist then add it
      currentModule:= fModules.findByName(moduleDestinations[destNo].Trim);
      if currentModule = nil then
        begin
        currentModule:=TModule.create(TModuleType.output, moduleDestinations[destNo].Trim,@moduleOutputChanged,@moduleReceivedPulse);
        fModules.push(currentModule);
        end;
      currentModule.addInput(TModuleInput.create(moduleName));
      end;
    end;
  //Conjunction modules are initially high because they initially have low inputs
  //Now note the initial state

  fInitialState:=fModules.getState;
end;

procedure TQueueManager.run(buttonPushes: integer);
var
  cycleLength,cycleCount:integer;
  remainingPushes:integer;
  done:boolean;

  function bitsToString(bits_:TBits):string;
  var
    i:integer;
  begin
    result:='';
  for i:=0 to pred(bits_.Size) do
    begin
    if bits_[i] then result:=result+'T' else result:=result+'F';
    end;
  end;

  function modulesToString(modules_:TModules):string;
  var
    i:integer;
  begin
   result:='';
   for i:=0 to pred(modules_.Size) do
     begin
     result:=result+modules_[i].fName+' ';
     end;
  end;

begin
//push the button either until the specified number of button pushes
//or until the state is the same as the initial state
writeln('modules '+modulesToString(fModules));

cycleLength:=0;
cycleCount:=0;
remainingPushes:=buttonPushes;
done:=false;
repeat
runSingle;
cycleLength:=cycleLength+1;
remainingPushes:=remainingPushes - 1;
writeln('Cycle length '+cycleLength.ToString+' initial state '+bitsToString(fInitialState)+' currentState '+bitstostring(fModules.getState));

if (fInitialState = fModules.getState) then
  begin
  //We can divide the number of buttonPushes by the cycle count to get the number of cycles
  cycleCount:=buttonPushes div cycleLength;
  //Now multiply the high count and low count by the cycle count
  fHighCount:=fHighCount * cycleCount;
  fLowCount:=fLowCount * cycleCount;
  remainingPushes:=remainingPushes - (cycleLength * (cycleCount - 1)) //since we've already done one full cycle
  end;
done:=remainingPushes = 0;
until done;
end;

procedure TQueueManager.push(entry_:TPulse);
begin
fQueue.Enqueue(entry_);
end;

function TQueueManager.pop: TPulse;
begin
result:= fQueue.Dequeue;
end;


{ TQueueEntry }

constructor TPulse.create(high_: boolean; source_:string);
begin
  fHigh:=high_;
  fSource:=source_;
end;

{ TDayTwenty }

constructor TDayTwenty.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
queue_:=TQueueManager.create;
end;

procedure TDayTwenty.runPartOne;
var
  highPulses,lowPulses:int64;
begin
  results.Clear;
  queue_:=TQueueManager.create;
  queue_.setUpModules(puzzleInputLines);
  queue_.run(1000);
  highPulses:=queue_.fHighCount;
  lowPulses:=queue_.fLowCount;
  results.Add('High pulses '+highPulses.ToString+' low pulses '+lowPulses.ToString+' product '+(highPulses*lowPulses).ToString);
end;

procedure TDayTwenty.runPartTwo;
begin
  results.Clear;
end;


end.


