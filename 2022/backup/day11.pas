unit day11;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;

type
  { TMonkey }
  TMonkey = class(TInterfacedObject)
  private
    fId: integer;
    fMultiply: integer;
    fAdd: integer;
    fDivide: integer;
    fIfTrue: integer;
    fIfFalse: integer;
    fItems: TIntArray;
    fInspected: integer;
    fNotifyThrowItem: TNotifyEvent;
    fThrownItem: integer;
    fThrowTo: integer;
    procedure doRound;
  public
    constructor Create(id, opMultiply, opAdd, testDivisor, testTrue, testFalse: integer;
      items: TIntArray; throwHandler: TNotifyEvent);
    property thrownItem: integer read fThrownItem;
    property throwTo: integer read fThrowTo;
    property inspectedItems:integer read fInspected;
  end;

  { TMonkeys }
  TMonkeys = array of TMonkey;

  { TMonkeysHelper }
  TMonkeysHelper = type helper for TMonkeys
    function size: integer;
    function push(element: TMonkey): integer;
  end;

  { TDayEleven }
  TDayEleven = class(TAocPuzzle)
  private
    fMonkeys: TMonkeys;
    procedure generateMonkeys;
    procedure handleThrownItem(Sender: TObject);
    function runPuzzle(rounds:integer):TIntArray;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayEleven }
constructor TDayEleven.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 11', paintbox_);
  fMonkeys := TMonkeys.Create;
end;

procedure TDayEleven.generateMonkeys;
var
  index: integer;
  monkeyId, opMultiply, opAdd, testDivisor, testTrue, testFalse: integer;
  stringItems: TStringArray;
  items: TIntArray;
  lineElements: TStringArray;
  firstWord: string;
begin
  items := TIntArray.Create;
  monkeyId := -1;
  opMultiply := 1;
  opAdd := 0;
  testDivisor := 1;
  testTrue := -1;
  testFalse := -1;
  for index := 0 to pred(puzzleInputLines.size) do
  begin
    if (puzzleInputLines[index] <> '') then
    begin
      lineElements := puzzleInputLines[index].Trim.Split(' ');
      firstWord := lineElements[0];
      case firstWord of
        'Monkey': monkeyId := lineElements[1].Substring(0, pred(length(lineElements[1])))
            .ToInteger;
        'Starting':
        begin
          //take the item after the colon and convert to intArray
          items := csvToIntArray(puzzleInputLines[index].Substring(
            puzzleInputLines[index].IndexOf(':') + 1).Trim);
        end;
        'Operation:':
        begin
          if (lineElements[lineElements.size - 2] = '*') then
          begin
            //if operation is old * old set multiply to -1
            if (lineElements[pred(lineElements.size)] = 'old') then
              opMultiply := -1
            else
              opMultiply := lineElements[pred(lineElements.size)].ToInteger;
            opAdd := 0;
          end
          else
          begin
            opAdd := lineElements[pred(lineElements.size)].ToInteger;
            opMultiply := 1;
          end;
        end;
        'Test:': testDivisor := lineElements[pred(lineElements.size)].ToInteger;
        'If':
        begin
          if (lineElements[1] = 'true:') then
            testTrue := lineElements[pred(lineElements.size)].ToInteger
          else
            testFalse := lineElements[pred(lineElements.size)].ToInteger;
        end;
      end;
    end;

    if (puzzleInputLines[index] = '') or (index = pred(puzzleInputLines.size)) then
    begin
      fMonkeys.push(TMonkey.Create(monkeyId, opMultiply, opAdd,
        testDivisor, testTrue, testFalse, items, @handleThrownItem));
      setLength(items, 0);
      monkeyId := -1;
      opMultiply := 1;
      opAdd := 0;
      testDivisor := 1;
      testTrue := -1;
      testFalse := -1;
    end;
  end;
end;

procedure TDayEleven.handleThrownItem(Sender: TObject);
begin
  if Sender is TMonkey then with Sender as TMonkey do
    fMonkeys[throwTo].fItems.push(thrownItem);
end;

function TDayEleven.runPuzzle(rounds: integer): TIntArray;
var
  monkeyIndex: integer;
  roundNo:integer;
begin
  generateMonkeys;

  for roundNo:=0 to 19 do
    begin
    for monkeyIndex := 0 to pred(fMonkeys.size) do
      fMonkeys[monkeyIndex].doRound;
    end;
  result:=TIntArray.create;

  for monkeyIndex:=0 to pred(fMonkeys.size) do
    result.push(fMonkeys[monkeyIndex].inspectedItems);

  sort(result,result.size,false);
end;

procedure TDayEleven.runPartOne;
var
  inspectCount:TIntArray;
begin
  inspectCount:=runPuzzle(20);
  results.add('Most active monkeys multiplied '+(inspectCount[0]*inspectCount[1]).toString);
end;

procedure TDayEleven.runPartTwo;

begin

end;

//=====================================

{ TMonkey }

constructor TMonkey.Create(id, opMultiply, opAdd, testDivisor, testTrue,
  testFalse: integer; items: TIntArray; throwHandler: TNotifyEvent);
begin
  fId := id;
  fmultiply := opMultiply;
  fAdd := opAdd;
  fDivide := testDivisor;
  fIfTrue := testTrue;
  fIfFalse := testFalse;
  fItems := items;
  fInspected := 0;
  fNotifyThrowItem := throwHandler;
end;

procedure TMonkey.doRound;
var
  index: integer;
  multiplier: integer;
begin
  if (fItems.size = 0) then exit;
  for index := 0 to pred(fItems.size) do
  begin
    fInspected := fInspected + 1;
    fThrownItem := fItems.shift;
    if (fMultiply = -1) then multiplier := fThrownItem
    else
      multiplier := fMultiply;
    fThrownItem := (fThrownItem * multiplier) + fAdd;
    fThrownItem := fThrownItem div 3;
    if (fThrownItem mod fDivide = 0) then
      fThrowTo := fIfTrue
    else
      fThrowTo := fIfFalse;
    fNotifyThrowItem(self);
  end;
end;

//======================================
//Type helper for TMonkeys to make adding and removing monkeys less of a pain

{ TMonkeysHelper }

function TMonkeysHelper.size: integer;
begin
  Result := length(self);
end;

function TMonkeysHelper.push(element: TMonkey): integer;
begin
  setLength(Self, self.size + 1);
  self[pred(self.size)] := element;
  Result := self.size;
end;

end.
