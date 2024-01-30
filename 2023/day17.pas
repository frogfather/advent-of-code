unit day17;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,anysort;
type

  TPriorityQueueEntry = record
    heatloss:integer;
    row:integer;
    col:integer;
    rowdir:integer;
    coldir:integer;
    steps:integer;
  end;

  TSeenQueueEntry = record
    row:integer;
    col:integer;
    rowdir:integer;
    coldir:integer;
    steps:integer;
  end;

  TPriorityQueue = array of TPriorityQueueEntry;
  TSeenQueue = array of TSeenQueueEntry;

  { TPriorityQueueHelper }

  TPriorityQueueHelper = type helper for TPriorityQueue
  function size:integer;
  procedure push(element:TPriorityQueueEntry);
  function pop: TPriorityQueueEntry;
  procedure sort(ascending:boolean=true);
  end;

  { TSeenQueueHelper }
  TSeenQueueHelper = type helper for TSeenQueue
  function size:integer;
  procedure push(element:TSeenQueueEntry);
  function seen(element:TSeenQueueEntry):boolean;
  procedure sort(ascending:boolean=true);
  end;

  { TDaySeventeen}
  TDaySeventeen = class(TAocPuzzle)
  private
  procedure setupGrid;
  procedure clearQueues;
  function seenEntryFromPQEntry(pqEntry:TPriorityQueueEntry):TSeenQueueEntry;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  grid_:T2DIntMap;
  pq_: TPriorityQueue;
  seen_:TSeenQueue;

{ TDaySeventeen }

procedure TDaySeventeen.setupGrid;
  var
    row,col:integer;
begin
  grid_:=T2DIntMap.create;
  for row:=0 to pred(puzzleInputLines.size) do
    for col:=0 to pred(length(puzzleInputLines[row])) do
    grid_.push(row,puzzleInputLines[row].Substring(col,1).ToInteger);
end;

procedure TDaySeventeen.clearQueues;
begin
  setLength(pq_,0);
  setLength(seen_,0);
end;

function TDaySeventeen.seenEntryFromPQEntry(pqEntry: TPriorityQueueEntry
  ): TSeenQueueEntry;
begin
  result.row:=pqEntry.row;
  result.col:=pqEntry.col;
  result.rowdir:=pqEntry.rowdir;
  result.coldir:=pqEntry.coldir;
  result.steps:=pqEntry.steps;
end;

constructor TDaySeventeen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
setupGrid;
pq_:=TPriorityQueue.create;
seen_:=TSeenQueue.create;
end;

procedure TDaySeventeen.runPartOne;
var
  pqentry:TPriorityQueueEntry;
  seenEntry:TSeenQueueEntry;
begin
  results.Clear;
  clearQueues;
  with pqentry do
    begin
    heatloss:=0;
    row:=0;
    col:=0;
    rowdir:=0;
    coldir:=0;
    steps:=0;
    end;
  pq_.push(pqentry);
  while (pq_.size > 0) do
    begin
    pqentry:=pq_.pop;
    //If we're at the end, log and quit
    if (pqentry.row = pred(grid_.rows)) and (pqentry.col = pred(grid_.size(pqentry.row)))
      then
        begin
        results.add('minimum heat loss '+pqentry.heatloss.ToString);
        exit;
        end;
    //If we've already been there then ignore
    seenEntry:=seenEntryFromPqEntry(pqEntry);
    if seen_.seen(seenEntry) then continue;
    //Add the entry to seen
    seen_.push(seenEntry);
    if (seen_.seen(seenEntry)) then results.Add('item in seen queue found');
    end;

// 10 if we've gone less than 3 steps in this direction continue in that direction
// 11 If we're in range push the updated entry onto the priority queue
// 12 for all directions from this position, if we're in range, and not going in the current direction or back then add to the queue

end;

procedure TDaySeventeen.runPartTwo;
begin
  results.Clear;
end;

{ TPriorityQueueHelper }
function ComparePriorityQueueEntryAsc(const d1,d2):integer;
var
  s1: TPriorityQueueEntry absolute d1;
  s2: TPriorityQueueEntry absolute d2;
  comparison:integer;
begin
  comparison:= s1.heatloss - s2.heatloss;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function ComparePriorityQueueEntryDesc(const d1,d2):integer;
var
  s1: TPriorityQueueEntry absolute d1;
  s2: TPriorityQueueEntry absolute d2;
  comparison:integer;
begin
  comparison:= s2.heatloss - s1.heatloss;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function TPriorityQueueHelper.size: integer;
begin
  result:=length(self);
end;

procedure TPriorityQueueHelper.push(element:TPriorityQueueEntry);
begin
  insert(element,self,length(self));
  self.sort();
end;

function TPriorityQueueHelper.pop: TPriorityQueueEntry;
begin
  if (self.size > 0) then result:=self[0];
  { #todo -oJC : Remove the item from the array }
end;

procedure TPriorityQueueHelper.sort(ascending: boolean);
begin
  if ascending then
    anysort.AnySort(self, length(self), sizeof(TPriorityQueueEntry), @ComparePriorityQueueEntryAsc)
  else
    anysort.AnySort(self, length(self), sizeof(TPriorityQueueEntry), @ComparePriorityQueueEntryDesc)
end;

{ TSeenQueueHelper }
function CompareSeenQueueEntryAsc(const d1,d2):integer;
var
  s1: TSeenQueueEntry absolute d1;
  s2: TSeenQueueEntry absolute d2;
  comparison:integer;
begin
  comparison:= s1.row - s2.row;
  if (comparison=0) then comparison:= s1.col - s2.col;
  if (comparison=0) then comparison:= s1.rowdir - s2.rowdir;
  if (comparison=0) then comparison:= s1.coldir - s2.coldir;
  if (comparison=0) then comparison:= s1.steps - s2.steps;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function CompareSeenQueueEntryDesc(const d1,d2):integer;
var
  s1: TSeenQueueEntry absolute d1;
  s2: TSeenQueueEntry absolute d2;
  comparison:integer;
begin
  comparison:= s2.row - s1.row;
  if (comparison=0) then comparison:= s2.col - s1.col;
  if (comparison=0) then comparison:= s2.rowdir - s1.rowdir;
  if (comparison=0) then comparison:= s2.coldir - s1.coldir;
  if (comparison=0) then comparison:= s2.steps - s1.steps;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function TSeenQueueHelper.size: integer;
begin
  result:=length(self);
end;

procedure TSeenQueueHelper.push(element: TSeenQueueEntry);
begin
  insert(element,self,length(self));
  self.sort();
end;

function TSeenQueueHelper.seen(element:TSeenQueueEntry): boolean;
var
  index:integer;
  found:boolean;
begin
  result:=false;
  //See if starting at 0 is usable - if not bisect
  if (self.size = 0) then exit;
  index:=0;
  found:=false;
  repeat
  //entries are sorted by row, col, rowdir, coldir, steps
  //so if the current element has a higher value that the required one then it's not there
  if (self[index].row > element.row)
  or (self[index].col > element.col) then exit;

  found:= (self[index].row = element.row)
  and (self[index].col = element.col)
  and (self[index].rowdir = element.rowdir)
  and (self[index].coldir = element.coldir)
  and (self[index].steps = element.steps);

  if index < pred(self.size) then index:=index+1 else exit;
  until found;
  result:=true;
end;

procedure TSeenQueueHelper.sort(ascending: boolean);
begin
  if ascending then
    anysort.AnySort(self, length(self), sizeof(TSeenQueueEntry), @CompareSeenQueueEntryAsc)
  else
    anysort.AnySort(self, length(self), sizeof(TSeenQueueEntry), @CompareSeenQueueEntryDesc)
end;


end.


