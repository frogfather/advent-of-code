unit day17;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,lgPriorityQueue,lgList,md5,hmac;
type

  TPriorityQueueEntry = record
    heatloss:integer;
    row:integer;
    col:integer;
    rowdir:integer;
    coldir:integer;
    steps:integer;
    class operator < (a, b: TPriorityQueueEntry): Boolean;
  end;

  TSeenQueueEntry = record
    row:integer;
    col:integer;
    rowdir:integer;
    coldir:integer;
    steps:integer;
    class operator < (a, b: TSeenQueueEntry): Boolean;
  end;

  { TSeenEqRel }

  TSeenEqRel = class
    class function HashCode(const aValue: TSeenQueueEntry): SizeInt;
    class function Equal(const L, R: TSeenQueueEntry): Boolean;
  end;

  { TDaySeventeen}
  TDaySeventeen = class(TAocPuzzle)
  private
  procedure setupGrid;
  procedure clearQueues;
  function pqEntry(hl,r,c,dr,dc,s:integer):TPriorityQueueEntry;
  function seenQueueEntry(pqEntry_:TPriorityQueueEntry):TSeenQueueEntry;
  function inRange(pqEntry_:TPriorityQueueEntry):boolean;
  function atEnd(pqEntry_:TPriorityQueueEntry):boolean;
  function atRightAngles(pqEntry_:TPriorityQueueEntry;direction:TPoint):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;



implementation
var
  grid_:T2DIntMap;
  seenq_:specialize TGLiteHashList<TSeenQueueEntry,TSeenEqRel>;
  directions_:TPointArray;
  pq_: specialize TGComparableBinHeapMin<TPriorityQueueEntry>;

{ TSeenEqRel }

class function TSeenEqRel.HashCode(const aValue: TSeenQueueEntry): SizeInt;
var
md5str:string;
begin
  md5Str:=hmacSha1('key',aValue.col.ToString+aValue.row.toString+aValue.coldir.ToString+aValue.rowdir.ToString+aValue.steps.ToString);
  result:=strToInt64('0x'+md5str.Substring(0,10));
end;

class function TSeenEqRel.Equal(const L, R: TSeenQueueEntry): Boolean;
begin
  result:= (L.col = R.col)
  and (L.row = R.row)
  and (L.rowdir = R.rowdir)
  and (L.coldir = R.coldir);
end;

class operator TPriorityQueueEntry.< (a, b: TPriorityQueueEntry): Boolean;
var
diff:integer;
begin
  diff := a.heatloss - b.heatloss;
  if (diff = 0) then diff:= a.row - b.row;
  if (diff = 0) then diff:= a.col - b.col;
  if (diff = 0) then diff:= a.rowdir - b.rowdir;
  if (diff = 0) then diff:= a.coldir - b.coldir;
  result:= diff < 0;
end;

class operator TSeenQueueEntry.< (a, b: TSeenQueueEntry): Boolean;
begin
  result:= (a.row < b.row) and (a.col < b.col);
end;

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
  pq_.Clear;
  seenq_.Clear;
end;

function TDaySeventeen.pqEntry(hl, r, c, dr, dc, s: integer
  ): TPriorityQueueEntry;
begin
  result.heatloss:=hl;
  result.row:=r;
  result.col:=c;
  result.rowdir:=dr;
  result.coldir:=dc;
  result.steps:=s;
end;

function TDaySeventeen.seenQueueEntry(pqEntry_: TPriorityQueueEntry
  ): TSeenQueueEntry;
begin
  result.row:=pqEntry_.row;
  result.col:=pqEntry_.col;
  result.rowdir:=pqEntry_.rowdir;
  result.coldir:=pqEntry_.coldir;
  result.steps:=pqEntry_.steps;
end;

function TDaySeventeen.inRange(pqEntry_: TPriorityQueueEntry): boolean;
begin
  result:= (pqEntry_.row >= 0)
    and (pqEntry_.row < grid_.rows)
    and (pqEntry_.col >= 0)
    and (pqEntry_.col < grid_.size(pqEntry_.row));
end;

function TDaySeventeen.atEnd(pqEntry_: TPriorityQueueEntry): boolean;
begin
  result:= (pqEntry_.col = pred(grid_.size(pqEntry_.row))) and (pqEntry_.row = pred(grid_.rows));
end;

function TDaySeventeen.atRightAngles(pqEntry_: TPriorityQueueEntry;
  direction: TPoint): boolean;
begin
  result:=(pqEntry_.coldir * direction.X = 0) and (pqEntry_.rowdir * direction.Y = 0);
end;

constructor TDaySeventeen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
setupGrid;
pq_:= specialize TGComparableBinHeapMin<TPriorityQueueEntry>.Create;
directions_:=TPointArray.create;
directions_.push(TPoint.Create(1,0));
directions_.push(TPoint.Create(-1,0));
directions_.push(TPoint.Create(0,1));
directions_.push(TPoint.Create(0,-1));
end;

procedure TDaySeventeen.runPartOne;
var
  minEntry,nextEntry:TPriorityQueueEntry;
  seenEntry:TSeenQueueEntry;
  dirNo:integer;
  direction:TPoint;
begin
  results.Clear;
  clearQueues;
  pq_.Enqueue(pqEntry(0,0,0,0,0,0));
  while not pq_.IsEmpty do
    begin
    minEntry:=pq_.Dequeue;
    if atEnd(minEntry) then
      begin
      results.Add('Total heat loss '+minEntry.heatloss.ToString);
      exit;
      end;
    seenEntry:=seenQueueEntry(minEntry);
    if (seenq_.IndexOf(seenEntry) >- 1) then
      begin
      continue;
      end;
    seenq_.Add(seenEntry);
    if (minEntry.steps < 3) and not( (minEntry.rowdir = 0) and (minEntry.coldir = 0) )then
      begin
      nextEntry.row:=minEntry.row + minEntry.rowdir;
      nextEntry.col:=minEntry.col + minEntry.coldir;
      nextEntry.rowdir:=minEntry.rowdir;
      nextEntry.coldir:=minEntry.coldir;
      nextEntry.steps:=minEntry.steps+1;
      if inRange(nextEntry) then
          begin
          nextEntry.heatloss:=minEntry.heatloss+grid_[nextEntry.row][nextEntry.col];
          pq_.Enqueue(nextEntry);
          end;
      end;
    for dirNo:=0 to pred(directions_.size) do
      begin
      direction:=directions_[dirNo];
      if atRightAngles(minEntry,direction) then
        begin
        nextEntry.row:=minEntry.row + direction.Y;
        nextEntry.col:=minEntry.col + direction.X;
        nextEntry.rowdir:=direction.Y;
        nextEntry.coldir:=direction.X;
        nextEntry.steps:=1;
        if inRange(nextEntry) then
          begin
          nextEntry.heatloss:=minEntry.heatloss+grid_[nextEntry.row][nextEntry.col];
          pq_.Enqueue(nextEntry);
          end;
        end;
      end;
    end;
end;

procedure TDaySeventeen.runPartTwo;
var
  minEntry,nextEntry:TPriorityQueueEntry;
  seenEntry:TSeenQueueEntry;
  dirNo:integer;
  direction:TPoint;
begin
  results.Clear;
  clearQueues;
  pq_.Enqueue(pqEntry(0,0,0,0,0,0));
  while not pq_.IsEmpty do
    begin
    minEntry:=pq_.Dequeue;
    if atEnd(minEntry) and (minEntry.steps >= 4) then
      begin
      results.Add('Total heat loss '+minEntry.heatloss.ToString);
      exit;
      end;
    seenEntry:=seenQueueEntry(minEntry);
    if (seenq_.IndexOf(seenEntry) >- 1) then
      begin
      continue;
      end;
    seenq_.Add(seenEntry);
    if (minEntry.steps < 10) and not( (minEntry.rowdir = 0) and (minEntry.coldir = 0) )then
      begin
      nextEntry.row:=minEntry.row + minEntry.rowdir;
      nextEntry.col:=minEntry.col + minEntry.coldir;
      nextEntry.rowdir:=minEntry.rowdir;
      nextEntry.coldir:=minEntry.coldir;
      nextEntry.steps:=minEntry.steps+1;
      if inRange(nextEntry) then
          begin
          nextEntry.heatloss:=minEntry.heatloss+grid_[nextEntry.row][nextEntry.col];
          pq_.Enqueue(nextEntry);
          end;
      end;
    //If we have gone at least 4 blocks we can turn
    if (minEntry.steps >= 4) or ((minEntry.rowdir = 0) and (minEntry.coldir = 0)) then
      for dirNo:=0 to pred(directions_.size) do
        begin
        direction:=directions_[dirNo];
        if atRightAngles(minEntry,direction) then
          begin
          nextEntry.row:=minEntry.row + direction.Y;
          nextEntry.col:=minEntry.col + direction.X;
          nextEntry.rowdir:=direction.Y;
          nextEntry.coldir:=direction.X;
          nextEntry.steps:=1;
          if inRange(nextEntry) then
            begin
            nextEntry.heatloss:=minEntry.heatloss+grid_[nextEntry.row][nextEntry.col];
            pq_.Enqueue(nextEntry);
            end;
          end;
        end;
    end;
end;



end.


