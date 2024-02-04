unit day17;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,lgPriorityQueue,lgList,md5;
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
begin
  result:=0;
end;

class function TSeenEqRel.Equal(const L, R: TSeenQueueEntry): Boolean;
begin
  result:= (L.col = R.col)
  and (L.row = R.row)
  and (L.rowdir = R.rowdir)
  and (L.coldir = R.coldir);
end;

class operator TPriorityQueueEntry.< (a, b: TPriorityQueueEntry): Boolean;
begin
  result:= a.heatloss < b.heatloss;
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
  test:TSeenQueueEntry;
  output:sizeInt;
  mstr:string;
begin
  results.Clear;

end;

procedure TDaySeventeen.runPartTwo;
begin
  results.Clear;
end;



end.


