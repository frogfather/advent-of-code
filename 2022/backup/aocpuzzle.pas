unit aocPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,iAoc,fileUtilities, lazLogger,graphics,ExtCtrls;

type
  
  { TAocPuzzle }

  TAocPuzzle = class(TInterfacedObject, iAdvent)
  private
    fPaintBox:TPaintbox;
    fResults: TStringList;
    fData: string;
    function getResults: TStringList;
    procedure setResults(results_:TStringlist);
    procedure runPartOne; virtual abstract;
    procedure runPartTwo; virtual abstract;
  public
    constructor Create(filename: string;paintbox:TPaintBox = nil);
    procedure run(partOne: boolean = True);
    property results: TStringList read getResults write setResults;
    property paintbox: TPaintbox read fPaintbox;
  end;

implementation

{ TAocPuzzle }

constructor TAocPuzzle.Create(filename: string; paintbox: TPaintBox);
begin
  fPaintbox:=paintbox;
  try
  fData:=readStream(filename);
  except
  on E: Exception do
  DebugLn('Error reading puzzle file '+E.Message);
  end;
end;

procedure TAocPuzzle.run(partOne: boolean);
begin
  if partOne then runPartOne else runPartTwo;
end;

function TAocPuzzle.getResults: TStringList;
begin
  result:=fResults;
end;

procedure TAocPuzzle.setResults(results_: TStringlist);
begin
  if (results_ is TStringlist) then fResults:=results_;
end;

end.
