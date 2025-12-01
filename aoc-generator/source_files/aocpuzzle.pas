unit aocPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,iAoc,fileUtilities, lazLogger,graphics,ExtCtrls;

type
  
  { TAocPuzzle }

  TAocPuzzle = class(TInterfacedObject, iAdvent)
  private
    fName:string;
    fPaintBox:TPaintbox;
    fResults: TStringList;
    fData: string;
    fDataAsLines: TStringArray;
    function getResults: TStringList;
    procedure setResults(results_:TStringlist);
  public
    constructor Create(filename,name_: string;paintbox:TPaintBox = nil);
    procedure runPartOne; virtual abstract;
    procedure runPartTwo; virtual abstract;
    procedure run(partOne: boolean = True);
    property name: string read fName;
    property results: TStringList read getResults write setResults;
    property paintbox: TPaintbox read fPaintbox;
    property puzzleInput:string read fData;
    property puzzleInputLines: TStringArray read fDataAsLines;
  end;

implementation

{ TAocPuzzle }

constructor TAocPuzzle.Create(filename,name_: string; paintbox: TPaintBox);
begin
  fName:=name_;
  fResults:=TStringlist.Create;
  fPaintbox:=paintbox;
  try
  fData:=readStream(filename);
  fDataAsLines:=fData.Split(#$0A,TStringSplitOptions.ExcludeLastEmpty);
  except
  on E: EFOpenError do
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
  fResults:=results_;
end;

end.
