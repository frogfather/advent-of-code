unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,iAoc,fileUtilities,lazLogger;
type
  
  { TDayOne }
  TDayOne = class(TInterfacedObject,iAdvent)
    private
    fResults:TStringlist;
    fData:string;
    function getResults:TStringlist;
    procedure runPartOne;
    procedure runPartTwo;
    public
    constructor create(filename:String);
    procedure run(partOne:boolean=true);
    property results:TStringlist read getResults;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename: String);
begin
  fData:=readStream(filename);
end;

function TDayOne.getResults: TStringlist;
begin
  result:=fResults;
end;

procedure TDayOne.run(partOne: boolean);
begin
  if partOne then runPartOne else runPartTwo;
end;

//Private methods
procedure TDayOne.runPartOne;
begin

end;

procedure TDayOne.runPartTwo;
begin

end;

end.

