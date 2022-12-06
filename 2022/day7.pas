unit day7;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type
  
  { TDaySeven }

  TDaySeven = class(TAocPuzzle)
    private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDaySeven }

constructor TDaySeven.create(filename: string; paintbox_: TPaintbox);
begin

end;

procedure TDaySeven.runPartOne;
begin

end;

procedure TDaySeven.runPartTwo;
begin

end;

end.

