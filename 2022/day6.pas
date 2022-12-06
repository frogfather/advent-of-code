unit day6;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;
type
  
  { TDay6 }

  TDay6 = class(TAocPuzzle)
  private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDay6 }

constructor TDay6.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 6';
end;

procedure TDay6.runPartOne;
begin

end;

procedure TDay6.runPartTwo;
begin

end;

end.

