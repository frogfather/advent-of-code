unit snailfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils;
type
  
  { TSnailfish }

  TSnailfish = class(TInterfacedObject)
    private
    fNumbers: TStringArray;
    fSum: int64;
    function addNumbers(number1,number2:string):string;
    function explodeNumber(input:string):string;
    function magnitude(input: string):int64;
    function replaceElementWithZero(input:string; elementStart:integer):string;
    public
    property sum: int64 read fSum;
    procedure doHomework;
  end;

implementation

{ TSnailfish }

function TSnailfish.addNumbers(number1, number2: string): string;
begin

end;

function TSnailfish.explodeNumber(input: string): string;
begin

end;

function TSnailfish.magnitude(input: string): int64;
begin

end;

function TSnailfish.replaceElementWithZero(input: string; elementStart: integer
  ): string;
var
  elementEnd:integer;
begin
  if (elementStart > pred(length(input))) or (input[elementStart] <> '[')
    then exit;
  elementEnd:=findCharPos(input,']',elementStart);
  result:=input.Substring(0,elementStart)+'0'+input.Substring(succ(elementEnd));
end;

procedure TSnailfish.doHomework;
var
  input,output:string;
begin
  input:='[[[[[9,8],1],2],3],4]';
  output:=replaceElementWithZero(input,4);
  writeLn(output);
end;

end.

