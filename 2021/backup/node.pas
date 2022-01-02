unit node;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

{ TInt } //provides an integer we can set to nil
  TInt = class(TInterfacedObject)
    private
    fValue:integer;
    public
    constructor create(value:integer);
    property value: integer read fValue write fValue;
  end;

  { TNode }
  TNode = class(TInterfacedObject)
    private
    fVal:TInt;
    fLeft: TNode;
    fRight: TNode;
    fParent: TNode;
    public
    constructor create(val:TInt);
    function getValue:integer;
    procedure setValue(val:integer);
    function toString:ansiString; override;
    property left: TNode read fLeft write fLeft;
    property right: TNode read fRight write fRight;
    property parent: TNode read fParent write fParent;
    property val: TInt read fVal write fVal;
  end;

implementation

{ TInt }
constructor TInt.create(value: integer);
begin
  fValue:=value;
end;

function TNode.getValue: integer;
begin
  if fVal <> nil
    then result := fVal.value
  else result:=0; //not strictly correct but we have to return something
end;

procedure TNode.setValue(val: integer);
begin
  if fVal = nil
    then fVal:= TInt.create(val)
  else fVal.value:=val;
end;

function TNode.toString: ansiString;
begin
  Result:='myToString';
end;

{ TNode }
constructor TNode.create(val: TInt);
begin
fLeft:=nil;
fRight:=nil;
fParent:=nil;
fVal:=val;
end;



end.

