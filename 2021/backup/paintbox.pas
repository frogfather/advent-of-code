unit paintbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,arrayUtils;

type

  { TpaintboxForm }

  TpaintboxForm = class(TForm)
    Button1: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure PaintBox1Paint(Sender: TObject);
  private
    //This is far too specialized - should be an array of things to paint
    fMap: T3DIntMap;
    fColours: TColours;
  public
    property map: T3DIntMap read fMap write fMap;
    property colours: TColours read fColours write fColours;
  end;

var
  paintboxForm: TpaintboxForm;

implementation

{$R *.lfm}

{ TpaintboxForm }

procedure TpaintboxForm.PaintBox1Paint(Sender: TObject);
const
  elementWidth:integer = 8;
  elementHeight:integer = 8;
  elementHBorder:integer = 0;
  elementVBorder:integer = 0;
var
x,y:integer;
basinNumber:integer;
basinColor:TColor;
boxL,boxR,boxT,boxB:integer;
  function generateNormalisedRandomValue(normalizeTo,offset:integer):byte;
    begin
    result:= random(normalizeTo)+offset mod normalizeTo;
    end;

begin
if length(fmap)=0 then exit;
with paintboxForm.PaintBox1 do
  begin
  for x:=0 to pred(length(map)) do
    begin
    for y:=0 to pred(length(map[0])) do
      begin
      basinNumber:=map[x][y][1];
      if (basinNumber > -1)
        then basinColor:=colours[basinNumber] else basinColor:=clWhite;
      canvas.brush.Color:=basinColor;
      boxL:=left+(x*(elementWidth+elementHBorder));
      boxT:=top +(y*(elementHeight+elementVBorder));
      boxR:=left+(x*(elementWidth+elementHBorder)) + elementWidth;
      boxB:=top +(y*(elementHeight+elementVBorder)) + elementHeight;
      canvas.Rectangle(boxL,boxT,boxR,boxB);
      if (x < pred(length(map))) and (map[x+1][y][1] <> basinNumber) then
        begin
        canvas.moveto(boxL,boxB);
        canvas.LineTo(boxR,boxB);
        end;
      end;
    end;
  end;
end;

end.

