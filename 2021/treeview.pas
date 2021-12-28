unit treeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  TNodeInfo = record
    id: integer;
    level:integer;
    childLeft: integer;
    childRight: integer;
    value: string;
  end;

  TNodeArray = array of TNodeInfo;

  { TNodeGraphForm }

  TNodeGraphForm = class(TForm)
    bClose: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure PaintBox1Paint(Sender: TObject);
  private
    fNodes: TNodeArray;
  public
    property nodes: TNodeArray read fNodes write fNodes;
  end;

var
  NodeGraphForm: TNodeGraphForm;

implementation

{$R *.lfm}

{ TNodeGraphForm }

procedure TNodeGraphForm.PaintBox1Paint(Sender: TObject);
const
  nodeHSpacing: integer = 5;
  nodeVSpacing: integer = 5;
  topMargin:integer = 10;
var
  index,level,maxLevel,itemsAtLevel,maxItemsAtLevel:integer;
  nodeWidth,nodeHeight:integer;
  nodeRect:TRect;
begin
  //work out how much space
  //max number of items at the same level = width
  //max of level = depth
  maxLevel:=0;
  for index:= 0 to pred(length(fNodes)) do
    begin
    if fNodes[index].level > maxLevel then maxLevel:= fNodes[index].level;
    end;
  maxItemsAtLevel:=0;
  for level:=0 to maxLevel do
    begin
    itemsAtLevel:=0;
    for index:=0 to pred(length(fNodes)) do
      begin
      if fNodes[index].level = level then itemsAtLevel:= itemsAtLevel + 1;
      end;
    if itemsAtLevel > maxItemsAtLevel then maxItemsAtLevel:=itemsAtLevel;
    end;
  nodeWidth:= (paintbox1.Width div maxItemsAtLevel) - nodeHSpacing;
  nodeHeight:= ((paintbox1.Height - topMargin) div maxLevel) - nodeVSpacing;
  level:=0;
  nodeRect.Top:=paintbox1.Top + topMargin;
  nodeRect.Bottom:= (nodeRect.Top + nodeHeight);
  nodeRect.Left:=(paintbox1.Width div 2) - (nodeWidth div 2);
  nodeRect.Right:=nodeRect.Left + nodeWidth;
  paintbox1.Canvas.Ellipse(nodeRect);
end;

end.

