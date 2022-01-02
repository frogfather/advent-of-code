unit treeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,node,arrayUtils,math;

type
  { TtreeForm }

  TtreeForm = class(TForm)
    Button1: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure setTreeDimensions;
    procedure drawNode(node:TNode; nodePos: TPoint);
    procedure drawLink(parentNodePos,childNodePos:TPoint);
    procedure crawlTree(node:TNode;level,hPos:integer; doPaint:boolean=false);
  private
    fNode: TNode;
    fMaxDepth:integer;
    fNodeHeight:integer;
    fBrushColour:TColor;
    fPenColour:TColor;
    property nodeHeight: integer read fNodeHeight write fNodeHeight;
  public
    property tree: TNode read fNode write fNode;
  end;

var
  treeForm: TtreeForm;

implementation

{$R *.lfm}

{ TtreeForm }

procedure TtreeForm.PaintBox1Paint(Sender: TObject);
begin
  with paintbox1 do
    begin
    canvas.Brush.Color:=clGray;
    canvas.rectangle(0,0,canvas.width,canvas.height);
    crawlTree(fNode,0,paintbox1.Width div 2, true);
    end;
end;

procedure TtreeForm.FormShow(Sender: TObject);
begin
  setTreeDimensions;
  fPenColour:=clWhite;
  fBrushColour:=$00009900;
end;

procedure TtreeForm.setTreeDimensions;
begin
  fMaxDepth:=0;
  crawlTree(fNode,0, paintbox1.Width div 2);
  nodeHeight:=paintbox1.Height div (2 * fMaxDepth);
end;

procedure TtreeForm.drawNode(node:TNode; nodePos:TPoint);
var
  nodeWidth,nodeHt:integer;
  nodeRect:TRect;
  nodeText:String;
begin
  nodeWidth:=paintbox1.Width div round(power(2,fMaxDepth));
  nodeHt:= (nodeWidth * 2) div 3;
  nodeRect.Left:=nodePos.X - (nodeWidth div 2);
  nodeRect.Right:=nodePos.X + (nodeWidth div 2);
  nodeRect.Top:=nodePos.Y;
  nodeRect.Bottom:=nodePos.Y + nodeHt;
  if node.val = nil then nodeText:= node.ToString
  else nodeText:=node.val.value.ToString;
  with paintbox1.Canvas do
    begin
    brush.Color:=$00009901;
    brush.Color:=fBrushColour;
    pen.Color:=clYellow;
    pen.Color:=fPenColour;
    Ellipse(nodeRect);
    TextOut(
      nodeRect.Left + (nodeWidth div 2) - (TextExtent(nodeText).Width div 2),
      nodeRect.Top + (nodeHt div 4),
      nodeText);
    end;
end;

procedure TtreeForm.drawLink(parentNodePos,childNodePos:TPoint);
begin
  with paintbox1.Canvas do
    begin
    brush.Color:=$00009901;
    brush.Color:=fBrushColour;
    pen.Color:=clYellow;
    pen.Color:=fPenColour;
    MoveTo(parentNodePos);
    LineTo(childNodePos);
    end;
end;

procedure TtreeForm.crawlTree(node: TNode; level,hPos:integer; doPaint: boolean=false);
var
  leftBranchCentre,rightBranchCentre:integer;
  levelStart,nextLevelStart,itemsOnNextLevel:integer;
  nodePos,childPos:TPoint;
begin
  levelStart:=(level * NodeHeight) + (nodeHeight div 2);
  if not doPaint then
    begin
    if level > fMaxDepth then fMaxDepth:=level;
    end else
    begin
    nodePos.X:= hPos;
    nodePos.Y:= levelStart;
    itemsOnNextLevel:=round(power(2,level + 1));
    nextLevelStart:=((level + 1) * NodeHeight) + (nodeHeight div 2);
    leftBranchCentre:= hPos - (paintbox1.Width div (2*itemsOnNextLevel));
    rightBranchCentre:= hPos + (paintbox1.Width div (2*itemsOnNextLevel));
    if node.left <> nil then
      begin
      childPos.X:=leftBranchCentre;
      childPos.Y:=nextLevelStart;
      drawLink(nodePos,childPos);
      end;
    if node.right <> nil then
      begin
      childPos.X:=rightBranchCentre;
      childPos.Y:=nextLevelStart;
      drawLink(nodePos,childPos);
      end;
    drawNode(node, nodePos);
    end;
  if node.left <> nil then
    begin
    level:=level+1;
    crawlTree(node.left, level, leftBranchCentre, doPaint);
    level:=level-1;
    end;
  if node.right <> nil then
    begin
    level:=level+1;
    crawlTree(node.right, level, rightBranchCentre, doPaint);
    level:=level-1;
    end;
end;


end.

