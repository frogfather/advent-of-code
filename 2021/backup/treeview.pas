unit treeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,node,arrayUtils;

type
  { TNodeInfo }
  TNodeInfo = record
    level:integer;
    id:integer;
    parent:integer;
    leftChild:integer;
    rightChild:integer;
    value:integer;
  end;

  { TNodeInfoList }
  TNodeInfoList = array of TNodeInfo;


  { TtreeForm }

  TtreeForm = class(TForm)
    Button1: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure setTreeDimensions;
    procedure crawlTree(node:TNode;level:integer; nodeInfo: TNodeInfo);
    procedure addToNodeInfoList(nodeInfo:TNodeInfo);
  private
    fTree: TNode;
    fLevelDistribution: TIntArray;
    fMaxDepth:integer;
    fMaxWidth:integer;
    fNodeInfoList: TNodeInfoList;
    fNodeInfoId: integer;
    function getNodeInfoId:integer;
  public
    property tree: TNode read fTree write fTree;
    property nodeInfoId: integer read getNodeInfoId;
  end;

var
  treeForm: TtreeForm;

implementation

{$R *.lfm}

{ TtreeForm }

procedure TtreeForm.PaintBox1Paint(Sender: TObject);
const
  margin:integer = 10;
  nodeHMargin: integer = 5;
  nodeVMargin: integer = 5;
  var
    index:integer;
    nodeInfo:TNodeInfo;
    nodestats:String;
begin
  //we should have the max depth and width
  for index:= 0 to pred(length(fNodeInfoList)) do
    begin
      nodeInfo:=fNodeInfoList[index];
      nodeStats:='Node: '+nodeInfo.id.ToString
          +', parent: '+nodeInfo.parent.ToString;
      nodeStats:=nodeStats+', leftChild: '+nodeInfo.leftChild.ToString
          +', rightChild: '+nodeInfo.rightChild.ToString;
      nodeStats:=nodeStats+', value: '+nodeInfo.value.ToString;
      paintbox1.Canvas.TextOut(10,index*20, nodeStats);
    end;
end;

procedure TtreeForm.FormShow(Sender: TObject);
begin
  setTreeDimensions;
end;

procedure TtreeForm.FormCreate(Sender: TObject);
begin
  fLevelDistribution:=TIntArray.create;
  fNodeInfoList:= TNodeInfoList.create;
end;

procedure TtreeForm.setTreeDimensions;
var
  index:integer;
  initialNodeInfo:TNodeInfo;
begin
  fMaxDepth:=0;
  fMaxWidth:=0;
  fNodeInfoId:=0;
  initialNodeInfo.id:= nodeInfoId;
  initialNodeInfo.level:=0;
  initialNodeInfo.parent:=-1;
  initialNodeInfo.leftChild:=-1;
  initialNodeInfo.rightChild:=-1;
  initialNodeInfo.value:=-1;
  crawlTree(fTree, 0, initialNodeInfo);
  //levelDistribution should show how many items at this level
  for index:= 0 to pred(length(fLevelDistribution)) do
    if fLevelDistribution[index] > fMaxWidth then fMaxWidth:=fLevelDistribution[index];
end;

procedure TtreeForm.crawlTree(node: TNode; level:integer; nodeInfo:TNodeInfo);
var
  leftNodeInfo,rightNodeInfo:TNodeInfo;
  leftId,rightId:integer;
  leftVal,rightVal:integer;
begin
  if level > fMaxDepth then fMaxDepth:=level;
  if level > pred(length(fLevelDistribution)) then
    begin
    setLength(fLevelDistribution,length(fLevelDistribution)+1);
    fLevelDistribution[pred(length(fLevelDistribution))]:=0;
    end;
  fLevelDistribution[level]:= fLevelDistribution[level] + 1;
  //we should create some objects to hold information about the tree to make drawing easier

  if (node.left <> nil) or (node.right <> nil) then
    begin
    if node.left <> nil then
      begin
      leftId:= nodeInfoId;
      if node.left.val <> nil
        then leftVal:= node.left.val.value else leftVal:=-1;
      end else leftId:=-1;
    if node.right <> nil then
      begin
      rightId:= nodeInfoId;
      if node.right.val <> nil
        then rightVal:=node.right.val.value else rightVal:=-1;
      end else rightId:= -1;
    nodeInfo.leftChild:=leftId;
    nodeInfo.rightChild:=rightId;
    addToNodeInfoList(nodeInfo);

    if node.left <> nil then
      begin
      level:=level+1;
      leftNodeInfo.id:=leftId;
      leftNodeInfo.level:= level;
      leftNodeInfo.value:=leftVal;
      leftNodeInfo.parent:=nodeInfo.id;
      leftNodeInfo.leftChild:=-1;
      leftNodeInfo.rightChild:=-1;
      crawlTree(node.left, level, leftNodeInfo);
      level:=level-1;
      end;
    if node.right <> nil then
      begin
      level:=level+1;
      rightNodeInfo.id:=rightId;
      rightNodeInfo.level:= level;
      rightNodeInfo.value:=rightVal;
      rightNodeInfo.parent:=nodeInfo.id;
      rightNodeInfo.leftChild:=-1;
      rightNodeInfo.rightChild:=-1;
      crawlTree(node.right, level, rightNodeInfo);
      level:=level-1;
      end;
    end else addToNodeInfoList(nodeInfo);
end;

procedure TtreeForm.addToNodeInfoList(nodeInfo: TNodeInfo);
begin
  //should check if it's there
  setLength(fNodeInfoList,length(fNodeInfoList)+1);
  fNodeInfoList[pred(length(fNodeInfoList))]:= nodeInfo;
end;

function TtreeForm.getNodeInfoId: integer;
begin
  result:=fNodeInfoId;
  fNodeInfoId:= fNodeInfoId + 1;
end;

end.

