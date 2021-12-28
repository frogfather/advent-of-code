unit treeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,node;

type
  { TtreeForm }

  TtreeForm = class(TForm)
    Button1: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure PaintBox1Paint(Sender: TObject);
  private
    fTree: TNode;

  public

  end;

var
  treeForm: TtreeForm;

implementation

{$R *.lfm}

{ TtreeForm }

procedure TtreeForm.PaintBox1Paint(Sender: TObject);
begin

end;

end.

