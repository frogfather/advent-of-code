unit visualise;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfVisualise }

  TfVisualise = class(TForm)
    PaintBox1: TPaintBox;
  private
    procedure doPaint(sender:TObject); virtual;
  public

  end;

var
  fVisualise: TfVisualise;

implementation

{$R *.lfm}

{ TfVisualise }

procedure TfVisualise.doPaint(sender: TObject);
begin

end;




end.

