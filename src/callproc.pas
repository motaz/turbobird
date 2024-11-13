unit CallProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Buttons;

type

  { TfmCallProc }

  TfmCallProc = class(TForm)
    BitBtn1: TBitBtn;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmCallProc: TfmCallProc;

implementation

{ TfmCallProc }


procedure TfmCallProc.FormActivate(Sender: TObject);
begin
  StringGrid1.SetFocus;
end;

procedure TfmCallProc.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StringGrid1.Row:= 1;
end;

procedure TfmCallProc.BitBtn1Click(Sender: TObject);
begin
  StringGrid1.Row:= 1;
end;


initialization
  {$I callproc.lrs}

end.

