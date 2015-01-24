unit ViewView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, LCLType;

type

  { TfmViewView }

  TfmViewView = class(TForm)
      bbClose: TSpeedButton;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    seScript: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewView: TfmViewView;

implementation

{ TfmViewView }

procedure TfmViewView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmViewView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmViewView.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewview.lrs}

end.

