unit CreateTrigger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfmCreateTrigger }

  TfmCreateTrigger = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbTables: TComboBox;
    cxUpdate: TCheckBox;
    cxDelete: TCheckBox;
    cxInsert: TCheckBox;
    edTriggerName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rbAfter: TRadioButton;
    rbBefor: TRadioButton;
    procedure BitBtn1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmCreateTrigger: TfmCreateTrigger;

implementation

{ TfmCreateTrigger }

procedure TfmCreateTrigger.BitBtn1Click(Sender: TObject);
begin
  if (Trim(edTriggerName.Text) = '') or (not (cxDelete.Checked or cxInsert.Checked or cxUpdate.Checked)) then
    MessageDlg('Incorrect configuration', mtError, [mbOk], 0)
  else
    ModalResult:= mrOK;
end;

initialization
  {$I createtrigger.lrs}

end.

