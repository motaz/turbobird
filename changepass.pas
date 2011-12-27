unit ChangePass;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfmChangePass }

  TfmChangePass = class(TForm)
    bbCanel: TBitBtn;
    bbCreate: TBitBtn;
    edPassword: TEdit;
    edConfirm: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure bbCreateClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmChangePass: TfmChangePass;

implementation

{ TfmChangePass }

procedure TfmChangePass.bbCreateClick(Sender: TObject);
begin
  if edPassword.Text = '' then
    ShowMessage('You have to input a password')
  else
  if edPassword.Text <> edConfirm.Text then
    ShowMessage('Passwords do not match')
  else
    ModalResult:= mrOK;
end;

initialization
  {$I changepass.lrs}

end.

