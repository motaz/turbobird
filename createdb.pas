unit createdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfmCreateDB }

  TfmCreateDB = class(TForm)
    bbCreate: TBitBtn;
    btBrowse: TButton;
    bbCancel: TBitBtn;
    cbCharset: TComboBox;
    edUserName: TEdit;
    edNewDatabase: TEdit;
    edPassword: TEdit;
    IBConnection1: TIBConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SaveDialog1: TSaveDialog;
    procedure bbCreateClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmCreateDB: TfmCreateDB;

implementation

uses Reg;

{ TfmCreateDB }

procedure TfmCreateDB.btBrowseClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    edNewDatabase.Text:= SaveDialog1.FileName;
  end;
end;

procedure TfmCreateDB.bbCreateClick(Sender: TObject);
begin
  IBConnection1.UserName:= edUserName.Text;
  IBConnection1.Password:= edPassword.Text;
  IBConnection1.DatabaseName:= edNewDatabase.Text;
  IBConnection1.CharSet:= cbCharset.Text;
  IBConnection1.CreateDB;

  ShowMessage('Successfully created');
  fmReg.edTitle.Clear;
  fmReg.edDatabaseName.Text:= edNewDatabase.Text;
  fmReg.edUserName.Text:= edUserName.Text;
  fmReg.edPassword.Text:= edPassword.Text;
  fmReg.cbCharset.Text:= cbCharset.Text;
  fmReg.NewReg:= True;
  ModalResult:= fmReg.ShowModal;
  ModalResult:= mrOK;
end;

initialization
  {$I createdb.lrs}

end.

