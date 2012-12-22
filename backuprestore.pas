unit BackupRestore;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type

  { TfmBackupRestore }

  TfmBackupRestore = class(TForm)
    bbStart: TBitBtn;
    cbOperation: TComboBox;
    edBackup: TEdit;
    edPassword: TEdit;
    edTargetDatabase: TEdit;
    edHost: TEdit;
    edUserName: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    meLog: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    sbBrowseTargetdb: TSpeedButton;
    procedure bbStartClick(Sender: TObject);
    procedure sbBrowseTargetdbClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

    { private declarations }
  public
    fDatabase: string;
    procedure Init(Title, Database, User, Password: string);
    { public declarations }
  end; 

var
  fmBackupRestore: TfmBackupRestore;

implementation

{ TfmBackupRestore }

uses UnitFirebirdServices;

procedure TfmBackupRestore.SpeedButton1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.fbk';
  if ((cbOperation.ItemIndex = 0) and (SaveDialog1.Execute)) or
   ((cbOperation.ItemIndex = 1) and (OpenDialog1.Execute)) then
  begin
    if cbOperation.ItemIndex = 0 then
      edBackup.Text:= SaveDialog1.FileName
    else
      edBackup.Text:= OpenDialog1.FileName;
  end;

end;

procedure TfmBackupRestore.Init(Title, Database, User, Password: string);
begin
  fDatabase:= Database;
  edUserName.Text:= User;
  edPassword.Text:= Password;

  // Linux: servername:/path/test.fdb  or /path/test.fdb
  // Windows: servername:c:\path\test.fdb or c:\path\test.fdb
  if Pos(':', Trim(fDatabase)) > 2 then
  begin
    edHost.Text:= Trim(Copy(fDatabase, 1, Pos(':', fDatabase) - 1));
    edTargetDatabase.Text:= Trim(Copy(fDatabase, Pos(':', fDatabase) + 1, Length(fDatabase)));
  end
  else
  begin
    edHost.Text := 'localhost';
    edTargetDatabase.Text := fDatabase;
  end;

end;

procedure TfmBackupRestore.bbStartClick(Sender: TObject);
var
  FireBirdServices: TFirebirdServices;
  Res: Ansistring;
  ADatabase: string;
begin
  FireBirdServices:= TFirebirdServices.Create;
  FireBirdServices.VerboseOutput:= True;
  meLog.Clear;
  with FireBirdServices do
  begin
    HostName:= edHost.Text;
    DBName:= edTargetDatabase.Text;
    UserName := edUserName.Text;
    Password := edPassword.Text;
    BkpName := Trim(edBackup.Text);

    try
      AttachService;

      if cbOperation.ItemIndex = 0 then
        StartBackup
      else
        StartRestore;

      while ServiceQuery(Res) do
        meLog.Lines.Add(Res);
    finally
      DetachService;
    end;
    meLog.Lines.Add('');
    FireBirdServices.Free;
  end;

end;


procedure TfmBackupRestore.sbBrowseTargetdbClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.fdb';
  if SaveDialog1.Execute then
    edTargetDatabase.Text:= SaveDialog1.FileName;
end;

initialization
  {$I backuprestore.lrs}

end.

