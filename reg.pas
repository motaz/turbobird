unit Reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  TRegisteredDatabase = packed record
    Title: string[30];
    DatabaseName: string[200];
    UserName: string[100];
    Password: string[100];
    Charset: string[40];
    Deleted: Boolean;
    SavePassword: Boolean;
    Role: string[100];
    LastOpened: TDateTime;
    Reserved: array [0 .. 40] of Byte;
  end;

  { TfmReg }

  TfmReg = class(TForm)
    bbCancel: TBitBtn;
    bbTest: TBitBtn;
    bbReg: TBitBtn;
    btBrowse: TButton;
    cbCharset: TComboBox;
    cxSavePassword: TCheckBox;
    edRole: TEdit;
    edDatabaseName: TEdit;
    edTitle: TEdit;
    edPassword: TEdit;
    edUserName: TEdit;
    IBConnection1: TIBConnection;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog1: TOpenDialog;
    procedure bbRegClick(Sender: TObject);
    procedure bbTestClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);

  private
    { private declarations }
    function EditRegisteration(Index: Integer; Title, DatabaseName, UserName, Password, Charset, Role: string;
      SavePassword: Boolean): Boolean;
  public
    { public declarations }
    NewReg: Boolean;
    RecPos: Integer;
    function RegisterDatabase(Title, DatabaseName, UserName, Password, Charset, Role: string;
      SavePassword: Boolean): Boolean;
    function TestConnection(DatabaseName, UserName, Password, Charset: string): Boolean;
    function GetEmptyRec: Integer;
    function SaveRegistrations: Boolean;
    procedure Sort;
  end;

var
  fmReg: TfmReg;

implementation

{ TfmReg }

uses main;

procedure TfmReg.bbRegClick(Sender: TObject);
begin
  if Trim(edTitle.Text) = '' then
    ShowMessage('You should fill all fields')
  else
  if TestConnection(edDatabaseName.Text, edUserName.Text, edPassword.Text, cbCharset.Text) then
  if NewReg then  // New registration
  begin
    if RegisterDatabase(edTitle.Text, edDatabaseName.Text, edUserName.Text, edPassword.Text, cbCharset.Text,
      edRole.Text, cxSavePassword.Checked) then
       ModalResult:= mrOK;
  end
  else // if not NewReg, edit registration
    if EditRegisteration(RecPos, edTitle.Text, edDatabaseName.Text, edUserName.Text, edPassword.Text,
      cbCharset.Text, edRole.Text, cxSavePassword.Checked) then
      MOdalResult:= mrOk;
end;

procedure TfmReg.bbTestClick(Sender: TObject);
begin
  if TestConnection(edDatabaseName.Text, edUserName.Text, edPassword.Text, cbCharset.Text) then
    ShowMessage('Connected successfully');
end;

procedure TfmReg.btBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edDatabaseName.Text:= OpenDialog1.FileName;
end;



function TfmReg.RegisterDatabase(Title, DatabaseName, UserName, Password, Charset, Role: string; SavePassword: Boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  EmptyIndex: Integer;
  FileName: string;
begin
  try
    FileName:= fmMain.getConfigurationDirectory + 'turbobird.reg';

    AssignFile(F, FileName);
    if FileExists(FileName) then
    begin
      EmptyIndex:= GetEmptyRec;
      FileMode:= 2;

      Reset(F);
      if EmptyIndex <> -1 then
        Seek(F, EmptyIndex)
      else
        Seek(F, System.FileSize(F));
    end
    else
      Rewrite(F);

    Rec.Title:= Title;
    Rec.DatabaseName:= DatabaseName;
    Rec.UserName:= UserName;
    if SavePassword then
      Rec.Password:= Password
    else
      Rec.Password:= '';
    Rec.Charset:= Charset;
    Rec.Role:= Role;
    Rec.SavePassword:= SavePassword;
    Rec.Deleted:= False;
    Rec.LastOpened:= Now;

    Write(F, Rec);
    CloseFile(F);
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage('Error: ' + e.Message);
    end;
  end;
end;

function TfmReg.EditRegisteration(Index: Integer; Title, DatabaseName, UserName, Password, Charset, Role: string;
   SavePassword: Boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
begin
  try
    FileName:= fmMain.getConfigurationDirectory + 'turbobird.reg';

    AssignFile(F, FileName);
    FileMode:= 2;
    Reset(F);
    Seek(F, Index);

    Rec.Title:= Title;
    Rec.DatabaseName:= DatabaseName;
    Rec.UserName:= UserName;
    if SavePassword then
      Rec.Password:= Password
    else
      Rec.Password:= '';
    Rec.Charset:= Charset;
    Rec.Role:= Role;
    Rec.SavePassword:= SavePassword;
    Rec.Deleted:= False;

    Write(F, Rec);
    CloseFile(F);
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage('Error: ' + e.Message);
    end;
  end;
end;

function TfmReg.TestConnection(DatabaseName, UserName, Password, Charset: string): Boolean;
begin
  try
    IBConnection1.Close;
    IBConnection1.DatabaseName:= DatabaseName;
    IBConnection1.UserName:= UserName;
    IBConnection1.Password:= Password;
    IBConnection1.CharSet:= Charset;
    IBConnection1.Open;
    IBConnection1.Close;
    Result:= True;
  except
    on d: EIBDatabaseError do
    begin
      Result:= False;
      ShowMessage('Unable to connect: '+ d.Message + LineEnding +
        'Details: GDS error code: '+inttostr(d.GDSErrorCode));
    end;
    on E: Exception do
    begin
      Result:= False;
      ShowMessage('Unable to connect: ' + e.Message);
    end;
  end;
end;

function TfmReg.GetEmptyRec: Integer;
var
  FileName: string;
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
begin
  Result:= -1;

  FileName:= fmMain.getConfigurationDirectory + 'turbobird.reg';

  AssignFile(F, FileName);
  if FileExists(FileName) then
  begin
    Reset(F);
    while not Eof(F) do
    begin
      Read(F, Rec);
      if Rec.Deleted then
      begin
        Result:= FilePos(F) - 1;
        Break;
      end;
    end;
    Closefile(F);
  end;
end;

function TfmReg.SaveRegistrations: Boolean;
var
  F: file of TRegisteredDatabase;
  FileName: string;
  i: Integer;
begin
  try
    Sort;
    FileName:= fmMain.getConfigurationDirectory + 'turbobird.reg';

    AssignFile(F, FileName);
    FileMode:= 2;
    Rewrite(F);

    for i:= 0 to High(fmMain.RegisteredDatabases) do
      Write(F, fmMain.RegisteredDatabases[i].OrigRegRec);
    CloseFile(F);
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
    end;
  end;
end;

procedure TfmReg.Sort;
var
  TempRec: TRegisteredDatabase;
  Done: Boolean;
  i: Integer;
  TempIndex: Integer;
begin
  repeat
    Done:= True;
    for i:= 0 to High(fmMain.RegisteredDatabases) - 1 do
    with fmMain do
      if RegisteredDatabases[i].RegRec.LastOpened < RegisteredDatabases[i + 1].RegRec.LastOpened then
      begin
        Done:= False;
        TempRec:= RegisteredDatabases[i].OrigRegRec;
        RegisteredDatabases[i].OrigRegRec:= RegisteredDatabases[i + 1].OrigRegRec;
        RegisteredDatabases[i].RegRec:= RegisteredDatabases[i + 1].RegRec;
        RegisteredDatabases[i + 1].OrigRegRec:= TempRec;
        RegisteredDatabases[i + 1].RegRec:= TempRec;

        TempIndex:= RegisteredDatabases[i].Index;
        RegisteredDatabases[i].Index:= RegisteredDatabases[i + 1].Index;
        RegisteredDatabases[i + 1].Index:= TempIndex;
      end;
  until Done;
end;

initialization
  {$I reg.lrs}

end.

