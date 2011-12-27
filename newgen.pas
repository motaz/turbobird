unit NewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfmNewGen }

  TfmNewGen = class(TForm)
    bbCreateGen: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    bbCreateTrigger: TBitBtn;
    cbTables: TComboBox;
    cbFields: TComboBox;
    cxTrigger: TCheckBox;
    edGenName: TEdit;
    gbTrigger: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    meSQL: TMemo;
    SQLQuery1: TSQLQuery;
    procedure bbCreateGenClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure bbCreateTriggerClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure cxTriggerChange(Sender: TObject);
  private
    { private declarations }
    ibConnection: TIBConnection;
    SQLTrans: TSQLTransaction;
  public
    DatabaseIndex: Integer;
    procedure Init(dbIndex: Integer);
    { public declarations }
  end; 

var
  fmNewGen: TfmNewGen;

implementation

{ TfmNewGen }

uses main;

procedure TfmNewGen.bbCreateGenClick(Sender: TObject);
begin
  if Trim(edGenName.Text) <> '' then
  begin
    SQLQuery1.SQL.Text:= 'create generator ' + edGenName.Text;
    SQLQuery1.ExecSQL;
    SQLTrans.Commit;
    fmMain.AddToSQLHistory(fmMain.RegisteredDatabases[DatabaseIndex].RegRec.Title, 'DDL', SQLQuery1.SQL.Text);
    MessageDlg('Generator ' + edGenName.Text + ' has been created successfully', mtInformation, [mbOK], 0);
    gbTrigger.Enabled:= True;
    cxTrigger.Enabled:= True;
  end
  else
    MessageDlg('You should write Generator name', mtError, [mbOK], 0);
end;

procedure TfmNewGen.BitBtn2Click(Sender: TObject);
begin
  if (cbTables.ItemIndex = -1) or (cbFields.ItemIndex = -1) then
    MessageDlg('You should select a table and a field', mtError, [mbOk], 0)
  else
  if Trim(edGenName.Text) = '' then
    MessageDlg('You should enter generator name', mtError, [mbOK], 0)
  else
  begin
    meSQL.Clear;
    meSQL.Lines.Add('CREATE TRIGGER ' + Trim(edGenName.Text) + ' FOR ' + cbTables.Text);
    meSQL.Lines.Add('ACTIVE BEFORE INSERT POSITION 0 ');
    meSQL.Lines.Add('AS BEGIN ');
    meSQL.Lines.Add('IF (NEW.' + cbFields.Text + ' IS NULL OR NEW.' + cbFields.Text + ' = 0) THEN ');
    meSQL.Lines.Add('  NEW.' + cbFields.Text + ' = GEN_ID(' + edGenName.Text + ', 1);');
    meSQL.Lines.Add('END');
    bbCreateTrigger.Enabled:= True;
  end;
end;

procedure TfmNewGen.bbCreateTriggerClick(Sender: TObject);
begin
  SQLQuery1.SQL.Text:= meSQL.Lines.Text;
  SQLQuery1.ExecSQL;
  SQLTrans.Commit;
  fmMain.AddToSQLHistory(fmMain.RegisteredDatabases[DatabaseIndex].RegRec.Title, 'DDL', SQLQuery1.SQL.Text);
  MessageDlg('Auto Increment Trigger has been created successfully for the table ' + cbTables.Text,
    mtInformation, [mbOK], 0);
  ModalResult:= mrOK;
end;

procedure TfmNewGen.cbTablesChange(Sender: TObject);
var
  FType: string;
begin
  if cbTables.ItemIndex <> -1 then
  begin
    fmMain.GetFields(DatabaseIndex, cbTables.Text, nil);
    cbFields.Clear;
    while not fmMain.SQLQuery1.EOF do
    begin
      FType:= Trim(fmMain.SQLQuery1.FieldByName('Field_Type_Str').AsString);
      if (FType = 'INTEGER') or (FType = 'INT64') or (FType = 'SMALLINT') then
        cbFields.Items.Add(Trim(fmMain.SQLQuery1.FieldByName('Field_Name').AsString));
      fmMain.SQLQuery1.Next;
    end;
    fmMain.SQLQuery1.Close;

  end;
end;

procedure TfmNewGen.cxTriggerChange(Sender: TObject);
begin
  gbTrigger.Enabled:= cxTrigger.Checked;
end;

procedure TfmNewGen.Init(dbIndex: Integer);
begin
  DatabaseIndex:= dbIndex;
  ibConnection:= fmMain.RegisteredDatabases[dbIndex].IBConnection;
  SQLTrans:= fmMain.RegisteredDatabases[dbIndex].SQLTrans;;
  SQLQuery1.DataBase:= ibConnection;
  cxTrigger.Checked:= False;
  bbCreateTrigger.Enabled:= False;
  meSQL.Clear;
end;

initialization
  {$I newgen.lrs}

end.

