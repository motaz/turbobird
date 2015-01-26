unit CopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, sqldb, turbocommon, IniFiles;

type

  { TfmCopyTable }

  TfmCopyTable = class(TForm)
    bbCopy: TBitBtn;
    bbClose: TBitBtn;
    cbSourceTable: TComboBox;
    cbDestDatabase: TComboBox;
    cbDestTable: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    laDatabase: TLabel;
    laDatabase1: TLabel;
    laSourceDatabase: TLabel;
    SynSQLSyn1: TSynSQLSyn;
    syScript: TSynEdit;
    procedure bbCopyClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure cbDestDatabaseChange(Sender: TObject);
    procedure cbSourceTableChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSourceIndex: Integer;
    { private declarations }
  public
    { public declarations }
    procedure Init(SourceIndex: Integer; ATableName: string);
  end; 

var
  fmCopyTable: TfmCopyTable;

implementation

{ TfmCopyTable }

uses main, SysTables, EnterPass, Reg;

procedure TfmCopyTable.cbDestDatabaseChange(Sender: TObject);
var
  Count: Integer;
begin
  laDatabase.Caption:= fmMain.RegisteredDatabases[cbDestDatabase.ItemIndex].RegRec.DatabaseName;
  cbDestDatabase.SetFocus;
  dmSysTables.Init(cbDestDatabase.ItemIndex);
  cbDestTable.Items.CommaText:= dmSysTables.GetDBObjectNames(cbDestDatabase.ItemIndex, otTables, count);
  if cbDestTable.Items.IndexOf(cbSourceTable.Text) <> -1 then
    cbDestTable.Text:= cbSourceTable.Text;
end;

procedure TfmCopyTable.cbSourceTableChange(Sender: TObject);
var
  List: TStringList;
  Line: string;
begin
  List:= TStringList.Create;
  try
    fmMain.GetFields(FSourceIndex, cbSourceTable.Text, List);
    Line:= List.CommaText;
  finally
    List.Free;
  end;
  syScript.Lines.Text:= 'select ' + Line;
  syScript.Lines.Add(' from ' + cbSourceTable.Text);
end;

procedure TfmCopyTable.FormCreate(Sender: TObject);
var
   configFile: TIniFile;
   configFilePath: String;
begin
     // Set the editor font from config.ini
    configFilePath:= ConcatPaths([ExtractFilePath(Application.ExeName), 'config.ini']);
    configFile:= TIniFile.Create(configFilePath);
    syScript.Font.Name:=configFile.ReadString('Editor Font', 'font_name', 'Monospace');
    syScript.Font.Size:=configFile.ReadInteger('Editor Font', 'font_size', 11);
    configFile.Free;
end;

procedure TfmCopyTable.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCopyTable.bbCopyClick(Sender: TObject);
var
  i: Integer;
  Statement: string;
  Values: string;
  SQLTarget: TSQLQuery;
  Num: Integer;
begin
  Statement:= 'insert into ' + cbDestTable.Text + ' (';
  dmSysTables.sqQuery.Close;
  dmSysTables.Init(FSourceIndex);
  dmSysTables.sqQuery.SQL.Text:= syScript.Lines.Text;
  dmSysTables.sqQuery.Open;
  Values:= '';

  // Get field names
  with dmSysTables.sqQuery do
  for i:= 0 to Fields.Count - 1 do
  begin
    Statement:= Statement + Fields[i].FieldName + ',';
    Values:= Values + ':' + Fields[i].FieldName + ',';
    Next;
  end;
  Delete(Statement, Length(Statement), 1);
  Delete(Values, Length(Values), 1);
  Statement:= Statement + ') Values (' + Values + ')';

  // Enter password if it is not saved
  with fmMain.RegisteredDatabases[cbDestDatabase.ItemIndex] do
  begin
    if IBConnection.Password = '' then
    begin
      if fmEnterPass.ShowModal = mrOk then
      begin
        if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
          RegRec.Charset) then
          with fmMain do
          begin
            RegisteredDatabases[cbDestDatabase.ItemIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
            RegisteredDatabases[cbDestDatabase.ItemIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
            RegisteredDatabases[cbDestDatabase.ItemIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
          end
          else
          begin
            Exit;
          end;
      end
    end;

    SQLTarget:= TSQLQuery.Create(nil);
    try
      SQLTarget.DataBase:= IBConnection;
      SQLTarget.Transaction:= SQLTrans;
      SQLTarget.SQL.Text:= Statement;

      // Start copy
      try
        dmSysTables.sqQuery.First;
        Num:= 0;
        with dmSysTables.sqQuery do
        while not EOF do
        begin
          for i:= 0 to Fields.Count - 1 do
            SQLTarget.Params.ParamByName(Fields[i].FieldName).Value:= Fields[i].Value;
          SQLTarget.ExecSQL;
          Inc(Num);
          Next;
        end;
        SQLTrans.Commit;
        ShowMessage(IntToStr(Num) + ' record(s) has been copied' + LineEnding + 'Don''t forget to set the Generator to the new value, ' +
          'if it exists');
        dmSysTables.sqQuery.Close;
        Close;
      except
        on E: Exception do
        begin
          MessageDlg('Error while copy: ' + e.Message, mtError, [mbOk], 0);
          SQLTrans.Rollback;
        end;
      end;
    finally
      SQLTarget.Free;
    end;
  end;

  dmSysTables.sqQuery.Close;
end;

procedure TfmCopyTable.Init(SourceIndex: Integer; ATableName: string);
var
  i: Integer;
  Count: Integer;
begin
  dmSysTables.sqQuery.Close; //todo: (low priority) is closing sqquery really necessary?
  FSourceIndex:= SourceIndex;
  laSourceDatabase.Caption:= fmMain.RegisteredDatabases[SourceIndex].RegRec.Title;
  cbDestDatabase.Clear;

  // Display databases in destination combo box
  for i:= 0 to High(fmMain.RegisteredDatabases) do
    cbDestDatabase.Items.Add(fmMain.RegisteredDatabases[i].RegRec.Title);
  laDatabase.Caption:= '';

  cbSourceTable.Items.CommaText:= dmSysTables.GetDBObjectNames(SourceIndex, otTables, count);
  cbSourceTable.Text:= ATableName;
  SynSQLSyn1.TableNames.Text:= cbSourceTable.Text;
  cbSourceTableChange(nil);
end;

initialization
  {$I copytable.lrs}

end.

