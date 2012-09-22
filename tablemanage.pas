unit TableManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Grids, Buttons, StdCtrls, CheckLst;

type

  { TfmTableManage }

  TfmTableManage = class(TForm)
    bbClose: TBitBtn;
    bbCreateIndex: TBitBtn;
    bbDrop: TBitBtn;
    bbDropConstraint: TBitBtn;
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    bbNewConstraint: TBitBtn;
    bbRefresh: TBitBtn;
    bbRefreshConstraint: TBitBtn;
    bbRefreshIndices: TBitBtn;
    bbRefreshTriggers: TBitBtn;
    bbNewTrigger: TBitBtn;
    bbEditTrigger: TBitBtn;
    bbDropTrigger: TBitBtn;
    bbRefreshPermissions: TBitBtn;
    bbAddUser: TBitBtn;
    edEditPermission: TBitBtn;
    cbIndexType: TComboBox;
    cbSortType: TComboBox;
    clbFields: TCheckListBox;
    cxUnique: TCheckBox;
    edDrop: TBitBtn;
    edIndexName: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    sgTriggers: TStringGrid;
    sgPermissions: TStringGrid;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    sgFields: TStringGrid;
    sgIndices: TStringGrid;
    sgConstraints: TStringGrid;
    tsPermissions: TTabSheet;
    tsTriggers: TTabSheet;
    tsIndices: TTabSheet;
    tsConstraints: TTabSheet;
    tsFields: TTabSheet;
    procedure bbAddUserClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbCreateIndexClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbDropConstraintClick(Sender: TObject);
    procedure bbDropTriggerClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure bbEditTriggerClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbNewConstraintClick(Sender: TObject);
    procedure bbNewTriggerClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbRefreshConstraintClick(Sender: TObject);
    procedure bbRefreshIndicesClick(Sender: TObject);
    procedure bbRefreshPermissionsClick(Sender: TObject);
    procedure bbRefreshTriggersClick(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
    procedure edDropClick(Sender: TObject);
    procedure edEditPermissionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    fdbIndex: Integer;
    fTableName: string;
    ibConnection: TIBConnection;
    sqlTrans: TSQLTransaction;
  public
    PKeyName, ConstraintName: string;
    procedure Init(dbIndex: Integer; TableName: string);
    procedure FillConstraints(dbIndex: Integer);
    procedure ViewTriggers;
    procedure FillPermissions;
  end;

var
  fmTableManage: TfmTableManage;

implementation

{ TfmTableManage }

uses NewEditField, Main, QueryWindow, SysTables, NewConstraint, PermissionManage;

procedure TfmTableManage.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;


procedure TfmTableManage.bbEditClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
  FieldName, FieldType, DefaultValue, Description: string;
  FieldOrder, FieldSize: Integer;
  AllowNull: Boolean;
begin
  fmNewEditField:= TfmNewEditField.Create(nil);
  with sgFields, fmNewEditField do
  begin
    FieldName:= Cells[1, Row];
    FieldType:= Cells[2, Row];
    FieldSize:= StrtoInt(Cells[3, Row]);
    AllowNull:= Boolean(StrToInt(Cells[4, Row]));
    DefaultValue:= Cells[5, Row];
    Description:= Cells[6, Row];
    FieldOrder:= Row;
    fmNewEditField.Init(fdbIndex, fTableName, foEdit, FieldName, FieldType, DefaultValue, Description, FieldSize,
      FieldOrder, AllowNull, bbRefresh);

    Caption:= 'Edit field: ' + OldFieldName;

    fmNewEditField.Show;
  end;
end;

procedure TfmTableManage.bbEditTriggerClick(Sender: TObject);
var
  ATriggerName: string;
  List: TStringList;
begin
  if sgTriggers.RowCount > 1 then
  begin
    List:= TStringList.Create;
    ATriggerName:= sgTriggers.Cells[0, sgTriggers.Row];
    dmSysTables.ScriptTrigger(fdbIndex, ATriggerName, List);
    fmMain.ShowCompleteQueryWindow(fdbIndex, 'Edit Trigger ', List.Text, bbRefreshTriggers.OnClick);
    List.Free;
  end;

end;

procedure TfmTableManage.bbDropClick(Sender: TObject);
begin
  with sgIndices do
  if RowCount > 1 then
    if MessageDlg('Are you sure you want to drop index: ' + Cells[0, Row], mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
      begin
        if Cells[0, Row] = PKeyName then // Delete primary key
          fmMain.ShowCompleteQueryWindow(fdbIndex,  'Drop Primary Key on Table: ' + fTableName,
            'alter table ' + fTableName + ' DROP constraint ' + ConstraintName, bbRefreshIndices.OnClick)
        else // Delete secondary key
          fmMain.ShowCompleteQueryWindow(fdbIndex, 'Drop Secondary Index on table: ' + fTableName,
            'DROP INDEX ' + Cells[0, Row], bbRefreshIndices.OnClick);

      end;
end;

procedure TfmTableManage.bbDropConstraintClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstName: string;
begin
  if sgConstraints.Row > 0 then
  begin
    ConstName:= sgConstraints.Cells[0, sgConstraints.Row];
    if MessageDlg('Are you sure you want to drop ' + ConstName, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      QWindow:= fmMain.ShowQueryWindow(fdbIndex, 'drop constrain: ' + ConstName);
      QWindow.meQuery.Lines.Text:= 'ALTER TABLE ' + fTableName + ' DROP CONSTRAINT ' + ConstName;
      fmMain.Show;
      QWindow.OnCommit:= bbRefreshConstraint.OnClick;
    end;
  end;
end;

procedure TfmTableManage.bbDropTriggerClick(Sender: TObject);
var
  ATriggerName: string;
begin
  if (sgTriggers.RowCount > 1) and
    (MessageDlg('Are You sure to drop this trigger', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ATriggerName:= sgTriggers.Cells[0, sgTriggers.Row];
      fmMain.ShowCompleteQueryWindow(fdbIndex, 'Drop Trigger : ' + ATriggerName,
        'drop trigger ' + ATriggerName, bbRefreshTriggers.OnClick);

  end;
end;

procedure TfmTableManage.bbCreateIndexClick(Sender: TObject);
var
  Fields: string;
  i: Integer;
  QWindow: TfmQueryWindow;
  FirstLine: string;
begin
  Fields:= '';
  for i:= 0 to clbFields.Count - 1 do
    if clbFields.Checked[i] then
      Fields:= Fields + Trim(clbFields.Items[i]) + ',';
  Delete(Fields, Length(Fields), 1);

  if Trim(Fields) = '' then
    MessageDlg('Error', 'Your should select one field at least', mtError, [mbOk], 0)
  else
  if Trim(edIndexName.Text) = '' then
    MessageDlg('Error', 'Your should enter new index name', mtError, [mbOk], 0)
  else
  begin
    QWindow:= fmMain.ShowQueryWindow(fdbIndex, 'Create new index');
    QWindow.meQuery.Lines.Clear;

    if cbIndexType.ItemIndex = 0 then // primary key
    begin
      QWindow.meQuery.Lines.Text:= 'alter table ' + fTableName + #13#10 +
      'add constraint ' + edIndexName.Text + #13#10 +
      'primary key (' + Fields + ')';
    end
    else    // Secondary index
    begin
      FirstLine:= 'create ';
      if cxUnique.Checked then
        FirstLine:= FirstLine + 'unique ';
      FirstLine:= FirstLine + cbSortType.Text + ' index ' + edIndexName.Text;


      QWindow.meQuery.Lines.Text:= FirstLine + #13+#10 + 'on ' + fTableName + #13#10 + Fields;
    end;
    QWindow.OnCommit:= bbRefreshIndices.OnClick;

    QWindow.Show;
  end;

end;

procedure TfmTableManage.bbAddUserClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
begin
  fmPermissions:= TfmPermissionManage.Create(nil);
  fmPermissions.Init(fdbIndex, fTableName, '', 1, bbRefreshPermissions.OnClick);
  fmPermissions.Show;
end;

procedure TfmTableManage.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmTableManage.bbNewClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
begin
  fmNewEditField:= TfmNewEditField.Create(nil);
  with fmNewEditField do
  begin
    Init(fdbIndex, fTableName, foNew, '', '', '', '', 0, 0, True, bbRefresh);
    Caption:= 'Add new field on Table: ' + fTableName;
    Show;
  end;
end;

procedure TfmTableManage.bbNewConstraintClick(Sender: TObject);
var
  Count: Integer;
  FieldsList: TStringList;
begin
  // Get current fields
  FieldsList:= TStringList.Create;
  fmMain.GetFields(fdbIndex, fTableName, FieldsList);
  fmNewConstraint.clxOnFields.Clear;
  fmNewConstraint.clxOnFields.Items.AddStrings(FieldsList);
  FieldsList.Free;
  fmMain.SQLQuery1.Close;
  fmNewConstraint.edNewName.Text:= 'FK_' + fTableName + '_' + IntToStr(sgConstraints.RowCount);

  // Foriegn tables
  fmNewConstraint.cbTables.Items.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);
  fmNewConstraint.DatabaseIndex:= fdbIndex;

  fmNewConstraint.laTable.Caption:= fTableName;
  fmNewConstraint.Caption:= 'New Constraint for : ' + fTableName;
  if fmNewConstraint.ShowModal = mrOK then
  begin
    fmNewConstraint.QWindow.OnCommit:= bbRefreshConstraint.OnClick;
  end;
end;

procedure TfmTableManage.bbNewTriggerClick(Sender: TObject);
begin
  fmMain.CreateNewTrigger(fdbIndex, fTableName, bbRefreshTriggers.OnClick);
end;

procedure TfmTableManage.bbRefreshClick(Sender: TObject);
begin
  fmMain.ViewTableFields(fTableName, fdbIndex, sgFields);
  Parent.Show;
  Show;
  //fmMain.PageControl1.ActivePage:= Self.Parent as TTabSheet;
end;

procedure TfmTableManage.bbRefreshConstraintClick(Sender: TObject);
begin
  SQLTrans.Commit;
  fmMain.FillAndShowConstraintsForm(Self, fTableName, fdbIndex);
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshIndicesClick(Sender: TObject);
begin
  fmMain.ShowIndicesManagement(Self, fdbIndex, fTableName);
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshPermissionsClick(Sender: TObject);
begin
  FillPermissions;
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshTriggersClick(Sender: TObject);
begin
  SQLTrans.Commit;
  ViewTriggers;
  Parent.Show;
  Show;
end;

procedure TfmTableManage.cbIndexTypeChange(Sender: TObject);
begin
  case cbIndexType.ItemIndex of
    0: edIndexName.Text:= 'PK_' + fTableName + '_1';
    1: edIndexName.Text:= 'IX_' + fTableName + '_' + IntToStr(sgIndices.RowCount);
  end;
end;

procedure TfmTableManage.edDropClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete the field: ' + sgFields.Cells[1, sgFields.Row] +
    ' with its data', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      fmMain.ShowCompleteQueryWindow(fdbIndex, 'Drop field', 'ALTER TABLE ' + fTableName + ' DROP ' +
        sgFields.Cells[1, sgFields.Row], @bbRefreshClick);
    end;
end;

procedure TfmTableManage.edEditPermissionClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  UserType: Integer;
begin
  if sgPermissions.Row > 0 then
  begin
    if sgPermissions.Cells[1, sgPermissions.Row] = 'User' then
      UserType:= 1
    else
      UserType:= 2;
    fmPermissions:= TfmPermissionManage.Create(nil);
    fmPermissions.Init(fdbIndex, fTableName, sgPermissions.Cells[0, sgPermissions.Row], UserType, @bbRefreshPermissionsClick);
    fmPermissions.Show;
  end
  else
    ShowMessage('There is no selected user/role');
end;


procedure TfmTableManage.Init(dbIndex: Integer; TableName: string);
begin
  try
    fdbIndex:= dbIndex;
    fTableName:= TableName;
    with fmMain do
    begin
      ibConnection:= RegisteredDatabases[dbIndex].IBConnection;
      ibConnection.Close;
      sqlTrans:= RegisteredDatabases[dbIndex].SQLTrans;
      ibConnection.Transaction:= sqlTrans;
    end;
    SQLQuery1.Close;
    SQLQuery1.DataBase:= ibConnection;
    SQLQuery2.Close;
    SQLQuery2.DataBase:= ibConnection;

  except
    on e: exception do
    begin
      MessageDlg('Error while initalizing Table Management: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TfmTableManage.FillConstraints(dbIndex: Integer);
begin
  SQLQuery1.First;
  fdbIndex:= dbIndex;
  sgConstraints.RowCount:= 1;
  with sgConstraints do
  while not SQLQuery1.EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= SQLQuery1.Fields[0].AsString;
    Cells[1, RowCount - 1]:= SQLQuery1.Fields[1].AsString;
    Cells[2, RowCount - 1]:= SQLQuery1.Fields[3].AsString;
    Cells[3, RowCount - 1]:= SQLQuery1.Fields[4].AsString;
    Cells[4, RowCount - 1]:= dmSysTables.GetConstraintForiegnKeyFields(SQLQuery1.Fields[5].AsString, SQLQuery2);

    Cells[5, RowCount - 1]:= SQLQuery1.Fields[6].AsString;
    Cells[6, RowCount - 1]:= SQLQuery1.Fields[7].AsString;
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.ViewTriggers;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'SELECT RDB$Trigger_Name, RDB$Trigger_Inactive FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 ' +
    'and RDB$Relation_Name = ''' + fTableName + '''';
  SQLQuery1.Open;
  sgTriggers.RowCount:= 1;
  with sgTriggers, SQLQuery1 do
  while not EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= SQLQuery1.Fields[0].AsString;
    if SQLQuery1.Fields[1].AsString = '1' then
      Cells[1, RowCount - 1]:= '0'
    else
      Cells[1, RowCount - 1]:= '1';
    Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.FillPermissions;
var
  UsersList: TStringList;
  i: Integer;
  UserName: string;
  ObjType: Integer;
  Permissions: string;
begin
  UsersList:= TStringList.Create;
  UsersList.CommaText:= dmSysTables.GetDBUsers(fdbIndex, fTableName);
  sgPermissions.RowCount:= UsersList.Count + 1;
  for i:= 0 to UsersList.Count - 1 do
  begin
    UserName:= UsersList[i];
    if Pos('<R>', UserName) = 1 then
      begin
        sgPermissions.Cells[1, i + 1]:= 'Role';
        Delete(UserName, 1, 3);
      end
    else
      sgPermissions.Cells[1, i + 1]:= 'User';

    sgPermissions.Cells[0, i + 1]:= UserName;

    // Permissions
    Permissions:= dmSysTables.GetObjectUserPermission(fdbIndex, fTableName, UserName, ObjType);

    if Pos('S', Permissions) > 0 then
      sgPermissions.Cells[2, i + 1]:= '1'
    else
      sgPermissions.Cells[2, i + 1]:= '0';

    if Pos('I', Permissions) > 0 then
      sgPermissions.Cells[3, i + 1]:= '1'
    else
      sgPermissions.Cells[3, i + 1]:= '0';

    if Pos('U', Permissions) > 0 then
      sgPermissions.Cells[4, i + 1]:= '1'
    else
      sgPermissions.Cells[4, i + 1]:= '0';

    if Pos('D', Permissions) > 0 then
      sgPermissions.Cells[5, i + 1]:= '1'
    else
      sgPermissions.Cells[5, i + 1]:= '0';

    if Pos('R', Permissions) > 0 then
      sgPermissions.Cells[6, i + 1]:= '1'
    else
      sgPermissions.Cells[6, i + 1]:= '0';

    if Pos('SG', Permissions) > 0 then
      sgPermissions.Cells[7, i + 1]:= '1'
    else
      sgPermissions.Cells[7, i + 1]:= '0';

    if Pos('IG', Permissions) > 0 then
      sgPermissions.Cells[8, i + 1]:= '1'
    else
      sgPermissions.Cells[8, i + 1]:= '0';

    if Pos('UG', Permissions) > 0 then
      sgPermissions.Cells[9, i + 1]:= '1'
    else
      sgPermissions.Cells[9, i + 1]:= '0';

    if Pos('DG', Permissions) > 0 then
      sgPermissions.Cells[10, i + 1]:= '1'
    else
      sgPermissions.Cells[10, i + 1]:= '0';

    if Pos('RG', Permissions) > 0 then
      sgPermissions.Cells[11, i + 1]:= '1'
    else
      sgPermissions.Cells[11, i + 1]:= '0';
  end;
  UsersList.Free;
end;

initialization
  {$I tablemanage.lrs}

end.

