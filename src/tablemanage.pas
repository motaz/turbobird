unit TableManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Grids, Buttons, StdCtrls, CheckLst, LCLType, turbocommon, types;

type

  { TfmTableManage }

  TfmTableManage = class(TForm)
      bbClose: TSpeedButton;
    bbCreateIndex: TBitBtn;
    bbDrop: TBitBtn;
    bbDropConstraint: TBitBtn;
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    bbNewConstraint: TBitBtn;
    bbRefresh: TBitBtn;
    bbRefreshConstraint: TBitBtn;
    bbRefreshReferences: TBitBtn;
    bbRefreshIndices: TBitBtn;
    bbRefreshTriggers: TBitBtn;
    bbNewTrigger: TBitBtn;
    bbEditTrigger: TBitBtn;
    bbDropTrigger: TBitBtn;
    bbRefreshPermissions: TBitBtn;
    bbAddUser: TBitBtn;
    cbIndexType: TComboBox;
    cbSortType: TComboBox;
    clbFields: TCheckListBox;
    cxUnique: TCheckBox;
    bbEditPermission: TBitBtn;
    edDrop: TBitBtn;
    edIndexName: TEdit;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl1: TPageControl;
    sgReferences: TStringGrid;
    sgTriggers: TStringGrid;
    sgPermissions: TStringGrid;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    sgFields: TStringGrid;
    sgIndices: TStringGrid;
    sgConstraints: TStringGrid;
    tsReferences: TTabSheet;
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
    procedure bbRefreshReferencesClick(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
    procedure edDropClick(Sender: TObject);
    procedure bbEditPermissionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgFieldsDblClick(Sender: TObject);
    procedure sgPermissionsDblClick(Sender: TObject);
    procedure sgTriggersDblClick(Sender: TObject);
    procedure tsFieldsContextPopup(Sender: TObject; MousePos: TPoint;
        var Handled: Boolean);
  private
    FDBIndex: Integer;
    FTableName: string;
    FIBConnection: TIBConnection;
    FSQLTrans: TSQLTransaction;
  public
    PKeyName, ConstraintName: string;
    procedure Init(dbIndex: Integer; TableName: string);
    // Fill grid with constraint info
    { Todo: fillconstraints relies on a query being set correctly elsewhere; get
    rid of that - see e.g. FillPermissions }
    procedure FillConstraints(dbIndex: Integer);
    // Get info on permissions and fill grid with it
    procedure FillPermissions;
    // Get info on triggers and fill grid with it
    procedure FillTriggers;
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

procedure TfmTableManage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmTableManage.sgFieldsDblClick(Sender: TObject);
begin
  // Double clicking on a row lets you edit the field
  bbEditClick(Sender);
end;

procedure TfmTableManage.sgPermissionsDblClick(Sender: TObject);
begin
  // Double clicking allows user to edit permissions
  bbEditPermissionClick(Sender);
end;

procedure TfmTableManage.sgTriggersDblClick(Sender: TObject);
begin
  // Double clicking allows user to edit trigger
  bbEditTriggerClick(Sender);
end;

procedure TfmTableManage.tsFieldsContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin

end;


procedure TfmTableManage.bbEditClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
  FieldName, FieldType,
  DefaultValue, Characterset, Collation, Description: string;
  FieldOrder, FieldSize, FieldScale: Integer;
  AllowNull: Boolean;
begin
  fmNewEditField:= TfmNewEditField.Create(nil);
  {todo: getting info from gui elements that got it from a function that got it
   from a query is awful. Rework to use e.g. the function}
  with sgFields, fmNewEditField do
  begin
    Characterset:= ''; //todo: support character set in field editing: add column to grid
    Collation:= ''; //todo: support collation in field editing: add column to grid
    FieldName:= Cells[1, Row];
    FieldType:= Cells[2, Row];
    FieldSize:= StrtoInt(Cells[3, Row]);
    FieldScale:= 0; //todo support fieldscale in field editing!!!
    AllowNull:= Boolean(StrToInt(Cells[4, Row]));
    DefaultValue:= Cells[5, Row];
    Description:= Cells[6, Row];
    FieldOrder:= Row;
    fmNewEditField.Init(FDBIndex, FTableName, foEdit,
      FieldName, FieldType, Characterset, Collation,
      DefaultValue, Description,
      FieldSize, FieldScale, FieldOrder, AllowNull, bbRefresh);

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
    try
      ATriggerName:= sgTriggers.Cells[0, sgTriggers.Row];
      dmSysTables.ScriptTrigger(FDBIndex, ATriggerName, List);
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Trigger ', List.Text, bbRefreshTriggers.OnClick);
    finally
      List.Free;
    end;
  end;

end;

procedure TfmTableManage.bbDropClick(Sender: TObject);
begin
  with sgIndices do
  begin
    if (RowCount > 1) and
      (MessageDlg('Are you sure you want to drop index: ' + Cells[0, Row], mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
    begin
      if Cells[0, Row] = PKeyName then // Delete primary key
        fmMain.ShowCompleteQueryWindow(FDBIndex,  'Drop Primary Key on Table: ' + FTableName,
          'alter table ' + FTableName + ' DROP CONSTRAINT ' + ConstraintName, bbRefreshIndices.OnClick)
      else // Delete normal index
        fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Index on table: ' + FTableName,
          'DROP INDEX ' + Cells[0, Row], bbRefreshIndices.OnClick);
    end;
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
      QWindow:= fmMain.ShowQueryWindow(FDBIndex, 'drop constraint: ' + ConstName);
      QWindow.meQuery.Lines.Text:= 'ALTER TABLE ' + FTableName + ' DROP CONSTRAINT ' + ConstName;
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
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Trigger : ' + ATriggerName,
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
    MessageDlg('Error', 'You should select at least one field', mtError, [mbOk], 0)
  else
  if Trim(edIndexName.Text) = '' then
    MessageDlg('Error', 'You should enter the new index name', mtError, [mbOk], 0)
  else
  begin
    QWindow:= fmMain.ShowQueryWindow(FDBIndex, 'Create new index');
    QWindow.meQuery.Lines.Clear;

    if cbIndexType.ItemIndex = 0 then // primary key
    begin
      QWindow.meQuery.Lines.Text:= 'alter table ' + FTableName + LineEnding +
      'add constraint ' + edIndexName.Text + LineEnding +
      'primary key (' + Fields + ')';
    end
    else    // Normal index
    begin
      FirstLine:= 'create ';
      if cxUnique.Checked then
        FirstLine:= FirstLine + 'unique ';
      FirstLine:= FirstLine + cbSortType.Text + ' index ' + edIndexName.Text;
      QWindow.meQuery.Lines.Text:= FirstLine + LineEnding + 'on ' + FTableName + LineEnding + Fields;
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
  fmPermissions.Init(FDBIndex, FTableName, '', 1, bbRefreshPermissions.OnClick);
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
    Init(FDBIndex, FTableName, foNew,
      '', '', '', '', '', '',
      0, 0, 0, True, bbRefresh);
    Caption:= 'Add new field to Table: ' + FTableName;
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
  try
    fmMain.GetFields(FDBIndex, FTableName, FieldsList);
    fmNewConstraint.clxOnFields.Clear;
    fmNewConstraint.clxOnFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
  end;
  fmMain.SQLQuery1.Close;
  fmNewConstraint.edNewName.Text:= 'FK_' + FTableName + '_' + IntToStr(sgConstraints.RowCount);

  // Foreign tables
  fmNewConstraint.cbTables.Items.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, otTables, Count);
  fmNewConstraint.DatabaseIndex:= FDBIndex;

  fmNewConstraint.laTable.Caption:= FTableName;
  fmNewConstraint.Caption:= 'New Constraint for : ' + FTableName;
  if fmNewConstraint.ShowModal = mrOK then
  begin
    fmNewConstraint.QWindow.OnCommit:= bbRefreshConstraint.OnClick;
  end;
end;

procedure TfmTableManage.bbNewTriggerClick(Sender: TObject);
begin
  fmMain.CreateNewTrigger(FDBIndex, FTableName, bbRefreshTriggers.OnClick);
end;

procedure TfmTableManage.bbRefreshClick(Sender: TObject);
begin
  fmMain.ViewTableFields(FTableName, FDBIndex, sgFields);
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshConstraintClick(Sender: TObject);
begin
  FSQLTrans.Commit;
  fmMain.FillAndShowConstraintsForm(Self, FTableName, FDBIndex);
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshIndicesClick(Sender: TObject);
begin
  fmMain.ShowIndicesManagement(Self, FDBIndex, FTableName);
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
  FSQLTrans.Commit;
  FillTriggers;
  Parent.Show;
  Show;
end;

procedure TfmTableManage.bbRefreshReferencesClick(Sender: TObject);
begin
  FSQLTrans.Commit;
  dmSysTables.Init(FDBIndex);
  dmSysTables.GetConstraintsOfTable(FTableName, SQLQuery1);
  sgReferences.RowCount:= 1;

  SQLQuery1.First;
  with SQLQuery1, sgReferences do
  while not EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= FieldByName('ConstName').AsString;
    Cells[1, RowCount - 1]:= FieldByName('OtherTableName').AsString;
    Cells[2, RowCount - 1]:= FieldByName('OtherFieldName').AsString;
    Cells[3, RowCount - 1]:= FieldByName('KeyName').AsString;
    Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.cbIndexTypeChange(Sender: TObject);
begin
  case cbIndexType.ItemIndex of
    0: edIndexName.Text:= 'PK_' + FTableName + '_1';
    1: edIndexName.Text:= 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);
  end;
end;

procedure TfmTableManage.edDropClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete the field: ' + sgFields.Cells[1, sgFields.Row] +
    ' with its data', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop field', 'ALTER TABLE ' + FTableName + ' DROP ' +
      sgFields.Cells[1, sgFields.Row], @bbRefreshClick);
  end;
end;

procedure TfmTableManage.bbEditPermissionClick(Sender: TObject);
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
    fmPermissions.Init(FDBIndex, FTableName, sgPermissions.Cells[0, sgPermissions.Row], UserType, @bbRefreshPermissionsClick);
    fmPermissions.Show;
  end
  else
    ShowMessage('There is no selected user/role');
end;


procedure TfmTableManage.Init(dbIndex: Integer; TableName: string);
begin
  try
    FDBIndex:= dbIndex;
    FTableName:= TableName;
    with fmMain do
    begin
      FIBConnection:= RegisteredDatabases[dbIndex].IBConnection;
      FIBConnection.Close;
      FSQLTrans:= RegisteredDatabases[dbIndex].SQLTrans;
      FIBConnection.Transaction:= FSQLTrans;
    end;
    SQLQuery1.Close;
    SQLQuery1.DataBase:= FIBConnection;
    SQLQuery2.Close;
    SQLQuery2.DataBase:= FIBConnection;
  except
    on E: Exception do
    begin
      MessageDlg('Error while initalizing Table Management: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TfmTableManage.FillConstraints(dbIndex: Integer);
begin
  SQLQuery1.First;
  FDBIndex:= dbIndex;
  sgConstraints.RowCount:= 1;
  with sgConstraints do
  // SQLQuery1 should have been filled by GetTableConstraints
  while not SQLQuery1.EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= SQLQuery1.FieldByName('ConstName').AsString;
    Cells[1, RowCount - 1]:= SQLQuery1.FieldByName('KeyName').AsString;
    Cells[2, RowCount - 1]:= SQLQuery1.FieldByName('OtherFieldName').AsString;
    Cells[3, RowCount - 1]:= SQLQuery1.FieldByName('CurrentTableName').AsString;
    Cells[4, RowCount - 1]:= dmSysTables.GetConstraintForeignKeyFields(SQLQuery1.FieldByName('CurrentFieldName').AsString, SQLQuery2);

    Cells[5, RowCount - 1]:= SQLQuery1.FieldByName('UpdateRule').AsString;
    Cells[6, RowCount - 1]:= SQLQuery1.FieldByName('DeleteRule').AsString;
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.FillTriggers;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= Format('SELECT RDB$Trigger_Name, RDB$Trigger_Inactive FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 ' +
    'and RDB$Relation_Name = ''%s'' ',[FTableName]);
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
  {todo: analyse transaction behaviour. Why do we have an explicit commit here? Also,
  get rid of the implicit rollbacks and use a separate read only transaction for extracting
  DDL and other read only info}
  FSQLTrans.Commit;
  UsersList:= TStringList.Create;
  try
    UsersList.CommaText:= dmSysTables.GetDBUsers(FDBIndex, FTableName);
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
      Permissions:= dmSysTables.GetObjectUserPermission(FDBIndex, FTableName, UserName, ObjType);

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
  finally
    UsersList.Free;
  end;
end;

initialization
  {$I tablemanage.lrs}

end.

