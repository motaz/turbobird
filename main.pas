unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, memds, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, Menus, ComCtrls, Reg, QueryWindow, Grids,
  ExtCtrls, Buttons, StdCtrls, TableManage;

type

  TDatabaseRec = record
    Index: Integer;
    RegRec: TRegisteredDatabase;
    OrigRegRec: TRegisteredDatabase;
    IBConnection: TIBConnection;
    SQLTrans: TSQLTransaction;
  end;

  { TfmMain }

  TfmMain = class(TForm)
    bbCreateNewDB: TBitBtn;
    bbRegisterDB: TBitBtn;
    bbRestoreDB: TBitBtn;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    mdsHistory: TMemDataset;
    MenuItem1: TMenuItem;
    lmDisplayView: TMenuItem;
    lmViewTrigger: TMenuItem;
    lmCreateDB: TMenuItem;
    lmRegdb: TMenuItem;
    lmRestore: TMenuItem;
    lmSweep: TMenuItem;
    lmAddUser: TMenuItem;
    lmChangePassword: TMenuItem;
    lmUserPermManagement: TMenuItem;
    lmRolePerManagement: TMenuItem;
    lmSetGen: TMenuItem;
    lmDisconnect: TMenuItem;
    lmCopyTable: TMenuItem;
    lmCopyUserPermission: TMenuItem;
    lmViewFields: TMenuItem;
    lmEditField: TMenuItem;
    lmDBIndo: TMenuItem;
    lmCopyRolePermission: TMenuItem;
    lmCompare: TMenuItem;
    mnExit: TMenuItem;
    mnCreateDB: TMenuItem;
    mnRegDB: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    lmEditReg: TMenuItem;
    lmUnregisterDatabase: TMenuItem;
    lmViewFirst1000: TMenuItem;
    lmViewStoredProcedure: TMenuItem;
    lmViewGen: TMenuItem;
    lmNewTable: TMenuItem;
    lmNewGen: TMenuItem;
    lmCreateAutoInc: TMenuItem;
    lmCreateStoredProc: TMenuItem;
    lmEditProc: TMenuItem;
    lmCreateView: TMenuItem;
    lmDisplay1000V: TMenuItem;
    lmEditView: TMenuItem;
    lmCreateTrigger: TMenuItem;
    lmEditTrigger: TMenuItem;
    lmActivateTrig: TMenuItem;
    lmDeactiveTrig: TMenuItem;
    lmScriptTable: TMenuItem;
    lmScriptTableCreate: TMenuItem;
    lmScriptInsert: TMenuItem;
    lmScriptUpdate: TMenuItem;
    lmEditTable: TMenuItem;
    lmCallStoreProc: TMenuItem;
    lmEditDataForm: TMenuItem;
    lmNewUDF: TMenuItem;
    lmViewUDF: TMenuItem;
    lmOpenSystemTable: TMenuItem;
    lmViewDomain: TMenuItem;
    lmNewDomain: TMenuItem;
    lmNewRole: TMenuItem;
    MenuItem7: TMenuItem;
    lmOpenQuery: TMenuItem;
    lmNewException: TMenuItem;
    lmRefresh: TMenuItem;
    lmDropException: TMenuItem;
    lmScriptException: TMenuItem;
    lmScriptDatabase: TMenuItem;
    lmConnectAs: TMenuItem;
    lmPermissions: TMenuItem;
    lmRolePermissions: TMenuItem;
    lmTableManage: TMenuItem;
    MenuItem8: TMenuItem;
    lmBackup: TMenuItem;
    mnRestore: TMenuItem;
    PageControl1: TPageControl;
    pmDatabase: TPopupMenu;
    Splitter1: TSplitter;
    SQLQuery1: TSQLQuery;
    TabSheet1: TTabSheet;
    tvMain: TTreeView;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lmAddUserClick(Sender: TObject);
    procedure lmBackupClick(Sender: TObject);
    procedure lmChangePasswordClick(Sender: TObject);
    procedure lmCloseTabClick(Sender: TObject);
    procedure lmCompareClick(Sender: TObject);
    procedure lmCopyRolePermissionClick(Sender: TObject);
    procedure lmCopyUserPermissionClick(Sender: TObject);
    procedure lmCopyTableClick(Sender: TObject);
    procedure lmCreateDBClick(Sender: TObject);
    procedure lmDBIndoClick(Sender: TObject);
    procedure lmDisconnectClick(Sender: TObject);
    procedure lmEditFieldClick(Sender: TObject);
    procedure lmOpenSystemTableClick(Sender: TObject);
    procedure lmActivateTrigClick(Sender: TObject);
    procedure lmCallStoreProcClick(Sender: TObject);
    procedure lmConnectAsClick(Sender: TObject);
    procedure lmCreateAutoIncClick(Sender: TObject);
    procedure lmCreateStoredProcClick(Sender: TObject);
    procedure lmCreateTriggerClick(Sender: TObject);
    procedure lmCreateViewClick(Sender: TObject);
    procedure lmDeactiveTrigClick(Sender: TObject);
    procedure lmDisplay1000VClick(Sender: TObject);
    procedure lmDropExceptionClick(Sender: TObject);
    procedure lmEditDataFormClick(Sender: TObject);
    procedure lmEditProcClick(Sender: TObject);
    procedure lmEditTableClick(Sender: TObject);
    procedure lmEditTriggerClick(Sender: TObject);
    procedure lmEditViewClick(Sender: TObject);
    procedure lmNewDomainClick(Sender: TObject);
    procedure lmNewExceptionClick(Sender: TObject);
    procedure lmNewGenClick(Sender: TObject);
    procedure lmNewTableClick(Sender: TObject);
    procedure lmNewUDFClick(Sender: TObject);
    procedure lmOpenQueryClick(Sender: TObject);
    procedure lmPermissionsClick(Sender: TObject);
    procedure lmRefreshClick(Sender: TObject);
    procedure lmRegdbClick(Sender: TObject);
    procedure lmRestoreClick(Sender: TObject);
    procedure lmRolePerManagementClick(Sender: TObject);
    procedure lmRolePermissionsClick(Sender: TObject);
    procedure lmScriptDatabaseClick(Sender: TObject);
    procedure lmScriptExceptionClick(Sender: TObject);
    procedure lmScriptInsertClick(Sender: TObject);
    procedure lmScriptTableCreateClick(Sender: TObject);
    procedure lmScriptUpdateClick(Sender: TObject);
    procedure lmSetGenClick(Sender: TObject);
    procedure lmSweepClick(Sender: TObject);
    procedure lmTableManageClick(Sender: TObject);
    procedure lmUserPermManagementClick(Sender: TObject);
    procedure lmViewDomainClick(Sender: TObject);
    procedure lmDisplayViewClick(Sender: TObject);
    procedure lmViewFieldsClick(Sender: TObject);
    procedure lmViewGenClick(Sender: TObject);
    procedure lmViewStoredProcedureClick(Sender: TObject);
    procedure lmViewTriggerClick(Sender: TObject);
    procedure lmViewUDFClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnCreateDBClick(Sender: TObject);
    procedure mnRegDBClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure lmEditRegClick(Sender: TObject);
    procedure lmUnregisterDatabaseClick(Sender: TObject);
    procedure lmViewFirst1000Click(Sender: TObject);
    procedure lmNewRoleClick(Sender: TObject);
    procedure mnRestoreClick(Sender: TObject);
    procedure PageControl1CloseTabClicked(Sender: TObject);
    procedure pmDatabasePopup(Sender: TObject);
    procedure tvMainDblClick(Sender: TObject);
    procedure tvMainExpanded(Sender: TObject; Node: TTreeNode);
  private
    ibConnection: TIBConnection;
    sqlTransaction: TSQLTransaction;
    CurrentHistoryFile: string;
    fActivated: Boolean;
    function FindCusomForm(ATitle: string; AClass: TClass): TComponent;
    procedure InitNewGen(DatabaseIndex: Integer);
    function GetServerNameNode(ServerName: string): TTreeNode;
    function RemoveSpecialChars(AText: string): string;
    procedure ReleaseRegisteredDatabases;
    procedure SetConnection(Index: Integer);
    procedure SetFocus; override; // solve a bug in Lazarus
    { private declarations }
  public
    RegisteredDatabases: array of TDatabaseRec;
    Version: string;
    VersionDate: string;
    function GetServerName(DBName: string): string;
    function RetreiveInputParamFromSP(Body: string): string;
    function LoadRegisteredDatabases: Boolean;
    function FindQueryWindow(ATitle: string): TComponent;
    function DeleteRegistration(Index: Integer): Boolean;
    function GetFBTypeName(Index: Integer): string;
    function GetPrimaryKeyIndexName(DatabaseIndex: Integer; ATableName: string; var ConstraintName: string): string;
    function GetConstraintFields(ATableName, AIndexName: string; var List: TStringList): Boolean;
    procedure GetFields(DatabaseIndex: Integer; ATableName: string; FieldsList: TStringList);
    function GetStoredProcBody(DatabaseIndex: Integer; AProcName: string; var SPOwner: string): string;
    function GetViewInfo(DatabaseIndex: Integer; AViewName: string; var Columns, Body: string): Boolean;
    function ChangeTriggerActivity(DatabaseIndex: Integer; ATriggerName: string; ActiveState: Boolean): Boolean;
    function GetIndices(ATableName: string; AQuery: TSQLQuery): Boolean;
    function GetIndexFields(ATableName, AIndexName: string; AQuery: TSQLQuery; var FieldsList: TStringList): Boolean;
    function GetUDFInfo(DatabaseIndex: Integer; UDFName: string; var ModuleName, EntryPoint, Params: string): Boolean;
    function ShowQueryWindow(DatabaseIndex: Integer; ATitle: string): TfmQueryWindow;
    procedure FillObjectRoot(Node: TTreeNode);
    procedure FillAndShowConstraintsForm(Form: TfmTableManage; ATableName: string; dbIndex: Integer);
    procedure ShowCompleteQueryWindow(DatabaseIndex: Integer; ATitle, AQueryText: string;
      OnCommitProcedure: TNotifyEvent = nil);
    procedure ViewTableFields(ATableName: string; dbIndex: Integer; AStringGrid: TStringGrid);
    procedure ShowIndicesManagement(AForm: TForm; DatabaseIndex: Integer; ATableName: string);
    function ChangeQueryToBIDirectional(DatabaseIndex: Integer; ATableName: string; sqQuery: TSQLQuery): Boolean;
    function GetTableNames(dbIndex: Integer): string;
    function CreateNewTrigger(dbIndex: Integer; ATableName: string; OnCommitProcedure: TNotifyEvent = nil): Boolean;
    function GetNumericFieldType(FieldType, SubType, FieldLength, Scale: Integer): string;
    function AddToSQLHistory(DatabaseTitle: string; SQLType, SQLStatement: string): Boolean;
    function SaveAndCloseSQLHistory: Boolean;
    function OpenSQLHistory(DatabaseTitle: string): Boolean;
  end;

var
  fmMain: TfmMain;

implementation


{ TfmMain }

uses CreateDB, ViewView, ViewTrigger, ViewSProc, ViewGen, NewTable, NewGen,
     EnterPass, CreateTrigger, EditTable, CallProc, EditDataFullRec, UDFInfo, ViewDomain,
     NewDomain, SysTables, Scriptdb, UserPermissions, BackupRestore, UnitFirebirdServices, CreateUser, ChangePass,
     PermissionManage, CopyTable, About, NewEditField, dbInfo, Comparison;


procedure TfmMain.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  fActivated:= False;
  LoadRegisteredDatabases;
end;

(*****************  Add New user  ***********************)

procedure TfmMain.lmAddUserClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
begin
  with fmCreateUser do
  try
    SelNode:= tvMain.Selected;
    dbIndex:= SelNode.Parent.OverlayIndex;
    Init(dbIndex);
    edUserName.Clear;
    edPassword.Clear;
    if ShowModal = mrOK then
    begin
      // Create user
      dmSysTables.Init(dbIndex);
      dmSysTables.sqQuery.Close;
      dmSysTables.sqQuery.SQL.Text:= 'create user ' + edUserName.Text + ' password ''' + edPassword.Text  + '''';
      dmSysTables.sqQuery.ExecSQL;

      // Grant rule
      if cxGrantRole.Checked then
      begin
        dmSysTables.sqQuery.SQL.Text:= 'grant ' + cbRoles.Text + ' to ' + edUserName.Text;
        dmSysTables.sqQuery.ExecSQL;
      end;
      dmSysTables.stTrans.Commit;
      MessageDlg('New user (' + edUserName.Text + ') has been created successfully', mtInformation, [mbOk], 0);
      if not cxGrantRole.Checked then
        ShowMessage('User (' + edUserName.Text + ') will not appear in users list unless you grant it a permission');
      lmRefresh.Click;
    end;

  except
  on e: exception do
  begin
    MessageDlg('Error while creating new user: ' + e.Message, mtError, [mbOk], 0);
  end;

  end;
end;

(***********  Backup / Restore database ************)

procedure TfmMain.lmBackupClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  with RegisteredDatabases[tvMain.Selected.OverlayIndex].RegRec do
    fmBackupRestore.Init(SelNode.Text, DatabaseName, UserName, Password);
  fmBackupRestore.cbOperation.Enabled:= True;
  fmBackupRestore.Show;
end;

(**********  change user password  **********)

procedure TfmMain.lmChangePasswordClick(Sender: TObject);
begin
  fmChangePass.Caption:= 'Change password for user: ' + tvMain.Selected.Text;
  fmChangePass.edPassword.Clear;
  fmChangePass.edConfirm.Clear;
  if fmChangePass.ShowModal = mrOK then
  begin
    try
       dmSysTables.Init(tvMain.Selected.Parent.Parent.OverlayIndex);
       dmSysTables.sqQuery.Close;
       dmSysTables.sqQuery.SQL.Text:= 'alter user ' + tvMain.Selected.Text + ' password ''' +
         fmChangePass.edPassword.Text + '''';
       dmSysTables.sqQuery.ExecSQL;
       dmSysTables.stTrans.Commit;
       MessageDlg('Password has been changed', mtInformation, [mbOk], 0);

    except
    on e: exception do
      ShowMessage('Error while changing password: ' + e.Message);
    end;

  end;
end;

procedure TfmMain.lmCloseTabClick(Sender: TObject);
begin
end;

procedure TfmMain.lmCompareClick(Sender: TObject);
begin
  fmComparison.Init(tvMain.Selected.OverlayIndex);
  fmComparison.Show;
end;

procedure TfmMain.lmCopyRolePermissionClick(Sender: TObject);
begin
  lmCopyUserPermissionClick(nil);
end;

procedure TfmMain.lmCopyUserPermissionClick(Sender: TObject);
var
  List: TStringList;
  dbIndex: Integer;
  UserName: string;
  NewUser: string;
begin
  if InputQuery('Permission', 'Please type a User/Role name to copy perissions to', NewUser) then
  begin
    UserName:= tvMain.Selected.Text;
    dbIndex:= tvMain.Selected.Parent.Parent.OverlayIndex;
    List:= TStringList.Create;
    Scriptdb.ScriptUserAllPermissions(dbIndex, UserName, List, NewUser);
    ShowCompleteQueryWindow(dbIndex, 'Script permissions for : ' + UserName, List.Text);
    List.Free;
  end;
end;

procedure TfmMain.lmCopyTableClick(Sender: TObject);
begin
  fmCopyTable.Init(tvMain.Selected.Parent.Parent.OverlayIndex, tvMain.Selected.Text);
  fmCopyTable.Show;
end;

procedure TfmMain.lmCreateDBClick(Sender: TObject);
begin
  fmCreateDB.edNewDatabase.Text:= tvMain.Selected.Text + ':';
  mnCreateDBClick(nil);
end;

procedure TfmMain.lmDBIndoClick(Sender: TObject);
var
  dbName, CreationDate, ACharSet: string;
  MajorVer, MinorVer, Pages, PageSize: Integer;
  ProcessList: TStringList;
  dbSize: Double;
  AType: string;
  ATab: TTabSheet;
  Title: string;
  dbIndex: Integer;
begin
  ProcessList:= TStringList.Create;
  dbIndex:= tvMain.Selected.OverlayIndex;
  Title:= 'Database information for: ' + tvMain.Selected.Text;
  if dmSysTables.GetDatabaseInfo(dbIndex, dbName, ACharSet, CreationDate,
    MajorVer, MinorVer, Pages, PageSize, ProcessList) then
  with fmDBInfo do
  begin
    fmDBInfo:= FindCusomForm(Title, TfmDBInfo) as TfmDBInfo;

    if fmDBInfo = nil then
    begin
      fmDBInfo:= TfmDBInfo.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmDBInfo.Parent:= ATab;
      fmDBInfo.Left:= 0;
      fmDBInfo.Top:= 0;
      fmDBInfo.BorderStyle:= bsNone;
      fmDBInfo.Align:= alClient;
      Caption:= Title;
    end
    else
      ATab:= fmDBInfo.Parent as TTabSheet;

    PageControl1.ActivePage:= ATab;
    ATab.Tag:= dbIndex;
    ATab.Caption:= Title;
    edName.Text:= dbName;
    edODSVer.Text:= IntToStr(MajorVer) + '.' + IntToStr(MinorVer);
    edCharset.Text:= ACharSet;
    edCreationDate.Text:= CreationDate;
    edPageSize.Text:= IntToStr(PageSize);
    dbSize:= Pages * PageSize;
    if dbSize > 1000000000 then
    begin
      dbSize:= ((dbSize / 1024) / 1024) / 1024;
      AType:= 'Giga bytes';
    end
    else
    if dbSize > 1000000 then
    begin
      dbSize:= ((dbSize / 1024) / 1024);
      AType:= 'Mega bytes';
    end
    else
    if dbSize > 1000 then
    begin
      dbSize:= (dbSize / 1024);
      AType:= 'Kilo bytes';
    end
    else
    begin
      AType:= 'Bytes';
    end;

    edDBSize.Text:= Format('%3.1n %s', [dbSize, AType]);
    meClients.Lines.Text:= ProcessList.Text;
    ProcessList.Free;
    Show;
  end
  else
    ShowMessage('Unable to get database information');
end;

procedure TfmMain.lmDisconnectClick(Sender: TObject);
var
  dbIndex: Integer;
  i: Integer;
  Form: TForm;
  j: Integer;
  TabSheet: TTabSheet;
begin
  dbIndex:= tvMain.Selected.OverlayIndex;
  RegisteredDatabases[dbIndex].IBConnection.Close;
  for i:= PageControl1.PageCount - 1 downto 0 do
    if (PageControl1.Pages[i] as TComponent).Tag = dbIndex then
    begin
      TabSheet:= PageControl1.Page[i] as TTabSheet;
      for j:= 0 to TabSheet.ControlCount - 1 do
      if TabSheet.Controls[j] is TForm then
      begin
        (TabSheet.Controls[j] as TForm).Close;
        TabSheet.Free;
        Break;
      end;
    end;
  tvMain.Selected.Collapse(True);
end;

procedure TfmMain.lmEditFieldClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
  FieldName: string;
  FieldType, DefaultValue: string;
  FSize: Integer;
  Description: string;
  NotNull: Boolean;
begin
  SelNode:= tvMain.Selected;
  dbIndex:= SelNode.Parent.Parent.Parent.OverlayIndex;
  FieldName:= Copy(SelNode.Text, 1, Pos(' ', SelNode.Text) - 1);
  if dmSysTables.GetFieldInfo(dbIndex, SelNode.Parent.Text, FieldName, FieldType, FSize, NotNull,
    DefaultValue, Description) then
  begin
    fmNewEditField:= TfmNewEditField.Create(nil);
    fmNewEditField.Init(dbIndex, SelNode.Parent.Text, foEdit, FieldName, FieldType, DefaultValue, Description, FSize,
      SelNode.OverlayIndex, not NotNull,  nil);

    fmNewEditField.Show;
  end
  else
    ShowMessage('Unable to locate the field: ' + SelNode.Text);
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Length(RegisteredDatabases) > 0 then
    fmReg.SaveRegistrations;
  SaveAndCloseSQLHistory;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  fActivated:= True;
end;


(***************  Open System table  **************)

procedure TfmMain.lmOpenSystemTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, SelNode.Text);
    QWindow.meQuery.Lines.Text:= 'select * from ' + SelNode.Text;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;

end;

procedure TfmMain.lmActivateTrigClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if ChangeTriggerActivity(SelNode.Parent.Parent.OverlayIndex, SelNode.Text, True) then
    MessageDlg('Trigger has been activated', mtInformation, [mbOk], 0);
end;


(*******************  Call stored procedure  *****************)

procedure TfmMain.lmCallStoreProcClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Body: string;
  AProcName: string;
  SPOwner: string;
  Params: string;
  OneParam: string;
  i: Integer;
  QWindow: TfmQueryWindow;
  Line: string;
  Called: Boolean;
  WithParams: Boolean;
begin
  SelNode:= tvMain.Selected;

  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AProcName:= SelNode.Text;
    Body:= GetStoredProcBody(SelNode.Parent.Parent.OverlayIndex, AProcName, SPOwner);
    Params:= RetreiveInputParamFromSP(Body);
    withParams:= Params <> '';
    with fmCallProc do
    begin
      StringGrid1.RowCount:= 1;
      if WithParams then
        Params:= Params + ',';
      i:= 1;
      if WithParams then
      while Params <> '' do
      begin
        OneParam:= Copy(Params, 1, Pos(',', Params) - 1);
        Delete(Params, 1, Pos(',', Params));
        Params:= Trim(Params);

        StringGrid1.RowCount:= i + 1;

        StringGrid1.Cells[1, i]:= Trim(Copy(OneParam, 1, Pos(' ', OneParam)));
        StringGrid1.Cells[2, i]:= Trim(Copy(OneParam, Pos(' ', OneParam), Length(OneParam)));

        Inc(i);
      end;

      Caption:= 'Call stored procedure: ' + AProcName;

      if WithParams then
        Called:= ShowModal = mrOK;  // View parameters form

      if (Called) or (not WithParams) then
      begin
        QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Call procedure: ' + AProcName);
        QWindow.meQuery.lines.Clear;
        if Pos('suspend', LowerCase(Body)) > 0 then
          Line:= 'select * from ' + AProcName
        else
          Line:= 'execute procedure ' + AProcName;

        if WithParams then
          Line:= Line + '(';

        for i:= 1 to StringGrid1.RowCount - 1 do
          if Pos('CHAR', StringGrid1.Cells[2, i]) > 0 then
            Line:= Line + '''' + StringGrid1.Cells[0, i] + ''', '
          else
            Line:= Line + StringGrid1.Cells[0, i] + ', ';
        if WithParams then
        begin
          Delete(Line, Length(Line) - 1, 2);
          Line:= Line + ')';
        end;
        Line:= Line + ';';
        QWindow.meQuery.Lines.Add(Line);
        QWindow.Show;

      end;
    end;
  end;
end;

(****************  Connect As  *****************)

procedure TfmMain.lmConnectAsClick(Sender: TObject);
var
  Node: TTreeNode;
  Rec: TRegisteredDatabase;
  Count: Integer;
begin
  Node:= tvMain.Selected;
  Rec:= RegisteredDatabases[Node.OverlayIndex].RegRec;
  fmEnterPass.edUser.Text:= Rec.UserName;
  fmEnterPass.edPassword.Clear;
  try
    fmEnterPass.cbRole.Items.CommaText:= dmSysTables.GetDBObjectNames(Node.OverlayIndex, 9, Count);
  except
  end;
  if fmEnterPass.ShowModal = mrOk then
  begin
    if fmReg.TestConnection(Rec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
      Rec.Charset) then
      begin
        RegisteredDatabases[Node.OverlayIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
        RegisteredDatabases[Node.OverlayIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
        RegisteredDatabases[Node.OverlayIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
        Node.Expand(False);
      end
      else
      begin
        Exit;
      end;
  end;
end;

(****************  Fill and show constraints form ************************)

procedure TfmMain.FillAndShowConstraintsForm(Form: TfmTableManage; ATableName: string; dbIndex: Integer);
begin
  dmSysTables.Init(dbIndex);
  dmSysTables.GetTableConstraints(ATableName, Form.SQLQuery1);
  Form.FillConstraints(dbIndex);
end;

(***********  Show and Fill Query Window *****************)

procedure TfmMain.ShowCompleteQueryWindow(DatabaseIndex: Integer; ATitle,
  AQueryText: string; OnCommitProcedure: TNotifyEvent = nil);
var
  QWindow: TfmQueryWindow;
  Part: string;
begin
  QWindow:= ShowQueryWindow(DatabaseIndex, ATitle);
  QWindow.meQuery.ClearAll;
  QWindow.OnCommit:= OnCommitProcedure;
  repeat
    if Pos(#10, AQueryText) > 0 then
      Part:= Copy(AQueryText, 1, Pos(#10, AQueryText))
    else
      Part:= AQueryText;
    Delete(AQueryText, 1, Length(Part));
    Part:= StringReplace(Part, #10, ' ', [rfReplaceAll]);

    QWindow.meQuery.Lines.Add(Part);

  until AQueryText = '';
end;

(***********************  Constraint Management  ********************)


(**********  Create Auto Increment Trigger from current generator  **********)

procedure TfmMain.lmCreateAutoIncClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    InitNewGen(SelNode.Parent.Parent.OverlayIndex);
    fmNewGen.edGenName.Text:= SelNode.Text;
    fmNewGen.edGenName.Enabled:= False;
    fmNewGen.cxTrigger.Checked:= True;
    fmNewGen.ShowModal;
  end;
end;

(****************  Create new stored proc  *******************)

procedure TfmMain.lmCreateStoredProcClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AProcName: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new stored procedure', 'Please enter new procedure name', AProcName) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new stored procedure');
    QWindow.meQuery.Lines.Clear;
 //   QWindow.meQuery.Lines.Add('SET TERM ^;');
    QWindow.meQuery.Lines.Add('CREATE PROCEDURE ' + AProcName);
    QWindow.meQuery.Lines.Add('-- Input parameters, you can modify,remove them');

    QWindow.meQuery.Lines.Add('( Input1 int, -- You can replace it by your first parameter');
    QWindow.meQuery.Lines.Add(' Input2 varchar(20) -- you can replace it by your second parameter');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('RETURNS');
    QWindow.meQuery.Lines.Add('( Out1 int -- You can replace it by your first parameter');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  -- Write your procedure code here');
    QWindow.meQuery.Lines.Add('END;');
   // QWindow.meQuery.Lines.Add('SET TERM ;^');
    QWindow.Show;
  end;
end;

(***************  Create new Trigger  ****************)

function TfmMain.CreateNewTrigger(dbIndex: Integer; ATableName: string; OnCommitProcedure: TNotifyEvent = nil): Boolean;
var
  QWindow: TfmQueryWindow;
  AViewName: string;
  TableNames: string;
  TrigType: string;
  Count: Integer;
begin
  Result:= False;
  if ATableName <> '' then
  begin
    fmCreateTrigger.cbTables.Clear;
    fmCreateTrigger.cbTables.Items.Add(ATableName);
    fmCreateTrigger.cbTables.ItemIndex:= 0;
  end;
  fmCreateTrigger.edTriggerName.Clear;
  fmCreateTrigger.cxUpdate.Checked:= False;
  fmCreateTrigger.cxInsert.Checked:= False;
  fmCreateTrigger.cxDelete.Checked:= False;

  if fmCreateTrigger.ShowModal = mrOK then
  begin
    Result:= True;
    QWindow:= ShowQueryWindow(dbIndex, 'Create new Trigger');
    if fmCreateTrigger.rbAfter.Checked then
      TrigType:= 'After'
    else
      TrigType:= 'Before';
    if fmCreateTrigger.cxInsert.Checked then
      TrigType:= TrigType + ' insert or';
    if fmCreateTrigger.cxUpdate.Checked then
      TrigType:= TrigType + ' update or';
    if fmCreateTrigger.cxDelete.Checked then
      TrigType:= TrigType + ' delete or';
    Delete(TrigType, Length(TrigType) - 2, 3);

    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE TRIGGER ' + fmCreateTrigger.edTriggerName.Text + ' for ' +
      fmCreateTrigger.cbTables.Text);
    QWindow.meQuery.Lines.Add('Active');
    QWindow.meQuery.Lines.Add(TrigType);
    QWindow.meQuery.Lines.Add('Position 0');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add(' -- Your code here');
    QWindow.meQuery.Lines.Add(' -- New.FieldName = YourData;');
    QWindow.meQuery.Lines.Add('END;');
    fmMain.Show;

    if OnCommitProcedure <> nil then
      QWindow.OnCommit:= OnCommitProcedure;
  end;
end;

(*******  Create Trigger click  ********)

procedure TfmMain.lmCreateTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  DBIndex: Integer;
  QWindow: TfmQueryWindow;
  AViewName: string;
  TableNames: string;
  TrigType: string;
  Count: Integer;
begin
  SelNode:= tvMain.Selected;
  DBIndex:= SelNode.Parent.OverlayIndex;

  TableNames:= dmSysTables.GetDBObjectNames(DBIndex, 1, Count);
  fmCreateTrigger.cbTables.Items.CommaText:= TableNames;
  CreateNewTrigger(DBIndex, '');
end;

(******************  Create New View   ***************)

procedure TfmMain.lmCreateViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AViewName: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new view', 'Please enter new view name', AViewName) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new view');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE VIEW "' + AViewName + '" (');
    QWindow.meQuery.Lines.Add('Field1Name, Field2Name) ');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('SELECT Field1, Field2 FROM ATableName');
    QWindow.meQuery.Lines.Add('-- WHERE condition');
    QWindow.Show;
  end;
end;

procedure TfmMain.lmDeactiveTrigClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if ChangeTriggerActivity(SelNode.Parent.Parent.OverlayIndex, SelNode.Text, False) then
    MessageDlg('Trigger has been DeActivated', mtInformation, [mbOk], 0);
end;

(***************  Display view top 1000 records  ************)

procedure TfmMain.lmDisplay1000VClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex,  'Select first 1000 from ' + SelNode.Text);
    QWindow.meQuery.Lines.Text:= 'select first 1000 * from "' + SelNode.Text + '"';
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

(**********  Drop Exception ********)

procedure TfmMain.lmDropExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if MessageDlg('Are you sure you want to delete ' + SelNode.Text + ' permenentaly', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Drop Exception');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DROP EXCEPTION ' + SelNode.Text + ';');
    QWindow.Show;
  end;
end;

(***********  Edit Data in a form  *****************)

procedure TfmMain.lmEditDataFormClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  EditForm: TfmEditDataFullRec;
  ATableName: string;
  i: Integer;
  ConstraintsList: TStringList;
  PKFieldsList: TStringList;
  FieldLine: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    ATableName:= SelNode.Text;
    Rec:= RegisteredDatabases[dbIndex];
    EditForm:= TfmEditDataFullRec(FindCusomForm(Rec.RegRec.Title + ': Edit Data (Form) for Table : ' +
      ATableName, TfmEditDataFullRec));
    if EditForm = nil then
    begin
      EditForm:= TfmEditDataFullRec.Create(Application);
      EditForm.Init(dbIndex, ATableName);
      EditForm.Caption:= Rec.RegRec.Title + ': Edit Data (Form) for Table : ' + ATableName;
    end;
    EditForm.Show;
  end;
end;

(***************  Edit stored procedure  *****************)

procedure TfmMain.lmEditProcClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AProcName: string;
  SPOwner: string;
  spBody: string;
  QWindow: TfmQueryWindow;
  DBIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AProcName:= SelNode.Text;
    DBIndex:= SelNode.Parent.Parent.OverlayIndex;
    SPBody:= GetStoredProcBody(DBIndex, AProcName, SPOwner);

    // Procedure body
    QWindow:= ShowQueryWindow(DBIndex, 'Edit stored procedure ' + AProcName);
    QWindow.meQuery.Lines.Clear;
  //  QWindow.meQuery.Lines.Add('SET TERM ^ ;');
    QWindow.meQuery.Lines.Add('ALTER PROCEDURE ' + AProcName + '(');
    QWindow.meQuery.Text:= QWindow.meQuery.Text + Trim(spBody) + ';';
   // QWindow.meQuery.Lines.Add('SET TERM ; ^');

    QWindow.Show;
  end;
end;

(**********************  Edit Table data  ***************************)


procedure TfmMain.lmEditTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  EditWindow: TfmEditTable;
  ATableName: string;
  i: Integer;
  ConstraintsList: TStringList;
  PKFieldsList: TStringList;
  FieldLine: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    Rec:= RegisteredDatabases[dbIndex];
    EditWindow:= TfmEditTable(FindCusomForm(Rec.RegRec.Title + ': Edit Data for Table : ' + ATableName, TfmEditTable));
    if EditWindow = nil then
    begin
      EditWindow:= TfmEditTable.Create(Application);
      EditWindow.Rec:= Rec;
      EditWindow.Caption:= EditWindow.Rec.RegRec.Title + ': Edit Data for Table : ' + ATableName;

      EditWindow.Init(dbIndex, ATableName);
    end;
    EditWindow.Show;
  end;
end;

(****************  Edit Trigger  ******************)

procedure TfmMain.lmEditTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATriggerName: string;
  Body: string;
  AfterBefore: string;
  Event: string;
  OnTable: string;
  TriggerEnabled: Boolean;
  TriggerPosition: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATriggerName:= SelNode.Text;
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Edit Trigger ' + ATriggerName);

    QWindow.meQuery.Lines.Clear;
    dmSysTables.ScriptTrigger(SelNode.Parent.Parent.OverlayIndex, ATriggerName, QWindow.meQuery.Lines);
    QWindow.Show;
  end;

end;

(********************  Edit View  ********************)

procedure TfmMain.lmEditViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AViewName: string;
  ViewBody, Columns: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AViewName:= SelNode.Text;
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Edit view ' + AViewName);

    GetViewInfo(SelNode.Parent.Parent.OverlayIndex, AViewName, Columns, ViewBody);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('drop view "' + AViewName + '";');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('CREATE VIEW "' + AViewName + '" (' + Columns + ')');
    QWindow.meQuery.Lines.Add('AS');

    QWindow.meQuery.Text:= QWindow.meQuery.Text + ViewBody;
    QWindow.Show;
  end;

end;

(***********  Show Indices Management  **************)

procedure TfmMain.ShowIndicesManagement(AForm: TForm; DatabaseIndex: Integer; ATableName: string);
var
  AQuery: TSQLQuery;
  i: Integer;
  IndexFields: string;
  ATitle: string;
  Rec: TDatabaseRec;
  CurrentRow: Integer;
  FieldsList: TStringList;
  ConstraintName: string;
  Form: TfmTableManage;
begin
  Form:= AForm as TfmTableManage;
  Rec:= RegisteredDatabases[DatabaseIndex];
  AQuery:= TSQLQuery.Create(nil);
  AQuery.Close;

  if ibConnection <> RegisteredDatabases[DatabaseIndex].IBConnection then
  begin
    ibConnection:= RegisteredDatabases[DatabaseIndex].IBConnection;
    sqlTransaction:= RegisteredDatabases[DatabaseIndex].SQLTrans;
  end;
  AQuery.DataBase:= ibConnection;

  Form.sgIndices.RowCount:= 1;

  // Get primary key index name
  Form.PKeyName:= GetPrimaryKeyIndexName(DatabaseIndex, ATableName, ConstraintName);
  Form.ConstraintName:= ConstraintName;

  // Index names
  if GetIndices(ATableName, AQuery) then
  with Form do
  while not AQuery.EOF do
  begin
    if Trim(AQuery.FieldByName('RDB$Index_name').AsString) = PKeyName then
    begin
      sgIndices.InsertColRow(False, 1);
      CurrentRow:= 1;
    end
    else
    begin
      sgIndices.RowCount:= sgIndices.RowCount + 1;
      CurrentRow:= sgIndices.RowCount - 1;
    end;
    sgIndices.Cells[0, CurrentRow]:= Trim(AQuery.FieldByName('RDB$Index_Name').AsString);
    if AQuery.FieldByName('RDB$Unique_Flag').AsString = '1' then
      sgIndices.Cells[1, CurrentRow]:= '1'
    else
      sgIndices.Cells[1, CurrentRow]:= '0';

    if AQuery.FieldByName('RDB$Index_Type').AsString = '1' then
      sgIndices.Cells[2, CurrentRow]:= 'Desc'
    else
      sgIndices.Cells[2, CurrentRow]:= 'Asc';

    if Trim(AQuery.FieldByName('RDB$Index_Name').AsString) = Form.PKeyName then
      sgIndices.Cells[4, CurrentRow]:= '1'
    else
      sgIndices.Cells[4, CurrentRow]:= '0';
    AQuery.Next;
  end;

  FieldsList:= TStringList.Create;

  // Index fields
  with Form do
  for i:= 1 to sgIndices.RowCount - 1 do
  begin
    IndexFields:= '';
    if GetIndexFields(ATableName, sgIndices.Cells[0, i], AQuery, FieldsList) then
    begin
      IndexFields:= FieldsList.CommaText;
      sgIndices.Cells[3, i]:= IndexFields;
    end;
  end;
  FieldsList.Free;
  Form.edIndexName.Text:= 'IX_' + ATableName + '_' + IntToStr(Form.sgIndices.RowCount);

  // Field names
  GetFields(DatabaseIndex, ATableName, nil);
  with Form, Self.SQLQuery1 do
  begin
    clbFields.Clear;
    while not EOF do
    begin
      if (Pos('CHAR', Trim(FieldByName('Field_Type_Str').AsString)) = 0) or
       (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
       (FieldByName('Field_Collation').IsNull) then
      if (FieldByName('Field_Type_Str').AsString <> 'BLOB') then
        clbFields.Items.Add(FieldByName('Field_Name').AsString);
      Next;
    end;
    Self.SQLQuery1.Close;
  end;
  AQuery.Close;
  AQuery.Free;
  if Form.sgIndices.RowCount > 1 then
    Form.sgIndices.Row:= 1;
end;

(**************  New Domain  *************)

procedure TfmMain.lmNewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  Line: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if fmNewDomain.ShowModal = mrOk then
  with QWindow do
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new domain');
    meQuery.Lines.Clear;
    Line:= 'CREATE DOMAIN ' + fmNewDomain.edName.Text + ' AS ' + fmNewDomain.cbType.Text;
    if Pos('char', LowerCase(fmNewDomain.cbType.Text)) > 0 then
      Line:= Line + '(' + IntToStr(fmNewDomain.seSize.Value) + ')';
    meQuery.Lines.Add(Line);

    if Trim(fmNewDomain.edDefault.Text) <> '' then
    begin
      if Pos('char', LowerCase(fmNewDomain.cbType.Text)) > 0 then
        meQuery.Lines.Add('default ''' + fmNewDomain.edDefault.Text + '''')
      else
        meQuery.Lines.Add('DEFAULT ' + fmNewDomain.edDefault.Text);
    end;
    Show;
  end;
end;

(***********  Add New exception  ****************)

procedure TfmMain.lmNewExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new Exception');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE EXCEPTION Exception_name_1 ''exception message'';');
    QWindow.Show;
  end;
end;


(**************  Initialize New Generator form  *************)

procedure TfmMain.InitNewGen(DatabaseIndex: Integer);
var
  Rec: TDatabaseRec;
  TableNames: string;
  Count: integer;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];

  fmNewGen.Init(DatabaseIndex);
end;

function TfmMain.GetServerName(DBName: string): string;
begin
  if Pos(':', DBName) > 2 then
    Result:= Copy(DBName, 1, Pos(':', DBName) - 1)
  else
    Result:= 'localhost';
end;

function TfmMain.GetServerNameNode(ServerName: string): TTreeNode;
var
  i: Integer;
  Node: TTreeNode;
begin
  Node:= nil;
  ServerName:= LowerCase(ServerName);
  if tvMain.Items.Count > 0 then
    Node:= tvMain.Items[0];
  Result:= nil;
  while Node <> nil do
  begin
    if (Node.Text <> '') and (LowerCase(Node.Text) = ServerName) then
    begin
      Result:= Node;
      Break;
    end;
    Node:= Node.GetNextSibling;
  end;
end;

function TfmMain.RemoveSpecialChars(AText: string): string;
var
  i: Integer;
begin
  for i:= Length(AText) to 1 do
    if Pos(AText[i], ' !@#$%^&*()[]{}/?<>:;"|\,.~`''') > 0 then
      System.Delete(AText, i, 1);
  Result:= AText;
end;

procedure TfmMain.ReleaseRegisteredDatabases;
var
  i: Integer;
begin
  for i:= 0 to High(RegisteredDatabases) do
  begin
    RegisteredDatabases[i].IBConnection.Close;
    RegisteredDatabases[i].SQLTrans.Free;
    RegisteredDatabases[i].IBConnection.Free;
  end;

  RegisteredDatabases:= nil;

end;

procedure TfmMain.SetConnection(Index: Integer);
begin
  if ibConnection <> RegisteredDatabases[Index].IBConnection then
  begin
    ibConnection:= RegisteredDatabases[Index].IBConnection;
    ibConnection.Close;
    sqlTransaction:= RegisteredDatabases[Index].SQLTrans;
    ibConnection.Transaction:= sqlTransaction;
    SQLQuery1.DataBase:= ibConnection;
    SQLQuery1.Transaction:= sqlTransaction;
  end;
end;

procedure TfmMain.SetFocus;
begin
  if not fActivated then
    inherited SetFocus;
end;

function TfmMain.GetNumericFieldType(FieldType, SubType, FieldLength,
  Scale: Integer): string;
begin
  if SubType = 0 then
  begin
    case FieldType of
        7: Result:= 'SMALLINT';
        8: Result:= 'INTEGER';
        16: Result:= 'BIGINT';
    end;

  end
  else
  begin
    if SubType = 1 then
      Result:= 'Numeric('
    else
    if SubType = 2 then
      Result:= 'Decimal(';
    case FieldLength of
        4: Result:= Result + '9,';
        8: Result:= Result + '18,';
      else
        Result:= Result + IntToStr(FieldLength) + ',';
    end;
    Result:= Result + IntToStr(Abs(Scale)) + ') ';
  end;
end;

function TfmMain.AddToSQLHistory(DatabaseTitle: string; SQLType, SQLStatement: string): Boolean;
begin
  try
    Result:= OpenSQLHistory(DatabaseTitle);
    if Result then
    begin
      mdsHistory.Last;
      if (SQLType <> 'SELECT') or (mdsHistory.FieldByName('SQLStatement').AsString <> SQLStatement) then
      begin
        mdsHistory.AppendRecord([Now, SQLType, SQLStatement, 0]);
        if SQLType = 'DDL' then
          mdsHistory.SaveToFile(CurrentHistoryFile);
      end;
    end;

  except
  on e: exception do
  begin
    Result:= False;
    ShowMessage(e.Message);
  end;

  end;
end;

function TfmMain.SaveAndCloseSQLHistory: Boolean;
begin
  try
    if mdsHistory.Active then
      mdsHistory.SaveToFile(CurrentHistoryFile);

    mdsHistory.Close;
    Result:= True;

  except
  on e: exception do
  begin
    Result:= False;
    ShowMessage(e.Message)
  end;

  end;
end;

function TfmMain.OpenSQLHistory(DatabaseTitle: string): Boolean;
var
  AFileName: string;
  i: Integer;
begin
  try
    AFileName:= ExtractFilePath(ParamStr(0)) + LowerCase(RemoveSpecialChars(DatabaseTitle)) + '.history';

    // Different opened history file
    if mdsHistory.Active and (AFileName <> CurrentHistoryFile) then
    begin
      if CurrentHistoryFile <> '' then
        mdsHistory.SaveToFile(CurrentHistoryFile);
       mdsHistory.Close;
    end;

    if not mdsHistory.Active then
    if FileExists(AFileName) then
    begin
      try
        mdsHistory.LoadFromFile(AFileName);

      except
      on e: exception do
        mdsHistory.SaveToFile(AFileName);
      end;
    end
    else
      mdsHistory.CreateTable;

    if not mdsHistory.Active then
      mdsHistory.Open;

    if mdsHistory.RecNo > 10000 then
    begin
      mdsHistory.First;
      for i:= 1 to 2 do
        mdsHistory.Delete;
    end;
    CurrentHistoryFile:= AFileName;
    Result:= True;

  except
  on e: exception do
  begin
    Result:= False;
    ShowMessage(e.Message);
  end;

  end;
end;

function TfmMain.RetreiveInputParamFromSP(Body: string): string;
var
  i: Integer;
  SizeStarted: Boolean;
begin
  SizeStarted:= False;
  if (Pos('(', Body) > 0) and (Pos('(', Body) < Pos(')', Body)) then
  for i:= 1 to Length(Body) do
  begin
    if (Body[i] = ')') and (not SizeStarted) then
    begin
      Result:= Trim(Copy(Body, 1, i - 1));
      Break;
    end;

    if (Body[i] = ')') and (SizeStarted) then
      SizeStarted:= False;

    if Body[i] = '(' then
      SizeStarted:= True;

  end
  else
    Result:= Trim(Copy(Body, 1, Pos(')', Body) - 1));
end;

(**************  New Generator  *******************)

procedure TfmMain.lmNewGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    InitNewGen(SelNode.Parent.OverlayIndex);
    fmNewGen.edGenName.Clear;
    fmNewGen.edGenName.Enabled:= True;
    fmNewGen.cxTrigger.Checked:= False;
    fmNewGen.ShowModal;
  end;
end;

(************  Add New Table   ******************)

procedure TfmMain.lmNewTableClick(Sender: TObject);
var
  Rec: TDatabaseRec;
  SelNode: TTreeNode;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  dbIndex:= SelNode.Parent.OverlayIndex;
  Rec:= RegisteredDatabases[dbIndex];

  fmNewTable.Init(dbIndex);
  fmNewTable.ShowModal;
  {= mrOK then
    if fmNewTable.cxCreateGen.Checked then // Create Auto Inc generator
    begin
      InitNewGen(dbIndex);
      fmNewGen.edGenName.Text:= GeneratorName;
      fmNewGen.edGenName.Enabled:= True;
      fmNewGen.cxTrigger.Checked:= True;

      // Select table name as default in create new generator form
      fmNewGen.cbTables.ItemIndex:= fmNewGen.cbTables.Items.IndexOf(UpperCase(Trim(fmNewTable.edNewTable.Text)));
      fmNewGen.cbTablesChange(nil);
      fmNewGen.gbTrigger.Enabled:= False;
      fmNewGen.cxTrigger.Enabled:= False;
      fmNewGen.Show;
    end;}
end;

(*************  Create new function  ******************)

procedure TfmMain.lmNewUDFClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AFuncName: string;
  ModuleName, EntryPoint: string;
begin
  SelNode:= tvMain.Selected;
  ModuleName:= '<modulename>';
  EntryPoint:= '<entryname>';
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new function', 'Please enter new function name', AFuncName) then
  if InputQuery('Create new function', 'Please enter module name (Library)', ModuleName) then
  if InputQuery('Create new function', 'Please enter entry point (External function name)', EntryPoint) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new function');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DEFINE EXTERNAL FUNCTION ' + AFuncName + ' [<datatype> | CSTRING (int)');
    QWindow.meQuery.Lines.Add('[, <datatype> | CSTRING (int) ...]]');
    QWindow.meQuery.Lines.Add('RETURNS {<datatype> [BY VALUE] | CSTRING (int)}');
    QWindow.meQuery.Lines.Add('ENTRY_POINT "' + entryPoint + '"');
    QWindow.meQuery.Lines.Add('MODULE_NAME "' + modulename + '" ;');
    QWindow.Show;
  end;
end;

(**********  Open Query 2 Click ************)


(**********  Open Query Window from Database  *************)

procedure TfmMain.lmOpenQueryClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  Rec: TRegisteredDatabase;
  Count: Integer;
  dbIndex: Integer;
begin
  dbIndex:= tvMain.Selected.OverlayIndex;
  Rec:= RegisteredDatabases[dbIndex].RegRec;
  // Password form
  if (Rec.Password = '') and (not tvMain.Selected.Expanded) then
  begin
    fmEnterPass.edPassword.Clear;
    try
      fmEnterPass.cbRole.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 9, Count);
    except
    end;
    if fmEnterPass.ShowModal = mrOk then
    begin
      if fmReg.TestConnection(Rec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
        Rec.Charset) then
          RegisteredDatabases[dbIndex].RegRec.Password:= fmEnterPass.edPassword.Text
        else
          Exit;
    end;
  end;
  QWindow:= ShowQueryWindow(dbIndex, 'Query Window');
  QWindow.Show;
end;

procedure TfmMain.lmPermissionsClick(Sender: TObject);
var
  dbIndex: Integer;
  Form: TfmUserPermissions;
  List: TStringList;
  UserName: string;
  i: Integer;
  Permission: string;
  ObjType: Integer;
  ObjTypeName: string;
  ObjName: string;
  ATab: TTabSheet;
  Title: string;
begin
  dbIndex:= tvMain.Selected.Parent.Parent.OverlayIndex;
  UserName:= tvMain.Selected.Text;
  List:= TStringList.Create;
  List.CommaText:= dmSysTables.GetUserObjects(dbIndex, UserName);
  Title:= 'Permissions for: ' + UserName;

  Form:= FindCusomForm(Title, TfmUserPermissions) as TfmUserPermissions;
  if Form = nil then
  begin
    Form:= TfmUserPermissions.Create(Application);
    ATab:= TTabSheet.Create(nil);
    ATab.Parent:= PageControl1;
    Form.Parent:= ATab;
    Form.Caption:= Title;
    ATab.Caption:= Form.Caption;
    Form.Left:= 0;
    Form.Top:= 0;
    Form.BorderStyle:= bsNone;
    Form.Align:= alClient;
  end
  else
    ATab:= Form.Parent as TTabSheet;

  ATab.Tag:= dbIndex;
  PageControl1.ActivePage:= ATab;
  Form.StringGrid1.RowCount:= 1;
  Form.laObject.Caption:= UserName;
  with Form do
  for i:= 0 to List.Count - 1 do
  begin
    ObjName:= List[i];
    if Pos('<G>', ObjName) = 1 then
      Delete(ObjName, 1, 3);
    Permission:= dmSysTables.GetObjectUserPermission(dbIndex, ObjName, UserName, ObjType);
    StringGrid1.RowCount:= StringGrid1.RowCount + 1;

    case ObjType of
      0: ObjTypeName:= 'Table/View';
      5: ObjTypeName:= 'Procedure';
      13: ObjTypeName:= 'Role';
    else
      ObjTypeName:= IntToStr(ObjType);
    end;
    StringGrid1.Cells[0, i + 1]:= ObjTypeName;
    StringGrid1.Cells[1, i + 1]:= ObjName;
    StringGrid1.Cells[2, i + 1]:= Permission;
  end;
  Form.Show;
  List.Free;
end;

(***********  Refresh Click  *************)

procedure TfmMain.lmRefreshClick(Sender: TObject);
begin
  if tvMain.Selected.Expanded then
    tvMain.Selected.Collapse(False);
  tvMainExpanded(nil, tvMain.Selected)
end;

procedure TfmMain.lmRegdbClick(Sender: TObject);
begin
  fmReg.edDatabaseName.Text:= tvMain.Selected.Text + ':';
  mnRegDBClick(nil);
end;

procedure TfmMain.lmRestoreClick(Sender: TObject);
begin
  fmBackupRestore.Init('', tvMain.Selected.Text +  ':', '', '');
  fmBackupRestore.cbOperation.ItemIndex:= 1;
  fmBackupRestore.cbOperation.Enabled:= False;
  fmBackupRestore.meLog.Clear;
  fmBackupRestore.Show;
end;

procedure TfmMain.lmRolePerManagementClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  ATab: TTabSheet;
  Title: string;
  dbIndex: Integer;
begin
  Title:= 'Permission management for: ' + tvMain.Selected.Text;
  fmPermissions:= FindCusomForm(Title, TfmPermissionManage) as TfmPermissionManage;
  if fmPermissions = nil then
  begin
    fmPermissions:= TfmPermissionManage.Create(nil);
    ATab:= TTabSheet.Create(nil);
    ATab.Parent:= PageControl1;
    fmPermissions.Parent:= ATab;
    fmPermissions.Left:= 0;
    fmPermissions.Top:= 0;
    fmPermissions.BorderStyle:= bsNone;
    fmPermissions.Align:= alClient;
    ATab.Caption:= Title;
  end
  else
    ATab:= fmViewGen.Parent as TTabSheet;
  PageControl1.ActivePage:= ATab;
  dbIndex:= tvMain.Selected.Parent.Parent.OverlayIndex;
  ATab.Tag:= dbIndex;
  fmPermissions.Init(dbIndex, '', tvMain.Selected.Text, 2);
  fmPermissions.Show;
end;

procedure TfmMain.lmRolePermissionsClick(Sender: TObject);
begin
  lmPermissionsClick(Sender);
end;

(***********  Script Database  ************)

procedure TfmMain.lmScriptDatabaseClick(Sender: TObject);
var
  QueryWindow: TfmQueryWindow;
  List: TStringList;
  dbIndex: Integer;
begin
  dbIndex:= tvMain.Selected.OverlayIndex;
  QueryWindow:= ShowQueryWindow(dbIndex, 'Database Script');
  List:= TStringList.Create;
  try
    Screen.Cursor:= crSQLWait;
    Application.ProcessMessages;
    with QueryWindow.meQuery do
    begin
      ClearAll;
      Lines.Add('-- ' + tvMain.Selected.Text + ' database script. Generated on: ' + DateTimeToStr(Now) );

      Lines.Add('');
      Lines.Add('--      Roles');
      Lines.Add('');
      ScriptAllRoles(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Functions (UDF)');
      Lines.Add('');
      ScriptAllFunctions(dbIndex, List);
      List.Text:= StringReplace(List.Text, #10, #13#10, [rfReplaceAll]);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--     Domains');
      Lines.Add('');
      ScriptAllDomains(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Generators');
      Lines.Add('');
      ScriptAllGenerators(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Tables');
      ScriptAllTables(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Stored Procedures');
      Lines.Add('');
      ScriptAllProcedureTemplates(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('/*      Views  */');
      Lines.Add('');
      ScriptAllViews(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Triggers');
      Lines.Add('');
      ScriptAllTriggers(dbIndex, List);
      Lines.AddStrings(List);


      Lines.Add('');
      Lines.Add('--      Secondary Indices');
      Lines.Add('');
      ScriptAllSecIndices(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Constraints');
      Lines.Add('');
      ScriptAllConstraints(dbIndex, List);
      Lines.AddStrings(List);

      Lines.Add('');
      Lines.Add('--      Permissions');
      Lines.Add('');
      ScriptAllPermissions(dbIndex, List);
      Lines.AddStrings(List);
      Lines.Add('');

    end;
    QueryWindow.Show;

  except
  on e: exception do
  begin
    Screen.Cursor:= crDefault;
    ShowMessage(e.Message);
  end;

  end;
  Screen.Cursor:= crDefault;
  List.Free;
end;

(**************  Script Exception  ****************)

procedure TfmMain.lmScriptExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Script, Msg, Desc: string;
begin
  SelNode:= tvMain.Selected;
  if dmSysTables.GetExceptionInfo(SelNode.Text, Msg, Desc, Script) then
    ShowCompleteQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Script Exception ' + SelNode.Text, Script, nil);
end;

(**************  Script table as Insert stored procedure ************)

procedure TfmMain.lmScriptInsertClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  i: Integer;
  ConstraintsList: TStringList;
  PKFieldsList: TStringList;
  FieldLine: string;
  FieldNames: string;
  ParamNames: string;
  Skipped: Boolean;
  dbIndex: Integer;
  LastParam: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'Script Table as insert : ' + ATableName);
    GetFields(dbIndex, ATableName, nil);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure InsertTo' + ATableName + ' (');

    // Params
    FieldNames:= '';
    ParamNames:= '';
    with SQLQuery1 do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('Computed_Source').AsString = '') and
          ((Pos('CHAR', Trim(FieldByName('Field_Type_Str').AsString)) = 0) or
          (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
          (FieldByName('Field_Collation').IsNull)) then
      begin
        FieldNames:= FieldNames + Trim(FieldByName('Field_Name').AsString);
        ParamNames:= ParamNames + ':' + Trim(FieldByName('Field_Name').AsString);
        FieldLine:= Trim(FieldByName('Field_Name').AsString) + ' ';
        FieldLine:= FieldLine + Trim(FieldByName('Field_Type_Str').AsString);
        if Pos('char', LowerCase(FieldByName('Field_Type_Str').AsString)) > 0 then
          FieldLine:= FieldLine + '(' + FieldByName('Field_Length').AsString + ') ';
      end
      else
        Skipped:= True;

      Next;

      if not Skipped then
      begin
        if not EOF then
        begin
          FieldLine:= FieldLine + ',';
          FieldNames:= FieldNames + ', ';
          ParamNames:= ParamNames + ', ';
        end;
        QWindow.meQuery.Lines.Add(FieldLine);
      end;

    end;
    SQLQuery1.Close;

    // Remote last , if any
    if RightStr(FieldNames, 2) = ', ' then
    begin
      System.Delete(FieldNames, Length(FieldNames) - 1, 2);
      System.Delete(ParamNames, Length(ParamNames) - 1, 2);
    end;

    // Remove last , if any
    LastParam:= QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1];
    if Pos(',', LastParam) > 0 then
    begin
      LastParam:= StringReplace(LastParam, ',', '', []);
      QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1]:= LastParam;
    end;

    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('insert into ' + ATableName + ' (' + FieldNames + ')');
    QWindow.meQuery.Lines.Add('values (' + ParamNames + ');');
    QWindow.meQuery.Lines.Add('end;');

    QWindow.Show;

  end;
end;

(********  Script table as Create  ***********)

procedure TfmMain.lmScriptTableCreateClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  dbIndex: Integer;
  ScriptList: TStringList;
  Line: string;
  PKName: string;
  ConstraintName: string;
  List: TStringList;
  i: Integer;
  UserName: string;
  ObjType: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    ScriptList:= TStringList.Create;
    ScriptTableAsCreate(dbIndex, ATableName, ScriptList);
    QWindow:= ShowQueryWindow(dbIndex, 'Script Table as Create: ' + ATableName);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.AddStrings(ScriptList);

    // Script table constraints
    dmSysTables.sqQuery.Close;
    SQLQuery1.Close;
    dmSysTables.GetTableConstraints(ATableName, dmSysTables.sqQuery);
    with dmSysTables do
    while not sqQuery.EOF do
    begin
       Line:= 'alter table ' + ATableName + ' add constraint ' + sqQuery.Fields[0].AsString +
         ' foreign key (' + sqQuery.Fields[3].AsString + ') references ' +  sqQuery.Fields[4].AsString  +
         ' (' + dmSysTables.GetConstraintForiegnKeyFields(sqQuery.Fields[5].AsString, fmMain.SQLQuery1) + ') ';
       if Trim(sqQuery.Fields[6].AsString) <> 'RESTRICT' then
         Line:= Line + ' on update ' + Trim(sqQuery.Fields[6].AsString);
       if Trim(sqQuery.Fields[7].AsString) <> 'RESTRICT' then
         Line:= Line + ' on delete ' + Trim(sqQuery.Fields[7].AsString);
       QWindow.meQuery.Lines.Add(Line + ';');
       sqQuery.Next;
    end;
    dmSysTables.sqQuery.Close;
    SQLQuery1.Close;
    QWindow.meQuery.Lines.Add('');

    // Script Secondary indices
    PKName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
    List:= TStringList.Create;

    with dmSysTables do
    if fmMain.GetIndices(ATableName, sqQuery) then
    with sqQuery do
    while not EOF do
    begin
      if PKName <> Trim(FieldByName('RDB$Index_name').AsString) then
      begin
        Line:= 'create ';
        if FieldByName('RDB$Unique_Flag').AsString = '1' then
          Line:= Line + 'Unique ';
        if FieldByName('RDB$Index_Type').AsString = '1' then
          Line:= Line + 'Descending ';

        Line:= Line + 'index ' + Trim(FieldByName('RDB$Index_name').AsString) + ' on ' + ATableName;

        GetIndexFields(ATableName, Trim(FieldByName('RDB$Index_Name').AsString), fmMain.SQLQuery1, List);
        Line:= Line + ' (' + List.CommaText + ') ;';
        QWindow.meQuery.Lines.Add(Line);

      end;
      Next;
    end;
    QWindow.meQuery.Lines.Add('');
    SQLQuery1.Close;
    dmSysTables.sqQuery.Close;

    // Script triggers
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'SELECT RDB$Trigger_Name, RDB$Trigger_Inactive FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 ' +
      'and RDB$Relation_Name = ''' + aTableName + '''';
    SQLQuery1.Open;
    with SQLQuery1 do
    while not EOF do
    begin
      List.Clear;
      dmSysTables.ScriptTrigger(dbIndex, Trim(SQLQuery1.Fields[0].AsString), List, True);
      // Search for generators
      Line:= '';
      for i:= 0 to List.Count - 1 do
        if Pos('gen_id', LowerCase(List[i])) > 0 then
        begin
          Line:= Copy(List[i], Pos('gen_id', LowerCase(List[i])), Length(List[i]));
          System.Delete(Line, 1, Pos('(', Line));
          Line:= Trim(Copy(Line, 1, Pos(', ', Line) - 1));
        end;

       // Script Generator
       if Trim(Line) <> '' then
       begin
         QWindow.meQuery.Lines.Add('Create Generator ' +Line + ';');
         QWindow.meQuery.Lines.Add('');
       end;

      QWindow.meQuery.Lines.AddStrings(List);


      Next;
    end;
    SQLQuery1.Close;

    QWindow.meQuery.Lines.Add('');

    // Script permissions
    List.CommaText:= dmSysTables.GetDBUsers(dbIndex);

    for i:= 0 to List.Count - 1 do
    begin
      if Pos('<R>', List[i]) = 1 then
        UserName:= Copy(List[i], 4, Length(List[i]) - 3)
      else
        UserName:= List[i];

      ScriptObjectPermission(dbIndex, '<T>' + ATableName, UserName, ObjType, QWindow.meQuery.Lines);
    end;


    ScriptList.Free;
    List.Free;
    QWindow.Show;
  end;
end;

(*****************  Script as Update table stored proc  ****************)

procedure TfmMain.lmScriptUpdateClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  i: Integer;
  PKFieldsList: TStringList;
  FieldLine: string;
  ParamAndValue: string;
  AFieldName: string;
  WhereClause: string;
  Skipped: Boolean;
  PKeyName: string;
  dbIndex: Integer;
  ConstraintName: string;
  LastParam: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'Script Table as update: ' + ATableName);
    GetFields(dbIndex, ATableName, nil);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure Update' + ATableName + ' (');

    // Params
    ParamAndValue:= '';
    with SQLQuery1 do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('Computed_Source').AsString = '') and
         ((Pos('CHAR', FieldByName('Field_Type_Str').AsString) = 0) or
          (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
          (FieldByName('Field_Collation').IsNull)) then
      begin
        AFieldName:= Trim(SQLQuery1.FieldByName('Field_Name').AsString);
        ParamAndValue:= ParamAndValue + AFieldName + ' = :' + AFieldName;
        FieldLine:= AFieldName + ' ';
        FieldLine:= FieldLine + Trim(FieldByName('Field_Type_Str').AsString);
        if Pos('char', LowerCase(FieldByName('Field_Type_Str').AsString)) > 0 then
          FieldLine:= FieldLine + '(' + FieldByName('Field_Length').AsString + ') ';
      end
      else
        Skipped:= True;
      Next;

      if not Skipped then
      begin
        if not EOF then
        begin
          FieldLine:= FieldLine + ',';
          ParamAndValue:= ParamAndValue + ', ';
        end;
        QWindow.meQuery.Lines.Add(FieldLine);

      end;
    end;

    // Remote last , if any
    if RightStr(ParamAndValue, 2) = ', ' then
      Delete(ParamAndValue, Length(ParamAndValue) - 1, 2);
    SQLQuery1.Close;

    // Primary Keys
    WhereClause:= '';
    PKFieldsList:= TStringList.Create;
    PKeyName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
    if PKeyName <> '' then
    begin
      GetConstraintFields(ATableName, PKeyName, PKFieldsList);
      for i:= 0 to PKFieldsList.Count - 1 do
      begin
        WhereClause:= WhereClause + PKFieldsList[i] + ' = :' + PKFieldsList[i];
        if i < PKFieldsList.Count - 1 then
          WhereClause:= WhereClause + ' and ';
      end;
    end;

    // Remove last , if any
    LastParam:= QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1];
    if Pos(',', LastParam) > 0 then
    begin
      LastParam:= StringReplace(LastParam, ',', '', []);
      QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1]:= LastParam;
    end;

    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('update ' + ATableName);
    QWindow.meQuery.Lines.Add('set ' + ParamAndValue);
    QWindow.meQuery.Lines.Add('where ' + WhereClause + ';');
    QWindow.meQuery.Lines.Add('END;');

    QWindow.Show;

  end;
end;

(******************  Set generator value  *********************)

procedure TfmMain.lmSetGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  AGenName: string;
  OrigValue: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    Rec:= RegisteredDatabases[dbIndex];
    SetConnection(dbIndex);

    AGenName:= SelNode.Text;
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'select GEN_ID(' + AGenName + ', 0) from RDB$Database;';

    SQLQuery1.Open;
    OrigValue:= SQLQuery1.Fields[0].AsString;
    SQLQuery1.Close;

    ShowCompleteQueryWindow(dbIndex, 'set generator value', 'set generator ' + AGenName + ' to ' + OrigValue);
  end;

end;

(*************   Sweep Database   ***********)

procedure TfmMain.lmSweepClick(Sender: TObject);
var
  FireBirdServices: TFirebirdServices;
  Res: Ansistring;
  ADatabase: string;
  dbIndex: Integer;
  AdbName: string;
  Lines: string;
  s: string;
begin
  dbIndex:= tvMain.Selected.OverlayIndex;
  FireBirdServices:= TFirebirdServices.Create;
  FireBirdServices.VerboseOutput:= True;
  with FireBirdServices, RegisteredDatabases[dbIndex] do
  begin
    HostName:= GetServerName(RegRec.DatabaseName);
    AdbName:= RegRec.DatabaseName;
    if Pos(':', AdbName) > 2 then
      Delete(AdbName, 1, Pos(':', AdbName));
    DBName:= AdbName;
    UserName := RegRec.UserName;
    Password := RegRec.Password;

    try
      AttachService;
      StartSweep;
      while ServiceQuery(S) do
        Lines:= Lines + S;

      ShowMessage('Sweep database: ' + AdbName + ' Completed');

    except
    on e: exception do
    begin
      MessageDlg('Error: ' + e.Message, mtError, [mbOK], 0);
    end;
    end;
    DetachService;
  end;
  FireBirdServices.Free;

end;


(*************  Table management  ****************)

procedure TfmMain.lmTableManageClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
  fmTableManage: TfmTableManage;
  ATab: TTabSheet;
  Title: string;
begin
  try
    SelNode:= tvMain.Selected;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;

    Title:= RegisteredDatabases[dbIndex].RegRec.Title +  ': Management of : ' + SelNode.Text;
    // Fields
    fmTableManage:= FindCusomForm(Title, TfmTableManage) as TfmTableManage;
    if fmTableManage = nil then
    begin
      fmTableManage:= TfmTableManage.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmTableManage.Parent:= ATab;
      fmTableManage.Left:= 0;
      fmTableManage.Top:= 0;
      fmTableManage.Align:= alClient;
      fmTableManage.BorderStyle:= bsNone;
    end
    else
      ATab:= fmTableManage.Parent as TTabSheet;

    PageControl1.ActivePage:= ATab;
    fmTableManage.Caption:= Title;
    ATab.Caption:= Title;
    ATab.Tag:= dbIndex;
    fmTableManage.Init(dbIndex, SelNode.Text);
    fmTableManage.PageControl1.TabIndex:= 0;
    ViewTableFields(SelNode.Text, dbIndex, fmTableManage.sgFields);

    // Indices
    ShowIndicesManagement(fmTableManage, dbIndex, SelNode.Text);

    // Constraints
    FillAndShowConstraintsForm(fmTableManage, SelNode.Text, dbIndex);

    // Triggers
    fmTableManage.ViewTriggers;

    // Permissions
    fmTableManage.FillPermissions;

    fmTableManage.Show;

  except
  on e: exception do
    MessageDlg('Error while opening Table Management: ' + e.Message, mtError, [mbOk], 0);
  end;
end;

procedure TfmMain.lmUserPermManagementClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
begin
  lmRolePerManagementClick(nil);
  fmPermissions:= TfmPermissionManage.Create(nil);
  fmPermissions.Init(tvMain.Selected.Parent.Parent.OverlayIndex, '', tvMain.Selected.Text, 1);
  fmPermissions.Show;
end;

(**********  View Domain info ************)

procedure TfmMain.lmViewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  ADomainName: string;
  DomainType: string;
  DomainSize: Integer;
  ADomainForm: TFmViewDomain;
  DefaultValue: string;
  ATab: TTabSheet;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ADomainName:= SelNode.Text;
    //ADomainForm:= TfmViewDomain(FindCusomForm('Domain : ' + ADomainName, TfmViewDomain));
    //if ADomainForm  = nil then
    begin
      ADomainForm:= TfmViewDomain.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      ADomainForm.Parent:= ATab;
      ADomainForm.Left:= 0;
      ADomainForm.Top:= 0;
      ADomainForm.BorderStyle:= bsNone;
      ADomainForm.Align:= alClient;
      PageControl1.ActivePage:= ATab;
    end;

    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    dmSysTables.GetDomainInfo(dbIndex, ADomainName, DomainType, DomainSize, DefaultValue);
    ATab.Tag:= dbIndex;
    if Pos('default', LowerCase(DefaultValue)) = 1 then
      DefaultValue:= Trim(Copy(DefaultValue, 8, Length(DefaultValue)));
    if Pos('CHAR', DomainType) > 0 then
      DomainType:= DomainType + '(' + IntToStr(DomainSize) + ')';

    // Fill ViewDomain form
    with ADomainForm do
    begin
      Caption:= 'Domain : ' + ADomainName;
      ATab.Caption:= Caption;
      edName.Caption:= ADomainName;
      laType.Caption:= DomainType;
      laSize.Caption:= IntToStr(DomainSize);
      laDefault.Caption:= DefaultValue;
    end;
    ADomainForm.Show;
  end;
end;


(********************  Get Fields  **************************)

procedure TfmMain.GetFields(DatabaseIndex: Integer; ATableName: string; FieldsList: TStringList);
var
  Rec: TDatabaseRec;
  FieldName: string;
begin
  SQLQuery1.Close;
  Rec:= RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);
  SQLQuery1.SQL.Text:= 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
      '  r.RDB$DESCRIPTION AS field_description, ' +
      '  r.RDB$DEFAULT_SOURCE AS field_default_value, ' +
      '  r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
      '  f.RDB$FIELD_LENGTH AS field_length, ' +
      '  f.RDB$FIELD_PRECISION AS field_precision, ' +
      '  f.RDB$FIELD_SCALE AS field_scale, ' +
      '  f.RDB$FIELD_TYPE as Field_Type_Int, ' +
      '  CASE f.RDB$FIELD_TYPE ' +
      '    WHEN 261 THEN ''BLOB'' ' +
      '    WHEN 14 THEN ''CHAR'' ' +
      '    WHEN 40 THEN ''CSTRING''  ' +
      '    WHEN 11 THEN ''D_FLOAT'' ' +
      '    WHEN 27 THEN ''DOUBLE Precision'' ' +
      '    WHEN 10 THEN ''FLOAT'' ' +
      '    WHEN 16 THEN ''BIGINT'' ' +
      '    WHEN 8 THEN ''INTEGER'' ' +
      '    WHEN 9 THEN ''QUAD'' ' +
      '    WHEN 7 THEN ''SMALLINT'' ' +
      '    WHEN 12 THEN ''DATE'' ' +
      '    WHEN 13 THEN ''TIME'' ' +
      '    WHEN 35 THEN ''TIMESTAMP'' ' +
      '    WHEN 37 THEN ''VARCHAR'' ' +
      '    ELSE ''UNKNOWN'' ' +
      '  END AS field_type_Str, ' +
      '  f.RDB$FIELD_SUB_TYPE AS field_subtype, ' +
      '  coll.RDB$COLLATION_NAME AS field_collation, ' +
      '  cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
      ' f.RDB$COMPUTED_Source AS Computed_Source ' +
      ' FROM RDB$RELATION_FIELDS r ' +
      ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID ' +
      ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
      ' WHERE r.RDB$RELATION_NAME=''' + ATableName + '''  ' +
      ' ORDER BY r.RDB$FIELD_POSITION;';

    SQLQuery1.Open;
    if FieldsList <> nil then
    begin
      FieldsList.Clear;
      while not SQLQuery1.EOF do
      begin
        FieldName:= Trim(SQLQuery1.FieldByName('field_name').AsString);
        if FieldsList.IndexOf(FieldName) = -1 then
          FieldsList.Add(FieldName);
        SQLQuery1.Next;
      end;
      SQLQuery1.First;
    end;
end;

(**********  Get Stored Proc body  ****************)

function TfmMain.GetStoredProcBody(DatabaseIndex: Integer; AProcName: string; var SPOwner: string): string;
var
  Rec: TDatabaseRec;
  i: Integer;
  Line: string;
  ParamName: string;
  FirstOutput: Boolean;
  ParamType: Byte;
  Seperator: Boolean;
  BodyList: TStringList;
begin
  try
    AProcName:= UpperCase(AProcName);
    BodyList:= TStringList.Create;
    Rec:= RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'SELECT rdb$parameter_name, rdb$field_type, rdb$field_sub_type, '+
      'rdb$field_length, rdb$field_scale, rdb$field_precision, '+
      'rdb$character_length, rdb$parameter_type '+
      'FROM rdb$procedure_parameters sp_param '+
      'JOIN rdb$fields fld '+
      'ON sp_param.rdb$field_source = fld.rdb$field_name '+
      'WHERE '+
      'sp_param.rdb$procedure_name =''' + AProcName + ''' ' +
      'order by rdb$parameter_type, rdb$parameter_number';

    SQLQuery1.Open;
    FirstOutput:= False;

      while not SQLQuery1.EOF do
      begin
        ParamName:= Trim(SQLQuery1.FieldByName('RDB$Parameter_Name').AsString);
        ParamType:= SQLQuery1.FieldByName('rdb$parameter_type').AsInteger;
        Seperator:= False;
        // Output parameter
        if (not FirstOutput) and (ParamType = 1) then
        begin
          BodyList.Add(')' + #10 + 'RETURNS (');
          FirstOutput:= True;
          Seperator:= True;
        end;
        Line:= '  ' + ParamName + '    ' +
          GetFBTypeName(SQLQuery1.FieldByName('RDB$Field_Type').AsInteger);
        if SQLQuery1.FieldByName('RDB$Field_Type').AsInteger = 37 then
          Line:= Line + '(' + SQLQuery1.FieldByName('RDB$Field_Length').AsString + ')';


        SQLQuery1.Next;

        if (not SQLQuery1.EOF) then
        if ((FirstOutput) or (SQLQuery1.FieldByName('rdb$parameter_Type').AsInteger = 0)) then
          Line:= Line + ',';

        BodyList.Add(Line);
      end;

    BodyList.Add(')');
    BodyList.Add('AS');

    SQLQuery1.Close;

    // Procedure body
    SQLQuery1.SQL.Text:= 'SELECT * FROM rdb$procedures where rdb$Procedure_name =  ''' + AProcName + '''';
    SQLQuery1.Open;
    SPOwner:= Trim(SQLQuery1.FieldByName('rdb$Owner_Name').AsString);
    BodyList.Add(SQLQuery1.FieldByName('rdb$Procedure_Source').AsString);
    SQLQuery1.Close;
    Result:= BodyList.Text;
    BodyList.Free;

  except
  on e: exception do
    MessageDlg('Error while getting stored procedure information: ' + e.Message, mtError, [mbOk], 0);
  end;
end;

(******************  Get View Info (SQL Source) ***************)

function TfmMain.GetViewInfo(DatabaseIndex: Integer; AViewName: string; var Columns, Body: string): Boolean;
var
  Rec: TDatabaseRec;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  // View Body
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'SELECT RDB$VIEW_SOURCE ' +
    'FROM RDB$RELATIONS ' +
    'WHERE RDB$VIEW_SOURCE IS NOT NULL ' +
    'AND UPPER(RDB$RELATION_NAME) = ''' + UpperCase(AViewName) + ''';';

  SQLQuery1.Open;
  Body:= SQLQuery1.Fields[0].AsString;

  // View Columns
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'SELECT d.RDB$DEPENDENT_NAME AS view_name, '+
    'r.RDB$FIELD_NAME AS field_name, '+
    'd.RDB$DEPENDED_ON_NAME AS depended_on_table, '+
    'd.RDB$FIELD_NAME AS depended_on_field '+
    'FROM RDB$DEPENDENCIES d '+
    'LEFT JOIN RDB$RELATION_FIELDS r ON d.RDB$DEPENDENT_NAME = r.RDB$RELATION_NAME '+
    '     AND d.RDB$FIELD_NAME = r.RDB$BASE_FIELD '+
    'WHERE UPPER(d.RDB$DEPENDENT_NAME)=''' + UpperCase(AViewName) + ''' '+
    '  AND r.RDB$SYSTEM_FLAG = 0 '+
    '  AND d.RDB$DEPENDENT_TYPE = 1 '+
    'ORDER BY r.RDB$FIELD_POSITION ';
  Columns:= '';
  SQLQuery1.Open;
  while not SQLQuery1.EOF do
  begin
    Columns:= Columns + Trim(SQLQuery1.FieldByName('Field_Name').AsString);
    SQLQuery1.Next;
    if not SQLQuery1.EOF then
      Columns:= Columns + ', ';
  end;
  SQLQuery1.Close;
  Result:= True;
end;


(************  Change Trigger activity  *************)

function TfmMain.ChangeTriggerActivity(DatabaseIndex: Integer;
  ATriggerName: string; ActiveState: Boolean): Boolean;
var
  Rec: TDatabaseRec;
  ActiveStr: string;
begin
  try
    Rec:= RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);

    SQLQuery1.Close;
    if ActiveState then
      ActiveStr:= 'Active'
    else
      ActiveStr:= 'InActive';
    SQLQuery1.SQL.Text:= 'alter trigger '+ ATriggerName + ' ' + ActiveStr;

    SQLQuery1.ExecSQL;
    Result:= True;
    SQLTransaction.Commit;
    AddToSQLHistory(Rec.RegRec.Title, 'DDL', SQLQuery1.SQL.Text);

  except
  on e: exception do
  begin
    ShowMessage('Error: ' + e.Message);
    Result:= False;
  end;
  end;
end;

(***************  Get Index fields  *******************)

function TfmMain.GetIndexFields(ATableName, AIndexName: string;
  AQuery: TSQLQuery; var FieldsList: TStringList): Boolean;
begin
  AQuery.Close;
  AQuery.SQL.Text:= 'SELECT RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS field_name, ' + #10 +
     'RDB$INDICES.RDB$DESCRIPTION AS description, ' +#10 +
     '(RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION + 1) AS field_position ' +#10 +
     'FROM RDB$INDEX_SEGMENTS ' +#10 +
     'LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' +#10 +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' +#10 +
     ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)=''' + UpperCase(ATablename) + '''         -- table name ' +#10 +
     '  AND UPPER(RDB$INDICES.RDB$INDEX_NAME)=''' + UpperCase(AIndexName) + ''' -- index name ' +#10 +
     '--  AND RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE IS NULL ' +#10 +
     'ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION;';
  AQuery.Open;
  Result:= AQuery.FieldCount > 0;
  FieldsList.Clear;
  if Result then
  while not AQuery.EOF do
  begin
    FieldsList.Add(Trim(AQuery.FieldByName('field_name').AsString));
    AQuery.Next;
  end;
  if not Result then
    AQuery.Close;
end;

(***********  Get UDF Info  ***************)

function TfmMain.GetUDFInfo(DatabaseIndex: Integer; UDFName: string;
  var ModuleName, EntryPoint, Params: string): Boolean;
var
  Rec: TDatabaseRec;
begin
  try
    Rec:= RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);

    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'SELECT * FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = ''' + UDFName + '''';
    SQLQuery1.Open;
    ModuleName:= Trim(SQLQuery1.FieldByName('RDB$MODULE_NAME').AsString);
    EntryPoint:= Trim(SQLQuery1.FieldByName('RDB$ENTRYPOINT').AsString);

    // input Params
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'SELECT * FROM RDB$FUNCTION_ARGUMENTS where RDB$FUNCTION_Name = ''' +
     UDFName + ''' and RDB$MECHANISM = 1';
    SQLQuery1.Open;
    Params:= '';
    while not SQLQuery1.EOF do
    begin
      Params:= Params + #10 + GetFBTypeName(SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger);
      if SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger in [14, 37, 40] then
        Params:= Params + '(' + SQLQuery1.FieldByName('RDB$FIELD_LENGTH').AsString + ')';
      SQLQuery1.Next;
      if not SQLQuery1.EOF then
        Params:= Params + ', ';
    end;
    SQLQuery1.Close;
    Params:= Params + ')' + #10 + #10 + 'Returns ';

    // Result Params
    SQLQuery1.SQL.Text:= 'SELECT * FROM RDB$FUNCTION_ARGUMENTS where RDB$FUNCTION_Name = ''' +
     UDFName + ''' and RDB$MECHANISM = 0';
    SQLQuery1.Open;
    while not SQLQuery1.EOF do
    begin
      Params:= Params + #10 + GetFBTypeName(SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger);
      if SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger in [14, 37, 40] then
        Params:= Params + '(' + SQLQuery1.FieldByName('RDB$FIELD_LENGTH').AsString + ')';
      SQLQuery1.Next;
      if not SQLQuery1.EOF then
        Params:= Params + ', ';
    end;
    SQLQuery1.Close;
    Result:= True;

  except
  on e: exception do
  begin
    ShowMessage(e.Message);
    IBConnection.Close;
    Result:= False;
  end;

  end;
end;

(***********  Show Query window  ************)

function TfmMain.ShowQueryWindow(DatabaseIndex: Integer; ATitle: string): TfmQueryWindow;
var
  Rec: TDatabaseRec;
  ATab: TTabSheet;
  ACaption: string;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];
  ACaption:= Rec.RegRec.Title + ': ' + ATitle;
  Result:= TfmQueryWindow(FindQueryWindow(ACaption));
  if Result = nil then
  begin
    Result:= TfmQueryWindow.Create(Application);
    ATab:= TTabSheet.Create(nil);
    ATab.Parent:= PageControl1;
    ATab.Caption:= ACaption;
    Result.Parent:= ATab;
    Result.Left:= 0;
    Result.Top:= 0;
    Result.Align:= alClient;
    Result.Font.Name:= 'Arial';
  end
  else
    ATab:= Result.Parent as TTabSheet;

  Result.Init(DatabaseIndex);
  ATab.Tag:= DatabaseIndex;
  Result.Caption:= ACaption;
  Result.Parent.Show;
  Result.BorderStyle:= bsNone;
  OpenSQLHistory(Rec.RegRec.Title);
  Result.Show;
  fmMain.Show;
end;

(******* Fill Object Root, like (Tables, Views, etc)  ******)

procedure TfmMain.FillObjectRoot(Node: TTreeNode);
var
  Rec: TRegisteredDatabase;
  Objects: TStringList;
  TableNode, Item, GenNode, TrigNode, ViewsNode :TTreeNode;
  StoredProcNode, UDFNode, SysTableNode, DomainsNode, ExceptionNode: TTreeNode;
  RoleNode, UserNode: TTreeNode;
  i: Integer;
  DBIndex: Integer;
  Count: Integer;
  ANodeText: string;
begin
  DBIndex:= Node.Parent.OverlayIndex;
  Rec:= RegisteredDatabases[DBIndex].RegRec;
  Objects:= TStringList.Create;

  ANodeText:= Node.Text;
  if Pos('(', ANodeText) > 0 then
    ANodeText:= Trim(Copy(ANodeText, 1, Pos('(', ANodeText) - 1));

  // Tables
  if ANodeText = 'Tables' then
  begin
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 1, Count);
    TableNode:= Node;
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';

    TableNode.DeleteChildren;

    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(TableNode, Objects[i]);
      Item.ImageIndex:= 4;
      Item.SelectedIndex:= 4;
    end;

  end
  else
    // Generators
  if ANodeText = 'Generators' then
  begin
    GenNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 2, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    GenNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(GenNode, Objects[i]);
      Item.ImageIndex:= 6;
      Item.SelectedIndex:= 6;
    end;

  end
  else
    // Triggers
  if Node.Text = 'Triggers' then
  begin
    TrigNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 3, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    TrigNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(TrigNode, Objects[i]);
      Item.ImageIndex:= 8;
      Item.SelectedIndex:= 8;
    end;

  end
  else
    // Views
  if Node.Text = 'Views' then
  begin
    ViewsNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 4, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    ViewsNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(ViewsNode, Objects[i]);
      Item.ImageIndex:= 10;
      Item.SelectedIndex:= 10;
    end;

  end
  else
    // Stored Procedures
  if Node.Text = 'Stored Procedures' then
  begin
    StoredProcNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 5, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    StoredProcNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(StoredProcNode, Objects[i]);
      Item.ImageIndex:= 12;
      Item.SelectedIndex:= 12;
    end;

  end
  else
    // UDF (Functions)
  if Node.Text = 'Functions' then
  begin
    UDFNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 6, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    UDFNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(UDFNode, Objects[i]);
      Item.ImageIndex:= 14;
      Item.SelectedIndex:= 14;
    end;

  end
  else
    // System Tables
  if Node.Text = 'System Tables' then
  begin
    SysTableNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 7, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    SysTableNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(SysTableNode, Objects[i]);
      Item.ImageIndex:= 16;
      Item.SelectedIndex:= 16;
    end;

  end
  else
    // Domains
  if Node.Text = 'Domains' then
  begin
    DomainsNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 8, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    DomainsNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(DomainsNode, Objects[i]);
      Item.ImageIndex:= 18;
      Item.SelectedIndex:= 18;
    end;

  end
  else
    // Roles
  if Node.Text = 'Roles' then
  begin
    RoleNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 9, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    RoleNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(RoleNode, Objects[i]);
      Item.ImageIndex:= 20;
      Item.SelectedIndex:= 20;
    end;
  end
  else
    // Exceptions
  if Node.Text = 'Exceptions' then
  begin
    ExceptionNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 10, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    ExceptionNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(ExceptionNode, Objects[i]);
      Item.ImageIndex:= 22;
      Item.SelectedIndex:= 22;
    end;
  end
  else
    // Users
  if Node.Text = 'Users' then
  begin
    UserNode:= Node;
    Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, 11, Count);
    Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
    UserNode.DeleteChildren;
    for i:= 0 to Objects.Count - 1 do
    begin
      Item:= tvMain.Items.AddChild(UserNode, Objects[i]);
      Item.ImageIndex:= 24;
      Item.SelectedIndex:= 24;
    end;
  end;

  if not Node.Expanded then
    Node.Expand(False);
  Objects.Free;

end;

(*************  Get main indices information  ******************)

function TfmMain.GetIndices(ATableName: string; AQuery: TSQLQuery): Boolean;
begin
  AQuery.Close;
  AQuery.SQL.Text:= 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';
  AQuery.Open;
  Result:= AQuery.RecordCount > 0;
  if not Result then
    AQuery.Close;


end;

(***************  View Table Fields/ Fields Management  ***************)

procedure TfmMain.ViewTableFields(ATableName: string; dbIndex: Integer;
  AStringGrid: TStringGrid);
var
  Rec: TDatabaseRec;
  QWindow: TfmQueryWindow;
  i: Integer;
  PKFieldsList: TStringList;
  DefaultValue: string;
  PKeyName: string;
  ConstraintName: string;
begin
  try
    GetFields(dbIndex, ATableName, nil);

    // Fill TableInfo grid
    AStringGrid.RowCount:= 1;
    with AStringGrid, SQLQuery1 do
    while not EOF do
    begin
      if (Pos('CHAR', Trim(FieldByName('Field_Type_Str').AsString)) = 0) or
       (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
       (FieldByName('Field_Collation').IsNull) then
      begin
        RowCount:= RowCount + 1;

        // Field Name
        Cells[1, RowCount - 1]:= Trim(FieldByName('Field_Name').AsString);

        // Field Type
        if FieldByName('Field_Type_Int').AsInteger in [7, 8, 16] then
          Cells[2, RowCount - 1]:= GetNumericFieldType(FieldByName('Field_Type_Int').AsInteger,
            FieldByName('Field_SubType').AsInteger, FieldByName('Field_Length').AsInteger,
            FieldByName('Field_Scale').AsInteger)
        else
          Cells[2, RowCount - 1]:= Trim(FieldByName('Field_Type_Str').AsString);

        // Computed fields (Calculated)
        if FieldByName('Computed_Source').AsString <> '' then
          Cells[2, RowCount - 1]:= FieldByName('Computed_Source').AsString;

        // Field Size
        Cells[3, RowCount - 1]:= FieldByName('Field_Length').AsString;

        // Null/Not null
        if FieldByName('field_not_null_constraint').AsString = '1' then
          Cells[4, RowCount - 1]:= '0'
        else
          Cells[4, RowCount - 1]:= '1';

        // Default Value
        DefaultValue:= FieldByName('Field_Default_Value').AsString;
        if Pos('default', DefaultValue) > 0 then
          DefaultValue:= Trim(StringReplace(DefaultValue, 'default', '', []));
        Cells[5, RowCount - 1]:= DefaultValue;

        Cells[6, RowCount - 1]:= FieldByName('Field_Description').AsString;
      end;
      Next;

    end;
    SQLQuery1.Close;

    // Primary Keys
    PKFieldsList:= TStringList.Create;
    PKeyName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
    if PKeyName <> '' then
      GetConstraintFields(ATableName, PKeyName, PKFieldsList);

    with AStringGrid do
    for i:= 1 to RowCount - 1 do
      if PKFieldsList.IndexOf(Cells[1, i]) <> -1 then
        Cells[0, i]:= '1'
      else
        Cells[0, i]:= '0';

    PKFieldsList.Free;

  except
  on e: exception do
    MessageDlg('Error while reading table fields: ' + e.Message, mtError, [mbOk], 0);

  end;
end;


(*************   Display View DDL *******************)

procedure TfmMain.lmDisplayViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  QWindow: TfmQueryWindow;
  AViewName: string;
  ViewBody, Columns: string;
  dbIndex: Integer;
  ATab: TTabSheet;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    Rec:= RegisteredDatabases[dbIndex];
    AViewName:= SelNode.Text;

    // Fill ViewView grid
    ATab:= TTabSheet.Create(nil);
    ATab.Parent:= PageControl1;
    fmViewView:= TfmViewView.Create(nil);
    fmViewView.Parent:= ATab;
    fmViewView.Left:= 0;
    fmViewView.Top:= 0;
    fmViewView.BorderStyle:= bsNone;
    fmViewView.Align:= alClient;
    fmViewView.SynSQLSyn1.TableNames.CommaText:= GetTableNames(dbIndex);
    fmViewView.Caption:= 'View DDL: ' + AViewName;
    ATab.Caption:= fmViewView.Caption;
    fmViewView.edName.Caption:= AViewName;
    ATab.Tag:= dbIndex;

    GetViewInfo(dbIndex, AViewName, Columns, ViewBody);
    fmViewView.seScript.Lines.Clear;
    fmViewView.seScript.Lines.Text:= 'create view "' + AviewName + '" (' + Columns + ')' + #13#10 + ViewBody;
    PageControl1.ActivePage:= ATab;
    fmViewView.Show;
  end;

end;

(***************  ExpandFields: Expand table fields  ************)

procedure TfmMain.lmViewFieldsClick(Sender: TObject);
var
  Node: TTreeNode;
  dbIndex: Integer;
  FieldsList: TStringList;
  FieldTitle: string;
  FieldNode: TTreeNode;
  PKFieldsList: TStringList;
  PKeyName: string;
  ConstraintName: string;
  AFieldName: string;
  i: Integer;
begin
  try
    Node:= tvMain.Selected;
    dbIndex:= Node.Parent.Parent.OverlayIndex;
    Node.DeleteChildren;

    // Primary Keys
    PKFieldsList:= TStringList.Create;
    PKeyName:= GetPrimaryKeyIndexName(dbIndex, Node.Text, ConstraintName);
    if PKeyName <> '' then
      GetConstraintFields(Node.Text, PKeyName, PKFieldsList);

    // Fields
    FieldsList:= TStringList.Create;
    GetFields(dbIndex, Node.Text, nil);
    i:= 1;
    with SQLQuery1 do
    while not EOF do
    begin
      AFieldName:= Trim(FieldByName('Field_Name').AsString);
      if (Pos('CHAR', Trim(FieldByName('Field_Type_Str').AsString)) = 0) or
       (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
       (FieldByName('Field_Collation').IsNull) then
       begin
        FieldTitle:= AFieldName + '   ' + Trim(FieldByName('Field_Type_str').AsString) +
          ' ' + FieldByName('Field_Length').AsString;
        FieldNode:= tvMain.Items.AddChild(Node, FieldTitle);
        FieldNode.OverlayIndex:= i;
        if PKFieldsList.IndexOf(AFieldname) <> -1 then // Primary key
        begin
          FieldNode.ImageIndex:= 28;
          FieldNode.SelectedIndex:= 28;
        end
        else
        begin
          FieldNode.ImageIndex:= 27;
          FieldNode.SelectedIndex:= 27;
        end;
        Inc(i);

       end;
      Next;
    end;
    SQLQuery1.Close;
    Node.Expand(False);
    PKFieldsList.Free;

  except
  on e: exception do
    ShowMessage(E.Message);
  end;

end;


(***************  View Generator  *****************)

procedure TfmMain.lmViewGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  AGenName: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    Rec:= RegisteredDatabases[dbIndex];
    SQLQuery1.Close;
    SetConnection(dbIndex);
    AGenName:= SelNode.Text;
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'select GEN_ID(' + AGenName + ', 0) from RDB$Database;';

    SQLQuery1.Open;

    // Fill ViewGen form
    Title:= 'Generator : ' + AGenName;
    fmViewGen:= FindCusomForm(Title, TfmViewGen) as TfmViewGen;
    if fmViewGen = nil then
    begin
      fmViewGen:= TfmViewGen.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmViewGen.Parent:= ATab;
      fmViewGen.Left:= 0;
      fmViewGen.Top:= 0;
      fmViewGen.BorderStyle:= bsNone;
      fmViewGen.Align:= alClient;
    end
    else
      ATab:= fmViewGen.Parent as TTabSheet;
    PageControl1.ActivePage:= ATab;
    ATab.Tag:= dbIndex;

    with fmViewGen do
    begin
      Caption:= Title;
      ATab.Caption:= Caption;
      edGenName.Caption:= AGenName;
      edValue.Caption:= SQLQuery1.Fields[0].AsString;
    end;
    ATab.Caption:= Title;
    fmViewGen.Show;
  end;

end;

(*******************  view Stored Procedure  ****************************)

procedure TfmMain.lmViewStoredProcedureClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AProcName: string;
  SPOwner: string;
  spBody: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AProcName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    SPBody:= GetStoredProcBody(dbIndex, AProcName, SPOwner);
    Title:= SelNode.Parent.Parent.Text +  ': StoredProcedure : ' + AProcName;
    // Fill SProc Parameters
    fmViewSProc:= FindCusomForm(Title, TfmViewSProc) as TfmViewSProc;
    if fmViewSProc = nil then
    begin
      fmViewSProc:= TfmViewSProc.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmViewSProc.Parent:= ATab;
      fmViewSProc.Left:= 0;
      fmViewSProc.Top:= 0;
      fmViewSProc.BorderStyle:= bsNone;
      fmViewSProc.Align:= alClient;
    end
    else
      ATab:= fmViewSProc.Parent as TTabSheet;
    PageControl1.ActivePage:= ATab;
    with fmViewSProc do
    begin
      SynSQLSyn1.TableNames.CommaText:= GetTableNames(dbIndex);
      Caption:= Title;
      ATab.Caption:= Caption;
      ATab.Tag:= dbIndex;
      edName.Caption:= AProcName;
      seScript.Lines.Clear;
      seScript.Lines.Add('create procedure ' + AProcName + '(');
      edOwner.Caption:= SPOwner;

      // Procedure body
      seScript.Lines.Text:= seScript.Lines.Text + spBody;

      fmViewSProc.Show;
    end; // with fmViewSProc
  end;

end;


(*******************  View Trigger   **********************)

procedure TfmMain.lmViewTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  ATriggerName: string;
  Event: string;
  TriggerEnabled: Boolean;
  Body: string;
  BeforeAfter: string;
  OnTable: string;
  TriggerPosition: Integer;
  ATab: TTabSheet;
  Title: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATriggerName:= SelNode.Text;
    Title:= SelNode.Parent.Parent.Text +  ': Trigger : ' + ATriggerName;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    dmSysTables.GetTriggerInfo(dbIndex, ATriggerName, BeforeAfter, OnTable,
      Event, Body, TriggerEnabled, TriggerPosition);

    // Fill ViewTrigger form
    fmViewTrigger:= FindCusomForm(Title, TfmViewTrigger) as TfmViewTrigger;
    if fmViewTrigger = nil then
    begin
      fmViewTrigger:= TfmViewTrigger.Create(Application);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmViewTrigger.Parent:= ATab;
      fmViewTrigger.Left:= 0;
      fmViewTrigger.Top:= 0;
      fmViewTrigger.BorderStyle:= bsNone;
      fmViewTrigger.Align:= alClient;
    end
    else
      ATab:= fmViewTrigger.Parent as TTabSheet;

    PageControl1.ActivePage:= ATab;
    ATab.Tag:= dbIndex;
    with fmViewTrigger do
    begin
      Caption:= Title;
      ATab.Caption:= Caption;
      edName.Caption:= ATriggerName;
      edOnTable.Caption:= OnTable;
      laEvent.Caption:= Event;
      laType.Caption:= BeforeAfter;
      laPos.Caption:= IntToStr(TriggerPosition);
      seScript.Lines.Text:= Body;
      if TriggerEnabled then
      begin
        laEnabled.Caption:= 'Yes';
        laEnabled.Font.Color:= clGreen;
      end
      else
      begin
        laEnabled.Caption:= 'No';
        laEnabled.Font.Color:= clRed;
      end;
    end;
    fmViewTrigger.Show;
  end;

end;

(********  View UDF  **********)

procedure TfmMain.lmViewUDFClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AFuncName: string;
  ModuleName, EntryPoint: string;
  Params: string;
  ATab: TTabSheet;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AFuncName:= SelNode.Text;
    dbIndex:= SelNode.Parent.Parent.OverlayIndex;
    if GetUDFInfo(dbIndex, AFuncName, ModuleName, EntryPoint, Params) then
    with fmUDFINfo do
    begin
      fmUDFInfo:= TfmUDFInfo.Create(nil);
      ATab:= TTabSheet.Create(nil);
      ATab.Parent:= PageControl1;
      fmUDFInfo.Parent:= ATab;
      ATab.Tag:= dbIndex;
      fmUDFInfo.Left:= 0;
      fmUDFInfo.Top:= 0;
      fmUDFInfo.BorderStyle:= bsNone;
      fmUDFInfo.Align:= alClient;
      PageControl1.ActivePage:= ATab;
      Caption:= 'Function : ' + AFuncName;
      ATab.Caption:= Caption;
      edName.Caption:= AFuncName;
      edModule.Caption:= ModuleName;
      edEntry.Caption:= EntryPoint;
      meBody.Clear;
      meBody.Lines.Add('function ' + AFuncName + '(');
      meBody.Lines.Add(Params);
      fmUDFInfo.Show;
    end; // with fmUDFInfo
  end;
end;

(********  Create new database  ********)

procedure TfmMain.mnCreateDBClick(Sender: TObject);
begin
  if fmCreateDB.ShowModal = mrOk then
    LoadRegisteredDatabases;
end;

(**********  Register New database  ***********)

procedure TfmMain.mnRegDBClick(Sender: TObject);
begin
  fmReg.NewReg:= True;
  fmReg.bbReg.Caption:= 'Register';
  if fmReg.ShowModal = mrOK then
  begin
    LoadRegisteredDatabases;
    fmReg.SaveRegistrations;
    LoadRegisteredDatabases;
  end;
end;

(**********  About  ****************)

procedure TfmMain.MenuItem6Click(Sender: TObject);
begin
  fmAbout:= TfmAbout.Create(nil);
  fmAbout.Init;
  fmAbout.Show;
end;

(************* Edit Registration  *************)

procedure TfmMain.lmEditRegClick(Sender: TObject);
var
  Rec: TRegisteredDatabase;
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if SelNode <> nil then
  begin
    fmReg.NewReg:= False;
    fmReg.bbReg.Caption:= 'Save';
    fmreg.RecPos:= RegisteredDatabases[SelNode.OverlayIndex].Index;

    Rec:= RegisteredDatabases[SelNode.OverlayIndex].OrigRegRec;
    fmReg.edDatabaseName.Text:= Rec.DatabaseName;
    fmReg.edTitle.Text:= Rec.Title;
    fmReg.edUserName.Text:= Rec.UserName;
    fmReg.edPassword.Text:= Rec.Password;
    fmReg.cbCharset.Text:= Rec.Charset;
    fmReg.edRole.Text:= Rec.Role;
    fmReg.cxSavePassword.Checked:= Rec.SavePassword;

    if fmReg.ShowModal = mrOK then
    begin
      LoadRegisteredDatabases;
      fmReg.SaveRegistrations;
      LoadRegisteredDatabases;
    end;
  end;
end;


(****************  Unregister database *************)

procedure TfmMain.lmUnregisterDatabaseClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) and (SelNode.Parent.Parent = nil) then
  if MessageDlg('Are you sure you want to Unregister this database', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DeleteRegistration(RegisteredDatabases[SelNode.OverlayIndex].Index);
    LoadRegisteredDatabases;
  end;
  SelNode:= nil;
end;

(**********  View 1000 records  **************)

procedure TfmMain.lmViewFirst1000Click(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.Parent.OverlayIndex, 'Select first 1000 from ' + SelNode.Text);
    QWindow.meQuery.Lines.Text:= 'select first 1000 * from ' + SelNode.Text;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

(***********  Create New Role   ************)

procedure TfmMain.lmNewRoleClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(SelNode.Parent.OverlayIndex, 'Create new Role');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE ROLE role_name;');
    QWindow.Show;
  end;
end;

procedure TfmMain.mnRestoreClick(Sender: TObject);
begin
  fmBackupRestore.Init('', '', '', '');
  fmBackupRestore.cbOperation.ItemIndex:= 1;
  fmBackupRestore.cbOperation.Enabled:= False;
  fmBackupRestore.meLog.Clear;
  fmBackupRestore.Show;
end;

procedure TfmMain.PageControl1CloseTabClicked(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TfmQueryWindow then
    begin
       (Application.Components[i] as TfmQueryWindow).lmCloseTabClick(nil);
       Break;
    end;
end;

(*****************   Database Popup menu   ********************)

procedure TfmMain.pmDatabasePopup(Sender: TObject);
var
  SelNode: TTreeNode;
  Filter: Integer;
  i: Integer;
  ParentNodeText: string;
  NodeText: string;

begin
  SelNode:= tvMain.Selected;

  if SelNode <> nil then
  begin
    NodeText:= SelNode.Text;
    if Pos('(', NodeText) > 0 then
      NodeText:= Trim(Copy(NodeText, 1, Pos('(', NodeText) - 1));

    ParentNodeText:= '';
    if SelNode.Parent <> nil then
      ParentNodeText:= SelNode.Parent.Text;
    if Pos('(', ParentNodeText) > 0 then
      ParentNodeText:= Trim(Copy(ParentNodeText, 1, Pos('(', ParentNodeText) - 1));

    if (SelNode <> nil) then
    if (SelNode.Parent = nil) then // Servers
      Filter:= -2
    else
    if (SelNode.Parent.Parent = nil) then // Database
      Filter:= 0
    else
    if ParentNodeText = 'Tables' then // Tables
      Filter:= 1
    else
    if ParentNodeText = 'Generators' then // Generators
      Filter:= 2
    else
    if ParentNodeText = 'Triggers' then // Triggers
      Filter:= 3
    else
    if ParentNodeText = 'Views' then // View
      Filter:= 4
    else
    if ParentNodeText = 'Stored Procedures' then // Stored Proc
      Filter:= 5
    else
    if ParentNodeText = 'Functions' then // UDF
      Filter:= 6
    else
    if ParentNodeText = 'System Tables' then // System Tables
      Filter:= 7
    else
    if ParentNodeText = 'Domains' then // Domains
      Filter:= 8
    else
    if ParentNodeText = 'Roles' then // Roles
      Filter:= 9
    else
    if ParentNodeText = 'Exceptions' then // Roles
      Filter:= 10
    else
    if ParentNodeText = 'Users' then // Users
      Filter:= 111
    else
    if NodeText = 'Tables' then // Tables root              //  Higher level (Roots)
      Filter:= 11
    else
    if NodeText = 'Generators' then // Generators root
      Filter:= 12
    else
    if NodeText = 'Stored Procedures' then // Stored Proc root
      Filter:= 15
    else
    if NodeText = 'Functions' then // UDF root
      Filter:= 16
    else
    if NodeText = 'Views' then // Views root
      Filter:= 14
    else
    if NodeText = 'Triggers' then // Triggers root
      Filter:= 13
    else
    if NodeText = 'Domains' then // Domains root
      Filter:= 18
    else
    if NodeText = 'Roles' then // Roles root
      Filter:= 19
    else
    if NodeText = 'Exceptions' then // Exceptions
      Filter:= 20
    else
    if NodeText = 'Users' then // Users
      Filter:= 21
    else
    if NodeText = 'Query Window' then // Query Window
      Filter:= 30
    else
      Filter:= -1;

    // Table Fields
    if (SelNode.Level = 4) then
    begin
      ParentNodeText:= SelNode.Parent.Parent.Text;
      if Pos('(', ParentNodeText) > 0 then
        ParentNodeText:= Trim(Copy(ParentNodeText, 1, Pos('(', ParentNodeText) - 1));
      if (ParentNodeText = 'Tables') then
        Filter:= 112;
    end;


  end
  else
    Filter:= -1;

  // Show menu for specific filter
  for i:= 0 to pmDatabase.Items.Count - 1 do
    pmDatabase.Items[i].Visible:= (pmDatabase.Items[i].Tag = Filter) or
      ((pmDatabase.Items[i].Tag = 100) and (SelNode <> nil) and (SelNode.Parent <> nil) and
      (SelNode.Parent.Parent <> nil) and (SelNode.Parent.Parent.Parent = nil));


  SelNode:= nil;
end;


(**********************            Double click        *********************************)

procedure TfmMain.tvMainDblClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  Rec: TRegisteredDatabase;
  Node: TTreeNode;
  ParentText: string;
begin
  Node:= tvMain.Selected;
  if Node.Level = 1 then // Database level, Fill objects
  begin
    // Do nothing
  end
  else
  if Node.Level = 2 then // Objects Type Level
  begin
    if tvMain.Selected.Text = 'Query Window' then
    begin
      QWindow:= ShowQueryWindow(tvMain.Selected.Parent.OverlayIndex, 'Query Window');
      QWindow.Show;
    end
    else  // Expand object
    begin
      tvMainExpanded(nil, Node);
      Rec:= RegisteredDatabases[Node.OverlayIndex].RegRec;
    end;
  end
  else
  if Node.Level = 3 then  // Object Item Level, like some tables, procedures.
  begin
    ParentText:= Node.Parent.Text;
    if Pos('(', ParentText) > 0 then
    ParentText:= Trim(Copy(ParentText, 1, Pos('(', ParentText) - 1));

    if ParentText = 'Tables' then
    begin
      lmViewFieldsClick(nil);
      lmViewFirst1000Click(nil);
    end
    else
    if ParentText = 'Generators' then
      lmViewGenClick(nil)
    else
    if ParentText = 'Triggers' then
      lmViewTriggerClick(nil)
    else
    if ParentText = 'Views' then
      lmDisplay1000VClick(nil)
    else
    if ParentText = 'Stored Procedures' then
      lmViewStoredProcedureClick(nil)
    else
    if ParentText = 'Functions' then
      lmViewUDFClick(nil)
    else
    if ParentText = 'System Tables' then
      lmOpenSystemTableClick(nil)
    else
    if ParentText = 'Domains' then
      lmViewDomainClick(nil)
    else
    if ParentText = 'Roles' then
      lmPermissionsClick(nil)
    else
    if ParentText = 'Exceptions' then
      lmScriptExceptionClick(nil)
    else
    if ParentText = 'Users' then
      lmPermissionsClick(nil)

  end
  else
  if Node.Level = 4 then // Table fields (Edit)
    lmEditFieldClick(nil)
end;

(**************    Expanded     *****************)

procedure TfmMain.tvMainExpanded(Sender: TObject; Node: TTreeNode);
var
  Rec: TRegisteredDatabase;
  Count: Integer;
begin
  if (Node <> nil) then
  if (Node.Parent <> nil) and (Node.Parent.Parent = nil) then   // Expand database
  begin
    Rec:= RegisteredDatabases[Node.OverlayIndex].RegRec;
    RegisteredDatabases[Node.OverlayIndex].RegRec.LastOpened:= Now;
    RegisteredDatabases[Node.OverlayIndex].OrigRegRec.LastOpened:= Now;
    // Password form
    if Rec.Password = '' then
    begin
      fmEnterPass.edUser.Text:= Rec.UserName;
      fmEnterPass.cbRole.Text:= Rec.Role;
      fmEnterPass.edPassword.Clear;
      try
        fmEnterPass.cbRole.Items.CommaText:= dmSysTables.GetDBObjectNames(Node.OverlayIndex, 9, Count);
      except
      end;

      if fmEnterPass.ShowModal = mrOk then
      begin
        if fmReg.TestConnection(Rec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
          Rec.Charset) then
          begin
           { Self.ibConnection.Close;
            Self.ibConnection.UserName:= fmEnterPass.edUser.Text;
            Self.ibConnection.Password:= fmEnterPass.edPassword.Text;}

            RegisteredDatabases[Node.OverlayIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
            RegisteredDatabases[Node.OverlayIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
            RegisteredDatabases[Node.OverlayIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
            Node.Expand(False);
          end
          else
          begin
            Exit;
          end;
      end
      else
      begin
        Node.Collapse(False);
        Exit;
      end;
    end;
  end
  else  // Expand objects root (Tables, Procedures, etc)
  if (Node.Parent <> nil) and (Node.Parent.Parent <> nil) and (Node.Parent.Parent.Parent = nil) and (not Node.Expanded) then
  begin
    if Node.HasChildren then
    begin
      Node.DeleteChildren;
      Node.Text:= Trim(Copy(Node.Text, 1, Pos('(', Node.Text) - 1));
    end;
    FillObjectRoot(Node);
  end;
end;

(**********************             Load databases            *********************************)

function TfmMain.LoadRegisteredDatabases: Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  MainNode, CNode: TTreeNode;
  i: Integer;
  AServerName: string;
  ServerNode: TTreeNode;
begin
  try
    tvMain.Items.Clear;
    ReleaseRegisteredDatabases;
    FileName:= ChangeFileExt(ParamStr(0), '.reg');
    AssignFile(F, FileName);
    if FileExists(FileName) then
    begin
      Reset(F);
      i:= 0;
      while not Eof(F) do
      begin
        Read(F, Rec);
        if not Rec.Deleted then
        begin
          SetLength(RegisteredDatabases, Length(RegisteredDatabases) + 1);
          with RegisteredDatabases[high(RegisteredDatabases)] do
          begin
            RegRec:= Rec;
            OrigRegRec:= Rec;
            Index:= FilePos(F) - 1;
            IBConnection:= TIBConnection.Create(nil);
            SQLTrans:= TSQLTransaction.Create(nil);
            IBConnection.Transaction:= SQLTrans;
            SQLTrans.DataBase:= IBConnection;
            IBConnection.DatabaseName:= Rec.DatabaseName;
            IBConnection.UserName:= Rec.UserName;
            IBConnection.Password:= Rec.Password;
            IBConnection.Role:= Rec.Role;
            IBConnection.CharSet:= Rec.Charset;
          end;

          // Server node
          AServerName:= GetServerName(Rec.DatabaseName);

          ServerNode:= GetServerNameNode(AServerName);
          if ServerNode = nil then // Add new Server node
          begin
            tvMain.Items.Add(nil, '');
            ServerNode:= tvMain.Items.Add(nil, AServerName);
            ServerNode.ImageIndex:= 25;
            ServerNode.SelectedIndex:= 26;
          end;

          // Put databases
          MainNode:= tvMain.Items.AddChild(ServerNode, Rec.Title);
          MainNode.ImageIndex:= 0;
          MainNode.SelectedIndex:= 3;
          MainNode.OverlayIndex:= i;
          tvMain.PopupMenu:= pmDatabase;

          CNode:= tvMain.Items.AddChild(MainNode, 'Query Window');
          CNode.ImageIndex:= 1;
          CNode.SelectedIndex:= 1;
          CNode:= tvMain.Items.AddChild(MainNode, 'Tables');
          CNode.ImageIndex:= 2;
          CNode.SelectedIndex:= 2;

          CNode:= tvMain.Items.AddChild(MainNode, 'Generators');
          CNode.ImageIndex:= 5;
          CNode.SelectedIndex:= 5;

          CNode:= tvMain.Items.AddChild(MainNode, 'Triggers');
          CNode.ImageIndex:= 7;
          CNode.SelectedIndex:= 7;

          CNode:= tvMain.Items.AddChild(MainNode, 'Views');
          CNode.ImageIndex:= 9;
          CNode.SelectedIndex:= 9;

          CNode:= tvMain.Items.AddChild(MainNode, 'Stored Procedures');
          CNode.ImageIndex:= 11;
          CNode.SelectedIndex:= 11;

          CNode:= tvMain.Items.AddChild(MainNode, 'Functions');
          CNode.ImageIndex:= 13;
          CNode.SelectedIndex:= 13;

          CNode:= tvMain.Items.AddChild(MainNode, 'System Tables');
          CNode.ImageIndex:= 15;
          CNode.SelectedIndex:= 15;

          CNode:= tvMain.Items.AddChild(MainNode, 'Domains');
          CNode.ImageIndex:= 17;
          CNode.SelectedIndex:= 17;

          CNode:= tvMain.Items.AddChild(MainNode, 'Roles');
          CNode.ImageIndex:= 19;
          CNode.SelectedIndex:= 19;

          CNode:= tvMain.Items.AddChild(MainNode, 'Exceptions');
          CNode.ImageIndex:= 21;
          CNode.SelectedIndex:= 21;

          CNode:= tvMain.Items.AddChild(MainNode, 'Users');
          CNode.ImageIndex:= 23;
          CNode.SelectedIndex:= 23;

          Inc(i);

        end;
      end;
      CloseFile(F);

      // Add spaces at end of tree
      tvMain.Items.Add(nil, '');
      tvMain.Items.Add(nil, '');
      tvMain.Items.Add(nil, '');
    end;
    Result:= True;

  except
  on e: exception do
  begin
    Result:= False;
    ShowMessage('Error: ' + e.message);
  end;
  end;
end;


(**********************           Find QueryWindow                *********************************)

function TfmMain.FindQueryWindow(ATitle: string): TComponent;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Application.ComponentCount- 1 do
    if Application.Components[i] is TfmQueryWindow then
      if (Application.Components[i] as TfmQueryWindow).Caption = ATitle then
        begin
          Result:= Application.Components[i];
          Break;
        end;
end;

(**********************   Find CustomForm   *********************************)

function TfmMain.FindCusomForm(ATitle: string; AClass: TClass): TComponent;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Application.ComponentCount- 1 do
    if Application.Components[i] is AClass then
      if (Application.Components[i] as TForm).Caption = ATitle then
        begin
          Result:= Application.Components[i];
          Break;
        end;
end;

(****************  Delete Registration   *************************)

function TfmMain.DeleteRegistration(Index: Integer): Boolean;
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
begin
  Result:= False;
    FileName:= ChangeFileExt(ParamStr(0), '.reg');
    AssignFile(F, FileName);
    if FileExists(FileName) then
    begin
      FileMode:= 2;
      Reset(F);
      Seek(F, Index);
      Read(F, Rec);
      Rec.Deleted:= True;
      Seek(F, Index);
      Write(F, Rec);
      CloseFile(F);
      Result:= True;
    end;
end;

(**************  Get Firebird Type name  *****************)

function TfmMain.GetFBTypeName(Index: Integer): string;
begin
  case Index of
     261 : Result:= 'BLOB';
      14 : Result:= 'CHAR';
      40 : Result:= 'CSTRING';
      11 : Result:= 'D_FLOAT';
      27 : Result:= 'DOUBLE Precision';
      10 : Result:= 'FLOAT';
      16 : Result:= 'INT64';
      8  : Result:= 'INTEGER';
      9  : Result:= 'QUAD';
      7  : Result:= 'SMALLINT';
      12 : Result:= 'DATE';
      13 : Result:= 'TIME';
      35 : Result:= 'TIMESTAMP';
      37 : Result:= 'VARCHAR';
  else
    Result:= 'Unknown Type';
  end;
end;

(*******************  Get Primary Key fields  ************************)

function TfmMain.GetPrimaryKeyIndexName(DatabaseIndex: Integer; ATableName: string; var ConstraintName: string): string;
begin
  SQLQuery1.Close;
  SetConnection(DatabaseIndex);
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'select RDB$Index_name, RDB$Constraint_Name from RDB$RELATION_CONSTRAINTS ' +
    'where RDB$Relation_Name = ''' + UpperCase(ATableName) + ''' and RDB$Constraint_Type = ''PRIMARY KEY'' ';
  SQLQuery1.Open;
  if SQLQuery1.RecordCount > 0 then
  begin
    Result:= Trim(SQLQuery1.Fields[0].AsString);
    ConstraintName:= Trim(SQLQuery1.Fields[1].AsString);
  end
  else
    Result:= '';
  SQLQuery1.Close;
end;

(*********  Get constrain fields  *********)

function TfmMain.GetConstraintFields(ATableName, AIndexName: string; var List: TStringList): Boolean;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'SELECT s.RDB$FIELD_NAME AS field_name ' +
     'FROM RDB$INDEX_SEGMENTS s ' +
     'LEFT JOIN RDB$INDICES i ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = refc.RDB$CONST_NAME_UQ ' +
     'LEFT JOIN RDB$INDICES i2 ON i2.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$INDEX_SEGMENTS s2 ON i2.RDB$INDEX_NAME = s2.RDB$INDEX_NAME ' +
     '   WHERE i.RDB$RELATION_NAME=''' + UpperCase(ATableName) + '''  ' +
      'AND rc.RDB$INDEX_NAME=''' + UpperCase(AIndexName) + ''' ' +
      'AND rc.RDB$CONSTRAINT_TYPE IS NOT NULL ' +
      'ORDER BY s.RDB$FIELD_POSITION';
  List.Clear;
  SQLQuery1.Open;
  while not SQLQuery1.EOF do
  begin
    List.Add(Trim(SQLQuery1.Fields[0].AsString));
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
  Result:= List.Count > 0;
end;


function TfmMain.ChangeQueryToBIDirectional(DatabaseIndex: Integer; ATableName: string; sqQuery: TSQLQuery): Boolean;
var
  KeyList, FieldsList: TStringList;
  PKName: string;
  sqPrimaryKey: TSQLQuery;
  i: Integer;
  WhereClause: string;
  ConstraintName: string;
begin
  SetConnection(DatabaseIndex);

  sqQuery.UpdateSQL.Clear;
  sqQuery.DeleteSQL.Clear;
  sqQuery.InsertSQL.Clear;

  KeyList:= TStringList.Create;
  FieldsList:= TStringList.Create;
  try
    PKName:= fmMain.GetPrimaryKeyIndexName(DatabaseIndex, ATableName, ConstraintName);
    Result:= PKName <> '';
    if Result then
    begin
      sqPrimaryKey:= TSQLQuery.Create(nil);
      sqPrimaryKey.DataBase:= IBConnection;
      GetIndexFields(ATableName, PKName, sqPrimaryKey, KeyList);
      GetFields(DatabaseIndex, ATableName, FieldsList);

      // Update SQL
      sqQuery.UpdateSQL.Add('update ' + ATableName + ' set ');
      for i:= 0 to FieldsList.Count - 1 do
        if KeyList.IndexOf(FieldsList[i]) = -1 then
        begin
          sqQuery.UpdateSQL.Add(FieldsList[i] + ' = :' + FieldsList[i]);
          sqQuery.UpdateSQL.Add(',');
        end;

      sqQuery.UpdateSQL.Delete(sqQuery.UpdateSQL.Count - 1); // Delete last comma

      // Key where clause
      WhereClause:= ' where ';
      for i:= 0 to KeyList.Count - 1 do
      begin
        WhereClause:= WhereClause + KeyList[i] + ' = :' + KeyList[i];
        if i + 1 < KeyList.Count then
          WhereClause:= WhereClause + ' and ';
      end;
      sqQuery.UpdateSQL.Add(WhereClause);

      // Insert SQL
      sqQuery.InsertSQL.Add('insert into ' + ATableName + ' (');
      for i:= 0 to FieldsList.Count - 1 do
      begin
        sqQuery.InsertSQL.Add(FieldsList[i]);
        if i < FieldsList.Count - 1 then
          sqQuery.InsertSQL.Add(',')
        else
         sqQuery.InsertSQL.Add(') values (');
      end;

      for i:= 0 to FieldsList.Count - 1 do
      begin
        sqQuery.InsertSQL.Add(':' + FieldsList[i]);
        if i < FieldsList.Count - 1 then
          sqQuery.InsertSQL.Add(',')
        else
          sqQuery.InsertSQL.Add(')');
      end;

      // Delete SQL
      sqQuery.DeleteSQL.Text:= 'delete from ' + ATableName + WhereClause;

      sqPrimaryKey.Free;
    end;

  finally
    KeyList.Free;
    FieldsList.Free;
  end;
end;

(********  Get table names   ********)

function TfmMain.GetTableNames(dbIndex: Integer): string;
var
  Count: Integer;
begin
  Result:= dmSysTables.GetDBObjectNames(dbIndex, 1, Count);
end;

initialization
  {$I main.lrs}

end.

