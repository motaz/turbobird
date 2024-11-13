unit PermissionManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Buttons, CheckLst, turbocommon;

type

  { TfmPermissionManage }

  TfmPermissionManage = class(TForm)
    bbApplyRoles: TBitBtn;
    bbApplyTable: TBitBtn;
    bbApplyProc: TBitBtn;
    bbApplyView: TBitBtn;
    bbClose: TSpeedButton;
    BitBtn1: TBitBtn;
    cbRolesUser: TComboBox;
    cbTables: TComboBox;
    cbViews: TComboBox;
    cbUsers: TComboBox;
    cbProcUsers: TComboBox;
    cbViewsUsers: TComboBox;
    cxViewAll: TCheckBox;
    cxViewAllGrant: TCheckBox;
    cxViewDelete: TCheckBox;
    cxViewDeleteGrant: TCheckBox;
    cxViewInsert: TCheckBox;
    cxViewInsertGrant: TCheckBox;
    cxProcGrant: TCheckBox;
    clbProcedures: TCheckListBox;
    clbRoles: TCheckListBox;
    cxViewReferences: TCheckBox;
    cxViewReferencesGrant: TCheckBox;
    cxRoleGrant: TCheckBox;
    cxSelect: TCheckBox;
    cxInsert: TCheckBox;
    cxDelete: TCheckBox;
    cxReferences: TCheckBox;
    cxAll: TCheckBox;
    cxViewSelect: TCheckBox;
    cxSelectGrant: TCheckBox;
    cxInsertGrant: TCheckBox;
    cxAllGrant: TCheckBox;
    cxViewSelectGrant: TCheckBox;
    cxViewUpdate: TCheckBox;
    cxUpdateGrant: TCheckBox;
    cxDeleteGrant: TCheckBox;
    cxReferencesGrant: TCheckBox;
    cxUpdate: TCheckBox;
    cxViewUpdateGrant: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    tsViews: TTabSheet;
    tsRoles: TTabSheet;
    tsProcedures: TTabSheet;
    tsTables: TTabSheet;
    procedure bbApplyProcClick(Sender: TObject);
    procedure bbApplyRolesClick(Sender: TObject);
    procedure bbApplyTableClick(Sender: TObject);
    procedure bbApplyViewClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure cbProcUsersChange(Sender: TObject);
    procedure cbRolesUserChange(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure cbViewsChange(Sender: TObject);
    procedure cbViewsUsersChange(Sender: TObject);
    procedure clbProceduresClick(Sender: TObject);
    procedure clbProceduresKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clbRolesClick(Sender: TObject);
    procedure clbRolesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxProcGrantChange(Sender: TObject);
    procedure cxRoleGrantChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDBIndex: Integer;
    FProcList: TStringList;
    FRoleList: TStringList;
    FProcGrant: array of Boolean;
    FOrigProcGrant: array of Boolean;
    FRoleGrant: array of Boolean;
    FOrigRoleGrant: array of Boolean;
    FOnCommitProcedure: TNotifyEvent;

    FOldTableSelectGrant: Boolean;
    FOldTableInsertGrant: Boolean;
    FOldTableUpdateGrant: Boolean;
    FOldTableDeleteGrant: Boolean;
    FOldTableReferencesGrant: Boolean;

    procedure UpdatePermissions;
    procedure UpdateViewsPermissions;
    procedure UpdateProcPermissions;
    procedure UpdateRolePermissions;
    procedure ComposeTablePermissionSQL(ATableName: string; OptionName: string; Grant, WithGrant: Boolean; var List: TStringList);
  public
    procedure Init(dbIndex: Integer; ATableName, AUserName: string; UserType: Integer;
      OnCommitProcedure: TNotifyEvent = nil);
    { public declarations }
  end;

var
  fmPermissionManage: TfmPermissionManage;

implementation

{ TfmPermissionManage }

uses SysTables, main;

procedure TfmPermissionManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
  SetLength(FProcGrant, 0);
  SetLength(FOrigProcGrant, 0);
end;

procedure TfmPermissionManage.FormCreate(Sender: TObject);
begin
  FProcList:= TStringList.Create;
  FRoleList:= TStringList.Create;
end;

procedure TfmPermissionManage.FormDestroy(Sender: TObject);
begin
  FProcList.Free;
  FRoleList.Free;
end;

procedure TfmPermissionManage.UpdatePermissions;
var
  Permissions: string;
  ObjType: Integer;
begin
  if (cbUsers.Text <> '') and (cbTables.Text <> '') then
  begin;
    Permissions := dmSysTables.GetObjectUserPermission(FDBIndex, cbTables.Text, cbUsers.Text, ObjType);
    cxAll.Checked:= False;
    cxAllGrant.Checked:= False;

    cxSelect.Checked := Pos('S', Permissions) > 0;
    cxInsert.Checked := Pos('I', Permissions) > 0;
    cxUpdate.Checked := Pos('U', Permissions) > 0;
    cxDelete.Checked := Pos('D', Permissions) > 0;
    cxReferences.Checked:= Pos('R', Permissions) > 0;

    cxSelectGrant.Checked:= Pos('SG', Permissions) > 0;;
    cxInsertGrant.Checked:= Pos('IG', Permissions) > 0;;
    cxUpdateGrant.Checked:= Pos('UG', Permissions) > 0;;
    cxDeleteGrant.Checked:= Pos('DG', Permissions) > 0;;
    cxReferencesGrant.Checked:= Pos('RG', Permissions) > 0;;

    FOldTableSelectGrant:= cxSelectGrant.Checked;
    FOldTableInsertGrant:= cxInsertGrant.Checked;
    FOldTableUpdateGrant:= cxUpdateGrant.Checked;
    FOldTableDeleteGrant:= cxDeleteGrant.Checked;
    FOldTableReferencesGrant:= cxReferencesGrant.Checked;
  end;

end;

procedure TfmPermissionManage.UpdateViewsPermissions;
var
  Permissions: string;
  ObjType: Integer;
begin
  if (cbViewsUsers.Text <> '') and (cbViews.Text <> '') then
  begin;
    Permissions := dmSysTables.GetObjectUserPermission(FDBIndex, cbViews.Text, cbViewsUsers.Text, ObjType);
    cxViewAll.Checked:= False;
    cxViewAllGrant.Checked:= False;

    cxViewSelect.Checked := Pos('S', Permissions) > 0;
    cxViewInsert.Checked := Pos('I', Permissions) > 0;
    cxViewUpdate.Checked := Pos('U', Permissions) > 0;
    cxViewDelete.Checked := Pos('D', Permissions) > 0;
    cxViewReferences.Checked:= Pos('R', Permissions) > 0;

    cxViewSelectGrant.Checked:= Pos('SG', Permissions) > 0;;
    cxViewInsertGrant.Checked:= Pos('IG', Permissions) > 0;;
    cxViewUpdateGrant.Checked:= Pos('UG', Permissions) > 0;;
    cxViewDeleteGrant.Checked:= Pos('DG', Permissions) > 0;;
    cxViewReferencesGrant.Checked:= Pos('RG', Permissions) > 0;;
  end;

end;

procedure TfmPermissionManage.UpdateProcPermissions;
var
  i: Integer;
  Index: Integer;
  ObjName: string;
  ProcIndex: Integer;
begin
  clbProcedures.Clear;
  if cbProcUsers.Text <> '' then
  begin
    clbProcedures.Items.CommaText:= dmSysTables.GetDBObjectsForPermissions(FDBIndex, 5);
    FProcList.Clear;
    FProcList.CommaText:= dmSysTables.GetUserObjects(FDBIndex, cbProcUsers.Text, 5);
    SetLength(FProcGrant, clbProcedures.Count);
    SetLength(FOrigProcGrant, clbProcedures.Count);
    for i:= 0 to FProcList.Count - 1 do
    begin
      ObjName:= FProcList[i];
      if Pos('<G>', ObjName) = 1 then
      begin
        Delete(ObjName, 1, 3);
        FProcList[i]:= ObjName;
        ProcIndex:= clbProcedures.Items.IndexOf(ObjName);
        if ProcIndex <> -1 then
        begin
          FProcGrant[ProcIndex]:= True;
          FOrigProcGrant[ProcIndex]:= True;
        end;
      end;

      Index:= clbProcedures.Items.IndexOf(ObjName);
      if Index <> -1 then
        clbProcedures.Checked[Index]:= True;
    end;

  end;
end;

procedure TfmPermissionManage.UpdateRolePermissions;
var
  i: Integer;
  Index: Integer;
  RoleIndex: Integer;
  Count: Integer;
  ObjName: string;
begin
  clbRoles.Clear;
  if cbRolesUser.Text <> '' then
  begin
    clbRoles.Items.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, otRoles, Count);
    FRoleList.Clear;
    FRoleList.CommaText:= dmSysTables.GetUserObjects(FDBIndex, cbRolesUser.Text, 13);
    SetLength(FRoleGrant, clbRoles.Count);
    SetLength(FOrigRoleGrant, clbRoles.Count);
    for i:= 0 to FRoleList.Count - 1 do
    begin
      ObjName:= FRoleList[i];
      if Pos('<G>', ObjName) = 1 then
      begin
        Delete(ObjName, 1, 3);
        FRoleList[i]:= ObjName;
        RoleIndex:= clbRoles.Items.IndexOf(ObjName);
        if RoleIndex <> -1 then
        begin
          FRoleGrant[RoleIndex]:= True;
          FOrigRoleGrant[RoleIndex]:= True;
        end;
      end;
      Index:= clbRoles.Items.IndexOf(FRoleList[i]);
      if Index <> -1 then
        clbRoles.Checked[Index]:= True;
    end;

  end;

end;

procedure TfmPermissionManage.ComposeTablePermissionSQL(ATableName: string; OptionName: string; Grant, WithGrant: Boolean;
  var List: TStringList);
var
  Line: string;
  ToFrom: string;
  Command: string;
begin
  if Grant then
  begin
    ToFrom:= ' to ';
    Command:= 'grant ';
  end
  else
  begin
    ToFrom:= ' from ';
    Command:= 'revoke ';
  end;

  Line:= Command +  OptionName + ' on ' + ATableName + ToFrom + cbUsers.Text;
  if Grant and WithGrant then
      Line:= Line + ' with grant option';
  Line:= Line + ';';

  if (Grant) and (not WithGrant) then
  begin
    if FOldTableSelectGrant and not cxSelectGrant.Checked and (LowerCase(OptionName) = 'select') then
      Line:= Line + LineEnding + 'REVOKE GRANT OPTION FOR SELECT ON ' + ATableName + ' FROM ' + cbUsers.Text + ';';

    if FOldTableUpdateGrant and not cxUpdateGrant.Checked and (LowerCase(OptionName) = 'update') then
      Line:= Line + LineEnding + 'REVOKE GRANT OPTION FOR Update ON ' + ATableName + ' FROM ' + cbUsers.Text + ';';

    if FOldTableReferencesGrant and not cxReferencesGrant.Checked and (LowerCase(OptionName) = 'references') then
      Line:= Line + LineEnding + 'REVOKE GRANT OPTION FOR References ON ' + ATableName + ' FROM ' + cbUsers.Text + ';';

    if FOldTableDeleteGrant and not cxDeleteGrant.Checked and (LowerCase(OptionName) = 'delete') then
      Line:= Line +LineEnding +  'REVOKE GRANT OPTION FOR Delete ON ' + ATableName + ' FROM ' + cbUsers.Text + ';';

    if FOldTableInsertGrant and not cxInsertGrant.Checked and (LowerCase(OptionName) = 'insert') then
      Line:= Line  +LineEnding +  'REVOKE GRANT OPTION FOR Insert ON ' + ATableName + ' FROM ' + cbUsers.Text + ';';
  end;

  List.Add(Line);
end;

procedure TfmPermissionManage.bbApplyTableClick(Sender: TObject);
var
  List: TStringList;
begin
  if (cbUsers.Text <> '') and (cbTables.ItemIndex <> -1) then
  begin
    List:= TStringList.Create;
    try
      if cxAll.Checked then
        ComposeTablePermissionSQL(cbTables.Text, 'All', cxAll.Checked, cxAllGrant.Checked, List)
      else
      begin
        ComposeTablePermissionSQL(cbTables.Text, 'Select', cxSelect.Checked, cxSelectGrant.Checked, List);
        ComposeTablePermissionSQL(cbTables.Text, 'Insert', cxInsert.Checked, cxInsertGrant.Checked, List);
        ComposeTablePermissionSQL(cbTables.Text, 'Update', cxUpdate.Checked, cxUpdateGrant.Checked, List);
        ComposeTablePermissionSQL(cbTables.Text, 'Delete', cxDelete.Checked, cxDeleteGrant.Checked, List);
        ComposeTablePermissionSQL(cbTables.Text, 'References', cxReferences.Checked, cxReferencesGrant.Checked, List);
      end;

      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Permission for: ' + cbTables.Text, List.Text, FOnCommitProcedure);
    finally
      List.Free;
    end;
    Close;
    Parent.Free;
  end
  else
    ShowMessage('You should enter user/role and a table');
end;

procedure TfmPermissionManage.bbApplyViewClick(Sender: TObject);
var
    List: TStringList;
begin
  if (cbViewsUsers.Text <> '') and (cbViews.ItemIndex <> -1) then
  begin
    List:= TStringList.Create;
    try
      if cxViewAll.Checked then
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'All', cxViewAll.Checked, cxViewAllGrant.Checked, List)
      else
      begin
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'Select', cxViewSelect.Checked, cxViewSelectGrant.Checked, List);
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'Insert', cxViewInsert.Checked, cxViewInsertGrant.Checked, List);
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'Update', cxViewUpdate.Checked, cxViewUpdateGrant.Checked, List);
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'Delete', cxViewDelete.Checked, cxViewDeleteGrant.Checked, List);
        ComposeTablePermissionSQL('"' + cbViews.Text + '"', 'References', cxViewReferences.Checked, cxViewReferencesGrant.Checked, List);
      end;

      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Permission for: ' + cbViews.Text, List.Text, FOnCommitProcedure);
    finally
      List.Free;
    end;
    Close;
    Parent.Free;
  end
  else
    ShowMessage('You should enter user/role and a table');

end;

procedure TfmPermissionManage.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmPermissionManage.BitBtn1Click(Sender: TObject);
begin
  UpdateRolePermissions;
end;

procedure TfmPermissionManage.bbApplyProcClick(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  Line: string;
begin
  if Trim(cbProcUsers.Text) <> '' then
  begin
    List:= TStringList.Create;
    try
      For i:= 0 to clbProcedures.Items.Count - 1 do
      begin
        if clbProcedures.Checked[i] and
          ((FProcList.IndexOf(clbProcedures.Items[i]) = -1) or (FProcGrant[i] and (not FOrigProcGrant[i]))) then // Grant this proc
          begin
            Line:= 'grant execute on procedure ' + clbProcedures.Items[i] + ' to ' + cbProcUsers.Text;
            if FProcGrant[i] then
              Line:= Line + ' with grant option';
            List.Add(Line + ';');

          end;

        if (not clbProcedures.Checked[i]) and (FProcList.IndexOf(clbProcedures.Items[i]) <> -1) then // Remove this proc
          List.Add('Revoke execute on procedure ' + clbProcedures.Items[i] + ' from ' + cbProcUsers.Text + ';');
      end;
      if List.Count > 0 then
      begin
        fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Permission for: ' + cbProcUsers.Text, List.Text, FOnCommitProcedure);
        Close;
        Parent.Free;
      end
      else
        ShowMessage('There is no change');
    finally
      List.Free;
    end;
  end;
end;

procedure TfmPermissionManage.bbApplyRolesClick(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  Line: string;
begin
  if Trim(cbRolesUser.Text) <> '' then
  begin
    List:= TStringList.Create;
    try
      For i:= 0 to clbRoles.Items.Count - 1 do
      begin
        if clbRoles.Checked[i] and
          ((FRoleList.IndexOf(clbRoles.Items[i]) = -1) or (FRoleGrant[i] and (not FOrigRoleGrant[i]))) then // Grant this Role
        begin
          Line:= 'grant ' + clbRoles.Items[i] + ' to ' + cbRolesUser.Text;
          if FRoleGrant[i] then
            Line:= Line + ' with admin option';
          List.Add(Line + ';');
        end;

        if (not clbRoles.Checked[i]) and (FRoleList.IndexOf(clbRoles.Items[i]) <> -1) then // Remove this Role
          List.Add('Revoke ' + clbRoles.Items[i] + ' from ' + cbRolesUser.Text + ';');
      end;

      if List.Count > 0 then
      begin
        fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Permission for: ' + cbRolesUser.Text, List.Text, FOnCommitProcedure);
        Close;
        Parent.Free;
      end
      else
        ShowMessage('There is no change');
    finally
      List.Free;
    end;
  end;
end;

procedure TfmPermissionManage.cbProcUsersChange(Sender: TObject);
begin
  UPdateProcPermissions;
end;

procedure TfmPermissionManage.cbRolesUserChange(Sender: TObject);
begin
  UpdateRolePermissions;
end;

procedure TfmPermissionManage.cbTablesChange(Sender: TObject);
begin
  UpdatePermissions;
end;

procedure TfmPermissionManage.cbViewsChange(Sender: TObject);
begin
  UpdateViewsPermissions;
end;

procedure TfmPermissionManage.cbViewsUsersChange(Sender: TObject);
begin
  UpdateViewsPermissions;
end;

procedure TfmPermissionManage.clbProceduresClick(Sender: TObject);
var
  Index: Integer;
  ProcIndex: Integer;
begin
  Index:= clbProcedures.ItemIndex;
  if Index <> -1 then
  begin
    cxProcGrant.Checked:= FProcGrant[Index];
    cxProcGrant.Caption:= 'With Grant for ' + clbProcedures.Items[Index];
  end;
end;

procedure TfmPermissionManage.clbProceduresKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  clbProceduresClick(nil);
end;

procedure TfmPermissionManage.clbRolesClick(Sender: TObject);
var
  Index: Integer;
  RoleIndex: Integer;
begin
  Index:= clbRoles.ItemIndex;
  if Index <> -1 then
  begin
    cxRoleGrant.Checked:= FRoleGrant[Index];
    cxRoleGrant.Caption:= 'With Admin for ' + clbRoles.Items[Index];
  end;
end;

procedure TfmPermissionManage.clbRolesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  clbRolesClick(nil);
end;

procedure TfmPermissionManage.cxProcGrantChange(Sender: TObject);
var
  Index: Integer;
begin
  Index:= clbProcedures.ItemIndex;
  if Index <> -1 then
    FProcGrant[Index]:= cxProcGrant.Checked;
end;

procedure TfmPermissionManage.cxRoleGrantChange(Sender: TObject);
var
  Index: Integer;
begin
  Index:= clbRoles.ItemIndex;
  if Index <> -1 then
    FRoleGrant[Index]:= cxRoleGrant.Checked;
end;

procedure TfmPermissionManage.Init(dbIndex: integer; ATableName, AUserName: string; UserType: Integer;
  OnCommitProcedure: TNotifyEvent = nil);
var
  Count: integer;
begin
  FOnCommitProcedure:= OnCommitProcedure;

  PageControl1.ActivePageIndex:= 0;
  FDBIndex := dbIndex;
  cbUsers.Text := AUserName;
  cbTables.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otTables, Count);
  cbViews.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otViews, Count);
  cbTables.Text:= ATableName;
  cbProcUsers.Text:= AUserName;
  cbViewsUsers.Text:= AUserName;

  // For users, add roles and users
  cbUsers.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otRoles, Count) + ',' +
    dmSysTables.GetDBObjectNames(dbIndex, otUsers, Count);
  cbProcUsers.Items.CommaText:= cbUsers.Items.CommaText;
  cbViewsUsers.Items.CommaText:= cbUsers.Items.CommaText;

  // Update table permissions
  UpdatePermissions;

  // stored procedures
  UpdateProcPermissions;

  // Roles
  clbRoles.Clear;
  cbRolesUser.Clear;

  if UserType = 1 then
  begin
    cbRolesUser.Text:= AUserName;
    UpdateRolePermissions;
  end;
  cbRolesUser.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otUsers, Count);
end;

initialization
  {$I permissionmanage.lrs}

end.

