unit PermissionManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Buttons, CheckLst;

type

  { TfmPermissionManage }

  TfmPermissionManage = class(TForm)
    bbApplyRoles: TBitBtn;
    bbApplyTable: TBitBtn;
    bbApplyProc: TBitBtn;
    BitBtn1: TBitBtn;
    cbRolesUser: TComboBox;
    cbTables: TComboBox;
    cbUsers: TComboBox;
    cbProcUsers: TComboBox;
    cxProcGrant: TCheckBox;
    clbProcedures: TCheckListBox;
    clbRoles: TCheckListBox;
    cxRoleGrant: TCheckBox;
    cxSelect: TCheckBox;
    cxInsert: TCheckBox;
    cxDelete: TCheckBox;
    cxReferences: TCheckBox;
    cxAll: TCheckBox;
    cxSelectGrant: TCheckBox;
    cxInsertGrant: TCheckBox;
    cxAllGrant: TCheckBox;
    cxUpdateGrant: TCheckBox;
    cxDeleteGrant: TCheckBox;
    cxReferencesGrant: TCheckBox;
    cxUpdate: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PageControl1: TPageControl;
    tsRoles: TTabSheet;
    tsProcedures: TTabSheet;
    tsTables: TTabSheet;
    procedure bbApplyProcClick(Sender: TObject);
    procedure bbApplyRolesClick(Sender: TObject);
    procedure bbApplyTableClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure cbProcUsersChange(Sender: TObject);
    procedure cbRolesUserChange(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure clbProceduresClick(Sender: TObject);
    procedure clbProceduresKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clbRolesClick(Sender: TObject);
    procedure clbRolesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxProcGrantChange(Sender: TObject);
    procedure cxRoleGrantChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    fdbIndex: Integer;
    ProcList: TStringList;
    RoleList: TStringList;
    ProcGrant: array of Boolean;
    OrigProcGrant: array of Boolean;
    RoleGrant: array of Boolean;
    OrigRoleGrant: array of Boolean;
    fOnCommitProcedure: TNotifyEvent;
    procedure UpdatePermissions;
    procedure UpdateProcPermissions;
    procedure UpdateRolePermissions;
    procedure ComposeTablePermissionSQL(OptionName: string; Grant, WithGrant: Boolean; var List: TStringList);
  public
    procedure Init(dbIndex: integer; ATableName, AUserName: string; UserType: Integer;
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
  ProcList.Free;
  RoleList.Free;
  CloseAction:= caFree;
  SetLength(ProcGrant, 0);
  SetLength(OrigProcGrant, 0);
end;

procedure TfmPermissionManage.UpdatePermissions;
var
  Permissions: string;
  ObjType: Integer;
begin
  if (cbUsers.Text <> '') and (cbTables.Text <> '') then
  begin;
    Permissions := dmSysTables.GetObjectUserPermission(fdbIndex, cbTables.Text, cbUsers.Text, ObjType);
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
    clbProcedures.Items.CommaText:= dmSysTables.GetDBObjectsForPermissions(fdbIndex, 5);
    ProcList:= TStringList.Create;
    ProcList.CommaText:= dmSysTables.GetUserObjects(fdbIndex, cbProcUsers.Text, 5);
    SetLength(ProcGrant, clbProcedures.Count);
    SetLength(OrigProcGrant, clbProcedures.Count);
    for i:= 0 to ProcList.Count - 1 do
    begin
      ObjName:= ProcList[i];
      if Pos('<G>', ObjName) = 1 then
      begin
        Delete(ObjName, 1, 3);
        ProcList[i]:= ObjName;
        ProcIndex:= clbProcedures.Items.IndexOf(ObjName);
        if ProcIndex <> -1 then
        begin
          ProcGrant[ProcIndex]:= True;
          OrigProcGrant[ProcIndex]:= True;
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
    clbRoles.Items.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 9, Count);
    RoleList:= TStringList.Create;
    RoleList.CommaText:= dmSysTables.GetUserObjects(fdbIndex, cbRolesUser.Text, 13);
    SetLength(RoleGrant, clbRoles.Count);
    SetLength(OrigRoleGrant, clbRoles.Count);
    for i:= 0 to RoleList.Count - 1 do
    begin
      ObjName:= RoleList[i];
      if Pos('<G>', ObjName) = 1 then
      begin
        Delete(ObjName, 1, 3);
        RoleList[i]:= ObjName;
        RoleIndex:= clbRoles.Items.IndexOf(ObjName);
        if RoleIndex <> -1 then
        begin
          RoleGrant[RoleIndex]:= True;
          OrigRoleGrant[RoleIndex]:= True;
        end;
      end;
      Index:= clbRoles.Items.IndexOf(RoleList[i]);
      if Index <> -1 then
        clbRoles.Checked[Index]:= True;
    end;

  end;

end;

procedure TfmPermissionManage.ComposeTablePermissionSQL(OptionName: string; Grant, WithGrant: Boolean;
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

  Line:= Command +  OptionName + ' on ' + cbTables.Text + ToFrom + cbUsers.Text;
  if Grant and WithGrant then
      Line:= Line + ' with grant option';

  Line:= Line + ';';

  List.Add(Line);
end;

procedure TfmPermissionManage.bbApplyTableClick(Sender: TObject);
var
  List: TStringList;
begin
  if (cbUsers.Text <> '') and (cbTables.ItemIndex <> -1) then
  begin
    List:= TStringList.Create;

    if cxAll.Checked then
      ComposeTablePermissionSQL('All', cxAll.Checked, cxAllGrant.Checked, List)
    else
    begin
      ComposeTablePermissionSQL('Select', cxSelect.Checked, cxSelectGrant.Checked, List);
      ComposeTablePermissionSQL('Insert', cxInsert.Checked, cxInsertGrant.Checked, List);
      ComposeTablePermissionSQL('Update', cxUpdate.Checked, cxUpdateGrant.Checked, List);
      ComposeTablePermissionSQL('Delete', cxDelete.Checked, cxDeleteGrant.Checked, List);
      ComposeTablePermissionSQL('References', cxReferences.Checked, cxReferencesGrant.Checked, List);
    end;

    fmMain.ShowCompleteQueryWindow(fdbIndex, 'Edit Permission for: ' + cbTables.Text, List.Text, fOnCommitProcedure);
    List.Free;
    Close;
  end
  else
    ShowMessage('You should enter user/role and a table');
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
    For i:= 0 to clbProcedures.Items.Count - 1 do
    begin
      if clbProcedures.Checked[i] and
        ((ProcList.IndexOf(clbProcedures.Items[i]) = -1) or (ProcGrant[i] and (not OrigProcGrant[i]))) then // Grant this proc
        begin
          Line:= 'grant execute on procedure ' + clbProcedures.Items[i] + ' to ' + cbProcUsers.Text;
          if ProcGrant[i] then
            Line:= Line + ' with grant option';
          List.Add(Line + ';');

        end;

      if (not clbProcedures.Checked[i]) and (ProcList.IndexOf(clbProcedures.Items[i]) <> -1) then // Remove this proc
        List.Add('Revoke execute on procedure ' + clbProcedures.Items[i] + ' from ' + cbProcUsers.Text + ';');
    end;
    if List.Count > 0 then
    begin
      fmMain.ShowCompleteQueryWindow(fdbIndex, 'Edit Permission for: ' + cbProcUsers.Text, List.Text, fOnCommitProcedure);
      List.Free;
      Close;
    end
    else
      ShowMessage('There is no change');
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
    For i:= 0 to clbRoles.Items.Count - 1 do
    begin
      if clbRoles.Checked[i] and
        ((RoleList.IndexOf(clbRoles.Items[i]) = -1) or (RoleGrant[i] and (not OrigRoleGrant[i]))) then // Grant this Role
      begin
        Line:= 'grant ' + clbRoles.Items[i] + ' to ' + cbRolesUser.Text;
        if RoleGrant[i] then
          Line:= Line + ' with admin option';
        List.Add(Line + ';');
      end;

      if (not clbRoles.Checked[i]) and (RoleList.IndexOf(clbRoles.Items[i]) <> -1) then // Remove this Role
        List.Add('Revoke ' + clbRoles.Items[i] + ' from ' + cbRolesUser.Text + ';');
    end;

    if List.Count > 0 then
    begin
      fmMain.ShowCompleteQueryWindow(fdbIndex, 'Edit Permission for: ' + cbRolesUser.Text, List.Text, fOnCommitProcedure);
      List.Free;
      Close;
    end
    else
      ShowMessage('There is no change');
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

procedure TfmPermissionManage.clbProceduresClick(Sender: TObject);
var
  Index: Integer;
  ProcIndex: Integer;
begin
  Index:= clbProcedures.ItemIndex;
  if Index <> -1 then
  begin
    cxProcGrant.Checked:= ProcGrant[Index];
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
    cxRoleGrant.Checked:= RoleGrant[Index];
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
  Index, ProcIndex: Integer;
begin
  Index:= clbProcedures.ItemIndex;
  if Index <> -1 then
    ProcGrant[Index]:= cxProcGrant.Checked;
end;

procedure TfmPermissionManage.cxRoleGrantChange(Sender: TObject);
var
  Index, RoleIndex: Integer;
begin
  Index:= clbRoles.ItemIndex;
  if Index <> -1 then
    RoleGrant[Index]:= cxRoleGrant.Checked;
end;

procedure TfmPermissionManage.Init(dbIndex: integer; ATableName, AUserName: string; UserType: Integer;
  OnCommitProcedure: TNotifyEvent = nil);
var
  Count: integer;
  StoredProcs: string;
begin
  fOnCommitProcedure:= OnCommitProcedure;
  ProcList:= TStringList.Create;
  RoleList:= TStringList.Create;

  PageControl1.ActivePageIndex:= 0;
  fdbIndex := dbIndex;
  cbUsers.Text := AUserName;
  cbTables.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 1, Count);
  cbTables.Text:= ATableName;
  cbProcUsers.Text:= AUserName;

  cbUsers.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 9, Count) + ',' +
    dmSysTables.GetDBObjectNames(dbIndex, 11, Count);
  cbProcUsers.Items.CommaText:= cbUsers.Items.CommaText;

  // Update table permissions
  UpdatePermissions;

  // stored procedures
  UPdateProcPermissions;

  // Roles
  clbRoles.Clear;
  cbRolesUser.Clear;

  if UserType = 1 then
  begin
    cbRolesUser.Text:= AUserName;
    UpdateRolePermissions;
  end;
  cbRolesUser.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 11, Count);
end;

initialization
  {$I permissionmanage.lrs}

end.
