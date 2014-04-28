unit Scriptdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function ScriptAllRoles(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllFunctions(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllDomains(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllGenerators(dbIndex: Integer; var List: TStringList): Boolean;
// Scripts a single table as CREATE TABLE DDL
procedure ScriptTableAsCreate(dbIndex: Integer; ATableName: string; ScriptList: TStringList);
function ScriptAllTables(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllProcedureTemplates(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllViews(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllTriggers(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllSecIndices(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptAllConstraints(dbIndex: Integer; var List: TStringList): Boolean;
function ScriptObjectPermission(dbIndex: Integer; ObjName, UserName: string; var ObjType: Integer;
   List: TStrings; NewUser: string = ''): Boolean;
function ScriptAllPermissions(dbIndex: Integer; var List: TStringList): Boolean;

function ScriptUserAllPermissions(dbIndex: Integer; UserName: string; var List: TStringList;
   NewUser: string = ''): Boolean;

procedure RemoveParamClosing(var AParams: string);



implementation

{ to do: add support for dependencies when extracting script; otherwise running the script may fail
evaluating dependency order:
http://rosettacode.org/wiki/Topological_sort#Object_Pascal
}
uses SysTables, Main;

(********************  Script Roles  ***********************)

function ScriptAllRoles(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
begin
  List.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 9, Count);
  for i:= 0 to List.Count - 1 do
    List[i]:= 'Create Role ' + List[i] + ';';
  Result:= List.Count > 0;
end;

(****************  Script Functions (UDFs)  *******************)

procedure RemoveParamClosing(var AParams: string);
var
  i: Integer;
  R: Integer;
begin
  R:= Pos('returns', LowerCase(AParams));
  if R > 0 then
    for i:= R downto 0 do
      if AParams[i] = ')' then
      begin
        Delete(AParams, i, 1);
        Break;
      end;
end;

function ScriptAllFunctions(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  FunctionsList: TStringList;
  ModuleName, EntryPoint, Params: string;
begin
  FunctionsList:= TStringList.Create;
  FunctionsList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 6, Count);
  List.Clear;
  for i:= 0 to FunctionsList.Count - 1 do
  begin
    List.Add('Declare External Function ' + FunctionsList[i]);
    if fmMain.GetUDFInfo(dbIndex, FunctionsList[i], ModuleName, EntryPoint, Params) then
    begin
      RemoveParamClosing(Params);
      List.Add(Params);
      List.Add('ENTRY_POINT ''' + EntryPoint + '''');
      List.Add('MODULE_NAME ''' + ModuleName + ''';');
      List.Add('');
    end;
  end;
  Result:= FunctionsList.Count > 0;
  FunctionsList.Free;
end;


(********************  Script Generators   ***********************)

function ScriptAllGenerators(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
begin
  List.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 2, Count);
  for i:= 0 to List.Count - 1 do
    List[i]:= 'Create Generator ' + List[i] + ' ;';
  Result:= List.Count > 0;
end;


(********************  Script Domains  ***********************)

function ScriptAllDomains(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  DomainType: string;
  DomainSize: Integer;
  DefaultValue: string;
begin
  List.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 8, Count);
  for i:= 0 to List.Count - 1 do
  begin
    // todo: add support for numeric field types (e.g.
    dmSysTables.GetDomainInfo(dbIndex, List[i], DomainType, DomainSize, DefaultValue);

    List[i]:= 'Create Domain ' + List[i] + ' as ' + DomainType;
    if Pos('CHAR', DomainType) > 0 then
      List[i]:= List[i] + '(' + IntToStr(DomainSize) + ')'
    else
      List[i]:= List[i] ;
    List[i]:= List[i] + ' ' + DefaultValue + ';';

  end;
  Result:= List.Count > 0;
end;


(********************  Script Tables   ***********************)

procedure ScriptTableAsCreate(dbIndex: Integer; ATableName: string; ScriptList: TStringList);
var
  i: Integer;
  PKeyName: string;
  PKFieldsList: TStringList;
  FieldLine: string;
  Skipped: Boolean;
  ConstraintName: string;
  CalculatedList: TStringList; // for calculated fields
  DefaultValue: string;
begin
  fmMain.GetFields(dbIndex, ATableName, nil);
  ScriptList.Clear;
  ScriptList.Add('create table ' + ATableName + ' (');
  CalculatedList:= TStringList.Create;
  try
    // Fields
    with fmMain.SQLQuery1 do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('Computed_Source').AsString = '') and
       ((Pos('CHAR', Trim(FieldByName('Field_Type_Str').AsString)) = 0) or
       (Trim(FieldByName('Field_Collation').AsString) = 'NONE') or
       (FieldByName('Field_Collation').IsNull)) then
      begin
        // Field Name
        FieldLine:= Trim(FieldByName('Field_Name').AsString) + ' ';

        // Field type
        FieldLine:= FieldLine + fmMain.GetFBTypeName(FieldByName('Field_Type_Int').AsInteger,
          FieldByName('Field_SubType').AsInteger,
          FieldByName('Field_Length').AsInteger,
          FieldByName('Field_Scale').AsInteger);

        if Pos('char', LowerCase(FieldByName('Field_Type_Str').AsString)) > 0 then
          FieldLine:= FieldLine + '(' + FieldByName('Character_Leng').AsString + ') ';

        // Rudimentary support for array datatypes (only covers 0 dimension types):
        if not(FieldByName('Array_Upper_Bound').IsNull) then
          FieldLine:= FieldLine + ' [' + FieldByName('Array_Upper_Bound').AsString + '] ';

        // Default value
        DefaultValue:= Trim(FieldByName('Field_Default_Value').AsString);
        if DefaultValue <> '' then
        begin
          if pos('default', DefaultValue) <> 1 then
            DefaultValue:= ' default ''' + DefaultValue + '''';
          FieldLine:= FieldLine + ' ' + DefaultValue;
        end;

        // Null/Not null
        if FieldByName('field_not_null_constraint').AsString = '1' then
           FieldLine:= FieldLine + ' not null ';
      end
      else
      begin
        Skipped:= True;
      end;

      // Computed Fields
      if FieldByName('Computed_Source').AsString <> '' then
        CalculatedList.Add('ALTER TABLE ' + ATableName + ' ADD ' +
          Trim(FieldByName('Field_Name').AsString) + ' COMPUTED BY ' + FieldByName('Computed_Source').AsString + ';');

      Next;

      if not Skipped then
      begin
        if not EOF then
          FieldLine:= FieldLine + ',';
        ScriptList.Add(FieldLine);
      end;
    end;

    if Pos(',', ScriptList[ScriptList.Count - 1]) > 0 then
      ScriptList[ScriptList.Count - 1]:= Copy(ScriptList[ScriptList.Count - 1], 1,
        Length(ScriptList[ScriptList.Count - 1]) - 1);

    fmMain.SQLQuery1.Close;

    // Primary Keys
    PKFieldsList:= TStringList.Create;
    PKeyName:= fmMain.GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
    if PKeyName <> '' then
    begin
      fmMain.GetConstraintFields(ATableName, PKeyName, PKFieldsList);
      FieldLine:= 'constraint ' + PKeyName + ' primary key (';
      for i:= 0 to PKFieldsList.Count - 1 do
        FieldLine:= FieldLine + PKFieldsList[i] + ', ';
      if PKFieldsList.Count > 0 then
      begin
        Delete(FieldLine, Length(FieldLine) - 1, 2);
        FieldLine:= FieldLine + ')';
        ScriptList.Add(', ' + FieldLine);
      end;
    end;

    ScriptList.Add(');');
    ScriptList.Add(CalculatedList.Text);
  finally
    CalculatedList.Free;
  end;
end;

(***************  Script All Tables  ********************)

function ScriptAllTables(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  TablesList: TStringList;
  TableScript: TStringList;
begin
  TablesList:= TStringList.Create;
  TableScript:= TStringList.Create;
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 1, Count);
    List.Clear;
    for i:= 0 to TablesList.Count - 1 do
    begin
      ScriptTableAsCreate(dbIndex, TablesList[i], TableScript);
      List.Add('');
      List.AddStrings(TableScript);
    end;
    Result:= TablesList.Count > 0;
  finally
    TablesList.Free;
    TableScript.Free;
  end;
end;

(********************  Script Procedure Template  ***********************)

function ScriptAllProcedureTemplates(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  ProceduresList: TStringList;
  ProcedureScript: TStringList;
  SPOwner: string;
  SPBody: string;
begin
  ProceduresList:= TStringList.Create;
  ProcedureScript:= TStringList.Create;
  try
    ProceduresList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 5, Count);
    List.Clear;
    for i:= 0 to ProceduresList.Count - 1 do
    begin
      ProcedureScript.Text:= fmMain.GetStoredProcBody(dbIndex, ProceduresList[i], SPOwner);
      ProcedureScript.Insert(0, 'SET TERM ^ ;');
      ProcedureScript.Insert(1, 'CREATE Procedure ' + ProceduresList[i] + '(');
      ProcedureScript.Add('^');
      ProcedureScript.Add('SET TERM ; ^');
      ProcedureScript.Add('');
      List.AddStrings(ProcedureScript);
    end;
    Result:= ProceduresList.Count > 0;
  finally
    ProceduresList.Free;
    ProcedureScript.Free;
  end;
end;

(********************  Script Views   ***********************)

function ScriptAllViews(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  ViewsList: TStringList;
  ViewsBodyList: TStringList;
  Columns, ViewBody: string;
begin
  ViewsList:= TStringList.Create;
  ViewsBodyList:= TStringList.Create;
  try
    ViewsList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 4, Count);
    List.Clear;
    for i:= 0 to ViewsList.Count - 1 do
    begin
      fmMain.GetViewInfo(dbIndex, ViewsList[i], Columns, ViewBody);
      ViewsBodyList.Text:= Trim(ViewBody);
      List.Add('CREATE VIEW "' + ViewsList[i] + '" (' + Columns + ')');
      List.Add('AS');
      List.AddStrings(ViewsBodyList);
      List.Add(' ;');
    end;
    Result:= ViewsList.Count > 0;
  finally
    ViewsList.Free;
    ViewsBodyList.Free;
  end;
end;


(********************  Script Triggers   ***********************)

function ScriptAllTriggers(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  TriggersList: TStringList;
  TriggerScript: TStringList;
begin
  TriggersList:= TStringList.Create;
  TriggerScript:= TStringList.Create;
  try
    TriggersList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 3, Count);
    List.Clear;
    for i:= 0 to TriggersList.Count - 1 do
    begin
      TriggerScript.Clear;
      dmSysTables.ScriptTrigger(dbIndex, TriggersList[i], TriggerScript, True);
      List.AddStrings(TriggerScript);
      List.Add('');
    end;
    Result:= TriggersList.Count > 0;
  finally
    TriggerScript.Free;
    TriggersList.Free;
  end;
end;

(********************  Script Secondary indices  ***********************)

function ScriptAllSecIndices(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i: Integer;
  TablesList: TStringList;
  PKName: string;
  FieldsList: TStringList;
  Line: string;
  ConstraintName: string;
begin
  TablesList:= TStringList.Create;
  FieldsList:= TStringList.Create;
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 1, Count);
    List.Clear;
    for i:= 0 to TablesList.Count - 1 do
    begin
      PKName:= fmMain.GetPrimaryKeyIndexName(dbIndex, TablesList[i], ConstraintName);

      if fmMain.GetIndices(TablesList[i], dmSysTables.sqQuery) then
      with dmSysTables.sqQuery do
      while not EOF do
      begin
        if PKName <> Trim(FieldByName('RDB$Index_name').AsString) then
        begin
          Line:= 'create ';
          if FieldByName('RDB$Unique_Flag').AsString = '1' then
            Line:= Line + 'Unique ';
          if FieldByName('RDB$Index_Type').AsString = '1' then
            Line:= Line + 'Descending ';

          Line:= Line + 'index ' + Trim(FieldByName('RDB$Index_name').AsString) + ' on ' + TablesList[i];

          fmMain.GetIndexFields(TablesList[i], Trim(FieldByName('RDB$Index_Name').AsString), fmMain.SQLQuery1, FieldsList);
          Line:= Line + ' (' + FieldsList.CommaText + ') ;';
          List.Add(Line);
        end;
        Next;
      end;
    end;
    dmSysTables.sqQuery.Close;
    Result:= List.Count > 0;
  finally
    TablesList.Free;
    FieldsList.Free;
  end;
end;


(********************  Script Constraints   ***********************)

function ScriptAllConstraints(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: integer;
  ConstraintArray: TConstraintCounts;
  ConstraintName: string;
  CompositeClauseFK: string;
  CompositeClauseRef: string;
  CompositeConstraint: string;
  CompositeCount: integer;
  CompositeCounter: integer;
  TableCounter: integer;
  TablesList: TStringList;
  Line: string;

  procedure WriteResult(
    const TableName, ConstraintName, CurrentFieldName, CurrentTableName,
    OtherFieldName, OtherTableName, DeleteRule, UpdateRule: string;
    var List: TStringList);
  const
    Template= 'alter table %s' +
      ' add constraint %s' +
      ' foreign key (%s)'+
      ' references %s' +
      ' (%s)';
  var
    Line: string;
  begin
    Line:= format(Template,[TableName,ConstraintName,CurrentFieldName,
      OtherTableName, OtherFieldName]);
    if UpdateRule <> 'RESTRICT' then
      Line:= Line + ' on update ' + UpdateRule;
    if DeleteRule <> 'RESTRICT' then
      Line:= Line + ' on delete ' + DeleteRule;
    List.Add(Line + ';');
  end;

begin
  TablesList:= TStringList.Create;
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, 1, Count);
    List.Clear;
    for TableCounter:= 0 to TablesList.Count - 1 do
    with dmSysTables do
    begin
      GetTableConstraints(TablesList[TableCounter], sqQuery);
      FillCompositeFKConstraints(TablesList[TableCounter],ConstraintArray);
      CompositeConstraint:= '';
      while not sqQuery.EOF do
      begin
        ConstraintName:= sqQuery.FieldByName('ConstName').AsString;
        CompositeCount:= GetCompositeFKConstraint(ConstraintName, ConstraintArray);
        if CompositeCount>0 then
        begin
          // Multiple columns form a composite foreign key index.
          if ConstraintName<>CompositeConstraint then
          begin
            // A new constraint just started
            CompositeConstraint:= ConstraintName;
            CompositeCounter:= 1;
            CompositeClauseFK:= sqQuery.FieldByName('CurrentFieldName').AsString+', ';
            CompositeClauseRef:= sqQuery.FieldByName('OtherFieldName').AsString+', ';
          end
          else
          begin
            inc(CompositeCounter);
            if CompositeCounter=CompositeCount then
            begin
              // Last record for this constraint, so write out
              CompositeClauseFK:= CompositeClauseFK + sqQuery.FieldByName('CurrentFieldName').AsString;
              CompositeClauseRef:= CompositeClauseRef + sqQuery.FieldByName('OtherFieldName').AsString;
              WriteResult(TablesList[TableCounter],
                ConstraintName,
                CompositeClauseFK,
                Trim(sqQuery.FieldByName('CurrentTableName').AsString),
                CompositeClauseRef,
                Trim(sqQuery.FieldByName('OtherTableName').AsString),
                Trim(sqQuery.FieldByName('DeleteRule').AsString),
                Trim(sqQuery.FieldByName('UpdateRule').AsString),
                List);
            end
            else
            begin
              // In middle of clause, so keep adding
              CompositeClauseFK:= CompositeClauseFK + sqQuery.FieldByName('CurrentFieldName').AsString + ', ';
              CompositeClauseRef:= CompositeClauseRef + sqQuery.FieldByName('OtherFieldName').AsString + ', ';
            end;
          end;
        end
        else
        begin
          // Normal, non-composite foreign key which we can write out based on one record in the query
          // We're using fieldbyname here instead of fields[x] because of maintainability and probably
          // low performance impact.
          // If performance is an issue, define field variables outside the loop and reference them instead
          WriteResult(TablesList[TableCounter],
            ConstraintName,
            Trim(sqQuery.FieldByName('CurrentFieldName').AsString),
            Trim(sqQuery.FieldByName('CurrentTableName').AsString),
            Trim(sqQuery.FieldByName('OtherFieldName').AsString),
            Trim(sqQuery.FieldByName('OtherTableName').AsString),
            Trim(sqQuery.FieldByName('DeleteRule').AsString),
            Trim(sqQuery.FieldByName('UpdateRule').AsString),
            List);
        end;
        sqQuery.Next;
      end;
      sqQuery.Close;
    end;
    Result:= List.Count > 0;
  finally
    TablesList.Free;
  end;
end;


function ScriptObjectPermission(dbIndex: Integer; ObjName, UserName: string; var ObjType: Integer;
   List: TStrings; NewUser: string = ''): Boolean;
var
  Permissions: string;
  Line: string;
  PermissionList: TStringList;
  OrigObjName: string;
begin
  try
    if NewUser = '' then
      NewUser:= UserName;
    OrigObjName:= ObjName;
    ObjName:= Copy(ObjName, 4, Length(ObjName) - 3);
    Permissions:= dmSysTables.GetObjectUserPermission(dbIndex, ObjName, UserName, ObjType);
    PermissionList:= TstringList.Create;
    try
      if Permissions <> '' then
      begin
        if Pos('<T>', OrigObjName) = 1 then // Table/View
        begin
          PermissionList.Clear;
          if Pos('S', Permissions) > 0 then
            PermissionList.Add('Select');

          if Pos('U', Permissions) > 0 then
            PermissionList.Add('Update');

          if Pos('I', Permissions) > 0 then
            PermissionList.Add('Insert');

          if Pos('D', Permissions) > 0 then
            PermissionList.Add('Delete');

          if Pos('R', Permissions) > 0 then
            PermissionList.Add('References');
          Line:= 'Grant ' + PermissionList.CommaText + ' on ' + ObjName + ' to ' + NewUser;
          if Pos('G', Permissions) > 0 then
            Line:= Line + ' with Grant option';
          List.Add(Line + ' ;');
        end
        else
        if Pos('<P>', OrigObjName) = 1 then // Procedure
          List.Add('Grant Execute on procedure ' + ObjName + ' to ' + NewUser + ' ;')
        else
        if Pos('<R>', OrigObjName) = 1 then // Role
          List.Add('Grant ' + ObjName + ' to ' + NewUser + ' ;');

      end;
    finally
      PermissionList.Free;
    end;
    Result:= True;
  except
    on e: exception do
      Result:= False;
  end;
end;

(********************  Script All Usesr and Rules permissions ***********************)

function ScriptAllPermissions(dbIndex: Integer; var List: TStringList): Boolean;
var
  Count: Integer;
  i, j: Integer;
  UsersList: TStringList;
  ObjectsList: TStringList;
  PermissionList: TStringList;
  ObjName: string;
  Permissions: string;
  UserName: string;
  Line: string;
  ObjType: Integer;
begin
  UsersList:= TStringList.Create;
  ObjectsList:= TStringList.Create;
  PermissionList:= TStringList.Create;
  try
    UsersList.CommaText:= dmSysTables.GetDBUsers(dbIndex);
    List.Clear;
    for i:= 0 to UsersList.Count - 1 do
      if Pos('<R>', UsersList[i]) = 1 then
        List.Add('/* Role ' + Copy(UsersList[i], 4, Length(UsersList[i]) - 3) + ' */')
      else
        List.Add('/* User ' + UsersList[i] + ' */');

    for i:= 0 to UsersList.Count - 1 do
    begin
      ObjectsList.CommaText:= dmSysTables.GetDBObjectsForPermissions(dbIndex);
      if Pos('<R>', UsersList[i]) = 1 then
        UserName:= Copy(UsersList[i], 4, Length(UsersList[i]) - 3)
      else
        UserName:= UsersList[i];

      List.Add('');
      List.Add('/* Permissions for: ' + UserName + ' */');

      for j:= 0 to ObjectsList.Count - 1 do
      begin
        Result:= ScriptObjectPermission(dbIndex,  ObjectsList[j], UserName, ObjType, List);
      end;
    end;
    Result:= UsersList.Count > 0;
  finally
    UsersList.Free;
    ObjectsList.Free;
    PermissionList.Free;
  end;
end;

(********************  Script One User or Rule permissions ***********************)

function ScriptUserAllPermissions(dbIndex: Integer; UserName: string; var List: TStringList;
   NewUser: string = ''): Boolean;
var
  j: Integer;
  UsersList: TStringList;
  ObjectsList: TStringList;
  ObjType: Integer;
begin
  if NewUser = '' then
    NewUser:= UserName;
  UsersList:= TStringList.Create;
  ObjectsList:= TStringList.Create;
  try
    UsersList.CommaText:= dmSysTables.GetDBUsers(dbIndex);
    List.Clear;

    ObjectsList.CommaText:= dmSysTables.GetDBObjectsForPermissions(dbIndex);

    List.Add('');
    List.Add('/* Permissions for: ' + UserName + ' */');

    for j:= 0 to ObjectsList.Count - 1 do
      Result:= ScriptObjectPermission(dbIndex,  ObjectsList[j], UserName, ObjType, List, NewUser);

    Result:= UsersList.Count > 0;
  finally
    UsersList.Free;
    ObjectsList.Free;
  end;
end;

end.

