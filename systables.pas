unit SysTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, LResources, Forms, Controls,
  Dialogs, db;

type

  { TdmSysTables }

  TdmSysTables = class(TDataModule)
    sqQuery: TSQLQuery;
  private
    { private declarations }
  public
    ibcDatabase: TIBConnection;
    stTrans: TSQLTransaction;
    procedure Init(dbIndex: Integer);
    function GetDBObjectNames(DatabaseIndex, TVIndex: Integer; var Count: Integer): string;
    function GetTriggerInfo(DatabaseIndex: Integer; ATriggername: string;
      var AfterBefor, OnTable, Event, Body: string; var TriggerEnabled: Boolean;
      var TriggerPosition: Integer): Boolean;
    procedure ScriptTrigger(dbIndex: Integer; ATriggerName: string; List: TStrings;
      AsCreate: Boolean = False);
    function GetTableConstraints(ATableName: string; var SqlQuery: TSQLQuery;
      ConstraintsList: TStringList = nil): Boolean;

    function GetConstraintInfo(dbIndex: Integer; ATableName, ConstraintName: string; var KeyName,
        CurrentTableName, CurrentFieldName, OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string): Boolean;

    function GetExceptionInfo(ExceptionName: string; var Msg, Description, SqlQuery: string): Boolean;
    procedure GetDomainInfo(dbIndex: Integer; DomainName: string; var DomainType: string;
      var DomainSize: Integer; var DefaultValue: string);
    function GetConstraintForiegnKeyFields(AIndexName: string; SqlQuery: TSQLQuery): string;

    function GetDBUsers(dbIndex: Integer; ObjectName: string = ''): string;
    function GetDBObjectsForPermissions(dbIndex: Integer; AObjectType: Integer = -1): string;
    function GetObjectUsers(dbIndex: Integer; ObjectName: string): string;
    function GetUserObjects(dbIndex: Integer; UserName: string; AObjectType: Integer = -1): string;
    function GetObjectUserPermission(dbIndex: Integer; ObjectName, UserName: string; var ObjType: Integer): string;

    procedure GetBasicTypes(List: TStrings);
    procedure GetDomainTypes(dbIndex: Integer; List: TStrings);
    function GetDefaultTypeSize(dbIndex: Integer; TypeName: string): Integer;
    function GetDomainTypeSize(dbIndex: Integer; DomainTypeName: string): Integer;

    function GetFieldInfo(dbIndex: Integer; TableName, FieldName: string; var FieldType: string;
      var FieldSize: Integer; var NotNull: Boolean; var DefaultValue, Description : string): Boolean;

    function GetDatabaseInfo(dbIndex: Integer; var DatabaseName, CharSet, CreationDate: string;
      var ODSVerMajor, ODSVerMinor, Pages, PageSize: Integer; var ProcessList: TStringList): Boolean;

    function GetIndices(dbIndex: Integer; ATableName: string; PrimaryIndexName: string;
      var List: TStringList): Boolean;

    function GetPrimaryKeyIndexName(dbIndex: Integer; ATableName: string; var ConstraintName: string): string;

    function GetIndexInfo(dbIndex: Integer; ATableName, AIndexName: string;
      var FieldsList: TStringList; var Unique, Ascending: Boolean): Boolean;

    procedure GetTableFields(dbIndex: Integer; ATableName: string; FieldsList: TStringList);


    { public declarations }
  end; 

var
  dmSysTables: TdmSysTables;

implementation

uses Main;

function DecToBin(Dec, Len: Byte): string;
var
  Temp: string;
  i: byte;
begin
  Temp:= '';
  for i:= 1 to Len do
  begin
    Temp:= Char((Dec mod 2) + 48) + Temp;
    Dec:= Dec shr 1;
  end;
  Result:= Temp;
end;

{ TdmSysTables }

procedure TdmSysTables.Init(dbIndex: Integer);
begin
  with fmMain.RegisteredDatabases[dbIndex] do
  begin
  //  IBConnection.Close;
    sqQuery.Close;
    IBConnection.DatabaseName:= RegRec.DatabaseName;
    IBConnection.UserName:= RegRec.UserName;
    IBConnection.Password:= RegRec.Password;
    IBConnection.Role:= RegRec.Role;
    IBConnection.CharSet:= RegRec.Charset;
    ibcDatabase:= IBConnection;
    stTrans:= SQLTrans;
    sqQuery.DataBase:= ibcDatabase;
    sqQuery.Transaction:= stTrans;
  end;
end;

(*****  GetDBObjectNames, like Table names, Triggers, Generators, etc according to TVIndex  ****)

function TdmSysTables.GetDBObjectNames(DatabaseIndex, TVIndex: Integer; var count: Integer): string;
begin
    Init(DatabaseIndex);
    sqQuery.Close;
    if TVIndex = 1 then // Tables
      sqQuery.SQL.Text:= 'select rdb$relation_name from rdb$relations where rdb$view_blr is null ' +
        ' and (rdb$system_flag is null or rdb$system_flag = 0) order by rdb$relation_name'
    else
    if TVIndex = 2 then // Generators
      sqQuery.SQL.Text:= 'select RDB$GENERATOR_Name from RDB$GENERATORS where RDB$SYSTEM_FLAG = 0 order by rdb$generator_Name'
    else
    if TVIndex = 3 then // Triggers
      sqQuery.SQL.Text:= 'SELECT rdb$Trigger_Name FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 order by rdb$Trigger_Name'
    else
    if TVIndex = 4 then // Views
      sqQuery.SQL.Text:= 'SELECT DISTINCT RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS order by rdb$View_Name'
    else
    if TVIndex = 5 then // Stored Procedures
      sqQuery.SQL.Text:= 'SELECT RDB$Procedure_Name FROM RDB$PROCEDURES order by rdb$Procedure_Name'
    else
    if TVIndex = 6 then // UDF
      sqQuery.SQL.Text:= 'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS where RDB$SYSTEM_FLAG=0 order by rdb$Function_Name'
    else
    if TVIndex = 7 then // System Tables
      sqQuery.SQL.Text:= 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS where RDB$SYSTEM_FLAG=1 ' +
        'order by RDB$RELATION_NAME'
    else
    if TVIndex = 8 then // Domains
      sqQuery.SQL.Text:= 'select RDB$FIELD_NAME from RDB$FIELDS where RDB$Field_Name not like ''RDB$%''  order by rdb$Field_Name'
    else
    if TVIndex = 9 then // Roles
      sqQuery.SQL.Text:= 'select RDB$ROLE_NAME from RDB$ROLES order by rdb$Role_Name'
    else
    if TVIndex = 10 then // Exceptions
      sqQuery.SQL.Text:= 'select RDB$EXCEPTION_NAME from RDB$EXCEPTIONS order by rdb$Exception_Name'
    else
    if TVIndex = 11 then // Users
      sqQuery.SQL.Text:= 'select distinct RDB$User from RDB$USER_PRIVILEGES where RDB$User_Type = 8 order by rdb$User';


    sqQuery.Open;
    while not sqQuery.EOF do
    begin
      Result:= Result + sqQuery.Fields[0].AsString;
      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';
    end;
    Count:= sqQuery.RecordCount;
    sqQuery.Close;
end;

(***********  Get Trigger Info  ***************)

function TdmSysTables.GetTriggerInfo(DatabaseIndex: Integer; ATriggername: string;
  var AfterBefor, OnTable, Event, Body: string; var TriggerEnabled: Boolean; var TriggerPosition: Integer): Boolean;
var
  Rec: TDatabaseRec;
  Encode: string;
begin
  try
    Init(DatabaseIndex);
    sqQuery.Close;
    sqQuery.SQL.Text:= 'SELECT RDB$TRIGGER_NAME AS trigger_name, ' +
      '  RDB$RELATION_NAME AS table_name, ' +
      '  RDB$TRIGGER_SOURCE AS trigger_body, ' +
      '  RDB$TRIGGER_TYPE as Trigger_Type, ' +
      '  RDB$Trigger_Sequence as TPos, ' +
      '   CASE RDB$TRIGGER_INACTIVE ' +
      '   WHEN 1 THEN 0 ELSE 1 ' +
      ' END AS trigger_enabled, ' +
      ' RDB$DESCRIPTION AS trigger_comment ' +
      ' FROM RDB$TRIGGERS ' +
      ' WHERE UPPER(RDB$TRIGGER_NAME)=''' + ATriggerName + ''' ';

    sqQuery.Open;
    Body:= Trim(sqQuery.FieldByName('Trigger_Body').AsString);
    OnTable:= Trim(sqQuery.FieldByName('Table_Name').AsString);
    TriggerEnabled:= sqQuery.FieldByName('Trigger_Enabled').AsBoolean;
    TriggerPosition:= sqQuery.FieldByName('TPos').AsInteger;
    Encode:= DecToBin(sqQuery.FieldByName('Trigger_Type').AsInteger + 1, 7);
    if Encode[7] = '1' then
      AfterBefor:= 'After'
    else
      AfterBefor:= 'Before';
    Delete(Encode, 7, 1);
    Event:= '';
    while Length(Encode) > 0 do
    begin
      if Copy(Encode, Length(Encode) - 1, 2) = '01' then
        Event:= Event + 'Insert'
      else
      if Copy(Encode, Length(Encode) - 1, 2) = '10' then
        Event:= Event + 'Update'
      else
      if Copy(Encode, Length(Encode) - 1, 2) = '11' then
        Event:= Event + 'Delete';
      Delete(Encode, Length(Encode) - 1, 2);
      if (Encode <> '') and (Copy(Encode, Length(Encode) - 1, 2) <> '00') then
        Event:= Event + ' or ';
    end;
    sqQuery.Close;
    Result:= True;

  except
  on e: exception do
  begin
    MessageDlg('Error: ' + e.Message, mtError, [mbOk], 0);
    Result:= False;
  end;
  end;
end;

(****************  Script Trigger  ***************)

procedure TdmSysTables.ScriptTrigger(dbIndex: Integer; ATriggerName: string; List: TStrings;
   AsCreate: Boolean = False);
var
  Body: string;
  AfterBefore: string;
  Event: string;
  OnTable: string;
  TriggerEnabled: Boolean;
  TriggerPosition: Integer;
begin
  GetTriggerInfo(dbIndex, ATriggerName, AfterBefore, OnTable, Event, Body, TriggerEnabled, TriggerPosition);
  List.Add('SET TERM ^ ;');
  if AsCreate then
    List.Add('Create Trigger ' + ATriggerName + ' for ' + OnTable)
  else
    List.Add('Alter Trigger ' + ATriggerName);
    if TriggerEnabled then
      List.Add('ACTIVE')
    else
      List.Add('INACTIVE');

  List.Add(AfterBefore + ' ' + Event);
  List.Add('Position ' + IntToStr(TriggerPosition));

  List.Text:= List.Text + Body + ' ^';
  List.Add('SET TERM ; ^');

end;

(**********  Get Table Constraints Info  ********************)

function TdmSysTables.GetTableConstraints(ATableName: string; var SqlQuery: TSQLQuery;
   ConstraintsList: TStringList = nil): Boolean;
begin
  SqlQuery.Close;
  SqlQuery.SQL.Text:= 'select Trim(Refc.RDB$Constraint_Name) as ConstName, ' +
    'Trim(Refc.RDB$CONST_NAME_UQ) as KeyName, ' +
    'Trim(Ind.RDB$Relation_Name) as CurrentTableName, ' +
    'Trim(Seg.RDB$Field_name) as CurrentFieldName, ' +
    'Trim(Con.RDB$Relation_Name) as OtherTableName, ' +
    'Trim(Ind.RDB$Foreign_key) as OtherFieldName, ' +
    'RDB$Update_Rule as UpdateRule, RDB$Delete_Rule as DeleteRule ' +
    'from RDB$RELATION_CONSTRAINTS Con, rdb$REF_Constraints Refc, RDB$INDEX_SEGMENTS Seg, ' +
    'RDB$INDICES Ind ' +
    'where Con.RDB$COnstraint_Name = Refc.RDB$Const_Name_UQ ' +
    '  and Refc.RDB$COnstraint_Name = Ind.RDB$Index_Name' +
    '  and Refc.RDB$COnstraint_Name = Seg.RDB$Index_Name' +
    '  and Ind.RDB$Relation_Name = ''' + UpperCase(ATableName) + '''';
  SqlQuery.Open;
  Result:= SqlQuery.RecordCount > 0;
  with SqlQuery do
  if Result and Assigned(ConstraintsList) then
  begin
    ConstraintsList.Clear;
    while not Eof do
    begin
      ConstraintsList.Add(FieldByName('ConstName').AsString);
      Next;
    end;
    First;
  end;

end;

(**********  Get Constraint Info  ********************)

function TdmSysTables.GetConstraintInfo(dbIndex: Integer; ATableName, ConstraintName: string; var KeyName,
    CurrentTableName, CurrentFieldName, OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select Trim(Refc.RDB$Constraint_Name) as ConstName, Trim(Refc.RDB$CONST_NAME_UQ) as KeyName, ' +
    'Trim(Ind.RDB$Relation_Name) as CurrentTableName, ' +
    'Trim(Seg.RDB$Field_name) as CurrentFieldName, ' +
    'Trim(Con.RDB$Relation_Name) as OtherTableName, ' +
    'Trim(Ind.RDB$Foreign_key) as OtherFieldName, ' +
    'RDB$Update_Rule as UpdateRule, RDB$Delete_Rule as DeleteRule ' +
    'from RDB$RELATION_CONSTRAINTS Con, rdb$REF_Constraints Refc, RDB$INDEX_SEGMENTS Seg, ' +
    'RDB$INDICES Ind ' +
    'where Con.RDB$COnstraint_Name = Refc.RDB$Const_Name_UQ ' +
    '  and Refc.RDB$COnstraint_Name = Ind.RDB$Index_Name' +
    '  and Refc.RDB$COnstraint_Name = Seg.RDB$Index_Name' +
    '  and Ind.RDB$Relation_Name = ''' + UpperCase(ATableName) + ''' ' +
    '  and Refc.RDB$Constraint_Name = ''' + ConstraintName + '''';
  sqQuery.Open;
  Result:= sqQuery.RecordCount > 0;
  with sqQuery do
  if Result then
  begin
    KeyName:= FieldByName('KeyName').AsString;
    CurrentTableName:= FieldByName('CurrentTableName').AsString;
    CurrentFieldName:= FieldByName('CurrentFieldName').AsString;
    OtherTableName:= FieldByName('OtherTableName').AsString;
    OtherFieldName:= FieldByName('OtherFieldName').AsString;
    UpdateRule:= FieldByName('UpdateRule').AsString;
    DeleteRule:= FieldByName('DeleteRule').AsString;
  end;
  sqQuery.Close;

end;

(*********  Get Exception Info ***************)

function TdmSysTables.GetExceptionInfo(ExceptionName: string; var Msg, Description,
  SqlQuery: string): Boolean;
begin
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select * from RDB$EXCEPTIONS where RDB$EXCEPTION_NAME = ''' + ExceptionName + '''';
  sqQuery.Open;
  Result:= sqQuery.RecordCount > 0;
  if Result then
  begin
    Msg:= sqQuery.FieldByName('RDB$MESSAGE').AsString;
    Description:= sqQuery.FieldByName('RDB$DESCRIPTION').AsString;
    SqlQuery:= 'CREATE EXCEPTION ' + ExceptionName + #10 +
               '''' + Msg + ''';' + #10 +
               'UPDATE RDB$EXCEPTIONS set ' + #10 +
               'RDB$DESCRIPTION = ''' + Description + ''' ' + #10 +
               'where RDB$EXCEPTION_NAME = ''' + ExceptionName + ''';';
  end;
  sqQuery.Close;
end;


(************  View Domain info  ***************)

procedure TdmSysTables.GetDomainInfo(dbIndex: Integer; DomainName: string; var DomainType: string;
  var DomainSize: Integer; var DefaultValue: string);
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select * from RDB$FIELDS where RDB$Field_Name = ''' + UpperCase(DomainName) + '''';
  sqQuery.Open;

  if sqQuery.RecordCount > 0 then
  begin
    DomainType:= fmMain.GetFBTypeName(sqQuery.FieldByName('RDB$FIELD_TYPE').AsInteger);
    DomainSize:= sqQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;
    DefaultValue:= sqQuery.FieldByName('RDB$DEFAULT_SOURCE').AsString;
  end
  else
    DomainSize:= 0;
  sqQuery.Close;
end;


(*************  Get constraint foreign key fields  *************)

function TdmSysTables.GetConstraintForiegnKeyFields(AIndexName: string; SqlQuery: TSQLQuery): string;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text:= 'select RDB$Index_Name as IndexName, RDB$Field_name as FieldName from RDB$INDEX_SEGMENTS ' +
    'where RDB$Index_name = ''' + UpperCase(Trim(AIndexName)) + '''';
  SQLQuery.Open;
  while not SQLQuery.EOF do
  begin
    Result:= Result + Trim(SQLQuery.FieldByName('FieldName').AsString);
    SQLQuery.Next;
    if not SQLQuery.EOF then
      Result:= Result + ',';
  end;
  SQLQuery.Close;
end;


(************  Get Database Users  ************)

function TdmSysTables.GetDBUsers(dbIndex: Integer; ObjectName: string = ''): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$User, RDB$User_Type from RDB$USER_PRIVILEGES ';
  if ObjectName <> '' then // Specify specific Object
    sqQuery.SQL.Add('where RDB$Relation_Name = ''' + UpperCase(ObjectName) + ''' ');
  sqQuery.SQL.Add('order by RDB$User_Type');
  sqQuery.Open;
  while not sqQuery.EOF do
  begin
    if sqQuery.Fields[1].AsInteger = 13 then // Role
      Result:= Result + '<R>';
    Result:= Result + Trim(sqQuery.Fields[0].Text);
    sqQuery.Next;
    if not sqQuery.EOF then
      Result:= Result + ',';
  end;
  sqQuery.Close;
end;


(************  Get Database Objects for permissions ************)

function TdmSysTables.GetDBObjectsForPermissions(dbIndex: Integer; AObjectType: Integer = -1): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$Relation_Name, RDB$Object_Type from RDB$USER_PRIVILEGES ';
  if AObjectType <> -1 then
    sqQuery.SQL.Add('where RDB$Object_Type = ' + IntToStr(AObjectType));
  sqQuery.SQL.Add(' order by RDB$Object_Type');
  sqQuery.Open;
  while not sqQuery.EOF do
  begin
    if Pos('$', sqQuery.Fields[0].AsString) = 0 then
    begin
      if AObjectType = -1 then
      case sqQuery.Fields[1].AsInteger of
        0: Result:= Result + '<T>'; // Table/View
        5: Result:= Result + '<P>'; // Procedure
        13: Result:= Result + '<R>'; // Role
      end;
      Result:= Result + Trim(sqQuery.Fields[0].Text);
      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';

    end
    else
      sqQuery.Next;
  end;
  sqQuery.Close;
end;

(************  Get Object Users ************)

function TdmSysTables.GetObjectUsers(dbIndex: Integer; ObjectName: string): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$User, RDB$User_Type from RDB$USER_PRIVILEGES  ' +
    'where RDB$Relation_Name = ''' + ObjectName + '''';
  sqQuery.Open;
  while not sqQuery.EOF do
  begin
      if sqQuery.Fields[1].AsInteger = 13 then // Role
        Result:= Result + '<R>';
      Result:= Result + Trim(sqQuery.Fields[0].Text);

      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';
  end;
  sqQuery.Close;
end;

(************  Get Users Objects ************)

function TdmSysTables.GetUserObjects(dbIndex: Integer; UserName: string; AObjectType: Integer = -1): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$Relation_Name, RDB$Grant_Option from RDB$USER_PRIVILEGES  ' +
    'where RDB$User = ''' + UserName + ''' ';
  if AObjectType <> -1 then
    sqQuery.SQL.Add(' and RDB$Object_Type = ' + IntToStr(AObjectType));
  sqQuery.SQL.Add(' order by RDB$Object_Type');
  sqQuery.Open;
  Result:= '';
  while not sqQuery.EOF do
  begin
    if sqQuery.FieldByName('RDB$Grant_Option').AsInteger <> 0 then
      Result:= Result + '<G>';
    Result:= Result + Trim(sqQuery.Fields[0].Text);

    sqQuery.Next;
    if not sqQuery.EOF then
      Result:= Result + ',';
  end;
  sqQuery.Close;
end;


(************  Get Object User permission ************)

function TdmSysTables.GetObjectUserPermission(dbIndex: Integer; ObjectName, UserName: string;
  var ObjType: Integer): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select * from RDB$User_Privileges where RDB$Relation_Name = ''' +
    ObjectName + ''' and RDB$User = ''' + UserName + '''';
  sqQuery.Open;
  Result:= '';
  if sqQuery.RecordCount >  0 then
  begin
    ObjType:= sqQuery.FieldByName('RDB$Object_Type').AsInteger;
    while not sqQuery.EOF do
    begin
      Result:= Result + Trim(sqQuery.FieldByName('RDB$Privilege').AsString);
      if sqQuery.FieldByName('RDB$Grant_Option').AsInteger <> 0 then
        Result:= Result + 'G';
      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';
    end;

  end;
  sqQuery.Close;
end;

procedure TdmSysTables.GetBasicTypes(List: TStrings);
begin
  List.CommaText:= List.CommaText + 'SMALLINT,INTEGER,BIGINT,VARCHAR,FLOAT,"DOUBLE Precision",CHAR,DATE,TIME,' +
    'TIMESTAMP,CSTRING,D_FLOAT,QUAD,BLOB';
end;

procedure TdmSysTables.GetDomainTypes(dbIndex: Integer; List: TStrings);
var
  Count: Integer;
begin
  List.CommaText:= List.CommaText + ',' + GetDBObjectNames(dbIndex, 8, Count);
end;

function TdmSysTables.GetDefaultTypeSize(dbIndex: Integer; TypeName: string): Integer;
begin
  TypeName:= LowerCase(TypeName);
  if TypeName = 'varchar' then
    Result:= 50
  else
  if TypeName = 'char' then
    Result:= 20
  else
  if TypeName = 'smallint' then
    Result:= 2
  else
  if TypeName = 'integer' then
    Result:= 4
  else
  if TypeName = 'bigint' then
    Result:= 8
  else
  if TypeName = 'float' then
    Result:= 4
  else
  if TypeName = 'timestamp' then
    Result:= 8
  else
  if TypeName = 'date' then
    Result:= 4
  else
  if TypeName = 'time' then
    Result:= 4
  else
  if TypeName = 'double precision' then
    Result:= 8
  else
    Result:= GetDomainTypeSize(dbIndex, TypeName);

end;

function TdmSysTables.GetDomainTypeSize(dbIndex: Integer; DomainTypeName: string): Integer;
var
  DomainType, DefaultValue: string;
begin
  GetDomainInfo(dbIndex, DomainTypeName, DomainType, Result, DefaultValue);
end;


function TdmSysTables.GetFieldInfo(dbIndex: Integer; TableName, FieldName: string; var FieldType: string;
  var FieldSize: Integer; var NotNull: Boolean; var DefaultValue, Description: string): Boolean;
begin
  Init(dbIndex);
  sqQuery.SQL.Text:= 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
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
      ' WHERE r.RDB$RELATION_NAME=''' + TableName + '''  and Trim(r.RDB$FIELD_NAME) = ''' + UpperCase(FieldName) + ''' ' +
      ' ORDER BY r.RDB$FIELD_POSITION';
  sqQuery.Open;
  Result:= sqQuery.RecordCount > 0;
  if Result then
  with sqQuery do
  begin
    FieldType:= Trim(FieldByName('Field_Type_Str').AsString);
    FieldSize:= FieldByName('Field_Length').AsInteger;
    NotNull:= FieldByName('Field_not_null_constraint').AsString = '1';
    DefaultValue:= FieldByName('Field_Default_Value').AsString;
    Description:= FieldByName('Field_Description').AsString;
  end;
  sqQuery.Close;
end;

function TdmSysTables.GetDatabaseInfo(dbIndex: Integer; var DatabaseName, CharSet, CreationDate: string;
  var ODSVerMajor, ODSVerMinor, Pages, PageSize: Integer; var ProcessList: TStringList): Boolean;
begin
  try
    Init(dbIndex);
    sqQuery.SQL.Text:= 'select * from RDB$DATABASE';
    sqQuery.Open;
    CharSet:= sqQuery.fieldbyName('RDB$Character_Set_Name').AsString;
    sqQuery.Close;

    sqQuery.SQL.Text:= 'select * from MON$DATABASE';
    sqQuery.Open;
    DatabaseName:= sqQuery.FieldByName('MON$Database_Name').AsString;
    PageSize:= sqQuery.FieldByName('MON$Page_Size').AsInteger;
    ODSVerMajor:= sqQuery.FieldByName('MON$ODS_Major').AsInteger;
    ODSVerMinor:= sqQuery.FieldByName('MON$ODS_Minor').AsInteger;
    CreationDate:= Trim(sqQuery.FieldByName('MON$Creation_Date').AsString);
    Pages:= sqQuery.FieldByName('MON$Pages').AsInteger;
    sqQuery.Close;

    sqQuery.SQL.Text:= 'select * from MON$ATTACHMENTS';
    if ProcessList = nil then
      ProcessList:= TStringList.Create;
    sqQuery.Open;
    with sqQuery do
    while not EOF do
    begin
      ProcessList.Add('Host: ' + Trim(FieldByName('MON$Remote_Address').AsString) +
        '   User: ' + Trim(FieldByName('Mon$User').AsString)  +
        '   Process: ' + Trim(FieldByName('Mon$Remote_Process').AsString));
      Next;
    end;
    sqQuery.Close;
    Result:= True;

  except
  on e: exception do
    Result:= False;
  end;
end;

function TdmSysTables.GetIndices(dbIndex: Integer; ATableName: string; PrimaryIndexName: string;
  var List: TStringList): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';
  sqQuery.Open;
  Result:= sqQuery.RecordCount > 0;
  with sqQuery do
  if Result then
  begin
    while not Eof do
    begin
      if UpperCase(Trim(PrimaryIndexName)) <> Trim(Fields[0].AsString) then
        List.Add(Trim(Fields[0].AsString));
      Next;
    end;
  end;
  sqQuery.Close

end;

function TdmSysTables.GetPrimaryKeyIndexName(dbIndex: Integer; ATableName: string; var ConstraintName: string): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select RDB$Index_name, RDB$Constraint_Name from RDB$RELATION_CONSTRAINTS ' +
    'where RDB$Relation_Name = ''' + UpperCase(ATableName) + ''' and RDB$Constraint_Type = ''PRIMARY KEY'' ';
  sqQuery.Open;
  if sqQuery.RecordCount > 0 then
  begin
    Result:= Trim(sqQuery.Fields[0].AsString);
    ConstraintName:= Trim(sqQuery.Fields[1].AsString);
  end
  else
    Result:= '';
  sqQuery.Close;
end;

function TdmSysTables.GetIndexInfo(dbIndex: Integer; ATableName, AIndexName: string;
  var FieldsList: TStringList; var Unique, Ascending: Boolean): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'SELECT RDB$Indices.*, RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS field_name, ' + #10 +
     'RDB$INDICES.RDB$DESCRIPTION AS description, ' + #10 +
     '(RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION + 1) AS field_position ' + #10 +
     'FROM RDB$INDEX_SEGMENTS ' + #10 +
     'LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' + #10 +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME '
     + #10 +
     ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)=''' + UpperCase(ATablename) + '''         -- table name ' + #10 +
     '  AND UPPER(RDB$INDICES.RDB$INDEX_NAME)=''' + UpperCase(AIndexName) + ''' -- index name ' + #10 +
     '--  AND RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE IS NULL ' + #10 +
     'ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION;';
  sqQuery.Open;
  Result:= sqQuery.FieldCount > 0;
  if Result then
  begin
    Unique:= sqQuery.FieldByName('RDB$Unique_Flag').AsString = '1';
    Ascending:= sqQuery.FieldByName('RDB$Index_Type').AsString <> '1';
  end;
  FieldsList.Clear;
  if Result then
  while not sqQuery.EOF do
  begin
    FieldsList.Add(Trim(sqQuery.FieldByName('field_name').AsString));
    sqQuery.Next;
  end;
  sqQuery.Close;
end;

procedure TdmSysTables.GetTableFields(dbIndex: Integer; ATableName: string; FieldsList: TStringList);
var
  FieldName: string;
begin
  Init(dbIndex);
  sqQuery.SQL.Text:= 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
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

    sqQuery.Open;
    FieldsList.Clear;
    while not sqQuery.EOF do
    begin
      FieldName:= Trim(sqQuery.FieldByName('field_name').AsString);
      if FieldsList.IndexOf(FieldName) = -1 then
        FieldsList.Add(FieldName);
      sqQuery.Next;
    end;
    sqQuery.Close;
end;

initialization
  {$I systables.lrs}

end.

