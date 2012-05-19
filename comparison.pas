unit Comparison;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, IBConnection, sqldb, QueryWindow;

const
  dbObjects: array [1 .. 13] of string = ('Tables', 'Generators', 'Triggers', 'Views', 'Stored Procedures', 'UDFs',
    'Sys Tables', 'Domains', 'Roles', 'Exceptions', 'Users', 'Indices', 'Constraints');

type

  { TfmComparison }

  TfmComparison = class(TForm)
    bbStart: TBitBtn;
    cbComparedDatabase: TComboBox;
    cxDomains: TCheckBox;
    cxRoles: TCheckBox;
    cxUsers: TCheckBox;
    cxTriggers: TCheckBox;
    cxGenerators: TCheckBox;
    cxTables: TCheckBox;
    cxStoredProcs: TCheckBox;
    cxUDFs: TCheckBox;
    cxViews: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    laScript: TLabel;
    laDatabase: TLabel;
    laComparedDatabase: TLabel;
    meLog: TMemo;
    procedure bbStartClick(Sender: TObject);
    procedure cbComparedDatabaseChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure laScriptClick(Sender: TObject);
  private
    fdbIndex: Integer;
    dbObjectsList: array [1 .. 13] of TStringList;
    dbExistingObjectsList: array [1 .. 13] of TStringList;
    MissingFieldsList: TStringList;

    ExistFieldsList: TStringList;
    ModifiedFieldsList: TStringList;

    ExistIndicesList: TStringList;
    ModifiedIndicesList: TStringList;

    ExistConstraintsList: TStringList;
    ModifiedConstraintsList: TStringList;

    ModifiedViewsList: TStringList;

    fQueryWindow: TfmQueryWindow;

    procedure CheckMissingIndices;
    procedure CheckMissingConstraints;
    procedure CheckMissingDBObjects;
    procedure CheckMissingFields;
    procedure CheckModifiedFields;
    procedure CheckModifiedIndices;
    procedure CheckModifiedConstraints;
    procedure CheckModifiedViews;

    procedure InitializeQueryWindow;
    procedure ScriptMissingFields;
    procedure ScriptModifiedFields;
    procedure ScriptModifiedIndices;
    procedure ScriptModifiedConstraints;
    procedure ScriptModifiedViews;
  public
    procedure Init(dbIndex: Integer);
    { public declarations }
  end;

var
  fmComparison: TfmComparison;

implementation

{$R *.lfm}

{ TfmComparison }

uses Main, SysTables, Scriptdb;

procedure TfmComparison.cbComparedDatabaseChange(Sender: TObject);
begin
  if cbComparedDatabase.ItemIndex <> -1 then
  with fmMain.RegisteredDatabases[cbComparedDatabase.ItemIndex].RegRec do
  begin
    laComparedDatabase.Caption:= DatabaseName;
    bbStart.Enabled:= True;
  end;
end;

procedure TfmComparison.bbStartClick(Sender: TObject);
begin
  CheckMissingDBObjects;

  if cxTables.Checked then
  begin
    CheckMissingIndices;
    CheckMissingConstraints;
    CheckMissingFields;
    CheckModifiedFields;
    CheckModifiedIndices;
    CheckModifiedConstraints;
  end;
  if cxViews.Checked then
    CheckModifiedViews;
end;

procedure TfmComparison.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:= 1 to High(dbObjectsList) do
    dbObjectsList[i].Free;

  for i:= 1 to High(dbExistingObjectsList) do
    dbExistingObjectsList[i].Free;

  MissingFieldsList.Free;

  ExistFieldsList.Free;
  ModifiedFieldsList.Free;

  ExistIndicesList.Free;
  ModifiedIndicesList.Free;

  ExistConstraintsList.Free;
  ModifiedConstraintsList.Free;

  ModifiedViewsList.Free;

end;

procedure TfmComparison.laScriptClick(Sender: TObject);
var
  x: Integer;
  i: Integer;
  ScriptList: TStringList;
  ViewBody: string;
  Columns: string;
  SPOwner: string;
  ModuleName, EntryPoint, Params: string;
  Line: string;
  DomainType, DefaultValue: string;
  DomainSize: Integer;
  ATableName, AIndexName: string;
  FieldsList: TStringList;
  Unique, Ascending: Boolean;
  KeyName, CurrentTableName, CurrentFieldName,
  OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string;
begin
  InitializeQueryWindow;
  meLog.Clear;
  ScriptList:= TStringList.Create;
  FieldsList:= TStringList.Create;

  if cxTables.Checked then
  begin
    ScriptMissingFields;
    ScriptModifiedFields;
    ScriptModifiedIndices;
    ScriptModifiedConstraints;
  end;

  if cxViews.Checked then
    ScriptModifiedViews;


  dmSysTables.Init(fdbIndex);

  fQueryWindow.meQuery.Lines.Add('');
  fQueryWindow.meQuery.Lines.Add('-- Missing db Objects');

  for x:= 1 to 13 do
  begin
    if (x = 1) and cxTables.Checked then // Tables
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      Scriptdb.ScriptTableAsCreate(fdbIndex, dbObjectsList[x].Strings[i], ScriptList);

      fQueryWindow.meQuery.Lines.AddStrings(ScriptList);
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 2) and cxGenerators.Checked then // Generators
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;

      fQueryWindow.meQuery.Lines.Add('create generator ' + dbObjectsList[x].Strings[i] + ';');
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 3) and cxTriggers.Checked then // Triggers
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      dmSysTables.ScriptTrigger(fdbIndex, dbObjectsList[x].Strings[i], ScriptList, True);

      fQueryWindow.meQuery.Lines.AddStrings(ScriptList);
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 4) and cxViews.Checked then // Views
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      fmMain.GetViewInfo(fdbIndex, dbObjectsList[x].Strings[i], Columns, ViewBody);
      ScriptList.Text:= Trim(ViewBody);

      fQueryWindow.meQuery.Lines.Add('CREATE VIEW "' + dbObjectsList[x].Strings[i] + '" (' + Columns + ')');
      fQueryWindow.meQuery.Lines.Add('AS');
      ScriptList.Text:= Trim(ViewBody);
      fQueryWindow.meQuery.Lines.AddStrings(ScriptList);
      fQueryWindow.meQuery.Lines.Add(' ;');
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 5) and cxStoredProcs.Checked then // Stored proc
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Text:= fmMain.GetStoredProcBody(fdbIndex, dbObjectsList[x].Strings[i], SPOwner);
      ScriptList.Insert(0, 'SET TERM ^ ;');
      ScriptList.Insert(1, 'CREATE Procedure ' + dbObjectsList[x].Strings[i] + '(');
      ScriptList.Add('^');
      ScriptList.Add('SET TERM ; ^');
      ScriptList.Add('');

      fQueryWindow.meQuery.Lines.AddStrings(ScriptList);
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 6) and cxUDFs.Checked then // UDF
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      ScriptList.Add('Declare External Function ' + dbObjectsList[x].Strings[i]);
      if fmMain.GetUDFInfo(fdbIndex, dbObjectsList[x].Strings[i], ModuleName, EntryPoint, Params) then
      begin
        RemoveParamClosing(Params);
        ScriptList.Add(Params);
        ScriptList.Add('ENTRY_POINT ''' + EntryPoint + '''');
        ScriptList.Add('MODULE_NAME ''' + ModuleName + ''';');
        ScriptList.Add('');
      end;
      fQueryWindow.meQuery.Lines.AddStrings(ScriptList);
    end
    else
    if (x = 8) and cxDomains.Checked then // Domains
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      dmSysTables.GetDomainInfo(fdbIndex, dbObjectsList[x].Strings[i], DomainType, DomainSize, DefaultValue);

      Line:= 'Create Domain ' + dbObjectsList[x].Strings[i] + ' as ' + DomainType;
        if Pos('CHAR', DomainType) > 0 then
          Line:= Line + '(' + IntToStr(DomainSize) + ')';
        Line:= Line + ' ' + DefaultValue + ';';

      fQueryWindow.meQuery.Lines.Add(Line);
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 9) and cxRoles.Checked then // Roles
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      fQueryWindow.meQuery.Lines.Add('create role ' + dbObjectsList[x].Strings[i] + ';');
      fQueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 12) and cxTables.Checked then // Indices
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      Line:= dbObjectsList[x].Strings[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;
      ScriptList.Clear;
      if dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, Unique, Ascending) then
      begin
        Line:= 'create ';
        if Unique then
          Line:= Line + 'Unique ';
        if not Ascending then
          Line:= Line + 'Descending ';

        Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

        Line:= Line + ' (' + FieldsList.CommaText + ') ;';

        fQueryWindow.meQuery.Lines.Add(Line);
        fQueryWindow.meQuery.Lines.Add('');
      end;

    end
    else
    if (x = 13) and cxTables.Checked then // Constraints
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      Line:= dbObjectsList[x].Strings[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;
      ScriptList.Clear;
      if dmSysTables.GetConstraintInfo(fdbIndex, ATableName, AIndexName, KeyName, CurrentTableName, CurrentFieldName,
        OtherTableName, OtherFieldName, UpdateRule, DeleteRule) then
      begin
        Line:= 'alter table ' + ATableName + ' add constraint ' + AIndexName +
          ' foreign key (' + CurrentFieldName + ') references ' +  OtherTableName  +
          ' (' + dmSysTables.GetConstraintForiegnKeyFields(OtherFieldName, dmSysTables.sqQuery) + ') ';
        if Trim(UpdateRule) <> 'RESTRICT' then
          Line:= Line + ' on update ' + Trim(UpdateRule);
        if Trim(DeleteRule) <> 'RESTRICT' then
          Line:= Line + ' on delete ' + Trim(DeleteRule);

        fQueryWindow.meQuery.Lines.Add(Line + ';');
        fQueryWindow.meQuery.Lines.Add('');
      end;


    end;

  end;

  fQueryWindow.Show;
  ScriptList.Free;
  FieldsList.Free;
  Close;

end;

procedure TfmComparison.CheckMissingDBObjects;
var
  List, ComparedList: TStringList;
  Count: Integer;
  x, i, j: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  meLog.Clear;
  meLog.Visible:= False;
  for x:= 1 to 11 do
  if ((x = 1) and cxTables.Checked) or
     ((x = 2) and cxGenerators.Checked) or
     ((x = 3) and cxTriggers.Checked) or
     ((x = 4) and cxViews.Checked) or
     ((x = 5) and cxStoredProcs.Checked) or
     ((x = 6) and cxUDFs.Checked) or
     ((x = 8) and cxDomains.Checked) or
     ((x = 9) and cxRoles.Checked) or
     ((x = 11) and cxUsers.Checked) then
  begin
    meLog.Lines.Add('');
    meLog.Lines.Add('Missing ' + dbObjects[x] + ':');

    List.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, x, Count);

    ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, x, Count);
    dbObjectsList[x].Clear;
    dbExistingObjectsList[x].Clear;
    for i:= 0 to List.Count -1 do
    if ComparedList.IndexOf(List[i]) = -1 then  // Not exist
    begin
      meLog.Lines.Add(' ' + List[i]);
      dbObjectsList[x].Add(List[i]);
    end
    else                                        // Exist
      dbExistingObjectsList[x].Add(List[i]);

  end;


  meLog.Visible:= True;
  laScript.Enabled:= True;
  ComparedList.Free;
  List.Free;
end;

procedure TfmComparison.CheckMissingFields;
var
  i, j: Integer;
  FieldsList: TStringList;
  ComparedList: TStringList;
begin
  FieldsList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  meLog.Lines.Add('');
  meLog.Lines.Add('Missing fields');
  MissingFieldsList.Clear;
  ExistFieldsList.Clear;
  for i:= 0 to dbExistingObjectsList[1].Count - 1 do
  begin
    dmSysTables.GetTableFields(fdbIndex, dbExistingObjectsList[1].Strings[i], FieldsList);
    dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, dbExistingObjectsList[1].Strings[i], ComparedList);

    // Get missing fields
    for j:= 0 to FieldsList.Count - 1 do
      if ComparedList.IndexOf(FieldsList[j]) = -1 then // Add to missing list
      begin
        meLog.Lines.Add(' ' + dbExistingObjectsList[1].Strings[i] + ': ' + FieldsList[j]);
        MissingFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + FieldsList[j]);
      end
      else                                             // Add to existing list
        ExistFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + FieldsList[j]);
  end;

  FieldsList.Free;
  ComparedList.Free;

end;

procedure TfmComparison.CheckModifiedFields;
var
  i: Integer;
  Line: string;
  ATableName: string;
  AFieldName: string;
  FieldSize: Integer;
  FieldType, DefaultValue, Description: string;
  CFieldType, CDefaultValue, CDescription: string;
  CFieldSize, CIsNull: Integer;
  NotNull, CNotNull: Boolean;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified fields');
  ModifiedFieldsList.Clear;

  for i:= 0 to ExistFieldsList.Count - 1 do
  begin
    Line:= ExistFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;

    dmSysTables.GetFieldInfo(fdbIndex, ATableName, AFieldName, FieldType, FieldSize, NotNull, DefaultValue, Description);
    dmSysTables.GetFieldInfo(cbComparedDatabase.ItemIndex, ATableName, AFieldName, CFieldType, CFieldSize, CNotNull,
      CDefaultValue, CDescription);
    if (FieldType <> CFieldType) or (FieldSize <> CFieldSize) or (NotNull <> CNotNull) or (DefaultValue <> CDefaultValue)
       or (Description <> CDescription) then
       begin
         meLog.Lines.Add(' ' + ExistFieldsList[i]);
         ModifiedFieldsList.Add(ExistFieldsList[i]);
       end;

  end;

end;

procedure TfmComparison.CheckModifiedIndices;
var
  i: Integer;
  Line: string;
  ATableName: string;
  AIndexName: string;
  FieldsList: TStringList;
  Unique, Ascending: Boolean;
  CFieldsList: TStringList;
  CUnique, CAscending: Boolean;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Indices');
  ModifiedIndicesList.Clear;
  FieldsList:= TStringList.Create;
  CFieldsList:= TStringList.Create;

  for i:= 0 to ExistIndicesList.Count - 1 do
  begin
    Line:= ExistIndicesList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AIndexName:= Line;

    dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, Unique, Ascending);
    dmSysTables.GetIndexInfo(cbComparedDatabase.ItemIndex, ATableName, AIndexName, CFieldsList, CUnique, CAscending);
    if (FieldsList.CommaText <> CFieldsList.CommaText) or (Unique <> CUnique) or (Ascending <> CAscending) then
    begin
      meLog.Lines.Add(' ' + ExistIndicesList[i]);
      ModifiedIndicesList.Add(ExistIndicesList[i]);
    end;

  end;
  FieldsList.Free;
  CFieldsList.Free;

end;

procedure TfmComparison.CheckModifiedConstraints;
var
  i: Integer;
  Line: string;
  ATableName: string;
  AConstraintName: string;
  KeyName, CurrentTableName, CurrentFieldName,
  OtherTablename, OtherFieldName, UpdateRole, DeleteRole: string;
  CKeyName, CCurrentTableName, CCurrentFieldName,
  COtherTablename, COtherFieldName, CUpdateRole, CDeleteRole: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Constraints');
  ModifiedIndicesList.Clear;

  for i:= 0 to ExistConstraintsList.Count - 1 do
  begin
    Line:= ExistConstraintsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AConstraintName:= Line;

    dmSysTables.GetConstraintInfo(fdbIndex, ATableName, AConstraintName, KeyName, CurrentTableName, CurrentFieldName,
      OtherTablename, OtherFieldName, UpdateRole, DeleteRole);
    dmSysTables.GetConstraintInfo(cbComparedDatabase.ItemIndex, ATableName, AConstraintName, CKeyName,
      CCurrentTableName, CCurrentFieldName, COtherTablename, COtherFieldName, CUpdateRole, CDeleteRole);
    if (CurrentTableName <> CCurrentTableName) or (CurrentFieldName <> CCurrentFieldName) or
       (OtherTablename <> COtherTablename) or (OtherFieldName <> COtherFieldName) or (UpdateRole <> CUpdateRole) or
       (DeleteRole <> CDeleteRole) then
    begin
      meLog.Lines.Add(' ' + ExistConstraintsList[i]);
      ModifiedConstraintsList.Add(ExistConstraintsList[i]);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedViews;
var
  i: Integer;
  ViewName: string;
  Columns, Body: string;
  CColumns, CBody: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Views');
  ModifiedViewsList.Clear;

  for i:= 0 to dbExistingObjectsList[4].Count - 1 do
  begin
    ViewName:= dbExistingObjectsList[4][i];
    fmMain.GetViewInfo(fdbIndex, ViewName, Columns, Body);
    if fmMain.GetViewInfo(cbComparedDatabase.ItemIndex, ViewName, CColumns, CBody) then
    if  (Trim(Body) <> Trim(CBody)) then
    begin
      meLog.Lines.Add(' ' + ViewName);
      ModifiedViewsList.Add(ViewName);
    end;

  end;


end;

procedure TfmComparison.InitializeQueryWindow;
begin
  fQueryWindow:= fmMain.ShowQueryWindow(cbComparedDatabase.ItemIndex, 'Database Differences');
  fQueryWindow.meQuery.ClearAll;
end;

procedure TfmComparison.ScriptMissingFields;

var
  i: Integer;
  ATableName, AFieldName: string;
  Line: string;
  FieldSize: Integer;
  NotNull: Boolean;
  DefaultValue, Description: string;
  FieldType: string;
  TableSpaces: Integer;
  FieldSpaces: Integer;
begin
  fQueryWindow.meQuery.Lines.Add('');
  fQueryWindow.meQuery.Lines.Add('-- Missing fields');
  for i:= 0 to MissingFieldsList.Count - 1 do
  begin
    Line:= MissingFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;
    dmSysTables.GetFieldInfo(fdbIndex, ATableName, AFieldName, FieldType, FieldSize, NotNull, DefaultValue, Description);

    // Script new field
    Line:= FieldType;
    if Pos('CHAR', Line) > 0 then
      Line:= Line + '(' + IntToStr(FieldSize) + ')';


    // Default value
    if Trim(DefaultValue) <> '' then
    begin
      if (Pos('CHAR', FieldType) > 0) and (Pos('''', DefaultValue) = 0) then
        Line:= Line + ' ''' + DefaultValue + ''''
      else
        Line:= Line + ' ' + DefaultValue;
    end;

    // Null/Not null
    if NotNull then
      Line:= Line + ' not null';

    TableSpaces:= 15 - Length(ATableName);
    if TableSpaces < 0 then
      TableSpaces:= 0;

    FieldSpaces:= 15 - Length(AFieldName);
    if FieldSpaces < 0 then
      FieldSpaces:= 0;

    fQueryWindow.meQuery.Lines.Add('ALTER TABLE ' + ATableName + Space(TableSpaces) +
      ' ADD ' + AFieldName + Space(FieldSpaces) + ' ' + Line + ';');

  end;

end;

procedure TfmComparison.ScriptModifiedFields;
var
  i: Integer;
  ATableName, AFieldName: string;
  Line: string;
  FieldType, DefaultValue, Description: string;
  FieldSize: Integer;
  CFieldType, CDefaultValue, CDescription: string;
  CFieldSize: Integer;
  NullFlag: string;
  NotNull, CNotNull: Boolean;
  ScriptList: TStringList;
begin
  ScriptList:= TStringList.Create;
  try
  fQueryWindow.meQuery.Lines.Add('');
  fQueryWindow.meQuery.Lines.Add('-- Modified fields');
  for i:= 0 to ModifiedFieldsList.Count - 1 do
  begin
    Line:= ModifiedFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;

    dmSysTables.GetFieldInfo(fdbIndex, ATableName, AFieldName, FieldType, FieldSize, NotNull, DefaultValue, Description);
    dmSysTables.GetFieldInfo(cbComparedDatabase.ItemIndex, ATableName, AFieldName, CFieldType, CFieldSize, CNotNull,
      cDefaultValue, cDescription);

    ScriptList.Clear;
    // check type/size change
    if (FieldType <> CFieldType) or (FieldSize <> CFieldSize) then
    begin
      Line:= 'ALTER TABLE ' + ATableName + ' ALTER ' + AFieldName + ' TYPE ' + FieldType;

      if Pos('CHAR', FieldType) > 0 then
        Line:= Line + '(' + IntToStr(FieldSize) + ')';
      Line:= Line + ';';
      ScriptList.Add(Line);
    end;

    // Allow Null
    if NotNull <> CNotNull then
    begin
      if NotNull then
        NullFlag:= '1'
      else
        NullFlag:= 'NULL';
      ScriptList.Add('UPDATE RDB$RELATION_FIELDS SET RDB$NULL_FLAG = ' + NullFlag);
      ScriptList.Add('WHERE RDB$FIELD_NAME = ''' + AFieldName + ''' AND RDB$RELATION_NAME = ''' + ATableName + ''';');
    end;

    // Description
    if Description <> CDescription then
    begin
      ScriptList.Add('UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = ''' + Description + '''');
      ScriptList.Add('where RDB$FIELD_NAME = ''' + UpperCase(AFieldName) + '''');
      ScriptList.Add('and RDB$RELATION_NAME = ''' + ATableName + ''';');
    end;

    // Default value
    if DefaultValue <> cDefaultValue then
    begin
      ScriptList.Add('UPDATE RDB$RELATION_FIELDS set RDB$Default_Source = ''' + DefaultValue + ''' ');
      ScriptList.Add('where RDB$FIELD_NAME = ''' + UpperCase(AFieldName) + '''');
      ScriptList.Add('and RDB$RELATION_NAME = ''' + ATableName + ''';');
    end;
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- ' + AFieldName + ' on ' + ATableName);
    fQueryWindow.meQuery.Lines.AddStrings(ScriptList);

  end;


  finally
    ScriptList.Free;
  end;
end;

procedure TfmComparison.ScriptModifiedIndices;
var
  i: Integer;
  ATableName, AIndexName: string;
  FieldsList: TStringList;
  Line: string;
  Unique, Ascending: Boolean;
begin
  FieldsList:= TStringList.Create;
  try
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Indices');
    for i:= 0 to ModifiedIndicesList.Count - 1 do
    begin
      Line:= ModifiedIndicesList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;
      if dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, Unique, Ascending) then
      begin
        fQueryWindow.meQuery.Lines.Add('drop index ' + AIndexName + ';');

        Line:= 'create ';
        if Unique then
          Line:= Line + 'Unique ';
        if not Ascending then
          Line:= Line + 'Descending ';

        Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

        Line:= Line + ' (' + FieldsList.CommaText + ') ;';

        fQueryWindow.meQuery.Lines.Add(Line);
        fQueryWindow.meQuery.Lines.Add('');
      end;

    end;

  finally
    FieldsList.Free;
  end;


end;

procedure TfmComparison.ScriptModifiedConstraints;
var
  i: Integer;
  ATableName, AConstraintName: string;
  Line: string;
  KeyName, CurrentTableName, CurrentFieldName,
  OtherTablename, OtherFieldName, UpdateRule, DeleteRule: string;
begin
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Constraints');
    for i:= 0 to ModifiedConstraintsList.Count - 1 do
    begin
      Line:= ModifiedConstraintsList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AConstraintName:= Line;
      if dmSysTables.GetConstraintInfo(fdbIndex, ATableName, AConstraintName, KeyName, CurrentTableName, CurrentFieldName,
          OtherTablename, OtherFieldName, UpdateRule, DeleteRule) then
      begin
        fQueryWindow.meQuery.Lines.Add('alter table ' + ATableName + ' drop constraint ' + AConstraintName + ';');

        Line:= 'alter table ' + ATableName + ' add constraint ' + AConstraintName +
          ' foreign key (' + CurrentFieldName + ') references ' +  OtherTableName  +
          ' (' + dmSysTables.GetConstraintForiegnKeyFields(OtherFieldName, dmSysTables.sqQuery) + ') ';
        if Trim(UpdateRule) <> 'RESTRICT' then
          Line:= Line + ' on update ' + Trim(UpdateRule);
        if Trim(DeleteRule) <> 'RESTRICT' then
          Line:= Line + ' on delete ' + Trim(DeleteRule);

        fQueryWindow.meQuery.Lines.Add(Line + ';');
        fQueryWindow.meQuery.Lines.Add('');
      end;

    end;

end;

procedure TfmComparison.ScriptModifiedViews;
var
  i: Integer;
  ViewName: string;
  Columns, Body: string;
begin
  fQueryWindow.meQuery.Lines.Add('');
  fQueryWindow.meQuery.Lines.Add('-- Modified Views');
  for i:= 0 to ModifiedViewsList.Count - 1 do
  begin
    ViewName:= ModifiedViewsList[i];
    fmMain.GetViewInfo(fdbIndex, ViewName, Columns, Body);
    fQueryWindow.meQuery.Lines.Add('drop view "' + ViewName + '";');
    fQueryWindow.meQuery.Lines.Add('create view "' + ViewName + '" (' + Columns + ')');
    fQueryWindow.meQuery.Lines.Add('as');
    fQueryWindow.meQuery.Lines.Add(Body);
    fQueryWindow.meQuery.Lines.Add(';');
    fQueryWindow.meQuery.Lines.Add('');
  end;

end;

procedure TfmComparison.Init(dbIndex: Integer);
var
  i: Integer;
  Servername: string;
begin
  cxTables.Checked:= True;
  cxGenerators.Checked:= True;
  cxDomains.Checked:= True;
  cxStoredProcs.Checked:= True;
  cxViews.Checked:= True;
  cxUDFs.Checked:= True;
  cxTriggers.Checked:= True;
  cxRoles.Checked:= True;
  cxUsers.Checked:= False;

  laScript.Enabled:= False;
  laComparedDatabase.Caption:= '[]';
  fdbIndex:= dbIndex;
  bbStart.Enabled:= False;
  with fmMain.RegisteredDatabases[dbIndex].RegRec do
  laDatabase.Caption:= Title + ' (' + DatabaseName + ')';
  cbComparedDatabase.Items.Clear;
  for i:= 0 to High(fmMain.RegisteredDatabases) do
  begin
    Servername:= fmMain.GetServerName(fmMain.RegisteredDatabases[i].RegRec.DatabaseName);
    cbComparedDatabase.Items.Add(ServerName + '-' + fmMain.RegisteredDatabases[i].RegRec.Title);
  end;

  for i:= 1 to High(dbObjectsList) do
    dbObjectsList[i]:= TStringList.Create;

  for i:= 1 to High(dbExistingObjectsList) do
    dbExistingObjectsList[i]:= TStringList.Create;

  MissingFieldsList:= TStringList.Create;

  ExistFieldsList:= TStringList.Create;
  ModifiedFieldsList:= TStringList.Create;

  ExistIndicesList:= TStringList.Create;
  ModifiedIndicesList:= TStringList.Create;

  ExistConstraintsList:= TStringList.Create;
  ModifiedConstraintsList:= TStringList.Create;

  ModifiedViewsList:= TStringList.Create;
end;

procedure TfmComparison.CheckMissingIndices;
var
  i, j: Integer;
  List, ComparedList: TStringList;
  TablesList: TStringList;
  PrimaryIndexName: string;
  ConstraintName: string;
  Count: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  TablesList:= TStringList.Create;
  TablesList.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);

  meLog.Lines.Add('');
  meLog.Lines.Add('Missing Indices:');
  dbObjectsList[12].Clear;
  ExistIndicesList.Clear;
  try
    for i:= 0 to TablesList.Count - 1 do
    begin
      PrimaryIndexName:= dmSysTables.GetPrimaryKeyIndexName(fdbIndex, TablesList[i], ConstraintName);
      List.Clear;
      dmSysTables.GetIndices(fdbIndex, TablesList[i], PrimaryIndexName, List);
      ComparedList.Clear;
      if dmSysTables.GetIndices(cbComparedDatabase.ItemIndex, TablesList[i], PrimaryIndexName, ComparedList) then
      begin
        for j:= 0 to List.Count - 1 do
          if ComparedList.IndexOf(List[j]) = -1 then // Add to missing indices
          begin
            meLog.Lines.Add(' ' + List[j]);
            dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
          end
          else
            ExistIndicesList.Add(TablesList[i] + ',' + List[j]); // Add to existing indices list
      end
      else // Table does not exist, all indices are missing
      if List.Count > 0 then
      for j:= 0 to List.Count - 1 do
      begin
        dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
        meLog.Lines.Add(' ' + List[j]);
      end;

    end;


  except
  on e: exception do
  begin
    meLog.Lines.Add('---- Error while comparing indices: ' + e.Message);
  end;
  end;
  List.Free;
  ComparedList.Free;
  TablesList.Free;

end;

procedure TfmComparison.CheckMissingConstraints;
var
  i, j: Integer;
  List, ComparedList: TStringList;
  TablesList: TStringList;
  Count: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  TablesList:= TStringList.Create;
  TablesList.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);
  ExistConstraintsList.Clear;

  meLog.Lines.Add('');
  meLog.Lines.Add('Missing Constraints:');
  try
    dbObjectsList[13].Clear;
    for i:= 0 to TablesList.Count - 1 do
    begin
      dmSysTables.Init(fdbIndex);
      List.Clear;
      dmSysTables.GetTableConstraints(TablesList[i], dmSysTables.sqQuery, List);
      dmSysTables.sqQuery.Close;

      dmSysTables.Init(cbComparedDatabase.ItemIndex);
      ComparedList.Clear;

      if dmSysTables.GetTableConstraints(TablesList[i], dmSysTables.sqQuery, ComparedList) then
      begin
        dmSysTables.sqQuery.Close;
        for j:= 0 to List.Count - 1 do
          if ComparedList.IndexOf(List[j]) = -1 then // Add to missing constraints
          begin
            meLog.Lines.Add(' ' + List[j]);
            dbObjectsList[13].Add(TablesList[i] + ',' + List[j]);
          end
          else
            ExistConstraintsList.Add(TablesList[i] + ',' + List[j]);
      end
      else // Table does not exist, all constraints are missing
      if List.Count > 0 then
      for j:= 0 to List.Count - 1 do
      begin
        dbObjectsList[13].Add(TablesList[i] + ',' + List[j]);
        meLog.Lines.Add(' ' + List[j]);
      end;

    end;


  except
  on e: exception do
  begin
    meLog.Lines.Add('---- Error while comparing constraints: ' + e.Message);
  end;
  end;

  List.Free;
  ComparedList.Free;
  TablesList.Free;

end;

end.

