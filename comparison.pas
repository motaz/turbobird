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
    fQueryWindow: TfmQueryWindow;
    procedure CheckMissingIndices;
    procedure CheckMissingConstraints;
    procedure CheckMissingDBObjects;
    procedure CheckMissingFields;
    procedure InitializeQueryWindow;
    procedure ScriptMissingFields;
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
    CheckMissingIndices;

  if cxTables.Checked then
    CheckMissingConstraints;

  if cxTables.Checked then
    CheckMissingFields;
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

  ScriptMissingFields;

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

        fQueryWindow.meQuery.Lines.Add(Line);
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
  for i:= 0 to dbExistingObjectsList[1].Count - 1 do
  begin
    dmSysTables.GetTableFields(fdbIndex, dbExistingObjectsList[1].Strings[i], FieldsList);
    dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, dbExistingObjectsList[1].Strings[i], ComparedList);

    // Get missing fields
    for j:= 0 to FieldsList.Count - 1 do
      if ComparedList.IndexOf(FieldsList[j]) = -1 then
      begin
        meLog.Lines.Add(' ' + dbExistingObjectsList[1].Strings[i] + ': ' + FieldsList[j]);
        MissingFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + FieldsList[j]);
      end;
  end;

  FieldsList.Free;
  ComparedList.Free;

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
  IsNull: Integer;
  DefaultValue, Description: string;
  FieldType: string;
begin
  fQueryWindow.meQuery.Lines.Add('');
  fQueryWindow.meQuery.Lines.Add('-- Missing fields');
  for i:= 0 to MissingFieldsList.Count - 1 do
  begin
    Line:= MissingFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;
    dmSysTables.GetFieldInfo(fdbIndex, ATableName, AFieldName, FieldType, FieldSize, IsNull, DefaultValue, Description);

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
    if IsNull = 1 then
      Line:= Line + ' not null';

    fQueryWindow.meQuery.Lines.Add('ALTER TABLE ' + ATableName + ' ADD ' + AFieldName + ' ' + Line + ';');

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
          if ComparedList.IndexOf(List[j]) = -1 then
          begin
            meLog.Lines.Add(' ' + List[j]);
            dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
          end
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
          if ComparedList.IndexOf(List[j]) = -1 then
          begin
            meLog.Lines.Add(' ' + List[j]);
            dbObjectsList[13].Add(TablesList[i] + ',' + List[j]);
          end
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

