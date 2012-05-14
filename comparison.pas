unit Comparison;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, IBConnection, sqldb;

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
    { private declarations }
  public
    procedure Init(dbIndex: Integer);
    { public declarations }
  end;

var
  fmComparison: TfmComparison;

implementation

{$R *.lfm}

{ TfmComparison }

uses Main, SysTables, Scriptdb, QueryWindow;

procedure TfmComparison.cbComparedDatabaseChange(Sender: TObject);
begin
  if cbComparedDatabase.ItemIndex <> -1 then
  with fmMain.RegisteredDatabases[cbComparedDatabase.ItemIndex].RegRec do
  begin
    laComparedDatabase.Caption:= DatabaseName;
    bbStart.Enabled:= True;
  end;
end;

procedure TfmComparison.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:= 1 to High(dbObjectsList) do
    dbObjectsList[i].Free;
end;

procedure TfmComparison.laScriptClick(Sender: TObject);
var
  x: Integer;
  i: Integer;
  ScriptList: TStringList;
  QueryWindow: TfmQueryWindow;
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
  meLog.Clear;
  ScriptList:= TStringList.Create;
  FieldsList:= TStringList.Create;

  QueryWindow:= fmMain.ShowQueryWindow(cbComparedDatabase.ItemIndex, 'Database Differences');
  QueryWindow.meQuery.ClearAll;
  dmSysTables.Init(fdbIndex);

  for x:= 1 to 13 do
  begin
    if (x = 1) and cxTables.Checked then // Tables
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      Scriptdb.ScriptTableAsCreate(fdbIndex, dbObjectsList[x].Strings[i], ScriptList);

      QueryWindow.meQuery.Lines.AddStrings(ScriptList);
      QueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 2) and cxGenerators.Checked then // Generators
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;

      QueryWindow.meQuery.Lines.Add('create generator ' + dbObjectsList[x].Strings[i] + ';');
      QueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 3) and cxTriggers.Checked then // Triggers
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      dmSysTables.ScriptTrigger(fdbIndex, dbObjectsList[x].Strings[i], ScriptList, True);

      QueryWindow.meQuery.Lines.AddStrings(ScriptList);
      QueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 4) and cxViews.Checked then // Views
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      fmMain.GetViewInfo(fdbIndex, dbObjectsList[x].Strings[i], Columns, ViewBody);
      ScriptList.Text:= Trim(ViewBody);

      QueryWindow.meQuery.Lines.Add('CREATE VIEW "' + dbObjectsList[x].Strings[i] + '" (' + Columns + ')');
      QueryWindow.meQuery.Lines.Add('AS');
      ScriptList.Text:= Trim(ViewBody);
      QueryWindow.meQuery.Lines.AddStrings(ScriptList);
      QueryWindow.meQuery.Lines.Add(' ;');
      QueryWindow.meQuery.Lines.Add('');
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

      QueryWindow.meQuery.Lines.AddStrings(ScriptList);
      QueryWindow.meQuery.Lines.Add('');
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
      QueryWindow.meQuery.Lines.AddStrings(ScriptList);
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

      QueryWindow.meQuery.Lines.Add(Line);
      QueryWindow.meQuery.Lines.Add('');
    end
    else
    if (x = 9) and cxRoles.Checked then // Roles
    for i:= 0 to dbObjectsList[x].Count - 1 do
    begin
      ScriptList.Clear;
      QueryWindow.meQuery.Lines.Add('create role ' + dbObjectsList[x].Strings[i] + ';');
      QueryWindow.meQuery.Lines.Add('');
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

        QueryWindow.meQuery.Lines.Add(Line);
        QueryWindow.meQuery.Lines.Add('');
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

        QueryWindow.meQuery.Lines.Add(Line);
        QueryWindow.meQuery.Lines.Add('');
      end;


    end;


  end;


  QueryWindow.Show;
  ScriptList.Free;
  FieldsList.Free;
  Close;

end;

procedure TfmComparison.bbStartClick(Sender: TObject);
var
  List, ComparedList: TStringList;
  Count: Integer;
  x, i, j: Integer;
  TablesList: TStringList;
  PrimaryIndexName: string;
  ConstraintName: string;
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
    meLog.Lines.Add(dbObjects[x] + ':');

    List.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, x, Count);

    ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, x, Count);
    dbObjectsList[x].Clear;
    for i:= 0 to List.Count -1 do
    if ComparedList.IndexOf(List[i]) = -1 then
    begin
      meLog.Lines.Add('Missing : ' + List[i]);
      dbObjectsList[x].Add(List[i]);
    end;

  end;

  if cxTables.Checked then
  begin
    TablesList:= TStringList.Create;
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);

    // Indices
    meLog.Lines.Add('');
    meLog.Lines.Add('Indices:');
    dbObjectsList[12].Clear;
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
            meLog.Lines.Add('Missing: ' + List[j]);
            dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
          end
      end
      else // Table does not exist, all indices are missing
      if List.Count > 0 then
      for j:= 0 to List.Count - 1 do
      begin
        dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
        meLog.Lines.Add('Missing: ' + List[j]);
      end;

    end;

    // Constraints
    meLog.Lines.Add('');
    meLog.Lines.Add('Constraints:');
    dbObjectsList[13].Clear;
    for i:= 0 to TablesList.Count - 1 do
    begin
      PrimaryIndexName:= dmSysTables.GetPrimaryKeyIndexName(fdbIndex, TablesList[i], ConstraintName);
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
            meLog.Lines.Add('Missing: ' + List[j]);
            dbObjectsList[13].Add(TablesList[i] + ',' + List[j]);
          end
      end
      else // Table does not exist, all constraints are missing
      if List.Count > 0 then
      for j:= 0 to List.Count - 1 do
      begin
        dbObjectsList[13].Add(TablesList[i] + ',' + List[j]);
        meLog.Lines.Add('Missing: ' + List[j]);
      end;

    end;


    TablesList.Free;
  end;

  meLog.Visible:= True;
  laScript.Enabled:= True;
  ComparedList.Free;
  List.Free;
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

end;

end.

