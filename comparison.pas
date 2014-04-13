unit Comparison;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, IBConnection, sqldb, QueryWindow;

const
  NumObjects = 13; //number of different objects in array below
  dbObjects: array [1 .. NumObjects] of string = ('Tables', 'Generators', 'Triggers', 'Views', 'Stored Procedures', 'UDFs',
    'Sys Tables', 'Domains', 'Roles', 'Exceptions', 'Users', 'Indices', 'Constraints');

type

  { TfmComparison }

  TfmComparison = class(TForm)
    bbClose: TBitBtn;
    bbStart: TBitBtn;
    bbCancel: TBitBtn;
    cbComparedDatabase: TComboBox;
    cxIgnoreLength: TCheckBox;
    cxRemovedObjects: TCheckBox;
    cxDomains: TCheckBox;
    cxRoles: TCheckBox;
    cxTriggers: TCheckBox;
    cxGenerators: TCheckBox;
    cxTables: TCheckBox;
    cxStoredProcs: TCheckBox;
    cxUDFs: TCheckBox;
    cxViews: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    laScript: TLabel;
    laDatabase: TLabel;
    laComparedDatabase: TLabel;
    meLog: TMemo;
    StatusBar1: TStatusBar;
    procedure bbCancelClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure cbComparedDatabaseChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure laScriptClick(Sender: TObject);
  private
    fdbIndex: Integer;
    DiffCount: Integer;
    dbObjectsList: array [1 .. NumObjects] of TStringList;
    dbExistingObjectsList: array [1 .. NumObjects] of TStringList;
    dbRemovedObjectsList: array [1 .. NumObjects] of TStringList;
    MissingFieldsList: TStringList;

    ExistFieldsList: TStringList;
    ModifiedFieldsList: TStringList;

    ExistIndicesList: TStringList;
    ModifiedIndicesList: TStringList;

    ExistConstraintsList: TStringList;
    ModifiedConstraintsList: TStringList;

    ModifiedViewsList: TStringList;
    ModifiedTriggersList: TStringList;
    ModifiedProceduresList: TStringList;
    ModifiedFunctionsList: TStringList;
    ModifiedDomainsList: TStringList;

    RemovedFieldsList: TStringList;

    fQueryWindow: TfmQueryWindow;
    fCanceled: Boolean;

    procedure CheckRemovedDBObjects;
    procedure DisplayStatus(AStatus: string);

    procedure CheckMissingIndices;
    procedure CheckMissingConstraints;
    procedure CheckMissingDBObjects;
    procedure CheckMissingFields;
    procedure CheckModifiedFields;

    procedure CheckModifiedIndices;
    procedure CheckModifiedConstraints;
    procedure CheckModifiedViews;
    procedure CheckModifiedTriggers;
    procedure CheckModifiedProcedures;
    procedure CheckModifiedFunctions;
    procedure CheckModifiedDomains;

    procedure CheckRemovedIndices;
    procedure CheckRemovedConstraints;
    procedure CheckRemovedFields;

    procedure InitializeQueryWindow;

    procedure ScriptMissingFields;

    procedure ScriptModifiedFields;
    procedure ScriptModifiedIndices;
    procedure ScriptModifiedConstraints;
    procedure ScriptModifiedViews;
    procedure ScriptModifiedTriggers;
    procedure ScriptModifiedProcedures;
    procedure ScriptModifiedFunctions;
    procedure ScriptModifiedDomains;

    procedure ScriptRemovedDBObjects;
    procedure ScriptRemovedFields;
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
var
  ComparedDBIndex: Integer;
  Connected: Boolean;
begin
  ComparedDBIndex:= cbComparedDatabase.ItemIndex;
  if ComparedDBIndex = fdbIndex then
  begin
    ShowMessage('Could not compare database with it self');
    cbComparedDatabase.ItemIndex:= -1;
  end
  else
  if (ComparedDBIndex <> -1) then
  begin
    Connected:= True;
    if (fmMain.RegisteredDatabases[ComparedDBIndex].RegRec.Password = '') then
      Connected:= fmMain.ConnectToDBAs(ComparedDBIndex);

    if not Connected then
    begin
      bbStart.Enabled:= False;
      cbComparedDatabase.ItemIndex:= -1;
    end;

    if Connected then
    with fmMain.RegisteredDatabases[ComparedDBIndex].RegRec do
    begin
      laComparedDatabase.Caption:= DatabaseName;
      bbStart.Enabled:= True;
    end;

  end;
end;

procedure TfmComparison.bbStartClick(Sender: TObject);
begin
  meLog.Clear;
  meLog.Visible:= False;
  DiffCount:= 0;
  StatusBar1.Color:= clBlue;
  DisplayStatus('Searching for missing DB Objects...');
  bbCancel.Enabled:= True;
  fCanceled:= False;

  CheckMissingDBObjects;
  Application.ProcessMessages;

  if cxTables.Checked and not fCanceled then
  begin
    CheckMissingIndices;
    if not fCanceled then
      CheckMissingConstraints;
    if not fCanceled then
      CheckMissingFields;
    if not fCanceled then
      CheckModifiedFields;
    if not fCanceled then
      CheckModifiedIndices;
    if not fCanceled then
      CheckModifiedConstraints;
  end;
  Application.ProcessMessages;

  DisplayStatus('Searching for modified db Objects...');
  if cxViews.Checked and not fCanceled then
    CheckModifiedViews;
  Application.ProcessMessages;

  if cxTriggers.Checked and not fCanceled then
    CheckModifiedTriggers;
  Application.ProcessMessages;

  if cxStoredProcs.Checked and not fCanceled then
    CheckModifiedProcedures;
  Application.ProcessMessages;

  if cxUDFs.Checked and not fCanceled then
    CheckModifiedFunctions;
  Application.ProcessMessages;

  if cxDomains.Checked and not fCanceled then
    CheckModifiedDomains;
  Application.ProcessMessages;

  DisplayStatus('Searching for removed db Objects...');

  if cxRemovedObjects.Checked and not fCanceled then
    CheckRemovedDBObjects;
  Application.ProcessMessages;

  if cxTables.Checked and cxRemovedObjects.Checked and not fCanceled then
    CheckRemovedFields;
  Application.ProcessMessages;

  StatusBar1.Color:= clDefault;
  if not fCanceled then
  begin
    DisplayStatus('Comparison Finished, ' + IntToStr(DiffCount) + ' difference(s) found');
    if DiffCount = 0 then
      meLog.Text:= 'No difference';
    meLog.Visible:= True;
  end
  else
    DisplayStatus('Canceled');
  bbCancel.Enabled:= False;

end;

procedure TfmComparison.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmComparison.bbCancelClick(Sender: TObject);
begin
  fCanceled:= True;
  laScript.Enabled:= False;
end;

procedure TfmComparison.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:= Low(dbObjectsList) to High(dbObjectsList) do
    dbObjectsList[i].Free;

  for i:= Low(dbExistingObjectsList) to High(dbExistingObjectsList) do
    dbExistingObjectsList[i].Free;

  for i:= Low(dbRemovedObjectsList) to High(dbRemovedObjectsList) do
    dbRemovedObjectsList[i].Free;

  MissingFieldsList.Free;

  ExistFieldsList.Free;
  ModifiedFieldsList.Free;

  ExistIndicesList.Free;
  ModifiedIndicesList.Free;

  ExistConstraintsList.Free;
  ModifiedConstraintsList.Free;

  ModifiedViewsList.Free;
  ModifiedTriggersList.Free;
  ModifiedProceduresList.Free;
  ModifiedFunctionsList.Free;
  ModifiedDomainsList.Free;
  RemovedFieldsList.Free;

  CloseAction:= caFree;
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
  IsPrimary: Boolean;
  ConstraintName: string;
begin
  InitializeQueryWindow;
  ScriptList:= TStringList.Create;
  FieldsList:= TStringList.Create;
  try
    if cxRemovedObjects.Checked then
    begin
      ScriptRemovedDBObjects;
      if cxTables.Checked then
        ScriptRemovedFields;
    end;

    if cxTables.Checked then
    begin
      ScriptMissingFields;
      ScriptModifiedFields;
      ScriptModifiedIndices;
      ScriptModifiedConstraints;
    end;

    if cxViews.Checked then
      ScriptModifiedViews;

    if cxTriggers.Checked then
      ScriptModifiedTriggers;

    if cxStoredProcs.Checked then
      ScriptModifiedProcedures;

    if cxUDFs.Checked then
      ScriptModifiedFunctions;

    if cxDomains.Checked then
      ScriptModifiedDomains;

    dmSysTables.Init(fdbIndex);

    fQueryWindow.meQuery.Lines.Add('');

    for x:= 1 to NumObjects do
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
        ScriptList.Add('Declare External Function "' + dbObjectsList[x].Strings[i] + '"');
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
        if dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique,
          Ascending, IsPrimary) then
        begin
          if IsPrimary then
          begin
            Line:= 'alter table ' + ATableName + #13#10 +
            'add constraint ' + AIndexName + #13#10 +
            'primary key (' + FieldsList.CommaText + ');';

          end
          else
          begin
            Line:= 'create ';
            if Unique then
              Line:= Line + 'Unique ';
            if not Ascending then
              Line:= Line + 'Descending ';

            Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

            Line:= Line + ' (' + FieldsList.CommaText + ') ;';

          end;

          fQueryWindow.meQuery.Lines.Add(Line);
          fQueryWindow.meQuery.Lines.Add('');
        end
        else
          fQueryWindow.meQuery.Lines.Add('-- Index ' + AIndexName + ' not exist on ' + ATableName);

      end
      else
      if (x = 13) and cxTables.Checked then // Constraints
      for i:= 0 to dbObjectsList[x].Count - 1 do
      begin
        Line:= dbObjectsList[x].Strings[i];
        ATableName:= copy(Line, 1, Pos(',', Line) - 1);
        System.Delete(Line, 1, Pos(',', Line));
        ConstraintName:= Line;
        ScriptList.Clear;
        if dmSysTables.GetConstraintInfo(fdbIndex, ATableName, ConstraintName, KeyName, CurrentTableName, CurrentFieldName,
          OtherTableName, OtherFieldName, UpdateRule, DeleteRule) then
        begin
          Line:= 'alter table ' + ATableName + ' add constraint ' + ConstraintName +
            ' foreign key (' + CurrentFieldName + ') references ' +  OtherTableName  +
            ' (' + dmSysTables.GetConstraintForiegnKeyFields(OtherFieldName, dmSysTables.sqQuery) + ') ';
          if Trim(UpdateRule) <> 'RESTRICT' then
            Line:= Line + ' on update ' + Trim(UpdateRule);
          if Trim(DeleteRule) <> 'RESTRICT' then
            Line:= Line + ' on delete ' + Trim(DeleteRule);

          fQueryWindow.meQuery.Lines.Add(Line + ';');
          fQueryWindow.meQuery.Lines.Add('');
        end
        else
          fQueryWindow.meQuery.Lines.Add('-- Constraint ' + ConstraintName + ' not exist on ' + ATableName);
      end;
    end;
  finally
    ScriptList.Free;
    FieldsList.Free;
  end;
  fQueryWindow.Show;

end;

procedure TfmComparison.DisplayStatus(AStatus: string);
begin
  StatusBar1.Panels[0].Text:= AStatus;
  Application.ProcessMessages;
end;

procedure TfmComparison.CheckMissingDBObjects;
var
  List, ComparedList: TStringList;
  Count: Integer;
  x, i, j: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    for x:= 1 to 11 do
    if ((x = 1) and cxTables.Checked) or
       ((x = 2) and cxGenerators.Checked) or
       ((x = 3) and cxTriggers.Checked) or
       ((x = 4) and cxViews.Checked) or
       ((x = 5) and cxStoredProcs.Checked) or
       ((x = 6) and cxUDFs.Checked) or
       ((x = 8) and cxDomains.Checked) or
       ((x = 9) and cxRoles.Checked) then
    begin
      meLog.Lines.Add('');
      meLog.Lines.Add('Checking Missing ' + dbObjects[x] + ':');

      List.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, x, Count);

      Application.ProcessMessages;
      if fCanceled then
        Exit;
      ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, x, Count);
      dbObjectsList[x].Clear;
      dbExistingObjectsList[x].Clear;
      for i:= 0 to List.Count -1 do
      if ComparedList.IndexOf(List[i]) = -1 then  // Not exist
      begin
        meLog.Lines.Add(' ' + List[i]);
        dbObjectsList[x].Add(List[i]);
        Inc(DiffCount);
      end
      else                                        // Exist
        dbExistingObjectsList[x].Add(List[i]);

    end;
    laScript.Enabled:= True;
  finally
    ComparedList.Free;
    List.Free;
  end;
end;

procedure TfmComparison.CheckRemovedDBObjects;
var
  List, ComparedList: TStringList;
  Count: Integer;
  x, i: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    for x:= 1 to 11 do
    if ((x = 1) and cxTables.Checked) or
       ((x = 2) and cxGenerators.Checked) or
       ((x = 3) and cxTriggers.Checked) or
       ((x = 4) and cxViews.Checked) or
       ((x = 5) and cxStoredProcs.Checked) or
       ((x = 6) and cxUDFs.Checked) or
       ((x = 8) and cxDomains.Checked) then
    begin
      meLog.Lines.Add('');
      meLog.Lines.Add('Checking Removed ' + dbObjects[x] + ':');

      List.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, x, Count);

      ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, x, Count);
      dbRemovedObjectsList[x].Clear;

      for i:= 0 to ComparedList.Count -1 do
      if List.IndexOf(ComparedList[i]) = -1 then  // Removed
      begin
        meLog.Lines.Add(' ' + ComparedList[i]);
        dbRemovedObjectsList[x].Add(ComparedList[i]);
        Inc(DiffCount);
      end;

    end;
    CheckRemovedIndices;
    CheckRemovedConstraints;
  finally
    ComparedList.Free;
    List.Free;
  end;
end;

procedure TfmComparison.CheckMissingFields;
var
  i, j: Integer;
  FieldsList: TStringList;
  ComparedList: TStringList;
begin
  FieldsList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    meLog.Lines.Add('');
    meLog.Lines.Add('Missing fields');
    MissingFieldsList.Clear;
    ExistFieldsList.Clear;
    for i:= 0 to dbExistingObjectsList[1].Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if fCanceled then
        Exit;

      dmSysTables.GetTableFields(fdbIndex, dbExistingObjectsList[1].Strings[i], FieldsList);
      dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, dbExistingObjectsList[1].Strings[i], ComparedList);

      // Get missing fields
      for j:= 0 to FieldsList.Count - 1 do
        if ComparedList.IndexOf(FieldsList[j]) = -1 then // Add to missing list
        begin
          meLog.Lines.Add(' ' + dbExistingObjectsList[1].Strings[i] + ': ' + FieldsList[j]);
          MissingFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + FieldsList[j]);
          Inc(DiffCount);
        end
        else                                             // Add to existing list
          ExistFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + FieldsList[j]);
    end;
  finally
    FieldsList.Free;
    ComparedList.Free;
  end;

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
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    Line:= ExistFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;

    // Read all field properties
    dmSysTables.GetFieldInfo(fdbIndex, ATableName, AFieldName, FieldType, FieldSize, NotNull, DefaultValue, Description);
    dmSysTables.GetFieldInfo(cbComparedDatabase.ItemIndex, ATableName, AFieldName, CFieldType, CFieldSize, CNotNull,
      CDefaultValue, CDescription);

    // Compare
    if (FieldType <> CFieldType) or ((FieldSize <> CFieldSize) and (not cxIgnoreLength.Checked)) or
       (NotNull <> CNotNull) or (DefaultValue <> CDefaultValue)
       or (Description <> CDescription) then
       begin
         meLog.Lines.Add(' ' + ExistFieldsList[i]);
         ModifiedFieldsList.Add(ExistFieldsList[i]);
         Inc(DiffCount);
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
  IsPrimary: Boolean;
  ConstraintName: string;
  CConstraintName: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Indices');
  ModifiedIndicesList.Clear;
  FieldsList:= TStringList.Create;
  CFieldsList:= TStringList.Create;
  try
    for i:= 0 to ExistIndicesList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if fCanceled then
        Exit;

      Line:= ExistIndicesList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;

      // Read all Index properties
      dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique, Ascending, IsPrimary);
      dmSysTables.GetIndexInfo(cbComparedDatabase.ItemIndex, ATableName, AIndexName, CFieldsList, CConstraintName,
        CUnique, CAscending, IsPrimary);

      // Compare
      if (FieldsList.CommaText <> CFieldsList.CommaText) or (Unique <> CUnique) or (Ascending <> CAscending) then
      begin
        meLog.Lines.Add(' ' + ExistIndicesList[i]);
        ModifiedIndicesList.Add(ExistIndicesList[i]);
        Inc(DiffCount);
      end;

    end;
  finally
    FieldsList.Free;
    CFieldsList.Free;
  end;

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
  Exist: Boolean;
  OtherFieldNames, COtherFieldNames: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Constraints');
  ModifiedConstraintsList.Clear;

  for i:= 0 to ExistConstraintsList.Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    Line:= ExistConstraintsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AConstraintName:= Line;

    // Read all contraint properties
    Exist:= (dmSysTables.GetConstraintInfo(fdbIndex, ATableName, AConstraintName, KeyName, CurrentTableName, CurrentFieldName,
        OtherTablename, OtherFieldName, UpdateRole, DeleteRole));

    if Exist then
      OtherFieldNames:= dmSysTables.GetConstraintForiegnKeyFields(OtherFieldName, dmSysTables.sqQuery);

    if Exist then
    Exist:= dmSysTables.GetConstraintInfo(cbComparedDatabase.ItemIndex, ATableName, AConstraintName, CKeyName,
      CCurrentTableName, CCurrentFieldName, COtherTablename, COtherFieldName, CUpdateRole, CDeleteRole);

    if Exist then
      COtherFieldNames:= dmSysTables.GetConstraintForiegnKeyFields(COtherFieldName, dmSysTables.sqQuery);

    if not Exist then
      meLog.Lines.Add(' -- Error: Constraint: ' + AConstraintName + ' not exist on table: ' + ATableName);

    // Compare
    if Exist then
    if (CurrentTableName <> CCurrentTableName) or (CurrentFieldName <> CCurrentFieldName) or
       (OtherTablename <> COtherTablename) or (OtherFieldNames <> COtherFieldNames) or (UpdateRole <> CUpdateRole) or
       (DeleteRole <> CDeleteRole) then
    begin
      meLog.Lines.Add(' ' + ExistConstraintsList[i]);
      ModifiedConstraintsList.Add(ExistConstraintsList[i]);
      Inc(DiffCount);
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
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    ViewName:= dbExistingObjectsList[4][i];
    fmMain.GetViewInfo(fdbIndex, ViewName, Columns, Body);

    // Compare
    if fmMain.GetViewInfo(cbComparedDatabase.ItemIndex, ViewName, CColumns, CBody) then
    if  (Trim(Body) <> Trim(CBody)) then
    begin
      meLog.Lines.Add(' ' + ViewName);
      ModifiedViewsList.Add(ViewName);
      Inc(DiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedTriggers;
var
  i: Integer;
  TriggerName: string;
  AfterBefor, OnTable, Event, Body : string;
  TriggerEnabled: Boolean;
  TPosition: Integer;
  CAfterBefor, COnTable, CEvent, CBody : string;
  CTriggerEnabled: Boolean;
  CTPosition: Integer;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Triggers');
  ModifiedTriggersList.Clear;

  for i:= 0 to dbExistingObjectsList[3].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    TriggerName:= dbExistingObjectsList[3][i];
    dmSysTables.GetTriggerInfo(fdbIndex, TriggerName, AfterBefor, OnTable, Event, Body, TriggerEnabled, TPosition);

    // Read all trigger properties
    if dmSysTables.GetTriggerInfo(cbComparedDatabase.ItemIndex, TriggerName, CAfterBefor, COnTable, CEvent, CBody,
      CTriggerEnabled, CTPosition) then // Compare
    if  (StringReplace(Body, ' ', '', [rfReplaceAll]) <> StringReplace(CBody, ' ', '', [rfReplaceAll]))
       or (AfterBefor <> CAfterBefor) or (TriggerEnabled <> CTriggerEnabled)
       or (TPosition <> CTPosition) then
    begin
      meLog.Lines.Add(' ' + TriggerName);
      ModifiedTriggersList.Add(TriggerName);
      Inc(DiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedProcedures;
var
  i: Integer;
  ProcName: string;
  Body : string;
  CBody : string;
  SPOwner: string;
  CSPOwner: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Procedures');
  ModifiedProceduresList.Clear;

  for i:= 0 to dbExistingObjectsList[5].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    ProcName:= dbExistingObjectsList[5][i];

    // Read procedure script
    Body:= fmMain.GetStoredProcBody(fdbIndex, ProcName, SPOwner);
    CBody:= fmMain.GetStoredProcBody(cbComparedDatabase.ItemIndex, ProcName, CSPOwner);

    // Compare
    if  (Trim(Body) <> Trim(CBody)) then
    begin
      meLog.Lines.Add(' ' + ProcName);
      ModifiedProceduresList.Add(ProcName);
      Inc(DiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedFunctions;
var
  i: Integer;
  FunctionName, ModuleName, EntryPoint, Params: string;
  CModuleName, CEntryPoint, CParams: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Functions');
  ModifiedFunctionsList.Clear;

  for i:= 0 to dbExistingObjectsList[6].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    FunctionName:= dbExistingObjectsList[6][i];

    // Get function properties
    fmMain.GetUDFInfo(fdbIndex, FunctionName, ModuleName, EntryPoint, Params);

    if fmMain.GetUDFInfo(cbComparedDatabase.ItemIndex, FunctionName, CModuleName, CEntryPoint, CParams) then
    // Compare
    if  (ModuleName <> CModuleName) or (EntryPoint <> CEntryPoint) or (Params <> CParams) then
    begin
      meLog.Lines.Add(' ' + FunctionName);
      ModifiedFunctionsList.Add(FunctionName);
      Inc(DiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedDomains;
var
  i: Integer;
  DomainName: string;
  DomainType, DefaultValue: string;
  CDomainType, CDefaultValue: string;
  DomainSize, CDomainSize: Integer;

begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified domains');
  ModifiedDomainsList.Clear;

  for i:= 0 to dbExistingObjectsList[8].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if fCanceled then
      Exit;

    DomainName:= dbExistingObjectsList[8][i];

    // Read all domain properties
    dmSysTables.GetDomainInfo(fdbIndex, DomainName, DomainType, DomainSize, DefaultValue);
    dmSysTables.GetDomainInfo(cbComparedDatabase.ItemIndex, DomainName, CDomainType, CDomainSize, CDefaultValue);

    // Compare
    if (DomainType <> CDomainType) or (DomainSize <> CDomainSize) or (DefaultValue <> CDefaultValue) then
    begin
      meLog.Lines.Add(' ' + DomainName);
      ModifiedDomainsList.Add(DomainName);
      Inc(DiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckRemovedIndices;
var
  i: Integer;
  OrigList: TStringList;
  ComparedList: TStringList;
  TablesList: TStringList;
  CTablesList: TStringList;
  Po: Integer;
begin
  OrigList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  TablesList:= TStringList.Create;
  CTablesList:= TStringList.Create;
  try
    dmSysTables.GetAllIndices(fdbIndex, OrigList, TablesList);
    dmSysTables.GetAllIndices(cbComparedDatabase.ItemIndex, ComparedList, CTablesList);
    dbRemovedObjectsList[12].Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Checking removed indices');
    for i:= 0 to ComparedList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if fCanceled then
        Break;

      Po:= OrigList.IndexOf(ComparedList[i]);
      // Compare
      if (Po = -1) or (TablesList[Po] <> CTablesList[i]) then
      begin
        dbRemovedObjectsList[12].Add(CTablesList[i] + ',' + ComparedList[i]);
        meLog.Lines.Add(' ' + CTableslist[i] + ':' + ComparedList[i]);
        Inc(DiffCount);
      end;
    end;
  finally
    OrigList.Free;
    ComparedList.Free;
    TablesList.Free;
    CTablesList.Free;
  end;
end;

procedure TfmComparison.CheckRemovedConstraints;
var
  i: Integer;
  OrigList: TStringList;
  ComparedList: TStringList;
  TablesList: TStringList;
  CTablesList: TStringList;
  Po: Integer;
begin
  OrigList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  TablesList:= TStringList.Create;
  CTablesList:= TStringList.Create;
  try
    dmSysTables.GetAllConstraints(fdbIndex, OrigList, TablesList);
    dmSysTables.GetAllConstraints(cbComparedDatabase.ItemIndex, ComparedList, CTablesList);
    dbRemovedObjectsList[13].Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Checking removed constraints');
    for i:= 0 to ComparedList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if fCanceled then
        Break;

      Po:= OrigList.IndexOf(ComparedList[i]);

      // Compare
      if (Po = -1) or (TablesList[Po] <> CTablesList[i]) then
      begin
        dbRemovedObjectsList[13].Add(CTablesList[i] + ',' + ComparedList[i]);
        meLog.Lines.Add(' ' + CTableslist[i] + ':' + ComparedList[i]);
        Inc(DiffCount);
      end;
    end;
  finally
    OrigList.Free;
    ComparedList.Free;
    TablesList.Free;
    CTablesList.Free;
  end;

end;

procedure TfmComparison.CheckRemovedFields;
var
  i, j: Integer;
  FieldsList: TStringList;
  ComparedList: TStringList;
begin
  FieldsList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    meLog.Lines.Add('');
    meLog.Lines.Add('Removed fields');
    RemovedFieldsList.Clear;
    for i:= 0 to dbExistingObjectsList[1].Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if fCanceled then
        Break;

      // Read all table fields
      dmSysTables.GetTableFields(fdbIndex, dbExistingObjectsList[1].Strings[i], FieldsList);
      dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, dbExistingObjectsList[1].Strings[i], ComparedList);

      // Get missing fields
      for j:= 0 to ComparedList.Count - 1 do
        if FieldsList.IndexOf(ComparedList[j]) = -1 then // Add to missing list
        begin
          meLog.Lines.Add(' ' + dbExistingObjectsList[1].Strings[i] + ': ' + ComparedList[j]);
          RemovedFieldsList.Add(dbExistingObjectsList[1].Strings[i] + ',' + ComparedList[j]);
          Inc(DiffCount);
        end;
    end;
  finally
    FieldsList.Free;
    ComparedList.Free;
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
  if MissingFieldsList.Count > 0 then
  begin
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Missing fields');
  end;

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
        DefaultValue:= ' ''' + DefaultValue + '''';
      if Pos('default', LowerCase(DefaultValue)) = 0 then
        DefaultValue:= ' default ' + DefaultValue;
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
  if ModifiedFieldsList.Count > 0 then
  try
    ScriptList:= TStringList.Create;
    if ModifiedFieldsList.Count > 0 then
    begin
      fQueryWindow.meQuery.Lines.Add('');
      fQueryWindow.meQuery.Lines.Add('-- Modified fields');
    end;

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
  IsPrimary: Boolean;
  ConstraintName: string;
begin
  if ModifiedIndicesList.Count > 0 then
  try
    FieldsList:= TStringList.Create;
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Indices');
    for i:= 0 to ModifiedIndicesList.Count - 1 do
    begin
      Line:= ModifiedIndicesList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;
      if dmSysTables.GetIndexInfo(fdbIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique, Ascending,
        IsPrimary) then
      begin
        if IsPrimary then
        begin
          fQueryWindow.meQuery.Lines.Add('alter table AAAA DROP constraint ' + ConstraintName + ';');

          Line:= 'alter table ' + ATableName + #13#10 +
          'add constraint ' + AIndexName + #13#10 +
          'primary key (' + FieldsList.CommaText + ')';

        end
        else // Secondary
        begin
          fQueryWindow.meQuery.Lines.Add('drop index ' + AIndexName + ';');

          Line:= 'create ';
          if Unique then
            Line:= Line + 'Unique ';
          if not Ascending then
            Line:= Line + 'Descending ';

          Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

          Line:= Line + ' (' + FieldsList.CommaText + ') ;';

        end;

        fQueryWindow.meQuery.Lines.Add(Line);
        fQueryWindow.meQuery.Lines.Add('');
      end
      else
        fQueryWindow.meQuery.Lines.Add('--Index ' + AIndexName + ' does not exist on table: ' + ATableName);

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
  if ModifiedConstraintsList.Count > 0 then
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

end;

procedure TfmComparison.ScriptModifiedViews;
var
  i: Integer;
  ViewName: string;
  Columns, Body: string;
begin
  if ModifiedViewsList.Count > 0 then
  try
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Views');
    for i:= 0 to ModifiedViewsList.Count - 1 do
    begin
      ViewName:= ModifiedViewsList[i];
      fmMain.GetViewInfo(fdbIndex, ViewName, Columns, Body);
      fQueryWindow.meQuery.Lines.Add('alter view "' + ViewName + '" (' + Columns + ')');
      fQueryWindow.meQuery.Lines.Add('as');
      fQueryWindow.meQuery.Lines.Add(Body);
      fQueryWindow.meQuery.Lines.Add(';');
      fQueryWindow.meQuery.Lines.Add('');
    end;

  except
  on e: exception do
  begin
    ShowMessage(e.Message);
  end;
  end;

end;

procedure TfmComparison.ScriptModifiedTriggers;
var
  i: Integer;
  TriggerName: string;
  List: TStringList;
begin
  if ModifiedTriggersList.Count > 0 then
  try
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Triggers');
    List:= TStringList.Create;
    for i:= 0 to ModifiedTriggersList.Count - 1 do
    begin
      TriggerName:= ModifiedTriggersList[i];
      List.Clear;
      if dmSysTables.ScriptTrigger(fdbIndex, TriggerName, List, True) then
      begin
        fQueryWindow.meQuery.Lines.Add('drop trigger ' + TriggerName + ';');
        fQueryWindow.meQuery.Lines.Add('');
        fQueryWindow.meQuery.Lines.AddStrings(List);
        fQueryWindow.meQuery.Lines.Add('');
      end;
    end;

  finally
    List.Free;
  end;

end;

procedure TfmComparison.ScriptModifiedProcedures;
var
  i: Integer;
  ProcName: string;
  Body: string;
  SOwner: string;
  List: TStringList;
begin
  if ModifiedProceduresList.Count > 0 then
  try
    List:= TStringList.Create;

    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Procedures');
    for i:= 0 to ModifiedProceduresList.Count - 1 do
    begin
      ProcName:= ModifiedProceduresList[i];
      Body:= fmMain.GetStoredProcBody(fdbIndex, ProcName, SOwner);
      fQueryWindow.meQuery.Lines.Add('alter procedure ' + ProcName + '(');
      List.Text:= Body + ';';
      fQueryWindow.meQuery.Lines.AddStrings(List);

      fQueryWindow.meQuery.Lines.Add('');
    end;


  finally
    List.Free;
  end;

end;

procedure TfmComparison.ScriptModifiedFunctions;
var
  i: Integer;
  FunctionName: string;
  ModuleName, EntryPoint, Params: string;
begin
  if ModifiedFunctionsList.Count > 0 then
  begin
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Functions (UDFs)');
  end;

  for i:= 0 to ModifiedFunctionsList.Count - 1 do
  begin
    FunctionName:= ModifiedFunctionsList[i];
    if fmMain.GetUDFInfo(fdbIndex, FunctionName, ModuleName, EntryPoint, Params) then
    begin
      fQueryWindow.meQuery.Lines.Add('');
      fQueryWindow.meQuery.Lines.Add('drop external function "' + FunctionName + '";');
      fQueryWindow.meQuery.Lines.Add('');
      fQueryWindow.meQuery.Lines.Add('DECLARE EXTERNAL FUNCTION "' + FunctionName + '"(');
      fQueryWindow.meQuery.Lines.Add(Params);
      fQueryWindow.meQuery.Lines.Add('ENTRY_POINT ''' + EntryPoint + '''');
      fQueryWindow.meQuery.Lines.Add('MODULE_NAME ''' + ModuleName + ''' ;');

      fQueryWindow.meQuery.Lines.Add('');

    end;

  end;

end;

procedure TfmComparison.ScriptModifiedDomains;
var
  i: Integer;
  DomainName: string;
  DomainType, DefaultValue: string;
  DomainSize: Integer;
  Line: string;
begin
  if ModifiedDomainsList.Count > 0 then
  begin
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Modified Domains');
  end;

  for i:= 0 to ModifiedDomainsList.Count - 1 do
  begin
    DomainName:= ModifiedDomainsList[i];
    dmSysTables.GetDomainInfo(fdbIndex, DomainName, DomainType, domainSize, DefaultValue);
    fQueryWindow.meQuery.Lines.Add('');
    Line:= 'Alter DOMAIN ' + DomainName + ' type ' + DomainType;
    if Pos('char', LowerCase(DomainType)) > 0 then
      Line:= Line + '(' + IntToStr(DomainSize) + ')';
    fQueryWindow.meQuery.Lines.Add(Line);

    if Trim(DefaultValue) <> '' then
    begin
      if (Pos('char', LowerCase(DomainType)) > 0) and (Pos('''', DefaultValue) = 0) then
        fQueryWindow.meQuery.Lines.Add('set ''' + DefaultValue + ''';')
      else
        fQueryWindow.meQuery.Lines.Add('set ' + DefaultValue + ';');
    end
    else
      fQueryWindow.meQuery.Lines.Add('set DEFAULT NULL;');


    fQueryWindow.meQuery.Lines.Add('');


  end;

end;

procedure TfmComparison.ScriptRemovedDBObjects;
var
  x: Integer;
  ObjName: string;
  i: Integer;
  IsPrimary: Boolean;
  TableName: string;
  IndexName: string;
  FieldsList: TStringList;
  Unique, Asc: Boolean;
  ConstraintName: string;
begin
  FieldsList:= TStringList.Create;
  try
    for x:= NumObjects downto 1 do
    begin
      if (x = 1) and cxTables.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Tables');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop table ' + ObjName + ';');
        end;
      end
      else
      if (x = 2) and cxGenerators.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Generators');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop generator ' + ObjName + ';');
        end;
      end
      else
      if (x = 3) and cxTriggers.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Triggers');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop trigger ' + ObjName + ';');
        end;
      end
      else
      if (x = 4) and cxViews.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Views');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop view "' + ObjName + '";');
        end;
      end
      else
      if (x = 5) and cxStoredProcs.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Procedures');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop procedure ' + ObjName + ';');
        end;
      end
      else
      if (x = 6) and cxUDFs.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Functions (UDFs)');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop external function "' + ObjName + '";');
        end;
      end
      else
      if (x = 8) and cxDomains.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Domains');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop domain ' + ObjName + ';');
        end;
      end
      else
      if (x = 9) and cxRoles.Checked then
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Roles');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          fQueryWindow.meQuery.Lines.Add('drop role ' + ObjName + ';');
        end;
      end
      else
      if (x = 12) and cxTables.Checked then // Indices are linked to tables
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Indices');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          TableName:= Copy(ObjName, 1, Pos(',', ObjName) - 1);
          System.Delete(ObjName, 1, Pos(',', ObjName));
          IndexName:= ObjName;
          dmSysTables.GetIndexInfo(cbComparedDatabase.ItemIndex, TableName, IndexName, FieldsList,
          ConstraintName, Unique, Asc, IsPrimary);
          if IsPrimary then
            fQueryWindow.meQuery.Lines.Add('alter table ' + TableName + ' DROP constraint ' + ConstraintName + ';')
          else
            fQueryWindow.meQuery.Lines.Add('drop index ' + IndexName + ';');
        end;
      end
      else
      if (x = 13) and cxTables.Checked then // Constraints are linked to tables
      begin
        if dbRemovedObjectsList[x].Count > 0 then
        begin
          fQueryWindow.meQuery.Lines.Add('');
          fQueryWindow.meQuery.Lines.Add('-- Removed Constraints');
        end;
        for i:= 0 to dbRemovedObjectsList[x].Count - 1 do
        begin
          ObjName:= dbRemovedObjectsList[x][i];
          TableName:= Copy(ObjName, 1, Pos(',', ObjName) - 1);
          System.Delete(ObjName, 1, Pos(',', ObjName));
          ConstraintName:= ObjName;
          fQueryWindow.meQuery.Lines.Add('alter table ' + TableName + ' DROP constraint ' + ConstraintName + ';');
        end;
      end;
    end;
  finally
    FieldsList.Free;
  end;

end;

procedure TfmComparison.ScriptRemovedFields;
var
  i: Integer;
  Line, ATableName, AFieldName: string;
begin
  if RemovedFieldsList.Count > 0 then
  begin
    fQueryWindow.meQuery.Lines.Add('');
    fQueryWindow.meQuery.Lines.Add('-- Removed Fields');
  end;

  for i:= 0 to RemovedFieldsList.Count - 1 do
  begin
    Line:= RemovedFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;
    fQueryWindow.meQuery.Lines.Add('alter table ' + ATableName + ' drop ' + AFieldName + ';');
  end;


end;

procedure TfmComparison.Init(dbIndex: Integer);
var
  i: Integer;
  Servername: string;
begin
  bbCancel.Enabled:= False;
  cxTables.Checked:= True;
  cxGenerators.Checked:= True;
  cxDomains.Checked:= True;
  cxStoredProcs.Checked:= True;
  cxViews.Checked:= True;
  cxUDFs.Checked:= True;
  cxTriggers.Checked:= True;
  cxRoles.Checked:= True;
  cxRemovedObjects.Checked:= False;

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

  for i:= 1 to High(dbRemovedObjectsList) do
    dbRemovedObjectsList[i]:= TStringList.Create;

  MissingFieldsList:= TStringList.Create;

  ExistFieldsList:= TStringList.Create;
  ModifiedFieldsList:= TStringList.Create;

  ExistIndicesList:= TStringList.Create;
  ModifiedIndicesList:= TStringList.Create;

  ExistConstraintsList:= TStringList.Create;
  ModifiedConstraintsList:= TStringList.Create;

  ModifiedViewsList:= TStringList.Create;
  ModifiedTriggersList:= TStringList.Create;
  ModifiedProceduresList:= TStringList.Create;
  ModifiedFunctionsList:= TStringList.Create;
  ModifiedDomainsList:= TStringList.Create;

  RemovedFieldsList:= TStringList.Create;
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
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);

    meLog.Lines.Add('');
    meLog.Lines.Add('Missing Indices:');
    dbObjectsList[12].Clear;
    ExistIndicesList.Clear;
    try
      for i:= 0 to TablesList.Count - 1 do
      begin

        // Check for cancel button press
        Application.ProcessMessages;
        if fCanceled then
          Exit;

        List.Clear;
        dmSysTables.GetIndices(fdbIndex, TablesList[i], '', List);
        ComparedList.Clear;
        if dmSysTables.GetIndices(cbComparedDatabase.ItemIndex, TablesList[i], '', ComparedList) then
        begin
          for j:= 0 to List.Count - 1 do
            if ComparedList.IndexOf(List[j]) = -1 then // Add to missing indices
            begin
              meLog.Lines.Add(' ' + List[j]);
              dbObjectsList[12].Add(TablesList[i] + ',' + List[j]);
              Inc(DiffCount);
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
          Inc(DiffCount);
        end;

      end;
    except
      on e: exception do
      begin
        meLog.Lines.Add('---- Error while comparing indices: ' + e.Message);
      end;
    end;
  finally
    List.Free;
    ComparedList.Free;
    TablesList.Free;
  end;
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
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(fdbIndex, 1, Count);
    ExistConstraintsList.Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Missing Constraints:');
    try

      dbObjectsList[13].Clear;
      for i:= 0 to TablesList.Count - 1 do
      begin

        // Check for cancel button press
        Application.ProcessMessages;
        if fCanceled then
          Exit;

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
              Inc(DiffCount);
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
          Inc(DiffCount);
        end;

      end;
    except
      on e: exception do
      begin
        meLog.Lines.Add('---- Error while comparing constraints: ' + e.Message);
      end;
    end;
  finally
    List.Free;
    ComparedList.Free;
    TablesList.Free;
  end;
end;

end.

