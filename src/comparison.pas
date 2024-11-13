unit Comparison;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, IBConnection, sqldb, QueryWindow, LCLType, turbocommon,
  dbugintf;


type

  { TfmComparison }

  TfmComparison = class(TForm)
      bbClose: TSpeedButton;
    bbStart: TBitBtn;
    bbCancel: TBitBtn;
    cbComparedDatabase: TComboBox;
    cxIgnoreLength: TCheckBox;
    cxRemovedObjects: TCheckBox;
    cxDomains: TCheckBox;
    cxRoles: TCheckBox;
    cxExceptions: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure laScriptClick(Sender: TObject);
  private
    FDBIndex: Integer;
    FDiffCount: Integer;
    FDBObjectsList: array [0 .. NumObjects - 1] of TStringList; //matches turbocmmon.dbObjects
    FDBExistingObjectsList: array [0 .. NumObjects - 1] of TStringList; //matches turbocmmon.dbObjects
    FDBRemovedObjectsList: array [0 .. NumObjects - 1] of TStringList; //matches turbocmmon.dbObjects
    FMissingFieldsList: TStringList;

    FExistFieldsList: TStringList;
    FModifiedFieldsList: TStringList;

    FExistIndicesList: TStringList;
    FModifiedIndicesList: TStringList;

    FExistConstraintsList: TStringList;
    FModifiedConstraintsList: TStringList;

    FModifiedViewsList: TStringList;
    FModifiedTriggersList: TStringList;
    FModifiedProceduresList: TStringList;
    FModifiedFunctionsList: TStringList;
    FModifiedDomainsList: TStringList;

    FRemovedFieldsList: TStringList;

    FQueryWindow: TfmQueryWindow;
    FCanceled: Boolean;

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
    // Script missing indexes as CREATE INDEX (no primary key or other constraints)
    // Requires missing objects to be set up first
    procedure ScriptMissingIndexes;

    procedure ScriptModifiedFields;
    procedure ScriptModifiedIndices;
    procedure ScriptModifiedConstraints;
    procedure ScriptModifiedViews;
    procedure ScriptModifiedTriggers;
    procedure ScriptModifiedProcedures;
    procedure ScriptModifiedFunctions;
    procedure ScriptModifiedDomains;

    // Outputs script to remove db objects to query window
    // Requires FDBRemovedObjectsList to be filled in advance
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
  if ComparedDBIndex = FDBIndex then
  begin
    ShowMessage('Cannot compare database with itself');
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
  FDiffCount:= 0;
  StatusBar1.Color:= clBlue;
  bbCancel.Enabled:= True;
  FCanceled:= False;

  Screen.Cursor := crHourglass;
  try
    DisplayStatus('Searching for missing DB Objects...');
    CheckMissingDBObjects;
    Application.ProcessMessages;

    if cxTables.Checked and not FCanceled then
    begin
      CheckMissingIndices;
      if not FCanceled then
        CheckMissingConstraints;
      if not FCanceled then
        CheckMissingFields;
      if not FCanceled then
        CheckModifiedFields;
      if not FCanceled then
        CheckModifiedIndices;
      if not FCanceled then
        CheckModifiedConstraints;
    end;
    Application.ProcessMessages;

    DisplayStatus('Searching for modified DB Objects...');
    if cxViews.Checked and not FCanceled then
      CheckModifiedViews;
    Application.ProcessMessages;

    if cxTriggers.Checked and not FCanceled then
      CheckModifiedTriggers;
    Application.ProcessMessages;

    if cxStoredProcs.Checked and not FCanceled then
      CheckModifiedProcedures;
    Application.ProcessMessages;

    if cxUDFs.Checked and not FCanceled then
      CheckModifiedFunctions;
    Application.ProcessMessages;

    if cxDomains.Checked and not FCanceled then
      CheckModifiedDomains;
    Application.ProcessMessages;

    DisplayStatus('Searching for removed DB Objects...');

    if cxRemovedObjects.Checked and not FCanceled then
      CheckRemovedDBObjects;
    Application.ProcessMessages;

    if cxTables.Checked and cxRemovedObjects.Checked and not FCanceled then
      CheckRemovedFields;
    Application.ProcessMessages;

    StatusBar1.Color:= clDefault;
    if not FCanceled then
    begin
      DisplayStatus('Comparison Finished, ' + IntToStr(FDiffCount) + ' difference(s) found');
      if FDiffCount = 0 then
        meLog.Text:= 'No difference';
      meLog.Visible:= True;
    end
    else
      DisplayStatus('Canceled');
    bbCancel.Enabled:= False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmComparison.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmComparison.bbCancelClick(Sender: TObject);
begin
  FCanceled:= True;
  laScript.Enabled:= False;
end;

procedure TfmComparison.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:= Low(FDBObjectsList) to High(FDBObjectsList) do
    FDBObjectsList[i].Free;

  for i:= Low(FDBExistingObjectsList) to High(FDBExistingObjectsList) do
    FDBExistingObjectsList[i].Free;

  for i:= Low(FDBRemovedObjectsList) to High(FDBRemovedObjectsList) do
    FDBRemovedObjectsList[i].Free;

  CloseAction:= caFree;
end;

procedure TfmComparison.FormCreate(Sender: TObject);
begin
  FMissingFieldsList:= TStringList.Create;

  FExistFieldsList:= TStringList.Create;
  FModifiedFieldsList:= TStringList.Create;

  FExistIndicesList:= TStringList.Create;
  FModifiedIndicesList:= TStringList.Create;

  FExistConstraintsList:= TStringList.Create;
  FModifiedConstraintsList:= TStringList.Create;

  FModifiedViewsList:= TStringList.Create;
  FModifiedTriggersList:= TStringList.Create;
  FModifiedProceduresList:= TStringList.Create;
  FModifiedFunctionsList:= TStringList.Create;
  FModifiedDomainsList:= TStringList.Create;

  FRemovedFieldsList:= TStringList.Create;
end;

procedure TfmComparison.FormDestroy(Sender: TObject);
begin
  FMissingFieldsList.Free;

  FExistFieldsList.Free;
  FModifiedFieldsList.Free;

  FExistIndicesList.Free;
  FModifiedIndicesList.Free;

  FExistConstraintsList.Free;
  FModifiedConstraintsList.Free;

  FModifiedViewsList.Free;
  FModifiedTriggersList.Free;
  FModifiedProceduresList.Free;
  FModifiedFunctionsList.Free;
  FModifiedDomainsList.Free;
  FRemovedFieldsList.Free;
end;

procedure TfmComparison.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmComparison.laScriptClick(Sender: TObject);
var
  ObjectType: TObjectType;
  i: Integer;
  ObjectCounter: integer;
  ScriptList: TStringList;
  ViewBody: string;
  Columns: string;
  SPOwner: string;
  ModuleName, EntryPoint, Params: string;
  Line: string;
  CheckConstraint: string; //for domain check constraints
  CharacterSet: string;
  Collation: string;
  DomainType, DefaultValue: string;
  DomainSize: Integer;
  ExceptionMesage, ExceptionDescription, ExceptionSQL: string;
  ATableName, AIndexName: string;
  FieldsList: TStringList;
  Unique, Ascending: Boolean;
  KeyName, CurrentTableName, CurrentFieldName,
  OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string;
  IsPrimary: Boolean;
  ConstraintName: string;
begin
  {todo: if posssible merge this with create object statements generated
   in scriptdb code}
  InitializeQueryWindow;
  ScriptList:= TStringList.Create;
  FieldsList:= TStringList.Create;
  try
    // Do this in rough order of dependency so a table can depend on a domain
    if cxRemovedObjects.Checked then
    begin
      ScriptRemovedDBObjects;
      if cxTables.Checked then
        ScriptRemovedFields;
    end;

    // Tables etc can depend on domains so have domains in front
    if cxDomains.Checked then
      ScriptModifiedDomains;

    // Presumably table check constraints can depend on UDFs so put
    // UDFs fairly in front
    if cxUDFs.Checked then
      ScriptModifiedFunctions;

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

    dmSysTables.Init(FDBIndex);

    FQueryWindow.meQuery.Lines.Add('');

    // Easiest would have been to have
    // for ObjectType:= low(TObjectType) to high(TObjectType)
    // but that does not take dependencies into account
    // e.g. domains need to be created before tables that depend on them
    // So we use a manual mapping table
    for ObjectCounter:= ord(low(TObjectType)) to ord(high(TObjectType)) do
    begin
      case ObjectCounter of
        ord(otTables): ObjectType:= otRoles; //these depend on nothing
        ord(otGenerators): ObjectType:= otExceptions; //these depend on nothing
        ord(otTriggers): ObjectType:= otDomains; //tables,views,procs can depend
        ord(otViews): ObjectType:= otUDF; //table check constraints can depend on these
        ord(otStoredProcedures): ObjectType:= otSystemTables; //for completeness
        ord(otUDF): ObjectType:= otTables; //from here on, more or less follow regular order
        ord(otSystemTables): ObjectType:= otGenerators;
        ord(otDomains): ObjectType:= otTriggers;
        ord(otRoles): ObjectType:= otViews;
        ord(otExceptions): ObjectType:= otStoredProcedures;
        ord(otUsers): ObjectType:= otIndexes;
        ord(otIndexes): ObjectType:= otConstraints;
        ord(otConstraints): ObjectType:= otUsers; //for completeness
        else raise Exception.Create('Technical error: laScriptClick: update case clause with all TObjectTypes');
      end;

      if (ObjectType = otTables) and cxTables.Checked then // Tables
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;
        Scriptdb.ScriptTableAsCreate(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], ScriptList);

        FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otGenerators) and cxGenerators.Checked then // Generators
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;

        FQueryWindow.meQuery.Lines.Add('create generator ' + FDBObjectsList[ord(ObjectType)].Strings[i] + ';');
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otTriggers) and cxTriggers.Checked then // Triggers
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;
        dmSysTables.ScriptTrigger(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], ScriptList, True);

        FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otViews) and cxViews.Checked then // Views
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        fmMain.GetViewInfo(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], Columns, ViewBody);
        ScriptList.Text:= Trim(ViewBody);

        FQueryWindow.meQuery.Lines.Add('CREATE VIEW "' + FDBObjectsList[ord(ObjectType)].Strings[i] + '" (' + Columns + ')');
        FQueryWindow.meQuery.Lines.Add('AS');
        ScriptList.Text:= Trim(ViewBody);
        FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
        FQueryWindow.meQuery.Lines.Add(' ;');
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otStoredProcedures) and cxStoredProcs.Checked then // Stored proc
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Text:= fmMain.GetStoredProcBody(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], SPOwner);
        ScriptList.Insert(0, 'SET TERM ^ ;');
        ScriptList.Insert(1, 'CREATE Procedure ' + FDBObjectsList[ord(ObjectType)].Strings[i]);
        ScriptList.Add('^');
        ScriptList.Add('SET TERM ; ^');
        ScriptList.Add('');

        FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otUDF) and cxUDFs.Checked then // UDF
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;
        ScriptList.Add('Declare External Function "' + FDBObjectsList[ord(ObjectType)].Strings[i] + '"');
        if fmMain.GetUDFInfo(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], ModuleName, EntryPoint, Params) then
        begin
          RemoveParamClosing(Params);
          ScriptList.Add(Params);
          ScriptList.Add('ENTRY_POINT ' + QuotedStr(EntryPoint));
          ScriptList.Add('MODULE_NAME ' + QuotedStr(ModuleName) + ';');
          ScriptList.Add('');
        end;
        FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
      end
      else
      if (ObjectType = otDomains) and cxDomains.Checked then // Domains
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        dmSysTables.GetDomainInfo(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i], DomainType, DomainSize, DefaultValue, CheckConstraint, CharacterSet, Collation);

        Line:= 'Create Domain ' + FDBObjectsList[ord(ObjectType)].Strings[i] + ' as ' + DomainType;
        // String size
        if Pos('CHAR', DomainType) > 0 then
          Line:= Line + '(' + IntToStr(DomainSize) + ')';
        // Default value, if any
        if DefaultValue<>'' then
          Line:= Line + ' ' + DefaultValue;

        // Check constraint, if any:
        if CheckConstraint <> '' then
          Line:= Line + ' ' + CheckConstraint;

        { Character sets not supported for domains apparently at
        least not in Firebird 2.5
        if CharacterSet <> '' then
          Line:= Line + ' CHARACTER SET ' +  CharacterSet;
        }
        // Collation for text types, if any:
        if Collation <> '' then
          Line:= Line + ' COLLATE ' +  Collation;
        Line:= Line+' ;';

        FQueryWindow.meQuery.Lines.Add(Line);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otRoles) and cxRoles.Checked then // Roles
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;
        FQueryWindow.meQuery.Lines.Add('create role ' + FDBObjectsList[ord(ObjectType)].Strings[i] + ';');
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otExceptions) and cxExceptions.Checked then // Exceptions
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        ScriptList.Clear;
        FQueryWindow.meQuery.Lines.Add('-- CREATE OR ALTER EXCEPTION supported since Firebird 2.0');
        if (dmSysTables.GetExceptionInfo(FDBIndex, FDBObjectsList[ord(ObjectType)].Strings[i],
          ExceptionMesage, ExceptionDescription, ExceptionSQL, true)) then
          FQueryWindow.meQuery.Lines.Add(ExceptionSQL)
        else
          raise Exception.Create('Error scripting exception '+FDBObjectsList[ord(ObjectType)].Strings[i]);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
      if (ObjectType = otTables) and cxTables.Checked then // Indices are part of tables
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        Line:= FDBObjectsList[ord(ObjectType)].Strings[i];
        ATableName:= copy(Line, 1, Pos(',', Line) - 1);
        System.Delete(Line, 1, Pos(',', Line));
        AIndexName:= Line;
        ScriptList.Clear;
        if dmSysTables.GetIndexInfo(FDBIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique,
          Ascending, IsPrimary) then
        begin
          if IsPrimary then
          begin
            Line:= 'alter table ' + ATableName + LineEnding +
            'add constraint ' + AIndexName + LineEnding +
            'primary key (' + FieldsList.CommaText + ');';
          end
          else
          begin
            Line:= 'create ';
            if Unique then
              Line:= Line + 'unique ';
            if not Ascending then
              Line:= Line + 'descending ';

            Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

            Line:= Line + ' (' + FieldsList.CommaText + ') ;';
          end;

          FQueryWindow.meQuery.Lines.Add(Line);
          FQueryWindow.meQuery.Lines.Add('');
        end
        else
          FQueryWindow.meQuery.Lines.Add('-- Index ' + AIndexName + ' does not exist on ' + ATableName);
      end
      else
      if (ObjectType = otConstraints) and cxTables.Checked then // Constraints are part of tables
      for i:= 0 to FDBObjectsList[ord(ObjectType)].Count - 1 do
      begin
        Line:= FDBObjectsList[ord(ObjectType)].Strings[i];
        ATableName:= copy(Line, 1, Pos(',', Line) - 1);
        System.Delete(Line, 1, Pos(',', Line));
        ConstraintName:= Line;
        ScriptList.Clear;
        if dmSysTables.GetConstraintInfo(FDBIndex, ATableName, ConstraintName, KeyName, CurrentTableName, CurrentFieldName,
          OtherTableName, OtherFieldName, UpdateRule, DeleteRule) then
        begin
          Line:= 'alter table ' + ATableName + ' add constraint ' + ConstraintName +
            ' foreign key (' + CurrentFieldName + ') references ' +  OtherTableName  +
            ' (' + dmSysTables.GetConstraintForeignKeyFields(OtherFieldName, dmSysTables.sqQuery) + ') ';
          if Trim(UpdateRule) <> 'RESTRICT' then
            Line:= Line + ' on update ' + Trim(UpdateRule);
          if Trim(DeleteRule) <> 'RESTRICT' then
            Line:= Line + ' on delete ' + Trim(DeleteRule);

          FQueryWindow.meQuery.Lines.Add(Line + ';');
          FQueryWindow.meQuery.Lines.Add('');
        end
        else
          FQueryWindow.meQuery.Lines.Add('-- Constraint ' + ConstraintName + ' does not exist on ' + ATableName);
      end;
    end;
  finally
    ScriptList.Free;
    FieldsList.Free;
  end;

  // Missing indices may have dependencies on tables created in block above,
  // so wait until now to create them
  if cxTables.Checked then
  begin
    ScriptMissingIndexes;
  end;

  FQueryWindow.Show;
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
  ObjectType: TObjectType;
  i: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    for ObjectType:= Low(TObjectType) to High(TObjectType) do
    begin
      if ((ObjectType = otTables) and cxTables.Checked) or
         ((ObjectType = otGenerators) and cxGenerators.Checked) or
         ((ObjectType = otTriggers) and cxTriggers.Checked) or
         ((ObjectType = otViews) and cxViews.Checked) or
         ((ObjectType = otStoredProcedures) and cxStoredProcs.Checked) or
         ((ObjectType = otUDF) and cxUDFs.Checked) or
         {otSystemTables: system tables are not compared }
         ((ObjectType = otDomains) and cxDomains.Checked) or
         ((ObjectType = otRoles) and cxRoles.Checked) or
         ((ObjectType = otExceptions) and cxExceptions.Checked)
         {otUsers are not compared}
         then
      begin
        meLog.Lines.Add('');
        meLog.Lines.Add('Checking Missing ' + dbObjects[ord(ObjectType)] + ':');

        List.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, ObjectType, Count);

        Application.ProcessMessages;
        if FCanceled then
          Exit;
        ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, ObjectType, Count);
        FDBObjectsList[ord(ObjectType)].Clear;
        FDBExistingObjectsList[ord(ObjectType)].Clear;
        for i:= 0 to List.Count -1 do
        begin
          if ComparedList.IndexOf(List[i]) = -1 then  // Does not exist
          begin
            meLog.Lines.Add(' ' + List[i]);
            FDBObjectsList[ord(ObjectType)].Add(List[i]);
            Inc(FDiffCount);
          end
          else                                        // Exists
            FDBExistingObjectsList[ord(ObjectType)].Add(List[i]);
        end;
      end;
    end;
    laScript.Enabled:= True;
  finally
    ComparedList.Free;
    List.Free;
  end;
end;

procedure TfmComparison.CheckRemovedDBObjects;
var
  List: TStringList; //objects in current db
  ComparedList: TStringList; //objects in compared db
  Count: Integer;
  ObjectType: TObjectType;
  i: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  try
    for ObjectType:= Low(TObjectType) to High(TObjectType) do
    if ((ObjectType = otTables) and cxTables.Checked) or
       ((ObjectType = otGenerators) and cxGenerators.Checked) or
       ((ObjectType = otTriggers) and cxTriggers.Checked) or
       ((ObjectType = otViews) and cxViews.Checked) or
       ((ObjectType = otStoredProcedures) and cxStoredProcs.Checked) or
       ((ObjectType = otUDF) and cxUDFs.Checked) or
       {otSystemTables: system tables are not compared }
       ((ObjectType = otDomains) and cxDomains.Checked) or
       ((ObjectTYpe = otRoles) and cxRoles.Checked) or
       ((ObjectType = otExceptions) and cxExceptions.Checked)
       {otUsers are not checked}
       then
    begin
      meLog.Lines.Add('');
      meLog.Lines.Add('Checking Removed ' + dbObjects[ord(ObjectType)] + ':');

      List.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, ObjectType, Count);
      {$IFDEF NEVER}
      //Left for debugging
      Senddebug('current '+dbobjects[ord(ObjectType)]+': '+List.CommaText);
      Sleep(40);
      {$ENDIF}

      ComparedList.CommaText:= dmSysTables.GetDBObjectNames(cbComparedDatabase.ItemIndex, ObjectType, Count);
      {$IFDEF NEVER}
      //Left for debugging
      Senddebug('compared '+dbobjects[ord(ObjectType)]+': '+ComparedList.CommaText);
      Sleep(40);
      {$ENDIF}
      FDBRemovedObjectsList[ord(ObjectType)].Clear;

      for i:= 0 to ComparedList.Count -1 do
      begin
        if List.IndexOf(ComparedList[i]) = -1 then  // Removed
        begin
          meLog.Lines.Add(' ' + ComparedList[i]);
          FDBRemovedObjectsList[ord(ObjectType)].Add(ComparedList[i]);
          Inc(FDiffCount);
        end;
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
    FMissingFieldsList.Clear;
    FExistFieldsList.Clear;
    for i:= 0 to FDBExistingObjectsList[ord(otTables)].Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if FCanceled then
        Exit;

      dmSysTables.GetTableFields(FDBIndex, FDBExistingObjectsList[ord(otTables)].Strings[i], FieldsList);
      dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, FDBExistingObjectsList[ord(otTables)].Strings[i], ComparedList);

      // Get missing fields
      for j:= 0 to FieldsList.Count - 1 do
        if ComparedList.IndexOf(FieldsList[j]) = -1 then // Add to missing list
        begin
          meLog.Lines.Add(' ' + FDBExistingObjectsList[ord(otTables)].Strings[i] + ': ' + FieldsList[j]);
          FMissingFieldsList.Add(FDBExistingObjectsList[ord(otTables)].Strings[i] + ',' + FieldsList[j]);
          Inc(FDiffCount);
        end
        else                                             // Add to existing list
          FExistFieldsList.Add(FDBExistingObjectsList[ord(otTables)].Strings[i] + ',' + FieldsList[j]);
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
  FieldSize, CFieldSize: integer;
  FieldScale, CFieldScale: integer;
  FieldType, DefaultValue, Characterset, Collation, Description: string;
  CFieldType, CDefaultValue, CCharacterset, CCollation, CDescription: string;
  NotNull, CNotNull: Boolean;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Fields');
  FModifiedFieldsList.Clear;

  for i:= 0 to FExistFieldsList.Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    Line:= FExistFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;

    // Read all field properties
    dmSysTables.GetFieldInfo(FDBIndex, ATableName, AFieldName,
      FieldType, FieldSize, FieldScale, NotNull,
      DefaultValue, CharacterSet, Collation, Description);
    dmSysTables.GetFieldInfo(cbComparedDatabase.ItemIndex, ATableName, AFieldName,
      CFieldType, CFieldSize, CFieldScale, CNotNull,
      CDefaultValue, CCharacterSet, CCollation, CDescription);

    // Compare
    if (FieldType <> CFieldType) or
      ((FieldSize <> CFieldSize) and (not cxIgnoreLength.Checked)) or
      ((FieldScale <> CFieldScale) and (not cxIgnoreLength.Checked)) or
      (NotNull <> CNotNull) or
      (Characterset <> CCharacterset) or
      (Collation <> CCollation) or
      (DefaultValue <> CDefaultValue) or
      (Description <> CDescription) then
      begin
        meLog.Lines.Add(' ' + FExistFieldsList[i]);
        FModifiedFieldsList.Add(FExistFieldsList[i]);
        Inc(FDiffCount);
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
  FModifiedIndicesList.Clear;
  FieldsList:= TStringList.Create;
  CFieldsList:= TStringList.Create;
  try
    for i:= 0 to FExistIndicesList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if FCanceled then
        Exit;

      Line:= FExistIndicesList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;

      // Read all Index properties
      dmSysTables.GetIndexInfo(FDBIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique, Ascending, IsPrimary);
      dmSysTables.GetIndexInfo(cbComparedDatabase.ItemIndex, ATableName, AIndexName, CFieldsList, CConstraintName,
        CUnique, CAscending, IsPrimary);

      // Compare
      if (FieldsList.CommaText <> CFieldsList.CommaText) or (Unique <> CUnique) or (Ascending <> CAscending) then
      begin
        meLog.Lines.Add(' ' + FExistIndicesList[i]);
        FModifiedIndicesList.Add(FExistIndicesList[i]);
        Inc(FDiffCount);
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
  COtherTablename, COtherFieldName, CUpdateRule, CDeleteRule: string;
  Exists: Boolean;
  OtherFieldNames, COtherFieldNames: string;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Constraints');
  FModifiedConstraintsList.Clear;

  for i:= 0 to FExistConstraintsList.Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    Line:= FExistConstraintsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AConstraintName:= Line;

    // Read all contraint properties
    Exists:= (dmSysTables.GetConstraintInfo(FDBIndex, ATableName, AConstraintName, KeyName, CurrentTableName, CurrentFieldName,
        OtherTablename, OtherFieldName, UpdateRole, DeleteRole));

    if Exists then
      OtherFieldNames:= dmSysTables.GetConstraintForeignKeyFields(OtherFieldName, dmSysTables.sqQuery);

    if Exists then
    begin
      Exists:= dmSysTables.GetConstraintInfo(cbComparedDatabase.ItemIndex, ATableName,
        AConstraintName, CKeyName,
        CCurrentTableName, CCurrentFieldName,
        COtherTablename, COtherFieldName,
        CUpdateRule, CDeleteRule);
    end;

    if Exists then
      COtherFieldNames:= dmSysTables.GetConstraintForeignKeyFields(COtherFieldName, dmSysTables.sqQuery)
    else
      meLog.Lines.Add('-- Error: Constraint: ' + AConstraintName + ' does not exist on table: ' + ATableName);

    // Compare
    if Exists and (
      (CurrentTableName <> CCurrentTableName) or
      (CurrentFieldName <> CCurrentFieldName) or
      (OtherTablename <> COtherTablename) or
      (OtherFieldNames <> COtherFieldNames) or
      (UpdateRole <> CUpdateRule) or
      (DeleteRole <> CDeleteRule)
      ) then
    begin
      meLog.Lines.Add(' ' + FExistConstraintsList[i]);
      FModifiedConstraintsList.Add(FExistConstraintsList[i]);
      Inc(FDiffCount);
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
  FModifiedViewsList.Clear;

  for i:= 0 to FDBExistingObjectsList[ord(otViews)].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    ViewName:= FDBExistingObjectsList[ord(otViews)][i];
    fmMain.GetViewInfo(FDBIndex, ViewName, Columns, Body);

    // Compare
    if fmMain.GetViewInfo(cbComparedDatabase.ItemIndex, ViewName, CColumns, CBody) then
    if  (Trim(Body) <> Trim(CBody)) then
    begin
      meLog.Lines.Add(' ' + ViewName);
      FModifiedViewsList.Add(ViewName);
      Inc(FDiffCount);
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
  FModifiedTriggersList.Clear;

  for i:= 0 to FDBExistingObjectsList[ord(otTriggers)].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    TriggerName:= FDBExistingObjectsList[ord(otTriggers)][i];
    dmSysTables.GetTriggerInfo(FDBIndex, TriggerName, AfterBefor, OnTable, Event, Body, TriggerEnabled, TPosition);

    // Read all trigger properties
    if dmSysTables.GetTriggerInfo(cbComparedDatabase.ItemIndex, TriggerName, CAfterBefor, COnTable, CEvent, CBody,
      CTriggerEnabled, CTPosition) then // Compare
    if  (StringReplace(Body, ' ', '', [rfReplaceAll]) <> StringReplace(CBody, ' ', '', [rfReplaceAll]))
       or (AfterBefor <> CAfterBefor) or (TriggerEnabled <> CTriggerEnabled)
       or (TPosition <> CTPosition) then
    begin
      meLog.Lines.Add(' ' + TriggerName);
      FModifiedTriggersList.Add(TriggerName);
      Inc(FDiffCount);
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
  FModifiedProceduresList.Clear;

  for i:= 0 to FDBExistingObjectsList[ord(otStoredProcedures)].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    ProcName:= FDBExistingObjectsList[ord(otStoredProcedures)][i];

    // Read procedure script
    Body:= fmMain.GetStoredProcBody(FDBIndex, ProcName, SPOwner);
    CBody:= fmMain.GetStoredProcBody(cbComparedDatabase.ItemIndex, ProcName, CSPOwner);

    // Compare
    if  (Trim(Body) <> Trim(CBody)) then
    begin
      meLog.Lines.Add(' ' + ProcName);
      FModifiedProceduresList.Add(ProcName);
      Inc(FDiffCount);
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
  FModifiedFunctionsList.Clear;

  for i:= 0 to FDBExistingObjectsList[ord(otUDF)].Count - 1 do
  begin
    // Check for cancel button press
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    FunctionName:= FDBExistingObjectsList[ord(otUDF)][i];

    // Get function properties
    fmMain.GetUDFInfo(FDBIndex, FunctionName, ModuleName, EntryPoint, Params);

    if fmMain.GetUDFInfo(cbComparedDatabase.ItemIndex, FunctionName, CModuleName, CEntryPoint, CParams) then
    // Compare
    if  (ModuleName <> CModuleName) or (EntryPoint <> CEntryPoint) or (Params <> CParams) then
    begin
      meLog.Lines.Add(' ' + FunctionName);
      FModifiedFunctionsList.Add(FunctionName);
      Inc(FDiffCount);
    end;

  end;

end;

procedure TfmComparison.CheckModifiedDomains;
var
  i: Integer;
  CheckConstraint, CharacterSet, Collation: string;
  DomainName: string;
  DomainType, DefaultValue: string;
  CCheckConstraint, CCharacterSet, CCollation: string;
  CDomainType, CDefaultValue: string;
  DomainSize, CDomainSize: Integer;
begin
  meLog.Lines.Add('');
  meLog.Lines.Add('Modified Domains');
  FModifiedDomainsList.Clear;

  for i:= 0 to FDBExistingObjectsList[ord(otDomains)].Count - 1 do
  begin
    // Check for pressed cancel button
    Application.ProcessMessages;
    if FCanceled then
      Exit;

    DomainName:= FDBExistingObjectsList[ord(otDomains)][i];

    // Read all domain properties
    dmSysTables.GetDomainInfo(FDBIndex, DomainName, DomainType, DomainSize, DefaultValue, CheckConstraint, CharacterSet, Collation);
    dmSysTables.GetDomainInfo(cbComparedDatabase.ItemIndex, DomainName, CDomainType, CDomainSize, CDefaultValue, CCheckConstraint, CCharacterSet, CCollation);

    // Compare
    if (DomainType <> CDomainType) or
       (DomainSize <> CDomainSize) or
       (DefaultValue <> CDefaultValue) or
       (CheckConstraint <> CCheckConstraint) or
       (CharacterSet <> CCharacterSet) or
       (Collation <> CCollation) then
    begin
      meLog.Lines.Add(' ' + DomainName);
      FModifiedDomainsList.Add(DomainName);
      Inc(FDiffCount);
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
    dmSysTables.GetAllIndices(FDBIndex, OrigList, TablesList);
    dmSysTables.GetAllIndices(cbComparedDatabase.ItemIndex, ComparedList, CTablesList);
    FDBRemovedObjectsList[ord(otIndexes)].Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Checking removed indices');
    for i:= 0 to ComparedList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if FCanceled then
        Break;

      Po:= OrigList.IndexOf(ComparedList[i]);
      // Compare
      if (Po = -1) or (TablesList[Po] <> CTablesList[i]) then
      begin
        FDBRemovedObjectsList[ord(otIndexes)].Add(CTablesList[i] + ',' + ComparedList[i]);
        meLog.Lines.Add(' ' + CTableslist[i] + ':' + ComparedList[i]);
        Inc(FDiffCount);
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
    dmSysTables.GetAllConstraints(FDBIndex, OrigList, TablesList);
    dmSysTables.GetAllConstraints(cbComparedDatabase.ItemIndex, ComparedList, CTablesList);
    FDBRemovedObjectsList[ord(otConstraints)].Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Checking removed constraints');
    for i:= 0 to ComparedList.Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if FCanceled then
        Break;

      Po:= OrigList.IndexOf(ComparedList[i]);

      // Compare
      if (Po = -1) or (TablesList[Po] <> CTablesList[i]) then
      begin
        FDBRemovedObjectsList[ord(otConstraints)].Add(CTablesList[i] + ',' + ComparedList[i]);
        meLog.Lines.Add(' ' + CTableslist[i] + ':' + ComparedList[i]);
        Inc(FDiffCount);
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
    FRemovedFieldsList.Clear;
    for i:= 0 to FDBExistingObjectsList[ord(otTables)].Count - 1 do
    begin
      // Check for cancel button press
      Application.ProcessMessages;
      if FCanceled then
        Break;

      // Read all table fields
      dmSysTables.GetTableFields(FDBIndex, FDBExistingObjectsList[ord(otTables)].Strings[i], FieldsList);
      dmSysTables.GetTableFields(cbComparedDatabase.ItemIndex, FDBExistingObjectsList[ord(otTables)].Strings[i], ComparedList);

      // Get missing fields
      for j:= 0 to ComparedList.Count - 1 do
        if FieldsList.IndexOf(ComparedList[j]) = -1 then // Add to missing list
        begin
          meLog.Lines.Add(' ' + FDBExistingObjectsList[ord(otTables)].Strings[i] + ': ' + ComparedList[j]);
          FRemovedFieldsList.Add(FDBExistingObjectsList[ord(otTables)].Strings[i] + ',' + ComparedList[j]);
          Inc(FDiffCount);
        end;
    end;
  finally
    FieldsList.Free;
    ComparedList.Free;
  end;
end;

procedure TfmComparison.InitializeQueryWindow;
begin
  FQueryWindow:= fmMain.ShowQueryWindow(cbComparedDatabase.ItemIndex, 'Database Differences');
  FQueryWindow.meQuery.ClearAll;
end;

procedure TfmComparison.ScriptMissingFields;
var
  i: Integer;
  ATableName, AFieldName: string;
  Line: string;
  FieldSize, FieldScale: Integer;
  NotNull: Boolean;
  DefaultValue, Characterset, Collation, Description: string;
  FieldType: string;
  TableSpaces: Integer;
  FieldSpaces: Integer;
begin
  if FMissingFieldsList.Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Missing fields');
  end;

  for i:= 0 to FMissingFieldsList.Count - 1 do
  begin
    Line:= FMissingFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;
    dmSysTables.GetFieldInfo(FDBIndex, ATableName, AFieldName,
      FieldType, FieldSize, FieldScale, NotNull,
      DefaultValue, CharacterSet, Collation, Description);

    // Script new field
    Line:= FieldType;
    if (Pos('CHAR', Line) > 0) or (Pos('CSTRING', Line) > 0) then
      Line:= Line + '(' + IntToStr(FieldSize) + ')'
    else if (Pos('DECIMAL', Line) > 0) or (Pos('NUMERIC', Line) > 0) then
      Line:= Line + '(' +
        IntToStr(FieldSize) + ',' +
        IntToStr(FieldScale) + ')';

    // Default value
    if trim(DefaultValue) <> '' then
    begin
      if ((Pos('CHAR', FieldType) > 0) or (Pos('CSTRING', FieldType) > 0)) and
        (Pos('''', DefaultValue) = 0) then
        DefaultValue:= ' ' + QuotedStr(DefaultValue);
      if Pos('default', LowerCase(DefaultValue)) = 0 then
        DefaultValue:= ' default ' + DefaultValue;
      Line:= Line + ' ' + DefaultValue;
    end;

    // Character set/collation for string type fields
    if (Pos('CHAR', Line) > 0) or (Pos('CSTRING', Line) > 0) then
    begin
      if (Characterset<>'') then
        Line:= Line + ' character set ' + Characterset;
      if (Collation<>'') then
        Line:= Line + ' collation ' + Collation;
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

    FQueryWindow.meQuery.Lines.Add('ALTER TABLE ' + ATableName + Space(TableSpaces) +
      ' ADD ' + AFieldName + Space(FieldSpaces) + ' ' + Line + ';');
  end;
end;

procedure TfmComparison.ScriptMissingIndexes;
//todo: move create index script to generic function e.g. in SysTables or turbocommon
var
  i: integer;
  FldCount: integer;
  TableName, IndexName: string;
  Line: string;
  FieldsList: TStringList;
  ConstraintName: string;
  Unique, Ascending, IsPrimary: boolean;
begin
  // Missing indexes are stored in
  // FDBObjectsList[ord(otIndexes)] as Table,IndexName
  if FDBObjectsList[ord(otIndexes)].Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Missing indices');
  end;

  FieldsList:= TStringList.Create;
  try
    for i:= 0 to FDBObjectsList[ord(otIndexes)].Count - 1 do
    begin
      Line:= FDBObjectsList[ord(otIndexes)][i];
      TableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      IndexName:= Line;

      // Try to avoid auto-generated primary key indexes. When creating a unique
      // primary key, the index will be autogenerated once the create table script
      // is run. Todo: verify this
      if IsPrimaryIndexSystemGenerated(IndexName) then
      begin
        Line:= '-- Not scripting auto-generated primary key index ' +
           IndexName + ' (on table ' + TableName + ')';
      end
      else
      begin
        FieldsList.Clear;
        dmSysTables.GetIndexInfo(FDBIndex, TableName, IndexName,
          FieldsList, ConstraintName, Unique, Ascending, IsPrimary);

        // Script new index
        Line:= 'CREATE ';
        if Unique then
          Line:= Line + 'UNIQUE ';
        if not(Ascending) then
          Line:= Line + 'DESCENDING ';
        Line:= Line + 'INDEX ' + IndexName + ' ON ' + TableName + ' (';
        // Add all fields in list
        for FldCount:= 0 to FieldsList.Count-1 do
        begin
          if FldCount = FieldsList.Count-1 then
            Line:= Line + FieldsList[i]
          else
            Line:= Line + FieldsList[i] + ', ';
        end;
        Line:= Line + '); ';
      end;
      FQueryWindow.meQuery.Lines.Add(Line);
    end;
  finally
    FieldsList.Free;
  end;
end;

procedure TfmComparison.ScriptModifiedFields;
var
  i: Integer;
  ATableName, AFieldName: string;
  Line: string;
  FieldType, DefaultValue, Characterset, Collation, Description: string;
  cFieldType, cDefaultValue, cCharacterset, cCollation, cDescription: string;
  FieldSize, cFieldSize: integer;
  FieldScale, cFieldScale: integer;
  NullFlag: string;
  NotNull, cNotNull: Boolean;
  ScriptList: TStringList;
begin
  if FModifiedFieldsList.Count > 0 then
  try
    ScriptList:= TStringList.Create;
    if FModifiedFieldsList.Count > 0 then
    begin
      FQueryWindow.meQuery.Lines.Add('');
      FQueryWindow.meQuery.Lines.Add('-- Modified Fields');
    end;

    for i:= 0 to FModifiedFieldsList.Count - 1 do
    begin
      Line:= FModifiedFieldsList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AFieldName:= Line;

      dmSysTables.GetFieldInfo(FDBIndex, ATableName,
        AFieldName, FieldType, FieldSize, FieldScale, NotNull,
        DefaultValue, Characterset, Collation, Description);
      dmSysTables.GetFieldInfo(cbComparedDatabase.ItemIndex, ATableName,
        AFieldName, CFieldType, CFieldSize, CFieldScale, CNotNull,
        cDefaultValue, cCharacterset, cCollation, cDescription);

      ScriptList.Clear;
      // check type/size change
      if (FieldType <> CFieldType) or (FieldSize <> CFieldSize) then
      begin
        Line:= 'ALTER TABLE ' + ATableName + ' ALTER ' + AFieldName + ' TYPE ' + FieldType;

        if (Pos('CHAR', FieldType) > 0) or
           (FieldType = 'CSTRING') then
          Line:= Line + '(' + IntToStr(FieldSize) + ')'
        else if (FieldType = 'DECIMAL') or
           (FieldType = 'NUMERIC') then
          Line:= Line + '(' +
            IntToStr(FieldSize) + ',' +
            IntToStr(FieldScale) + ')';
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
        ScriptList.Add('UPDATE RDB$RELATION_FIELDS ' +
          'set RDB$DESCRIPTION = ' + QuotedStr(Description));
        ScriptList.Add('where RDB$FIELD_NAME = ' + QuotedStr(UpperCase(AFieldName)));
        ScriptList.Add('and RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ';');
      end;

      if Characterset <> cCharacterset then
        ScriptList.Add('-- WARNING: Character set changed from '+Characterset+' to '+cCharacterset+'. Please update manually (e.g. using the fbclone tool).');
      if Collation <> cCollation then
        ScriptList.Add('-- WARNING: Collation changed from '+Collation+' to '+cCollation+'. Please update manually (e.g. using the fbclone tool).');

      // Default value
      if DefaultValue <> cDefaultValue then
      begin
        ScriptList.Add('UPDATE RDB$RELATION_FIELDS ' +
          'set RDB$Default_Source = ' + QuotedStr(DefaultValue) + ' ');
        ScriptList.Add('where RDB$FIELD_NAME = ' + QuotedStr(UpperCase(AFieldName)));
        ScriptList.Add('and RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ';');
      end;
      FQueryWindow.meQuery.Lines.Add('');
      FQueryWindow.meQuery.Lines.Add('-- ' + AFieldName + ' in table ' + ATableName);
      FQueryWindow.meQuery.Lines.AddStrings(ScriptList);
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
  if FModifiedIndicesList.Count > 0 then
  try
    FieldsList:= TStringList.Create;
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Indices');
    for i:= 0 to FModifiedIndicesList.Count - 1 do
    begin
      Line:= FModifiedIndicesList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AIndexName:= Line;
      if dmSysTables.GetIndexInfo(FDBIndex, ATableName, AIndexName, FieldsList, ConstraintName, Unique, Ascending,
        IsPrimary) then
      begin
        if IsPrimary then
        begin
          FQueryWindow.meQuery.Lines.Add('alter table ' + ATableName + ' DROP constraint ' + ConstraintName + ';');

          Line:= 'alter table ' + ATableName + LineEnding +
          'add constraint ' + AIndexName + LineEnding +
          'primary key (' + FieldsList.CommaText + ')';
        end
        else // Secondary
        begin
          FQueryWindow.meQuery.Lines.Add('drop index ' + AIndexName + ';');

          Line:= 'create ';
          if Unique then
            Line:= Line + 'Unique ';
          if not Ascending then
            Line:= Line + 'Descending ';

          Line:= Line + 'index ' + AIndexName + ' on ' + ATableName;

          Line:= Line + ' (' + FieldsList.CommaText + ') ;';
        end;

        FQueryWindow.meQuery.Lines.Add(Line);
        FQueryWindow.meQuery.Lines.Add('');
      end
      else
        FQueryWindow.meQuery.Lines.Add('--Index ' + AIndexName + ' does not exist on table: ' + ATableName);
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
  if FModifiedConstraintsList.Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Constraints');
    for i:= 0 to FModifiedConstraintsList.Count - 1 do
    begin
      Line:= FModifiedConstraintsList[i];
      ATableName:= copy(Line, 1, Pos(',', Line) - 1);
      System.Delete(Line, 1, Pos(',', Line));
      AConstraintName:= Line;
      if dmSysTables.GetConstraintInfo(FDBIndex, ATableName, AConstraintName, KeyName, CurrentTableName, CurrentFieldName,
          OtherTablename, OtherFieldName, UpdateRule, DeleteRule) then
      begin
        FQueryWindow.meQuery.Lines.Add('alter table ' + ATableName + ' drop constraint ' + AConstraintName + ';');

        Line:= 'alter table ' + ATableName + ' add constraint ' + AConstraintName +
          ' foreign key (' + CurrentFieldName + ') references ' +  OtherTableName  +
          ' (' + dmSysTables.GetConstraintForeignKeyFields(OtherFieldName, dmSysTables.sqQuery) + ') ';
        if Trim(UpdateRule) <> 'RESTRICT' then
          Line:= Line + ' on update ' + Trim(UpdateRule);
        if Trim(DeleteRule) <> 'RESTRICT' then
          Line:= Line + ' on delete ' + Trim(DeleteRule);

        FQueryWindow.meQuery.Lines.Add(Line + ';');
        FQueryWindow.meQuery.Lines.Add('');
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
  if FModifiedViewsList.Count > 0 then
  try
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Views');
    for i:= 0 to FModifiedViewsList.Count - 1 do
    begin
      ViewName:= FModifiedViewsList[i];
      fmMain.GetViewInfo(FDBIndex, ViewName, Columns, Body);
      FQueryWindow.meQuery.Lines.Add('alter view "' + ViewName + '" (' + Columns + ')');
      FQueryWindow.meQuery.Lines.Add('as');
      FQueryWindow.meQuery.Lines.Add(Body);
      FQueryWindow.meQuery.Lines.Add(';');
      FQueryWindow.meQuery.Lines.Add('');
    end;

  except
  on E: Exception do
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
  if FModifiedTriggersList.Count > 0 then
  try
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Triggers');
    List:= TStringList.Create;
    for i:= 0 to FModifiedTriggersList.Count - 1 do
    begin
      TriggerName:= FModifiedTriggersList[i];
      List.Clear;
      if dmSysTables.ScriptTrigger(FDBIndex, TriggerName, List, True) then
      begin
        FQueryWindow.meQuery.Lines.Add('drop trigger ' + TriggerName + ';');
        FQueryWindow.meQuery.Lines.Add('');
        FQueryWindow.meQuery.Lines.AddStrings(List);
        FQueryWindow.meQuery.Lines.Add('');
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
  if FModifiedProceduresList.Count > 0 then
  try
    List:= TStringList.Create;

    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Procedures');
    for i:= 0 to FModifiedProceduresList.Count - 1 do
    begin
      ProcName:= FModifiedProceduresList[i];
      Body:= fmMain.GetStoredProcBody(FDBIndex, ProcName, SOwner);
      FQueryWindow.meQuery.Lines.Add('alter procedure ' + ProcName);
      List.Text:= Body + ';';
      FQueryWindow.meQuery.Lines.AddStrings(List);

      FQueryWindow.meQuery.Lines.Add('');
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
  if FModifiedFunctionsList.Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Functions (UDFs)');
  end;

  for i:= 0 to FModifiedFunctionsList.Count - 1 do
  begin
    FunctionName:= FModifiedFunctionsList[i];
    if fmMain.GetUDFInfo(FDBIndex, FunctionName, ModuleName, EntryPoint, Params) then
    begin
      FQueryWindow.meQuery.Lines.Add('');
      FQueryWindow.meQuery.Lines.Add('drop external function "' + FunctionName + '";');
      FQueryWindow.meQuery.Lines.Add('');
      FQueryWindow.meQuery.Lines.Add('DECLARE EXTERNAL FUNCTION "' + FunctionName + '"(');
      FQueryWindow.meQuery.Lines.Add(Params);
      FQueryWindow.meQuery.Lines.Add('ENTRY_POINT ' + QuotedStr(EntryPoint));
      FQueryWindow.meQuery.Lines.Add('MODULE_NAME ' + QuotedStr(ModuleName) + ' ;');

      FQueryWindow.meQuery.Lines.Add('');
    end;
  end;
end;

procedure TfmComparison.ScriptModifiedDomains;
var
  i: Integer;
  CheckConstraint, CharacterSet, Collation: string;
  DomainName: string;
  DomainType, DefaultValue: string;
  DomainSize: Integer;
  Line: string;
begin
  if FModifiedDomainsList.Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Modified Domains');
  end;

  for i:= 0 to FModifiedDomainsList.Count - 1 do
  begin
    DomainName:= FModifiedDomainsList[i];
    dmSysTables.GetDomainInfo(FDBIndex, DomainName, DomainType, DomainSize, DefaultValue, CheckConstraint, CharacterSet, Collation);
    FQueryWindow.meQuery.Lines.Add('');
    Line:= 'Alter DOMAIN ' + DomainName + ' type ' + DomainType;
    if Pos('char', LowerCase(DomainType)) > 0 then
      Line:= Line + '(' + IntToStr(DomainSize) + ')';
    FQueryWindow.meQuery.Lines.Add(Line);

    // todo: verify if this check constraint clause works correctly
    if Trim(CheckConstraint) <> '' then
      FQueryWindow.meQuery.Lines.Add(CheckConstraint);

    if Trim(CharacterSet) <> '' then
      FQueryWindow.meQuery.Lines.Add('characterset ' + CharacterSet);

    if Trim(Collation) <> '' then
      FQueryWindow.meQuery.Lines.Add('collation ' + Collation);

    if Trim(DefaultValue) <> '' then
    begin
      if (Pos('char', LowerCase(DomainType)) > 0) and (Pos('''', DefaultValue) = 0) then
        FQueryWindow.meQuery.Lines.Add('set ''' + DefaultValue + ''';')
      else
        FQueryWindow.meQuery.Lines.Add('set ' + DefaultValue + ';');
    end
    else
      FQueryWindow.meQuery.Lines.Add('set DEFAULT NULL;');

    FQueryWindow.meQuery.Lines.Add('');
  end;
end;

procedure TfmComparison.ScriptRemovedDBObjects;
var
  ObjectType: TObjectType;
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
    for ObjectType:= high(TObjectType) downto low(TObjectType) do
    begin
      if (ObjectType = otTables) and cxTables.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Tables');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop table ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otGenerators) and cxGenerators.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Generators');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop generator ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otTriggers) and cxTriggers.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Triggers');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop trigger ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otViews) and cxViews.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Views');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop view "' + ObjName + '";');
        end;
      end
      else
      if (ObjectType = otStoredProcedures) and cxStoredProcs.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Procedures');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop procedure ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otUDF) and cxUDFs.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Functions (UDFs)');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop external function "' + ObjName + '";');
        end;
      end
      else
      if (ObjectType = otDomains) and cxDomains.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Domains');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop domain ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otRoles) and cxRoles.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Roles');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop role ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otExceptions) and cxExceptions.Checked then
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Exceptions');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          FQueryWindow.meQuery.Lines.Add('drop exception ' + ObjName + ';');
        end;
      end
      else
      if (ObjectType = otIndexes) and cxTables.Checked then // Indices are linked to tables
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Indices');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          TableName:= Copy(ObjName, 1, Pos(',', ObjName) - 1);
          System.Delete(ObjName, 1, Pos(',', ObjName));
          IndexName:= ObjName;
          dmSysTables.GetIndexInfo(cbComparedDatabase.ItemIndex, TableName, IndexName, FieldsList,
          ConstraintName, Unique, Asc, IsPrimary);
          if IsPrimary then
            FQueryWindow.meQuery.Lines.Add('alter table ' + TableName + ' DROP constraint ' + ConstraintName + ';')
          else
            FQueryWindow.meQuery.Lines.Add('drop index ' + IndexName + ';');
        end;
      end
      else
      if (ObjectType = otConstraints) and cxTables.Checked then // Constraints are linked to tables
      begin
        if FDBRemovedObjectsList[ord(ObjectType)].Count > 0 then
        begin
          FQueryWindow.meQuery.Lines.Add('');
          FQueryWindow.meQuery.Lines.Add('-- Removed Constraints');
        end;
        for i:= 0 to FDBRemovedObjectsList[ord(ObjectType)].Count - 1 do
        begin
          ObjName:= FDBRemovedObjectsList[ord(ObjectType)][i];
          TableName:= Copy(ObjName, 1, Pos(',', ObjName) - 1);
          System.Delete(ObjName, 1, Pos(',', ObjName));
          ConstraintName:= ObjName;
          FQueryWindow.meQuery.Lines.Add('alter table ' + TableName + ' DROP constraint ' + ConstraintName + ';');
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
  if FRemovedFieldsList.Count > 0 then
  begin
    FQueryWindow.meQuery.Lines.Add('');
    FQueryWindow.meQuery.Lines.Add('-- Removed Fields');
  end;

  for i:= 0 to FRemovedFieldsList.Count - 1 do
  begin
    Line:= FRemovedFieldsList[i];
    ATableName:= copy(Line, 1, Pos(',', Line) - 1);
    System.Delete(Line, 1, Pos(',', Line));
    AFieldName:= Line;
    FQueryWindow.meQuery.Lines.Add('alter table ' + ATableName + ' drop ' + AFieldName + ';');
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
  cxExceptions.Checked:= True;
  cxRemovedObjects.Checked:= False;

  laScript.Enabled:= False;
  laComparedDatabase.Caption:= '[]';
  FDBIndex:= dbIndex;
  bbStart.Enabled:= False;
  with fmMain.RegisteredDatabases[dbIndex].RegRec do
    laDatabase.Caption:= Title + ' (' + DatabaseName + ')';
  cbComparedDatabase.Items.Clear;
  for i:= 0 to High(fmMain.RegisteredDatabases) do
  begin
    Servername:= fmMain.GetServerName(fmMain.RegisteredDatabases[i].RegRec.DatabaseName);
    cbComparedDatabase.Items.Add(ServerName + '-' + fmMain.RegisteredDatabases[i].RegRec.Title);
  end;

  for i:= Low(FDBObjectsList) to High(FDBObjectsList) do
    FDBObjectsList[i]:= TStringList.Create;

  for i:= Low(FDBExistingObjectsList) to High(FDBExistingObjectsList) do
    FDBExistingObjectsList[i]:= TStringList.Create;

  for i:= Low(FDBRemovedObjectsList) to High(FDBRemovedObjectsList) do
    FDBRemovedObjectsList[i]:= TStringList.Create;
end;

procedure TfmComparison.CheckMissingIndices;
var
  i, j: Integer;
  IndexList, ComparedList: TStringList;
  TablesList: TStringList;
  Count: Integer;
begin
  IndexList:= TStringList.Create;
  ComparedList:= TStringList.Create;
  TablesList:= TStringList.Create;
  try
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, otTables, Count);

    meLog.Lines.Add('');
    meLog.Lines.Add('Missing Indices:');
    FDBObjectsList[ord(otIndexes)].Clear;
    FExistIndicesList.Clear;
    try
      // Go through each table and compare indexes:
      for i:= 0 to TablesList.Count - 1 do
      begin
        // Check for cancel button press
        Application.ProcessMessages;
        if FCanceled then
          Exit;

        IndexList.Clear;
        dmSysTables.GetIndices(FDBIndex, TablesList[i], '', IndexList);
        ComparedList.Clear;
        if dmSysTables.GetIndices(cbComparedDatabase.ItemIndex, TablesList[i], '', ComparedList) then
        begin
          for j:= 0 to IndexList.Count - 1 do
            if ComparedList.IndexOf(IndexList[j]) = -1 then // Add to missing indices
            begin
              meLog.Lines.Add(' ' + IndexList[j]);
              FDBObjectsList[ord(otIndexes)].Add(TablesList[i] + ',' + IndexList[j]);
              Inc(FDiffCount);
            end
            else
            begin
              FExistIndicesList.Add(TablesList[i] + ',' + IndexList[j]); // Add to existing indices IndexList
            end;
        end
        else // Table does not exist so all indices are missing
        if IndexList.Count > 0 then
        for j:= 0 to IndexList.Count - 1 do
        begin
          FDBObjectsList[ord(otIndexes)].Add(TablesList[i] + ',' + IndexList[j]);
          meLog.Lines.Add(' ' + IndexList[j]);
          Inc(FDiffCount);
        end;
      end;
    except
      on E: Exception do
      begin
        meLog.Lines.Add('-- Error while comparing indices: ' + E.Message);
      end;
    end;
  finally
    IndexList.Free;
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
    TablesList.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, otTables, Count);
    FExistConstraintsList.Clear;

    meLog.Lines.Add('');
    meLog.Lines.Add('Missing Constraints:');
    try

      FDBObjectsList[ord(otConstraints)].Clear;
      for i:= 0 to TablesList.Count - 1 do
      begin

        // Check for cancel button press
        Application.ProcessMessages;
        if FCanceled then
          Exit;

        dmSysTables.Init(FDBIndex);
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
              FDBObjectsList[ord(otConstraints)].Add(TablesList[i] + ',' + List[j]);
              Inc(FDiffCount);
            end
            else
              FExistConstraintsList.Add(TablesList[i] + ',' + List[j]);
        end
        else // Table does not exist, all constraints are missing
        if List.Count > 0 then
        for j:= 0 to List.Count - 1 do
        begin
          FDBObjectsList[ord(otConstraints)].Add(TablesList[i] + ',' + List[j]);
          meLog.Lines.Add(' ' + List[j]);
          Inc(FDiffCount);
        end;
      end;
    except
      on E: Exception do
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

