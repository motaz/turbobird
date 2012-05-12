unit Comparison;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, IBConnection, sqldb;

const
  dbObjects: array [1 .. 11] of string = ('Tables', 'Generators', 'Triggers', 'Views', 'Stored Procedures', 'UDFs',
    'Sys Tables', 'Domains', 'Roles', 'Exceptions', 'Users');

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
    dbObjectsList: array [1 .. 11] of TStringList;
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
begin
  meLog.Clear;
  ScriptList:= TStringList.Create;

  QueryWindow:= fmMain.ShowQueryWindow(cbComparedDatabase.ItemIndex, 'Database Differences');
  QueryWindow.meQuery.ClearAll;
  dmSysTables.Init(fdbIndex);

  for x:= 1 to 11 do
  if x <> 7 then
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
    end;

  end;

  QueryWindow.Show;
  ScriptList.Free;
  Close;

end;

procedure TfmComparison.bbStartClick(Sender: TObject);
var
  List, ComparedList: TStringList;
  Count: Integer;
  x, i: Integer;
begin
  List:= TStringList.Create;
  ComparedList:= TStringList.Create;
  meLog.Clear;
  meLog.Visible:= False;
  for x:= 1 to 11 do
  if ((x <> 7) and (X <> 10)) and
  (((x = 1) and cxTables.Checked) or
  ((x = 2) and cxGenerators.Checked) or
  ((x = 3) and cxTriggers.Checked) or
  ((x = 4) and cxViews.Checked) or
  ((x = 5) and cxStoredProcs.Checked) or
  ((x = 6) and cxUDFs.Checked) or
  ((x = 8) and cxDomains.Checked) or
  ((x = 9) and cxRoles.Checked) or
  ((x = 11) and cxUsers.Checked)) then
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

