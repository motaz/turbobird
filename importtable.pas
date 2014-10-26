unit importtable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, SynEdit, SynHighlighterSQL, sqldb,
  turbocommon, fileimport;

type

  { TfmImportTable }

  TfmImportTable = class(TForm)
    bbImport: TBitBtn;
    bbClose: TBitBtn;
    btnAddMapping: TButton;
    btnDeleteMapping: TButton;
    btnPrepare: TButton;
    btnSourceFileOpen: TButton;
    chkSkipFirstRow: TCheckBox;
    chkTabDelimiter: TCheckBox;
    dlgSourceOpen: TOpenDialog;
    edSourceFile: TEdit;
    edDelimiter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    MappingGrid: TStringGrid;
    MappingPanel: TPanel;
    cbSourceField: TComboBox;
    cbDestField: TComboBox;
    SourcePanel: TPanel;
    procedure bbImportClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure btnAddMappingClick(Sender: TObject);
    procedure btnDeleteMappingClick(Sender: TObject);
    procedure btnPrepareClick(Sender: TObject);
    procedure btnSourceFileOpenClick(Sender: TObject);
    procedure chkTabDelimiterEditingDone(Sender: TObject);
    procedure edDelimiterEditingDone(Sender: TObject);
    procedure edSourceFileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDestinationQuery: TSQLQuery;
    FImporter: TFileImport;
    FDestDB: string; //destination database
    FDestTable: string; //destination table
    FDestIndex: Integer; //index of destination database
    // Load source and destination fields in mapping comboboxes
    procedure LoadMappingCombos;
    // Opens destination table query
    procedure OpenDestinationTable;
    // Load/update mapping grid
    procedure UpdateMappingGrid;
    { private declarations }
  public
    { public declarations }
    procedure Init(DestinationIndex: Integer; DestinationTableName: string);
  end; 

var
  fmImportTable: TfmImportTable;

implementation

{ TfmImportTable }

uses main, SysTables, EnterPass, Reg;


procedure TfmImportTable.edSourceFileEditingDone(Sender: TObject);
begin
  if edSourceFile.Text='' then exit;
  try
    FImporter.FileName:=edSourceFile.Text;
    if FImporter.Delimiter = #9 then //tab
    begin
      chkTabDelimiter.Checked:=true;
      edDelimiter.Text:='<TAB>';
      edDelimiter.Enabled:=false;
    end
    else
    begin
      edDelimiter.Text:=FImporter.Delimiter;
      edDelimiter.Enabled:=true;
    end;
  except
    edDelimiter.Enabled:=true;
    edDelimiter.Text:='';
    chkTabDelimiter.Checked:=false;
  end;
end;

procedure TfmImportTable.FormCreate(Sender: TObject);
begin
  FImporter:=TFileImport.Create;
end;

procedure TfmImportTable.FormDestroy(Sender: TObject);
begin
  FImporter.Free;
  if assigned(FDestinationQuery) then
    FreeAndNil(FDestinationQuery);
end;

procedure TfmImportTable.LoadMappingCombos;
var
  i: integer;
begin
  cbSourceField.Clear;
  cbDestField.Clear;
  for i:=0 to FImporter.SourceFields.Count-1 do
  begin
    cbSourceField.Items.Add(FImporter.SourceFields[i]);
  end;
  if cbSourceField.Items.Count > -1 then
    cbSourceField.ItemIndex := 0;

  if assigned(FDestinationQuery) then
  begin
    if not(FDestinationQuery.Active) then FDestinationQuery.Open;
    for i:=0 to FDestinationQuery.FieldCount-1 do
    begin
      cbDestField.Items.Add(FDestinationQuery.Fields[i].FieldName);
    end;
  end;
  if cbDestField.Items.Count > -1 then
    cbDestField.ItemIndex := 0;
end;

procedure TfmImportTable.OpenDestinationTable;
var
  i: Integer;
  Statement: string;
  Num: Integer;
begin
  // Enter password if it is not saved
  with fmMain.RegisteredDatabases[FDestIndex] do
  begin
    // If client/server password is empty, get it from user:
    if (IBConnection.HostName<>'') and
      (IBConnection.Password = '') then
    begin
      if fmEnterPass.ShowModal = mrOk then
      begin
        if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
          RegRec.Charset) then
        begin
          with fmMain do
          begin
            RegisteredDatabases[FDestIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
            RegisteredDatabases[FDestIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
            RegisteredDatabases[FDestIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
          end;
        end
        else
        begin
          exit;
        end;
      end;
    end;
    if not(assigned(FDestinationQuery)) then
      FDestinationQuery:=TSQLQuery.Create(nil);
    FDestinationQuery.Close;
    FDestinationQuery.DataBase:=IBConnection;
    FDestinationQuery.Transaction:=SQLTrans;
    FDestinationQuery.ParseSQL; //belts and braces - generate InsertSQL
    FDestinationQuery.SQL.Text:='select * from '+FDestTable;
    FDestinationQuery.Open;
  end;
end;

procedure TfmImportTable.UpdateMappingGrid;
var
  i: integer;
  MappingCount: integer;
begin
  // MappingCount will map fields if necessary so we need destination fields
  if FImporter.DestinationFields.Count=0 then
  begin
    if not(assigned(FDestinationQuery)) then
      raise Exception.Create('Cannot update mapping info without valid destination query.');
    MappingCount := FDestinationQuery.Fields.Count;
    for i := 0 to MappingCount - 1 do
    begin
      FImporter.DestinationFields.Add(FDestinationQuery.Fields[i].FieldName);
    end;
  end;

  MappingCount:=FImporter.MappingCount;
  MappingGrid.RowCount:=MappingCount;
  for i:=0 to MappingCount-1 do
  begin
    MappingGrid.Cells[0,i]:=FImporter.Mapping[i].SourceField;
    MappingGrid.Cells[1,i]:=FImporter.Mapping[i].DestinationField;
    {
    // Grid InsertRowWithValues ssems to fail if there are no columns or rows
    // even if there are it doesn't always work
    MappingGrid.InsertRowWithValues(0,
      [FImporter.Mapping[i].SourceField,
      FImporter.Mapping[i].DestinationField]);
    }
  end;
end;

procedure TfmImportTable.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmImportTable.btnAddMappingClick(Sender: TObject);
begin
  if FImporter.AddMapping(cbSourceField.Text, cbDestField.Text) then
    UpdateMappingGrid;
end;

procedure TfmImportTable.btnDeleteMappingClick(Sender: TObject);
begin
  // Delete mapping for selected destination field
  if FImporter.DeleteMapping(MappingGrid.Cells[1,MappingGrid.Row]) then
    UpdateMappingGrid;
end;

procedure TfmImportTable.btnPrepareClick(Sender: TObject);
begin
  // Only try if valid import file specified
  if (FImporter.FileName<>'') and
    (FImporter.Delimiter<>#0) then
  begin
    OpenDestinationTable;
    LoadMappingCombos;
    UpdateMappingGrid;
  end;
end;

procedure TfmImportTable.btnSourceFileOpenClick(Sender: TObject);
begin
  if dlgSourceOpen.Execute then
    edSourceFile.Text:=dlgSourceOpen.FileName;
  // Process delimiters etc
  edSourceFileEditingDone(Sender);
end;

procedure TfmImportTable.chkTabDelimiterEditingDone(Sender: TObject);
begin
  if chkTabDelimiter.Checked then
  begin
    edDelimiter.Text:='<TAB>';
    edDelimiter.Enabled:=false;
    FImporter.Delimiter:=#9;
  end
  else
  begin
    edDelimiter.Enabled:=true;
    FImporter.Delimiter:=#0; //more or less a placeholder
  end;
end;

procedure TfmImportTable.edDelimiterEditingDone(Sender: TObject);
begin
  case length(edDelimiter.Text) of
    0: FImporter.Delimiter:=#0; //placeholder
    1: FImporter.Delimiter:=edDelimiter.Text[1];
    else
    begin
      ShowMessage('Delimiter must be 1 character only.');
      exit;
    end;
  end;
end;

procedure TfmImportTable.bbImportClick(Sender: TObject);
var
  DestColumn: string;
  i: Integer;
  Num: Integer;
begin
  if not(assigned(FDestinationQuery)) and (FDestinationQuery.Active=false) then
    exit; //no destination fields

  Screen.Cursor:=crHourGlass;
  bbClose.Enabled:=false;
  bbImport.Enabled:=false;
  try
    // Skip first row if necessary
    if chkSkipFirstRow.Checked then
    begin
      if not(FImporter.ReadRow) then
        exit; //error: end of file?
    end;

    // Enter password if it is not saved... and we're not connected to an embedded
    // database
    with fmMain.RegisteredDatabases[FDestIndex] do
    begin
      if (IBConnection.HostName<>'') and (IBConnection.Password = '') then
      begin
        if fmEnterPass.ShowModal = mrOk then
        begin
          if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
            RegRec.Charset) then
          begin
            with fmMain do
            begin
              RegisteredDatabases[FDestIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
              RegisteredDatabases[FDestIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
              RegisteredDatabases[FDestIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
            end
          end
          else
          begin
            exit;
          end;
        end;
      end;

      // Start import
      if SQLTrans.Active then
        SQLTrans.RollBack;
      SQLTrans.StartTransaction;
      FDestinationQuery.Open;
      Num:=0;
      try
        while FImporter.ReadRow do
        begin
          FDestinationQuery.Insert;
          for I:=0 to FImporter.MappingCount-1 do
          begin
            DestColumn:=FImporter.Mapping[I].DestinationField;
            // Note: csv import sees everything as strings so let the db convert if possible
            FDestinationQuery.Fields.FieldByName(DestColumn).AsString:=FImporter.GetData(i);
          end;
          FDestinationQuery.Post;
          Inc(Num);
        end;
        FDestinationQuery.ApplyUpdates;
        // could be also done after e.g. every 1000 records for
        // higher performance
        SQLTrans.Commit;
        FDestinationQuery.Close;
        Screen.Cursor:=crDefault; // for message
        ShowMessage(IntToStr(Num) + ' record(s) have been imported');
      except
        on E: Exception do
        begin
          MessageDlg('Error while importing: ' + e.Message, mtError, [mbOk], 0);
          SQLTrans.Rollback;
        end;
      end;
    end;
  finally
    bbClose.Enabled:=true;
    bbImport.Enabled:=true;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfmImportTable.Init(DestinationIndex: Integer; DestinationTableName: string);
var
  i: Integer;
  Count: Integer;
begin
  FDestIndex:=DestinationIndex;
  FDestDB:=fmMain.RegisteredDatabases[FDestIndex].RegRec.Title;
  FDestTable:=DestinationTableName;
  Caption:='Import '+FDestTable;
end;

initialization
  {$I ImportTable.lrs}

end.

