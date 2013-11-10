unit QueryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, PairSplitter, StdCtrls, Buttons,
  DBGrids, Menus, ComCtrls, SynEdit, SynHighlighterSQL, Reg, sqlscript,
  SynEditTypes, SynCompletion, Clipbrd, grids, DbCtrls, types, LCLType;

type

  { TQueryThread }

  TQueryThread = class(TThread)
    private
      fSQLQuery: TSQLQuery;
      fTrans: TSQLTransaction;
      fConnection: TIBConnection;

    public
      Error: Boolean;
      ErrorMsg: string;
      fTerminated: Boolean;
      fType: string;
      fStatement: string;
      property Query: TSQLQuery read fSQLQuery write fSQLQuery;
      property Trans: TSQLTransaction read fTrans write fTrans;
      property Connection: TIBConnection read fConnection write fConnection;
      property Statement: String read fStatement write fStatement;
      procedure DoJob;
      procedure Execute; override;
      constructor Create(aType: string);
  end;


  { TfmQueryWindow }

  TfmQueryWindow = class(TForm)
    bbClose: TBitBtn;
    FindDialog1: TFindDialog;
    imTools: TImageList;
    imTabs: TImageList;
    lmCloseTab: TMenuItem;
    lmCopy: TMenuItem;
    lmPaste: TMenuItem;
    lmSelectAll: TMenuItem;
    lmUndo: TMenuItem;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    lmCut: TMenuItem;
    lmExport: TMenuItem;
    lmCommaDelemited: TMenuItem;
    lmHTML: TMenuItem;
    lmRedo: TMenuItem;
    MenuItem2: TMenuItem;
    lmFind: TMenuItem;
    lmFindAgain: TMenuItem;
    MenuItem3: TMenuItem;
    lmCopyCell: TMenuItem;
    lmExportAsComma: TMenuItem;
    lmExportAsHTML: TMenuItem;
    lmCopyAll: TMenuItem;
    MenuItem5: TMenuItem;
    lmRun: TMenuItem;
    lmRunSelect: TMenuItem;
    lmRunExec: TMenuItem;
    lmRunScript: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pmTab: TPopupMenu;
    pmMemo: TPopupMenu;
    pmGrid: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    meQuery: TSynEdit;
    SynCompletion1: TSynCompletion;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar1: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbRun: TToolButton;
    tbCommit: TToolButton;
    tbRollback: TToolButton;
    tbCommitRetaining: TToolButton;
    tbRollbackRetaining: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbHistory: TToolButton;
    ToolButton5: TToolButton;
    tbMenu: TToolButton;
    procedure bbRunClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGridTitleClick(column: TColumn);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lmCloseTabClick(Sender: TObject);
    procedure lmCommaDelemitedClick(Sender: TObject);
    procedure lmCopyAllClick(Sender: TObject);
    procedure lmCopyCellClick(Sender: TObject);
    procedure lmCopyClick(Sender: TObject);
    procedure lmCutClick(Sender: TObject);
    procedure lmExportAsCommaClick(Sender: TObject);
    procedure lmExportAsHTMLClick(Sender: TObject);
    procedure lmHTMLClick(Sender: TObject);
    procedure lmPasteClick(Sender: TObject);
    procedure lmRedoClick(Sender: TObject);
    procedure lmRunClick(Sender: TObject);
    procedure lmRunExecClick(Sender: TObject);
    procedure lmRunScriptClick(Sender: TObject);
    procedure lmRunSelectClick(Sender: TObject);
    procedure lmSelectAllClick(Sender: TObject);
    procedure lmUndoClick(Sender: TObject);
    procedure lmFindClick(Sender: TObject);
    procedure lmFindAgainClick(Sender: TObject);
    procedure meQueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure SQLScript1Exception(Sender: TObject; Statement: TStrings;
      TheException: Exception; var Continue: boolean);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure tbCloseClick(Sender: TObject);
    procedure tbCommitClick(Sender: TObject);
    procedure tbCommitRetainingClick(Sender: TObject);
    procedure tbHistoryClick(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbRollbackClick(Sender: TObject);
    procedure tbRollbackRetainingClick(Sender: TObject);
    procedure tbRunClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
  private
    { private declarations }
    fdbIndex: Integer;
    RegRec: TRegisteredDatabase;
    ResultControls: array of TObject;
    ParentResultControls: array of TObject;
    fOptions: set of TSynSearchOption;
    ibConnection: TIBConnection;
    SqlTrans: TSQLTransaction;
    fCanceled: Boolean;
    fStartLine: Integer;
    fList: TStringList;
    fQuery: string;
    fOrigQueryType: Integer;
    fFinished: Boolean;
    fQT: TQueryThread;
    fQueryPart: string;
    fTab: TTabSheet;
    fmeResult: TMemo;
    fSqlQuery: TSQLQuery;
    fSqlScript: TSQLScript;
    faText: string;
    fModifyCount: Integer;
    fCnt: Integer;
    ModifiedRecords: array of array of Integer;

    procedure EnableCommitButton;
    procedure ExecuteQuery;
    function GetNewTabNum: string;
    procedure FinishCellEditing(DataSet: TDataSet);
    function GetRecordSet(TabIndex: Integer): TSQLQuery;
    function getQuerySQLType(QueryList: TStringList; var SecondRealStart: Integer;
      var IsDDL: Boolean): Integer;
    procedure NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
    procedure RemoveComments(QueryList: TStringList; StartLine: Integer;
      var RealStartLine: Integer);
    procedure RemoveAllSingleLineComments(QueryList: TStringList);
    procedure removeEmptyLines(QueryList: TStringList;
      var SecondRealStart: Integer; const RealStartLine: Integer);
    procedure StartEdit(Sender: TObject; const Field: TField;
      var Value: string);
    procedure InsertModifiedRecord(RecordNo, TabIndex: Integer);
    procedure ApplyClick(Sender: TObject);
    procedure EnableApplyButton;
    function GetTableName(SQLText: string): string;
    function GetCurrentSQLText: string;
    procedure CommitResultClick(Sender: TObject);
  public
    OnCommit: TNotifyEvent;
    procedure Init(dbIndex: Integer);
    function GetQueryType(AQuery: string): Integer;
    function GetQuery: string;
    function CreateResultTab(QueryType: Byte; var aSqlQuery: TSQLQuery; var aSQLScript: TSqlScript;
      var meResult: TMemo; AdditionalTitle: string = ''): TTabSheet;
    function ExecuteScript(Script: string): Boolean;
    procedure AddResultControl(ParentControl: TObject; AControl: TObject);
    procedure NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
    procedure RemoveControls;
    function FindSqlQuery: TSqlQuery;
    function GetSQLType(Query: string; var Command: string): string;
    function GetSQLSegment(QueryList: TStringList; StartLine: Integer; var QueryType, EndLine: Integer;
      var SQLSegment: string; var IsDDL: Boolean): Boolean;
    procedure QueryAfterScroll(DataSet: TDataSet);
    procedure CallExecuteQuery(aQueryType: Integer);
    procedure sortsyncompletion;
    procedure ThreadTerminated(Sender: TObject);
    procedure EnableButtons;

    { public declarations }
  end; 

var
  fmQueryWindow: TfmQueryWindow;

implementation

{ TfmQueryWindow }

uses main, SQLHistory;

procedure TfmQueryWindow.NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
var
  Commit: TBitBtn;
begin
  Commit:= TBitBtn.Create(nil);
  Commit.Parent:= Pan;
  Commit.Caption:= 'Commit';
  Commit.Left:= 400;
  Commit.Visible:= False;
  Commit.OnClick:= @CommitResultClick;
  Commit.Tag:= ATab.TabIndex;
  AddResultControl(ATab, Commit);
end;

procedure TfmQueryWindow.RemoveComments(QueryList: TStringList; StartLine: Integer; var RealStartLine: Integer);
var
  Comment: Boolean;
  i: Integer;
  MultiComment: Boolean;
begin
  MultiComment:= False;
  for i:= StartLine to QueryList.Count - 1 do
  begin
    if Pos('/*', Trim(QueryList[i])) = 1 then
    begin
      MultiComment:= True;
      Comment:= False;
    end;

    if not MultiComment then
      Comment:= Pos('--', Trim(QueryList[i])) = 1;

    if (Trim(QueryList[i]) <> '') and (not Comment) and (not MultiComment) then
    begin
      RealStartLine:= i;
      Break;
    end;

    if MultiComment and (Pos('*/', QueryList[i]) > 0) then // End of multi-line comment
    begin
      QueryList[i]:= Trim(Copy(QueryList[i], Pos('*/', QueryList[i]) + 2, Length
        (QueryList[i])));
      RealStartLine:= i;
      MultiComment:= False;
      Comment:= False;
      if (i = QueryList.Count - 1) or
         ((Trim(QueryList[i + 1]) <> '') and  (Pos('/*', Trim(QueryList[i + 1])
           ) <> 1) and
         (Pos('--', Trim(QueryList[i + 1])) <> 1)) then
          Break;
    end;

  end;
end;

procedure TfmQueryWindow.RemoveAllSingleLineComments(QueryList: TStringList);
var
  i: Integer;
begin
  for i:= QueryList.Count - 1 downto 0 do
  if Pos('--', QueryList[i]) > 0 then
  begin
    if Pos('--', Trim(QueryList[i])) = 1 then
      QueryList.Delete(i)
    else
      QueryList[i]:= Copy(QueryList[i], 1, Pos('--', QueryList[i]) - 1);
  end;

end;

procedure TfmQueryWindow.removeEmptyLines(QueryList: TStringList; var SecondRealStart: Integer;
  const RealStartLine: Integer);
var
  i: integer;
begin
  for i:= RealStartLine to QueryList.Count - 1 do
  begin
    if Trim(QueryList[i]) <> '' then
    begin
      SecondRealStart:= i;
      Break;
    end;
  end;
end;

{ TQueryThread }

procedure TfmQueryWindow.FinishCellEditing(DataSet: TDataSet);
begin
  InsertModifiedRecord(Dataset.RecNo, PageControl1.TabIndex);
end;

procedure TfmQueryWindow.StartEdit(Sender: TObject; const Field: TField;
  var Value: string);
begin
  ShowMessage('Editing started: ' + Value);
end;

procedure TfmQueryWindow.InsertModifiedRecord(RecordNo, TabIndex: Integer);
var
  i: Integer;
  Exist: Boolean;
begin
  Exist:= False;
  if TabIndex > High(ModifiedRecords) then // Insert new tab
  begin
    SetLength(ModifiedRecords, TabIndex + 1);
  end;

  // check if record already inserted
  for i:= 0 to High(ModifiedRecords[TabIndex]) do
  if ModifiedRecords[TabIndex][i] = RecordNo then
  begin
    Exist:= True;
    Break;
  end;

  if not Exist then  // Insert record pointer
  begin
    setLength(ModifiedRecords[TabIndex], Length(ModifiedRecords[TabIndex]) + 1);
    ModifiedRecords[TabIndex][High(ModifiedRecords[TabIndex])]:= RecordNo;
  end;

  // Enable apply/save button
  if Length(ModifiedRecords[TabIndex]) = 1 then
  begin
    EnableApplyButton;
  end;

end;

procedure TfmQueryWindow.ApplyClick(Sender: TObject);
var
  i, x: Integer;
  aTableName: string;
  aQuery: TSQLQuery;
  PKName: string;
  ConstraintName: string;
  KeyList, FieldsList: TStringList;
  WhereClause: string;
  RecordSet: TSQLQuery;
  TabIndex: Integer;
  FieldsSQL: string;
begin
  try
    TabIndex:= PageControl1.TabIndex;
    aTableName:= GetTableName(GetCurrentSQLText);
    RecordSet:= GetRecordSet(TabIndex);

    // Get primary key name
    PKName:= fmMain.GetPrimaryKeyIndexName(fdbIndex, ATableName, ConstraintName);
    if PKName <> '' then
    begin

      aQuery:= TSQLQuery.Create(nil);
      aQuery.DataBase:= ibConnection;
      aQuery.Transaction:= SqlTrans;
      KeyList:= TStringList.Create;
      Fieldslist:= tstringList.Create;

      // Get primary key fields
      fmMain.GetIndexFields(ATableName, PKName, aQuery, KeyList);
      fmMain.GetFields(fdbIndex, ATableName, FieldsList);
      WhereClause:= 'where ';

      RecordSet.DisableControls;
      // Check modified fields
      for i:= 0 to High(ModifiedRecords[TabIndex]) do
      begin
        FieldsSQL:= '';
        RecordSet.RecNo:= ModifiedRecords[TabIndex][i];
        for x:= 0 to RecordSet.Fields.Count - 1 do
        if FieldsList.IndexOf(RecordSet.Fields[x].FieldName) <> -1 then // Field exist in origional table

        if (RecordSet.Fields[x].NewValue <> RecordSet.Fields[x].OldValue) then // field data has been modified
        begin
         if FieldsSQL <> '' then
             FieldsSQL += ',';
           FieldsSQL += RecordSet.Fields[x].FieldName + '=';

           // Typecast field values according to thier main type
           case RecordSet.Fields[x].DataType of
            ftInteger, ftSmallint: FieldsSQL += IntToStr(RecordSet.Fields[x].NewValue);
            ftFloat: FieldsSQL += FloatToStr(RecordSet.Fields[x].NewValue);
            ftTimeStamp, ftDateTime: FieldsSQL += '''' + DateTimeToStr(RecordSet.Fields[x].NewValue) + '''';
            ftTime: FieldsSQL += '''' + TimeToStr(RecordSet.Fields[x].NewValue) + '''';
            ftDate: FieldsSQL += '''' + DateToStr(RecordSet.Fields[x].NewValue) + '''';

           else // Other types like string
              FieldsSQL += '''' + RecordSet.Fields[x].NewValue + '''';

         end;
        end;


        // Update current record
        if FieldsSQL <> '' then
        begin
          aQuery.Close;
          aQuery.SQL.Text:= 'update ' + aTableName + ' set ' + FieldsSQL;

          WhereClause:= 'where ';
          // where clause
          for x:= 0 to KeyList.Count - 1 do
          if Trim(KeyList[x]) <> '' then
          begin
            WhereClause += KeyList[x] + ' = ';

            // Typecase index values
            case RecordSet.Fields[x].DataType of
             ftInteger, ftSmallint: WhereClause += IntToStr(RecordSet.Fields[x].OldValue);
             ftFloat: WhereClause += FloatToStr(RecordSet.Fields[x].OldValue);
            else
               WhereClause += '''' + RecordSet.Fields[x].OldValue + '''';
            end;
            if x < KeyList.Count - 1 then
              WhereClause += ' and ';
          end;

          aQuery.SQL.Add(WhereClause);
          aQuery.ExecSQL;
          (Sender as TBitBtn).Visible:= False;
          EnableCommitButton;
        end;
      end;

      // Reset ModifedRecords pointer
      ModifiedRecords[TabIndex]:= nil;

      RecordSet.EnableControls;
      FieldsList.Free;
      KeyList.Free;
      aQuery.Free;
    end
    else
      ShowMessage('There is no primary key on the table: ' + aTableName);


  except
  on e: exception do
  begin
    ShowMessage('Error in save data: ' + e.Message);
  end;

  end;


end;

procedure TfmQueryWindow.EnableApplyButton;
var
  i: Integer;
begin
  for i:= 0 to High(ResultControls) do
  if (ResultControls[i] is TBitBtn) and ((ResultControls[i] as TBitBtn).Tag = PageControl1.TabIndex) and
    ((ResultControls[i] as TBitBtn).Caption = 'Apply') then
  begin
    (ResultControls[i] as TBitBtn).Visible:= True;
    Break;
  end;
end;

procedure TfmQueryWindow.EnableCommitButton;
var
  i: Integer;
begin
  for i:= 0 to High(ResultControls) do
  if (ResultControls[i] is TBitBtn) and ((ResultControls[i] as TBitBtn).Tag = PageControl1.TabIndex)
    and ((ResultControls[i] as TBitBtn).Caption = 'Commit') then
  begin
    (ResultControls[i] as TBitBtn).Visible:= True;
    Break;
  end;
end;

function TfmQueryWindow.GetTableName(SQLText: string): string;
begin
  SQLText:= Trim(Copy(SQLText, Pos('from', LowerCase(SQLText)) + 4, Length(SQLText)));
  if Pos('"', SQLText) = 1 then
  begin
    Delete(SQLText, 1, 1);
    Result:= Copy(SQLText, 1, Pos('"', SQLText) - 1);
  end
  else
  begin
    if Pos(' ', SQLText) > 0 then
      Result:= Copy(SQLText, 1, Pos(' ', SQLText) - 1)
    else
      Result:= SQLText;
  end;
  if Pos(';', Result) > 0 then
    Delete(Result, Pos(';', Result), 1);

end;

function TfmQueryWindow.GetCurrentSQLText: string;
var
  i: Integer;
begin
  for i:= 0 to High(ResultControls) do
  if (ResultControls[i] is TDBGrid) and ((ResultControls[i] as TDBGrid).Tag = PageControl1.TabIndex) then
  begin
    Result:= ((ResultControls[i] as TDBGrid).DataSource.DataSet as TSQLQuery).SQL.Text;
    Break;
  end;

end;

procedure TfmQueryWindow.CommitResultClick(Sender: TObject);
begin
  SqlTrans.CommitRetaining;
  (Sender as TBitBtn).Visible:= False;
end;

function TfmQueryWindow.GetRecordSet(TabIndex: Integer): TSQLQuery;
var
  i: Integer;
begin
  for i:= 0 to High(ResultControls) do
  if (ResultControls[i] is TSQLQuery) and ((ResultControls[i] as TSQLQuery).Tag = TabIndex) then
  begin
    Result:= ResultControls[i] as TSQLQuery;
    Break;
  end;

end;

function TfmQueryWindow.getQuerySQLType(QueryList: TStringList; var SecondRealStart: Integer; var IsDDL: Boolean): Integer;
var
  SQLSegment: string;
begin
  IsDDL:= False;
  if SecondRealStart < QueryList.Count then
  begin
    SQLSegment:= SQLSegment + QueryList[SecondRealStart];

    if (Pos('select', LowerCase(Trim(SQLSegment))) = 1) then
      Result:= 1 // Selectable
    else
    if Pos('setterm', LowerCase(StringReplace(SQLSegment, ' ', '', [rfReplaceAll
      ]))) = 1 then
      Result:= 3 // Script
    else
    begin
      Result:= 2; // Executable
      IsDDL:= (Pos('create', lowerCase(Trim(SQLSegment))) = 1) or (Pos('alter',
        lowerCase(Trim(SQLSegment))) = 1) or
         (Pos('modify', lowerCase(Trim(SQLSegment))) = 1);
    end;
  end;
end;

procedure TQueryThread.DoJob;
begin
  try
    if fType = 'open' then
      fSQLQuery.Open
    else
    if fType = 'exec' then
      fSQLQuery.ExecSQL
    else
    if fType = 'ddl' then
      fConnection.ExecuteDirect(fStatement)
    else
    if fType = 'commit' then
      fTrans.Commit
    else
    if fType = 'commitret' then
      fTrans.CommitRetaining
    else
    if fType = 'rollback' then
      fTrans.Rollback
    else
    if fType = 'rollbackret' then
      fTrans.RollbackRetaining;

    Error:= False;
    fTerminated:= True;

  except
  on e: exception do
  begin
    Error:= True;
    ErrorMsg:= e.Message;
    fTerminated:= True;
  end;
  end;

end;

procedure TQueryThread.Execute;
begin
  try
    fTerminated:= False;
    Error:= False;
    DoJob;
    fTerminated:= True;

  except
  on e: exception do
  begin
    Error:= True;
    ErrorMsg:= e.Message;
    fTerminated:= True;
  end;
  end;
end;

constructor TQueryThread.Create(aType: string);
begin
  inherited Create(True);
  fType:= aType;
  FreeOnTerminate:= False;
end;

procedure TfmQueryWindow.SQLScript1Exception(Sender: TObject;
  Statement: TStrings; TheException: Exception; var Continue: boolean);
begin
  ShowMessage(TheException.Message);
end;

procedure TfmQueryWindow.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  SynCompletion1.Deactivate;
end;

procedure TfmQueryWindow.tbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmQueryWindow.tbCommitClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TSQLQuery;
  SqlScript: TSQLScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  RemoveControls;
  ATab:= CreateResultTab(2, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create('commit');
  try
    QT.Trans:= SqlTrans;
    ATab.ImageIndex:= 6;

    // Run thread
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
    begin
      ATab.ImageIndex:= 3;
      meResult.Lines.Text:= QT.ErrorMsg;
      meResult.Font.Color:= clRed;
    end
    else
    begin
      ATab.ImageIndex:= 4;
      meResult.Lines.Add('Commited');
      meResult.Font.Color:= clGreen;

      // Call OnCommit procedure if assigned, it is used to refresh table management view
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
    end;

  finally
    QT.Free;
  end;

end;

procedure TfmQueryWindow.tbCommitRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create('commitret');
  try
    QT.Trans:= SqlTrans;

    // Run thread
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
      ShowMessage(QT.ErrorMsg)
    else
    begin
      // Call OnCommit procedure if assigned, it is used to refresh table management view
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
    end;

  finally
    QT.Free;
  end;
end;

procedure TfmQueryWindow.tbHistoryClick(Sender: TObject);
begin
  fmSQLHistory.Init(RegRec.Title, Self);
  fmSQLHistory.Show;
end;

procedure TfmQueryWindow.tbMenuClick(Sender: TObject);
begin
  pmTab.PopUp;
end;

procedure TfmQueryWindow.tbNewClick(Sender: TObject);
var
  i: Integer;
begin
  // Get a free number to be assigned to the new Query window
  for i:= 1 to 1000 do
  begin
    if fmMain.FindQueryWindow(RegRec.Title + ': Query Window # ' + IntToStr(i)) = nil then
    begin
      fmMain.ShowCompleteQueryWindow(fdbIndex, 'Query Window # ' + IntToStr(i), '');
      Break;
    end;
  end;
end;

procedure TfmQueryWindow.tbOpenClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt:= '.sql';
  if OpenDialog1.Execute then
    meQuery.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfmQueryWindow.tbRollbackClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TSQLQuery;
  SqlScript: TSQLScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  RemoveControls;
  ATab:= CreateResultTab(2, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create('rollback');
  try
    QT.Trans:= SqlTrans;
    ATab.ImageIndex:= 6;
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
    begin
      ATab.ImageIndex:= 3;
      meResult.Lines.Text:= QT.ErrorMsg;
      meResult.Font.Color:= clRed;
    end
    else
    begin
      ATab.ImageIndex:= 4;
      meResult.Lines.Add('Rollback');
      meResult.Font.Color:= clGreen;
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
      meResult.Font.Color:= $AA6666;
    end;

  finally
    QT.Free;
  end;
end;

procedure TfmQueryWindow.tbRollbackRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create('rollbackret');
  try
    QT.Trans:= SqlTrans;

    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated or (fCanceled);
    if QT.Error then
      ShowMessage(QT.ErrorMsg);

  finally
    QT.Free;
  end;

end;

procedure TfmQueryWindow.tbRunClick(Sender: TObject);
begin
  CallExecuteQuery(0);
end;

procedure TfmQueryWindow.tbSaveClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.sql';
  if SaveDialog1.Execute then
    meQuery.Lines.SaveToFile(SaveDialog1.FileName);
end;

function TfmQueryWindow.GetNewTabNum: string;
var
  i: Integer;
  Cnt: Integer;
begin
  Cnt:= 0;
  for i:= 0 to PageControl1.ControlCount - 1 do
  if PageControl1.Pages[i].TabVisible then
   Inc(Cnt);
  Result:= IntToStr(Cnt);
end;

procedure TfmQueryWindow.Init(dbIndex: Integer);
begin
  fdbIndex:= dbIndex;
  RegRec:= fmMain.RegisteredDatabases[dbIndex].RegRec;

  // Initialize new instance of IBConnection and SQLTransaction to the current Query Window
  ibConnection:= TIBConnection.Create(nil);
  SqlTrans:= TSQLTransaction.Create(nil);
  SqlTrans.DataBase:= ibConnection;
  SqlTrans.Params.Add('isc_tpb_read_commited');

  // Set connection parameters to IBConnection
  with fmMain.RegisteredDatabases[dbIndex] do
  begin
    Self.ibConnection.DatabaseName:= RegRec.DatabaseName;
    Self.ibConnection.UserName:= RegRec.UserName;
    Self.ibConnection.Password:= RegRec.Password;
    Self.IBConnection.CharSet:= RegRec.Charset;
    Self.ibConnection.Role:= RegRec.Role;
  end;

  // Get current database tables to be hilighted in SQL query editor
  SynSQLSyn1.TableNames.CommaText:= fmMain.GetTableNames(dbIndex);
  SynCompletion1.ItemList.AddStrings(SynSQLSyn1.TableNames);
  sortsyncompletion;
end;

(************* Is Selectable (Check statement type Select, Update, Alter, etc) *******************)

function TfmQueryWindow.GetQueryType(AQuery: string): Integer;
var
  List: TStringList;
  i: Integer;
  Line: string;
  StartPos, EndPos: Integer;
begin
  List:= TStringList.Create;
  try
    List.Text:= AQuery;

    Result:= 2; // Default Execute

    for i:= 0 to List.Count - 1 do
    begin
      Line:= List[i];
      // Remove comments
      if Pos('--', Line) > 0 then
        Line:= Copy(Line, 1, Pos('--', Line) - 1);
      if (Pos('/*', Line) > 0) and (Pos('*/', Line) > 0) then
      begin
        StartPos:= (Pos('/*', Line));
        EndPos:= (Pos('*/', Line));
        Delete(Line, StartPos, EndPos - StartPos + 1);
      end;

      if (Pos('select', LowerCase(Trim(Line))) = 1) then
      begin
        Result:= 1; // Selectable
        Break;
      end
      else
      if Pos('setterm', LowerCase(StringReplace(Line, ' ', '', [rfReplaceAll]))) = 1 then
      begin
        Result:= 3;
        Break;
      end;

      if Trim(Line) <> '' then
      begin
        Result:= 2; // Executable
        Break;
      end;

    end;

  finally
    List.Free;
  end;
end;

function TfmQueryWindow.GetQuery: string;
begin
  Result:= meQuery.SelText;
  if Result = '' then
    Result:= meQuery.Lines.Text;
end;

function TfmQueryWindow.CreateResultTab(QueryType: Byte;
  var aSqlQuery: TSQLQuery; var aSQLScript: TSqlScript; var meResult: TMemo;
  AdditionalTitle: string): TTabSheet;
var
  ATab: TTabSheet;
  DBGrid: TDBGrid;
  DataSource: TDataSource;
  StatusBar: TStatusBar;
  Nav: TDBNavigator;
  Pan: TPanel;
begin
  ATab:= TTabSheet.Create(nil);
  Result:= ATab;
  ATab.Parent:= PageControl1;
  ATab.Caption:= 'Result # ' + GetNewTabNum + ' ' + AdditionalTitle;
  if QueryType = 1 then // Select, need record set result
  begin
    // Query
    aSqlQuery:= TSQLQuery.Create(nil);
    aSqlQuery.DataBase:= ibConnection;
    aSqlQuery.Transaction:= SqlTrans;
    aSqlQuery.AfterScroll:= @QueryAfterScroll;
    AddResultControl(ATab, aSqlQuery);
    aSqlQuery.AfterPost:= @FinishCellEditing;
    aSqlQuery.Tag:= ATab.TabIndex;


    // Status Bar
    StatusBar:= TStatusBar.Create(nil);
    StatusBar.Parent:= ATab;
    AddResultControl(ATab, StatusBar);

    // Datasource
    DataSource:= TDataSource.Create(nil);
    DataSource.DataSet:= aSqlQuery;
    AddResultControl(ATab, DataSource);

    // Panel
    pan:= TPanel.Create(nil);
    pan.Parent:= ATab;
    Pan.Height:= 30;
    Pan.Align:= alTop;
    AddResultControl(ATab, Pan);

    // Query result Grid
    DBGrid:= TDBGrid.Create(nil);
    DBGrid.Parent:= ATab;
    DBGrid.DataSource:= DataSource;
    DBGrid.Align:= alClient;
    DBGrid.OnDblClick:= @DBGrid1DblClick;

    DBGrid.OnFieldEditMask:= @StartEdit;
    DBGrid.Tag:= ATab.TabIndex;
    DBGrid.ReadOnly:= False;
    DBGrid.AutoEdit:= True;

    DBGrid.PopupMenu:= pmGrid;
    DBGrid.TitleStyle:= tsNative;
    DBGrid.Options:= DBGrid.Options + [dgAutoSizeColumns, dgHeaderHotTracking, dgHeaderPushedLook, dgAnyButtonCanSelect];

    DBGrid.OnTitleClick:= @DBGridTitleClick;
    AddResultControl(ATab, DBGrid);

    // Navigator
    Nav:= TDBNavigator.Create(nil);
    Nav.Parent:= Pan;
    Nav.VisibleButtons:= [nbFirst, nbNext, nbPrior, nbLast];
    Nav.DataSource:= DataSource;
    AddResultControl(ATab, Nav);

    // Apply button
    NewApplyButton(Pan, ATab);

    // Commit button
    NewCommitButton(Pan, ATab);
  end
  else
  if QueryType in [2, 3] then
  begin
    meResult:= TMemo.Create(nil);
    meResult.Parent:= ATab;
    meResult.ReadOnly:= True;
    meResult.Align:= alClient;
    AddResultControl(ATab, meResult);

    if QueryType = 2 then
    begin
      aSqlQuery:= TSQLQuery.Create(nil);
      aSqlQuery.DataBase:= ibConnection;
      aSqlQuery.Transaction:= SqlTrans;
      AddResultControl(ATab, aSqlQuery);
    end;


    if QueryType = 3 then // Script
    begin
      aSQLScript:= TSQLScript.Create(nil);
      aSQLScript.DataBase:= ibConnection;
      aSQLScript.Transaction:= SqlTrans;
      AddResultControl(ATab, aSQLScript);
    end;
  end;
  AddResultControl(nil, ATab);
end;

(***************  Execute Query   ******************)

procedure TfmQueryWindow.ExecuteQuery;
var
  StartTime: TDateTime;
  SqlType: string;
  EndLine: Integer;
  Command: string;
  IsDDL: Boolean;
  Affected: Integer;
  fQueryType: Integer;
begin
  try

    // Script
    if (fOrigQueryType = 3) then
    begin
      ExecuteScript(fQuery);
      Inc(fModifyCount);
      SqlType:= GetSQLType(fQuery, Command);
      fmMain.AddToSQLHistory(RegRec.Title, SqlType, fQuery);
      fFinished:= True;
      fList.Free;
    end
    else       // normal statement / Multi statements
    begin
      Inc(fCnt);
      if not GetSQLSegment(fList, fStartline, fQueryType, EndLine, fQueryPart, IsDDL) then
      begin
        fFinished:= True;
        Exit;
      end;

      {if EndLine < fStartLine then
        fStartLine:= fStartLine + 1
      else}
        fStartLine:= EndLine + 1;

      if Trim(fQueryPart) <> '' then   // Select
      if fQueryType = 1 then
      begin
        fTab:= nil;
        try
          fTab:= CreateResultTab(1, fSqlQuery, fSqlScript, fmeResult);
          fTab.ImageIndex:= 6;
          fTab.Hint:= fQueryPart;
          fTab.ShowHint:= True;
          fSQLQuery.SQL.Text:= fQueryPart;

          // Create thread to open dataset
          fQT:= TQueryThread.Create('open');
          fQT.Query:= fSqlQuery;
         // fQT.OnTerminate:= @ThreadTerminated;
          faText:= fTab.Caption;
          fTab.Caption:= 'Running..';
          fQT.Resume;


          // Wait for the thread to complete
          repeat

              Sleep(100);
              application.ProcessMessages; // This prevents display freeze

            until fQT.fTerminated;

            // Raise exception if an error occured during thread execution (Open)
            if fQT.Error then
                raise Exception.Create(fQT.ErrorMsg);

            fQT.Free;
            fTab.Caption:= faText;
            fTab.ImageIndex:= 0;
            fmMain.AddToSQLHistory(RegRec.Title, 'SELECT', fQueryPart);

          except
          on e: exception do
          begin
            if Assigned(fTab) then
              fTab.TabVisible:= False;
            SetLength(ResultControls, High(ResultControls));
            SetLength(ParentResultControls, High(ParentResultControls));
            fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);
            PageControl1.ActivePage:= fTab;

            fmeResult.Text:= e.message;
            fmeResult.Lines.Add(fQueryPart);
            fmeResult.Font.Color:= clRed;
            fTab.Font.Color:= clRed;
            fTab.ImageIndex:= 3;
          end;
          end;
        end
        else  // Execute
        if fQueryType = 2 then
        begin
          fTab:= nil;
          fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);

          fTab.ImageIndex:= 1;
          SqlType:= GetSQLType(fQueryPart, Command);
          StartTime:= Now;
          Affected:= 0;
          try
            if IsDDL then
            begin
              // Execute the statement in thread
              fQT:= TQueryThread.Create('ddl');
              fQT.Connection:= ibConnection;
              fQT.Statement:= fQueryPart;
              fQT.Resume;
              faText:= fTab.Caption;
              fTab.Caption:= 'Running..';

              // Wait for thread completion
              repeat
                application.ProcessMessages;
              until (fQT.fTerminated) or (fCanceled);

              // Raise exception if an error occured during thread execution (ExecProc)
              if fQT.Error then
                raise Exception.Create(fQT.ErrorMsg);

              fQT.Free;
            end
            else
            begin   // DML
              fSqlQuery.Close;
              fSqlQuery.SQL.Text:= fQueryPart;
              fTab.ImageIndex:= 6;
              fTab.Hint:= fQueryPart;
              fTab.ShowHint:= True;
              fSQLQuery.SQL.Text:= fQueryPart;

              // Execute the statement in thread
              fQT:= TQueryThread.Create('exec');
              fQT.Query:= fSqlQuery;
              fQT.Resume;
              faText:= fTab.Caption;
              fTab.Caption:= 'Running..';

              // Wait for thread completion
              repeat
                application.ProcessMessages;
              until (fQT.fTerminated) or (fCanceled);

              // Raise exception if an error occured during thread execution (ExecProc)
              if fQT.Error then
                raise Exception.Create(fQT.ErrorMsg);

              fQT.Free;
              fTab.Caption:= faText;
              fTab.ImageIndex:= 1;
              Affected:= fsqlQuery.RowsAffected;
            end;
            Inc(fModifyCount);

            fmMain.AddToSQLHistory(RegRec.Title, SQLType, fQueryPart);
            fmeResult.Visible:= True;
            fmeResult.Clear;
            fmeResult.Lines.Add('statement #' + IntToStr(fCnt));
            if IsDDL then
              fmeResult.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DDL Executed. Takes (H:M:S.MS) ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime))
            else // DML
            begin
              fmeResult.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DML Executed. Takes (H:M:S.MS) ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime));
              fmeResult.Lines.Add('Rows affected: ' + Format('%3.0n', [Affected / 1]));

            end;
            fmeResult.Lines.Add('----');
            fmeResult.Lines.Add(fQueryPart);

          except
          on e: exception do
          begin
            if Assigned(fTab) then
              fTab.TabVisible:= False;
            fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);
            PageControl1.ActivePage:= fTab;
            fmeResult.Text:= e.message;
            fmeResult.Lines.Add(fQueryPart);
            fmeResult.Font.Color:= clRed;
            fTab.Font.Color:= clRed;
            fTab.ImageIndex:= 3;
          end;
          end

        end
        else  // Script
        begin
          try
          if ExecuteScript(fQueryPart) then
          begin
            Inc(fModifyCount);
            SqlType:= GetSQLType(fQueryPart, Command);
            fmMain.AddToSQLHistory(RegRec.Title, SqlType, fQueryPart);
          end;

          except
          on e: exception do
          begin
            if Assigned(fTab) then
              fTab.TabVisible:= False;
            fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);
            PageControl1.ActivePage:= fTab;
            fmeResult.Text:= e.message;
            fmeResult.Lines.Add(fQueryPart);
            fmeResult.Lines.Add('--------');
            fmeResult.Font.Color:= clRed;
            fTab.Font.Color:= clRed;
            fTab.ImageIndex:= 3;
          end;
        end;

        end;
        if (fModifyCount > 50) then
        if (MessageDlg('Commit', 'There are too many transactions, did you want to commit',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          SqlTrans.CommitRetaining;
          fModifyCount:= 0;
        end
        else
          fModifyCount:= 0;

      if fStartLine >= fList.Count then
        fFinished:= True;
    end;

  except
  on e: exception do
  begin
    if Assigned(fTab) then
      fTab.TabVisible:= False;
    fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);
    fTab.ImageIndex:= 2;
    PageControl1.ActivePage:= fTab;

    fmeResult.Text:= e.message;
    fmeResult.Lines.Add('--------');
    fmeResult.Lines.Add(fQueryPart);
    fmeResult.Font.Color:= clRed;
    fFinished:= True;
  end;
  end;

end;

function TfmQueryWindow.ExecuteScript(Script: string): Boolean;
var
  StartTime: TDateTime;
  SqlQuery: TSQLQuery;
  SqlScript: TSQLScript;
  meResult: TMemo;
  ATab: TTabSheet;
begin
  ATab:= nil;
  try
    StartTime:= Now;
    ATab:= CreateResultTab(3, SqlQuery, SqlScript, meResult);
    ATab.ImageIndex:= 2;
    SQLScript.Script.Text:= Script;
    SQLScript.ExecuteScript;
    Result:= True;
    meResult.Lines.Text:= FormatDateTime('hh:nn:ss.z', Now) + ' - Script Executed. It takes (H:M:S.MS) ' +
      FormatDateTime('HH:nn:ss.z', Now - StartTime);
    meResult.Lines.Add('--------');
    meResult.Lines.Add(Script);

  except
  on e: exception do
  begin
    Result:= False;
    if Assigned(ATab) then
      ATab.TabVisible:= False;
    ATab:= CreateResultTab(2, SqlQuery, SqlScript, meResult);
    PageControl1.ActivePage:= ATab;
    meResult.Text:= e.Message;
    meResult.Lines.Add('--------');
    meResult.Lines.Add(Script);
    meResult.Font.Color:= clRed;
    ATab.Font.Color:= clRed;
    ATab.ImageIndex:= 3;
  end;

  end;
end;

procedure TfmQueryWindow.AddResultControl(ParentControl: TObject; AControl: TObject);
begin
  SetLength(ResultControls, Length(ResultControls) + 1);
  SetLength(ParentResultControls, Length(ParentResultControls) + 1);
  ResultControls[High(ParentResultControls)]:= AControl;
  ParentResultControls[High(ParentResultControls)]:= ParentControl;
end;

procedure TfmQueryWindow.NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
var
  Apply: TBitBtn;
begin
  Apply:= TBitBtn.Create(nil);
  Apply.Parent:= Pan;
  Apply.Caption:= 'Apply';
  Apply.Left:= 300;
  Apply.Visible:= False;
  Apply.OnClick:= @ApplyClick;
  Apply.Tag:= ATab.TabIndex;
  AddResultControl(ATab, Apply);
end;

procedure TfmQueryWindow.RemoveControls;
var
  i: Integer;
  CannotFree: Boolean;
begin
  for i:= High(ResultControls) downto  0 do
  begin
    if ResultControls[i] is TSQLQuery then
    begin
      (ResultControls[i] as TSQLQuery).AfterScroll:= nil;
      (ResultControls[i] as TSQLQuery).Close;
      (ResultControls[i] as TSQLQuery).DataSource:= nil;
    end;

      ResultControls[i].Free;
      ResultControls[i]:= nil;
  end;

  SetLength(ResultControls, 0);
  SetLength(ParentResultControls, 0);
end;

function TfmQueryWindow.FindSqlQuery: TSqlQuery;
var
  i: Integer;
begin
  Result:= nil;
  if PageControl1.PageCount > 0 then
  begin
    with PageControl1.ActivePage do
      for i:= 0 to ControlCount - 1 do
        if Controls[i] is TDBGrid then
        begin
          Result:= TSqlQuery((Controls[i] as TDBGrid).DataSource.DataSet);
          Break;
        end;
  end;

end;

function TfmQueryWindow.GetSQLType(Query: string; var Command: string): string;
begin
  Query:= Trim(Query);
  if (Query <> '') and (Pos(' ', Query) > 0) then
  begin
    Command:= Copy(Query, 1, Pos(' ', Query) - 1);
    Command:= LowerCase(Command);
    if (Command = 'alter') or (Command = 'create') or (Command = 'drop') or (Command = 'grant') or
       (Command = 'revoke') then
      Result:= 'DDL'
    else
      Result:= 'DML';
  end;
end;

function TfmQueryWindow.GetSQLSegment(QueryList: TStringList; StartLine: Integer; var QueryType, EndLine: Integer;
  var SQLSegment: string; var IsDDL: Boolean): Boolean;
var
  i: Integer;
  RealStartLine: Integer;
  SecondRealStart: Integer;
  BeginExist: Boolean;
begin
  // Get start
  SQLSegment:= '';
  RealStartLine:= StartLine;
  SecondRealStart:= RealStartLine;
  Result:= False;

  // Remove comments
  RemoveAllSingleLineComments(QueryList);
  RemoveComments(QueryList, StartLine, RealStartLine);

  SecondRealStart:= RealStartLine;

  // remove empty lines
  removeEmptyLines(QueryList, SecondRealStart, RealStartLine);

  // Get SQL type
  QueryType:= getQuerySQLType(QueryList, SecondRealStart, IsDDL);

  // Concatinate
  SQLSegment:= '';
  BeginExist:= False;
  for i:= SecondRealStart to QueryList.Count - 1 do
  begin
    if Pos('begin', Trim(LowerCase(QueryList[i]))) > 0 then
      BeginExist:= True;

    SQLSegment:= SQLSegment + QueryList[i] + #10;

    if (QueryType in [1, 2]) and
      (((Pos(';', QueryList[i]) > 0) and (Not BeginExist)) or
      ((Pos('end', LowerCase(Trim(QueryList[i]))) = 1) and BeginExist)
      or (i = QueryList.Count - 1)) then
    begin
      Result:= True;
      if (not BeginExist) and (Pos(';', QueryList[i]) > 0) then
      begin
        QueryList[i]:= Trim(Copy(QueryList[i],  Pos(';', QueryList[i]) + 1, Length(QueryList[i])));
        if QueryList[i] = '' then
        EndLine:= i
        else
        begin
          EndLine:= i - 1;
          SQLSegment:= Trim(Copy(SQLSegment, 1, Pos(';',  SQLSegment)));
        end;
      end
      else
        EndLine:= i;

      Break;
    end
    else
    if (QueryType = 3) and ((i > SecondRealStart) and (Pos('setterm', LowerCase(StringReplace(QueryList[i],
      ' ', '', [rfReplaceAll]))) > 0)) or (i = QueryList.Count - 1) then
    begin
      Result:= True;
      EndLine:= i;
      Break;
    end;
  end;

end;


procedure TfmQueryWindow.bbRunClick(Sender: TObject);
begin
  CallExecuteQuery(0);
end;


procedure TfmQueryWindow.DBGrid1DblClick(Sender: TObject);
begin
  if (Sender as TDBGrid).SelectedField.DataType in [ftBlob, ftMemo] then
    ShowMessage((Sender as TDBGrid).SelectedField.AsString)
  else
    ShowMessage((Sender as TDBGrid).SelectedField.KeyFields);
end;

procedure TfmQueryWindow.DBGridTitleClick(column: TColumn);
var
  SqlQuery: TSQLQuery;
//    indexoption : TIndexOptions;
begin
  SqlQuery:= FindSqlQuery;
  if  SqlQuery <> Nil then
  if SqlQuery.IndexFieldNames = Column.Field.FieldName then
    SqlQuery.IndexFieldNames := Column.Field.FieldName //+ 'DESC'
  //   indexoption :=[ixDescending];
  //   SqlQuery.AddIndex('',Column.Field.FieldName,indexoption,'');
  else
    SqlQuery.IndexFieldNames := Column.Field.FieldName

end;

procedure TfmQueryWindow.FindDialog1Find(Sender: TObject);
begin
  fOptions:= [];

  if frMatchCase in FindDialog1.Options then
    fOptions:= fOptions + [ssoMatchCase];

  if frWholeWord in FindDialog1.Options then
    fOptions:= fOptions + [ssoWholeWord];

  if not (frDown in FindDialog1.Options) then
    fOptions:= fOptions + [ssoBackwards];

   if frEntireScope in FindDialog1.Options then
     fOptions:= fOptions + [ssoEntireScope];

  meQuery.SearchReplace(FindDialog1.FindText, '', fOptions);
end;


procedure TfmQueryWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  RemoveControls;

  // Check if the transaction is active commit it
  if SqlTrans.Active then
  begin
    SqlTrans.CommitRetaining;
    if OnCommit <> nil then
      OnCommit(self);
    OnCommit:= nil;
  end;
  IBConnection.Close;
  CloseAction:= caFree;
end;

procedure TfmQueryWindow.FormCreate(Sender: TObject);
var
  F:TextFile;
  str:string;
begin
  if FileExists('querycompletion.txt') then
    SynCompletion1.ItemList.LoadFromFile('querycompletion.txt')
  else
  begin
    SynCompletion1.ItemList.CommaText:= 'create,table,Select,From,INTEGER,FLOAT';
    SynCompletion1.ItemList.SaveToFile('querycompletion.txt');
  end;
      sortsyncompletion;
end;

procedure TfmQueryWindow.FormShow(Sender: TObject);
begin
  meQuery.SetFocus;
end;

procedure TfmQueryWindow.lmCloseTabClick(Sender: TObject);
begin
  if (Trim(meQuery.Lines.Text) = '') or
    (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
  begin
    Close;
    Parent.Free;
  end;
end;


procedure TfmQueryWindow.lmCommaDelemitedClick(Sender: TObject);
var
  i: Integer;
  F: TextFile;
  SqlQuery: TSQLQuery;
begin
  SaveDialog1.DefaultExt:= '.txt';
  SqlQuery:= FindSqlQuery;
  if SqlQuery = nil then
  begin
    ShowMessage('There is no recordset in result');
    Exit;
  end;
  if (not SQLQuery.Active) or (SQLQuery.RecordCount = 0) then
    MessageDlg('No data', mtError, [mbOk], 0)
  else
  if SaveDialog1.Execute then
  begin
    SQLQuery.DisableControls;
    SQLQuery.First;
    AssignFile(F, SaveDialog1.FileName);
    Rewrite(F);
    for i:= 0 to SQLQuery.FieldCount - 1 do
    begin
      Write(F, '"', SQLQuery.Fields[i].FieldName, '"');
      if i = SQLQuery.FieldCount - 1 then
        Writeln(F)
      else
        Write(F, ', ');
    end;

    while not SQLQuery.EOF do
    begin
      for i:= 0 to SQLQuery.FieldCount - 1 do
      begin
        Write(F, '"', SQLQuery.Fields[i].AsString, '"');
        if i = SQLQuery.FieldCount - 1 then
          Writeln(F)
        else
          Write(F, ', ');
      end;
      SQLQuery.Next;
    end;
    CloseFile(F);
    SQLQuery.EnableControls;
  end;

end;

procedure TfmQueryWindow.lmCopyAllClick(Sender: TObject);
var
   Grid: TDBGrid;
   i: Integer;
   List: TStringList;
   Line: string;
begin
  Grid:= TDBGrid(pmGrid.PopupComponent);
  try
    Grid.DataSource.DataSet.DisableControls;
    Grid.DataSource.DataSet.First;
    List:= TStringList.Create;
    Line:= '';

    // Copy fields header
    with Grid.DataSource.DataSet do
    for i:= 0 to FieldCount - 1 do
    begin
      Line:= Line + '"' + Fields[i].FieldName + '"';
      if i + 1 < FieldCount then
        Line:= Line + ',';
    end;
    List.Add(Line);

    // Copy table data
    with Grid.DataSource.DataSet do
    while not Eof do
    begin
      Line:= '';
      for i:= 0 to FieldCount - 1 do
      begin
        Line:= Line + '"' + Trim(Fields[i].AsString) + '"';
        if i + 1 < FieldCount then
          Line:= Line + ',';
      end;
      List.Add(Line);
      Next;
    end;
   Clipboard.AsText:= List.Text;

  except
  on e: exception do
    ShowMessage(e.Message);
  end;
  grid.DataSource.DataSet.EnableControls;
end;

procedure TfmQueryWindow.lmCopyCellClick(Sender: TObject);
begin
  Clipboard.AsText:= TdbGrid(pmGrid.PopupComponent).SelectedField.AsString;
end;

procedure TfmQueryWindow.lmCopyClick(Sender: TObject);
begin
  meQuery.CopyToClipboard;
end;

procedure TfmQueryWindow.lmCutClick(Sender: TObject);
begin
  meQuery.CutToClipboard;
end;

procedure TfmQueryWindow.lmExportAsCommaClick(Sender: TObject);
begin
  lmCommaDelemitedClick(nil);
end;

procedure TfmQueryWindow.lmExportAsHTMLClick(Sender: TObject);
begin
  lmHTMLClick(nil);
end;


procedure TfmQueryWindow.lmHTMLClick(Sender: TObject);
var
  i: Integer;
  F: TextFile;
  SqlQuery: TSQLQuery;
begin
  SaveDialog1.DefaultExt:= '.htm';
  SqlQuery:= FindSqlQuery;
  if SqlQuery = nil then
  begin
    ShowMessage('There is no record set in result');
  end
  else
  if (not SQLQuery.Active) or (SQLQuery.RecordCount = 0) then
    MessageDlg('No data', mtError, [mbOk], 0)
  else
  if SaveDialog1.Execute then
  begin
    SQLQuery.DisableControls;
    SQLQuery.First;
    AssignFile(F, SaveDialog1.FileName);
    Rewrite(F);
    Writeln(F, '<table border=0><tr bgcolor="DDDDDD">');
    for i:= 0 to SQLQuery.FieldCount - 1 do
    begin
      Write(F, '<th>', SQLQuery.Fields[i].FieldName, '</th>');
      if i = SQLQuery.FieldCount - 1 then
        Writeln(F, '</tr>');
    end;

    while not SQLQuery.EOF do
    begin
      Write(f, '<tr bgcolor="');
      if SQLQuery.RecNo mod 2 = 0 then
        Write(F, '#EEDDFF">')
      else
        Write(F, '#FFFFFF">');

      for i:= 0 to SQLQuery.FieldCount - 1 do
      begin
        Write(F, '<td>', SQLQuery.Fields[i].AsString, '</td>');
        if i = SQLQuery.FieldCount - 1 then
          Writeln(F, '</tr>');
      end;
      SQLQuery.Next;
    end;
    Writeln(F, '</table>');
    CloseFile(F);
    SQLQuery.EnableControls;
  end;

end;



procedure TfmQueryWindow.lmPasteClick(Sender: TObject);
begin
  meQuery.PasteFromClipboard;
end;

procedure TfmQueryWindow.lmRedoClick(Sender: TObject);
begin
  meQuery.Redo;
end;

procedure TfmQueryWindow.lmRunClick(Sender: TObject);
begin
  CallExecuteQuery(0);
end;

procedure TfmQueryWindow.lmRunExecClick(Sender: TObject);
begin
  CallExecuteQuery(2);
end;

procedure TfmQueryWindow.lmRunScriptClick(Sender: TObject);
begin
  CallExecuteQuery(3);
end;

procedure TfmQueryWindow.lmRunSelectClick(Sender: TObject);
begin
  CallExecuteQuery(1);
end;

procedure TfmQueryWindow.lmSelectAllClick(Sender: TObject);
begin
  meQuery.SelectAll;
end;


procedure TfmQueryWindow.lmUndoClick(Sender: TObject);
begin
  meQuery.Undo;
end;


procedure TfmQueryWindow.lmFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TfmQueryWindow.lmFindAgainClick(Sender: TObject);
begin
  meQuery.SearchReplace(FindDialog1.FindText, '', fOptions);
end;

procedure TfmQueryWindow.meQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Execute query by pressing Ctrl + Enter
  if (ssCtrl in shift) and (key = 13) then
  begin
    CallExecuteQuery(0);
    key:= 0;
  end;
end;


procedure TfmQueryWindow.QueryAfterScroll(DataSet: TDataSet);
var
  TabSheet: TTabSheet;
  i: Integer;
begin
  TabSheet:= nil;
  // Get DataSet's TTabsheet
  for i:= 0 to High(ResultControls) do
  if ResultControls[i] <> nil then
  if DataSet = ResultControls[i] then
  begin
    TabSheet:= ParentResultControls[i] as TTabSheet;
    Break;
  end;

  // Search for status bar inside current query result TabSheet
  if TabSheet <> nil then
  for i:= 0 to High(ResultControls) do
  if ResultControls[i] <> nil then
    if  (ParentResultControls[i] <> nil) and ((ParentResultControls[i] as TTabSheet) = TabSheet)
      and (ResultControls[i] is TStatusBar) then
      begin
        // Display current record and number of total records in status bar
        (ResultControls[i] as TStatusBar).SimpleText:= IntToStr(DataSet.RecordCount) +
        ' records fetched. At record # ' + IntToStr(DataSet.RecNo);
      break;
  end;

end;

procedure TfmQueryWindow.CallExecuteQuery(aQueryType: Integer);
begin
  fList:= TStringList.Create;

  // Get query text from memo
  fQuery:= Trim(GetQuery);
  fList.Text:= fQuery;
  fStartLine:= 0;

  // Disable buttons to prevent query interrupt
  tbRun.Enabled:= False;
  tbCommit.Enabled:= False;
  tbCommitRetaining.Enabled:= False;
  tbRollback.Enabled:= False;
  tbRollbackRetaining.Enabled:= False;

  fModifyCount:= 0;
  RemoveControls;

  // Get initial query type, it could be changed later in the next parts
  if aQueryType = 0 then // Auto
    fOrigQueryType:= GetQueryType(fQuery)
  else
    fOrigQueryType:= aQueryType;

  // Call execute query for each parts until finished
  fCnt:= 0;
  fFinished:= False;
  repeat
    ExecuteQuery;
  until fFinished;
  EnableButtons;
end;

procedure TfmQueryWindow.sortsyncompletion;
var
  sortinglis:TStringList;
  i:Integer;
begin
  sortinglis:=TStringList.Create;
    for i:=0 to SynCompletion1.ItemList.Count-1 do
        sortinglis.Add(SynCompletion1.ItemList.Strings[i]);
    sortinglis.Sort;
    SynCompletion1.ItemList.Clear;
    for i:=0 to sortinglis.Count-1 do
    SynCompletion1.ItemList.Add(sortinglis.Strings[i]);

end;

procedure TfmQueryWindow.ThreadTerminated(Sender: TObject);
begin
  // Raise exception if an error occured during thread execution (Open)
  if fQT.Error then
  begin
    if Assigned(fTab) then
      fTab.TabVisible:= False;
    SetLength(ResultControls, High(ResultControls));
    SetLength(ParentResultControls, High(ParentResultControls));
    fTab:= CreateResultTab(2, fSqlQuery, fSqlScript, fmeResult);
    PageControl1.ActivePage:= fTab;

    fmeResult.Text:= fQT.ErrorMsg;
    fmeResult.Lines.Add(fQueryPart);
    fmeResult.Font.Color:= clRed;
    fTab.Font.Color:= clRed;
    fTab.ImageIndex:= 3;
  end
  else
  begin
    fTab.Caption:= faText;
    fTab.ImageIndex:= 0;
    fmMain.AddToSQLHistory(RegRec.Title, 'SELECT', fQueryPart);
  end;
  fQT.Free;
  if fFinished then
    EnableButtons;

  if not fFinished then
    ExecuteQuery;

end;

procedure TfmQueryWindow.EnableButtons;
begin
  tbRun.Enabled:= True;
  tbCommit.Enabled:= True;
  tbCommitRetaining.Enabled:= True;
  tbRollback.Enabled:= True;
  tbRollbackRetaining.Enabled:= True;
end;

initialization
  {$I querywindow.lrs}

end.

