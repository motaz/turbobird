unit QueryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, db, sqldb, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, PairSplitter, StdCtrls, Buttons,
  DBGrids, Menus, ComCtrls, SynEdit, SynHighlighterSQL, Reg,
  SynEditTypes, SynCompletion, Clipbrd, grids, DbCtrls, types, LCLType, modsqlscript,dbugintf;

type

  TQueryTypes = (
    qtUnknown=0,
    qtSelectable=1,
    qtExecute=2,
    qtScript=3);

  TQueryActions = (
    qaCommit,
    qaCommitRet,
    qaRollBack,
    qaRollbackRet,
    qaOpen,
    qaDDL,
    qaExec );


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
      fType: TQueryActions;
      fStatement: string;
      property Query: TSQLQuery read fSQLQuery write fSQLQuery;
      property Trans: TSQLTransaction read fTrans write fTrans;
      property Connection: TIBConnection read fConnection write fConnection;
      property Statement: String read fStatement write fStatement;
      procedure DoJob;
      procedure Execute; override;
      constructor Create(aType: TQueryActions);
  end;


  { TfmQueryWindow }

  TfmQueryWindow = class(TForm)
    bbClose: TBitBtn;
    cxAutoCommit: TCheckBox;
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
    ToolButton1: TToolButton;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    fdbIndex: Integer; // Index of selected registered database
    RegRec: TRegisteredDatabase;
    fResultControls: array of TObject;
    fParentResultControls: array of TObject;
    fOptions: set of TSynSearchOption;
    ibConnection: TIBConnection;
    fSqlTrans: TSQLTransaction;
    fCanceled: Boolean;
    fStartLine: Integer;
    fQuery: TStringList; //query text
    fOrigQueryType: TQueryTypes;
    fFinished: Boolean;
    fQT: TQueryThread;
    fQueryPart: string;
    fTab: TTabSheet;
    fmeResult: TMemo;
    fSqlQuery: TSQLQuery;
    fSqlScript: TModSQLScript;
    // Text for caption
    faText: string;
    fModifyCount: Integer;
    fCnt: Integer;
    fModifiedRecords: array of array of Integer;

    procedure EnableCommitButton;
    procedure ExecuteQuery;
    function GetNewTabNum: string;
    procedure FinishCellEditing(DataSet: TDataSet);
    function GetRecordSet(TabIndex: Integer): TSQLQuery;
    // Gets both querytype and whether SQL is DML or DDL
    // Investigates QueryList[LookAtIndex] to find out
    function GetQuerySQLType(QueryList: TStringList; var LookAtIndex: Integer;
      var IsDDL: Boolean): TQueryTypes;
    procedure NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
    procedure RemoveComments(QueryList: TStringList; StartLine: Integer;
      var RealStartLine: Integer);
    procedure RemoveAllSingleLineComments(QueryList: TStringList);
    procedure RemoveEmptyLines(QueryList: TStringList;
      var SecondRealStart: Integer; const RealStartLine: Integer);
    procedure InsertModifiedRecord(RecordNo, TabIndex: Integer);
    procedure ApplyClick(Sender: TObject);
    procedure EnableApplyButton;
    function GetTableName(SQLText: string): string;
    function GetCurrentSQLText: string;
    procedure CommitResultClick(Sender: TObject);
  protected
    // This procedure will receive the events that are logged by the connection:
    procedure GetLogEvent(Sender: TSQLConnection; EventType: TDBEventType; Const Msg : String);
  public
    OnCommit: TNotifyEvent;
    procedure Init(dbIndex: Integer);
    function GetQueryType(AQuery: string): TQueryTypes;
    // Get query text from GUI/memo into
    // QueryContents
    function GetQuery(QueryContents: tstrings): boolean;
    function CreateResultTab(QueryType: TQueryTypes; var aSqlQuery: TSQLQuery; var aSQLScript: TModSQLScript;
      var meResult: TMemo; AdditionalTitle: string = ''): TTabSheet;
    // Runs SQL script; returns result
    function ExecuteScript(Script: string): Boolean;
    procedure NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
    function FindSqlQuery: TSqlQuery;
    // Returns whether query is DDL or DML
    function GetSQLType(Query: string; var Command: string): string;
    // Tries to split up text into separate queries
    function GetSQLSegment(QueryList: TStringList; StartLine: Integer;
      var QueryType: TQueryTypes; var EndLine: Integer;
      var SQLSegment: string; var IsDDL: Boolean): Boolean;
    procedure QueryAfterScroll(DataSet: TDataSet);
    // Run query; use aQueryType to force running as e.g. script or open query
    procedure CallExecuteQuery(aQueryType: TQueryTypes);
    procedure SortSynCompletion;
    procedure ThreadTerminated(Sender: TObject);
    procedure EnableButtons;

    { public declarations }
  end; 

var
  fmQueryWindow: TfmQueryWindow;

implementation



uses main, SQLHistory;

{ TfmQueryWindow }
{ NewCommitButton: Create commit button for editable query result }

procedure TfmQueryWindow.NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
var
  Commit: TBitBtn;
begin
  Commit:= TBitBtn.Create(self);
  Commit.Parent:= Pan;
  Commit.Caption:= 'Commit';
  Commit.Left:= 400;
  Commit.Visible:= False;
  Commit.OnClick:= @CommitResultClick;
  Commit.Tag:= ATab.TabIndex;
end;


{ RemoveComments: Remove comments from Query window }

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

    // Avoid checking for comments if there's any chance they're within
    // a string literal e.g. select 'this is -- no -- comment' from rdb$database
    if (not MultiComment) and (pos('''',QueryList[i])=0) then
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
      //todo: verify check of -- regarding string literals below.
      if (i = QueryList.Count - 1) or
         ((Trim(QueryList[i + 1]) <> '') and  (Pos('/*', Trim(QueryList[i + 1])
           ) <> 1) and
         (Pos('--', Trim(QueryList[i + 1])) <> 1)) then
          Break;
    end;
  end;
end;


{ RemoveAllSingleLineComments: remove single line comments from query }

procedure TfmQueryWindow.RemoveAllSingleLineComments(QueryList: TStringList);
var
  i: Integer;
begin
  for i:= QueryList.Count - 1 downto 0 do
  begin
    if Pos('--', QueryList[i]) > 0 then
    begin
      if Pos('--', Trim(QueryList[i])) = 1 then
        QueryList.Delete(i);
      {
      else
        // this will also pick up -- within string literals which is wrong
        QueryList[i]:= Copy(QueryList[i], 1, Pos('--', QueryList[i]) - 1);
      }
    end;
  end;
end;


{ RemoveEmptyLines: remove empty lines in query }

procedure TfmQueryWindow.RemoveEmptyLines(QueryList: TStringList; var SecondRealStart: Integer;
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


{ FinishCellEditing: Insert current just edited record in ModifiedRecords array }

procedure TfmQueryWindow.FinishCellEditing(DataSet: TDataSet);
begin
  InsertModifiedRecord(Dataset.RecNo, PageControl1.TabIndex);
end;



{ InsertModifiedRecord: insert modified query record in ModifiedRecords array }

procedure TfmQueryWindow.InsertModifiedRecord(RecordNo, TabIndex: Integer);
var
  i: Integer;
  Exists: Boolean;
begin
  Exists:= False;
  if TabIndex > High(fModifiedRecords) then // Insert new tab
  begin
    SetLength(fModifiedRecords, TabIndex + 1);
  end;

  // Check if record already inserted
  for i:= 0 to High(fModifiedRecords[TabIndex]) do
  begin
    if fModifiedRecords[TabIndex][i] = RecordNo then
    begin
      Exists:= True;
      Break;
    end;
  end;

  if not Exists then  // Insert record pointer
  begin
    setLength(fModifiedRecords[TabIndex], Length(fModifiedRecords[TabIndex]) + 1);
    fModifiedRecords[TabIndex][High(fModifiedRecords[TabIndex])]:= RecordNo;
  end;

  // Enable apply/save button
  if Length(fModifiedRecords[TabIndex]) = 1 then
  begin
    EnableApplyButton;
  end;
end;


{ ApplyClick: Save Updates for the query }

procedure TfmQueryWindow.ApplyClick(Sender: TObject);
var
  i, x: Integer;
  aTableName: string;
  aQuery: TSQLQuery;
  PKIndexName: string;
  ConstraintName: string;
  KeyList, FieldsList: TStringList;
  WhereClause: string;
  RecordSet: TSQLQuery;
  TabIndex: Integer;
  //todo: review this and use regular FPC databound controls? autogenerated updatesql etc
  FieldsSQL: string;
begin
  try
    TabIndex:= PageControl1.TabIndex;
    aTableName:= GetTableName(GetCurrentSQLText);
    RecordSet:= GetRecordSet(TabIndex);

    // Get primary key name
    PKIndexName:= fmMain.GetPrimaryKeyIndexName(fdbIndex, ATableName, ConstraintName);
    if PKIndexName <> '' then
    begin
      KeyList:= TStringList.Create;
      Fieldslist:= TStringList.Create;
      aQuery:= TSQLQuery.Create(nil);
      try
        aQuery.DataBase:= ibConnection;
        aQuery.Transaction:= fSqlTrans;

        // Get primary key fields
        fmMain.GetIndexFields(ATableName, PKIndexName, aQuery, KeyList);
        fmMain.GetFields(fdbIndex, ATableName, FieldsList);
        WhereClause:= 'where ';

        RecordSet.DisableControls;
        // Check modified fields
        for i:= Low(fModifiedRecords[TabIndex]) to High(fModifiedRecords[TabIndex]) do
        begin
          FieldsSQL:= '';
          RecordSet.RecNo:= fModifiedRecords[TabIndex][i];
          for x:= 0 to RecordSet.Fields.Count - 1 do
          begin
            if (FieldsList.IndexOf(RecordSet.Fields[x].FieldName) <> -1) and  // Field exist in origional table
              (RecordSet.Fields[x].NewValue <> RecordSet.Fields[x].OldValue) then // field data has been modified
            begin
              if FieldsSQL <> '' then
                FieldsSQL += ',';
              FieldsSQL += RecordSet.Fields[x].FieldName + '=';

              // Typecast field values according to their main type
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
          end;

          // Update current record
          if FieldsSQL <> '' then
          begin
            aQuery.Close;
            aQuery.SQL.Text:= 'update ' + aTableName + ' set ' + FieldsSQL;

            WhereClause:= 'where ';
            // where clause
            for x:= 0 to KeyList.Count - 1 do
            begin
              if Trim(KeyList[x]) <> '' then
              begin
                WhereClause += KeyList[x] + ' = ';

                // Typecast index values
                case RecordSet.Fields[x].DataType of
                  ftInteger, ftSmallint: WhereClause += IntToStr(RecordSet.Fields[x].OldValue);
                  ftFloat: WhereClause += FloatToStr(RecordSet.Fields[x].OldValue);
                else
                  WhereClause += '''' + RecordSet.Fields[x].OldValue + '''';
                end;
                if x < KeyList.Count - 1 then
                  WhereClause += ' and ';
              end;
            end;
            aQuery.SQL.Add(WhereClause);
            aQuery.ExecSQL;
            (Sender as TBitBtn).Visible:= False;

            // Auto commit
            if cxAutoCommit.Checked then
              fSqlTrans.CommitRetaining
            else
              EnableCommitButton;
          end;
        end;

        // Reset fModifiedRecords pointer
        fModifiedRecords[TabIndex]:= nil;
        RecordSet.EnableControls;
      finally
        FieldsList.Free;
        KeyList.Free;
        aQuery.Free;
      end;
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

{ EnableApplyButton: enable save updates button when records have been modified }

procedure TfmQueryWindow.EnableApplyButton;
var
  i: Integer;
begin
  for i:= 0 to High(fResultControls) do
  if (fResultControls[i] is TBitBtn) and ((fResultControls[i] as TBitBtn).Tag = PageControl1.TabIndex) and
    ((fResultControls[i] as TBitBtn).Caption = 'Apply') then
  begin
    (fResultControls[i] as TBitBtn).Visible:= True;
    Break;
  end;
end;


{ EnableCommitButton: enable commit button after applying updates }

procedure TfmQueryWindow.EnableCommitButton;
var
  i: Integer;
begin
  for i:= 0 to High(fResultControls) do
  if (fResultControls[i] is TBitBtn) and ((fResultControls[i] as TBitBtn).Tag = PageControl1.TabIndex)
    and ((fResultControls[i] as TBitBtn).Caption = 'Commit') then
  begin
    (fResultControls[i] as TBitBtn).Visible:= True;
    Break;
  end;
end;


{ GetTableName: get table name from query text }

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


{ GetCurrentSQLText: return current SQL query text }

function TfmQueryWindow.GetCurrentSQLText: string;
var
  i: Integer;
begin
  for i:= 0 to High(fResultControls) do
  begin
    if (fResultControls[i] is TDBGrid) and ((fResultControls[i] as TDBGrid).Tag = PageControl1.TabIndex) then
    begin
      Result:= ((fResultControls[i] as TDBGrid).DataSource.DataSet as TSQLQuery).SQL.Text;
      Break;
    end;
  end;
end;


{ CommitResultClick: commit current transaction }

procedure TfmQueryWindow.CommitResultClick(Sender: TObject);
begin
  fSqlTrans.CommitRetaining;
  (Sender as TBitBtn).Visible:= False;
end;

procedure TfmQueryWindow.GetLogEvent(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
// Used to log everything sent through the connection
var
  Source: string;
begin
  case EventType of
    detCustom:   Source:='Custom:  ';
    detPrepare:  Source:='Prepare: ';
    detExecute:  Source:='Execute: ';
    detFetch:    Source:='Fetch:   ';
    detCommit:   Source:='Commit:  ';
    detRollBack: Source:='Rollback:';
    else Source:='Unknown event. Please fix program code.';
  end;
  SendDebug(Source + Msg);
end;


{ GetRecordSet: return result recordset of a page tab }

function TfmQueryWindow.GetRecordSet(TabIndex: Integer): TSQLQuery;
var
  i: Integer;
begin
  for i:= 0 to High(fResultControls) do
  begin
    if (fResultControls[i] is TSQLQuery) and
      ((fResultControls[i] as TSQLQuery).Tag = TabIndex) then
    begin
      Result:= fResultControls[i] as TSQLQuery;
      Break;
    end;
  end;
end;


{ GetQuerySQLType: get query type: select, script, execute from current string list }

function TfmQueryWindow.GetQuerySQLType(QueryList: TStringList; var LookAtIndex: Integer; var IsDDL: Boolean): TQueryTypes;
var
  MassagedSQL: string;
begin
  Result:= qtUnknown;
  IsDDL:= False; //default
  if LookAtIndex < QueryList.Count then
  begin
    MassagedSQL:= LowerCase(Trim(QueryList[LookAtIndex]));

    // Script overrides rest
    if Pos('set term', MassagedSQL) = 1 then
    begin
      // Using set term does not mean the SQL you're running has to be
      // DDL (could be an execute block or something) but it most probably is
      IsDDL:= true;
      exit(qtScript);
    end;

    if (Pos('select', MassagedSQL) = 1) then
      { todo: low priority misses insert...returning,
       update...returning, merge.. returning...}
      Result:= qtSelectable
    else
    begin
      Result:= qtExecute;
      IsDDL:= (Pos('alter', MassagedSQL) = 1) or
        (Pos('create', MassagedSQL) = 1) or
        (Pos('drop', MassagedSQL) = 1) or
        (Pos('grant', MassagedSQL) = 1) {actually DCL} or
        (Pos('revoke', MassagedSQL) = 1) {actually DCL};
    end;
  end;
end;


{ DoJob: Execute thread job: open query, execute, commit, rollback, etc }

procedure TQueryThread.DoJob;
begin
  try
    if fType = qaOpen then
      fSQLQuery.Open
    else
    if fType = qaExec then
      fSQLQuery.ExecSQL
    else
    if fType = qaDDL then
      fConnection.ExecuteDirect(fStatement)
    else
    if fType = qaCommit then
      fTrans.Commit
    else
    if fType = qaCommitRet then
      fTrans.CommitRetaining
    else
    if fType = qaRollBack then
      fTrans.Rollback
    else
    if fType = qaRollbackRet then
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


{ Execute: Query thread main loop }

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


{ Create query thread }

constructor TQueryThread.Create(aType: TQueryActions);
begin
  inherited Create(True);
  fType:= aType;
  FreeOnTerminate:= False;
end;


{ Display SQL script exception message }

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


{ Close button pressed: close current Query window and free parent page tab }

procedure TfmQueryWindow.tbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;


{ Commit current transaction }

procedure TfmQueryWindow.tbCommitClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TSQLQuery;
  SqlScript: TModSQLScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  ATab:= CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create(qaCommit);
  try
    QT.Trans:= fSqlTrans;
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


{ Commit retaining for current transaction }

procedure TfmQueryWindow.tbCommitRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create(qaCommitRet);
  try
    QT.Trans:= fSqlTrans;

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


{HistoryClick: show SQL history form }

procedure TfmQueryWindow.tbHistoryClick(Sender: TObject);
begin
  fmSQLHistory.Init(RegRec.Title, Self);
  fmSQLHistory.Show;
end;


{ Display popup menu }

procedure TfmQueryWindow.tbMenuClick(Sender: TObject);
begin
  pmTab.PopUp;
end;


{ display New SQL Window tab }

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


{ Read SQL query from text file }

procedure TfmQueryWindow.tbOpenClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt:= '.sql';
  if OpenDialog1.Execute then
    meQuery.Lines.LoadFromFile(OpenDialog1.FileName);
end;


{ RollBack current transaction }

procedure TfmQueryWindow.tbRollbackClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TSQLQuery;
  SqlScript: TModSQLScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  ATab:= CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create(qaRollBack);
  try
    QT.Trans:= fSqlTrans;
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


{ Rollback retaning for current transaction }

procedure TfmQueryWindow.tbRollbackRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create(qaRollbackRet);
  try
    QT.Trans:= fSqlTrans;

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


{ Run current SQL, auto-detect type }
procedure TfmQueryWindow.tbRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;


{ Save current SQL in a text file }

procedure TfmQueryWindow.tbSaveClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.sql';
  if SaveDialog1.Execute then
    meQuery.Lines.SaveToFile(SaveDialog1.FileName);
end;


{GetNewTabNum: get last tab number and increase result by one }

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


{ Initialize query window: fill connection parameters from selected registered database }

procedure TfmQueryWindow.Init(dbIndex: Integer);
begin
  fdbIndex:= dbIndex;
  RegRec:= fmMain.RegisteredDatabases[dbIndex].RegRec;

  // Set instances of IBConnection and SQLTransaction for the current Query Window
  fmMain.setTransactionIsolation(fSqlTrans.Params);
  fSqlTrans.DataBase:= ibConnection;

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
  SortSynCompletion;
end;

(************* Is Selectable (Check statement type Select, Update, Alter, etc) *******************)

function TfmQueryWindow.GetQueryType(AQuery: string): TQueryTypes;
var
  List: TStringList;
  i: Integer;
  Line: string;
  StartPos, EndPos: Integer;
begin
  List:= TStringList.Create;
  try
    List.Text:= AQuery;

    Result:= qtExecute; // Default Execute

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
        Result:= qtSelectable; // Selectable
        Break;
      end
      else
      if Pos('set term', LowerCase(Trim(Line))) = 1 then
      begin
        Result:= qtScript;
        Break;
      end;

      if Trim(Line) <> '' then
      begin
        Result:= qtExecute; // Executable
        Break;
      end;
    end;
  finally
    List.Free;
  end;
end;


{ GetQuery: get query text from editor }

function TfmQueryWindow.GetQuery(QueryContents: TStrings): boolean;
var
  Seltext: string;
begin
  Result:= false;
  if assigned(QueryContents) then
  begin
    SelText:= trim(meQuery.SelText);
    if SelTExt<>'' then
      QueryContents.Text:= SelText
    else
      QueryContents.Text:= trim(meQuery.Lines.Text);
    Result:= true;
  end;
end;


{ Create new result tab depending on query type }

function TfmQueryWindow.CreateResultTab(QueryType: TQueryTypes;
  var aSqlQuery: TSQLQuery; var aSQLScript: TModSQLScript; var meResult: TMemo;
  AdditionalTitle: string): TTabSheet;
var
  ATab: TTabSheet;
  DBGrid: TDBGrid;
  DataSource: TDataSource;
  StatusBar: TStatusBar;
  Nav: TDBNavigator;
  Pan: TPanel;
begin
  ATab:= TTabSheet.Create(self);
  BeginUpdateBounds;
  Result:= ATab;
  ATab.Parent:= PageControl1;
  ATab.Caption:= 'Result # ' + GetNewTabNum + ' ' + AdditionalTitle;
  if QueryType = qtSelectable then // Select, need record set result
  begin
    // Query
    aSqlQuery:= TSQLQuery.Create(self);
    aSqlQuery.DataBase:= ibConnection;
    aSqlQuery.Transaction:= fSqlTrans;
    aSqlQuery.AfterScroll:= @QueryAfterScroll;
    aSqlQuery.AfterPost:= @FinishCellEditing;
    aSqlQuery.Tag:= ATab.TabIndex;

    // Status Bar
    StatusBar:= TStatusBar.Create(self);
    StatusBar.Parent:= ATab;

    // Datasource
    DataSource:= TDataSource.Create(self);
    DataSource.DataSet:= aSqlQuery;

    // Panel
    pan:= TPanel.Create(self);
    pan.Parent:= ATab;
    Pan.Height:= 30;
    Pan.Align:= alTop;

    // Query result Grid
    DBGrid:= TDBGrid.Create(self);
    DBGrid.Parent:= ATab;
    DBGrid.DataSource:= DataSource;
    DBGrid.Align:= alClient;
    DBGrid.OnDblClick:= @DBGrid1DblClick;

    DBGrid.Tag:= ATab.TabIndex;
    DBGrid.ReadOnly:= False;
    DBGrid.AutoEdit:= True;

    DBGrid.PopupMenu:= pmGrid;
    DBGrid.TitleStyle:= tsNative;
    DBGrid.Options:= DBGrid.Options + [dgAutoSizeColumns, dgHeaderHotTracking, dgHeaderPushedLook, dgAnyButtonCanSelect];

    DBGrid.OnTitleClick:= @DBGridTitleClick;

    // Navigator
    Nav:= TDBNavigator.Create(self);
    Nav.Parent:= Pan;
    Nav.VisibleButtons:= [nbFirst, nbNext, nbPrior, nbLast];
    Nav.DataSource:= DataSource;

    // Apply button
    NewApplyButton(Pan, ATab);

    // Commit button
    NewCommitButton(Pan, ATab);
  end
  else
  if QueryType in [qtExecute, qtScript] then
  begin
    meResult:= TMemo.Create(self);
    meResult.Parent:= ATab;
    meResult.ReadOnly:= True;
    meResult.Align:= alClient;
    case QueryType of
      qtExecute:
      begin
        aSqlQuery:= TSQLQuery.Create(self);
        aSqlQuery.DataBase:= ibConnection;
        aSqlQuery.Transaction:= fSqlTrans;
      end;
      qtScript: // Script
      begin
        aSQLScript:= TModSQLScript.Create(self);
        aSQLScript.DataBase:= ibConnection;
        aSQLScript.Transaction:= fSqlTrans;
        aSQLScript.CommentsInSQL:= true;
        aSQLScript.UseSetTerm:= true; //needed if set term is used, e.g. for stored procedures
      end;
    end;
  end;
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
  fQueryType: TQueryTypes;
begin
  try
    // Script
    if (fOrigQueryType = qtScript) then
    begin // script
      ExecuteScript(fQuery.Text);
      Inc(fModifyCount);
      SqlType:= GetSQLType(fQuery.Text, Command);
      fmMain.AddToSQLHistory(RegRec.Title, SqlType, fQuery.Text);
      fFinished:= True;
      fQuery.Clear;
    end
    else  // normal statement / Multi statements
    begin
      Inc(fCnt);
      if not GetSQLSegment(fQuery, fStartline, fQueryType, EndLine, fQueryPart, IsDDL) then
      begin
        fFinished:= True;
        Exit;
      end;

      {if EndLine < fStartLine then
        fStartLine:= fStartLine + 1
      else}
        fStartLine:= EndLine + 1;

      if Trim(fQueryPart) <> '' then   // Select
      if fQueryType = qtSelectable then
      begin
        fTab:= nil;
        try
          fTab:= CreateResultTab(qtSelectable, fSqlQuery, fSqlScript, fmeResult);
          fTab.ImageIndex:= 6;
          fTab.Hint:= fQueryPart;
          fTab.ShowHint:= True;
          fSQLQuery.SQL.Text:= fQueryPart;

          // Create thread to open dataset
          fQT:= TQueryThread.Create(qaOpen);
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
          on e: Exception do
          begin
            if Assigned(fTab) then
              fTab.TabVisible:= False;
            SetLength(fResultControls, High(fResultControls));
            SetLength(fParentResultControls, High(fParentResultControls));
            fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);
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
        if fQueryType = qtExecute then
        begin
          fTab:= nil;
          fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);

          fTab.ImageIndex:= 1;
          SqlType:= GetSQLType(fQueryPart, Command);
          StartTime:= Now;
          Affected:= 0;
          try
            if IsDDL then
            begin
              // Execute the statement in thread
              fQT:= TQueryThread.Create(qaDDL);
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

              fTab.Caption:= faText;

              // Auto commit
              if cxAutoCommit.Checked then
                fSqlTrans.Commit;
              fQT.Free;
            end
            else
            begin // DML
              fSqlQuery.Close;
              fSqlQuery.SQL.Text:= fQueryPart;
              fTab.ImageIndex:= 6;
              fTab.Hint:= fQueryPart;
              fTab.ShowHint:= True;
              fSQLQuery.SQL.Text:= fQueryPart;

              // Execute the statement in thread
              fQT:= TQueryThread.Create(qaExec);
              try
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

                // Auto commit
                if cxAutoCommit.Checked then
                  fSqlTrans.Commit;
              finally
                fQT.Free;
              end;
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
              fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);
              PageControl1.ActivePage:= fTab;
              fmeResult.Text:= e.message;
              fmeResult.Lines.Add(fQueryPart);
              fmeResult.Font.Color:= clRed;
              fTab.Font.Color:= clRed;
              fTab.ImageIndex:= 3;
            end;
          end;
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
              fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);
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
        begin
          if (MessageDlg('Commit', 'There are too many transactions, do you want to commit',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            fSqlTrans.CommitRetaining;
            fModifyCount:= 0;
          end
          else
          begin
            fModifyCount:= 0;
          end;
        end;
      if fStartLine >= fQuery.Count then
        fFinished:= True;
    end;
  except
    on e: exception do
    begin
      if Assigned(fTab) then
        fTab.TabVisible:= False;
      fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);
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


{ Execute script }

function TfmQueryWindow.ExecuteScript(Script: string): Boolean;
var
  StartTime: TDateTime;
  SqlQuery: TSQLQuery;
  SqlScript: TModSQLScript;
  meResult: TMemo;
  ATab: TTabSheet;
begin
  StartTime:= Now;
  ATab:= nil;
  SQLScript:= nil;
  try
    // CreateResultTab creates the SQLScript object for us.
    ATab:= CreateResultTab(qtScript, SqlQuery, SQLScript, meResult);
    try
      ATab.ImageIndex:= 2;
      SQLScript.Script.Text:= Script;
      {$IFDEF DEBUG}
      SendDebug('going to run script: '+SQLScript.Script.Text);
      {$Endif}
      SQLScript.ExecuteScript;

      // Auto commit
      if cxAutoCommit.Checked then
        fSqlTrans.Commit;

      Result:= True;
      meResult.Lines.Text:= FormatDateTime('hh:nn:ss.z', Now) + ' - Script Executed. It took (H:M:S.MS) ' +
        FormatDateTime('HH:nn:ss.z', Now - StartTime);
      meResult.Lines.Add('--------');
      meResult.Lines.Add(Script);
    finally
      SQLScript.Free;
    end;
  except
    on e: exception do
    begin
      {$IFDEF DEBUG}
      SendDebug('ExecuteScript failed; error '+E.Message);
      {$Endif}
      Result:= False;
      if Assigned(ATab) then
        ATab.TabVisible:= False;
      ATab:= CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);
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



{ Display new Save/Apply button for current query result been edited }

procedure TfmQueryWindow.NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
var
  Apply: TBitBtn;
begin
  Apply:= TBitBtn.Create(self);
  Apply.Parent:= Pan;
  Apply.Caption:= 'Apply';
  Apply.Left:= 300;
  Apply.Visible:= False;
  Apply.OnClick:= @ApplyClick;
  Apply.Tag:= ATab.TabIndex;
end;


{ FindSQLQuery: Return current TSQLQuery component from current query window }

function TfmQueryWindow.FindSqlQuery: TSqlQuery;
var
  i: Integer;
begin
  Result:= nil;
  if PageControl1.PageCount > 0 then
  begin
    with PageControl1.ActivePage do
    begin
      for i:= 0 to ControlCount - 1 do
      begin
        if Controls[i] is TDBGrid then
        begin
          Result:= TSqlQuery((Controls[i] as TDBGrid).DataSource.DataSet);
          Break;
        end;
      end;
    end;
  end;
end;


{ GetSQLType: get SQL type of current SQL text }

function TfmQueryWindow.GetSQLType(Query: string; var Command: string): string;
begin
  Result:= 'DML'; //default
  Query:= Trim(Query);
  if (Query <> '') and (Pos(' ', Query) > 0) then
  begin
    // to do: this does not take comments into account...
    Command:= Copy(Query, 1, Pos(' ', Query) - 1);
    Command:= LowerCase(Command);
    if (Command = 'alter') or
       (Command = 'create') or
       (Command = 'drop') or
       (Command = 'grant') {actually DCL} or
       (Command = 'revoke') {actually DCL} then
      Result:= 'DDL';
  end;
end;


{ GetSQLSeqment: read part of SQL end by ; }

function TfmQueryWindow.GetSQLSegment(QueryList: TStringList; StartLine: Integer;
  var QueryType: TQueryTypes; var EndLine: Integer;
  var SQLSegment: string; var IsDDL: Boolean): Boolean;
var
  i: Integer;
  RealStartLine: Integer;
  SecondRealStart: Integer;
  BeginExists: Boolean;
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
  RemoveEmptyLines(QueryList, SecondRealStart, RealStartLine);

  // Get SQL type
  QueryType:= GetQuerySQLType(QueryList, SecondRealStart, IsDDL);

  // Concatenate
  SQLSegment:= '';
  BeginExists:= False;
  for i:= SecondRealStart to QueryList.Count - 1 do
  begin
    if Pos('begin', Trim(LowerCase(QueryList[i]))) > 0 then
      BeginExists:= True;

    SQLSegment:= SQLSegment + QueryList[i] + LineEnding;

    if (QueryType in [qtSelectable, qtExecute]) and
      (((Pos(';', QueryList[i]) > 0) and (Not BeginExists)) or
      ((Pos('end', LowerCase(Trim(QueryList[i]))) = 1) and BeginExists)
      or (i = QueryList.Count - 1)) then
    begin
      Result:= True;
      if (not BeginExists) and (Pos(';', QueryList[i]) > 0) then
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
    if (QueryType = qtScript) and
      ((i > SecondRealStart) and (Pos('set term', LowerCase(Trim(QueryList[i]))) = 1)) or
      (i = QueryList.Count - 1) then
    begin
      Result:= True;
      EndLine:= i;
      Break;
    end;
  end;
end;



{ Run query, 0 for auto-detect query type }

procedure TfmQueryWindow.bbRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;


{ Display Blob contents in a message box }

procedure TfmQueryWindow.DBGrid1DblClick(Sender: TObject);
begin
  ShowMessage((Sender as TDBGrid).SelectedField.AsString)
end;


{ Sort by columns }

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


{ Find text }

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


{ QueryWindow onClose event, commit active transaction, remove controls }

procedure TfmQueryWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // Check if the transaction is active; then commit it
  if fSqlTrans.Active then
  begin
    fSqlTrans.CommitRetaining;
    if OnCommit <> nil then
      OnCommit(self);
    OnCommit:= nil;
  end;
  IBConnection.Close;
  CloseAction:= caFree;
end;


{ Initialize auto-completion text in QueryWindow OnCreate event }

procedure TfmQueryWindow.FormCreate(Sender: TObject);
begin
  {$IFNDEF DEBUG}
  // Do not log to debug server if built as release instead of debug
  SetDebuggingEnabled(false);
  {$ENDIF}
  fQuery:= TStringList.Create;
  // Initialize new instance of IBConnection and SQLTransaction
  ibConnection:= TIBConnection.Create(nil);
  {$IFDEF DEBUG}
  ibConnection.OnLog:=@GetLogEvent;
  ibConnection.LogEvents:=[detCustom,detExecute,detCommit,detRollBack];
  {$ENDIF DEBUG}
  fSqlTrans:= TSQLTransaction.Create(nil);
  SynCompletion1.ItemList.CommaText:= 'create,table,Select,From,INTEGER,FLOAT';
  SortSynCompletion;
end;

procedure TfmQueryWindow.FormDestroy(Sender: TObject);
begin
  // Clean up resources to avoid memory leaks
  fSqlTrans.Free;
  IBConnection.Free;
  fQuery.Free;
end;

procedure TfmQueryWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if ((Trim(meQuery.Lines.Text) = '') or
      (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes))
      then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;


{ focus on Query SQL window editor on form show }

procedure TfmQueryWindow.FormShow(Sender: TObject);
begin
  meQuery.SetFocus;
end;


{ Close current Query window }

procedure TfmQueryWindow.lmCloseTabClick(Sender: TObject);
begin
  if (Trim(meQuery.Lines.Text) = '') or
    (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
  begin
    Close;
    Parent.Free;
  end;
end;


{ Save query result in a comma delemited file }

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


{ Copy query result in Clipboard }

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
    try
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
    finally
      List.Free;
    end;
  except
    on e: exception do
      ShowMessage(e.Message);
  end;
  grid.DataSource.DataSet.EnableControls;
end;


{ Copy cell in clipboard }

procedure TfmQueryWindow.lmCopyCellClick(Sender: TObject);
begin
  Clipboard.AsText:= TdbGrid(pmGrid.PopupComponent).SelectedField.AsString;
end;


{ Copy query text into clipboard }

procedure TfmQueryWindow.lmCopyClick(Sender: TObject);
begin
  meQuery.CopyToClipboard;
end;


{ Cut query text into clipboard}

procedure TfmQueryWindow.lmCutClick(Sender: TObject);
begin
  meQuery.CutToClipboard;
end;


{ Export to comma delimeted file }

procedure TfmQueryWindow.lmExportAsCommaClick(Sender: TObject);
begin
  lmCommaDelemitedClick(nil);
end;

{ Export as HTML }

procedure TfmQueryWindow.lmExportAsHTMLClick(Sender: TObject);
begin
  lmHTMLClick(nil);
end;


{ Save query result as HTML }

procedure TfmQueryWindow.lmHTMLClick(Sender: TObject);
var
  i: Integer;
  F: TextFile;
  SqlQuery: TSQLQuery;
begin
  SaveDialog1.DefaultExt:= '.htm';
  SqlQuery:= FindSqlQuery;
  if SqlQuery = nil then
    ShowMessage('There is no record set in result')
  else
  if (not SQLQuery.Active) or (SQLQuery.RecordCount = 0) then
    MessageDlg('No data', mtError, [mbOk], 0)
  else // Valid data, let's try to save:
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
      // Zebra stripes in output:
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



{ Paste from clipboard into SQL editor }

procedure TfmQueryWindow.lmPasteClick(Sender: TObject);
begin
  meQuery.PasteFromClipboard;
end;


{ SQL Editor Redo }

procedure TfmQueryWindow.lmRedoClick(Sender: TObject);
begin
  meQuery.Redo;
end;


{ Run Query, auto type detection }

procedure TfmQueryWindow.lmRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;


{ Run query and force its type as executable statement}

procedure TfmQueryWindow.lmRunExecClick(Sender: TObject);
begin
  CallExecuteQuery(qtExecute);
end;


{ Run query, and force its type as script }

procedure TfmQueryWindow.lmRunScriptClick(Sender: TObject);
begin
  CallExecuteQuery(qtScript);
end;


{ Run query, force its type as select statement }

procedure TfmQueryWindow.lmRunSelectClick(Sender: TObject);
begin
  CallExecuteQuery(qtSelectable);
end;


{ select all in SQL Editor }

procedure TfmQueryWindow.lmSelectAllClick(Sender: TObject);
begin
  meQuery.SelectAll;
end;


{ SQL Editor undo }

procedure TfmQueryWindow.lmUndoClick(Sender: TObject);
begin
  meQuery.Undo;
end;


{ Search in SQL Editor }

procedure TfmQueryWindow.lmFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;


{ Find again }

procedure TfmQueryWindow.lmFindAgainClick(Sender: TObject);
begin
  meQuery.SearchReplace(FindDialog1.FindText, '', fOptions);
end;


{ Run query by pressing Ctrl + Enter }

procedure TfmQueryWindow.meQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Execute query by pressing Ctrl + Enter
  if (ssCtrl in shift) and (key = VK_RETURN) then
  begin
    CallExecuteQuery(qtUnknown);
    key:= 0;
  end;
end;


{ Scrolling in query result recordset }

procedure TfmQueryWindow.QueryAfterScroll(DataSet: TDataSet);
var
  TabSheet: TTabSheet;
  i: Integer;
begin
  TabSheet:= nil;
  // Get DataSet's TTabsheet
  for i:= 0 to High(fResultControls) do
  if (fResultControls[i] <> nil) and
    (DataSet = fResultControls[i]) then
  begin
    TabSheet:= fParentResultControls[i] as TTabSheet;
    Break;
  end;


  // Search for status bar inside current query result TabSheet
  if TabSheet <> nil then
  for i:= 0 to High(fResultControls) do
  if fResultControls[i] <> nil then
    if  (fParentResultControls[i] <> nil) and ((fParentResultControls[i] as TTabSheet) = TabSheet)
      and (fResultControls[i] is TStatusBar) then
    begin
      // Display current record and number of total records in status bar
      (fResultControls[i] as TStatusBar).SimpleText:= IntToStr(DataSet.RecordCount) +
      ' records fetched. At record # ' + IntToStr(DataSet.RecNo);
    break;
  end;

end;


{ Execute query according to passed query type }

procedure TfmQueryWindow.CallExecuteQuery(aQueryType: TQueryTypes);
begin
  // Get query text from memo
  if not(GetQuery(fQuery)) then
  begin
    ShowMessage('Could not get valid query');
    exit;
  end;
  fStartLine:= 0;

  // Disable buttons to prevent query interrupt
  tbRun.Enabled:= False;
  tbCommit.Enabled:= False;
  tbCommitRetaining.Enabled:= False;
  tbRollback.Enabled:= False;
  tbRollbackRetaining.Enabled:= False;

  fModifyCount:= 0;

  // Get initial query type; this can be changed later in the next parts
  if aQueryType = qtUnknown then // Auto
    fOrigQueryType:= GetQueryType(fQuery.Text)
  else
    fOrigQueryType:= aQueryType;

  // Call execute query for each part until finished
  fCnt:= 0;
  fFinished:= False;
  repeat
    ExecuteQuery;
  until fFinished;
  EnableButtons;
end;


{ sort auto completion options }

procedure TfmQueryWindow.SortSynCompletion;
var
  SortingList: TStringList;
  i: Integer;
begin
  SortingList:=TStringList.Create;
  try
    for i:=0 to SynCompletion1.ItemList.Count-1 do
       SortingList.Add(SynCompletion1.ItemList.Strings[i]);
    SortingList.Sort;
    SynCompletion1.ItemList.Clear;
    for i:=0 to SortingList.Count-1 do
      SynCompletion1.ItemList.Add(SortingList.Strings[i]);
  finally
    SortingList.Free;
  end;
end;


{ SQL thread termination }

procedure TfmQueryWindow.ThreadTerminated(Sender: TObject);
begin
  // Raise exception if an error occured during thread execution (Open)
  if fQT.Error then
  begin
    if Assigned(fTab) then
      fTab.TabVisible:= False;
    SetLength(fResultControls, High(fResultControls));
    SetLength(fParentResultControls, High(fParentResultControls));
    fTab:= CreateResultTab(qtExecute, fSqlQuery, fSqlScript, fmeResult);
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

{ Enable SQL buttons: Run, Commit, Rollbak after thread termination }

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

