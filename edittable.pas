unit EditTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, IBConnection, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, DbCtrls, DBGrids, StdCtrls, ComCtrls,
  Buttons, main;

type

  { TfmEditTable }

  TfmEditTable = class(TForm)
    bbSave: TBitBtn;
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    laPos: TLabel;
    Panel1: TPanel;
    sqEditTable: TSQLQuery;
    procedure bbSaveClick(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure sqEditTableAfterScroll(DataSet: TDataSet);
  private
    ibConnection: TIBConnection;
    sqlTrans: TSQLTransaction;
    { private declarations }
  public
    { public declarations }
    Rec: TDatabaseRec;
    procedure Init(dbIndex: Integer; ATableName: string);
  end; 

var
  fmEditTable: TfmEditTable;

implementation

{ TfmEditTable }

procedure TfmEditTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  sqEditTable.Close;
  CloseAction:= caFree;
  ibConnection:= nil;
  sqlTrans:= nil;
end;

procedure TfmEditTable.FormCreate(Sender: TObject);
begin
  ibConnection:= nil;
  sqlTrans:= nil;
end;

procedure TfmEditTable.bbSaveClick(Sender: TObject);
begin
  try
    if sqEditTable.State in [dsInsert, dsEdit] then
      sqEditTable.Post;
    if sqEditTable.Active then
      sqEditTable.ApplyUpdates;
    if SQLTrans.Active then
      SQLTrans.CommitRetaining;
  except
    on e: exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TfmEditTable.DBGrid1TitleClick(Column: TColumn);
begin
  //todo: implement sorting a la
  //http://wiki.lazarus.freepascal.org/Grids_Reference_Page#Sorting_columns_or_rows_in_DBGrid_with_sort_arrows_in_column_header
{ if sqEditTable.IndexFieldNames = Column.Field.FieldName then
   sqEditTable.IndexFieldNames := Column.Field.FieldName //+ 'DESC'
 else
   sqEditTable.IndexFieldNames := Column.Field.FieldName}
end;

procedure TfmEditTable.sqEditTableAfterScroll(DataSet: TDataSet);
begin
  laPos.Caption:= IntToStr(sqEditTable.RecNo) + ' of ' + IntToStr(sqEditTable.RecordCount);
end;

procedure TfmEditTable.Init(dbIndex: Integer; ATableName: string);
var
  FieldsList: TStringList;
  i: integer;
  PKField: TField;
begin
  sqEditTable.Close;
  if ibConnection = nil then
  begin
    ibConnection:= Rec.IBConnection;
    if not(ibConnection.Connected) then
      ibConnection.Open;
    sqlTrans:= Rec.SQLTrans;
    sqEditTable.DataBase:= ibConnection;
  end;
  //todo: deal with quoted identifiers in ATableName here and elsewhere
  sqEditTable.SQL.Text:= 'select * from ' + ATableName;
  sqEditTable.Open; // need to have open query in order to access fields below

  bbSave.Visible:= true;
  {
  // ASSUME there's a generator/trigger
  //todo: verify this assumption using code to check this out. Then also modify
  //insert statement to leave out the relevant fields if not present
  FieldsList:= TStringList.Create;
  try
    if fmmain.GetPrimaryKeyFields(dbIndex, ATableName, FieldsList) then
    begin
      bbSave.Visible:= true;
      for i:= 0 to FieldsList.Count -1 do
      begin
        try
          sqEditTable.FieldByName(FieldsList[i]).Required:=false;
        except
          // field does not exist => error
          bbSave.Visible:=false;
          break;
        end;
      end;
    end
    else
    begin
      bbSave.Visible:= false;
    end;
  finally
    FieldsList.Free;
  end;
  }
  if not(bbSave.Visible) then
    ShowMessage('Primary key is not found for this table. It can not be edited.');
end;

initialization
  {$I edittable.lrs}

end.

