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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
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

procedure TfmEditTable.Panel1Click(Sender: TObject);
begin
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

procedure TfmEditTable.sqEditTableAfterScroll(DataSet: TDataSet);
begin
  laPos.Caption:= IntToStr(sqEditTable.RecNo) + ' of ' + IntToStr(sqEditTable.RecordCount);
end;

procedure TfmEditTable.Init(dbIndex: Integer; ATableName: string);
begin
  sqEditTable.Close;
  if ibConnection = nil then
  begin
    ibConnection:= Rec.IBConnection;
    ibConnection.Close;
    sqlTrans:= Rec.SQLTrans;
    sqEditTable.DataBase:= ibConnection;
  end;

  bbSave.Visible:= fmMain.ChangeQueryToBIDirectional(dbIndex, ATableName, sqEditTable);
  if not bbSave.Visible then
    ShowMessage('Primary key is not found on this table. It can not be edited');

  sqEditTable.Close;
  sqEditTable.SQL.Text:= 'select * from ' + ATableName;
  sqEditTable.Open;

end;

initialization
  {$I edittable.lrs}

end.

