unit EditDataFullRec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, IBConnection, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, DbCtrls, StdCtrls, main,
  Buttons;

type

  { TfmEditDataFullRec }

  TfmEditDataFullRec = class(TForm)
    bbSave: TBitBtn;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    laPos: TLabel;
    sqEditTable: TSQLQuery;
    procedure bbSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sqEditTableAfterScroll(DataSet: TDataSet);
  private
    FIBConnection: TIBConnection;
    FSQLTrans: TSQLTransaction;
    { private declarations }
  public
    procedure CurrentDateClick(Sender: TObject);
    procedure Init(dbIndex: Integer; ATableName: string);
    { public declarations }
  end; 

var
  fmEditDataFullRec: TfmEditDataFullRec;

implementation

uses Calen;

procedure TfmEditDataFullRec.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if bbSave.Visible then
    bbSaveClick(nil);
  sqEditTable.Close;
  CloseAction:= caFree;
end;

procedure TfmEditDataFullRec.bbSaveClick(Sender: TObject);
begin
  if sqEditTable.State in [dsInsert, dsEdit] then
    sqEditTable.Post;
  sqEditTable.ApplyUpdates;
  FSQLTrans.CommitRetaining;
end;

procedure TfmEditDataFullRec.sqEditTableAfterScroll(DataSet: TDataSet);
begin
  laPos.Caption:= IntToStr(sqEditTable.RecNo) + ' of ' + IntToStr(sqEditTable.RecordCount);
end;

procedure TfmEditDataFullRec.CurrentDateClick(Sender: TObject);
var
  FieldNum: Integer;
begin
  FieldNum:= (Sender as TBitBtn).Tag;
  fmCalen.DateTimeValue:= sqEditTable.Fields[FieldNum].AsDateTime;

  if fmCalen.ShowModal = mrOK then
  begin
    sqEditTable.Edit;
    sqEditTable.Fields[FieldNum].AsDateTime:= fmCalen.DateTimeValue;
  end;
end;

procedure TfmEditDataFullRec.Init(dbIndex: Integer; ATableName: string);
var
  ALabel: TLabel;
  ADBEdit: TDBEdit;
  i: Integer;
  AWidth: Integer;
  ADBMemo: TDBMemo;
  ATop: Integer;
  bbDate: TBitBtn;
begin
  FIBConnection:= fmMain.RegisteredDatabases[dbIndex].IBConnection;
  FIBConnection.Close;
  FSQLTrans:= fmMain.RegisteredDatabases[dbIndex].SQLTrans;
  sqEditTable.DataBase:= FIBConnection;

  sqEditTable.SQL.Text:= 'select * from ' +  ATableName;
  sqEditTable.Open;
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

  ATop:= 70;
  for i:= 0 to sqEditTable.Fields.Count - 1 do
  begin
    ALabel:= TLabel.Create(self);
    ALabel.Parent:= self;

    ALabel.Left:= 20;
    ALabel.Top:= ATop + 5;
    ALabel.Caption:= sqEditTable.Fields[i].FieldName;

    // Memo
    if sqEditTable.Fields[i].DataType = ftBlob then
    begin
      ADBMemo:= TDBMemo.Create(self);
      ADBMemo.Parent:= self;
      ADBMemo.Left:= 160;
      ADBMemo.Top:= ATop;
      ADBMemo.Width:= 400;
      ADBMemo.Height:= 200;
      ADBMemo.Anchors:= [akLeft, akTop, akRight];
      ADBMemo.ScrollBars:= ssBoth;
      ADBMemo.DataSource:= Datasource1;
      ADBMemo.DataField:= sqEditTable.Fields[i].FieldName;
    end
    else   // Edit control
    begin
      ADBEdit:= TDBEdit.Create(self);
      ADBEdit.Parent:= self;

      ADBEdit.Left:= 160;
      ADBEdit.Top:= ATop;
      ADBEdit.DataSource:= Datasource1;
      ADBEdit.DataField:= sqEditTable.Fields[i].FieldName;
      AWidth:= 80;
      if sqEditTable.Fields[i].DataType = ftString then
        AWidth:= sqEditTable.Fields[i].DataSize * 10
      else
      if sqEditTable.Fields[i].DataType = ftDateTime then
        AWidth:= 140;
      if AWidth > 400 then
        AWidth:= 400;
      ADBEdit.Width:= AWidth;
    end;

    // DateTime field
    if sqEditTable.Fields[i].DataType in [ftDate, ftDateTime, ftTime] then
    begin
      bbDate:= TBitBtn.Create(self);
      bbDate.Parent:= self;
      bbDate.Left:= 180 + AWidth;
      bbDate.Top:= ATop;
      bbDate.Caption:= 'Calen.';
      bbDate.Width:= 60;
      bbDate.Hint:= 'Date/Time Selector';
      bbDate.ShowHint:= True;
      bbDate.Tag:= i;
      bbDate.OnClick:= @CurrentDateClick;
    end;

    if sqEditTable.Fields[i].DataType = ftBlob then
      Inc(ATop, 200)
    else
      Inc(ATop, 30);
  end;
  Height:= ATop + 10;
end;

initialization
  {$I editdatafullrec.lrs}

end.

