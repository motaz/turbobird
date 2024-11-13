unit SQLHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, DBGrids, Buttons, StdCtrls, EditBtn;

type

  { TfmSQLHistory }

  TfmSQLHistory = class(TForm)
    bbInsert: TBitBtn;
    bbDelete: TBitBtn;
    bbExport: TBitBtn;
    cbSQLType: TComboBox;
    cxOverwrite: TCheckBox;
    cxAfterDate: TCheckBox;
    Datasource1: TDatasource;
    DateEdit1: TDateEdit;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    procedure bbDeleteClick(Sender: TObject);
    procedure bbExportClick(Sender: TObject);
    procedure bbInsertClick(Sender: TObject);
    procedure cxAfterDateClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FQueryForm: TForm;
    { private declarations }
  public
    { public declarations }
    procedure Init(DatabaseTitle: string; QueryForm: TForm);
  end; 

var
  fmSQLHistory: TfmSQLHistory;

implementation

{ TfmSQLHistory }

uses Main, QueryWindow;

procedure TfmSQLHistory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Datasource1.DataSet:= nil;
end;

procedure TfmSQLHistory.FormCreate(Sender: TObject);
begin
  DateEdit1.Date:= Now - 7;
end;

procedure TfmSQLHistory.bbInsertClick(Sender: TObject);
var
  SQLStatement: string;
  i: Integer;
  aStatement: string;
begin
//  SQLStatement:= (fmMain.mdsHistory.FieldByName('SQLStatement').AsString);
  for i:=0 to DBGrid1.SelectedRows.Count - 1 do
  begin
    Datasource1.DataSet.GotoBookmark(DBGrid1.SelectedRows.Items[i]);
    aStatement := fmMain.mdsHistory.FieldByName('SQLStatement').AsString;
    if Pos(';', aStatement) = 0 then
      aStatement:= aStatement + ';';
    SQLStatement += aStatement;

  end;

  if cxOverwrite.Checked then
    (FQueryForm as TfmQueryWindow).meQuery.Lines.Clear;

  (FQueryForm as TfmQueryWindow).meQuery.Lines.Text:= (FQueryForm as TfmQueryWindow).meQuery.Lines.Text + SQLStatement;
  Close;
end;

procedure TfmSQLHistory.cxAfterDateClick(Sender: TObject);
begin
  DateEdit1.Visible:= cxAfterDate.Checked;
end;

procedure TfmSQLHistory.DBGrid1DblClick(Sender: TObject);
begin
  bbInsertClick(nil);
end;

procedure TfmSQLHistory.bbDeleteClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete current record of history', mtConfirmation, [mbYes, mbNo], 0) = mrYes
    then
      fmMain.mdsHistory.Delete;
end;

procedure TfmSQLHistory.bbExportClick(Sender: TObject);
var
  CurrType: string;
  List: TStringList;
  Line: string;
begin
  if SaveDialog1.Execute then
  with fmMain.mdsHistory do
  begin
    DBGrid1.Visible:= False;
    First;
    List:= TStringList.Create;
    try
      while not Eof do
      begin
        if (not cxAfterDate.Checked) or (FieldByName('Time').AsDateTime > DateEdit1.Date) then
        begin
          CurrType:= FieldByName('SQLType').AsString;
          if (cbSQLType.ItemIndex = 0) or
            ((CurrType = 'DDL') and (cbSQLType.ItemIndex in [1, 2])) or
            ((CurrType = 'DML') and (cbSQLType.ItemIndex in [1, 3])) or
            ((CurrType = 'SELECT') and (cbSQLType.ItemIndex = 4)) then
          begin
            List.Add('-- ' + FieldByName('Time').AsString);
            Line:= FieldByName('SQLStatement').AsString;
            if Pos(';', Line) = 0 then
              Line:= Line + ';';
            List.Add(Line);
          end;
        end;
        Next;
      end;
      List.SaveToFile(SaveDialog1.FileName);
    finally
      List.Free;
    end;
    DBGrid1.Visible:= True;
  end;
end;

procedure TfmSQLHistory.Init(DatabaseTitle: string; QueryForm: TForm);
begin
  FQueryForm:= QueryForm;
  Caption:= 'SQL History for: ' + DatabaseTitle;
  Datasource1.DataSet:= fmMain.mdsHistory;
  fmMain.mdsHistory.Last;
end;

initialization
  {$I sqlhistory.lrs}

end.
