unit NewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, turbocommon;

type

  { TfmNewGen }

  TfmNewGen = class(TForm)
    bbCreateGen: TBitBtn;
    BitBtn1: TBitBtn;
    cbTables: TComboBox;
    cbFields: TComboBox;
    cxTrigger: TCheckBox;
    edGenName: TEdit;
    gbTrigger: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure bbCreateGenClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure cxTriggerChange(Sender: TObject);
  private
    { private declarations }
    FDBIndex: Integer;
    FIBConnection: TIBConnection;
    FSQLTrans: TSQLTransaction;
  public
    procedure Init(dbIndex: Integer);
    { public declarations }
  end; 

var
  fmNewGen: TfmNewGen;

implementation

{ TfmNewGen }

uses main, SysTables;

procedure TfmNewGen.bbCreateGenClick(Sender: TObject);
var
  List: TStringList;
  Valid: Boolean;
begin
  if Trim(edGenName.Text) <> '' then
  begin
    Valid:= True;
    List:= TStringList.Create;
    try
      List.Add('create generator ' + edGenName.Text + ';');
      if cxTrigger.Checked then
      begin
        Valid:= False;
        if (cbTables.ItemIndex = -1) or (cbFields.ItemIndex = -1) then
          MessageDlg('You should select a table and a field', mtError, [mbOk], 0)
        else
        if Trim(edGenName.Text) = '' then
          MessageDlg('You should enter generator name', mtError, [mbOK], 0)
        else
        begin
          List.Add('CREATE TRIGGER ' + Trim(edGenName.Text) + ' FOR ' + cbTables.Text);
          List.Add('ACTIVE BEFORE INSERT POSITION 0 ');
          List.Add('AS BEGIN ');
          List.Add('IF (NEW.' + cbFields.Text + ' IS NULL OR NEW.' + cbFields.Text + ' = 0) THEN ');
          List.Add('  NEW.' + cbFields.Text + ' = GEN_ID(' + edGenName.Text + ', 1);');
          List.Add('END;');
          Valid:= True;
        end;

      end;
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Create Generator: ' + edGenName.Text, List.Text);
      Close;
    finally
      List.Free;
    end;
  end
  else
    MessageDlg('You should write Generator name', mtError, [mbOK], 0);
end;

procedure TfmNewGen.cbTablesChange(Sender: TObject);
var
  FType: string;
begin
  if cbTables.ItemIndex <> -1 then
  begin
    fmMain.GetFields(FDBIndex, cbTables.Text, nil);
    cbFields.Clear;
    while not fmMain.SQLQuery1.EOF do
    begin
      FType:= GetFBTypeName(fmMain.SQLQuery1.FieldByName('field_type_int').AsInteger,
        fmMain.SQLQuery1.FieldByName('field_sub_type').AsInteger,
        fmMain.SQLQuery1.FieldByName('field_length').AsInteger,
        fmMain.SQLQuery1.FieldByName('field_precision').AsInteger,
        fmMain.SQLQuery1.FieldByName('field_scale').AsInteger);

      // Only show field name if they are numeric/suitable for generators
      // In practice, integer type fields are probably always used
      if (FType = 'INTEGER') or (FType = 'BIGINT') or (FType = 'SMALLINT') then
        cbFields.Items.Add(Trim(fmMain.SQLQuery1.FieldByName('Field_Name').AsString));
      fmMain.SQLQuery1.Next;
    end;
    fmMain.SQLQuery1.Close;
  end;
end;

procedure TfmNewGen.cxTriggerChange(Sender: TObject);
begin
  gbTrigger.Enabled:= cxTrigger.Checked;
end;

procedure TfmNewGen.Init(dbIndex: Integer);
var
  TableNames: string;
  Count: Integer;
begin
  FDBIndex:= dbIndex;
  TableNames:= dmSysTables.GetDBObjectNames(dbIndex, otTables, Count);

  fmNewGen.cbTables.Items.CommaText:= TableNames;

  cxTrigger.Checked:= False;
end;

initialization
  {$I newgen.lrs}

end.

