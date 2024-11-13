unit NewConstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, CheckLst, QueryWindow;

type

  { TfmNewConstraint }

  TfmNewConstraint = class(TForm)
    bbScript: TBitBtn;
    cbUpdateAction: TComboBox;
    cbTables: TComboBox;
    clxForFields: TCheckListBox;
    clxOnFields: TCheckListBox;
    edNewName: TEdit;
    cbDeleteAction: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    laTable: TLabel;
    procedure bbScriptClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
  private
    { private declarations }
  public
    DatabaseIndex: Integer;
    QWindow: TfmQueryWindow;
    { public declarations }
  end; 

var
  fmNewConstraint: TfmNewConstraint;

implementation

uses main;

{ TfmNewConstraint }

procedure TfmNewConstraint.cbTablesChange(Sender: TObject);
var
  FieldsList: TStringList;
begin
  // Get foreign table fields
  FieldsList:= TStringList.Create;
  try
    fmMain.GetFields(DatabaseIndex, cbTables.Text, FieldsList);
    clxForFields.Clear;
    clxForFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
  end;
  fmMain.SQLQuery1.Close;
end;

procedure TfmNewConstraint.bbScriptClick(Sender: TObject);
var
  CurrFields, ForFields: string;
  i: Integer;
begin
  CurrFields:= '';
  for i:= 0 to clxOnFields.Count - 1 do
    if clxOnFields.Checked[i] then
      CurrFields:= CurrFields + clxOnFields.Items[i] + ', ';
  if CurrFields <> '' then
    Delete(CurrFields, Length(CurrFields) - 1, 2);

  ForFields:= '';
  for i:= 0 to clxForFields.Count - 1 do
    if clxForFields.Checked[i] then
      ForFields:= ForFields + clxForFields.Items[i] + ', ';
  if ForFields <> '' then
    Delete(ForFields, Length(ForFields) - 1, 2);

  QWindow:= fmMain.ShowQueryWindow(DatabaseIndex, 'new constraint on table : ' + laTable.Caption);
  QWindow.meQuery.Lines.Text:= 'alter table ' + laTable.Caption + ' ADD CONSTRAINT ' + edNewName.Text;
  QWindow.meQuery.Lines.Add(' foreign key (' + CurrFields + ') ');
  QWindow.meQuery.Lines.Add(' references ' + cbTables.Text + ' (' + ForFields + ') ');
  if cbUpdateAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' on update ' + cbUpdateAction.Text + ' ');
  if cbDeleteAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' on delete ' + cbDeleteAction.Text + ' ');

  fmMain.Show;
end;

initialization
  {$I newconstraint.lrs}

end.

