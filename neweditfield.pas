unit NewEditField;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons;

type
  TFormMode = (foNew, foEdit);

  { TfmNewEditField }

  TfmNewEditField = class(TForm)
    bbAdd: TBitBtn;
    cbType: TComboBox;
    cxAllowNull: TCheckBox;
    edDescription: TEdit;
    edFieldName: TEdit;
    edDefault: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    seSize: TSpinEdit;
    seOrder: TSpinEdit;
    procedure bbAddClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FDBIndex: Integer;
    FTableName: string;
    FRefreshButton: TBitBtn;
  public
    fFormMode: TFormMode;
    OldFieldName: string;
    OldFieldType: string;
    OldFieldSize: Integer;
    OldAllowNull: Boolean;
    OldOrder: Integer;
    OldDefault: string;
    OldDesciption: string;
    procedure Init(dbIndex: Integer; TableName: string; FormMode: TFormMode;
      FieldName, FieldType, DefaultValue, Description: string; FSize, FOrder: Integer; AllowNull: Boolean; RefreshButton: TBitBtn);

    { public declarations }
  end; 

var
  fmNewEditField: TfmNewEditField;

implementation

{ TfmNewEditField }

uses Main, SysTables;

procedure TfmNewEditField.bbAddClick(Sender: TObject);
var
  Line: string;
  Nullflag: string;
  Clk: TNotifyEvent;
begin
  if FRefreshButton = nil then
   clk:= nil
  else
    clk:= FRefreshButton.OnClick;

  if fFormMode = foNew then  // New field
  begin
    Line:= cbType.Text;
    if Pos('CHAR', Line) > 0 then
      Line:= Line + '(' + IntToStr(seSize.Value) + ')';

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      if (Pos('CHAR', cbType.Text) > 0) and (Pos('''', edDefault.Text) = 0) then
        Line:= Line + ' default ''' + edDefault.Text + ''''
      else
        Line:= Line + ' default ' + edDefault.Text;
    end;

    // Null/Not null
    if not cxAllowNull.Checked then
      Line:= Line + ' not null';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field on Table: ' + FTableName,
      'ALTER TABLE ' + FTableName + ' ADD ' + edFieldName.Text + ' ' + Line, Clk);
  end
  else  // Upate
  begin
    Line:= '';
    // Check name change
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line:= 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' +
      edFieldName.Text + ';' + LineEnding;

    // check type/size change
    if (cbType.Text <> OldFieldType) or (seSize.Value <> OldFieldSize) then
    begin
      Line:= Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + UpperCase(Trim(edFieldName.Text))
        + ' TYPE ' + cbType.Text;

      if Pos('CHAR', Line) > 0 then
        Line:= Line + '(' + IntToStr(seSize.Value) + ');' + LineEnding;
    end;

    // Field Order
    if seOrder.Value <> OldOrder then
    begin
      Line:= Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' POSITION ' +
        IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // Allow Null
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        NullFlag:= 'NULL'
      else
        NullFlag:= '1';
        Line:= Line + 'UPDATE RDB$RELATION_FIELDS SET RDB$NULL_FLAG = ' + NullFlag + LineEnding +
          'WHERE RDB$FIELD_NAME = ''' + UpperCase(Trim(edFieldName.Text)) + ''' AND RDB$RELATION_NAME = ''' +
          FTableName + '''' + LineEnding;
    end;

    // Description
    if edDescription.Text <> OldDesciption then
    begin
      Line:= Line + 'UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = ''' + edDescription.Text +
        '''  where RDB$FIELD_NAME = ''' + UpperCase(Trim(edFieldName.Text)) +
        ''' and RDB$RELATION_NAME = ''' + FTableName + ''';' + LineEnding;
    end;

    // Default value
    if edDefault.Text <> OldDefault then
    begin
      Line:= Line + 'UPDATE RDB$RELATION_FIELDS set RDB$Default_Source = ''' + edDefault.Text +
        '''  where RDB$FIELD_NAME = ''' + UpperCase(Trim(edFieldName.Text)) +
        ''' and RDB$RELATION_NAME = ''' + FTableName + ''';' + LineEnding;
    end;

    if Line <> '' then
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit field: ' + OldFieldName, Line, clk);
  end;
  Close;
end;

procedure TfmNewEditField.cbTypeChange(Sender: TObject);
begin
  seSize.Value:= dmSysTables.GetDefaultTypeSize(FDBIndex, cbType.Text);
end;

procedure TfmNewEditField.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmNewEditField.Init(dbIndex: Integer; TableName: string; FormMode: TFormMode;
  FieldName, FieldType, DefaultValue, Description: string; FSize, FOrder: Integer; AllowNull: Boolean;
  RefreshButton: TBitBtn);
begin
  cbType.Clear;
  // Add Basic types
  dmSysTables.GetBasicTypes(cbType.Items);

  // Add Domain types
  dmSysTables.GetDomainTypes(dbIndex, cbType.Items);

  FDBIndex:= dbIndex;
  FTableName:= TableName;
  fFormMode:= FormMode;
  FRefreshButton:= RefreshButton;

  OldFieldName:= FieldName;
  OldFieldSize:= FSize;
  OldFieldType:= FieldType;
  OldAllowNull:= AllowNull;
  OldOrder:= FOrder;
  OldDefault:= DefaultValue;
  OldDesciption:= Description;

  edFieldName.Text:= OldFieldName;
  seSize.Value:= OldFieldSize;
  cbType.Text:= OldFieldType;
  cxAllowNull.Checked:= OldAllowNull;
  seOrder.Value:= OldOrder;
  edDefault.Text:= OldDefault;
  edDescription.Text:= OldDesciption;
  if FormMode = foEdit then
  begin
    bbAdd.Caption:= 'Update';
    Caption:= 'Edit field: ' + FieldName + ' on : ' + TableName;
  end
  else
  begin
    bbAdd.Caption:= 'Add';
    Caption:= 'Add new in : ' + TableName;
  end;

end;

initialization
  {$I neweditfield.lrs}

end.

