unit NewEditField;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, turbocommon;

type
  TFormMode = (foNew, foEdit);

  { TfmNewEditField }

  TfmNewEditField = class(TForm)
    bbAdd: TBitBtn;
    cbCharset: TComboBox;
    cbCollation: TComboBox;
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
    Label7: TLabel;
    lblCharset: TLabel;
    lblCollation: TLabel;
    seSize: TSpinEdit;
    seOrder: TSpinEdit;
    seScale: TSpinEdit;
    procedure bbAddClick(Sender: TObject);
    procedure cbCharsetEditingDone(Sender: TObject);
    procedure cbTypeEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FDBIndex: Integer;
    FTableName: string;
    FRefreshButton: TBitBtn;
  public
    fFormMode: TFormMode;
    OldFieldName: string;
    OldFieldType: string;
    OldFieldSize: integer;
    OldFieldScale: integer;
    OldAllowNull: Boolean;
    OldOrder: integer;
    OldDefault: string;
    OldCharacterSet: string;
    OldCollation: string;
    OldDescription: string;
    procedure Init(dbIndex: Integer; TableName: string;
      FormMode: TFormMode;
      FieldName, FieldType,
      CharacterSet, Collation,
      DefaultValue, Description: string;
      Size, Scale, Order: Integer;
      AllowNull: Boolean;
      RefreshButton: TBitBtn);
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
    if (Line='CHAR') or
      (Line='CSTRING') or
      (Line='VARCHAR') then
      Line:= Line + '(' + IntToStr(seSize.Value) + ')';

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      if ((cbType.Text='CHAR') or
        (cbType.Text='CSTRING') or
        (cbType.Text='VARCHAR')) and
        (Pos('''', edDefault.Text) = 0) then
        Line:= Line + ' default ' + QuotedStr(edDefault.Text)
      else
        Line:= Line + ' default ' + edDefault.Text;
    end;

    // Null/Not null
    if not cxAllowNull.Checked then
      Line:= Line + ' not null';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field to Table: ' + FTableName,
      'ALTER TABLE ' + FTableName +
      ' ADD ' + edFieldName.Text + ' ' + Line,
      Clk);
  end
  else  // Update
  begin
    Line:= '';
    // Check name change
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line:= 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' +
      edFieldName.Text + ';' + LineEnding;

    // Check type/size/scale change
    if (cbType.Text <> OldFieldType) or
      (seSize.Value <> OldFieldSize) or
      (seScale.Value <> OldFieldScale) then
    begin
      Line:= Line + 'ALTER TABLE ' + FTableName +
        ' ALTER ' + UpperCase(Trim(edFieldName.Text)) +
        ' TYPE ' + cbType.Text;

      if (cbType.Text='NUMERIC') or
       (cbType.Text='DECIMAL') then
        Line:= Line + '(' + IntToStr(seSize.Value) + ',' +
          IntToStr(seScale.Value)+');' + LineEnding
      else
      if (cbType.Text='CHAR') or
        (cbType.Text='CSTRING') or
        (cbType.Text='VARCHAR') then
        Line:= Line + '(' + IntToStr(seSize.Value) + ');' + LineEnding;
    end;

    //Character set and collation
    Line:= Line + '-- warning: character set changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;
    Line:= Line + '-- warning: collation changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;

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
          'WHERE RDB$FIELD_NAME = ' + QuotedStr(UpperCase(Trim(edFieldName.Text))) + ' ' +
          'AND RDB$RELATION_NAME = ' + QuotedStr(FTableName) + LineEnding;
    end;

    // Description
    if edDescription.Text <> OldDescription then
    begin
      Line:= Line + 'UPDATE RDB$RELATION_FIELDS ' +
        'set RDB$DESCRIPTION = ' + QuotedStr(edDescription.Text) + ' ' +
        'where RDB$FIELD_NAME = ' + QuotedStr(UpperCase(Trim(edFieldName.Text))) + ' ' +
        'and RDB$RELATION_NAME = ' + QuotedStr(FTableName) + ';' + LineEnding;
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

procedure TfmNewEditField.cbCharsetEditingDone(Sender: TObject);
var
  Collations: TStringList;
begin
  // Available collations depend on the chosen character set,
  // so update that whenever user changes character set
  Collations:= TStringList.Create;
  try
    GetCollations(cbCharSet.Text,Collations);
    cbCollation.Items.Assign(Collations);
  finally
    Collations.Free;
  end;
end;



procedure TfmNewEditField.cbTypeEditingDone(Sender: TObject);
begin
  seSize.Value:= dmSysTables.GetDefaultTypeSize(FDBIndex, cbType.Text);

  {todo: (low priority) allow/disallow gui elements when using domain datatypes.
   Check what can be overridden (e.g. collate for text-type domain fields)}
  // Allow character set, lblCollation for text type fields; otherwise disable
  case cbType.Text of
    'CHAR','CSTRING','VARCHAR':
    begin
      // Allow character set/lblCollation for text type fields
      cbCharset.Enabled:= true;
      cbCollation.Enabled:= true;
      seScale.Enabled:= false;
    end;
    'DECIMAL','NUMERIC':
    begin
      // Allow scale for numeric, decimal
      seScale.Enabled:= true;
      cbCharset.Enabled:= false;
      cbCollation.Enabled:= false;
    end
    else
    begin
      cbCharset.Enabled:= false;
      cbCollation.Enabled:= false;
      seScale.Enabled:= false;
    end;
  end;
end;

procedure TfmNewEditField.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmNewEditField.FormCreate(Sender: TObject);
begin
  // Load available character sets
  // todo: (low priority) character sets should be retrieved from current database server
  CbCharSet.Items.AddStrings(FBCharacterSets);
  // Do not set a default value, but leave it empty, because specifying a charset
  // is not mandatory - it should only be done if the charset differs from the
  // db charset
end;

procedure TfmNewEditField.Init(dbIndex: Integer; TableName: string;
  FormMode: TFormMode;
  FieldName, FieldType,
  CharacterSet, Collation,
  DefaultValue, Description: string;
  Size, Scale, Order: integer;
  AllowNull: Boolean;
  RefreshButton: TBitBtn);
begin
  cbType.Clear;

  // Load basic datatypes for fields into combobox....
  dmSysTables.GetBasicTypes(cbType.Items);
  // ... add domain types for fields
  dmSysTables.GetDomainTypes(dbIndex, cbType.Items);

  FDBIndex:= dbIndex;
  FTableName:= TableName;
  fFormMode:= FormMode;
  FRefreshButton:= RefreshButton;

  OldFieldName:= FieldName;
  OldFieldSize:= Size;
  OldFieldScale:= Scale;
  OldFieldType:= FieldType;
  OldAllowNull:= AllowNull;
  OldOrder:= Order;
  OldDefault:= DefaultValue;
  OldCharacterSet:= CharacterSet;
  OldCollation:= Collation;
  OldDescription:= Description;

  edFieldName.Text:= OldFieldName;
  seSize.Value:= OldFieldSize;
  cbType.Text:= OldFieldType;
  cxAllowNull.Checked:= OldAllowNull;
  seOrder.Value:= OldOrder;
  edDefault.Text:= OldDefault;
  edDescription.Text:= OldDescription;
  if FormMode = foEdit then
  begin
    bbAdd.Caption:= 'Update';
    Caption:= 'Edit field: ' + FieldName + ' on : ' + TableName;
  end
  else
  begin
    bbAdd.Caption:= 'Add';
    Caption:= 'Add new field in : ' + TableName;
  end;
end;

initialization
  {$I neweditfield.lrs}

end.

