unit NewTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Grids, Buttons, ExtCtrls, SynEdit, SynCompletion,
  SynHighlighterSQL, LCLType;

type

  { TfmNewTable }

  TfmNewTable = class(TForm)
      bbClose: TSpeedButton;
    bbScript: TBitBtn;
    bbCancel: TBitBtn;
    cbPermission: TComboBox;
    cbRolesUsers: TComboBox;
    cxGrantPermission: TCheckBox;
    cxCreateGen: TCheckBox;
    edNewTable: TEdit;
    Image1: TImage;
    Label1: TLabel;
    laPermission: TLabel;
    StringGrid1: TStringGrid;
    procedure bbCloseClick(Sender: TObject);
    procedure bbScriptClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure cxGrantPermissionChange(Sender: TObject);
    procedure edNewTableKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GenerateCreateSQL(var KeyField, GeneratorName: string): string;
    procedure Init(dbIndex: Integer);
    procedure StringGrid1PickListSelect(Sender: TObject);
  private
    FDBIndex: Integer;
    function Validate: Boolean;
    // Get field count using stringgrid count
    function GetFieldsCount: Integer;
    function GetClosestType(ATypePart: string): string;
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmNewTable: TfmNewTable;

implementation

{ TfmNewTable }

uses SysTables, Main;


function TfmNewTable.GenerateCreateSQL(var KeyField, GeneratorName: string): string;
var
  i: Integer;
  FieldLine: string;
  FieldType: string;
  PKey: string;
begin
  Result:= 'create table ' + edNewTable.Text + ' (' + LineEnding;
  for i:= 1 to StringGrid1.RowCount - 1 do
    if Trim(StringGrid1.Cells[0, i]) <> '' then
    begin
      FieldLine:= StringGrid1.Cells[0, i]; // Field Name
      FieldType:= StringGrid1.Cells[1, i];
      FieldLine:= FieldLine + ' ' + FieldType;

      // Char size
      if (LowerCase(FieldType) = 'varchar') or (LowerCase(FieldType) = 'char') then
      begin
        FieldLine:= FieldLine + '(' + StringGrid1.Cells[2, i] + ')';
      end;

      // Allow Null
      if StringGrid1.Cells[3, i] = '0' then
        FieldLine:= FieldLine + ' not null ';

      // Primary Key
      if StringGrid1.Cells[4, i] = '1' then
      begin
        PKey:= PKey + StringGrid1.Cells[0, i] + ',';
        GeneratorName:= Trim(edNewTable.Text) + '_' + StringGrid1.Cells[0, i] + '_Gen';
        KeyField:= StringGrid1.Cells[0, i]; // Generator should work if there is only one Key field
      end;
      // Default value
      if Trim(StringGrid1.Cells[5, i]) <> '' then
      begin
        if ((Pos('CHAR', FieldType) > 0) or (FieldType='CSTRING')) and
          (Pos('''', StringGrid1.Cells[5, i]) = 0) then
          FieldLine:= FieldLine + ' default ' + QuotedStr(StringGrid1.Cells[5, i])
        else
          FieldLine:= FieldLine + ' default ' + StringGrid1.Cells[5, i];
      end;

      if (StringGrid1.RowCount > i + 1) and (Trim(stringGrid1.Cells[0, i + 1]) <> '') then
        FieldLine:= FieldLine + ',' + LineEnding;
      Result:= Result + FieldLine;
    end;

  // Set Primary key
  if PKey <> '' then
  begin
    Delete(PKey, Length(PKey), 1);
    Result:= Result + ', ' + LineEnding + ' constraint ' + edNewTable.Text + '_pk_1 primary key (' + PKey + ') ' + LineEnding;
  end;
  Result:= Result + ');' + LineEnding;

  // Permission
  if cxGrantPermission.Checked and (Trim(cbRolesUsers.Text) <> '') then
  begin
    case cbPermission.ItemIndex of
      0: Result:= Result + 'grant All on ' + edNewTable.Text + ' to ' + cbRolesUsers.Text + ';';
      1: Result:= Result + 'grant Select, insert, update, references on ' + edNewTable.Text + ' to ' + cbRolesUsers.Text + ';';
      2: Result:= Result + 'grant Select, references on ' + edNewTable.Text + ' to ' + cbRolesUsers.Text + ';';
    end;

  end;
end;

procedure TfmNewTable.Init(dbIndex: Integer);
var
  i: Integer;
begin
  FDBIndex:= dbIndex;
  edNewTable.Clear;
  cxCreateGen.Checked:= False;
  StringGrid1.RowCount:= 3;

  StringGrid1.Columns[1].PickList.Clear;
  // Add Basic types
  dmSysTables.GetBasicTypes(StringGrid1.Columns[1].PickList);

  // Add Domain types
  dmSysTables.GetDomainTypes(dbIndex, StringGrid1.Columns[1].PickList);
  for i:= 1 to StringGrid1.RowCount - 1 do
  begin
    StringGrid1.Cells[0, i]:= '';
    StringGrid1.Cells[1, i]:= '';
    StringGrid1.Cells[2, i]:= '';
    StringGrid1.Cells[3, i]:= '1';
    StringGrid1.Cells[4, i]:= '0';
  end;

  cbRolesUsers.Items.CommaText:= StringReplace(dmSysTables.GetDBUsers(FDBIndex), '<R>', '', [rfReplaceAll]);
end;

procedure TfmNewTable.StringGrid1PickListSelect(Sender: TObject);
var
  SelType: string;
begin
  with StringGrid1 do
  if (Col = 1) then
  begin
    SelType:= Cells[1, Row];
    SelType:= GetClosestType(SelType);
    if SelType <> '' then
    begin
      Cells[1, Row]:= SelType;
      Cells[2, Row]:= IntToStr(dmSysTables.GetDefaultTypeSize(FDBIndex, SelType));
    end;
  end;
end;

function TfmNewTable.Validate: Boolean;
var
  i: Integer;
  PKeyFound: Boolean;
begin
  Result:= False;
  if Trim(edNewTable.Text) = '' then
    MessageDlg('Warning', 'You should enter new table name', mtWarning, [mbOk], 0)
  else
  if GetFieldsCount = 0 then
    MessageDlg('Warning', 'You should enter fields', mtWarning, [mbOk], 0)
  else
  begin
    Result:= True;
    with StringGrid1 do
    for i:= 1 to RowCount - 1 do
    if (Trim(Cells[0, i]) <> '') and (Trim(Cells[1, i]) = '') then
    begin
      Result:= False;
      MessageDlg('Warning', 'You should write field type for the column: ' + Cells[0, i], mtWarning, [mbOk], 0);
    end
    else
    if (Trim(Cells[0, i]) = '') and (Trim(Cells[1, i]) <> '') then
    begin
      Result:= False;
      MessageDlg('Warning', 'You should select field name for the column number ' + IntToStr(i), mtWarning, [mbOk], 0);
    end;

    // Check Primary key
    if cxCreateGen.Checked then
    begin
      PKeyFound:= False;
      with StringGrid1 do
      for i:= 1 to RowCount - 1 do
      if Cells[4, i] = '1' then
      begin
        PKeyFound:= True;
        Break;
      end;

      if not PKeyFound then
      begin
        Result:= False;
        MessageDlg('Warning', 'There is no primary key', mtWarning, [mbOk], 0);
      end;
    end;
  end;
end;

function TfmNewTable.GetFieldsCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  with StringGrid1 do
  begin
    for i:= 1 to RowCount - 1 do
    begin
      if Trim(Cells[0, i]) <> '' then
        Inc(Result);
    end;
  end;
end;

function TfmNewTable.GetClosestType(ATypePart: string): string;
var
  i: Integer;
begin
  Result:= '';
  ATypePart:= UpperCase(ATypePart);
  with StringGrid1 do
  if Columns[1].PickList.IndexOf(ATypePart) <> -1 then
    Result:= ATypePart
  else
  begin
    for i:= 0 to Columns[1].PickList.Count -1 do
    begin
      if Pos(ATypePart, UpperCase(Columns[1].PickList[i])) = 1 then
      begin
        Result:= Columns[1].PickList[i];
        Break;
      end;
    end;

    if Result = '' then
    for i:= 0 to Columns[1].PickList.Count -1 do
    begin
      if Pos(ATypePart, UpperCase(Columns[1].PickList[i])) > 0 then
      begin
        Result:= Columns[1].PickList[i];
        Break;
      end;
    end;
  end;
end;


procedure TfmNewTable.bbScriptClick(Sender: TObject);
var
  List: TStringList;
  KeyField: string;
  GeneratorName: string;
begin
  if Validate then
  begin
    List:= TStringList.Create;
    try
      List.Text:= GenerateCreateSQL(KeyField, GeneratorName);
      if cxCreateGen.Checked then
      begin;
        //todo: move this generator/trigger creation to a utility function somewhere; likewise for other create/alter DDL code
        List.Add('');
        List.Add('-- Generator');
        List.Add('create generator ' + GeneratorName + ';');

        List.Add('');
        List.Add('-- Trigger');
        List.Add('CREATE TRIGGER ' + GeneratorName + ' FOR ' + edNewTable.Text);
        List.Add('ACTIVE BEFORE INSERT POSITION 0 ');
        List.Add('AS BEGIN ');
        List.Add('IF (NEW.' + KeyField + ' IS NULL OR NEW.' + KeyField + ' = 0) THEN ');
        List.Add('  NEW.' + KeyField + ' = GEN_ID(' + GeneratorName + ', 1);');
        List.Add('END;');
      end;
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Create New Table: ' + edNewTable.Text, List.Text);
    finally
      List.Free;
    end;
    bbCloseClick(nil);
  end;
end;

procedure TfmNewTable.bbCancelClick(Sender: TObject);
begin
  bbCloseClick(nil);
end;

procedure TfmNewTable.cxGrantPermissionChange(Sender: TObject);
begin
  cbPermission.Enabled:= cxGrantPermission.Checked;
  laPermission.Visible:= cxGrantPermission.Checked;
  cbRolesUsers.Visible:= cxGrantPermission.Checked;
end;

procedure TfmNewTable.edNewTableKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then
    StringGrid1.SetFocus;
end;

procedure TfmNewTable.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmNewTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmNewTable.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;


procedure TfmNewTable.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aType: string;
begin
  with StringGrid1 do
  if Key = VK_DOWN then // Cursor down
  if Trim(Cells[0, RowCount - 1]) <> '' then
  begin
    RowCount:= RowCount + 1;
    Row:= RowCount - 1;
    Cells[3, Row]:= '1';
    Cells[4, Row]:= '0';
  end
  else
  if Key = VK_INSERT then // Insert
  begin
    InsertColRow(False, Row);
  end
  else
  if Key = VK_DELETE then // Delete
  begin
    if RowCount > 1 then
      DeleteColRow(False, Row);
  end
  else
  if key = VK_RETURN then // Enter
  begin
    if (Row + 1 = RowCount) and (Col > 1) then
    begin
      RowCount:= RowCount + 1;
      Col:= 0;
      Row:= Row + 1;
    end
    else
    if Col > 1 then
    begin
      Col:= 0;
      Row:= Row + 1;
    end;
    Key:= 0;
  end;
end;

procedure TfmNewTable.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Primary Key checked
  if (StringGrid1.Col = 4) and (StringGrid1.Cells[4, StringGrid1.Row] = '0') then
  begin
    if not cxCreateGen.Checked then
      StringGrid1.Cells[3, StringGrid1.Row]:= '0'; // Uncheck Allow null
    StringGrid1.Cells[5, StringGrid1.Row]:= ''; // Remove default value
  end;

  // Allow Null checked
  if (StringGrid1.Col = 3) and (StringGrid1.Cells[3, StringGrid1.Row] = '1') then
  begin
    StringGrid1.Cells[5, StringGrid1.Row]:= ''; // Remove default value
  end;
end;

initialization
  {$I newtable.lrs}

end.

