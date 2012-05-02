unit NewTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Grids, Buttons, SynEdit, SynCompletion,
  SynHighlighterSQL;

type

  { TfmNewTable }

  TfmNewTable = class(TForm)
    bbScript: TBitBtn;
    BitBtn2: TBitBtn;
    cxCreateGen: TCheckBox;
    edNewTable: TEdit;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure bbScriptClick(Sender: TObject);
    procedure cxCreateGenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure StringGrid1ChangeBounds(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GenerateCreateSQL(var KeyField, GeneratorName: string): string;
    procedure Init(dbIndex: Integer);
    procedure StringGrid1PickListSelect(Sender: TObject);
  private
    fdbIndex: Integer;
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
  Result:= 'create table ' + edNewTable.Text + ' (' + #10;
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
        if (Pos('CHAR', FieldType) > 0) and (Pos('''', StringGrid1.Cells[5, i]) = 0) then
          FieldLine:= FieldLine + ' default ''' + StringGrid1.Cells[5, i] + ''''
        else
          FieldLine:= FieldLine + ' default ' + StringGrid1.Cells[5, i];
      end;

      if (StringGrid1.RowCount > i + 1) and (Trim(stringGrid1.Cells[0, i + 1]) <> '') then
        FieldLine:= FieldLine + ',' + #10;
      Result:= Result + FieldLine;
    end;

  // Set Primary key
  if PKey <> '' then
  begin
    Delete(PKey, Length(PKey), 1);
    Result:= Result + ', ' + #10 + ' primary key (' + PKey + ') ' + #10;
  end;
  Result:= Result + ');';
end;

procedure TfmNewTable.Init(dbIndex: Integer);
var
  i: Integer;
begin
  fdbIndex:= dbIndex;
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
end;

procedure TfmNewTable.StringGrid1PickListSelect(Sender: TObject);
var
  SelType: string;
begin
  if (StringGrid1.Col = 1) then
  begin
    SelType:= StringGrid1.Cells[1, StringGrid1.Row];
    StringGrid1.Cells[2, StringGrid1.Row]:= IntToStr(dmSysTables.GetDefaultTypeSize(fdbIndex, SelType));
  end;
end;

procedure TfmNewTable.StringGrid1EditingDone(Sender: TObject);
var
  SelType: string;
begin
//  if (StringGrid1.Col = 1) and (Trim(StringGrid1.Cells[2, StringGrid1.Row]) = '') then
//  begin
//    SelType:= StringGrid1.Cells[1, StringGrid1.Row];
//    StringGrid1.Cells[2, StringGrid1.Row]:= IntToStr(dmSysTables.GetDefaultTypeSize(fdbIndex, SelType));
//  end;

end;

procedure TfmNewTable.bbScriptClick(Sender: TObject);
var
  List: TStringList;
  KeyField: string;
  GeneratorName: string;
begin
  List:= TStringList.Create;
  List.Text:= GenerateCreateSQL(KeyField, GeneratorName);
  if cxCreateGen.Checked then
  begin;
    List.Add('');
    List.Add('-- Generator');
    List.Add('create generator ' + GeneratorName + ';');

    List.Add('-- Trigger');
    List.Add('CREATE TRIGGER ' + GeneratorName + ' FOR ' + edNewTable.Text);
    List.Add('ACTIVE BEFORE INSERT POSITION 0 ');
    List.Add('AS BEGIN ');
    List.Add('IF (NEW.' + KeyField + ' IS NULL OR NEW.' + KeyField + ' = 0) THEN ');
    List.Add('  NEW.' + KeyField + ' = GEN_ID(' + GeneratorName + ', 1);');
    List.Add('END;');
  end;

  fmMain.ShowCompleteQueryWindow(fdbIndex, 'Create New Table: ' + edNewTable.Text, List.Text);
  List.Free;
  Close;
end;

procedure TfmNewTable.cxCreateGenClick(Sender: TObject);
begin
  if cxCreateGen.Checked and (StringGrid1.RowCount > 1) then
    StringGrid1.Cells[3, 1]:= '1';
end;

procedure TfmNewTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StringGrid1.Row:= 1;
end;

procedure TfmNewTable.StringGrid1ChangeBounds(Sender: TObject);
begin

end;


procedure TfmNewTable.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 40 then // Key down
    if Trim(StringGrid1.Cells[0, StringGrid1.RowCount - 1]) <> '' then
    begin
      StringGrid1.RowCount:= StringGrid1.RowCount + 1;
      StringGrid1.Row:= StringGrid1.RowCount - 1;
      StringGrid1.Cells[3, StringGrid1.Row]:= '1';
      StringGrid1.Cells[4, StringGrid1.Row]:= '0';
    end;
  if Key = 45 then // Insert
  begin
    StringGrid1.InsertColRow(False, StringGrid1.Row);
  end
  else
  if Key = 46 then // Delete
  begin
    if StringGrid1.RowCount > 1 then
      StringGrid1.DeleteColRow(False, StringGrid1.Row);
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

