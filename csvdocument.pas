{
  CSV Parser, Builder and Document classes.
  Version 0.5 2012-09-20

  Copyright (C) 2010-2012 Vladimir Zhirov <vvzh.home@gmail.com>

  Contributors:
    Luiz Americo Pereira Camara
    Mattias Gaertner
    Reinier Olislagers

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit CsvDocument;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, StrUtils;

type
  {$IFNDEF FPC}
  TFPObjectList = TObjectList;
  {$ENDIF}

  TCSVChar = Char;

  TCSVHandler = class(TObject)
  private
    procedure SetDelimiter(const AValue: TCSVChar);
    procedure SetQuoteChar(const AValue: TCSVChar);
    procedure UpdateCachedChars;
  protected
    // special chars
    FDelimiter: TCSVChar;
    FQuoteChar: TCSVChar;
    FLineEnding: String;
    // cached values to speed up special chars operations
    FSpecialChars: TSysCharSet;
    FDoubleQuote: String;
    // parser settings
    FIgnoreOuterWhitespace: Boolean;
    // builder settings
    FQuoteOuterWhitespace: Boolean;
    // document settings
    FEqualColCountPerRow: Boolean;
  public
    constructor Create;
    procedure AssignCSVProperties(ASource: TCSVHandler);
    // Delimiter that separates the field, e.g. comma, semicolon, tab
    property Delimiter: TCSVChar read FDelimiter write SetDelimiter;
    // Character used to quote "problematic" data
    // (e.g. with delimiters or spaces in them)
    // A common quotechar is "
    property QuoteChar: TCSVChar read FQuoteChar write SetQuoteChar;
    // String at the end of the line of data (e.g. CRLF)
    property LineEnding: String read FLineEnding write FLineEnding;
    // Ignore whitespace between delimiters and field data
    property IgnoreOuterWhitespace: Boolean read FIgnoreOuterWhitespace write FIgnoreOuterWhitespace;
    // Use quotes when outer whitespace is found
    property QuoteOuterWhitespace: Boolean read FQuoteOuterWhitespace write FQuoteOuterWhitespace;
    // When reading and writing: make sure every line has the same column count, create empty cells in the end of row if required
    property EqualColCountPerRow: Boolean read FEqualColCountPerRow write FEqualColCountPerRow;
  end;

  // Sequential input from CSV stream
  TCSVParser = class(TCSVHandler)
  private
    // fields
    FSourceStream: TStream;
    FStrStreamWrapper: TStringStream;
    // parser state
    EndOfFile: Boolean;
    EndOfLine: Boolean;
    FCurrentChar: TCSVChar;
    FCurrentRow: Integer;
    FCurrentCol: Integer;
    FMaxColCount: Integer;
    // output buffers
    FCellBuffer: String;
    FWhitespaceBuffer: String;
    procedure ClearOutput;
    // basic parsing
    procedure SkipEndOfLine;
    procedure SkipDelimiter;
    procedure SkipWhitespace;
    procedure NextChar;
    // complex parsing
    procedure ParseCell;
    procedure ParseQuotedValue;
    // simple parsing
    procedure ParseValue;
  public
    constructor Create;
    destructor Destroy; override;
    // Source data stream
    procedure SetSource(AStream: TStream); overload;
    // Source data string
    procedure SetSource(const AString: String); overload;
    // Rewind to beginning of data
    procedure ResetParser;
    // Read next cell data; return false if end of file reached
    function  ParseNextCell: Boolean;
    // Current row (0 based)
    property CurrentRow: Integer read FCurrentRow;
    // Current column (0 based); -1 if invalid/before beginning of file
    property CurrentCol: Integer read FCurrentCol;
    // Data in current cell
    property CurrentCellText: String read FCellBuffer;
    // The maximum number of columns found in the stream:
    property MaxColCount: Integer read FMaxColCount;
  end;

  // Sequential output to CSV stream
  TCSVBuilder = class(TCSVHandler)
  private
    FOutputStream: TStream;
    FDefaultOutput: TMemoryStream;
    FNeedLeadingDelimiter: Boolean;
    function GetDefaultOutputAsString: String;
  protected
    procedure AppendStringToStream(const AString: String; AStream: TStream);
    function  QuoteCSVString(const AValue: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    // Set output/destination stream.
    // If not called, output is sent to DefaultOutput
    procedure SetOutput(AStream: TStream);
    // If using default stream, reset output to beginning.
    // If using user-defined stream, user should reposition stream himself
    procedure ResetBuilder;
    // Add a cell to the output with data AValue
    procedure AppendCell(const AValue: String);
    // Write end of row to the output, starting a new row
    procedure AppendRow;
    // Default output as memorystream (if output not set using SetOutput)
    property DefaultOutput: TMemoryStream read FDefaultOutput;
    // Default output in string format (if output not set using SetOutput)
    property DefaultOutputAsString: String read GetDefaultOutputAsString;
  end;

  // Random access to CSV document. Reads entire document into memory.
  TCSVDocument = class(TCSVHandler)
  private
    FRows: TFPObjectList;
    FParser: TCSVParser;
    FBuilder: TCSVBuilder;
    // helpers
    procedure ForceRowIndex(ARowIndex: Integer);
    function  CreateNewRow(const AFirstCell: String = ''): TObject;
    // property getters/setters
    function  GetCell(ACol, ARow: Integer): String;
    procedure SetCell(ACol, ARow: Integer; const AValue: String);
    function  GetCSVText: String;
    procedure SetCSVText(const AValue: String);
    function  GetRowCount: Integer;
    function  GetColCount(ARow: Integer): Integer;
    function  GetMaxColCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    // Input/output

    // Load document from file AFileName
    procedure LoadFromFile(const AFilename: String);
    // Load document from stream AStream
    procedure LoadFromStream(AStream: TStream);
    // Save document to file AFilename
    procedure SaveToFile(const AFilename: String);
    // Save document to stream AStream
    procedure SaveToStream(AStream: TStream);

    // Row and cell operations

    // Add a new row and a cell with content AFirstCell
    procedure AddRow(const AFirstCell: String = '');
    // Add a cell at row ARow with data AValue
    procedure AddCell(ARow: Integer; const AValue: String = '');
    // Insert a row at row ARow with first cell data AFirstCell
    // If there is no row ARow, insert row at end
    procedure InsertRow(ARow: Integer; const AFirstCell: String = '');
    // Insert a cell at specified position with data AValue
    procedure InsertCell(ACol, ARow: Integer; const AValue: String = '');
    // Remove specified row
    procedure RemoveRow(ARow: Integer);
    // Remove specified cell
    procedure RemoveCell(ACol, ARow: Integer);
    // Indicates if there is a row at specified position
    function  HasRow(ARow: Integer): Boolean;
    // Indicates if there is a cell at specified position
    function  HasCell(ACol, ARow: Integer): Boolean;
    
    // Search
    
    // Return column for cell data AString at row ARow
    function  IndexOfCol(const AString: String; ARow: Integer): Integer;
    // Return row for cell data AString at coloumn ACol
    function  IndexOfRow(const AString: String; ACol: Integer): Integer;

    // Utils

    // Remove all data
    procedure Clear;
    // Copy entire row ARow to row position AInsertPos.
    // Adds empty rows if necessary
    procedure CloneRow(ARow, AInsertPos: Integer);
    // Exchange contents of the two specified rows
    procedure ExchangeRows(ARow1, ARow2: Integer);
    // Rewrite all line endings within cell data to LineEnding
    procedure UnifyEmbeddedLineEndings;
    // Remove empty cells at end of rows from entire document
    procedure RemoveTrailingEmptyCells;

    // Properties

    // Cell data at column ACol, row ARow.
    property Cells[ACol, ARow: Integer]: String read GetCell write SetCell; default;
    // Number of rows
    property RowCount: Integer read GetRowCount;
    // Number of columns for row ARow
    property ColCount[ARow: Integer]: Integer read GetColCount;
    // Maximum number of columns found in all rows in document
    property MaxColCount: Integer read GetMaxColCount;
    // Document formatted as CSV text
    property CSVText: String read GetCSVText write SetCSVText;
  end;

implementation

const
  CsvCharSize = SizeOf(TCSVChar);
  CR    = #13;
  LF    = #10;
  HTAB  = #9;
  SPACE = #32;
  WhitespaceChars = [HTAB, SPACE];
  LineEndingChars = [CR, LF];

// The following implementation of ChangeLineEndings function originates from
// Lazarus CodeTools library by Mattias Gaertner. It was explicitly allowed
// by Mattias to relicense it under modified LGPL and include into CsvDocument.

function ChangeLineEndings(const AString, ALineEnding: String): String;
var
  I: Integer;
  Src: PChar;
  Dest: PChar;
  DestLength: Integer;
  EndingLength: Integer;
  EndPos: PChar;
begin
  if AString = '' then
    Exit(AString);
  EndingLength := Length(ALineEnding);
  DestLength := Length(AString);

  Src := PChar(AString);
  EndPos := Src + DestLength;
  while Src < EndPos do
  begin
    if (Src^ = CR) then
    begin
      Inc(Src);
      if (Src^ = LF) then
      begin
        Inc(Src);
        Inc(DestLength, EndingLength - 2);
      end else
        Inc(DestLength, EndingLength - 1);
    end else
    begin
      if (Src^ = LF) then
        Inc(DestLength, EndingLength - 1);
      Inc(Src);
    end;
  end;

  SetLength(Result, DestLength);
  Src := PChar(AString);
  Dest := PChar(Result);
  EndPos := Dest + DestLength;
  while (Dest < EndPos) do
  begin
    if Src^ in LineEndingChars then
    begin
      for I := 1 to EndingLength do
      begin
        Dest^ := ALineEnding[I];
        Inc(Dest);
      end;
      if (Src^ = CR) and (Src[1] = LF) then
        Inc(Src, 2)
      else
        Inc(Src);
    end else
    begin
      Dest^ := Src^;
      Inc(Src);
      Inc(Dest);
    end;
  end;
end;

{ TCSVHandler }

procedure TCSVHandler.SetDelimiter(const AValue: TCSVChar);
begin
  if FDelimiter <> AValue then
  begin
    FDelimiter := AValue;
    UpdateCachedChars;
  end;
end;

procedure TCSVHandler.SetQuoteChar(const AValue: TCSVChar);
begin
  if FQuoteChar <> AValue then
  begin
    FQuoteChar := AValue;
    UpdateCachedChars;
  end;
end;

procedure TCSVHandler.UpdateCachedChars;
begin
  FDoubleQuote := FQuoteChar + FQuoteChar;
  FSpecialChars := [CR, LF, FDelimiter, FQuoteChar];
end;

constructor TCSVHandler.Create;
begin
  inherited Create;
  FDelimiter := ',';
  FQuoteChar := '"';
  FLineEnding := CR + LF;
  FIgnoreOuterWhitespace := False;
  FQuoteOuterWhitespace := True;
  FEqualColCountPerRow := True;
  UpdateCachedChars;
end;

procedure TCSVHandler.AssignCSVProperties(ASource: TCSVHandler);
begin
  FDelimiter := ASource.FDelimiter;
  FQuoteChar := ASource.FQuoteChar;
  FLineEnding := ASource.FLineEnding;
  FIgnoreOuterWhitespace := ASource.FIgnoreOuterWhitespace;
  FQuoteOuterWhitespace := ASource.FQuoteOuterWhitespace;
  FEqualColCountPerRow := ASource.FEqualColCountPerRow;
  UpdateCachedChars;
end;

{ TCSVParser }

procedure TCSVParser.ClearOutput;
begin
  FCellBuffer := '';
  FWhitespaceBuffer := '';
  FCurrentRow := 0;
  FCurrentCol := -1;
  FMaxColCount := 0;
end;

procedure TCSVParser.SkipEndOfLine;
begin
  // treat LF+CR as two linebreaks, not one
  if (FCurrentChar = CR) then
    NextChar;
  if (FCurrentChar = LF) then
    NextChar;
end;

procedure TCSVParser.SkipDelimiter;
begin
  if FCurrentChar = FDelimiter then
    NextChar;
end;

procedure TCSVParser.SkipWhitespace;
begin
  while FCurrentChar = SPACE do
    NextChar;
end;

procedure TCSVParser.NextChar;
begin
  if FSourceStream.Read(FCurrentChar, CsvCharSize) < CsvCharSize then
  begin
    FCurrentChar := #0;
    EndOfFile := True;
  end;
  EndOfLine := FCurrentChar in LineEndingChars;
end;

procedure TCSVParser.ParseCell;
begin
  FCellBuffer := '';
  if FIgnoreOuterWhitespace then
    SkipWhitespace;
  if FCurrentChar = FQuoteChar then
    ParseQuotedValue
  else
    ParseValue;
end;

procedure TCSVParser.ParseQuotedValue;
var
  QuotationEnd: Boolean;
begin
  NextChar; // skip opening quotation char
  repeat
    // read value up to next quotation char
    while not ((FCurrentChar = FQuoteChar) or EndOfFile) do
    begin
      if EndOfLine then
      begin
        AppendStr(FCellBuffer, FLineEnding);
        SkipEndOfLine;
      end else
      begin
        AppendStr(FCellBuffer, FCurrentChar);
        NextChar;
      end;
    end;
    // skip quotation char (closing or escaping)
    if not EndOfFile then
      NextChar;
    // check if it was escaping
    if FCurrentChar = FQuoteChar then
    begin
      AppendStr(FCellBuffer, FCurrentChar);
      QuotationEnd := False;
      NextChar;
    end else
      QuotationEnd := True;
  until QuotationEnd;
  // read the rest of the value until separator or new line
  ParseValue;
end;

procedure TCSVParser.ParseValue;
begin
  while not ((FCurrentChar = FDelimiter) or EndOfLine or EndOfFile) do
  begin
    AppendStr(FWhitespaceBuffer, FCurrentChar);
    NextChar;
  end;
  // merge whitespace buffer
  if FIgnoreOuterWhitespace then
    RemoveTrailingChars(FWhitespaceBuffer, WhitespaceChars);
  AppendStr(FCellBuffer, FWhitespaceBuffer);
  FWhitespaceBuffer := '';
end;

constructor TCSVParser.Create;
begin
  inherited Create;
  ClearOutput;
  FStrStreamWrapper := nil;
  EndOfFile := True;
end;

destructor TCSVParser.Destroy;
begin
  FreeAndNil(FStrStreamWrapper);
  inherited Destroy;
end;

procedure TCSVParser.SetSource(AStream: TStream);
begin
  FSourceStream := AStream;
  ResetParser;
end;

procedure TCSVParser.SetSource(const AString: String); overload;
begin
  FreeAndNil(FStrStreamWrapper);
  FStrStreamWrapper := TStringStream.Create(AString);
  SetSource(FStrStreamWrapper);
end;

procedure TCSVParser.ResetParser;
begin
  ClearOutput;
  FSourceStream.Seek(0, soFromBeginning);
  EndOfFile := False;
  NextChar;
end;

// Parses next cell; returns True if there are more cells in the input stream.
function TCSVParser.ParseNextCell: Boolean;
var
  LineColCount: Integer;
begin
  if EndOfLine or EndOfFile then
  begin
    // Having read the previous line, adjust column count if necessary:
    LineColCount := FCurrentCol + 1;
    if LineColCount > FMaxColCount then
      FMaxColCount := LineColCount;
  end;

  if EndOfFile then
    Exit(False);

  // Handle line ending
  if EndOfLine then
  begin
    SkipEndOfLine;
    if EndOfFile then
      Exit(False);
    FCurrentCol := 0;
    Inc(FCurrentRow);
  end else
    Inc(FCurrentCol);

  // Skipping a delimiter should be immediately followed by parsing a cell
  // without checking for line break first, otherwise we miss last empty cell.
  // But 0th cell does not start with delimiter unlike other cells, so
  // the following check is required not to miss the first empty cell:
  if FCurrentCol > 0 then
    SkipDelimiter;
  ParseCell;
  Result := True;
end;

{ TCSVBuilder }

function TCSVBuilder.GetDefaultOutputAsString: String;
var
  StreamSize: Integer;
begin
  Result := '';
  StreamSize := FDefaultOutput.Size;
  if StreamSize > 0 then
  begin
    SetLength(Result, StreamSize);
    FDefaultOutput.ReadBuffer(Result[1], StreamSize);
  end;
end;

procedure TCSVBuilder.AppendStringToStream(const AString: String; AStream: TStream);
var
  StrLen: Integer;
begin
  StrLen := Length(AString);
  if StrLen > 0 then
    AStream.WriteBuffer(AString[1], StrLen);
end;

function TCSVBuilder.QuoteCSVString(const AValue: String): String;
var
  I: Integer;
  ValueLen: Integer;
  NeedQuotation: Boolean;
begin
  ValueLen := Length(AValue);

  NeedQuotation := (AValue <> '') and FQuoteOuterWhitespace
    and ((AValue[1] in WhitespaceChars) or (AValue[ValueLen] in WhitespaceChars));

  if not NeedQuotation then
    for I := 1 to ValueLen do
    begin
      if AValue[I] in FSpecialChars then
      begin
        NeedQuotation := True;
        Break;
      end;
    end;

  if NeedQuotation then
  begin
    // double existing quotes
    Result := FDoubleQuote;
    Insert(StringReplace(AValue, FQuoteChar, FDoubleQuote, [rfReplaceAll]),
      Result, 2);
  end else
    Result := AValue;
end;

constructor TCSVBuilder.Create;
begin
  inherited Create;
  FDefaultOutput := TMemoryStream.Create;
  FOutputStream := FDefaultOutput;
end;

destructor TCSVBuilder.Destroy;
begin
  FreeAndNil(FDefaultOutput);
  inherited Destroy;
end;

procedure TCSVBuilder.SetOutput(AStream: TStream);
begin
  if Assigned(AStream) then
    FOutputStream := AStream
  else
    FOutputStream := FDefaultOutput;

  ResetBuilder;
end;

procedure TCSVBuilder.ResetBuilder;
begin
  if FOutputStream = FDefaultOutput then
    FDefaultOutput.Clear;

  // Do not clear external FOutputStream because it may be pipe stream
  // or something else that does not support size and position.
  // To clear external output is up to the user of TCSVBuilder.

  FNeedLeadingDelimiter := False;
end;

procedure TCSVBuilder.AppendCell(const AValue: String);
var
  CellValue: String;
begin
  if FNeedLeadingDelimiter then
    FOutputStream.WriteBuffer(FDelimiter, CsvCharSize);

  CellValue := ChangeLineEndings(AValue, FLineEnding);
  CellValue := QuoteCSVString(CellValue);
  AppendStringToStream(CellValue, FOutputStream);

  FNeedLeadingDelimiter := True;
end;

procedure TCSVBuilder.AppendRow;
begin
  AppendStringToStream(FLineEnding, FOutputStream);
  FNeedLeadingDelimiter := False;
end;

//------------------------------------------------------------------------------

type
  TCSVCell = class
  public
    // Value (contents) of cell in string form
    Value: String;
  end;

  TCSVRow = class
  private
    FCells: TFPObjectList;
    procedure ForceCellIndex(ACellIndex: Integer);
    function  CreateNewCell(const AValue: String): TCSVCell;
    function  GetCellValue(ACol: Integer): String;
    procedure SetCellValue(ACol: Integer; const AValue: String);
    function  GetColCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    // cell operations
    // Add cell with value AValue to row
    procedure AddCell(const AValue: String = '');
    // Insert cell with value AValue at specified column
    procedure InsertCell(ACol: Integer; const AValue: String);
    // Remove cell from specified column
    procedure RemoveCell(ACol: Integer);
    // Indicates if specified column contains a cell/data
    function  HasCell(ACol: Integer): Boolean;
    // utilities
    // Copy entire row
    function  Clone: TCSVRow;
    // Remove all empty cells at the end of the row
    procedure TrimEmptyCells;
    // Replace various line endings in data with ALineEnding
    procedure SetValuesLineEnding(const ALineEnding: String);
    // properties
    // Value/data of cell at column ACol
    property CellValue[ACol: Integer]: String read GetCellValue write SetCellValue;
    // Number of columns in row
    property ColCount: Integer read GetColCount;
  end;

{ TCSVRow }

procedure TCSVRow.ForceCellIndex(ACellIndex: Integer);
begin
  while FCells.Count <= ACellIndex do
    AddCell();
end;

function TCSVRow.CreateNewCell(const AValue: String): TCSVCell;
begin
  Result := TCSVCell.Create;
  Result.Value := AValue;
end;

function TCSVRow.GetCellValue(ACol: Integer): String;
begin
  if HasCell(ACol) then
    Result := TCSVCell(FCells[ACol]).Value
  else
    Result := '';
end;

procedure TCSVRow.SetCellValue(ACol: Integer; const AValue: String);
begin
  ForceCellIndex(ACol);
  TCSVCell(FCells[ACol]).Value := AValue;
end;

function TCSVRow.GetColCount: Integer;
begin
  Result := FCells.Count;
end;

constructor TCSVRow.Create;
begin
  inherited Create;
  FCells := TFPObjectList.Create;
end;

destructor TCSVRow.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TCSVRow.AddCell(const AValue: String = '');
begin
  FCells.Add(CreateNewCell(AValue));
end;

procedure TCSVRow.InsertCell(ACol: Integer; const AValue: String);
begin
  FCells.Insert(ACol, CreateNewCell(AValue));
end;

procedure TCSVRow.RemoveCell(ACol: Integer);
begin
  if HasCell(ACol) then
    FCells.Delete(ACol);
end;

function TCSVRow.HasCell(ACol: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < FCells.Count);
end;

function TCSVRow.Clone: TCSVRow;
var
  I: Integer;
begin
  Result := TCSVRow.Create;
  for I := 0 to ColCount - 1 do
    Result.AddCell(CellValue[I]);
end;

procedure TCSVRow.TrimEmptyCells;
var
  I: Integer;
  MaxCol: Integer;
begin
  MaxCol := FCells.Count - 1;
  for I := MaxCol downto 0 do
  begin
    if (TCSVCell(FCells[I]).Value = '') then
    begin
      if (FCells.Count > 1) then
        FCells.Delete(I);
    end else
      break; // We hit the first non-empty cell so stop
  end;
end;

procedure TCSVRow.SetValuesLineEnding(const ALineEnding: String);
var
  I: Integer;
begin
  for I := 0 to FCells.Count - 1 do
    CellValue[I] := ChangeLineEndings(CellValue[I], ALineEnding);
end;

{ TCSVDocument }

procedure TCSVDocument.ForceRowIndex(ARowIndex: Integer);
begin
  while FRows.Count <= ARowIndex do
    AddRow();
end;

function TCSVDocument.CreateNewRow(const AFirstCell: String): TObject;
var
  NewRow: TCSVRow;
begin
  NewRow := TCSVRow.Create;
  if AFirstCell <> '' then
    NewRow.AddCell(AFirstCell);
  Result := NewRow;
end;

function TCSVDocument.GetCell(ACol, ARow: Integer): String;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).CellValue[ACol]
  else
    Result := '';
end;

procedure TCSVDocument.SetCell(ACol, ARow: Integer; const AValue: String);
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).CellValue[ACol] := AValue;
end;

function TCSVDocument.GetCSVText: String;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    SaveToStream(StringStream);
    Result := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TCSVDocument.SetCSVText(const AValue: String);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(AValue);
  try
    LoadFromStream(StringStream);
  finally
    FreeAndNil(StringStream);
  end;
end;

function TCSVDocument.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TCSVDocument.GetColCount(ARow: Integer): Integer;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).ColCount
  else
    Result := 0;
end;

// Returns maximum number of columns in the document
function TCSVDocument.GetMaxColCount: Integer;
var
  I, CC: Integer;
begin
  // While calling MaxColCount in TCSVParser could work,
  // we'd need to adjust for any subsequent changes in
  // TCSVDocument
  Result := 0;
  for I := 0 to RowCount - 1 do
  begin
    CC := ColCount[I];
    if CC > Result then
      Result := CC;
  end;
end;

constructor TCSVDocument.Create;
begin
  inherited Create;
  FRows := TFPObjectList.Create;
  FParser := nil;
  FBuilder := nil;
end;

destructor TCSVDocument.Destroy;
begin
  FreeAndNil(FBuilder);
  FreeAndNil(FParser);
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TCSVDocument.LoadFromFile(const AFilename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCSVDocument.LoadFromStream(AStream: TStream);
var
  I, J, MaxCol: Integer;
begin
  Clear;

  if not Assigned(FParser) then
    FParser := TCSVParser.Create;

  FParser.AssignCSVProperties(Self);
  with FParser do
  begin
    SetSource(AStream);
    while ParseNextCell do
      Cells[CurrentCol, CurrentRow] := CurrentCellText;
  end;

  if FEqualColCountPerRow then
  begin
    MaxCol := MaxColCount - 1;
    for I := 0 to RowCount - 1 do
      for J := ColCount[I] to MaxCol do
        Cells[J, I] := '';
  end;
end;

procedure TCSVDocument.SaveToFile(const AFilename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCSVDocument.SaveToStream(AStream: TStream);
var
  I, J, MaxCol: Integer;
begin
  if not Assigned(FBuilder) then
    FBuilder := TCSVBuilder.Create;

  FBuilder.AssignCSVProperties(Self);
  with FBuilder do
  begin
    if FEqualColCountPerRow then
      MaxCol := MaxColCount - 1;

    SetOutput(AStream);
    for I := 0 to RowCount - 1 do
    begin
      if not FEqualColCountPerRow then
        MaxCol := ColCount[I] - 1;
      for J := 0 to MaxCol do
        AppendCell(Cells[J, I]);
      AppendRow;
    end;
  end;
end;

procedure TCSVDocument.AddRow(const AFirstCell: String = '');
begin
  FRows.Add(CreateNewRow(AFirstCell));
end;

procedure TCSVDocument.AddCell(ARow: Integer; const AValue: String = '');
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).AddCell(AValue);
end;

procedure TCSVDocument.InsertRow(ARow: Integer; const AFirstCell: String = '');
begin
  if HasRow(ARow) then
    FRows.Insert(ARow, CreateNewRow(AFirstCell))
  else
    AddRow(AFirstCell);
end;

procedure TCSVDocument.InsertCell(ACol, ARow: Integer; const AValue: String);
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).InsertCell(ACol, AValue);
end;

procedure TCSVDocument.RemoveRow(ARow: Integer);
begin
  if HasRow(ARow) then
    FRows.Delete(ARow);
end;

procedure TCSVDocument.RemoveCell(ACol, ARow: Integer);
begin
  if HasRow(ARow) then
    TCSVRow(FRows[ARow]).RemoveCell(ACol);
end;

function TCSVDocument.HasRow(ARow: Integer): Boolean;
begin
  Result := (ARow >= 0) and (ARow < FRows.Count);
end;

function TCSVDocument.HasCell(ACol, ARow: Integer): Boolean;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).HasCell(ACol)
  else
    Result := False;
end;

function TCSVDocument.IndexOfCol(const AString: String; ARow: Integer): Integer;
var
  CC: Integer;
begin
  CC := ColCount[ARow];
  Result := 0;
  while (Result < CC) and (Cells[Result, ARow] <> AString) do
    Inc(Result);
  if Result = CC then
    Result := -1;
end;

function TCSVDocument.IndexOfRow(const AString: String; ACol: Integer): Integer;
var
  RC: Integer;
begin
  RC := RowCount;
  Result := 0;
  while (Result < RC) and (Cells[ACol, Result] <> AString) do
    Inc(Result);
  if Result = RC then
    Result := -1;
end;

procedure TCSVDocument.Clear;
begin
  FRows.Clear;
end;

procedure TCSVDocument.CloneRow(ARow, AInsertPos: Integer);
var
  NewRow: TObject;
begin
  if not HasRow(ARow) then
    Exit;
  NewRow := TCSVRow(FRows[ARow]).Clone;
  if not HasRow(AInsertPos) then
  begin
    ForceRowIndex(AInsertPos - 1);
    FRows.Add(NewRow);
  end else
    FRows.Insert(AInsertPos, NewRow);
end;

procedure TCSVDocument.ExchangeRows(ARow1, ARow2: Integer);
begin
  if not (HasRow(ARow1) and HasRow(ARow2)) then
    Exit;
  FRows.Exchange(ARow1, ARow2);
end;

procedure TCSVDocument.UnifyEmbeddedLineEndings;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    TCSVRow(FRows[I]).SetValuesLineEnding(FLineEnding);
end;

procedure TCSVDocument.RemoveTrailingEmptyCells;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    TCSVRow(FRows[I]).TrimEmptyCells;
end;

end.
