unit fileimport;

{ CSV/tab separated/semicolon separated text file and spreadsheet file import
  useful for database import etc.

  Copyright (c) 2014 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

// Future plans:
// - look into replacing array mapping with more efficient implementation
// - spreadsheet support

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {laz_fpspreadsheet,} csvdocument;

type
  TMap = record
    SourceField: string;
    SourceFieldIndex: integer; //0-based
    DestinationField: string;
  end;

  { TFileImport }

  TFileImport = class(TObject)
  private
    // Stream backing CSV file. Always open once filename assigned; otherwise nil
    FCSVStream: TFileStream;
    // Source CSV file parser
    FCSVParser: TCSVParser;
    FDelimiter: char;
    FDestinationFields: TStringList;
    // End of file marker
    FEOF: boolean;
    // Maps file/source field to db field
    FMapping: array of TMap;
    FRow: integer; //current row (0-based)
    FRowCount: integer; //Cached row count
    FRowData: TStringList; //data in fields being read. Ordered in order of FMapping
    FSourceFields: TStringList;
    FSourceFile: string;
    // Try to automatically map source/file fields to database fields
    procedure AutoMap;
    // Retrieve single source=>destination mapping
    // Calls automap if no mapping exists yet -
    // which obviously requires DestinationFields to be filled
    function GetMapping(i: integer): TMap;
    // Checks if source field number SourceIndex is mapped; returns
    // map number or -1 if not mapped
    function GetMappingIndex(SourceIndex: integer): integer;
    // Checks if destination field is mapped; returns
    // map number or -1 if not mapped
    function GetMappingIndex(DestinationField: string): integer;
    // Number of mapped items
    function GetMappingCount: integer;
    function GetSourceFields: TStringlist;
    // Indicates if field mapping already exists for this field
    function MappingExists(SourceField, DestinationField: string): boolean;
    // Extracts likely delimiters from text file
    procedure GuessDelimiter();
    // Read in source fields into FSourceFields.
    // Used in Automap as well as GetSourceFields
    procedure ReadSourceFields;
    // Moves back to beginning of CSV file
    procedure ResetCSVParser;
    procedure SetDelimiter(AValue: char);
    procedure SetSourceFile(AValue: string);
    // Initialize CSV parser with file, delimiter,
    procedure SetupCSVParser;
  public
    // Adds mapping from source/file field to db/destination field
    // if it doesn't already exist. Returns true if mapping added
    function AddMapping(SourceField, DestinationField: string): boolean;
    // Delete mapping number specified. Mapping and listbox numbers match
    // if -1 specified: delete entire mapping array
    // Returns true if one or more mappings were deleted
    function DeleteMapping(Item: integer): boolean;
    // Delete mapping for specified destination field
    // Returns true if one or more mappings were deleted
    function DeleteMapping(DestinationField: string): boolean;
    // After calling readrow, getdata returns data in the field mapped to
    // DestinationField or empty if nothing found
    function GetData(DestinationField: string): string;
    // After calling readrow, getdata returns data in the field with the
    // specified mapping or empty if nothing found
    function GetData(MapIndex: integer): string;
    // Reads file and counts number of rows
    // Returns 0 if invalid/empty
    function GetRowCount: integer;
    // Reads a row of data. Returns false if last row read (end of file)
    // Data in fields can be retrieved by calling GetData
    function ReadRow: boolean;

    // For delimited text files: delimiter that separates fields, such as ;
    property Delimiter: char read FDelimiter write SetDelimiter;
    // Field names of destination/target dataset. Required.
    property DestinationFields: TStringList read FDestinationFields;
    // Source file name. Required.
    property FileName: string read FSourceFile write SetSourceFile;
    // Mapping from source to destination field
    // 0 based.
    property Mapping[i: integer]: TMap read GetMapping;
    // Number of mappings
    property MappingCount: integer read GetMappingCount;
    // Row number (0-based)
    property Row: integer read FRow;
    // Field names (or field content in first line) in input/source file.
    property SourceFields: TStringlist read GetSourceFields;
    //property SpreadsheetParser: to do implement spreadsheet support
    constructor Create;
    destructor Destroy; override;
  end;
implementation


{ TFileImport }

function TFileImport.AddMapping(SourceField, DestinationField: string): boolean;
var
  SourceIndex: integer;
begin
  result:=false;
  SourceIndex:=FSourceFields.IndexOf(SourceField);
  if SourceIndex=-1 then
    raise Exception.CreateFmt('Source field %s does not exist in file.',[SourceField]);
  if not MappingExists(SourceField,DestinationField) then
  begin
    // Enlarge array by 1 more item
    SetLength(Fmapping,High(FMapping)+2);
    FMapping[high(FMapping)].SourceField:=SourceField;
    FMapping[high(FMapping)].SourceFieldIndex:=SourceIndex;
    FMapping[high(FMapping)].DestinationField:=DestinationField;
    while FRowData.Count<Length(FMapping) do
    begin
      FRowData.Add(''); //reserve space for data
    end;
    result:=true;
  end;
end;

procedure TFileImport.AutoMap;
var
  DestFields: integer;
  DestField: string; //database field name
  FileField: string; //source field
  FileFieldNo: integer; // source field number
begin
  SetLength(FMapping,0);

  GuessDelimiter;
  ReadSourceFields;

  // Try to use header/first row field names...
  for FileFieldNo := 0 to FSourceFields.Count-1 do
  begin
    FileField:=FSourceFields[FileFieldNo];
    // This is a bit clunky but it works
    for DestFields:=0 to FDestinationFields.Count -1 do
    begin
      DestField:=FDestinationFields[DestFields];
      // Add mapping if it doesn't already exist
      if trim(lowercase(FileField)) = trim(lowercase(DestField)) then
      begin
        AddMapping(FileField, DestField);
      end;
    end;
  end;
end;

constructor TFileImport.Create;
begin
  FCSVParser:=nil;
  FCSVStream:=nil;
  FRowData:=TStringList.Create;
  FDelimiter:=',';
  FDestinationFields:=TStringList.Create;
  FSourceFields:=TStringList.Create;
end;

function TFileImport.DeleteMapping(Item: integer): boolean;
var
  i:integer;
begin
  result:=false;
  // Clear entire mapping array?
  if Item=-1 then
  begin
    SetLength(FMapping,0);
    FRowData.Clear;
    result:=true;
  end
  else
  begin
    if Item>High(FMapping) then
      exit(false); //can't delete item that doesn't exist
    // Delete item, move everything down
    for i:=Item to High(FMapping)-1 do
    begin
      FMapping[i].SourceField:=FMapping[i+1].SourceField;
      FMapping[i].SourceFieldIndex:=FMapping[i+1].SourceFieldIndex;
      FMapping[i].DestinationField:=FMapping[i+1].DestinationField;
    end;
    FRowData.Delete(Item);
    // Release last array item
    SetLength(FMapping,High(FMapping));
    result:=true;
  end;
end;

function TFileImport.DeleteMapping(DestinationField: string): boolean;
var
  Index: integer;
begin
  result:=false;
  Index:=GetMappingIndex(DestinationField);
  if Index>-1 then
  begin
    result:=DeleteMapping(Index);
  end;
end;

destructor TFileImport.Destroy;
begin
  FRowData.Free;
  FDestinationFields.Free;
  FSourceFields.Free;
  if assigned(FCSVParser) then FCSVParser.Free;
  if assigned(FCSVStream) then FCSVStream.Free;
  inherited Destroy;
end;

function TFileImport.GetData(MapIndex: integer): string;
begin
  if (MapIndex>=0) and (MapIndex<FRowData.Count) then
    result:=FRowData[MapIndex]
  else
    result:=''; //invalid data
end;

function TFileImport.GetData(DestinationField: string): string;
var
  MapIndex: integer;
begin
  MapIndex:=GetMappingIndex(DestinationField);
  if MapIndex>-1 then
    result:=FRowData[MapIndex]
  else
    result:='';
end;

function TFileImport.GetMapping(i: integer): TMap;
begin
  if (length(FMapping)=0) then
  begin
    AutoMap;
  end;
  result:=FMapping[i];
end;

function TFileImport.GetMappingCount: integer;
begin
  if (length(FMapping)=0) then
  begin
    AutoMap;
  end;
  result:=length(FMapping); //1-based
end;

function TFileImport.GetMappingIndex(SourceIndex: integer): integer;
var
  i:integer;
begin
  result:=-1;
  for i:=low(FMapping) to high(FMapping) do
  begin
    if FMapping[i].SourceFieldIndex=SourceIndex then
      exit(i);
  end;
end;

function TFileImport.GetMappingIndex(DestinationField: string): integer;
var
  i:integer;
begin
  result:=-1;
  for i:=low(FMapping) to high(FMapping) do
  begin
    if FMapping[i].DestinationField=DestinationField then
      exit(i);
  end;
end;

function TFileImport.GetRowCount: integer;
begin
  // Return cached value if possible:
  if FRowCount>-1 then exit(FRowCount);
  // Check for existing file file
  if assigned(FCSVStream) then
  begin
    if not(assigned(FCSVParser)) then
      SetupCSVParser;
    ResetCSVParser;
    while FCSVParser.ParseNextCell do
    begin
      // just loop
    end;
    FRowCount:=FCSVParser.CurrentRow+1;
    ResetCSVParser;
  end
  else
  begin
    FRowCount:=-1; //invalidate cache
  end;
  result:=FRowCount;
end;

function TFileImport.GetSourceFields: TStringlist;
begin
  // Create fields if they don't exist
  if assigned(FCSVStream) then
  begin
    if FSourceFields.Count=0 then
      ReadSourceFields;
  end
  else
  begin
    FSourceFields.Clear;
  end;
  result:=FSourceFields;
end;

function TFileImport.MappingExists(SourceField, DestinationField: string): boolean;
var
  i: integer;
begin
  result := false;
  for i:= low(FMapping) to high(FMapping) do
  begin
    if (lowercase(FMapping[i].SourceField)=lowercase(SourceField)) and
      (lowercase(FMapping[i].DestinationField)=lowercase(DestinationField)) then
    begin
      result:= true;
      break;
    end;
  end;
end;

procedure TFileImport.ReadSourceFields;
var
  ColCount: integer;
begin
  if not(assigned(FCSVStream)) then
    raise Exception.Create('CSVStream must exist.');
  if not(assigned(FCSVParser)) then
    SetupCSVParser
  else
    ResetCSVParser; //go to first row
  FSourceFields.Clear;

  ColCount:=0;
  while FCSVParser.CurrentRow<1 do
  begin
    FCSVParser.ParseNextCell;
    if FCSVParser.CurrentRow<1 then
      FSourceFields.Add(FCSVParser.CurrentCellText);
    inc(ColCount);
  end;
  ResetCSVParser;
end;

procedure TFileImport.ResetCSVParser;
begin
  FCSVParser.ResetParser; //rewind to beginning of file
  FEOF:=false;
  FRow:=FCSVParser.CurrentRow;
end;

procedure TFileImport.SetupCSVParser;
begin
  if assigned(FCSVParser) then
    FreeAndNil(FCSVParser);
  FCSVStream.Position:=0;
  FCSVParser:=TCSVParser.Create;
  FEOF:=false;
  FCSVParser.SetSource(FCSVStream);
  FCSVParser.Delimiter:=FDelimiter;
end;

function TFileImport.ReadRow: boolean;
var
  i: integer;
  MapIndex: integer;
begin  
  result:=false;
  // At end of file; there's nothing left...
  if FEOF then exit;

  if assigned(FCSVStream) then
  begin
    if not(assigned(FCSVParser)) then
      SetupCSVParser;

    // Remove any previous data
    for i:=0 to FRowData.Count-1 do
    begin
      FRowData[i]:='';
    end;

    // FRow should still be set to last row we read here
    FRow:=FCSVParser.CurrentRow;
    while (FCSVParser.CurrentRow=FRow) do
    begin
      MapIndex:=GetMappingIndex(FCSVParser.CurrentCol);
      if MapIndex>-1 then
        FRowData[MapIndex]:=FCSVParser.CurrentCellText;

      // Move to the next cell; check if at end of file
      if not(FCSVParser.ParseNextCell) then
      begin
        // End of file
        FEOF:=true; //remember for next ReadRow run
        FRow:=FCSVParser.CurrentRow;
        FRowCount:=FCSVParser.CurrentRow+1;
        break;
      end;
    end;
    result:=true;
  end
  else
  begin
    raise Exception.Create('ReadRow: input file is not assigned.');
  end;
end;

procedure TFileImport.SetDelimiter(AValue: char);
begin
  if FDelimiter=AValue then Exit;
  FDelimiter:=AValue;
  if assigned(FCSVParser) then
    FCSVParser.Delimiter:=FDelimiter;
end;

procedure TFileImport.GuessDelimiter();
var
  Byte: char;
  FoundDelims: string;
begin
  FDelimiter := ',';

  try
    if not(assigned(FCSVStream)) then
      raise Exception.Create('ReadFirstLineFields: CSVStream must exist.');

    // Only read 1st line to avoid this becoming EXTREMELY slow for large files
    // as everything will need to be loaded into memory.
    FCSVStream.Position:=0;
    Byte:=#0;
    FoundDelims:='';
    try
      while (Byte<>#13) and (Byte<>#10) do
      begin
        Byte:=Char(FCSVStream.ReadByte);
        if (Byte in [#9, ';', '|', ',']) and (pos(byte,FoundDelims)=0) then
          FoundDelims:=FoundDelims+Byte;
      end;
    except
      // end of file etc
    end;

    // Only replace existing delimiter if it is invalid
    if FoundDelims<>'' then
    begin
      if (FDelimiter='') or
        (pos(FDelimiter,FoundDelims)=0) then
        FDelimiter:=FoundDelims[1];
    end;
  except
    // File access error etc
  end;
end;

procedure TFileImport.SetSourceFile(AValue: string);
begin
  if FSourceFile=AValue then Exit;
  FSourceFile:=AValue;
  if assigned(FCSVParser) then
    FreeAndNil(FCSVParser);
  if assigned(FCSVStream) then
    FreeAndNil(FCSVStream);
  //if AValue<>'' then
    //FCSVStream:=TFileStream.Create(FSourceFile,fmOpenRead and fmShareDenyNoneFlags);
  // We do not create the csv parser as we want to do things with the stream first
  FRowCount:=-1; //invalidate cache
  FSourceFields.Clear;
  DeleteMapping(-1);
  if AValue<>'' then
    GuessDelimiter(); //process delimiters etc
end;

end.

