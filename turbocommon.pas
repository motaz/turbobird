unit turbocommon;

{ Non-GUI common code for TurboBird }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

const
  // Some field types used in e.g. RDB$FIELDS
  // todo: (low priority) perhaps move to enumeration with fixed constant values
  BlobType = 261;
  CharType = 14;
  CStringType = 40; // probably null-terminated string used for UDFs
  VarCharType = 37;

  // Available character set encodings for Firebird.
  // Update this whenever Firebird supports new character sets
  DefaultFBCharacterSet=42; //Used for GUI controls etc. UTF8 in CharacterSets below.
  FBCharacterSets: array[1..52] of string =
    ('NONE',
    'ASCII',
    'BIG_5',
    'CP943C',
    'CYRL',
    'DOS437',
    'DOS737',
    'DOS775',
    'DOS850',
    'DOS852',
    'DOS857',
    'DOS858',
    'DOS860',
    'DOS861',
    'DOS862',
    'DOS863',
    'DOS864',
    'DOS865',
    'DOS866',
    'DOS869',
    'EUCJ_0208',
    'GB18030',
    'GBK',
    'GB_2312',
    'ISO8859_1',
    'ISO8859_13',
    'ISO8859_2',
    'ISO8859_3',
    'ISO8859_4',
    'ISO8859_5',
    'ISO8859_6',
    'ISO8859_7',
    'ISO8859_8',
    'ISO8859_9',
    'KOI8R',
    'KOI8U',
    'KSC_5601',
    'NEXT',
    'OCTETS',
    'SJIS_0208',
    'TIS620',
    'UNICODE_FSS', //obsolete
    'UTF8', //good default
    'WIN1250',
    'WIN1251',
    'WIN1252',
    'WIN1253',
    'WIN1254',
    'WIN1255',
    'WIN1256',
    'WIN1257',
    'WIN1258');

// Given field retrieval query in FieldQuery, return field type and size.
// Includes support for field types that are domains and arrays
procedure GetFieldType(FieldQuery: TSQLQuery; var FieldType: string; var FieldSize: integer);

// Returns field type DDL given a RDB$FIELD_TYPE value as well
// as subtype/length/scale (use -1 for empty/unknown values)
function GetFBTypeName(Index: Integer;
  SubType: integer=-1; FieldLength: integer=-1; Precision: integer=-1;
  Scale: integer=-1): string;

// Tries to guess if an RDB$RELATION_FIELDS.RDB$FIELD_SOURCE domain name for a column is system-generated.
function IsFieldDomainSystemGenerated(FieldSource: string): boolean;

// Given TIBConnection parameters, sets transaction isolation level
procedure SetTransactionIsolation(Params: TStringList);

implementation

procedure SetTransactionIsolation(Params: TStringList);
begin
  Params.Clear;
  Params.Add('isc_tpb_read_commited');
  Params.Add('isc_tpb_concurrency');
  Params.Add('isc_tpb_nowait');
end;

procedure GetFieldType(FieldQuery: TSQLQuery; var FieldType: string; var FieldSize: integer);
// Requires FieldQuery to be the correct field retrieval query.
// todo: migrate field retrieval query to turbocommon
begin
  FieldType:= '';
  FieldSize:= 0;

  if (FieldQuery.FieldByName('field_source').IsNull) or
    (trim(FieldQuery.FieldByName('field_source').AsString)='') or
    (IsFieldDomainSystemGenerated(trim(FieldQuery.FieldByname('field_source').AsString))) then
  begin
    // Field type is not based on a domain but a standard SQL type
    FieldType:= GetFBTypeName(FieldQuery.FieldByName('field_type_int').AsInteger,
      FieldQuery.FieldByName('field_sub_type').AsInteger,
      FieldQuery.FieldByName('field_length').AsInteger,
      FieldQuery.FieldByName('field_precision').AsInteger,
      FieldQuery.FieldByName('field_scale').AsInteger);
    // Array should really be [lowerbound:upperbound] (if dimension is 0)
    // but for now don't bother as arrays are not supported anyway
    // Assume 0 dimension, 1 lower bound; just fill in upper bound
    if not(FieldQuery.FieldByName('array_upper_bound').IsNull) then
      FieldType := FieldType +
        ' [' +
        FieldQuery.FieldByName('array_upper_bound').AsString +
        ']';
    if FieldQuery.FieldByName('field_type_int').AsInteger = VarCharType then
      FieldSize:= FieldQuery.FieldByName('characterlength').AsInteger
    else
      FieldSize:= FieldQuery.FieldByName('field_length').AsInteger;
  end
  else
  begin
    // Field is based on a domain
    FieldType:= trim(FieldQuery.FieldByName('field_source').AsString);
  end;
end;

(**************  Get Firebird Type name  *****************)

function GetFBTypeName(Index: Integer;
  SubType: integer=-1; FieldLength: integer=-1;
  Precision: integer=-1; Scale: integer=-1
  ): string;
begin
  //todo: (low priority) add Firebird 3.0 beta BOOLEAN datatype number
  case Index of
    // See also
    // http://firebirdsql.org/manual/migration-mssql-data-types.html
    // http://stackoverflow.com/questions/12070162/how-can-i-get-the-table-description-fields-and-types-from-firebird-with-dbexpr
    BlobType : Result:= 'BLOB';
    14 : Result:= 'CHAR';
    CStringType : Result:= 'CSTRING'; // probably null-terminated string used for UDFs
    12 : Result:= 'DATE';
    11 : Result:= 'D_FLOAT';
    16 : Result:= 'BIGINT'; // Further processed below
    27 : Result:= 'DOUBLE PRECISION';
    10 : Result:= 'FLOAT';
    8  : Result:= 'INTEGER'; // further processed below
    9  : Result:= 'QUAD'; // ancient VMS 64 bit datatype; see also IB6 Language Reference RDB$FIELD_TYPE
    7  : Result:= 'SMALLINT'; // further processed below
    13 : Result:= 'TIME';
    35 : Result:= 'TIMESTAMP';
    VarCharType : Result:= 'VARCHAR';
  else
    Result:= 'Unknown Type';
  end;
  // Subtypes for numeric types
  if Index in [7, 8, 16] then
  begin
    if SubType = 0 then {integer}
    begin
      case Index of
        7: Result:= 'SMALLINT';
        8: Result:= 'INTEGER';
        16: Result:= 'BIGINT';
      end;
    end
    else
    begin
      // Numeric/decimal: use precision/scale
      if SubType = 1 then
        Result:= 'Numeric('
      else
      if SubType = 2 then
        Result:= 'Decimal(';

      if Precision=-1 then {sensible default}
        Result:= Result + '2,'
      else
        Result:= Result + IntToStr(Precision)+',';
      Result:= Result + IntToStr(Abs(Scale)) + ') ';
    end;
  end;
end;

function IsFieldDomainSystemGenerated(FieldSource: string): boolean;
begin
  // Unfortunately there does not seem to be a way to search the system tables to find out
  // if the constraint name is system-generated
  result:=(pos('RDB$',uppercase(Trim(FieldSource)))=1);
end;


end.

