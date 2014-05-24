unit turbocommon;

{ Non-GUI common code for TurboBird that do not depend on a database connection.
SysTables covers functionality for which a db connection is required. }
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
  // Available character sets as per Firebird 2.5
  FBCharacterSets: array[0..51] of string =
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
  // Available collations as per Firebird 2.5
  // Pairs of collation names and the character set name
  // that must be used to support this collation
  FBCollations: array[0..148-1,0..1] of string =
    (
    ('ASCII','ASCII'),
    ('BIG_5','BIG_5'),
    ('BS_BA','WIN1250'),
    ('CP943C','CP943C'),
    ('CP943C_UNICODE','CP943C'),
    ('CS_CZ','ISO8859_2'),
    ('CYRL','CYRL'),
    ('DA_DA','ISO8859_1'),
    ('DB_CSY','DOS852'),
    ('DB_DAN865','DOS865'),
    ('DB_DEU437','DOS437'),
    ('DB_DEU850','DOS850'),
    ('DB_ESP437','DOS437'),
    ('DB_ESP850','DOS850'),
    ('DB_FIN437','DOS437'),
    ('DB_FRA437','DOS437'),
    ('DB_FRA850','DOS850'),
    ('DB_FRC850','DOS850'),
    ('DB_FRC863','DOS863'),
    ('DB_ITA437','DOS437'),
    ('DB_ITA850','DOS850'),
    ('DB_NLD437','DOS437'),
    ('DB_NLD850','DOS850'),
    ('DB_NOR865','DOS865'),
    ('DB_PLK','DOS852'),
    ('DB_PTB850','DOS850'),
    ('DB_PTG860','DOS860'),
    ('DB_RUS','CYRL'),
    ('DB_SLO','DOS852'),
    ('DB_SVE437','DOS437'),
    ('DB_SVE850','DOS850'),
    ('DB_TRK','DOS857'),
    ('DB_UK437','DOS437'),
    ('DB_UK850','DOS850'),
    ('DB_US437','DOS437'),
    ('DB_US850','DOS850'),
    ('DE_DE','ISO8859_1'),
    ('DOS437','DOS437'),
    ('DOS737','DOS737'),
    ('DOS775','DOS775'),
    ('DOS850','DOS850'),
    ('DOS852','DOS852'),
    ('DOS857','DOS857'),
    ('DOS858','DOS858'),
    ('DOS860','DOS860'),
    ('DOS861','DOS861'),
    ('DOS862','DOS862'),
    ('DOS863','DOS863'),
    ('DOS864','DOS864'),
    ('DOS865','DOS865'),
    ('DOS866','DOS866'),
    ('DOS869','DOS869'),
    ('DU_NL','ISO8859_1'),
    ('EN_UK','ISO8859_1'),
    ('EN_US','ISO8859_1'),
    ('ES_ES','ISO8859_1'),
    ('ES_ES_CI_AI','ISO8859_1'),
    ('EUCJ_0208','EUCJ_0208'),
    ('FI_FI','ISO8859_1'),
    ('FR_CA','ISO8859_1'),
    ('FR_FR','ISO8859_1'),
    ('FR_FR_CI_AI','ISO8859_1'),
    ('GB18030','GB18030'),
    ('GB18030_UNICODE','GB18030'),
    ('GBK','GBK'),
    ('GBK_UNICODE','GBK'),
    ('GB_2312','GB_2312'),
    ('ISO8859_1','ISO8859_1'),
    ('ISO8859_13','ISO8859_13'),
    ('ISO8859_2','ISO8859_2'),
    ('ISO8859_3','ISO8859_3'),
    ('ISO8859_4','ISO8859_4'),
    ('ISO8859_5','ISO8859_5'),
    ('ISO8859_6','ISO8859_6'),
    ('ISO8859_7','ISO8859_7'),
    ('ISO8859_8','ISO8859_8'),
    ('ISO8859_9','ISO8859_9'),
    ('ISO_HUN','ISO8859_2'),
    ('ISO_PLK','ISO8859_2'),
    ('IS_IS','ISO8859_1'),
    ('IT_IT','ISO8859_1'),
    ('KOI8R','KOI8R'),
    ('KOI8R_RU','KOI8R'),
    ('KOI8U','KOI8U'),
    ('KOI8U_UA','KOI8U'),
    ('KSC_5601','KSC_5601'),
    ('KSC_DICTIONARY','KSC_5601'),
    ('LT_LT','ISO8859_13'),
    ('NEXT','NEXT'),
    ('NONE','NONE'),
    ('NO_NO','ISO8859_1'),
    ('NXT_DEU','NEXT'),
    ('NXT_ESP','NEXT'),
    ('NXT_FRA','NEXT'),
    ('NXT_ITA','NEXT'),
    ('NXT_US','NEXT'),
    ('OCTETS','OCTETS'),
    ('PDOX_ASCII','DOS437'),
    ('PDOX_CSY','DOS852'),
    ('PDOX_CYRL','CYRL'),
    ('PDOX_HUN','DOS852'),
    ('PDOX_INTL','DOS437'),
    ('PDOX_ISL','DOS861'),
    ('PDOX_NORDAN4','DOS865'),
    ('PDOX_PLK','DOS852'),
    ('PDOX_SLO','DOS852'),
    ('PDOX_SWEDFIN','DOS437'),
    ('PT_BR','ISO8859_1'),
    ('PT_PT','ISO8859_1'),
    ('PXW_CSY','WIN1250'),
    ('PXW_CYRL','WIN1251'),
    ('PXW_GREEK','WIN1253'),
    ('PXW_HUN','WIN1250'),
    ('PXW_HUNDC','WIN1250'),
    ('PXW_INTL','WIN1252'),
    ('PXW_INTL850','WIN1252'),
    ('PXW_NORDAN4','WIN1252'),
    ('PXW_PLK','WIN1250'),
    ('PXW_SLOV','WIN1250'),
    ('PXW_SPAN','WIN1252'),
    ('PXW_SWEDFIN','WIN1252'),
    ('PXW_TURK','WIN1254'),
    ('SJIS_0208','SJIS_0208'),
    ('SV_SV','ISO8859_1'),
    ('TIS620','TIS620'),
    ('TIS620_UNICODE','TIS620'),
    ('UCS_BASIC','UTF8'),
    ('UNICODE','UTF8'),
    ('UNICODE_CI','UTF8'),
    ('UNICODE_CI_AI','UTF8'),
    ('UNICODE_FSS','UNICODE_FSS'),
    ('UTF8','UTF8'),
    ('WIN1250','WIN1250'),
    ('WIN1251','WIN1251'),
    ('WIN1251_UA','WIN1251'),
    ('WIN1252','WIN1252'),
    ('WIN1253','WIN1253'),
    ('WIN1254','WIN1254'),
    ('WIN1255','WIN1255'),
    ('WIN1256','WIN1256'),
    ('WIN1257','WIN1257'),
    ('WIN1257_EE','WIN1257'),
    ('WIN1257_LT','WIN1257'),
    ('WIN1257_LV','WIN1257'),
    ('WIN1258','WIN1258'),
    ('WIN_CZ','WIN1250'),
    ('WIN_CZ_CI_AI','WIN1250'),
    ('WIN_PTBR','WIN1252')
    );

// Retrieve available collations for specified Characterset into Collations
function GetCollations(const Characterset: string; var Collations: TStringList): boolean;

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

function GetCollations(const Characterset: string; var Collations: TStringList): boolean;
var
  i: integer;
begin
  result:= false;
  Collations.Clear;
  Collations.BeginUpdate;
  for i:= low(FBCollations) to high(FBCollations) do
  begin
    if FBCollations[i,1]=Characterset then
    begin
      Collations.Add(FBCollations[i,0]);
    end;
  end;
  Collations.EndUpdate;
  result:= true;
end;

procedure SetTransactionIsolation(Params: TStringList);
begin
  Params.Clear;
  Params.Add('isc_tpb_read_commited');
  Params.Add('isc_tpb_concurrency');
  Params.Add('isc_tpb_nowait');
end;

procedure GetFieldType(FieldQuery: TSQLQuery; var FieldType: string; var FieldSize: integer);
// Requires FieldQuery to be the correct field retrieval query.
// todo: migrate field retrieval query to systables if not already done
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

