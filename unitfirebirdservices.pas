{ Written by Zoran }

unit UnitFirebirdServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ibase60dyn, LazUTF8;

type

  EFBServiceError = class(Exception);

  { TFirebirdServices }

  TFirebirdServices = class(TObject)
  private
    FBackupFile: String;
    FDBName: String;
    FHostName: String;
    FPassword: String;
    FUserName: String;
    FVerboseOutput: Boolean;
    FPServiceHandle: pisc_svc_handle;
    FArrIStatus: array [0..19] of ISC_STATUS;

    procedure SetBkpName(const AValue: String);
    procedure SetDBName(const AValue: String);
    procedure SetHostName(const AValue: String);
    procedure SetPassword(const AValue: String);
    procedure SetUserName(const AValue: String);
    // Raise exception if we cannot attach to service
    procedure RaiseIfNotAttachedService(const S: String);
    // Run backup or restore depending on BR
    procedure StartBackupRestore(BR: Byte);
    procedure RaiseServiceErr;
  public
    constructor Create;
    destructor Destroy; override;
    function AttachService: Boolean;
    function DetachService: Boolean;

    // Runs a backup on a database
    procedure StartBackup;
    // Restores a backup file to database
    procedure StartRestore;
    function ServiceQuery(out S: String): Boolean; //Returns True if there is more output.
    // Runs sweep on database
    procedure StartSweep;
    function ServiceAttached: Boolean;

    // Name/IP address of host for database connection
    property HostName: String read FHostName write SetHostName;
    // Database name for connection
    property DBName: String read FDBName write SetDBName;
    // File that contains a Firebird .fbk file
    property BackupFile: String read FBackupFile write SetBkpName;
    // Username for database connection
    property UserName: String read FUserName write SetUserName;
    // Password for database connection
    property Password: String read FPassword write SetPassword;
    // If true, give verbose output when running services functions
    property VerboseOutput: Boolean read FVerboseOutput write FVerboseOutput;
  end;

implementation

{ TFirebirdServices }

procedure TFirebirdServices.SetDBName(const AValue: String);
begin
  if FDBName <> AValue then begin
    if ServiceAttached then
      raise EFBServiceError.Create('You cannot change database file name when service is attached!!!');

    FDBName := AValue;
  end;
end;

procedure TFirebirdServices.SetHostName(const AValue: String);
begin
  if FHostName <> AValue then begin
    if ServiceAttached then
      raise EFBServiceError.Create('You cannot change host name when service is already attached!!!');

    FHostName := AValue;
  end;
end;

procedure TFirebirdServices.SetBkpName(const AValue: String);
begin
  if FBackupFile <> AValue then begin
    if ServiceAttached then
      raise EFBServiceError.Create('You cannot change backup file name when service is attached!!!');

    FBackupFile := AValue;
  end;
end;

procedure TFirebirdServices.SetPassword(const AValue: String);
begin
  if FPassword <> AValue then
    FPassword := AValue;
end;

procedure TFirebirdServices.SetUserName(const AValue: String);
begin
  if FUserName <> AValue then begin
    if ServiceAttached then
      raise EFBServiceError.Create('You cannot change user when service is already attached!!!');

    FUserName := AValue;
  end;
end;

procedure TFirebirdServices.RaiseIfNotAttachedService(const S: String);
begin
  if not ServiceAttached then
    raise EFBServiceError.Create('Cannot start ' + S + ' process, as service is not attached!');
end;

procedure TFirebirdServices.StartBackupRestore(BR: Byte);
var
  W: Word;
  Msg: String;
  B: Byte;
  Buff: array [0..1023] of Char;

  N: Cardinal;
  S: String;
begin
  case BR of
    isc_action_svc_backup: Msg := 'BACKUP';
    isc_action_svc_restore: Msg := 'RESTORE';
  else
    raise EFBServiceError.CreateFmt('Invalid parameter value %d passed to StartBackupRestore',[BR]);
  end;

  RaiseIfNotAttachedService(Msg);

  S := Char(BR); // Either isc_action_svc_backup or isc_action_svc_restore

  S := S + Char(isc_spb_dbname); // this cluster describes db name.
  W := Length(FDBName);
  B := W mod 256;
  S := S + Char(B);
  B := W div 256;
  S := S + Char(B);
  S := S + FDBName;

  B := isc_spb_bkp_file; // this cluster describes backup file
  S := S + Char(B);
  W := Length(FBackupFile);
  B := W mod 256;
  S := S + Char(B);
  B := W div 256;
  S := S + Char(B);
  S := S + FBackupFile;

  if FVerboseOutput then begin
    B := isc_spb_verbose; // verbose output
    S := S + Char(B);
  end;

  if BR = isc_action_svc_restore then begin
    B := isc_spb_options; // options
    S := S + Char(B);
                          // followed now by for-byte bitmask
    N := isc_spb_res_replace; // this says replace db file if it exists.
    B := N mod 256;     // Now, the bytes order in number must be inverted!
    S := S + Char(B);
    N := N div 256;
    B := N mod 256;
    S := S + Char(B);
    N := N div 256;
    B := N mod 256;
    S := S + Char(B);
    N := N div 256;
    B := N mod 256;
    S := S + Char(B);
  end;

  W := Length(S);
  Buff := S;

  if isc_service_start(@FArrIStatus, @FPServiceHandle, nil, W, @Buff) <> 0 then
    RaiseServiceErr;
end;

procedure TFirebirdServices.RaiseServiceErr;
var
  Msg:  array [0..1023] of Char;
  Ps: PISC_STATUS;
begin
  Ps := @FArrIStatus;
  isc_interprete(@Msg, @Ps); // Firebird interpretes the error code and
                             // turns it into a human-readable error message.

  raise EFBServiceError.Create(
                   'Error: ' + IntToStr(FArrIStatus[1]) + LineEnding + Msg);
end;

function TFirebirdServices.ServiceQuery(out S: String): Boolean;
var
  S2: String;
  RequestBuff, ResultBuff: array [0..1023] of Char;
  W: Word;
  I: Integer;
begin
  S2 := Char(isc_info_svc_line);
  RequestBuff := S2;

  for I := Low(ResultBuff) to High(ResultBuff) do
    ResultBuff[I] := #0;

  isc_service_query(@FArrIStatus, @FPServiceHandle, nil, 0, nil,
                     Length(S2), @RequestBuff, Length(ResultBuff), @ResultBuff);

  Result := False;
  if ResultBuff[0] = Char(isc_info_svc_line) then begin
    W := Byte(ResultBuff[2]);
    W := 256 * W + Byte(ResultBuff[1]) + 2;
    if W > High(ResultBuff) then
      W := High(ResultBuff);

    S := '';
    I := 3;
    while I <= W do begin
      S := S + ResultBuff[I];
      Inc(I);
    end;
    Result := S > '';
  end;

end;

procedure TFirebirdServices.StartSweep;
var
  Msg, S: String;
  N: Cardinal;
  W: Word;
  B: Byte;
  Buff: array [0..1023] of Char;
begin
  Msg := 'SWEEP';
  RaiseIfNotAttachedService(Msg);

  S := Char(isc_action_svc_repair);
  S := S + Char(isc_spb_dbname);
  W := Length(FDBName);
  B := W mod 256;
  S := S + Char(B);
  B := W div 256;
  S := S + Char(B);
  S := S + FDBName;

  B := isc_spb_options;
  S := S + Char(B);

  N := isc_spb_rpr_sweep_db;
  B := N mod 256;
  S := S + Char(B);
  N := N div 256;
  B := N mod 256;
  S := S + Char(B);
  N := N div 256;
  B := N mod 256;
  S := S + Char(B);
  N := N div 256;
  B := N mod 256;
  S := S + Char(B);

  W := Length(S);

  Buff := S;

  if isc_service_start(@FArrIStatus, @FPServiceHandle, nil, W, @Buff) <> 0 then
    RaiseServiceErr;
end;

function TFirebirdServices.ServiceAttached: Boolean;
begin
  Result := FPServiceHandle <> nil;
end;

constructor TFirebirdServices.Create;
begin
  inherited Create;

  FPServiceHandle := nil;
  FVerboseOutput := True;
  FHostName := '';
  FBackupFile := '';
  FDBName := '';
  FUserName := '';
  FPassword := '';
end;

destructor TFirebirdServices.Destroy;
begin
  DetachService;
  inherited Destroy;
end;

function TFirebirdServices.AttachService: Boolean;
var
  S: String;
  ServiceName, Buff: array [0..255] of Char;

  W1, W2: Word;
  B: Byte;
begin
  if ServiceAttached then
    raise EFBServiceError.Create('Service already attached!!!');

  S := Trim(FHostName);
  if (Length(S) > 0) and (UTF8UpperCase(S) <> 'LOCALHOST') then
    S := FHostName + ':service_mgr'
  else
    S := 'service_mgr';

  W1 := Length(S);
  ServiceName := S;

  B := isc_spb_version;
  S := Char(B);
  B := isc_spb_current_version;
  S := S + Char(B);
  B := isc_spb_user_name;
  S := S + Char(B);
  B := Length(FUserName);
  S := S + Char(B) + FUserName;

  B := isc_spb_password;
  S := S + Char(B);
  B := Length(FPassword);
  S := S + Char(B) + FPassword;
  W2 := Length(S);

  Buff := S;

  Result := isc_service_attach(@FArrIStatus, W1, @ServiceName, @FPServiceHandle, W2, @Buff) = 0;

  if not Result then
    RaiseServiceErr;
end;

function TFirebirdServices.DetachService: Boolean;
begin
  Result := True;
  if FPServiceHandle <> nil then begin
    Result := isc_service_detach(@FArrIStatus, @FPServiceHandle) = 0;
    if not Result then
      RaiseServiceErr;

    FPServiceHandle := nil;
  end;
end;

procedure TFirebirdServices.StartBackup;
begin
  StartBackupRestore(isc_action_svc_backup);
end;

procedure TFirebirdServices.StartRestore;
begin
  StartBackupRestore(isc_action_svc_restore);
end;

initialization


finalization

end.

