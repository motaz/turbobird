unit turbocommon;

{ Non-GUI common code for TurboBird }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

end.

