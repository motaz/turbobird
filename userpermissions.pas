unit UserPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids;

type

  { TfmUserPermissions }

  TfmUserPermissions = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmUserPermissions: TfmUserPermissions;

implementation

{ TfmUserPermissions }

procedure TfmUserPermissions.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I userpermissions.lrs}

end.

