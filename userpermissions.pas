unit UserPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls;

type

  { TfmUserPermissions }

  TfmUserPermissions = class(TForm)
    bbClose: TBitBtn;
    Label1: TLabel;
    laObject: TLabel;
    StringGrid1: TStringGrid;
    procedure bbCloseClick(Sender: TObject);
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

procedure TfmUserPermissions.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I userpermissions.lrs}

end.

