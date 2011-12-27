unit UDFInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfmUDFInfo }

  TfmUDFInfo = class(TForm)
    edModule: TEdit;
    edEntry: TEdit;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    meBody: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmUDFInfo: TfmUDFInfo;

implementation

{ TfmUDFInfo }

procedure TfmUDFInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I udfinfo.lrs}

end.

