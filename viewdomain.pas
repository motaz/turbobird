unit ViewDomain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfmViewDomain }

  TfmViewDomain = class(TForm)
    edName: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    laDefault: TLabel;
    laType: TLabel;
    laSize: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewDomain: TfmViewDomain;

implementation

{ TfmViewDomain }

procedure TfmViewDomain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I viewdomain.lrs}

end.

