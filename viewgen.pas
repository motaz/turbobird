unit ViewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfmViewGen }

  TfmViewGen = class(TForm)
    edGenName: TEdit;
    edValue: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewGen: TfmViewGen;

implementation

{ TfmViewGen }

procedure TfmViewGen.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I viewgen.lrs}

end.

