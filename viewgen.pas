unit ViewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfmViewGen }

  TfmViewGen = class(TForm)
    bbClose: TBitBtn;
    edGenName: TEdit;
    edValue: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    procedure bbCloseClick(Sender: TObject);
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

procedure TfmViewGen.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free
end;

initialization
  {$I viewgen.lrs}

end.

