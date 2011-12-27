unit NewDomain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons;

type

  { TfmNewDomain }

  TfmNewDomain = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbType: TComboBox;
    edDefault: TEdit;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    seSize: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
    procedure Init;
  end; 

var
  fmNewDomain: TfmNewDomain;

implementation

{ TfmNewDomain }

procedure TfmNewDomain.Init;
begin
  edName.Clear;
  cbType.ItemIndex:= -1;
  edDefault.Clear;
  seSize.Value:= 0;
end;

initialization
  {$I newdomain.lrs}

end.

