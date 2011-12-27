unit ViewSProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SynEdit, SynHighlighterSQL;

type

  { TfmViewSProc }

  TfmViewSProc = class(TForm)
    edName: TEdit;
    edOwner: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    seScript: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewSProc: TfmViewSProc;

implementation

{ TfmViewSProc }

procedure TfmViewSProc.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I viewsproc.lrs}

end.

