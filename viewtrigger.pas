unit ViewTrigger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SynEdit, SynHighlighterSQL;

type

  { TfmViewTrigger }

  TfmViewTrigger = class(TForm)
    edName: TEdit;
    edOnTable: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    laPos: TLabel;
    laEvent: TLabel;
    laType: TLabel;
    laEnabled: TLabel;
    seScript: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewTrigger: TfmViewTrigger;

implementation

{ TfmViewTrigger }

procedure TfmViewTrigger.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

initialization
  {$I viewtrigger.lrs}

end.

