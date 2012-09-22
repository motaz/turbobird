unit dbInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfmDBInfo }

  TfmDBInfo = class(TForm)
    bbClose: TBitBtn;
    bbRefresh: TBitBtn;
    edCreationDate: TEdit;
    edConnections: TEdit;
    edPageSize: TEdit;
    edDBSize: TEdit;
    edName: TEdit;
    edODSVer: TEdit;
    edCharset: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    meClients: TMemo;
    procedure bbCloseClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmDBInfo: TfmDBInfo;

implementation

{$R *.lfm}

{ TfmDBInfo }

uses Main;

procedure TfmDBInfo.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmDBInfo.bbRefreshClick(Sender: TObject);
begin
  fmMain.lmDBIndoClick(nil);
end;

procedure TfmDBInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;


end.

