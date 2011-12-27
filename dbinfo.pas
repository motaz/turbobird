unit dbInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfmDBInfo }

  TfmDBInfo = class(TForm)
    edCreationDate: TEdit;
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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmDBInfo: TfmDBInfo;

implementation

{$R *.lfm}

end.

