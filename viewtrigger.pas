unit ViewTrigger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, LCLType, IniFiles;

type

  { TfmViewTrigger }

  TfmViewTrigger = class(TForm)
      bbClose: TSpeedButton;
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
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TfmViewTrigger.FormCreate(Sender: TObject);
var
   configFile: TIniFile;
   configFilePath: String;
begin
    // Set the editor font from config.ini
    configFilePath:= ConcatPaths([ExtractFilePath(Application.ExeName), 'config.ini']);
    configFile:= TIniFile.Create(configFilePath);
    seScript.Font.Name:=configFile.ReadString('Editor Font', 'font_name', 'Monospace');
    seScript.Font.Size:=configFile.ReadInteger('Editor Font', 'font_size', 11);
    configFile.Free;
end;

procedure TfmViewTrigger.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmViewTrigger.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewtrigger.lrs}

end.

