unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, URIParser, LCLIntf;

const
  {$IFDEF LINUX}
  Target = 'Linux';
  {$ENDIF}

  {$IFDEF WIN32}
  Target = 'Win32';
  {$ENDIF}

  {$IFDEF Win64}
  Target = 'Win64';
  {$ENDIF}

  {$IFDEF MAC}
  Target = 'Mac';
  {$ENDIF}

  {$IFDEF BSD}
  Target = 'BSD';
  {$ENDIF}

type

  { TfmAbout }

  TfmAbout = class(TForm)
    BitBtn1: TBitBtn;
    Image2: TImage;
    Label2: TLabel;
    Label5: TLabel;
    laTarget: TLabel;
    laVersion: TLabel;
    Label4: TLabel;
    laWebSite: TLabel;
    Label6: TLabel;
    laVersionDate: TLabel;
    Shape1: TShape;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Label6Click(Sender: TObject);
    procedure laWebSiteClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure Init;
    { public declarations }
  end; 

var
  fmAbout: TfmAbout;

implementation

{ TfmAbout }

uses Main;

procedure TfmAbout.laWebSiteClick(Sender: TObject);
begin
  OpenURL(laWebSite.Caption);
end;

procedure TfmAbout.Init;
begin
  laVersion.Caption:= 'Beta Version ' + fmMain.Version;
  laVersionDate.Caption:= fmMain.VersionDate;
  laTarget.Caption:= 'Target : ' + Target;
end;

procedure TfmAbout.Label6Click(Sender: TObject);
begin
  OpenURL('http://lazarus.freepascal.org');
end;

procedure TfmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmAbout.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I about.lrs}

end.

