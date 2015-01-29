unit Calen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Calendar, Buttons, ExtCtrls;

type

  { TfmCalen }

  TfmCalen = class(TForm)
    BitBtn1: TBitBtn;
    bbOk: TBitBtn;
    BitBtn3: TBitBtn;
    Calendar1: TCalendar;
    edTime: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure bbOkClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    DateTimeValue: TDateTime;
    { public declarations }
  end; 

var
  fmCalen: TfmCalen;

implementation

{ TfmCalen }

procedure TfmCalen.BitBtn1Click(Sender: TObject);
begin
  edTime.Text:= TimeToStr(Time);
  Calendar1.DateTime:= Date;
end;

procedure TfmCalen.FormActivate(Sender: TObject);
begin
  edTime.Text:= TimeToStr(DateTimeValue);
  Calendar1.DateTime:= DateTimeValue;
end;

procedure TfmCalen.FormCreate(Sender: TObject);
begin

end;

procedure TfmCalen.bbOkClick(Sender: TObject);
begin
  DateTimeValue:= StrToTime(edTime.Text) + Trunc(Calendar1.DateTime);
end;

initialization
  {$I calen.lrs}

end.

