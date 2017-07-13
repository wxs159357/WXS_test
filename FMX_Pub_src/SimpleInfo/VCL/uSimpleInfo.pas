unit uSimpleInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, xSimpleInfoControl;

type
  TfSimpleInfo = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    pnl1: TPanel;
    btn1: TButton;
    btn2: TButton;
    bvl1: TBevel;
    edtSIName: TEdit;
    mmoSIRemark: TMemo;
  private
    { Private declarations }
    FInfo : TSimpleInfo;
  public
    { Public declarations }
    procedure ShowInfo(AInfo : TSimpleInfo);
    procedure SaveInfo;

  end;

var
  fSimpleInfo: TfSimpleInfo;

implementation

{$R *.dfm}

{ TfSimpleInfo }

procedure TfSimpleInfo.SaveInfo;
begin
  if Assigned(FInfo) then
  begin
    FInfo.SIName    := edtSIName.Text;
    FInfo.SIRemark1 := mmoSIRemark.Lines.Text;
  end;
end;

procedure TfSimpleInfo.ShowInfo(AInfo: TSimpleInfo);
begin
  FInfo := AInfo;

  if Assigned(FInfo) then
  begin
    edtSIName.Text         := FInfo.SIName;
    mmoSIRemark.Lines.Text := FInfo.SIRemark1;
  end;
end;

end.
