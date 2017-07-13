unit uSImpleInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, xSimpleInfoControl,
  FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects;

type
  TfSimpleInfo = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    edtSIName: TEdit;
    mmoSIRemark: TMemo;
    lyt1: TLayout;
    ln1: TLine;
    btn1: TButton;
    btn2: TButton;
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

{$R *.fmx}

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
