unit uQuestionInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  xQuestionInfo;

type
  TfQuestionInfo = class(TForm)
    pgcntrl1: TPageControl;
    tbsht1: TTabSheet;
    tbsht2: TTabSheet;
    pnlBottom: TPanel;
    lblQuestionName: TLabel;
    lblQuestionDescribe: TLabel;
    lblQuestionRemark: TLabel;
    edtQuestionName: TEdit;
    mmoQuestionDescribe: TMemo;
    mmoQuestionRemark: TMemo;
    pnlBR: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }


  protected
    FInfo : TQuestionInfo;

  public
    { Public declarations }

    /// <summary>
    /// 显示信息
    /// </summary>
    procedure ShowInfo(AInfo : TQuestionInfo); virtual;

    /// <summary>
    /// 保存信息
    /// </summary>
    procedure SaveInfo; virtual;


  end;

var
  fQuestionInfo: TfQuestionInfo;

implementation

{$R *.dfm}

procedure TfQuestionInfo.SaveInfo;
begin
  if Assigned(FInfo) then
  begin
    FInfo.QName := edtQuestionName.Text;
    FInfo.QDescribe := mmoQuestionDescribe.Text;
    FInfo.QRemark1 := mmoQuestionRemark.Text;
  end;
end;

procedure TfQuestionInfo.ShowInfo(AInfo: TQuestionInfo);
begin
  if Assigned(AInfo) then
  begin
    FInfo := AInfo;

    edtQuestionName.Text := FInfo.QName;
    mmoQuestionDescribe.Text := FInfo.QDescribe;
    mmoQuestionRemark.Text := FInfo.QRemark1;
  end;
end;

end.
