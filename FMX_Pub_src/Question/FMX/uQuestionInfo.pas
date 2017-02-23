unit uQuestionInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, xQuestionInfo,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.TabControl;

type
  TfQuestionInfo = class(TForm)
    pnl3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    tbcntrl1: TTabControl;
    tbtm1: TTabItem;
    tbtm2: TTabItem;
    edtQuestionName: TEdit;
    lbl3: TLabel;
    mmoQuestionDescribe: TMemo;
    mmoQuestionRemark: TMemo;
    lbl2: TLabel;
    lbl1: TLabel;
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

{$R *.fmx}

{ TfQuestionInfo }

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
