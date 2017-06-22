unit FrmQInfoC;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  uQuestionInfo, FrmErrorSelect, xQuestionInfo, xWiringError, FrmWEDetails2,
  U_WIRING_ERROR, U_ERRORF_TO_WIRING, U_DIAGRAM_TYPE;

type
  TfQInfo = class(TfQuestionInfo)
    pnl1: TPanel;
    pnl2: TPanel;
    spltr1: TSplitter;
    tbcntrl1: TTabControl;
    chkIsElec: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbcntrl1Change(Sender: TObject);
    procedure chkIsElecClick(Sender: TObject);
  private
    { Private declarations }
    FFormESelect : TfErrorSelect;
    FFormDetail : TfWEDetails2;
    FError : TWIRINGF_ERROR;
    FWError : TWIRING_ERROR;

    /// <summary>
    /// 是否带电描述
    /// </summary>
    function GetIsElecStr : string;

    /// <summary>
    /// 考题改变
    /// </summary>
    procedure ErrorChange(Sender: TObject);
  public
    { Public declarations }
    /// <summary>
    /// 显示信息
    /// </summary>
    procedure ShowInfo(AInfo : TQuestionInfo); override;

    procedure ShowInfoReadOnly(AInfo : TQuestionInfo);

    /// <summary>
    /// 保存信息
    /// </summary>
    procedure SaveInfo; override;

  end;

var
  fQInfo: TfQInfo;

implementation

{$R *.dfm}

procedure TfQInfo.chkIsElecClick(Sender: TObject);
begin
  inherited;
  mmoQuestionDescribe.Text := FError.GetDescription + GetIsElecStr;
end;

procedure TfQInfo.ErrorChange(Sender: TObject);
begin
  ErrorFToWiring(FError, FWError);

  mmoQuestionDescribe.Text := FError.GetDescription + GetIsElecStr;
  FFormDetail.LoadEquation(FWError, 20);
end;

procedure TfQInfo.FormCreate(Sender: TObject);
begin
  inherited;
  FError := TWIRINGF_ERROR.Create;
  FWError := TWIRING_ERROR.Create;
  spltr1.Parent := tbsht2;
  pnl1.Parent := tbsht2;
  pnl2.Parent := tbsht2;
  FFormESelect := TfErrorSelect.Create(nil);
  FFormESelect.Parent := pnl1;
  FFormESelect.Align := alClient;
  FFormESelect.BorderStyle := bsNone;
  FFormESelect.OnChange := ErrorChange;

  FFormDetail := TfWEDetails2.Create(nil);
  FFormDetail.Parent := pnl2;
  FFormDetail.Align := alClient;
  FFormDetail.BorderStyle := bsNone;


end;

procedure TfQInfo.FormDestroy(Sender: TObject);
begin
  FFormESelect.free;
  FFormDetail.Free;
  FError.Free;
  FWError.free;
end;

procedure TfQInfo.FormShow(Sender: TObject);
begin
  FFormESelect.Show;
  FFormDetail.Show;
end;

function TfQInfo.GetIsElecStr: string;
begin
  if chkIsElec.Checked then
  begin
    Result := '表箱外壳带电;';
  end
  else
  begin
    Result := '';
  end;
end;

procedure TfQInfo.SaveInfo;
begin
  inherited;
  FInfo.QCode := FError.ID;
  if chkIsElec.Checked then
  begin
    FInfo.QRemark2 := '1';
  end
  else
  begin
    FInfo.QRemark2 := '0';
  end;
end;

procedure TfQInfo.ShowInfo(AInfo: TQuestionInfo);
begin
  inherited;
  FError.ID := AInfo.QCode;
  chkIsElec.Checked := AInfo.QRemark2 = '1';
  if FError.PhaseType = ptfThree then
  begin
    tbcntrl1.TabIndex := 0;
    case FError.DiagramType of
      2 : FFormDetail.ADiagramType := dt3M;
      3 : FFormDetail.ADiagramType := dt3L4;
      4 : FFormDetail.ADiagramType := dt3CTClear;
    else
      FFormDetail.ADiagramType := dt3M;
    end;
  end
  else
  begin
    tbcntrl1.TabIndex := 1;
    case FError.DiagramType of
      1 : FFormDetail.ADiagramType := dt4Direct;
      2 : FFormDetail.ADiagramType := dt4M_PT;
      3 : FFormDetail.ADiagramType := dt4_PT_L6;
      4 : FFormDetail.ADiagramType := dt4_PT_CT_CLear;
    else
      FFormDetail.ADiagramType := dt4_PT_L6;
    end;
  end;

  FFormESelect.ShowInfo(FError);
  ErrorChange(FError);
end;

procedure TfQInfo.ShowInfoReadOnly(AInfo: TQuestionInfo);
begin
  tbcntrl1.Enabled := False;
  tbsht1.Enabled := False;
  pnl1.Enabled := False;
  ShowInfo(AInfo);
end;

procedure TfQInfo.tbcntrl1Change(Sender: TObject);
begin
  inherited;
  if tbcntrl1.TabIndex = 0 then
  begin
    FError.PhaseType := ptfThree;
  end
  else
  begin
    FError.PhaseType := ptfFour;
  end;
  FFormESelect.ShowInfo(FError);
  ErrorChange(FError);
end;

end.
