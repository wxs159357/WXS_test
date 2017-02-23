unit FrmAnalysisMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, U_POWER_ANALYSIS, U_WIRING_ERROR,
  U_POWER_LIST_INFO, U_WE_PHASE_MAP, U_WE_EQUATION, StdCtrls, U_DIAGRAM_TYPE, FrmErrorInfo;

type
  TfAnalysisMap = class(TForm)
    tbcntrl1: TTabControl;
    stsbrMain: TStatusBar;
    imgMap: TImage;
    btn1: TButton;
    pnl1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbcntrl1Change(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FPhaseDraw : TWE_PHASE_MAP;
    FEquation : TWE_EQUATION;
    FPowerAnalysis : TPowerAnalysis;
    FErrorList : TStringList;
    FError : TWIRING_ERROR;
  public
    { Public declarations }

    /// <summary>
    /// 接线图类型
    /// </summary>
    ADiagramType : TDiagramType;

    /// <summary>
    /// 显示功率信息
    /// </summary>
    procedure ShowPowerInfo(APower: TFourPower; dAngle : Double); overload;
    procedure ShowPowerInfo(APower: TThreePower; dAngle : Double); overload;
    procedure ShowPowerInfoAT(APower: TObject; dAngle : Double); 
  end;

//var
//  fAnalysisMap: TfAnalysisMap;

implementation

{$R *.dfm}

procedure TfAnalysisMap.btn1Click(Sender: TObject);
var
  AErrorInfo : TfErrorInfo;
  AThreePower : TThreePower;
  AFourPower : TFourPower;
begin
  if tbcntrl1.Tabs.Count > 0 then
  begin
    AErrorInfo := TfErrorInfo.Create(nil);
    AErrorInfo.ADiagramType := ADiagramType;

    if FErrorList.Objects[tbcntrl1.TabIndex] is TThreePower then
    begin
      AThreePower := TThreePower(FErrorList.Objects[tbcntrl1.TabIndex]);
      AErrorInfo.LoadEquation( FError, AThreePower.Angle );
    end
    else
    begin
      AFourPower := TFourPower(FErrorList.Objects[tbcntrl1.TabIndex]);
      AErrorInfo.LoadEquation( FError, AFourPower.Angle );
    end;

    AErrorInfo.ShowModal;
    AErrorInfo.Free;
  end;
end;

procedure TfAnalysisMap.FormCreate(Sender: TObject);
begin
  FPowerAnalysis := TPowerAnalysis.Create;
  FErrorList := TStringList.Create;
  FError := TWIRING_ERROR.Create;
  FPhaseDraw := TWE_PHASE_MAP.Create( nil );
  FEquation := TWE_EQUATION.Create;
end;

procedure TfAnalysisMap.FormDestroy(Sender: TObject);
begin
  FPowerAnalysis.Free;
  FErrorList.Free;
  FError.Free;
  FPhaseDraw.Free;
  FEquation.Free;
end;

procedure TfAnalysisMap.ShowPowerInfo(APower: TFourPower; dAngle : Double);
begin
  if not Assigned(APower) then
    Exit;

  FPowerAnalysis.AnalysisPower(APower, FErrorList, dAngle);

  tbcntrl1.Tabs.Text := FErrorList.Text;

  if tbcntrl1.Tabs.Count > 0 then
    tbcntrl1Change(nil);
end;

procedure TfAnalysisMap.ShowPowerInfo(APower: TThreePower; dAngle : Double);
begin
  if not Assigned(APower) then
    Exit;

  FPowerAnalysis.AnalysisPower(APower, FErrorList, dAngle);

  tbcntrl1.Tabs.Text := FErrorList.Text;
  if tbcntrl1.Tabs.Count > 0 then
    tbcntrl1Change(nil);
end;

procedure TfAnalysisMap.ShowPowerInfoAT(APower: TObject; dAngle: Double);
begin
  if Assigned(APower) then
  begin
    if APower is TThreePower then
      ShowPowerInfo(TThreePower(APower), dAngle)
    else if APower is TFourPower then
      ShowPowerInfo(TFourPower(APower), dAngle);
  end;
end;

procedure TfAnalysisMap.tbcntrl1Change(Sender: TObject);
var
  nErrorID : Integer;

  AThreePower : TThreePower;
  AFourPower : TFourPower;

begin
  if FErrorList.Objects[tbcntrl1.TabIndex] is TThreePower then
  begin
    AThreePower := TThreePower(FErrorList.Objects[tbcntrl1.TabIndex]);
    TryStrToInt('$' + AThreePower.Errorcode, nErrorID);
    FError.ID := nErrorID;
    stsbrMain.Panels.Items[0].Text := '错误个数：' + IntToStr(AThreePower.Errorcount);
    stsbrMain.Panels.Items[1].Text := 'φ角：' + FloatToStr(AThreePower.Angle);
    stsbrMain.Panels.Items[2].Text := FError.Description;
    FEquation.GenerateEquations( FError, AThreePower.Angle );

    FPhaseDraw.Canvas := imgMap.Canvas;
    FPhaseDraw.Rect := Rect( 2, 2, 2 + 230, 2 + 180);
    FPhaseDraw.DrawPhaseMap( FEquation, '' );
  end
  else
  begin
    AFourPower := TFourPower(FErrorList.Objects[tbcntrl1.TabIndex]);
    TryStrToInt('$' + AFourPower.Errorcode, nErrorID);
    FError.ID := nErrorID;
    stsbrMain.Panels.Items[0].Text := '错误个数：' + IntToStr(AFourPower.Errorcount);
    stsbrMain.Panels.Items[1].Text := 'φ角：' + FloatToStr(AFourPower.Angle);
    stsbrMain.Panels.Items[2].Text := FError.Description;
    FEquation.GenerateEquations( FError, AFourPower.Angle );

    FPhaseDraw.Canvas := imgMap.Canvas;
    FPhaseDraw.Rect := Rect( 2, 2, 2 + 230, 2 + 180);
    FPhaseDraw.DrawPhaseMap( FEquation, '' );
  end;
end;

end.

