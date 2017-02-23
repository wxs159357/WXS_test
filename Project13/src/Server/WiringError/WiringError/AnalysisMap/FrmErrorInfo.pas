unit FrmErrorInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, U_POWER_PHASE_MAP,U_WE_EQUATION, U_WE_PHASE_MAP,FrmWEDetails2,
  ByxEdit,  U_WIRING_ERROR, U_CKM_DEVICE, ExtCtrls, StdCtrls,
  U_POWER_ANALYSIS, U_POWER_LIST_INFO, ComCtrls, Buttons,
  U_PRINT_CONTROL, Math, U_DIAGRAM_TYPE;

type
  TfErrorInfo = class(TForm)
    pnl1: TPanel;
    lbl1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
//    FPower : TCKM_POWER;
//    FPowerStatus: TPOWER_STATUS;
    FWED : TfWEDetails2;
//    FPA : TPowerAnalysis;
//    FFourPower: TFourPower;
//    FThreePower: TThreePower;
//    FUIAngle : Double;
//    FErrorList : TStringList;
//
//    /// <summary>
//    /// 实时向量图
//    /// </summary>
//    PowerStatusDraw : TPOWER_PHASE_MAP;
  public
    { Public declarations }

    /// <summary>
    /// 接线图类型
    /// </summary>
    ADiagramType : TDiagramType;

    /// <summary>
    /// 显示错误接线信息
    /// </summary>
    procedure LoadEquation( AWiringError : TWIRING_ERROR; AAngle : Double ); overload;
  end;

var
  fErrorInfo: TfErrorInfo;

implementation

{$R *.dfm}

procedure TfErrorInfo.FormCreate(Sender: TObject);
begin
//  FErrorList := TStringList.Create;
//  FPower := TCKM_POWER.Create(nil);
  FWED := TfWEDetails2.Create(nil);
  FWED.Parent := Self;
  FWED.Align := alClient;

//  FPA := TPowerAnalysis.Create;
//  FFourPower:= TFourPower.Create;
//  FThreePower:= TThreePower.Create;

//  FPowerStatus:= TPOWER_STATUS.Create;
//  PowerStatusDraw := TPOWER_PHASE_MAP.Create( nil );
//  PowerStatusDraw.Canvas := img1.Canvas;
//  PowerStatusDraw.Rect := Rect( 0, 0, img1.Width, img1.Height );
end;

procedure TfErrorInfo.FormDestroy(Sender: TObject);
begin
//  PowerStatusDraw.Free;
//  FPower.Free;
//  FPowerStatus.Free;
//  FPA.Free;
  FWED.Free;
//  FFourPower.Free;
//  FThreePower.Free;
//  FErrorList.Free;
end;

procedure TfErrorInfo.FormShow(Sender: TObject);
begin
  FWED.Show;
end;

procedure TfErrorInfo.LoadEquation(AWiringError: TWIRING_ERROR; AAngle: Double);
begin
  FWED.ADiagramType := ADiagramType;
  FWED.LoadEquation(AWiringError, AAngle);

  lbl1.Caption := AWiringError.Description;
end;

end.
