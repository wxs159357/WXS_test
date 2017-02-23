unit xElecMeter;

interface

uses SysUtils, Classes, xElecLine, xElecFunction, xElecOrgan, FMX.Types, Math;

type
  /// <summary>
  /// 电表相线
  /// </summary>
  TEPhase = (epSingle,        // 单相
             epThere,         // 三相三线
             epFour,          // 三相四线
             epThereReactive, // 三相三线无功表
             epFourReactive,  // 三相四线无功表
             epNot  // 没有定义
             );

  function GetPhaseStr(AEPhase : TEPhase) : string;
  function GetPhase(sPhaseStr:string) : TEPhase;

type
  /// <summary>
  /// 计量表类
  /// </summary>
  TElecMeter = class
  private
    FOrganList: TStringList;
    FElecPhase: TEPhase;
    FOnChange: TNotifyEvent;
    FActivePulseCount : Integer;     // 有功脉冲数
    FReactivePulseCount : Integer;   // 无功脉冲数

    FActiveTimer: TTimer;    // 正向有功
    FReactiveTimer: TTimer;  // 反向有功
    FPositiveActiveTimer: TTimer;    //正向有功功率
    FReverseActiveTimer: TTimer;     //反向有功功率
    FPositiveReactiveTimer: TTimer;  //正向无功功率
    FReverseReactiveTimer: TTimer;   //反向无功功率
    FQuadrant1ReactiveTimer: TTimer;   //第一象限无功功率
    FQuadrant2ReactiveTimer: TTimer;   //第二象限无功功率
    FQuadrant3ReactiveTimer: TTimer;   //第三象限无功功率
    FQuadrant4ReactiveTimer: TTimer;   //第四象限无功功率

    FActiveConstant: Integer;
    FReactiveConstant: Integer;
    FOnActivePulse: TNotifyEvent;
    FOnReactivePulse: TNotifyEvent;
    FOnkvarhChange: TNotifyEvent;
    FOnkWhChange: TNotifyEvent;
    FOnPositivekvarhChange: TNotifyEvent;
    FOnPositivekWhChange: TNotifyEvent;
    FOnReversekvarhChange: TNotifyEvent;
    FOnReversekWhChange: TNotifyEvent;
    FOnQuadrant4kvarhChange: TNotifyEvent;
    FOnQuadrant2kvarhChange: TNotifyEvent;
    FOnQuadrant3kvarhChange: TNotifyEvent;
    FOnQuadrant1kvarhChange: TNotifyEvent;
    FIsReverseOrder:Boolean;
    FIsIbReverse: Boolean;
    FIsIcReverse: Boolean;
    FIsIaReverse: Boolean;
    FIsUbBreak: Boolean;
    FIsUcBreak: Boolean;
    FIsUaBreak: Boolean;
    FIsIbBreak: Boolean;
    FIsIcBreak: Boolean;
    FIsIaBreak: Boolean;
    FIsEnable: Boolean;
    FTerminalList: TStringList;

    /// <summary>
    /// 电压或电流值改变
    /// </summary>
    procedure ValueChange(Sender: TObject);
    function GetOrganInfo(nIndex: Integer): TElecOrgan;
    procedure SetElecPhase(const Value: TEPhase);
    function GetActivePower: Double;
    function GetReactivePower: Double;
    procedure ActiveTimerChange(Sender: TObject);
    procedure ReactiveTimerChange(Sender: TObject);
    procedure TimerChange(Sender: TObject);

    // 重新设置有无功输出间隔
    procedure ReSetPulseTime;
    procedure SetActiveConstant(const Value: Integer);
    procedure SetReactiveConstant(const Value: Integer);
    function GetPowerFactor: Double;
    function GetPositiveActivePower: Double;
    function GetPositiveReactivePower: Double;
    function GetReverseActivePower: Double;
    function GetReverseReactivePower: Double;
    function GetIsIsReverseOrder: Boolean;
    procedure SetIsEnable(const Value: Boolean);

    /// <summary>
    /// 检查电表异常
    /// </summary>
    procedure CheckMeterError;
    function GetTerminalInfo(nIndex: Integer): TElecLine;
    function GetTerminalInfoByName(sName: string): TElecLine;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 电表类型
    /// </summary>
    property ElecPhase : TEPhase read FElecPhase write SetElecPhase;

    /// <summary>
    /// 是否有效 (脉冲事件和电量改变事件是否有效)
    /// </summary>
    property IsEnable : Boolean read FIsEnable write SetIsEnable;

    /// <summary>
    /// 有功常数
    /// </summary>
    property ActiveConstant : Integer read FActiveConstant write SetActiveConstant;

    /// <summary>
    /// 无功常数
    /// </summary>
    property ReactiveConstant : Integer read FReactiveConstant write SetReactiveConstant;

    /// <summary>
    /// 元件列表
    /// </summary>
    property OrganList : TStringList read FOrganList write FOrganList;

    /// <summary>
    /// 获取原件列表
    /// </summary>
    property OrganInfo[nIndex : Integer] : TElecOrgan read GetOrganInfo;

    /// <summary>
    /// 清空元件列表
    /// </summary>
    procedure ClearOrgan;

    /// <summary>
    /// 添加元件
    /// </summary>
    function AddOrgan : TElecOrgan;

  public
    /// <summary>
    /// 接线柱列表
    /// </summary>
    property TerminalList : TStringList read FTerminalList write FTerminalList;

    /// <summary>
    /// 接线柱信息
    /// </summary>
    property TerminalInfo[nIndex : Integer] : TElecLine read GetTerminalInfo;
    property TerminalInfoByName[sName : string] : TElecLine read GetTerminalInfoByName;

    /// <summary>
    /// 添加表尾接线端子
    /// </summary>
    function AddTerminal(sTerminalName : string) : TElecLine;


  public
    /// <summary>
    /// 清空电压值
    /// </summary>
    procedure ClearVolVlaue;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure ClearWValue;

    /// <summary>
    /// 清空电流列表
    /// </summary>
    procedure ClearCurrentList;

  public
    /// <summary>
    /// 有功功率
    /// </summary>
    property ActivePower : Double read GetActivePower;

    /// <summary>
    /// 无功功率
    /// </summary>
    property ReactivePower : Double read GetReactivePower;

    /// <summary>
    /// 功率因数
    /// </summary>
    property PowerFactor : Double read GetPowerFactor;

    /// <summary>
    /// 正向有功功率
    /// </summary>
    property PositiveActivePower : Double read GetPositiveActivePower;

    /// <summary>
    /// 反向有功功率
    /// </summary>
    property ReverseActivePower : Double read GetReverseActivePower;

    /// <summary>
    /// 正向无功功率
    /// </summary>
    property PositiveReactivePower : Double read GetPositiveReactivePower;

    /// <summary>
    /// 反向无功功率
    /// </summary>
    property ReverseReactivePower : Double read GetReverseReactivePower;

    /// <summary>
    /// 象限无功功率
    /// </summary>
    function GetQuadrantReactivePower(nSN: Integer): Double;
  public
    /// <summary>
    /// 是否逆向序
    /// </summary>
    property IsReverseOrder : Boolean read FIsReverseOrder;

    /// <summary>
    /// Ua失压
    /// </summary>
    property IsUaBreak : Boolean read FIsUaBreak;

    /// <summary>
    /// Ub失压
    /// </summary>
    property IsUbBreak : Boolean read FIsUbBreak;

    /// <summary>
    /// Uc失压
    /// </summary>
    property IsUcBreak : Boolean read FIsUcBreak;

    /// <summary>
    /// Ia失流
    /// </summary>
    property IsIaBreak : Boolean read FIsIaBreak;

    /// <summary>
    /// Ib失流
    /// </summary>
    property IsIbBreak : Boolean read FIsIbBreak;

    /// <summary>
    /// Ic失流
    /// </summary>
    property IsIcBreak : Boolean read FIsIcBreak;

    /// <summary>
    /// Ia反向
    /// </summary>
    property IsIaReverse : Boolean read FIsIaReverse;

    /// <summary>
    /// Ib反向
    /// </summary>
    property IsIbReverse : Boolean read FIsIbReverse;

    /// <summary>
    /// Ic反向
    /// </summary>
    property IsIcReverse : Boolean read FIsIcReverse;

  public
    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 有功脉冲输出信号事件
    /// </summary>
    property OnActivePulse : TNotifyEvent read FOnActivePulse write FOnActivePulse;

    /// <summary>
    /// 无功脉冲输出信号事件
    /// </summary>
    property OnReactivePulse : TNotifyEvent read FOnReactivePulse write FOnReactivePulse;

    /// <summary>
    /// 有功电量改变0.01度事件
    /// </summary>
    property OnkWhChange : TNotifyEvent read FOnkWhChange write FOnkWhChange;

    /// <summary>
    /// 无功电量改变0.01度事件
    /// </summary>
    property OnkvarhChange : TNotifyEvent read FOnkvarhChange write FOnkvarhChange;

    /// <summary>
    /// 正向有功电量改变0.01度事件
    /// </summary>
    property OnPositivekWhChange : TNotifyEvent read FOnPositivekWhChange write FOnPositivekWhChange;

    /// <summary>
    /// 反向有功电量改变0.01度事件
    /// </summary>
    property OnReversekWhChange : TNotifyEvent read FOnReversekWhChange write FOnReversekWhChange;

    /// <summary>
    /// 正向无功电量改变0.01度事件
    /// </summary>
    property OnPositivekvarhChange : TNotifyEvent read FOnPositivekvarhChange write FOnPositivekvarhChange;

    /// <summary>
    /// 反向无功电量改变0.01度事件
    /// </summary>
    property OnReversekvarhChange : TNotifyEvent read FOnReversekvarhChange write FOnReversekvarhChange;

    /// <summary>
    /// 四象限无功电量改变0.01度事件
    /// </summary>
    property OnQuadrant1kvarhChange : TNotifyEvent read FOnQuadrant1kvarhChange write FOnQuadrant1kvarhChange;   //第一象限无功功率
    property OnQuadrant2kvarhChange : TNotifyEvent read FOnQuadrant2kvarhChange write FOnQuadrant2kvarhChange;   //第二象限无功功率
    property OnQuadrant3kvarhChange : TNotifyEvent read FOnQuadrant3kvarhChange write FOnQuadrant3kvarhChange;   //第三象限无功功率
    property OnQuadrant4kvarhChange : TNotifyEvent read FOnQuadrant4kvarhChange write FOnQuadrant4kvarhChange;   //第四象限无功功率
  end;
implementation

function GetPhaseStr(AEPhase : TEPhase) : string;
begin
  case AEPhase of
    epSingle: Result := '单相';
    epThere: Result := '三相三线';
    epFour: Result := '三相四线';
    epThereReactive: Result := '三相三线无功表';
    epFourReactive: Result := '三相四线无功表';

  else
    Result := '三相四线';
  end;
end;

function GetPhase(sPhaseStr:string) : TEPhase;
var
  i: TEPhase;
begin
  Result := epFour;
  for i := epSingle to epNot do
  begin
    if GetPhaseStr(i) = Trim(sPhaseStr) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

{ TElecOrgan }

procedure TElecMeter.ActiveTimerChange(Sender: TObject);
begin
  if not FIsEnable then
    Exit;

  if Assigned(FOnActivePulse) then
    FOnActivePulse(Self);

  Inc(FActivePulseCount);

  if Assigned(FOnkWhChange) then
  begin
    if FActivePulseCount >= Trunc(FActiveConstant/100) then
    begin
      FActivePulseCount := 0;

      if FIsEnable then
        FOnkWhChange(Self);
    end;
  end;
end;

function TElecMeter.AddOrgan: TElecOrgan;
begin
  Result := TElecOrgan.Create;
  Result.OnChange := valuechange;

  FOrganList.AddObject('', Result);
end;

function TElecMeter.AddTerminal(sTerminalName : string): TElecLine;
begin
  Result := TElecLine.Create;
  Result.OnChange := valuechange;
  Result.LineName := sTerminalName;
  FTerminalList.AddObject(sTerminalName, Result);
end;

procedure TElecMeter.CheckMeterError;
  procedure SetValue(AOrgan: TElecOrgan; var bIsUBreak, bIsIBreak, bIsIReverse : Boolean);
  begin
    if Assigned(AOrgan) then
    begin
      bIsUBreak   := AOrgan.IsUBreak;
      bIsIBreak   := AOrgan.IsIBreak;
      bIsIReverse := AOrgan.IsIReverse;
    end
    else
    begin
      bIsUBreak   := False;
      bIsIBreak   := False;
      bIsIReverse := False;
    end;

  end;
var
  AOrgan1, AOrgan2, AOrgan3 : TElecOrgan;
begin
  AOrgan1 := OrganInfo[0];
  AOrgan2 := OrganInfo[1];
  AOrgan3 := OrganInfo[2];

  FIsUaBreak:= False;
  FIsIaBreak:= False;
  FIsIaReverse:= False;
  FIsUbBreak:= False;
  FIsIbBreak:= False;
  FIsIbReverse:= False;
  FIsUcBreak:= False;
  FIsIcBreak:= False;
  FIsIcReverse:= False;

  case FElecPhase of
    epSingle :
    begin
      SetValue(AOrgan1, FIsUaBreak, FIsIaBreak, FIsIaReverse);
    end;
    epThere :
    begin
      SetValue(AOrgan1, FIsUaBreak, FIsIaBreak, FIsIaReverse);
      SetValue(AOrgan2, FIsUcBreak, FIsIcBreak, FIsIcReverse);

      FIsUbBreak := AOrgan1.VolPointOut.Voltage.Value < 0.001;
      FIsReverseOrder:= GetIsIsReverseOrder;
    end;
    epFour :
    begin
      SetValue(AOrgan1, FIsUaBreak, FIsIaBreak, FIsIaReverse);
      SetValue(AOrgan2, FIsUbBreak, FIsIbBreak, FIsIbReverse);
      SetValue(AOrgan3, FIsUcBreak, FIsIcBreak, FIsIcReverse);

      FIsReverseOrder:= GetIsIsReverseOrder;
    end;
  end;
end;

procedure TElecMeter.ClearCurrentList;
var
  i: Integer;
begin
  for i := FOrganList.Count - 1 downto 0 do
  begin
    OrganInfo[i].CurrentPointIn.ClearCurrentList;
    OrganInfo[i].CurrentPointOut.ClearCurrentList;
  end;

  for i := FTerminalList.Count -1 downto 0 do
    TerminalInfo[i].ClearCurrentList;
end;

procedure TElecMeter.ClearOrgan;
var
  i: Integer;
begin
  for i := FOrganList.Count - 1 downto 0 do
    FOrganList.Objects[i].Free;

  FOrganList.Clear;
end;

procedure TElecMeter.ClearVolVlaue;
var
  i: Integer;
begin
  for i := FOrganList.Count - 1 downto 0 do
    OrganInfo[i].ClearVolVlaue;

  for i := FTerminalList.Count -1 downto 0 do
    TerminalInfo[i].Voltage.ClearValue;

end;

procedure TElecMeter.ClearWValue;
var
  i: Integer;
begin
  for i := FOrganList.Count - 1 downto 0 do
    OrganInfo[i].ClearWValue;

  for i := FTerminalList.Count -1 downto 0 do
    TerminalInfo[i].ClearWValue;
end;

constructor TElecMeter.Create;
begin
  FOrganList:= TStringList.Create;
  FActiveTimer:= TTimer.Create(nil);
  FReactiveTimer:= TTimer.Create(nil);
  FPositiveActiveTimer:= TTimer.Create(nil);   //正向有功功率
  FReverseActiveTimer:= TTimer.Create(nil);    //反向有功功率
  FPositiveReactiveTimer:= TTimer.Create(nil); //正向无功功率
  FReverseReactiveTimer:= TTimer.Create(nil);   //反向无功功率

  FQuadrant1ReactiveTimer:= TTimer.Create(nil);   //第一象限无功功率
  FQuadrant2ReactiveTimer:= TTimer.Create(nil);   //第二象限无功功率
  FQuadrant3ReactiveTimer:= TTimer.Create(nil);   //第三象限无功功率
  FQuadrant4ReactiveTimer:= TTimer.Create(nil);   //第四象限无功功率

  FTerminalList:= TStringList.Create;

  ElecPhase := epNot;
  FActiveTimer.Enabled := False;
  FReactiveTimer.Enabled := False;
  FPositiveActiveTimer.Enabled := False;
  FReverseActiveTimer.Enabled := False;
  FPositiveReactiveTimer.Enabled := False;
  FReverseReactiveTimer.Enabled := False;

  FQuadrant1ReactiveTimer.Enabled:= False;
  FQuadrant2ReactiveTimer.Enabled:= False;
  FQuadrant3ReactiveTimer.Enabled:= False;
  FQuadrant4ReactiveTimer.Enabled:= False;

  FActiveTimer.OnTimer := ActiveTimerChange;
  FReactiveTimer.OnTimer := ReactiveTimerChange;

  FPositiveActiveTimer.OnTimer := TimerChange;
  FReverseActiveTimer.OnTimer := TimerChange;
  FPositiveReactiveTimer.OnTimer := TimerChange;
  FReverseReactiveTimer.OnTimer := TimerChange;
  FQuadrant1ReactiveTimer.OnTimer := TimerChange;
  FQuadrant2ReactiveTimer.OnTimer := TimerChange;
  FQuadrant3ReactiveTimer.OnTimer := TimerChange;
  FQuadrant4ReactiveTimer.OnTimer := TimerChange;

  FActiveConstant := 2000;
  FReactiveConstant := 2000;
  FActivePulseCount := 0;
  FReactivePulseCount := 0;

end;

destructor TElecMeter.Destroy;
var
  i: Integer;
begin
  for i := FTerminalList.Count - 1 downto 0 do
    FTerminalList.Objects[i].Free;

  FTerminalList.Clear;

  ClearOrgan;

  FTerminalList.Free;
  FOrganList.Free;
  FActiveTimer.Free;
  FReactiveTimer.Free;

  inherited;
end;

function TElecMeter.GetActivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].ActivePower;
end;

function TElecMeter.GetIsIsReverseOrder: Boolean;
var
  A, B, C : TElecOrgan;
  dAngle1, dAngle2, dAngle3 : Double;
begin
  A := OrganInfo[0];
  B := OrganInfo[1];
  C := OrganInfo[2];

  if Assigned(A) and Assigned(B) and Assigned(C) then
  BEGIN
    dAngle1 := AdjustAngle(B.VolOrgan.Angle - A.VolOrgan.Angle);
    dAngle2 := AdjustAngle(C.VolOrgan.Angle - B.VolOrgan.Angle);
    dAngle3 := AdjustAngle(C.VolOrgan.Angle - A.VolOrgan.Angle);

    Result := (dAngle1 <> 120) or (dAngle2 <> 120 ) or (dAngle3 <> 240 );
  END
  else
  begin
    Result := False;
  end;
end;

function TElecMeter.GetOrganInfo(nIndex: Integer): TElecOrgan;
begin
  if (nIndex >= 0) and (nIndex < FOrganList.Count) then
  begin
    Result := TElecOrgan(FOrganList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TElecMeter.GetPositiveActivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].PositiveActivePower;

end;

function TElecMeter.GetPositiveReactivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].PositiveReactivePower;

end;

function TElecMeter.GetPowerFactor: Double;
var
  d : Double;
begin
  d := ActivePower;

  if Abs(d) > 0.0001 then
  begin
    //  Cosφ=1/(1+(无功力调电量/有功力调电量)^2)^0.5
    Result := 1 / sqrt( 1 + sqr( ReactivePower / d ));
  end
  else
  begin
    Result := 0;
  end;
end;

function TElecMeter.GetQuadrantReactivePower(nSN: Integer): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].QuadrantReactivePower[nSN];
end;

function TElecMeter.GetReactivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].ReActivePower;
end;

function TElecMeter.GetReverseActivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].ReverseActivePower;
end;

function TElecMeter.GetReverseReactivePower: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FOrganList.Count - 1 do
    Result := Result + OrganInfo[i].ReverseReactivePower;

end;

function TElecMeter.GetTerminalInfo(nIndex: Integer): TElecLine;
begin
  if (nIndex >= 0) and (nIndex < FTerminalList.Count) then
  begin
    Result := TElecLine(FTerminalList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TElecMeter.GetTerminalInfoByName(sName: string): TElecLine;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FTerminalList.Count -1 do
  begin
    if UpperCase(TElecLine(FTerminalList.Objects[i]).LineName) = UpperCase(sName) then
    begin
      Result := TElecLine(FTerminalList.Objects[i]);
      Break;
    end;
  end;
end;

procedure TElecMeter.ReactiveTimerChange(Sender: TObject);
begin
  if not FIsEnable then
    Exit;

  if Assigned(FOnReactivePulse) then
    FOnReactivePulse(Self);

  Inc(FReactivePulseCount);

  if Assigned(FOnkvarhChange) then
  begin
    if FReactivePulseCount >= Trunc(FReactiveConstant/100) then
    begin
      FReactivePulseCount := 0;
      if FIsEnable then
        FOnkvarhChange(Self);
    end;
  end;
end;

procedure TElecMeter.ReSetPulseTime;
  procedure SetTimerInterval(ATimer : TTimer; dP : Double);
  begin
    if Abs(dP) > 0.00001 then
    begin
      ATimer.Interval   := Abs(Round(1*3600*1000/100*1000/dP));
    end
    else
    begin
      ATimer.Interval := 0;
    end;
    ATimer.Enabled   := (ATimer.Interval > 0) and FIsEnable;
  end;
begin
  if Abs(ActivePower*ActiveConstant) > 0.001 then
    FActiveTimer.Interval   := Abs(Round(1/((ActivePower*ActiveConstant)/3600000) * 1000))
  else
    FActiveTimer.Interval := 0;

  if Abs(ReactivePower*ReactiveConstant) > 0.001 then
    FReactiveTimer.Interval := Abs(Round(1/((ReactivePower*ReactiveConstant)/3600000) * 1000))
  else
    FReactiveTimer.Interval := 0;

  FActiveTimer.Enabled   := (FActiveTimer.Interval > 0) and FIsEnable;
  FReactiveTimer.Enabled := (FReactiveTimer.Interval > 0) and FIsEnable;

  SetTimerInterval(FPositiveActiveTimer, PositiveActivePower);
  SetTimerInterval(FReverseActiveTimer, ReverseActivePower);
  {07协议里没有正向无功和反向无功}
//  SetTimerInterval(FPositiveReactiveTimer, PositiveReactivePower);
//  SetTimerInterval(FReverseReactiveTimer, ReverseReactivePower);

  SetTimerInterval(FQuadrant1ReactiveTimer, GetQuadrantReactivePower(1));
  SetTimerInterval(FQuadrant2ReactiveTimer, GetQuadrantReactivePower(2));
  SetTimerInterval(FQuadrant3ReactiveTimer, GetQuadrantReactivePower(3));
  SetTimerInterval(FQuadrant4ReactiveTimer, GetQuadrantReactivePower(4));
end;

procedure TElecMeter.SetActiveConstant(const Value: Integer);
begin
  if FActiveConstant <> Value then
  begin
    FActiveConstant := Value;

    ReSetPulseTime;
  end;
end;

procedure TElecMeter.SetElecPhase(const Value: TEPhase);
  procedure AddOrganCount(nCount : Integer; bIsRecative:Boolean);
  var
    j : Integer;
    AOrgan : TElecOrgan;
  begin
    for j := 0 to nCount - 1 do
    begin
      AOrgan := AddOrgan;
      AOrgan.IsReactive := bIsRecative;
      AOrgan.OrganName := '元件' + IntToStr(j+1);
    end;
  end;
var
  AOrgan : TElecOrgan;
  ATerminal, ATemp : TElecLine;
begin
  if FElecPhase <> Value then
  begin
    FElecPhase := Value;

    ClearOrgan;

    case FElecPhase of
      epSingle:
      begin
        AddOrganCount(1, False);
      end;
      epThere:
      begin
        AddOrganCount(2, False);

        AOrgan := OrganInfo[0];
        ATerminal := AddTerminal('Ia+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ua');
        ATerminal.ConnPointAdd(AOrgan.VolPointIn);
        ATerminal := AddTerminal('Ia-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        ATemp := AddTerminal('Ub');
        ATemp.ConnPointAdd(AOrgan.VolPointOut);

        AOrgan := OrganInfo[1];
        ATerminal := AddTerminal('Ic+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Uc');
        ATerminal.ConnPointAdd(AOrgan.VolPointIn);
        ATerminal := AddTerminal('Ic-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);
        ATemp.ConnPointAdd(AOrgan.VolPointOut);

      end;
      epFour:
      begin
        AddOrganCount(3, False);

        AOrgan := OrganInfo[0];
        ATerminal := AddTerminal('Ia+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ua');
        ATerminal.ConnPointAdd(AOrgan.VolPointIn);
        ATerminal := AddTerminal('Ia-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        AOrgan := OrganInfo[1];
        ATerminal := AddTerminal('Ib+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ub');
        ATerminal.ConnPointAdd(AOrgan.VolPointIn);
        ATerminal := AddTerminal('Ib-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        AOrgan := OrganInfo[2];
        ATerminal := AddTerminal('Ic+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Uc');
        ATerminal.ConnPointAdd(AOrgan.VolPointIn);
        ATerminal := AddTerminal('Ic-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        ATemp := AddTerminal('Un');
        ATemp.ConnPointAdd(OrganInfo[0].VolPointOut);
        ATemp.ConnPointAdd(OrganInfo[1].VolPointOut);
        ATemp.ConnPointAdd(OrganInfo[2].VolPointOut);
      end;
      epThereReactive:
      begin
        AddOrganCount(2, True);

        AOrgan := OrganInfo[0];
        ATerminal := AddTerminal('Ia+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ua');
        ATerminal.ConnPointAdd(OrganInfo[1].VolPointIn);
        ATerminal := AddTerminal('Ia-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        ATemp := AddTerminal('Ub');
        ATemp.ConnPointAdd(AOrgan.VolPointIn);

        AOrgan := OrganInfo[1];
        ATerminal := AddTerminal('Ic+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Uc');
        ATerminal.ConnPointAdd(OrganInfo[0].VolPointOut);
        ATerminal.ConnPointAdd(AOrgan.VolPointOut);
        ATerminal := AddTerminal('Ic-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);
      end;
      epFourReactive:
      begin
        AddOrganCount(3, True);

        AOrgan := OrganInfo[0];
        ATerminal := AddTerminal('Ia+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ua');
        ATerminal.ConnPointAdd(OrganInfo[1].VolPointOut);
        ATerminal.ConnPointAdd(OrganInfo[2].VolPointIn);
        ATerminal := AddTerminal('Ia-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        AOrgan := OrganInfo[1];
        ATerminal := AddTerminal('Ib+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Ub');
        ATerminal.ConnPointAdd(OrganInfo[2].VolPointOut);
        ATerminal.ConnPointAdd(OrganInfo[0].VolPointIn);
        ATerminal := AddTerminal('Ib-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);

        AOrgan := OrganInfo[2];
        ATerminal := AddTerminal('Ic+');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointIn);
        ATerminal := AddTerminal('Uc');
        ATerminal.ConnPointAdd(OrganInfo[0].VolPointOut);
        ATerminal.ConnPointAdd(OrganInfo[1].VolPointIn);
        ATerminal := AddTerminal('Ic-');
        ATerminal.ConnPointAdd(AOrgan.CurrentPointOut);
      end;
    end;
  end;
end;

procedure TElecMeter.SetIsEnable(const Value: Boolean);
begin
  FIsEnable := Value;
  ReSetPulseTime;
end;

procedure TElecMeter.SetReactiveConstant(const Value: Integer);
begin
  if FReactiveConstant <> Value then
  begin
    FReactiveConstant := Value;

    ReSetPulseTime;
  end;
end;

procedure TElecMeter.TimerChange(Sender: TObject);
begin
  if not FIsEnable then
    Exit;

  if Sender = FPositiveActiveTimer then
  begin
    if Assigned(FOnPositivekWhChange) then
        FOnPositivekWhChange(Self);
  end
  else if Sender = FReverseActiveTimer then
  begin
    if Assigned(FOnReversekWhChange) then
        FOnReversekWhChange(Self);
  end
  else if Sender = FPositiveReactiveTimer then
  begin
    if Assigned(FOnPositivekvarhChange) then
        FOnPositivekvarhChange(Self);
  end
  else if Sender = FReverseReactiveTimer then
  begin
    if Assigned(FOnReversekvarhChange) then
        FOnReversekvarhChange(Self);
  end
  else if Sender = FQuadrant1ReactiveTimer then
  begin
    if Assigned(FOnQuadrant1kvarhChange) then
        FOnQuadrant1kvarhChange(Self);
  end
  else if Sender = FQuadrant2ReactiveTimer then
  begin
    if Assigned(FOnQuadrant2kvarhChange) then
        FOnQuadrant2kvarhChange(Self);
  end
  else if Sender = FQuadrant3ReactiveTimer then
  begin
    if Assigned(FOnQuadrant3kvarhChange) then
        FOnQuadrant3kvarhChange(Self);
  end
  else if Sender = FQuadrant4ReactiveTimer then
  begin
    if Assigned(FOnQuadrant4kvarhChange) then
        FOnQuadrant4kvarhChange(Self);
  end;
end;

procedure TElecMeter.ValueChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);

  ReSetPulseTime;
  CheckMeterError;
end;

end.





