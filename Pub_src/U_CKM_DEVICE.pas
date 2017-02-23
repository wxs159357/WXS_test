{===============================================================================
  Copyright(c) 2013, 高山流水（QQ121926828）
  All rights reserved.

  程控模拟设备
  + TCKM_DEVICE  设备基类
  + TCKM_TIMER   计时器类
  + TCKM_POWER   功率源

===============================================================================}

unit U_CKM_DEVICE;

interface

uses SysUtils, Classes, ExtCtrls, DateUtils;

type
  /// <summary>
  /// 程控模拟设备基类
  /// </summary>
  TCKM_DEVICE = class( TComponent )
  private
    FControlAdd: Integer;
    FSN: String;
    FVersion: string;
    FOwner: TObject;
    FControlAdd2: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    /// <summary>
    /// 设备序号
    /// </summary>
    property SN : string read FSN write FSN;

    /// <summary>
    /// 控制地址1
    /// </summary>
    property ControlAdd : Integer read FControlAdd write FControlAdd;

    /// <summary>
    /// 控制地址2
    /// </summary>
    property ControlAdd2 : Integer read FControlAdd2 write FControlAdd2;

    /// <summary>
    /// 版本
    /// </summary>
    property Version : string read FVersion write FVersion;

    /// <summary>
    /// 所有者
    /// </summary>
    property Owner : TObject read FOwner write FOwner;
  end;

type
  /// <summary>
  /// 语音板对象
  /// </summary>
  TVOICE = class(TCKM_DEVICE)
  private
    FVoiceType: Integer;

  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    /// 语音提示类型
    /// 考试开始      01
    /// 考试剩余10分钟02
    /// 考试剩余5分钟 03
    /// 考试结束      04
    /// 培训开始      05
    /// 培训结束      06
    /// </summary>
    property VoiceType : Integer read FVoiceType write FVoiceType;
  end;

type
  /// <summary>
  /// 计时类型, 时钟 / 倒计时 / 正计时
  /// </summary>
  TCKM_TIMER_TYPE = ( ttClock, ttRevTimer, ttPosTimer );

  /// <summary>
  /// 获取计时类型名称
  /// </summary>
  function GetTimerTypeName( AType : TCKM_TIMER_TYPE ) : string;

type
  /// <summary>
  /// 计时器工作状态
  /// </summary>
  TTIMER_WORK_STATUS = ( tsStop, tsRuning, tsPaused );

  /// <summary>
  /// 获取计时器工作状态名称
  /// </summary>
  function GetTimerWorkStatusName( AStatus : TTIMER_WORK_STATUS ) : string;

type
  /// <summary>
  /// 计时器
  /// </summary>
  TCKM_TIMER = class( TCKM_DEVICE )
  private
    FTimerStart : TDateTime; // 计时开始时间
    FTimerPaused : Integer; // 暂停时间 (秒)
    FCurrentTime: TDateTime; // 当前时间
    FExamPeriod: Integer;
    FWorkType: TCKM_TIMER_TYPE;
    FWorkStatus: TTIMER_WORK_STATUS;
    FOnMinChange: TNotifyEvent;
    FPausedRefreshTimes: Integer;
    FOnPausedRefresh: TNotifyEvent;
    FDevPort: Integer;
    FOnTimeOver: TNotifyEvent;
    FMins : Integer;
    procedure SetExamPeriod(const Value: Integer);
    procedure SetWorkType(const Value: TCKM_TIMER_TYPE);
    function GetCurrentTime: TDateTime;
    procedure SetCurrentTime(const Value: TDateTime);
  protected
    /// <summary>
    /// 内部定时器
    /// </summary>
    Timer : TTimer;

    /// <summary>
    /// 内部定时器计时
    /// </summary>
    procedure OnTimer( Sender : TObject );
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    /// <summary>
    /// 计时方式
    /// </summary>
    property WorkType : TCKM_TIMER_TYPE read FWorkType write SetWorkType;

    /// <summary>
    /// 考试计时( 分钟 )
    /// </summary>
    property ExamPeriod : Integer read FExamPeriod write SetExamPeriod;

    /// <summary>
    /// 当前的时间
    /// </summary>
    property CurrentTime : TDateTime read GetCurrentTime write SetCurrentTime;

    /// <summary>
    /// 计时器工作状态
    /// </summary>
    property WorkStatus : TTIMER_WORK_STATUS read FWorkStatus write FWorkStatus;

    /// <summary>
    /// 开始
    /// </summary>
    procedure Start;

    /// <summary>
    /// 暂停
    /// </summary>
    procedure Paused;

    /// <summary>
    /// 继续
    /// </summary>
    procedure Continue;

    /// <summary>
    /// 停止
    /// </summary>
    procedure Stop;

    /// <summary>
    /// 分钟级改变事件
    /// </summary>
    property OnMinChange : TNotifyEvent read FOnMinChange write FOnMinChange;

    /// <summary>
    /// 暂停刷新时间 -1为不刷新
    /// </summary>
    property PausedRefreshTimes : Integer read FPausedRefreshTimes
      write FPausedRefreshTimes;

    /// <summary>
    /// 暂停刷新事件
    /// </summary>
    property OnPausedRefresh : TNotifyEvent read FOnPausedRefresh write FOnPausedRefresh;

    /// <summary>
    /// 倒计时时间到事件
    /// </summary>
    property OnTimeOver : TNotifyEvent read FOnTimeOver write FOnTimeOver;

    /// <summary>
    /// 对应设备串口地址
    /// </summary>
    property DevPort : Integer read FDevPort write FDevPort;
  end;

const
  /// <summary>
  /// 最小电流
  /// </summary>
  C_POWER_MIN_CURRENT = 0;

type
  /// <summary>
  /// 功率源工作状态
  /// </summary>
  TPOWER_WORK_STATUS = ( psOff, psOn, psAlarm );

  /// <summary>
  /// 获取功率源工作状态名称
  /// </summary>
  function GetPowerWorkStatusName( AStatus : TPOWER_WORK_STATUS ) : string;

type
  /// <summary>
  /// 功率输出类别
  /// </summary>
  TPOWER_OUTPUT_TYPE = ( ptSingle,   // 单相
                         ptThree,    // 三相三线
                         ptFour,     // 三相四线
                         ptFour577   // 三相四线57.7
                         );

  /// <summary>
  /// 获取功率输出类别名称
  /// </summary>
  function GetPowerOutputTypeName( AType : TPOWER_OUTPUT_TYPE ) : string;

type
  /// <summary>
  /// 功率输出, False 断相， True为通
  /// </summary>
  PPOWER_OUTPUT = ^TPOWER_OUTPUT;
  TPOWER_OUTPUT = record
    Ua : Boolean;
    Ub : Boolean;
    Uc : Boolean;
    Un : Boolean;
    Ia : Boolean;
    Ib : Boolean;
    Ic : Boolean;
  end;

  /// <summary>
  /// 获取默认的输出类型
  /// </summary>
  function GetPowerOutputDefault( AType : TPOWER_OUTPUT_TYPE ) : TPOWER_OUTPUT;

type
  /// <summary>
  /// 报警类型
  /// </summary>
  TPOWER_ALARM_TYPE = ( plUa, plIa, plUb, plIb, plUc, plIc );

type
  /// <summary>
  /// 报警
  /// </summary>
  TPOWER_ALARM = set of TPOWER_ALARM_TYPE;

  /// <summary>
  /// 获取报警信息
  /// </summary>
  function GetPowerAlarmDescription( APAlarm : TPOWER_ALARM ) : string;

type
  /// <summary>
  /// 功率源操作类型
  /// </summary>
  TPOWER_OPERATE_TYPE = ( poTurnOn, poTurnOff, poAdjust);

type
  /// <summary>
  /// 功率源操作事件
  /// </summary>
  TPOWER_OPERATE_EVENT = procedure( Sender : TObject;
    AOperateType : TPOWER_OPERATE_TYPE ) of object;

type
  /// <summary>
  /// 功率源
  /// </summary>
  TCKM_POWER = class( TCKM_DEVICE )
  private
    FPowerOutputType: TPOWER_OUTPUT_TYPE;
    FPowerOutput: TPOWER_OUTPUT;
    FCurrent: Double;
    FVoltage: Double;
    FMaxVoltage: Double;

    FWorkStatus: TPOWER_WORK_STATUS;
    FAlarm : TPOWER_ALARM;
    FOnAlarm : TNotifyEvent;
    FOnOperation : TPOWER_OPERATE_EVENT;
    FCurrentPercent: Integer;
    FVoltagePercent: Integer;
    FAngle: Double;

    FOnSettingChange : TNotifyEvent;
    FMaxCurrent: Double;
    FDefAngle: Double;
    FMaxAngle: Double;
    FMinAngle: Double;
    FAdjustAngle3: Double;
    FAdjustAngle4: Double;
    procedure SetAngle(const Value: Double);
    procedure SetCurrent(const Value: Double);
    procedure SetPowerOutputType(const Value: TPOWER_OUTPUT_TYPE);
    procedure SetVoltage(const Value: Double);
    procedure SetMaxVoltage(const Value: Double);
    procedure SetPowerOutput(const Value: TPOWER_OUTPUT);
    function GetAlarmDescription : string;
    procedure SetCurrentPercent(const Value: Integer);
    procedure SetVoltagePercent(const Value: Integer);
    procedure SetAlarm(const Value: TPOWER_ALARM);
    procedure SetMaxCurrent(const Value: Double);
    /// <summary>
    /// 功率源满量程电压值
    /// </summary>
    function GetFullMaxVol : Double;
    /// <summary>
    /// 功率源满量程电流值
    /// </summary>
    function GetFullMaxCur : Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    /// 开功率源
    /// </summary>
    procedure TurnOn;

    /// <summary>
    /// 关功率源
    /// </summary>
    procedure TurnOff;

    /// <summary>
    /// 调整功率源
    /// </summary>
    procedure Adjust;
  public
    /// <summary>
    /// 相线类型
    /// </summary>
    property PowerOutputType: TPOWER_OUTPUT_TYPE read FPowerOutputType write SetPowerOutputType;

    /// <summary>
    /// 最大电压
    /// </summary>
    property MaxVoltage : Double read FMaxVoltage write SetMaxVoltage;

    /// <summary>
    /// 最大电流
    /// </summary>
    property MaxCurrent : Double read FMaxCurrent write SetMaxCurrent;

    /// <summary>
    /// 默认角度
    /// </summary>
    property DefAngle : Double read FDefAngle write FDefAngle;

    /// <summary>
    /// 最大角度
    /// </summary>
    property MaxAngle : Double read FMaxAngle write FMaxAngle;

    /// <summary>
    /// 最小角度
    /// </summary>
    property MinAngle : Double read FMinAngle write FMinAngle;

    /// <summary>
    /// 统一电压值
    /// </summary>
    property Voltage : Double read FVoltage write SetVoltage;

    /// <summary>
    /// 统一电压输出比例
    /// </summary>
    property VoltagePercent : Integer read FVoltagePercent write SetVoltagePercent;

    /// <summary>
    /// 统一电流
    /// </summary>
    property Current : Double read FCurrent write SetCurrent;

    /// <summary>
    /// 统一电压输出比例
    /// </summary>
    property CurrentPercent : Integer read FCurrentPercent write SetCurrentPercent;

    /// <summary>
    /// 角度
    /// </summary>
    property Angle : Double read FAngle write SetAngle;

    /// <summary>
    /// 输出状态
    /// </summary>
    property PowerOutput : TPOWER_OUTPUT read FPowerOutput write SetPowerOutput;



    /// <summary>
    /// 工作状态
    /// </summary>
    property WorkStatus : TPOWER_WORK_STATUS read FWorkStatus;

    /// <summary>
    /// 报警
    /// </summary>
    property Alarm : TPOWER_ALARM read FAlarm write SetAlarm;

    /// <summary>
    /// 补偿角度
    /// </summary>
    property AdjustAngle3 : Double read FAdjustAngle3 write FAdjustAngle3;
    property AdjustAngle4 : Double read FAdjustAngle4 write FAdjustAngle4;
    function GetAdjustAngle:Double;

    /// <summary>
    /// 报警信息
    /// </summary>
    property AlarmDescription : string read GetAlarmDescription;

    /// <summary>
    /// 报警事件
    /// </summary>
    property OnAlarm : TNotifyEvent read FOnAlarm write FOnAlarm;

    /// <summary>
    /// 操作事件
    /// </summary>
    property OnOperation : TPOWER_OPERATE_EVENT read FOnOperation write FOnOperation;

    /// <summary>
    /// 设置改变事件
    /// </summary>
    property OnSettingChange : TNotifyEvent read FOnSettingChange write FOnSettingChange;
  end;

type
  /// <summary>
  /// 低压排故报警板
  /// </summary>
  TLFAlarm = class(TCKM_DEVICE)
  private
    FIsCurrentOverB: Boolean;
    FIsCurrentOverC: Boolean;
    FIsCurrentOverA: Boolean;
    FIsVolOut: Boolean;
    FOnChange: TNotifyEvent;
    FIsControlPowerOn: Boolean;
    FIsReplay: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// A相过流
    /// </summary>
    property IsCurrentOverA : Boolean read FIsCurrentOverA write FIsCurrentOverA;
    /// <summary>
    /// B相过流
    /// </summary>
    property IsCurrentOverB : Boolean read FIsCurrentOverB write FIsCurrentOverB;
    /// <summary>
    /// C相过流
    /// </summary>
    property IsCurrentOverC : Boolean read FIsCurrentOverC write FIsCurrentOverC;
    /// <summary>
    /// 漏电
    /// </summary>
    property IsVolOut : Boolean read FIsVolOut write FIsVolOut;

    /// <summary>
    /// 是否通电
    /// </summary>
    property IsControlPowerOn : Boolean read FIsControlPowerOn write FIsControlPowerOn;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 通讯是否回复
    /// </summary>
    property IsReplay  : Boolean read FIsReplay write FIsReplay;
  end;

type
  /// <summary>
  /// 载波控制板
  /// </summary>
  TCarrierControl = class(TCKM_DEVICE)
  private
    FIsOn: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// 是否打开载波
    /// </summary>
    property IsOn : Boolean read FIsOn write FIsOn;
  end;

implementation

function GetTimerTypeName( AType : TCKM_TIMER_TYPE ) : string;
begin
  case AType of
    ttClock: Result := '时钟';
    ttRevTimer: Result := '倒计时';
    ttPosTimer : Result := '正计时';
  end;
end;

function GetTimerWorkStatusName( AStatus : TTIMER_WORK_STATUS ) : string;
begin
  case AStatus of
    tsStop: Result := '停止';
    tsRuning: Result := '运行';
    tsPaused: Result := '暂停';
  end;
end;

{ TCKM_TIMER }

procedure TCKM_TIMER.Continue;
begin
  case FWorkStatus of
    tsPaused: WorkStatus := tsRuning;
  end;
end;

constructor TCKM_TIMER.Create(AOwner: TComponent);
begin
  inherited;

  // 内部定时器
  Timer := TTimer.Create( nil );
  Timer.Enabled := False;
  Timer.OnTimer := OnTimer;

  FTimerStart := Now;
  FExamPeriod := 30;
  FWorkType   := ttClock;
  FCurrentTime := -1;
  FWorkStatus := tsStop;
  FPausedRefreshTimes := 60;
end;

destructor TCKM_TIMER.Destroy;
begin
  Timer.Free;
  inherited;
end;

function TCKM_TIMER.GetCurrentTime: TDateTime;
begin
  case FWorkStatus of
    tsStop:
    begin
      if FWorkType = ttRevTimer then
        Result := FExamPeriod/minsperday
      else
        Result := FCurrentTime;
    end;
  else
    Result := FCurrentTime;
  end;
end;

procedure TCKM_TIMER.OnTimer(Sender: TObject);
begin
  case FWorkStatus of
    tsStop:
    begin
      FTimerStart := Now;
      FTimerPaused := 0;
    end;
    tsRuning:
    begin

    end;
    tsPaused:
    begin
      Inc(FTimerPaused);
    end;
  end;

  case FWorkType of
    ttClock:    CurrentTime := Now;
    ttRevTimer:
    begin
      CurrentTime := FTimerStart + (FExamPeriod / MinsPerDay) - Now +
        FTimerPaused/SecsPerDay;

      if CurrentTime <= 0 then
      begin
        if Assigned(FOnTimeOver) then
          FOnTimeOver(Self);
      end;
    end;

    ttPosTimer: CurrentTime := Now - FTimerStart - FTimerPaused/SecsPerDay;
  end;

  // 暂停刷新
  if (FPausedRefreshTimes <> -1) and (WorkStatus = tsPaused) then
  begin
    if (FTimerPaused mod FPausedRefreshTimes) = 0 then
    begin
      if Assigned(FOnPausedRefresh) then
        FOnPausedRefresh(Self);
    end;
  end;
end;

procedure TCKM_TIMER.Paused;
begin
  case FWorkStatus of
    tsRuning:
    begin
      WorkStatus := tsPaused;

      if Assigned(FOnPausedRefresh) then
        FOnPausedRefresh(Self);
    end;
  end;
end;

procedure TCKM_TIMER.SetCurrentTime(const Value: TDateTime);
var
  nMin : Integer;
begin
  FCurrentTime := Value;
  nMin := MinuteOf(Value);

  if nMin <> FMins then
  begin
    FMins := nMin;
    if Assigned(FOnMinChange) then
      FOnMinChange(Self);
  end;
end;

procedure TCKM_TIMER.SetExamPeriod(const Value: Integer);
begin
  FExamPeriod := Value;
end;

procedure TCKM_TIMER.SetWorkType(const Value: TCKM_TIMER_TYPE);
begin
  FWorkType := Value;

end;

procedure TCKM_TIMER.Start;
begin
  case FWorkStatus of
    tsStop:
    begin
      Timer.Enabled := True;
      OnTimer(Timer);
      FTimerStart := Now;
      FTimerPaused := 0;
      WorkStatus := tsRuning;
      
    end;
    tsPaused:
    begin
      Timer.Enabled := True;
      WorkStatus := tsRuning;
    end;
  end;
end;

procedure TCKM_TIMER.Stop;
begin
  WorkStatus := tsStop;

  OnTimer(Timer);
  Timer.Enabled := False;
  FCurrentTime := -1;
end;

function GetPowerWorkStatusName( AStatus : TPOWER_WORK_STATUS ) : string;
begin
  case AStatus of
    psOff: Result := '关闭';
    psOn:  Result := '打开';
    psAlarm: Result := '报警';
  end;
end;

function GetPowerOutputTypeName( AType : TPOWER_OUTPUT_TYPE ) : string;
begin
  case AType of
    ptThree  : Result := '三相三线';
    ptFour   : Result := '三相四线';
    ptFour577: Result := '三相四线57.7';
  else
    Result := '单相';
  end;
end;

function GetPowerOutputDefault( AType : TPOWER_OUTPUT_TYPE ) : TPOWER_OUTPUT;
begin
  case AType of
    ptSingle:
    with Result do
    begin
      Ua := True;
      Ub := True;
      Uc := True;
      Un := True;
      Ia := True;
      Ib := True;
      Ic := True;
    end;

    ptThree:
    with Result do
    begin
      Ua := True;
      Ub := True;
      Uc := True;
      Un := False;
      Ia := True;
      Ib := True;
      Ic := True;
    end;

    ptFour, ptFour577:
    with Result do
    begin
      Ua := True;
      Ub := True;
      Uc := True;
      Un := True;
      Ia := True;
      Ib := True;
      Ic := True;
    end;
  end;
end;

function GetPowerAlarmDescription( APAlarm : TPOWER_ALARM ) : string;
var
  i : TPOWER_ALARM_TYPE;
begin
  Result := '';

  if APAlarm <> [] then
  begin
    for i in APAlarm do
    begin
      case i of
        plUa : Result := Result + 'Ua短路,';
        plIa : Result := Result + 'Ia开路,';
        plUb : Result := Result + 'Ub短路,';
        plIb : Result := Result + 'Ib开路,';
        plUc : Result:= Result +  'Uc短路,';
        PlIc : Result:= Result +  'Ic开路,';
      end;
    end;

    Result := Copy( Result, 1, Length( Result ) - 1 );
  end;
end;

{ TCKM_POWER }

procedure TCKM_POWER.Adjust;
begin
  if FWorkStatus = psOn then
    if Assigned( FOnOperation ) then
      FOnOperation( Self, poAdjust );
end;

procedure TCKM_POWER.Assign(Source: TPersistent);
begin
  inherited;
  Assert( Source is TCKM_POWER );
  
  FPowerOutputType := TCKM_POWER( Source ).PowerOutputType;
  FPowerOutput     := TCKM_POWER( Source ).PowerOutput;
  FCurrent         := TCKM_POWER( Source ).Current;
  FVoltage         := TCKM_POWER( Source ).Voltage;
  FMaxVoltage      := TCKM_POWER( Source ).MaxVoltage;
  FAngle           := TCKM_POWER( Source ).Angle;
  FWorkStatus      := TCKM_POWER( Source ).WorkStatus;
  FCurrentPercent  := TCKM_POWER( Source ).CurrentPercent;
  FVoltagePercent  := TCKM_POWER( Source ).VoltagePercent;
end;

constructor TCKM_POWER.Create(AOwner: TComponent);
begin
  inherited;
  FDefAngle := 0;
  FMaxAngle := 360;
  FMinAngle := -180;
  FAdjustAngle3:= 0;
  FAdjustAngle4:= 0;

  SetPowerOutputType(ptFour);

  FWorkStatus := psOff;
end;

function TCKM_POWER.GetAdjustAngle: Double;
begin
  if FPowerOutputType = ptThree then
    Result := FAdjustAngle3
  else
    Result := FAdjustAngle4
end;

function TCKM_POWER.GetAlarmDescription: string;
begin
  Result := GetPowerAlarmDescription( FAlarm );
end;

function TCKM_POWER.GetFullMaxCur: Double;
begin
  Result := 5;
end;

function TCKM_POWER.GetFullMaxVol: Double;
begin
  case FPowerOutputType of
    ptThree: Result := 100;
  else
    Result := 220;
  end;
end;

procedure TCKM_POWER.SetAlarm(const Value: TPOWER_ALARM);
begin
  try
    FAlarm := Value;

    if FAlarm <> [] then
    begin
      FWorkStatus := psAlarm;

      if Assigned( FOnAlarm ) then
        FOnAlarm ( Self );
    end;
  finally

  end;
end;

procedure TCKM_POWER.SetAngle(const Value: Double);
begin
  if FAngle <> Value then
  begin
    // 控制角度在 0.0-359.9之间
    FAngle := (round(Value * 10) mod 3600)/10;

    if FAngle < FMinAngle then
      FAngle := FMinAngle
    else if FAngle > FMaxAngle then
      FAngle := FMaxAngle;
  end;
end;

procedure TCKM_POWER.SetCurrent(const Value: Double);
begin
  if FCurrent <> Value then
  begin
    if Value > FMaxCurrent then
      FCurrent := FMaxCurrent
    else if Value < 0 then
      FCurrent := 0
    else
      FCurrent := Value;

    FCurrentPercent := Round( FCurrent / GetFullMaxCur * 100 );
  end;
end;

procedure TCKM_POWER.SetCurrentPercent(const Value: Integer);
begin
  if FCurrentPercent <> Value then
    SetCurrent(GetFullMaxCur * Value / 100);
end;

procedure TCKM_POWER.SetMaxCurrent(const Value: Double);
begin
  FMaxCurrent := Value;

  if FCurrent > FMaxCurrent then
    FCurrent := FMaxCurrent;

  FCurrentPercent := Round( FCurrent / GetFullMaxCur * 100 );
end;

procedure TCKM_POWER.SetMaxVoltage(const Value: Double);
begin
  FMaxVoltage := Value;

  if FVoltage > FMaxVoltage then
    FVoltage := FMaxVoltage;

  FVoltagePercent := Round( FVoltage / GetFullMaxVol * 100 );
end;

procedure TCKM_POWER.SetPowerOutputType(const Value: TPOWER_OUTPUT_TYPE);
begin
  if Value <> FPowerOutputType then
  begin
    FPowerOutputType := Value;
    // 赋值最大电流
    MaxCurrent := 5;

    // 赋值最大电压
    case FPowerOutputType of
      ptThree: MaxVoltage := 100;
      ptFour577: MaxVoltage := 57.7;
    else
      MaxVoltage := 220;
    end;

    // 输出电压电流角度
//    SetAngle(FDefAngle);
//    SetVoltagePercent( Round(MaxVoltage/GetFullMaxVol) );
//    SetCurrentPercent( 100 );

    // 改变功率源输出状态
    SetPowerOutput(GetPowerOutputDefault( FPowerOutputType ));
  end;
end;

procedure TCKM_POWER.SetPowerOutput(const Value: TPOWER_OUTPUT);
begin
  if (FPowerOutput.Ua <> Value.Ua)or
    (FPowerOutput.Ub <> Value.Ub)or
    (FPowerOutput.Uc <> Value.Uc)or
    (FPowerOutput.Ia <> Value.Ia)or
    ((FPowerOutput.Ib <> Value.Ib))or
    (FPowerOutput.Ic <> Value.Ic)then
  begin
    FPowerOutput := Value;
  end;
end;

procedure TCKM_POWER.SetVoltage(const Value: Double);
begin
  if FVoltage <> Value then
  begin
    if Value > FMaxVoltage then
      FVoltage := FMaxVoltage
    else if Value < 0 then
      FVoltage := 0
    else
      FVoltage := Value;

    FVoltagePercent := Round( FVoltage / GetFullMaxVol * 100 );
  end;
end;

procedure TCKM_POWER.SetVoltagePercent(const Value: Integer);
begin
  if FVoltagePercent <> Value then
  begin
    SetVoltage(GetFullMaxVol * Value/100);
  end;

end;

procedure TCKM_POWER.TurnOff;
begin
  FWorkStatus := psOff;
  
  if Assigned( FOnOperation ) then
    FOnOperation( Self, poTurnOff );
end;

procedure TCKM_POWER.TurnOn;
begin
  FWorkStatus := psOn;

  if Assigned( FOnOperation ) then
    FOnOperation( Self, poTurnOn );
end;

{ TCKM_DEVICE }

procedure TCKM_DEVICE.Assign(Source: TPersistent);
begin
  Assert(Source is TCKM_DEVICE);
  FControlAdd := TCKM_DEVICE(Source).ControlAdd;
  FControlAdd2:= TCKM_DEVICE(Source).ControlAdd2;
  FSN         := TCKM_DEVICE(Source).SN;
  FVersion    := TCKM_DEVICE(Source).Version;
end;

constructor TCKM_DEVICE.Create(AOwner: TComponent);
begin
  inherited;
  FControlAdd := -1;
  FControlAdd2 := -1;
end;

{ TVOICE }

constructor TVOICE.Create(AOwner: TComponent);
begin
  inherited;
  FVoiceType := 1;
end;

{ TLFAlarm }

constructor TLFAlarm.Create(AOwner: TComponent);
begin
  inherited;
  FIsCurrentOverB:= False;
  FIsCurrentOverC:= False;
  FIsCurrentOverA:= False;
  FIsVolOut      := False;
  FIsControlPowerOn := False;
  FIsReplay := False;
end;

{ TCarrierControl }

constructor TCarrierControl.Create(AOwner: TComponent);
begin
  inherited;
  FIsOn := False;
end;

end.
