unit xProtocolDev;

interface

uses System.Types, xTypes, xConsts, System.Classes, xFunction,
  system.SysUtils, xVCL_FMX, System.DateUtils;

type
  /// <summary>
  /// 通讯设备基类
  /// </summary>
  TProtocolDevBase = class
  private
    FControlAdd: Integer;
    FSN: String;
    FVersion: string;
    FOwnerDEV: TObject;

  protected
    procedure SetSN(const Value: String); virtual;
    procedure SetControlAdd(const Value: Integer);  virtual;
  public
    constructor Create; virtual;
    procedure Assign(Source: TObject); virtual;

    /// <summary>
    /// 设备序号
    /// </summary>
    property SN : String read FSN write SetSN;

    /// <summary>
    /// 控制地址
    /// </summary>
    property ControlAdd : Integer read FControlAdd write SetControlAdd;

    /// <summary>
    /// 版本
    /// </summary>
    property Version : string read FVersion write FVersion;

    /// <summary>
    /// 所有者
    /// </summary>
    property OwnerDEV : TObject read FOwnerDEV write FOwnerDEV;
  end;

type
  /// <summary>
  /// 语音对象
  /// </summary>
  TVoice = class(TProtocolDevBase)
  private
    FVoiceType: Integer;

  public
    constructor Create; override;

    /// <summary>
    /// 语音提示类型
    /// 考试时间开始         01
    /// 考试时间到           02
    /// 考试时间还剩十分钟   03
    /// 考试时间还剩五分钟   04
    /// </summary>
    property VoiceType : Integer read FVoiceType write FVoiceType;
  end;

type
  TMinuteLeftEvent = procedure(nMinuteLeft: Integer) of object;

type
  /// <summary>
  /// 时钟类型
  /// </summary>
  TWorkTimerType = ( ttClock,    // 时钟
                      ttPosTimer, // 正计时
                      ttRevTimer  // 倒计时
                      );

  /// <summary>
  /// 获取计时类型名称
  /// </summary>
  function GetTimerTypeName( AType : TWorkTimerType ) : string;

type
  /// <summary>
  /// 计时器
  /// </summary>
  TPDTimer = class( TProtocolDevBase )
  private
    FEnabled: Boolean;
    FWorkType: TWorkTimerType;
    FOnMinuteLeft: TMinuteLeftEvent;
    FCurrentTime: TDateTime;
    FStartTime : TDateTime; // 开始时间
    FPauseTimeStart : TDateTime; // 暂停开始时间
    FPauseTimeValue : TDateTime; // 暂停总时间
    FPaused: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetWorkType(const Value: TWorkTimerType);
    procedure SetPaused(const Value: Boolean);
    /// <summary>
    /// 清除计时临时对象
    /// </summary>
    procedure ClearTime;
    function GetCurrentTime: TDateTime;
    procedure SetCurrentTime(const Value: TDateTime);
  protected
    /// <summary>
    /// 内部定时器
    /// </summary>
    Timer : TMyTimer;

    /// <summary>
    /// 内部定时器计时
    /// </summary>
    procedure OnTimer( Sender : TObject );

    /// <summary>
    /// 更新时间
    /// </summary>
    procedure UpdateTime;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    /// <summary>
    /// 计时方式
    /// </summary>
    property WorkType : TWorkTimerType read FWorkType write SetWorkType;

    /// <summary>
    /// 是否启动计时
    /// </summary>
    property Enabled : Boolean read FEnabled write SetEnabled;

    /// <summary>
    /// 当前的时间
    /// </summary>
    property CurrentTime : TDateTime read GetCurrentTime write SetCurrentTime;

    /// <summary>
    /// 暂停
    /// </summary>
    property Paused : Boolean read FPaused write SetPaused;

    /// <summary>
    /// 剩余时间提示 只有倒计时才有
    /// </summary>
    property OnSecLeft: TMinuteLeftEvent read FOnMinuteLeft write FOnMinuteLeft;
  end;

implementation

{ TProtocolDevBase }

procedure TProtocolDevBase.Assign(Source: TObject);
begin
  Assert(Source is TProtocolDevBase);
  FControlAdd := TProtocolDevBase(Source).ControlAdd;
  FSN         := TProtocolDevBase(Source).SN;
  FVersion   := TProtocolDevBase(Source).Version;
end;

constructor TProtocolDevBase.Create;
begin
  FControlAdd := -1;
end;

procedure TProtocolDevBase.SetControlAdd(const Value: Integer);
begin
  FControlAdd := Value;
end;

procedure TProtocolDevBase.SetSN(const Value: String);
begin
  FSN := Value;
end;


{ TVoice }

constructor TVoice.Create;
begin
  inherited;
  FVoiceType := 0;
end;

{ TPDTimer }

function GetTimerTypeName( AType : TWorkTimerType ) : string;
begin
  case AType of
    ttClock: Result := '时钟';
    ttPosTimer: Result := '正计时';
    ttRevTimer: Result := '倒计时';
  else
    Result := '未定义';
  end;
end;

procedure TPDTimer.ClearTime;
begin

  FStartTime := 0;
  FPauseTimeStart := 0;
  FPauseTimeValue := 0;
end;

constructor TPDTimer.Create;
begin
  inherited;
  Timer := TMyTimer.Create( nil );
  Timer.Enabled := False;
  Timer.OnTimer := OnTimer;

  FEnabled    := False;
  FWorkType   := ttClock;
  FCurrentTime := 0;
  ClearTime;
end;

destructor TPDTimer.Destroy;
begin
  Timer.Free;
  inherited;
end;

function TPDTimer.GetCurrentTime: TDateTime;
begin
  if FEnabled then
  begin
    case FWorkType of
      ttClock: Result := Now;
      ttPosTimer:
      begin
        if FPaused then
        begin
          Result := FPauseTimeStart - FStartTime - FPauseTimeValue + FCurrentTime;
        end
        else
        begin
          Result := Now - FStartTime - FPauseTimeValue + FCurrentTime;
        end;
      end;
      ttRevTimer:
      begin
        if FPaused then
        begin
          Result := FCurrentTime - (FPauseTimeStart - FStartTime - FPauseTimeValue);
        end
        else
        begin
          Result := FCurrentTime - (Now - FStartTime - FPauseTimeValue);
        end;
      end;
    else
      Result := FCurrentTime;
    end;
  end
  else
  begin
    Result := FCurrentTime;
  end;

end;

procedure TPDTimer.OnTimer(Sender: TObject);
begin
  UpdateTime;
end;

procedure TPDTimer.SetCurrentTime(const Value: TDateTime);
begin
  if not FEnabled then
  begin
    FCurrentTime := Value;
  end;
end;

procedure TPDTimer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Timer.Enabled := FEnabled;

  if FEnabled then
  begin
    ClearTime;
    FStartTime := Now;
  end;
end;

procedure TPDTimer.SetPaused(const Value: Boolean);
begin
  if FWorkType <> ttClock then
  begin
    if FEnabled then
    begin
      FPaused := Value;

      if FPaused then
      begin
        FPauseTimeStart := now;
      end
      else
      begin
        FPauseTimeValue := FPauseTimeValue + Now - FPauseTimeStart;
      end;
    end;
  end;
end;

procedure TPDTimer.SetWorkType(const Value: TWorkTimerType);
begin
  FWorkType := Value;
end;

procedure TPDTimer.UpdateTime;
var
  dtTemp : TDateTime;
begin
  if not Enabled then
    Exit;

  if FWorkType = ttRevTimer then
  begin
    dtTemp := CurrentTime;

    if SecondOf(dtTemp) = 0 then
    begin
      if Assigned(FOnMinuteLeft) then
      begin
        FOnMinuteLeft(MinuteOfTheDay(dtTemp));
      end;
    end;
  end;
end;

end.



