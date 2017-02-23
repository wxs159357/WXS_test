unit U_POWER_STATUS;

interface

uses SysUtils, Classes, U_CKM_DEVICE;

type
  /// <summary>
  /// 三相功率状态
  /// </summary>
  TPOWER_STATUS = class( TPersistent )
  private
    FPowerType : TPOWER_OUTPUT_TYPE;
    FFrequency : Double;
    FPowerFactor : Double;
    FPowerActive : Double;
    FPowerReactive : Double;
    FU1 : Double;
    FI1 : Double;
    FO1 : Double;
    FU2 : Double;
    FI2 : Double;
    FO2 : Double;
    FU3 : Double;
    FI3 : Double;
    FO3 : Double;
    FOU1U2 : Double;
    FOU1U3 : Double;
    FOnChanged : TNotifyEvent;
    FOwnerPower: TCKM_POWER;
  public
    /// <summary>
    /// 相线类型
    /// </summary>
    property PowerType : TPOWER_OUTPUT_TYPE read FPowerType write FPowerType;

    /// <summary>
    /// 频率
    /// </summary>
    property Frequency : Double read FFrequency write FFrequency;

    /// <summary>
    /// 功率因数
    /// </summary>
    property PowerFactor : Double read FPowerFactor write FPowerFactor;

    /// <summary>
    /// 有功功率
    /// </summary>
    property PowerActive : Double read FPowerActive write FPowerActive;

    /// <summary>
    /// 无功功率
    /// </summary>
    property PowerReactive : Double read FPowerReactive write FPowerReactive;

    /// <summary>
    /// 相线电压，电流及电压电流夹角
    /// </summary>
    property U1 : Double read FU1 write FU1;    // 三线时为U12
    property I1 : Double read FI1 write FI1;
    property O1 : Double read FO1 write FO1;    // 三线时为U12, I1夹角
    property U2 : Double read FU2 write FU2;    // 三线时不用
    property I2 : Double read FI2 write FI2;    // 三线时不用
    property O2 : Double read FO2 write FO2;    // 三线时不用
    property U3 : Double read FU3 write FU3;    // 三线时为U32
    property I3 : Double read FI3 write FI3;
    property O3 : Double read FO3 write FO3;    // 三线时为U32, I3夹角

    /// <summary>
    /// 电压与电压之间夹角
    /// </summary>
    property OU1U2 : Double read FOU1U2 write FOU1U2;
    property OU1U3 : Double read FOU1U3 write FOU1U3;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;

    /// <summary>
    /// 所属功率源
    /// </summary>
    property OwnerPower : TCKM_POWER read FOwnerPower write FOwnerPower;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    /// 清除
    /// </summary>
    procedure Clear;
  end;

/// <summary>
/// 校准返回的实时向量图数据
/// </summary>
procedure CalibratePowerStatus( AStatus : TPOWER_STATUS; APower : TCKM_POWER );

implementation

procedure CalibratePowerStatus( AStatus : TPOWER_STATUS; APower : TCKM_POWER );
  // 校准的值
  function CalibrateUValue( AU : array of Double; AStdU : Double ) : Double;
  const
    C_MAX_VALUE = 0.15;
  var
    i: Integer;
    dSum : Double;
    nCount : Integer;
  begin
    dSum := 0;
    nCount := 0;

    for i := 0 to Length( AU ) - 1 do
      if ( Abs( AU[ i ] - AStdU ) < C_MAX_VALUE ) and
         ( Abs( AU[ i ] - AStdU ) < C_MAX_VALUE ) then
      begin
        dSum := dSum + AU[ i ];
        Inc( nCount );
      end;

    if nCount > 0 then
      Result := AStdU - dSum / nCount
    else
      Result := 0;
  end;
var
  dValue : Double;
  dStdValue : Double;
begin
  if not ( Assigned( AStatus ) and Assigned( APower ) ) then
    Exit;

  if APower.WorkStatus = psOn then
    dStdValue := APower.VoltagePercent / 100
  else
    dStdValue := 0;

  with AStatus do
  begin
    if APower.PowerOutputType = ptThree then
    begin
      dValue := CalibrateUValue( [ U1, U3 ], dStdValue );
      U1 := U1 + dValue;
      U3 := U3 + dValue;
    end
    else
    begin
      dValue := CalibrateUValue( [ U1, U2, U3 ], dStdValue );
      U1 := U1 + dValue;
      U2 := U2 + dValue;
      U3 := U3 + dValue;
    end;
  end;
end;

{ TPOWER_STATUS }

procedure TPOWER_STATUS.Assign(Source: TPersistent);
begin
  Assert( Source is TPOWER_STATUS );

  FPowerType     := TPOWER_STATUS( Source ).PowerType     ;
  FFrequency     := TPOWER_STATUS( Source ).Frequency     ;
  FPowerFactor   := TPOWER_STATUS( Source ).PowerFactor   ;
  FPowerActive   := TPOWER_STATUS( Source ).PowerActive   ;
  FPowerReactive := TPOWER_STATUS( Source ).PowerReactive ;
  FU1            := TPOWER_STATUS( Source ).U1            ;
  FI1            := TPOWER_STATUS( Source ).I1            ;
  FO1            := TPOWER_STATUS( Source ).O1            ;
  FU2            := TPOWER_STATUS( Source ).U2            ;
  FI2            := TPOWER_STATUS( Source ).I2            ;
  FO2            := TPOWER_STATUS( Source ).O2            ;
  FU3            := TPOWER_STATUS( Source ).U3            ;
  FI3            := TPOWER_STATUS( Source ).I3            ;
  FO3            := TPOWER_STATUS( Source ).O3            ;
  FOU1U2         := TPOWER_STATUS( Source ).OU1U2         ;
  FOU1U3         := TPOWER_STATUS( Source ).OU1U3         ;
end;

procedure TPOWER_STATUS.Clear;
begin
  FFrequency     := 0;
  FPowerFactor   := 0;
  FPowerActive   := 0;
  FPowerReactive := 0;
  FU1            := 0;
  FI1            := 0;
  FO1            := 0;
  FU2            := 0;
  FI2            := 0;
  FO2            := 0;
  FU3            := 0;
  FI3            := 0;
  FO3            := 0;
  FOU1U2         := 0;
  FOU1U3         := 0;
end;

constructor TPOWER_STATUS.Create;
begin
  Clear;
end;

end.

