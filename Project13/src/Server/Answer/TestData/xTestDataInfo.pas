unit xTestDataInfo;

interface

uses System.Classes, System.SysUtils;

type
  /// <summary>
  /// 相线类别
  /// </summary>
  TPhaseType = ( tptThree,    // 三相三线
                 tptFour      // 三相四线
                );
type
  TSequenceType = (stABC, // 三线时为 AC
                   stACB,
                   stBAC,
                   stBCA,
                   stCAB,
                   stCBA  // 三线时为 CA
                   );

type
  /// <summary>
  /// 三相功率状态
  /// </summary>
  TTestDataInfo = class
  private
    FPhaseType: TPhaseType;
    FOU2U3: Double;
    FOU1U2: Double;
    FOU1U3: Double;
    FU2: Double;
    FU3: Double;
    FU1: Double;
    FI2: Double;
    FI3: Double;
    FO2: Double;
    FO3: Double;
    FI1: Double;
    FO1: Double;
    FIsCT2Reverse: Boolean;
    FIsCT3Reverse: Boolean;
    FUSequence: TSequenceType;
    FIsCT1Reverse: Boolean;
    FISequence: TSequenceType;
    FIsPT2Reverse: Boolean;
    FIsPT3Reverse: Boolean;
    FIsPT1Reverse: Boolean;
    FIsLoadL: Boolean;
    procedure SetI1(const Value: Double);
    procedure SetI2(const Value: Double);
    procedure SetI3(const Value: Double);
    procedure SetO1(const Value: Double);
    procedure SetO2(const Value: Double);
    procedure SetO3(const Value: Double);
    procedure SetOU1U2(const Value: Double);
    procedure SetOU1U3(const Value: Double);
    procedure SetOU2U3(const Value: Double);
    procedure SetU1(const Value: Double);
    procedure SetU2(const Value: Double);
    procedure SetU3(const Value: Double);
    procedure SetIsCT1Reverse(const Value: Boolean);
    procedure SetIsCT2Reverse(const Value: Boolean);
    procedure SetIsCT3Reverse(const Value: Boolean);
    procedure SetISequence(const Value: TSequenceType);
    procedure SetIsPT1Reverse(const Value: Boolean);
    procedure SetIsPT2Reverse(const Value: Boolean);
    procedure SetIsPT3Reverse(const Value: Boolean);
    procedure SetUSequence(const Value: TSequenceType);
    procedure SetLoadL(const Value: Boolean);

  public
    /// <summary>
    /// 相线类型
    /// </summary>
    property PhaseType : TPhaseType read FPhaseType write FPhaseType;

    /// <summary>
    /// 电压，电流
    /// </summary>
    property U1 : Double read FU1 write SetU1;   // 三线时为U12
    property U2 : Double read FU2 write SetU2;   // 三线时为U32
    property U3 : Double read FU3 write SetU3;   // 三线时为U13
    property I1 : Double read FI1 write SetI1;
    property I2 : Double read FI2 write SetI2;    // 三线时不用
    property I3 : Double read FI3 write SetI3;
    property O1 : Double read FO1 write SetO1;    // 三线时为U12, I1夹角
    property O2 : Double read FO2 write SetO2;    // 三线时不用
    property O3 : Double read FO3 write SetO3;    // 三线时为U32, I3夹角

    /// <summary>
    /// 电压与电压之间夹角
    /// </summary>
    property OU1U2 : Double read FOU1U2 write SetOU1U2; // 三线时为U12, U32夹角
    property OU1U3 : Double read FOU1U3 write SetOU1U3;
    property OU2U3 : Double read FOU2U3 write SetOU2U3;

    /// <summary>
    /// 电压相序
    /// </summary>
    property USequence : TSequenceType read FUSequence write SetUSequence;

    /// <summary>
    /// 电流相序
    /// </summary>
    property ISequence : TSequenceType read FISequence write SetISequence;

    /// <summary>
    /// 负载性质 是否是容性
    /// </summary>
    property IsLoadL : Boolean read FIsLoadL write SetLoadL;

    /// <summary>
    /// PT1极性反
    /// </summary>
    property IsPT1Reverse : Boolean read FIsPT1Reverse write SetIsPT1Reverse;

    /// <summary>
    /// PT2极性反
    /// </summary>
    property IsPT2Reverse : Boolean read FIsPT2Reverse write SetIsPT2Reverse;

    /// <summary>
    /// PT3极性反
    /// </summary>
    property IsPT3Reverse : Boolean read FIsPT3Reverse write SetIsPT3Reverse;

    /// <summary>
    /// CT1极性反
    /// </summary>
    property IsCT1Reverse : Boolean read FIsCT1Reverse write SetIsCT1Reverse;

    /// <summary>
    /// CT2极性反
    /// </summary>
    property IsCT2Reverse : Boolean read FIsCT2Reverse write SetIsCT2Reverse;

    /// <summary>
    /// CT3极性反
    /// </summary>
    property IsCT3Reverse : Boolean read FIsCT3Reverse write SetIsCT3Reverse;

  public
    constructor Create;

    /// <summary>
    /// 清除
    /// </summary>
    procedure Clear;
  end;

implementation


{ TTestDataInfo }

procedure TTestDataInfo.Clear;
begin
  FU1    := 0;
  FU2    := 0;
  FU3    := 0;
  FI1    := 0;
  FI2    := 0;
  FI3    := 0;
  FO1    := 0;
  FO2    := 0;
  FO3    := 0;
  FOU1U2 := 0;
  FOU1U3 := 0;
  FOU2U3 := 0;
end;

constructor TTestDataInfo.Create;
begin
  FPhaseType := tptThree;
  FIsPT1Reverse:= False;
  FIsPT2Reverse:= False;
  FIsPT3Reverse:= False;
  FIsCT1Reverse:= False;
  FIsCT2Reverse:= False;
  FIsCT3Reverse:= False;
end;

procedure TTestDataInfo.SetI1(const Value: Double);
begin

end;

procedure TTestDataInfo.SetI2(const Value: Double);
begin

end;

procedure TTestDataInfo.SetI3(const Value: Double);
begin

end;

procedure TTestDataInfo.SetIsCT1Reverse(const Value: Boolean);
begin
  FIsCT1Reverse := Value;
end;

procedure TTestDataInfo.SetIsCT2Reverse(const Value: Boolean);
begin
  FIsCT2Reverse := Value;
end;

procedure TTestDataInfo.SetIsCT3Reverse(const Value: Boolean);
begin
  FIsCT3Reverse := Value;
end;

procedure TTestDataInfo.SetISequence(const Value: TSequenceType);
begin
  FISequence := Value;
end;

procedure TTestDataInfo.SetIsPT1Reverse(const Value: Boolean);
begin
  FIsPT1Reverse := Value;
end;

procedure TTestDataInfo.SetIsPT2Reverse(const Value: Boolean);
begin
  FIsPT2Reverse := Value;
end;

procedure TTestDataInfo.SetIsPT3Reverse(const Value: Boolean);
begin
  FIsPT3Reverse := Value;
end;

procedure TTestDataInfo.SetLoadL(const Value: Boolean);
begin
  FIsLoadL := Value;
end;

procedure TTestDataInfo.SetO1(const Value: Double);
begin

end;

procedure TTestDataInfo.SetO2(const Value: Double);
begin

end;

procedure TTestDataInfo.SetO3(const Value: Double);
begin

end;

procedure TTestDataInfo.SetOU1U2(const Value: Double);
begin

end;

procedure TTestDataInfo.SetOU1U3(const Value: Double);
begin

end;

procedure TTestDataInfo.SetOU2U3(const Value: Double);
begin

end;

procedure TTestDataInfo.SetU1(const Value: Double);
begin

end;

procedure TTestDataInfo.SetU2(const Value: Double);
begin

end;

procedure TTestDataInfo.SetU3(const Value: Double);
begin

end;

procedure TTestDataInfo.SetUSequence(const Value: TSequenceType);
begin
  FUSequence := Value;
end;

end.
