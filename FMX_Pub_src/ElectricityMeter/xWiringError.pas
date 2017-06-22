unit xWiringError;

interface

uses System.Classes, System.SysUtils, xFunction;

type
  /// <summary>
  /// 相序故障
  /// </summary>
  TPHASE_RECT = class
  private
    FAValue: Boolean;
    FBValue: Boolean;
    FCValue: Boolean;
    FOnChanged: TNotifyEvent;
    FNValue: Boolean;
    procedure SetAValue(const Value: Boolean);
    procedure SetBValue(const Value: Boolean);
    procedure SetCValue(const Value: Boolean);
    procedure Changed(Sender: TObject);
    procedure SetNValue(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(source : TPHASE_RECT);

    property AValue : Boolean read FAValue write SetAValue;
    property BValue : Boolean read FBValue write SetBValue;
    property CValue : Boolean read FCValue write SetCValue;
    property NValue : Boolean read FNValue write SetNValue;
    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  end;

type
  /// <summary>
  /// 二次故障
  /// </summary>
  TSECOND_ERROR = class
  private
    FSenUGroundBroken: Boolean;
    FSenUReverse: TPHASE_RECT;
    FSenIGroundBroken: TPHASE_RECT;
    FSenUBroken: TPHASE_RECT;
    FSenIShort: TPHASE_RECT;
    FSenIReverse: TPHASE_RECT;
    FSenIBroken: TPHASE_RECT;
    FOnChanged: TNotifyEvent;

    procedure setSenUGroundBroken(const Value: Boolean);
    procedure Changed(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source : TSECOND_ERROR);

    /// <summary>
    /// 电压开路
    /// </summary>
    property SenUBroken   : TPHASE_RECT read FSenUBroken write FSenUBroken;

    /// <summary>
    /// 电压极性反
    /// </summary>
    property SenUReverse   : TPHASE_RECT read FSenUReverse write FSenUReverse;

    /// <summary>
    /// 电压地线断开
    /// </summary>
    property SenUGroundBroken : Boolean read FSenUGroundBroken write setSenUGroundBroken;

    /// <summary>
    /// 电流开路
    /// </summary>
    property SenIBroken   : TPHASE_RECT read FSenIBroken write FSenIBroken;

    /// <summary>
    /// 电流极性反
    /// </summary>
    property SenIReverse   : TPHASE_RECT read FSenIReverse write FSenIReverse;

    /// <summary>
    /// 电流短路
    /// </summary>
    property SenIShort : TPHASE_RECT read FSenIShort write FSenIShort;

    /// <summary>
    /// 电流接地断开
    /// </summary>
    property SenIGroundBroken : TPHASE_RECT read FSenIGroundBroken write FSenIGroundBroken;

    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  end;


type
  /// <summary>
  /// 项类型
  /// </summary>
  TPHASE_TYPE = (ptNot, ptA, ptB, ptC, ptN);

type
  /// <summary>
  /// 原件连接
  /// </summary>
  TORGAN_LINK = class
  private
    FOrganType: Integer;
    FOrganIn2: TPHASE_TYPE;
    FOrganIn3: TPHASE_TYPE;
    FOrganIn1: TPHASE_TYPE;
    FOrganOut2: TPHASE_TYPE;
    FOrganOut3: TPHASE_TYPE;
    FOrganOut1: TPHASE_TYPE;
    FOnChanged: TNotifyEvent;
    procedure Changed(Sender: TObject);
    procedure SetOrganType(const Value: Integer);
    procedure SetOrganIn1(const Value: TPHASE_TYPE);
    procedure SetOrganIn2(const Value: TPHASE_TYPE);
    procedure SetOrganIn3(const Value: TPHASE_TYPE);
    procedure SetOrganOut1(const Value: TPHASE_TYPE);
    procedure SetOrganOut2(const Value: TPHASE_TYPE);
    procedure SetOrganOut3(const Value: TPHASE_TYPE);
    function GetOrganStr: string;
    procedure SetOrganStr(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 原件类型 0三相四线电压，1三相四线电流， 2三相三线电压，3三相三相电流 4单相
    /// </summary>
    property OrganType : Integer read FOrganType write SetOrganType;

    /// <summary>
    /// 第1原件 进
    /// </summary>
    property OrganIn1 : TPHASE_TYPE read FOrganIn1 write SetOrganIn1;

    /// <summary>
    /// 第1原件 出
    /// </summary>
    property OrganOut1 : TPHASE_TYPE read FOrganOut1 write SetOrganOut1;

    /// <summary>
    /// 第2原件 进
    /// </summary>
    property OrganIn2 : TPHASE_TYPE read FOrganIn2 write SetOrganIn2;

    /// <summary>
    /// 第2原件 出
    /// </summary>
    property OrganOut2 : TPHASE_TYPE read FOrganOut2 write SetOrganOut2;

    /// <summary>
    /// 第3原件 进
    /// </summary>
    property OrganIn3 : TPHASE_TYPE read FOrganIn3 write SetOrganIn3;

    /// <summary>
    /// 第3原件 出
    /// </summary>
    property OrganOut3 : TPHASE_TYPE read FOrganOut3 write SetOrganOut3;

    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;

    /// <summary>
    /// 相序描述  三相三线不能用b代替n   比如电流相序 AN, NA, AC, CA, NA, NC
    /// </summary>
    property OrganStr : string read GetOrganStr write SetOrganStr;
  end;

/// <summary>
/// 获取所有相许列表
/// </summary>
function GetUSequenceStrAll : string;

/// <summary>
/// 获取电流相许列表
/// </summary>
function GetISequenceStrAll(bIsThree, bIsClear : Boolean) : string;

type
  /// <summary>
  ///  电能表(终端)故障
  /// </summary>
  TMETER_ERROR = class
  private
    FUSequence: TORGAN_LINK;
    FISequence: TORGAN_LINK;
    FIReverse: TPHASE_RECT;
    FUBroken: TPHASE_RECT;
    FIShort: TPHASE_RECT;
    FIBroken: TPHASE_RECT;
    FOnChanged: TNotifyEvent;
    FMeterID: Integer;
    FMeterType: Integer;
    procedure Changed(Sender: TObject);
    procedure SetMeterType(const Value: Integer);
    procedure setISequence(const Value: TORGAN_LINK);
    procedure setUSequence(const Value: TORGAN_LINK);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source : TMETER_ERROR);

    /// <summary>
    /// 电表类型 0三相四线 1三相三线
    /// </summary>
    property MeterType : Integer read FMeterType write SetMeterType;

    /// <summary>
    /// 电表ID
    /// </summary>
    property MeterID : Integer read FMeterID write FMeterID;

    /// <summary>
    /// 电压相序
    /// </summary>
    property USequence : TORGAN_LINK read FUSequence write setUSequence;

    /// <summary>
    /// 失压
    /// </summary>
    property UBroken : TPHASE_RECT read FUBroken write FUBroken;

    /// <summary>
    /// 电流相序
    /// </summary>
    property ISequence  : TORGAN_LINK read FISequence  write setISequence;

    /// <summary>
    /// 开路
    /// </summary>
    property IBroken   : TPHASE_RECT read FIBroken write FIBroken;

    /// <summary>
    /// 短路
    /// </summary>
    property IShort   : TPHASE_RECT read FIShort write FIShort;

    /// <summary>
    /// 反接
    /// </summary>
    property IReverse   : TPHASE_RECT read FIReverse write FIReverse;

    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  end;

type
  /// <summary>
  /// 相线类型 ，ptfSingle 单相， ptThree 三线， ptFour 四线
  /// </summary>
  TWEF_PHASE_TYPE = (ptfSingle, ptfThree, ptfFour);


type
  /// <summary>
  /// 接线错误
  /// </summary>
  TWIRINGF_ERROR = class
  private
    FPhaseType : TWEF_PHASE_TYPE;
    FMeterError: TMETER_ERROR;
    FFirError: TPHASE_RECT;
    FSenError: TSECOND_ERROR;
    FOnChanged: TNotifyEvent;
    FIsTrans: Boolean;
    FDiagramType: Integer;

    procedure SetPhaseType(const Value: TWEF_PHASE_TYPE);

    function GetIsCorrect: Boolean;
    function GetID: string;
    procedure SetID(const Value: string);
    procedure Changed(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);
    /// <summary>
    /// 错误接线编码
    /// </summary>
    property ID : string read GetID write SetID;

    /// <summary>
    /// 是否是正确接线
    /// </summary>
    property IsCorrect : Boolean read GetIsCorrect;

    /// <summary>
    /// 设置正确接线
    /// </summary>
    procedure SetCorrect;

    /// <summary>
    /// 相线
    /// </summary>
    property PhaseType : TWEF_PHASE_TYPE read FPhaseType write SetPhaseType;

    /// <summary>
    /// 是否是连接互感器
    /// </summary>
    property IsTrans : Boolean read FIsTrans write FIsTrans;

    /// <summary>
    /// 接线方式 "
    ///【单相】0：直通
    ///【三线】0：直通
    ///1：直通 有无功
    ///2：四线制多功能
    ///3：四线制有功-无功
    ///4：简化接线多功能
    ///5：简化接线有功-无功
    ///【四线】0：直通
    ///1：直通 有无功
    ///2：六线制多功能
    ///3：六线制有功-无功
    ///4：简化接线多功能
    ///5：简化接线有功-无功"
    /// </summary>
    property DiagramType :  Integer read FDiagramType write FDiagramType;

    /// <summary>
    /// 一次故障
    /// </summary>
    property FirError   : TPHASE_RECT read FFirError write FFirError;

    /// <summary>
    /// 二次故障
    /// </summary>
    property SenError :  TSECOND_ERROR read FSenError write FSenError;

    /// <summary>
    /// 表位故障
    /// </summary>
    property MeterError : TMETER_ERROR read FMeterError write FMeterError;

    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;

    /// <summary>
    /// 获取错误接线描述 nAbc (0:abc, 1:uvw, 2:123)
    /// </summary>
    function GetDescription(nAbc: Integer=0; bPTCT : Boolean=True): string;
  end;



implementation

{ TPHASE_RECT }


procedure TPHASE_RECT.Assign(source: TPHASE_RECT);
begin
  Assert(source is TPHASE_RECT);

  FAValue := TPHASE_RECT(source).AValue;
  FBValue := TPHASE_RECT(source).BValue;
  FCValue := TPHASE_RECT(source).CValue;
  FNValue := TPHASE_RECT(source).NValue;
end;

procedure TPHASE_RECT.Changed(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TPHASE_RECT.Create;
begin
  FAValue:= False;
  FBValue:= False;
  FCValue:= False;
  FNValue:= False;
end;

procedure TPHASE_RECT.SetAValue(const Value: Boolean);
begin
  if Value <> FAValue then
  begin
    FAValue := Value;
    Changed(Self);
  end;
end;

procedure TPHASE_RECT.SetBValue(const Value: Boolean);
begin
  if Value <> FBValue then
  begin
    FBValue := Value;
    Changed(Self);
  end;
end;

procedure TPHASE_RECT.SetCValue(const Value: Boolean);
begin
  if Value <> FCValue then
  begin
    FCValue := Value;
    Changed(Self);
  end;
end;
procedure TPHASE_RECT.SetNValue(const Value: Boolean);
begin
  if Value <> FNValue then
  begin
    FNValue := Value;
    Changed(Self);
  end;
end;

{ TORGAN_LINK }

procedure TORGAN_LINK.Changed(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TORGAN_LINK.Create;
begin
  SetOrganType(0);
end;

destructor TORGAN_LINK.Destroy;
begin

  inherited;
end;

function TORGAN_LINK.GetOrganStr: string;
  function GetStr(APhase: TPHASE_TYPE):string;
  begin
    case APhase of
      ptA : Result := 'a';
      ptB : Result := 'b';
      ptC : Result := 'c';
      ptN : Result := 'n';
    else
      Result := 'z';
    end;
  end;
begin
  //原件类型 0三相四线电压，1三相四线电流， 2三相三线电压，3三相三相电流 4单相
  case FOrganType of
    0, 1 : Result := GetStr(FOrganIn1)+ GetStr(FOrganIn2)+ GetStr(FOrganIn3);
    2    : Result := GetStr(FOrganIn1)+ GetStr(FOrganOut1)+ GetStr(FOrganIn2);
    3    : Result := GetStr(FOrganIn1)+ GetStr(FOrganIn2);
  else
    Result := 'abc'
  end;
end;

procedure TORGAN_LINK.SetOrganType(const Value: Integer);
begin
  FOrganType := Value;
  case FOrganType of
    0, 1 :
    begin
      FOrganIn1  := ptA;
      FOrganOut1 := ptN;
      FOrganIn2  := ptB;
      FOrganOut2 := ptN;
      FOrganIn3  := ptC;
      FOrganOut3 := ptN;
    end;
    2 :
    begin
      FOrganIn1  := ptA;
      FOrganOut1 := ptB;
      FOrganIn2  := ptC;
      FOrganOut2 := ptB;
      FOrganIn3  := ptNot;
      FOrganOut3 := ptNot;

    end;
    3 :
    begin
      FOrganIn1  := ptA;
      FOrganOut1 := ptN;
      FOrganIn2  := ptC;
      FOrganOut2 := ptN;
      FOrganIn3  := ptNot;
      FOrganOut3 := ptNot;
    end;
    4 :
    begin
      FOrganIn1  := ptA;
      FOrganOut1 := ptN;
      FOrganIn2  := ptNot;
      FOrganOut2 := ptNot;
      FOrganIn3  := ptNot;
      FOrganOut3 := ptNot;
    end;
  end;
end;

procedure TORGAN_LINK.SetOrganStr(const Value: string);
  function GetPhase(sPhase : char):TPHASE_TYPE;
  begin
    case sPhase of
      'a' : Result := ptA;
      'b' : Result := ptB;
      'c' : Result := ptC;
      'n' : Result := ptN;
    else
      Result := ptN;
    end;
  end;
  function GetNotChar : Char;
  begin
    Result := 'a';

    if Pos('a', Value) = 0 then
      Result := 'a';

    if Pos('b', Value) = 0 then
      Result := 'b';

    if Pos('c', Value) = 0 then
      Result := 'c';

    if Pos('n', Value) = 0 then
      Result := 'n';
  end;

  function GetThreeNotChar : Char;
  begin
    Result := 'a';

    if Pos('a', Value) = 0 then
      Result := 'a';

    if Pos('c', Value) = 0 then
      Result := 'c';

    if Pos('n', Value) = 0 then
      Result := 'n';
  end;
begin
  {$ZEROBASEDSTRINGS ON}  // 开启字符串下标从0开始
  //0三相四线电压，1三相四线电流， 2三相三线电压，3三相三相电流 4单相
  case FOrganType of
    0, 1 :
    begin
      if Length(Value) = 3 then
      begin
        FOrganIn1  := GetPhase(Value[0]);
        FOrganIn2  := GetPhase(Value[1]);
        FOrganIn3  := GetPhase(Value[2]);
        FOrganOut1 := GetPhase(GetNotChar);
        FOrganOut2 := FOrganOut1;
        FOrganOut3 := FOrganOut1;
      end;
    end;
    2    :
    begin
      FOrganIn1  := GetPhase(Value[0]);
      FOrganIn2  := GetPhase(Value[2]);
      FOrganOut1 := GetPhase(Value[1]);
      FOrganOut2 := FOrganOut1;
    end;
    3    :
    begin
      if Length(Value) = 2 then
      begin
        FOrganIn1  := GetPhase(Value[0]);
        FOrganIn2  := GetPhase(Value[1]);
        FOrganOut1 := GetPhase(GetThreeNotChar);
        FOrganOut2 := FOrganOut1;
      end;
    end;
  end;

  {$ZEROBASEDSTRINGS OFF}  // 关闭字符串下标从0开始
end;

procedure TORGAN_LINK.SetOrganIn1(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganIn1 then
  begin
    FOrganIn1 := Value;
    Changed(Self);
  end;
end;

procedure TORGAN_LINK.SetOrganIn2(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganIn2 then
  begin
    FOrganIn2 := Value;
    Changed(Self);
  end;
end;

procedure TORGAN_LINK.SetOrganIn3(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganIn3 then
  begin
    FOrganIn3 := Value;
    Changed(Self);
  end;
end;

procedure TORGAN_LINK.SetOrganOut1(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganOut1 then
  begin
    FOrganOut1 := Value;
    Changed(Self);
  end;
end;

procedure TORGAN_LINK.SetOrganOut2(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganOut2 then
  begin
    FOrganOut2 := Value;
    Changed(Self);
  end;
end;

procedure TORGAN_LINK.SetOrganOut3(const Value: TPHASE_TYPE);
begin
  if Value <> FOrganOut3 then
  begin
    FOrganOut3 := Value;
    Changed(Self);
  end;
end;

{ TSECOND_ERROR }

procedure TSECOND_ERROR.Assign(source: TSECOND_ERROR);
begin
  FSenUGroundBroken :=  TSECOND_ERROR(source).SenUGroundBroken;
  FSenUReverse.Assign(TSECOND_ERROR(source).SenUReverse);

  FSenIGroundBroken.Assign(TSECOND_ERROR(source).SenIGroundBroken);

  FSenUBroken.Assign(TSECOND_ERROR(source).SenUBroken);

  FSenIShort.Assign(TSECOND_ERROR(source).SenIShort);

  FSenIReverse.Assign(TSECOND_ERROR(source).SenIReverse);

  FSenIBroken.Assign(TSECOND_ERROR(source).SenIBroken);
end;

procedure TSECOND_ERROR.Changed(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TSECOND_ERROR.Create;
begin
  FSenUReverse := TPHASE_RECT.Create;
  FSenIGroundBroken := TPHASE_RECT.Create;
  FSenUBroken := TPHASE_RECT.Create;
  FSenIShort := TPHASE_RECT.Create;
  FSenIReverse:= TPHASE_RECT.Create;
  FSenIBroken:= TPHASE_RECT.Create;


  FSenUReverse.OnChanged := Changed;
  FSenIGroundBroken.OnChanged := Changed;
  FSenUBroken.OnChanged := Changed;
  FSenIShort.OnChanged := Changed;
  FSenIReverse.OnChanged := Changed;
  FSenIBroken.OnChanged := Changed;

end;

destructor TSECOND_ERROR.Destroy;
begin
  if Assigned(FSenUReverse) then
    FSenUReverse.Free;

  if Assigned(FSenIGroundBroken) then
    FSenIGroundBroken.Free;

  if Assigned(FSenUBroken) then
    FSenUBroken.Free;

  if Assigned(FSenIShort) then
    FSenIShort.Free;

  if Assigned(FSenIReverse) then
    FSenIReverse.Free;

  if Assigned(FSenIBroken) then
    FSenIBroken.Free;

  inherited;
end;

procedure TSECOND_ERROR.setSenUGroundBroken(const Value: Boolean);
begin
  if Value <> FSenUGroundBroken then
  begin
    FSenUGroundBroken := Value;
    Changed(Self);
  end;
end;

/// <summary>
/// 获取所有相许列表
/// </summary>
function GetUSequenceStrAll : string;
begin
  Result := 'Uabc' + #13#10 + 'Uacb' + #13#10 + 'Ubac' + #13#10 +
            'Ubca' + #13#10 + 'Ucab' + #13#10 + 'Ucba';
end;

/// <summary>
/// 获取电流相许列表
/// </summary>
function GetISequenceStrAll(bIsThree, bIsClear : Boolean) : string;
begin
  if bIsThree then
  begin
    if bIsClear then
    begin
      Result := 'Iac' + #13#10 + 'Ica';
    end
    else
    begin
      Result := 'Iac' + #13#10 + 'Ica';
    end;
  end
  else
  begin
    if bIsClear then
    begin
      Result := 'Iabc' + #13#10 + 'Iacb' + #13#10 + 'Ibac' + #13#10 +
                'Ibca' + #13#10 + 'Icab' + #13#10 + 'Icba';
    end
    else
    begin
      Result := 'Iabc' + #13#10 + 'Iacb' + #13#10 + 'Ibac' + #13#10 +
                'Ibca' + #13#10 + 'Icab' + #13#10 + 'Icba';
    end;
  end;

end;


{ TMETER_ERROR }


procedure TMETER_ERROR.Assign(source: TMETER_ERROR);
begin
  FUSequence := (TMETER_ERROR(Source).USequence);
  FISequence := (TMETER_ERROR(Source).ISequence);

  FIReverse.Assign((TMETER_ERROR(Source).IReverse));

  FUBroken.Assign((TMETER_ERROR(Source).UBroken));

  FIShort.Assign((TMETER_ERROR(Source).IShort));

  FIBroken.Assign((TMETER_ERROR(Source).IBroken));
end;

procedure TMETER_ERROR.Changed(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TMETER_ERROR.Create;
begin
  FUSequence := TORGAN_LINK.Create;
  FISequence := TORGAN_LINK.Create;
  FIReverse  := TPHASE_RECT.Create;
  FUBroken   := TPHASE_RECT.Create;
  FIShort    := TPHASE_RECT.Create;
  FIBroken   := TPHASE_RECT.Create;

  // 设置成三相四线
  SetMeterType(0);

  FIReverse.OnChanged  := Changed;
  FUBroken.OnChanged   := Changed;
  FIShort.OnChanged    := Changed;
  FIBroken.OnChanged   := Changed;
  FUSequence.OnChanged := Changed;
  FISequence.OnChanged := Changed;

end;

destructor TMETER_ERROR.Destroy;
begin
  if Assigned(FIReverse)  then FIReverse.Free;
  if Assigned(FUBroken)   then FUBroken.Free;
  if Assigned(FIShort)    then FIShort.Free;
  if Assigned(FIBroken)   then FIBroken.Free;
  if Assigned(FUSequence) then FUSequence.Free;
  if Assigned(FISequence) then FISequence.Free;

  inherited;
end;

procedure TMETER_ERROR.setISequence(const Value: TORGAN_LINK);
begin
  FISequence := Value;
end;

procedure TMETER_ERROR.SetMeterType(const Value: Integer);
begin
  FMeterType := Value;
  case FMeterType of
    0:
    begin
      FISequence.OrganType := 1;
      FUSequence.OrganType := 0;
    end;
    1:
    begin
      FISequence.OrganType := 3;
      FUSequence.OrganType := 2;
    end;
  end;
end;

procedure TMETER_ERROR.setUSequence(const Value: TORGAN_LINK);
begin
  FUSequence := Value;
end;

{ TWIRINGF_ERROR }

procedure TWIRINGF_ERROR.Assign(Source: TPersistent);
begin
  inherited;
  FIsTrans:= TWIRINGF_ERROR(Source).IsTrans;
  FDiagramType := TWIRINGF_ERROR(Source).DiagramType;

  //相线
  FPhaseType := TWIRINGF_ERROR(Source).PhaseType;

  //一次故障
  FFirError.Assign(TWIRINGF_ERROR(Source).FirError);

  //二次故障
  FSenError.Assign(TWIRINGF_ERROR(Source).SenError);

  //表故障
  FMeterError.Assign(TWIRINGF_ERROR(Source).MeterError);
end;

procedure TWIRINGF_ERROR.Changed(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TWIRINGF_ERROR.Create;
begin
//  inherited;
  FPhaseType := ptfThree;
  FMeterError := TMETER_ERROR.Create;
  FMeterError.MeterID := 1;
  FFirError := TPHASE_RECT.Create;
  FSenError := TSECOND_ERROR.Create;
  FIsTrans:= True;
  FDiagramType := 2;
  FMeterError.OnChanged := Changed;
  FFirError.OnChanged := Changed;
  FSenError.OnChanged := Changed;
end;

destructor TWIRINGF_ERROR.Destroy;
begin
  if Assigned(FMeterError) then
     FMeterError.Free;

  if Assigned(FFirError) then
     FFirError.Free;

  if Assigned(FSenError) then
    FSenError.Free;
  inherited;
end;

function TWIRINGF_ERROR.GetDescription(nAbc: Integer; bPTCT : Boolean): string;
  function GetABC(nType : Integer; bUpperCase : Boolean) : string;
  begin
    case nType of
      0 :
      begin
        case nAbc of
          0 : Result := 'a';
          1 : Result := 'u';
          2 : Result := '1';
        end;
      end;
      1 :
      begin
        case nAbc of
          0 : Result := 'b';
          1 : Result := 'v';
          2 : Result := '2';
        end;
      end;
      2 :
      begin
        case nAbc of
          0 : Result := 'c';
          1 : Result := 'w';
          2 : Result := '3';
        end;
      end;
    end;

    if bUpperCase then
      Result := UpperCase(Result);
  end;

  function GetPTStr : string;
  begin
    if bPTCT then
      Result := 'PT'
    else
      Result := 'TV';
  end;

  function GetCTStr : string;
  begin
    if bPTCT then
      Result := 'CT'
    else
      Result := 'TA';
  end;

  function GetTypeStr(Avalue, Bvalue, Cvalue: Boolean; bUpperCase : Boolean=False): string;
  begin
    Result := '';
    if Avalue then Result := Result + GetABC(0, bUpperCase) + ',';
    if Bvalue then Result := Result + GetABC(1, bUpperCase) + ',';
    if Cvalue then Result := Result + GetABC(2, bUpperCase) + ',';

    if Result <> '' then
      Result := Copy(Result, 1, Length(Result)-1);
  end;

  function GetPTTypeStr(Avalue, Bvalue, Cvalue: Boolean): string;
  begin
    Result := '';
    if Avalue then Result := Result + GetPTStr + '1,';
    if Bvalue then Result := Result + GetPTStr + '2,';
    if Cvalue then Result := Result + GetPTStr + '3,';

    if Result <> '' then
      Result := Copy(Result, 0, Length(Result)-1);
  end;

  function GetCTTypeStr(Avalue, Bvalue, Cvalue: Boolean): string;
  begin
    Result := '';
    if Avalue then Result := Result + GetCTStr + '1,';
    if Bvalue then Result := Result + GetCTStr + '2,';
    if Cvalue then Result := Result + GetCTStr + '3,';

    if Result <> '' then
      Result := Copy(Result, 1, Length(Result)-1);
  end;

  function GetSequenceStr(s : string): string;
  begin
    Result := s;
    if nAbc = 1 then
    begin
      Result := StringReplace(Result, 'a', 'u', [rfReplaceAll]);
      Result := StringReplace(Result, 'b', 'v', [rfReplaceAll]);
      Result := StringReplace(Result, 'c', 'w', [rfReplaceAll]);
    end
    else if nAbc = 2 then
    begin
      Result := StringReplace(Result, 'a', '1', [rfReplaceAll]);
      Result := StringReplace(Result, 'b', '2', [rfReplaceAll]);
      Result := StringReplace(Result, 'c', '3', [rfReplaceAll]);
    end;
  end;
var
  sErrorText : string;
  s, s1 : string;
begin
  sErrorText := EmptyStr;

  if Assigned(FirError) then
  begin
    s := EmptyStr;

    if FirError.AValue or FirError.BValue or FirError.CValue then
    begin
      s := GetTypeStr(FirError.AValue, FirError.BValue, FirError.CValue, True);
      sErrorText := sErrorText + '【一次】断相:' + s+';';
    end;
  end;

  if Assigned(SenError) then
  begin
    with SenError do
    begin
      s := EmptyStr;
      s1 := EmptyStr;
      //电压开路
      if SenUBroken.AValue or SenUBroken.BValue or SenUBroken.CValue then
      begin
        s := GetTypeStr(SenUBroken.AValue, SenUBroken.BValue, SenUBroken.CValue);
        s1 := s1 + '开路:' + s + ';';
      end;

      s := EmptyStr;
      //极性反
      if SenUReverse.AValue or SenUReverse.BValue or SenUReverse.CValue then
      begin
        s := GetPTTypeStr(SenUReverse.AValue, SenUReverse.BValue, SenUReverse.CValue);
        s1 := s1 + '极性反:' + s + ';';
      end;

      // 接地断开
      if SenUGroundBroken then
        s1 := s1 + '接地断开' + ';';


      if s1 <> '' then
      begin
        sErrorText := sErrorText + '【二次电压】' + s1;
      end;

      s := EmptyStr;
      s1 := EmptyStr;
      //电流短路
      if SenIShort.AValue or SenIShort.BValue or SenIShort.CValue then
      begin
        s := GetCTTypeStr(SenIShort.AValue, SenIShort.BValue, SenIShort.CValue);
        s1 := s1 + '短路:' + s +';';
      end;

      s := EmptyStr;
      //电流开路
      if SenIBroken.FAValue or SenIBroken.FBValue or SenIBroken.FCValue then
      begin
        s := GetCTTypeStr(SenIBroken.AValue, SenIBroken.BValue, SenIBroken.CValue);
        s1 := s1 + '开路:' + s + ';';
      end;

      s := EmptyStr;
      //电流极性反
      if SenIReverse.FAValue or SenIReverse.FBValue or SenIReverse.FCValue then
      begin
        s := GetCTTypeStr(SenIReverse.FAValue, SenIReverse.FBValue, SenIReverse.FCValue);
        s1 := s1 + '极性反:' + s + ';';
      end;

      s := EmptyStr;
      //电流接地断开
      if SenIGroundBroken.FAValue or SenIGroundBroken.FBValue or SenIGroundBroken.FCValue then
      begin
        s := GetCTTypeStr(SenIGroundBroken.AValue, SenIGroundBroken.BValue, SenIGroundBroken.CValue);
        s1 := s1 + '接地断开:' + s + ';';
      end;

      if s1 <> '' then
      begin
        sErrorText := sErrorText + '【二次电流】' + s1;
      end;
    end;
  end;

  //表位1
  if Assigned(MeterError) then
  begin
    with MeterError do
    begin
      //电压相序
      s1 := EmptyStr;
      s := USequence.OrganStr;

      if s <> 'abc' then
        s1 := 'U' + GetSequenceStr(s) + ';';

      //电压失压
      s := EmptyStr;
      if UBroken.FAValue or UBroken.FBValue or UBroken.CValue then
      begin
        s := GetTypeStr(UBroken.AValue, UBroken.BValue, UBroken.CValue);
        s1 := s1 + '失压:' + s + ';';
      end;

      if s1 <> '' then
        sErrorText := sErrorText + '【电表电压】' + s1;

      //电流相序
      s1 := EmptyStr;
      s := ISequence.OrganStr;

      if (s <> 'abc') and (s <> 'ac') then
        s1 := 'I' + GetSequenceStr(s) + ';';

      //电流开路
      s := EmptyStr;
      if IBroken.AValue or IBroken.BValue or IBroken.CValue then
      begin
        s := GetTypeStr(IBroken.AValue, IBroken.BValue, IBroken.CValue);
        s1 := s1 + '开路:' + s + ';';
      end;

      //电流短路
      s := EmptyStr;
      if IShort.AValue or IShort.BValue or IShort.CValue then
      begin
        s := GetTypeStr(IShort.AValue, IShort.BValue, IShort.CValue);
        s1 := s1 + '短路:' + s + ';';
      end;

      //电流反接
      s := EmptyStr;
      if IReverse.AValue or IReverse.BValue or IReverse.CValue then
      begin
        s := GetTypeStr(IReverse.AValue, IReverse.BValue, IReverse.CValue);
        s1 := s1 + '反接:'+ s + ';';
      end;

      if s1 <> '' then
      begin
        sErrorText := sErrorText + '【电表电流】' + s1;
      end;
    end;
  end;

  if sErrorText <> '' then
  begin
    Result := sErrorText;
  end
  else
  begin
    Result := '接线正确;';
  end;
end;

function TWIRINGF_ERROR.GetID: string;
  function GetPhaseValue : Integer;
  begin
    case FPhaseType of
      ptfSingle : Result := 1;
      ptfThree : Result := 3;
    else
      Result := 4;
    end;
  end;
  function GetBoolValue(bValue : Boolean; nShl : Byte) : Byte;
  begin
    if bValue then
      Result := (1 shl nShl) and $FF
    else
      Result := 0;
  end;

  function GetTypeValue(APT : TPHASE_TYPE; nShl : Byte) : Byte;
  begin
    case APT of
      ptA : Result := 1;
      ptB : Result := 2;
      ptC : Result := 3;
      ptN : Result := 0;
    else
      Result := 8;
    end;

    Result := (Result shl nShl) and $FF
  end;
var
  aBytes : TBytes;
begin
  SetLength(aBytes, 10);


  aBytes[0] := GetPhaseValue shl 4;
  aBytes[0] := aBytes[0] + GetBoolValue(FIsTrans, 3) + FDiagramType;

  aBytes[1] := GetBoolValue(FFirError.AValue, 0) +
               GetBoolValue(FFirError.BValue, 1) +
               GetBoolValue(FFirError.CValue, 2);

  aBytes[2] := GetBoolValue(FSenError.SenUBroken.AValue, 4) +
               GetBoolValue(FSenError.SenUBroken.BValue, 5) +
               GetBoolValue(FSenError.SenUBroken.CValue, 6) +
               GetBoolValue(FSenError.SenUBroken.NValue, 7) +
               GetBoolValue(FSenError.SenUGroundBroken, 3) +
               GetBoolValue(FSenError.SenUReverse.AValue, 0) +
               GetBoolValue(FSenError.SenUReverse.BValue, 1) +
               GetBoolValue(FSenError.SenUReverse.CValue, 2);

  aBytes[3] := GetBoolValue(FSenError.SenIShort.AValue, 0) +
               GetBoolValue(FSenError.SenIShort.BValue, 1) +
               GetBoolValue(FSenError.SenIShort.CValue, 2) +
               GetBoolValue(FSenError.SenIBroken.AValue, 3) +
               GetBoolValue(FSenError.SenIBroken.BValue, 4) +
               GetBoolValue(FSenError.SenIBroken.CValue, 5);

  aBytes[4] := GetBoolValue(FSenError.SenIReverse.AValue, 0) +
               GetBoolValue(FSenError.SenIReverse.BValue, 1) +
               GetBoolValue(FSenError.SenIReverse.CValue, 2) +
               GetBoolValue(FSenError.SenIGroundBroken.AValue, 3) +
               GetBoolValue(FSenError.SenIGroundBroken.BValue, 4) +
               GetBoolValue(FSenError.SenIGroundBroken.CValue, 5);

  aBytes[5] := GetTypeValue(FMeterError.USequence.OrganOut3, 0) +
               GetTypeValue(FMeterError.USequence.OrganIn3, 2) +
               GetBoolValue(FMeterError.UBroken.AValue, 4) +
               GetBoolValue(FMeterError.UBroken.BValue, 5) +
               GetBoolValue(FMeterError.UBroken.CValue, 6) +
               GetBoolValue(FMeterError.UBroken.NValue, 7);

  aBytes[6] := GetTypeValue(FMeterError.USequence.OrganOut1, 0) +
               GetTypeValue(FMeterError.USequence.OrganIn1, 2) +
               GetTypeValue(FMeterError.USequence.OrganOut2, 4) +
               GetTypeValue(FMeterError.USequence.OrganIn2, 6);

  aBytes[7] := GetBoolValue(FMeterError.IShort.AValue, 0) +
               GetBoolValue(FMeterError.IShort.BValue, 1) +
               GetBoolValue(FMeterError.IShort.CValue, 2) +
               GetBoolValue(FMeterError.IBroken.AValue, 3) +
               GetBoolValue(FMeterError.IBroken.BValue, 4) +
               GetBoolValue(FMeterError.IBroken.CValue, 5);

  aBytes[8] := GetTypeValue(FMeterError.ISequence.OrganOut2, 0) +
               GetTypeValue(FMeterError.ISequence.OrganIn2, 2) +
               GetTypeValue(FMeterError.ISequence.OrganOut3, 4) +
               GetTypeValue(FMeterError.ISequence.OrganIn3, 6);

  aBytes[9] := GetTypeValue(FMeterError.ISequence.OrganOut1, 4) +
               GetTypeValue(FMeterError.ISequence.OrganIn1, 6) +
               GetBoolValue(FMeterError.IReverse.AValue, 0) +
               GetBoolValue(FMeterError.IReverse.BValue, 1) +
               GetBoolValue(FMeterError.IReverse.CValue, 2);

  Result := BCDPacksToStr(aBytes);
end;

function TWIRINGF_ERROR.GetIsCorrect: Boolean;
begin
  Result := GetDescription(0, True) = '接线正确';
end;

procedure TWIRINGF_ERROR.SetCorrect;
begin
  case FPhaseType of
    ptfSingle :
    begin
      FMeterError.USequence.OrganType := 4;
      FMeterError.ISequence.OrganType := 4;
    end;
    ptfThree :
    begin
      FMeterError.USequence.OrganType := 2;
      FMeterError.ISequence.OrganType := 3;
    end;
    ptfFour :
    begin
      FMeterError.USequence.OrganType := 0;
      FMeterError.ISequence.OrganType := 1;
    end;
  end;

  FFirError.AValue := False;
  FFirError.BValue := False;
  FFirError.CValue := False;

  FSenError.SenUBroken.AValue := False;
  FSenError.SenUBroken.BValue := False;
  FSenError.SenUBroken.CValue := False;
  FSenError.SenUBroken.NValue := False;
  FSenError.SenUGroundBroken := False;
  FSenError.SenUReverse.AValue := False;
  FSenError.SenUReverse.BValue := False;
  FSenError.SenUReverse.CValue := False;

  FSenError.SenIShort.AValue := False;
  FSenError.SenIShort.BValue := False;
  FSenError.SenIShort.CValue := False;
  FSenError.SenIBroken.AValue := False;
  FSenError.SenIBroken.BValue := False;
  FSenError.SenIBroken.CValue := False;

  FSenError.SenIReverse.AValue := False;
  FSenError.SenIReverse.BValue := False;
  FSenError.SenIReverse.CValue := False;
  FSenError.SenIGroundBroken.AValue := False;
  FSenError.SenIGroundBroken.BValue := False;
  FSenError.SenIGroundBroken.CValue := False;
  FMeterError.UBroken.AValue := False;
  FMeterError.UBroken.BValue := False;
  FMeterError.UBroken.CValue := False;
  FMeterError.UBroken.NValue := False;
  FMeterError.IShort.AValue := False;
  FMeterError.IShort.BValue := False;
  FMeterError.IShort.CValue := False;
  FMeterError.IBroken.AValue := False;
  FMeterError.IBroken.BValue := False;
  FMeterError.IBroken.CValue := False;
  FMeterError.IReverse.AValue := False;
  FMeterError.IReverse.BValue := False;
  FMeterError.IReverse.CValue := False;
end;

procedure TWIRINGF_ERROR.SetID(const Value: string);
  function GetPhaseValue(nValue : Byte) : TWEF_PHASE_TYPE;
  begin
    case nValue shr 4 of
      1 : Result := ptfSingle;
      3 : Result := ptfThree;
    else
      Result := ptfFour;
    end;
  end;

  function GetValue(nValue, nIndex : Byte) : Boolean;
  begin
    Result := ((nValue shr nIndex) and 1) = 1;
  end;

  function GetSequenceValue(nValue, nIndex : Byte) : TPHASE_TYPE;
  begin
    case ((nValue shr nIndex) and 3) and $FF of
      0 : Result :=  ptN;
      1 : Result :=  ptA;
      2 : Result :=  ptB;
      3 : Result :=  ptC;
    else
      Result := ptNot;
    end;
  end;

var
  aBytes : TBytes;
begin
  aBytes := StrToBCDPacks(Trim(Value));


  if Length(aBytes) <> 10 then
    aBytes := StrToBCDPacks('3A 00 00 00 00 0C E6 00 CC 40');

  if Length(aBytes) = 10 then
  begin
    PhaseType := GetPhaseValue(aBytes[0]);
    FIsTrans := GetValue(aBytes[0], 3);
    FDiagramType := aBytes[0] and 7;

    FFirError.AValue := GetValue(aBytes[1], 0);
    FFirError.BValue := GetValue(aBytes[1], 1);
    FFirError.CValue := GetValue(aBytes[1], 2);
    FFirError.NValue := GetValue(aBytes[1], 3);

    FSenError.SenUReverse.AValue := GetValue(aBytes[2], 0);
    FSenError.SenUReverse.BValue := GetValue(aBytes[2], 1);
    FSenError.SenUReverse.CValue := GetValue(aBytes[2], 2);
    FSenError.SenUGroundBroken   := GetValue(aBytes[2], 3);
    FSenError.SenUBroken.AValue  := GetValue(aBytes[2], 4);
    FSenError.SenUBroken.BValue  := GetValue(aBytes[2], 5);
    FSenError.SenUBroken.CValue  := GetValue(aBytes[2], 6);
    FSenError.SenUBroken.NValue  := GetValue(aBytes[2], 7);

    FSenError.SenIShort.AValue := GetValue(aBytes[3], 0);
    FSenError.SenIShort.BValue := GetValue(aBytes[3], 1);
    FSenError.SenIShort.CValue := GetValue(aBytes[3], 2);
    FSenError.SenIBroken.AValue:= GetValue(aBytes[3], 3);
    FSenError.SenIBroken.BValue:= GetValue(aBytes[3], 4);
    FSenError.SenIBroken.CValue:= GetValue(aBytes[3], 5);

    FSenError.SenIReverse.AValue := GetValue(aBytes[4], 0);
    FSenError.SenIReverse.BValue := GetValue(aBytes[4], 1);
    FSenError.SenIReverse.CValue := GetValue(aBytes[4], 2);
    FSenError.SenIGroundBroken.AValue:= GetValue(aBytes[4], 3);
    FSenError.SenIGroundBroken.BValue:= GetValue(aBytes[4], 4);
    FSenError.SenIGroundBroken.CValue:= GetValue(aBytes[4], 5);


    FMeterError.UBroken.AValue := GetValue(aBytes[5], 4);
    FMeterError.UBroken.BValue := GetValue(aBytes[5], 5);
    FMeterError.UBroken.CValue := GetValue(aBytes[5], 6);
    FMeterError.UBroken.NValue := GetValue(aBytes[5], 7);
    FMeterError.USequence.OrganOut3 := GetSequenceValue(aBytes[5], 0);
    FMeterError.USequence.OrganIn3  := GetSequenceValue(aBytes[5], 2);

    FMeterError.USequence.OrganOut1 := GetSequenceValue(aBytes[6], 0);
    FMeterError.USequence.OrganIn1  := GetSequenceValue(aBytes[6], 2);
    FMeterError.USequence.OrganOut2 := GetSequenceValue(aBytes[6], 4);
    FMeterError.USequence.OrganIn2  := GetSequenceValue(aBytes[6], 6);

    FMeterError.IShort.AValue := GetValue(aBytes[7], 0);
    FMeterError.IShort.BValue := GetValue(aBytes[7], 1);
    FMeterError.IShort.CValue := GetValue(aBytes[7], 2);
    FMeterError.IBroken.AValue := GetValue(aBytes[7], 3);
    FMeterError.IBroken.BValue := GetValue(aBytes[7], 4);
    FMeterError.IBroken.CValue := GetValue(aBytes[7], 5);

    FMeterError.ISequence.OrganOut2 := GetSequenceValue(aBytes[8], 0);
    FMeterError.ISequence.OrganIn2  := GetSequenceValue(aBytes[8], 2);
    FMeterError.ISequence.OrganOut3 := GetSequenceValue(aBytes[8], 4);
    FMeterError.ISequence.OrganIn3  := GetSequenceValue(aBytes[8], 6);

    FMeterError.ISequence.OrganOut1 := GetSequenceValue(aBytes[9], 4);
    FMeterError.ISequence.OrganIn1  := GetSequenceValue(aBytes[9], 6);
    FMeterError.IReverse.AValue := GetValue(aBytes[9], 0);
    FMeterError.IReverse.BValue := GetValue(aBytes[9], 1);
    FMeterError.IReverse.CValue := GetValue(aBytes[9], 2);
  end;
end;

procedure TWIRINGF_ERROR.SetPhaseType(const Value: TWEF_PHASE_TYPE);
begin
  FPhaseType := Value;

  if FPhaseType = ptfThree then
  begin
    FMeterError.MeterType := 1;
  end
  else
  begin
    FMeterError.MeterType := 0;
  end;

end;

end.



