{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，接线错误类单元
  + TWIRING_ERROR 接线错误类

===============================================================================}

unit U_WIRING_ERROR;

interface

uses SysUtils, Classes, IniFiles, Forms;

const
  /// <summary>
  /// 三线编号基本值
  /// </summary>
  C_WE_ID_BASE_THREE = $30000000;

  /// <summary>
  /// 三线编号正确值
  /// </summary>
  C_WE_ID_CORRECT_THREE = $30000000;

const
  /// <summary>
  /// 四线编号基本值
  /// </summary>
  C_WE_ID_BASE_FOUR  = $40000000;

const
  /// <summary>
  /// 四线带PT编号基本值
  /// </summary>
  C_WE_ID_BASE_FOUR_PT  = $08000000;

const
  /// <summary>
  /// 四线编号正确值
  /// </summary>
  C_WE_ID_CORRECT_FOUR  = $40000000;

const
  /// <summary>
  /// 四线编号正确值
  /// </summary>
  C_WE_ID_CORRECT_FOUR_PT  = $08000000;

type
  /// <summary>
  /// 相线类型， ptThree 三线， ptFour 四线
  /// </summary>
  TWE_PHASE_TYPE = (ptThree, ptFour, ptFourPT, ptSingle); //ptFourPT为三相四线带电压互感器

  /// <summary>
  /// 获取相线类型字符串
  /// </summary>
  function GetPhaseTypeStr( AType : TWE_PHASE_TYPE ) : string;

type
  /// <summary>
  /// 相序类型
  /// </summary>
  TWE_SEQUENCE_TYPE = (stABC, stACB, stBAC, stBCA, stCAB, stCBA);

  /// <summary>
  /// 获取相序类型字符串
  /// </summary>
  function GetSequenceTypeStr( AType : TWE_SEQUENCE_TYPE; bAbc : Integer = 0 ) : string;

type
  /// <summary>
  /// 相线类型 plB1为简化接线-Ia-Ic的矢量和
  /// </summary>
  TWE_PHASE_LINE_TYPE = (plA, plB, plC, plN);

  /// <summary>
  /// 获取相线类型字符串
  /// </summary>
  function GetPhaseLineTypeStr( AType : TWE_PHASE_LINE_TYPE; bAbc : Integer = 0 ) : string;
  {function GetPhaseLineTypeStr( AType : TWE_PHASE_LINE_TYPE; bAbc : Boolean = True ) : string;}

type
  /// <summary>
  /// 接线错误类型
  /// </summary>
  TWE_ERROR_TYPE = ( etUSequence, etUBroken, etUsBroken, etPTReverse,
    etISequence, etIBroken, etCTShort, etCTReverse );

type
  /// <summary>
  /// 接线错误类型集合
  /// </summary>
  TWE_ERROR_TYPE_SET = set of TWE_ERROR_TYPE;

type
  /// <summary>
  /// 接线错误
  /// </summary>
  TWIRING_ERROR = class( TPersistent )
  private
    // 电压部分(三相四线)
    FUSequence : TWE_SEQUENCE_TYPE;  // 电压相序

    FUaBroken  : Boolean;            // 电压断, 一次电压断
    FUbBroken  : Boolean;
    FUcBroken  : Boolean;
    FUnBroken  : Boolean;

    // 电压附加（三相三线）
    FUsaBroken : Boolean;            // 二次电压断
    FUsbBroken : Boolean;
    FUscBroken : Boolean;
    FUsnBroken : Boolean;            //三相四线带电压互感器N相断开，胡红明2013.5.10
    FGroundBroken: Boolean;          //三相四线PT没有接地，胡红明2013.5.14
    FPT1Reverse : Boolean;           // 电压互感器反
    FPT2Reverse : Boolean;
    FPT3Reverse : Boolean;           //用于三相四线电压互感器反，胡红明2013.5.10

    // 电流部分(三相四线)
    FISequence  : TWE_SEQUENCE_TYPE;    // 电流相序
    FIaBroken   : Boolean          ;    // 电流断
    FIbBroken   : Boolean          ;
    FIcBroken   : Boolean          ;
    FInBroken   : Boolean          ;
    FCT1Short   : Boolean          ;    // 电流互感器短路
    FCT2Short   : Boolean          ;
    FCT3Short   : Boolean          ;
    FCT1Reverse : Boolean          ;    // 电流互感器反
    FCT2Reverse : Boolean          ;
    FCT3Reverse : Boolean          ;

    // 三相四线电表电流线表尾反接
    FI1Reverse : Boolean;
    FI2Reverse : Boolean;
    FI3Reverse : Boolean;

    // 电流附加（三相三线）
    FI1In  : TWE_PHASE_LINE_TYPE;       // 表尾电流输入输出
    FI1Out : TWE_PHASE_LINE_TYPE;
    FI2In  : TWE_PHASE_LINE_TYPE;
    FI2Out : TWE_PHASE_LINE_TYPE;

    //  显示方式（abc、uvw、123）
    Fabc : Integer;
    Fptct : Boolean;

    // 其它
    FPhaseType: TWE_PHASE_TYPE;
    FOnChanged : TNotifyEvent;
    FIbGroundBroken: Boolean;
    FIcGroundBroken: Boolean;
    FIaGroundBroken: Boolean;
    FIGroundBroken: Boolean;
    FI3Out: TWE_PHASE_LINE_TYPE;
    FI3In: TWE_PHASE_LINE_TYPE;
    FIsClearLinke: Boolean;
    FIsCanSetClearLinkeError: Boolean;

//    FProVersion: string;
    function GetID : Cardinal;
    function GetIDThree : Cardinal;
    function GetIDFour : Cardinal;
    function GetIDFourPT: Cardinal;
    procedure SetID( const Value : Cardinal );
    procedure SetIDThree( const Value : Cardinal );
    procedure SetIDFour( const Value : Cardinal );
    procedure SetIDFourPT(const Value : Cardinal );
    procedure SetPhaseType(const Value: TWE_PHASE_TYPE);
    function GetDescription : string;

    /// <summary>
    /// 判断某一位是否为1
    /// </summary>
    function BitIsOne( AIntValue, ABitPos : Integer ) : Boolean;
    function GetErrorCount: Integer;
    procedure SetIaGroundBroken(const Value: Boolean);
    procedure SetI1In(const Value: TWE_PHASE_LINE_TYPE);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    /// 清空错误接线
    /// </summary>
    procedure Clear;

    /// <summary>
    /// 错误接线编码字符串格式
    /// </summary>
    function IDInStr : string;

    /// <summary>
    /// 是否有U断相
    /// </summary>
    function UHasBroken : Boolean;

    /// <summary>
    /// 是否为正确的接线
    /// </summary>
    function IsCorrect : Boolean;

    /// <summary>
    /// 设置为正确接线
    /// </summary>
    procedure SetToCorrect;

    /// <summary>
    /// 错误接线描述(一句话)
    /// </summary>
    procedure GetDescriptionText( AText : TStrings );

    /// <summary>
    /// 设置错误数量
    /// </summary>
    property ErrorCount : Integer read GetErrorCount;
  public
    /// <summary>
    /// 错误接线编码
    /// </summary>
    property ID : Cardinal read GetID write SetID;

    /// <summary>
    /// 错误接线描述
    /// </summary>
    property Description : string read GetDescription;

    /// <summary>
    /// 错误改变事件
    /// </summary>
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;

    /// <summary>
    /// 相线类型
    /// </summary>
    property PhaseType : TWE_PHASE_TYPE read FPhaseType write SetPhaseType;

    /// <summary>
    /// 电压相序
    /// </summary>
    property USequence : TWE_SEQUENCE_TYPE read FUSequence write FUSequence ;

    /// <summary>
    /// 电压断
    /// </summary>
    property UaBroken  : Boolean read FUaBroken  write FUaBroken  ;
    property UbBroken  : Boolean read FUbBroken  write FUbBroken  ;
    property UcBroken  : Boolean read FUcBroken  write FUcBroken  ;
    property UnBroken  : Boolean read FUnBroken  write FUnBroken  ;

    /// <summary>
    /// 二次电压断
    /// </summary>
    property UsaBroken : Boolean read FUsaBroken write FUsaBroken;
    property UsbBroken : Boolean read FUsbBroken write FUsbBroken;
    property UscBroken : Boolean read FUscBroken write FUscBroken;
    property UsnBroken : Boolean read FUsnBroken write FUsnBroken;

    /// <summary>
    /// 地线断（没有接地）
    /// </summary>
    property GroundBroken: Boolean read FGroundBroken write FGroundBroken;
    
    /// <summary>
    /// 电压互感器反
    /// </summary>
    property PT1Reverse : Boolean read FPT1Reverse write FPT1Reverse;
    property PT2Reverse : Boolean read FPT2Reverse write FPT2Reverse;
    property PT3Reverse : Boolean read FPT3Reverse write FPT3Reverse;

    /// <summary>
    /// 电流相序
    /// </summary>
    property ISequence  : TWE_SEQUENCE_TYPE read FISequence  write FISequence ;

    /// <summary>
    /// 电流断
    /// </summary>

    property IaBroken   : Boolean read FIaBroken write FIaBroken  ;
    property IbBroken   : Boolean read FIbBroken write FIbBroken  ;
    property IcBroken   : Boolean read FIcBroken write FIcBroken  ;
    property InBroken   : Boolean read FInBroken write FInBroken  ;

    /// <summary>
    /// 电流互感器短路
    /// </summary>
    property CT1Short   : Boolean read FCT1Short write FCT1Short  ;
    property CT2Short   : Boolean read FCT2Short write FCT2Short  ;
    property CT3Short   : Boolean read FCT3Short write FCT3Short  ;

    /// <summary>
    /// 电流互感器反
    /// </summary>
    property CT1Reverse : Boolean read FCT1Reverse write FCT1Reverse;
    property CT2Reverse : Boolean read FCT2Reverse write FCT2Reverse;
    property CT3Reverse : Boolean read FCT3Reverse write FCT3Reverse;

    /// <summary>
    /// 表尾电流输入输出  三相三线用I1和I2   简化接线时plB
    /// </summary>
    property I1In  : TWE_PHASE_LINE_TYPE read FI1In  write SetI1In  ;
    property I1Out : TWE_PHASE_LINE_TYPE read FI1Out write FI1Out ;
    property I2In  : TWE_PHASE_LINE_TYPE read FI2In  write FI2In  ;
    property I2Out : TWE_PHASE_LINE_TYPE read FI2Out write FI2Out ;

    property I3In  : TWE_PHASE_LINE_TYPE read FI3In  write FI3In  ;
    property I3Out : TWE_PHASE_LINE_TYPE read FI3Out write FI3Out ;

    /// <summary>
    /// 三相四线电流反接
    /// </summary>
    property I1Reverse : Boolean read FI1Reverse write FI1Reverse default False;
    property I2Reverse : Boolean read FI2Reverse write FI2Reverse default False;
    property I3Reverse : Boolean read FI3Reverse write FI3Reverse default False;

    /// <summary>
    /// 电流接地断开
    /// </summary>
    property IaGroundBroken : Boolean read FIaGroundBroken write SetIaGroundBroken;
    property IbGroundBroken : Boolean read FIbGroundBroken write FIbGroundBroken;
    property IcGroundBroken : Boolean read FIcGroundBroken write FIcGroundBroken;

    property IGroundBroken : Boolean read FIGroundBroken write FIGroundBroken;

//    /// <summary>
//    /// 测试代码
//    /// </summary>
//    property Temp : Integer read FTemp write FTemp;

    /// <summary>
    /// 是否是简化接线
    /// </summary>
    property IsClearLinke : Boolean read FIsClearLinke write FIsClearLinke;

    /// <summary>
    /// 是否可以设置简化接线错误
    /// </summary>
    property IsCanSetClearLinkeError : Boolean read FIsCanSetClearLinkeError write FIsCanSetClearLinkeError;

    /// <summary>
    /// 三相四线简化接线时电流进线转换为对象中的表位接线
    /// </summary>
    procedure SetClearLinkeISequence(sI1, sI2, sI3 : string;
      bI1Reverse, bI2Reverse, bI3Reverse : Boolean);

    // 三相四线简化接线 获取电流相序和是否反接
    procedure GetClearLinkeISequence(var sI1, sI2, sI3 : string;
      var bI1Reverse, bI2Reverse, bI3Reverse : Boolean);
  end;





implementation

function GetPhaseTypeStr( AType : TWE_PHASE_TYPE ) : string;
begin
  case AType of
    ptThree: Result := '三相三线';
    ptFour:  Result := '三相四线';
    ptFourPT:  Result := '三相四线（电压互感）';
  else
    Result := '';
  end;
end;  

function GetSequenceTypeStr( AType : TWE_SEQUENCE_TYPE; bAbc : Integer ) : string;
begin
  {if bAbc then
  begin
    case AType of
      stACB: Result := 'acb';
      stBAC: Result := 'bac';
      stBCA: Result := 'bca';
      stCAB: Result := 'cab';
      stCBA: Result := 'cba';
    else     // stABC
      Result := 'abc';
    end;
  end
  else
  begin
    case AType of
      stACB: Result := 'uwv';
      stBAC: Result := 'vuw';
      stBCA: Result := 'vwu';
      stCAB: Result := 'wuv';
      stCBA: Result := 'wvu';
    else     // stABC
      Result := 'uvw';
    end;
  end;  }
  case bAbc of
    0:
    begin
      case AType of
        stACB: Result := 'acb';
        stBAC: Result := 'bac';
        stBCA: Result := 'bca';
        stCAB: Result := 'cab';
        stCBA: Result := 'cba';
      else     // stABC
        Result := 'abc';
      end;
    end;
    1:
    begin
      case AType of
        stACB: Result := 'uwv';
        stBAC: Result := 'vuw';
        stBCA: Result := 'vwu';
        stCAB: Result := 'wuv';
        stCBA: Result := 'wvu';
      else     // stABC
        Result := 'uvw';
      end;
    end;
    2:
    begin
      case AType of
        stACB: Result := '132';
        stBAC: Result := '213';
        stBCA: Result := '231';
        stCAB: Result := '312';
        stCBA: Result := '321';
      else     // stABC
        Result := '123';
      end;
    end;  
  end;
end;

function GetPhaseLineTypeStr( AType : TWE_PHASE_LINE_TYPE; bAbc : Integer ) : string;
begin
  {if bAbc then
  begin
    case AType of
      plA: Result := 'a';
      plB: Result := 'b';
      plC: Result := 'c';
    else
      Result := 'n';
    end;
  end
  else
  begin
    case AType of
      plA: Result := 'u';
      plB: Result := 'v';
      plC: Result := 'w';
    else
      Result := 'n';
    end;
  end;  }
  case bAbc of
    0:
    begin
      case AType of
        plA: Result := 'a';
        plB: Result := 'b';
        plC: Result := 'c';
      else
        Result := 'n';
      end;
    end;
    1:
    begin
      case AType of
        plA: Result := 'u';
        plB: Result := 'v';
        plC: Result := 'w';
      else
        Result := 'n';
      end;
    end;
    2:
    begin
      case AType of
        plA: Result := '1';
        plB: Result := '2';
        plC: Result := '3';
      else
        Result := 'n';
      end;
    end;  
  end;
end;   

{ TWIRING_ERROR }

procedure TWIRING_ERROR.Assign(Source: TPersistent);
begin
  Assert( Source is TWIRING_ERROR );
  FPhaseType   := TWIRING_ERROR( Source ).PhaseType  ;
  FUSequence   := TWIRING_ERROR( Source ).USequence  ;
  FUaBroken    := TWIRING_ERROR( Source ).UaBroken   ;
  FUbBroken    := TWIRING_ERROR( Source ).UbBroken   ;
  FUcBroken    := TWIRING_ERROR( Source ).UcBroken   ;
  FUnBroken    := TWIRING_ERROR( Source ).UnBroken   ;
  FUsaBroken   := TWIRING_ERROR( Source ).UsaBroken  ;
  FUsbBroken   := TWIRING_ERROR( Source ).UsbBroken  ;
  FUscBroken   := TWIRING_ERROR( Source ).UscBroken  ;
  FUsnBroken   := TWIRING_ERROR( Source ).UsnBroken  ;   //胡红明2013.5.13
  FPT1Reverse  := TWIRING_ERROR( Source ).PT1Reverse ;
  FPT2Reverse  := TWIRING_ERROR( Source ).PT2Reverse ;
  FPT3Reverse  := TWIRING_ERROR( Source ).PT3Reverse ;   //胡红明2013.5.13
  FGroundBroken:= TWIRING_ERROR( Source ).GroundBroken;  //胡红明2013.5.14
  FISequence   := TWIRING_ERROR( Source ).ISequence  ;
  FIaBroken    := TWIRING_ERROR( Source ).IaBroken   ;
  FIbBroken    := TWIRING_ERROR( Source ).IbBroken   ;
  FIcBroken    := TWIRING_ERROR( Source ).IcBroken   ;
  FInBroken    := TWIRING_ERROR( Source ).InBroken   ;
  FCT1Short    := TWIRING_ERROR( Source ).CT1Short   ;
  FCT2Short    := TWIRING_ERROR( Source ).CT2Short   ;
  FCT3Short    := TWIRING_ERROR( Source ).CT3Short   ;
  FCT1Reverse  := TWIRING_ERROR( Source ).CT1Reverse ;
  FCT2Reverse  := TWIRING_ERROR( Source ).CT2Reverse ;
  FCT3Reverse  := TWIRING_ERROR( Source ).CT3Reverse ;
  FI1In        := TWIRING_ERROR( Source ).I1In       ;
  FI1Out       := TWIRING_ERROR( Source ).I1Out      ;
  FI2In        := TWIRING_ERROR( Source ).I2In       ;
  FI2Out       := TWIRING_ERROR( Source ).I2Out      ;
  FI3In        := TWIRING_ERROR( Source ).I3In       ;
  FI3Out       := TWIRING_ERROR( Source ).I3Out      ;


  FIaGroundBroken  := TWIRING_ERROR( Source ).IaGroundBroken;
  FIbGroundBroken  := TWIRING_ERROR( Source ).IbGroundBroken;
  FIcGroundBroken  := TWIRING_ERROR( Source ).IcGroundBroken;
  FIGroundBroken  := TWIRING_ERROR( Source ).IGroundBroken;

  FI1Reverse := TWIRING_ERROR( Source ).I1Reverse;
  FI2Reverse := TWIRING_ERROR( Source ).I2Reverse;
  FI3Reverse := TWIRING_ERROR( Source ).I3Reverse;
//  FTemp := TWIRING_ERROR( Source ).Temp;
  FIsClearLinke := TWIRING_ERROR( Source ).IsClearLinke;
  FIsCanSetClearLinkeError := TWIRING_ERROR( Source ).IsCanSetClearLinkeError;

//  FProVersion  := TWIRING_ERROR( Source ).ProVersion ;
end;

function TWIRING_ERROR.BitIsOne(AIntValue, ABitPos: Integer): Boolean;
begin
  if ABitPos in [ 0..32 ] then
    Result := AIntValue and ( 1 shl ABitPos ) = 1 shl ABitPos
  else
    Result := False;
end;

procedure TWIRING_ERROR.Clear;
begin
  FUSequence := stABC;
  FUaBroken  := False;
  FUbBroken  := False;
  FUcBroken  := False;
  FUnBroken  := False;

  FUsaBroken  := False;
  FUsbBroken  := False;
  FUscBroken  := False;
  FPT1Reverse := False;
  FPT2Reverse := False;
  FPT3Reverse := False; //胡红明2013.5.21

  FISequence  := stABC;
  FIaBroken   := False;
  FIbBroken   := False;
  FIcBroken   := False;
  FInBroken   := False;
  FCT1Short   := False;
  FCT2Short   := False;
  FCT3Short   := False;
  FCT1Reverse := False;
  FCT2Reverse := False;
  FCT3Reverse := False;

  FIaGroundBroken := False;
  FIbGroundBroken := False;
  FIcGroundBroken := False;
//  FIsClearLinke := False;
//  FIsCanSetClearLinkeError := False;

  if FPhaseType = ptThree then
  begin
    FI1In  := plA;
    FI1Out := plN;
    FI2In  := plC;
    FI2Out := plN;
  end
  else
  begin
    FI1In  := plA;
    FI1Out := plN;
    FI2In  := plB;
    FI2Out := plN;
    FI3In  := plC;
    FI3Out := plN;
  end;



end;

constructor TWIRING_ERROR.Create;
begin
  FPhaseType := ptThree;
//  FProVersion := 'V1';
  Clear;
end;

procedure TWIRING_ERROR.GetClearLinkeISequence(var sI1, sI2, sI3: string;
  var bI1Reverse, bI2Reverse, bI3Reverse: Boolean);
  function GetInOut(AI1In, AI1Out, AI2In, AI2Out, AI3In, AI3Out : TWE_PHASE_LINE_TYPE):Boolean;
  begin
    Result := ([FI1In , FI1Out] = [AI1In, AI1Out]) and
       ([FI2In , FI2Out] = [AI2In, AI2Out]) and
       ([FI3In , FI3Out] = [AI3In, AI3Out]);

    if Result then
    begin
      bI1Reverse := (FI1In = AI1Out) and (FI1Out = AI1In);
      bI2Reverse := (FI2In = AI2Out) and (FI2Out = AI2In);
      bI3Reverse := (FI3In = AI3Out) and (FI3Out = AI3In);
    end;
  end;
var
  s : string;
begin
//  s := UpperCase(sI1) + UpperCase(sI2) + UpperCase(sI3);

  if GetInOut(plA, plN, plB, plN,plC, plN) then s := 'ABC';
  if GetInOut(plA, plN, plC, plN,plB, plN) then s := 'ACB';
  if GetInOut(plA, plC, plB, plC,plN, plC) then s := 'ABN';
  if GetInOut(plA, plB, plC, plB,plN, plB) then s := 'ACN';
  if GetInOut(plA, plC, plN, plC,plB, plC) then s := 'ANB';
  if GetInOut(plA, plB, plN, plB,plC, plB) then s := 'ANC';
  if GetInOut(plB, plC, plA, plC,plN, plC) then s := 'BAN';
  if GetInOut(plB, plA, plC, plA,plN, plA) then s := 'BCN';
  if GetInOut(plB, plC, plN, plC,plA, plC) then s := 'BNA';
  if GetInOut(plB, plA, plN, plA,plC, plA) then s := 'BNC';
  if GetInOut(plB, plN, plA, plN,plC, plN) then s := 'BAC';
  if GetInOut(plB, plN, plC, plN,plA, plN) then s := 'BCA';
  if GetInOut(plC, plA, plB, plA,plN, plA) then s := 'CBN';
  if GetInOut(plC, plB, plA, plB,plN, plB) then s := 'CAN';
  if GetInOut(plC, plA, plN, plA,plB, plA) then s := 'CNB';
  if GetInOut(plC, plB, plN, plB,plA, plB) then s := 'CNA';
  if GetInOut(plC, plN, plA, plN,plB, plN) then s := 'CAB';
  if GetInOut(plC, plN, plB, plN,plA, plN) then s := 'CBA';
  if GetInOut(plN, plA, plB, plA,plC, plA) then s := 'NBC';
  if GetInOut(plN, plC, plB, plC,plA, plC) then s := 'NBA';
  if GetInOut(plN, plA, plC, plA,plB, plA) then s := 'NCB';
  if GetInOut(plN, plB, plC, plB,plA, plB) then s := 'NCA';
  if GetInOut(plN, plC, plA, plC,plB, plC) then s := 'NAB';
  if GetInOut(plN, plB, plA, plB,plC, plB) then s := 'NAC';


  if Length(s) = 3 then
  begin
    sI1 := s[1];
    sI2 := s[2];
    sI3 := s[3];

  end;
end;

function TWIRING_ERROR.GetDescription: string;
var
  sl : TStrings;
begin
  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
  begin
    Fabc := ReadInteger( 'Like', 'abc', 0 );
    Fptct := ReadBool( 'Like', 'PTCT', True );
    Free;
  end;

  sl := TStringList.Create;
  GetDescriptionText( sl );


  if sl.Count = 0 then
    Result := '正确接线'
  else
  begin
    Result := StringReplace( sl.Text, #13#10, '; ', [rfReplaceAll] );
    Result := Copy( Result, 1, Length( Result ) - 2 );
    if Fabc = 0 then
      StringReplace( sl.Text, #13#10, '; ', [rfReplaceAll] );
  end;

  

  sl.Free;
end;

procedure TWIRING_ERROR.GetDescriptionText(AText: TStrings);
  function GetPhaseValue(nPhase : Integer) : string;
  begin
    Result := '';
    case Fabc of
      0:
      begin
        case nPhase of
          1 : Result := 'Ia';
          2 : Result := 'Ib';
          3 : Result := 'Ic';
          4 : Result := 'In';
        end;
      end;
      1:
      begin
        case nPhase of
          1 : Result := 'Iu';
          2 : Result := 'Iv';
          3 : Result := 'Iw';
          4 : Result := 'In';
        end;
      end;
      2:
      begin
        case nPhase of
          1 : Result := 'I1';
          2 : Result := 'I2';
          3 : Result := 'I3';
          4 : Result := 'In';
        end;
      end;
    end;
  end;
var
  s : string;
  sI1, sI2, sI3 : string;
  bI1Reverse, bI2Reverse, bI3Reverse : Boolean;

begin
  if not Assigned( AText ) then
    Exit;

  AText.Clear;

  if FUSequence <> stABC then
      AText.Add( 'U' +  GetSequenceTypeStr( FUSequence, Fabc ) );

  if FPhaseType = ptFour then
  begin
    if FUaBroken or FUbBroken or FUcBroken or FUnBroken then
    begin
      s := EmptyStr;
      case Fabc of
        0:
        begin
          if FUaBroken then s := s + 'Ua,';
          if FUbBroken then s := s + 'Ub,';
          if FUcBroken then s := s + 'Uc,';
          if FUnBroken then s := s + 'Un,';
        end;
        1: begin
          if FUaBroken then s := s + 'Uu,';
          if FUbBroken then s := s + 'Uv,';
          if FUcBroken then s := s + 'Uw,';
          if FUnBroken then s := s + 'Un,';
        end;
        2:
        begin
          if FUaBroken then s := s + 'U1,';
          if FUbBroken then s := s + 'U2,';
          if FUcBroken then s := s + 'U3,';
          if FUnBroken then s := s + 'Un,';
        end;
      end;
     { if Fabc then
      begin
        if FUaBroken then s := s + 'Ua,';
        if FUbBroken then s := s + 'Ub,';
        if FUcBroken then s := s + 'Uc,';
        if FUnBroken then s := s + 'Un,';
      end
      else
      begin
        if FUaBroken then s := s + 'Uu,';
        if FUbBroken then s := s + 'Uv,';
        if FUcBroken then s := s + 'Uw,';
        if FUnBroken then s := s + 'Un,';
      end;  }

      AText.Add( Copy( s, 1, Length( s ) - 1 )  + '电压断');
    end;
  end
  else  if  FPhaseType = ptThree then
  begin
    if FUaBroken or FUbBroken or FUcBroken then
    begin
      s := EmptyStr;
      case Fabc of
        0:
        begin
          if FUaBroken then s := s + 'A相,';
          if FUbBroken then s := s + 'B相,';
          if FUcBroken then s := s + 'C相,';
        end;
        1:
        begin
          if FUaBroken then s := s + 'U相,';
          if FUbBroken then s := s + 'V相,';
          if FUcBroken then s := s + 'W相,';
        end;
        2:
        begin
          if FUaBroken then s := s + '1相,';
          if FUbBroken then s := s + '2相,';
          if FUcBroken then s := s + '3相,';
        end;
      end;
     { if Fabc then
      begin
        if FUaBroken then s := s + 'A相,';
        if FUbBroken then s := s + 'B相,';
        if FUcBroken then s := s + 'C相,';
      end
      else
      begin
        if FUaBroken then s := s + 'U相,';
        if FUbBroken then s := s + 'V相,';
        if FUcBroken then s := s + 'W相,';
      end; }
      AText.Add( Copy( s, 1, Length( s ) - 1 )  + ' 一次失压');
    end;

    if FUsaBroken or FUsbBroken or FUscBroken then
    begin
      s := EmptyStr;
     { if Fabc then
      begin
        if FUsaBroken then s := s + 'a相,';
        if FUsbBroken then s := s + 'b相,';
        if FUscBroken then s := s + 'c相,';
      end
      else
      begin
        if FUsaBroken then s := s + 'u相,';
        if FUsbBroken then s := s + 'v相,';
        if FUscBroken then s := s + 'w相,';
      end;  }
      case Fabc of
        0:
        begin
          if FUsaBroken then s := s + 'a相,';
          if FUsbBroken then s := s + 'b相,';
          if FUscBroken then s := s + 'c相,';
        end;
        1:
        begin
          if FUsaBroken then s := s + 'u相,';
          if FUsbBroken then s := s + 'v相,';
          if FUscBroken then s := s + 'w相,';
        end;
        2:
        begin
          if FUsaBroken then s := s + '1相,';
          if FUsbBroken then s := s + '2相,';
          if FUscBroken then s := s + '3相,';
        end;      
      end;

      AText.Add(  Copy( s, 1, Length( s ) - 1 ) + '二次失压');
    end;

    if FGroundBroken then  AText.Add('电压接地断开');

    if FPT1Reverse or FPT2Reverse then
    begin
      s := EmptyStr;

      if Fptct then
      begin
        if FPT1Reverse then s := s + 'PT1,';
        if FPT2Reverse then s := s + 'PT2,';
      end
      else
      begin
        if FPT1Reverse then s := s + 'TV1,';
        if FPT2Reverse then s := s + 'TV2,';
      end;

      AText.Add( Copy( s, 1, Length( s ) - 1 ) + '极性反' );
    end;
  end
  else if FPhaseType = ptFourPT then
  begin
    if FUaBroken or FUbBroken or FUcBroken then
    begin
      s := EmptyStr;
     { if Fabc then
      begin
        if FUaBroken then s := s + 'A相,';
        if FUbBroken then s := s + 'B相,';
        if FUcBroken then s := s + 'C相,';

      end
      else
      begin
        if FUaBroken then s := s + 'U相,';
        if FUbBroken then s := s + 'V相,';
        if FUcBroken then s := s + 'W相,';
      end;   }
      case Fabc of
        0:
        begin
          if FUaBroken then s := s + 'A相,';
          if FUbBroken then s := s + 'B相,';
          if FUcBroken then s := s + 'C相,';
        end;
        1:
        begin
          if FUaBroken then s := s + 'U相,';
          if FUbBroken then s := s + 'V相,';
          if FUcBroken then s := s + 'W相,';
        end;
        2:
        begin
          if FUaBroken then s := s + '1相,';
          if FUbBroken then s := s + '2相,';
          if FUcBroken then s := s + '3相,';
        end;     
      end;

      AText.Add( Copy( s, 1, Length( s ) - 1 ) + '一次失压' );
    end;

    if FUsaBroken or FUsbBroken or FUscBroken or
                     FUsnBroken or FGroundBroken then
    begin
      s := EmptyStr;
      {if Fabc then
      begin
        if FUsaBroken then s := s + 'a相,';
        if FUsbBroken then s := s + 'b相,';
        if FUscBroken then s := s + 'c相,';
      end
      else
      begin
        if FUsaBroken then s := s + 'u相,';
        if FUsbBroken then s := s + 'v相,';
        if FUscBroken then s := s + 'w相,';
      end; }
      case Fabc of
        0:
        begin
          if FUsaBroken then s := s + 'a相,';
          if FUsbBroken then s := s + 'b相,';
          if FUscBroken then s := s + 'c相,';
        end;
        1:
        begin
          if FUsaBroken then s := s + 'u相,';
          if FUsbBroken then s := s + 'v相,';
          if FUscBroken then s := s + 'w相,';
        end;
        2:
        begin
          if FUsaBroken then s := s + '1相,';
          if FUsbBroken then s := s + '2相,';
          if FUscBroken then s := s + '3相,';
        end;  
      end;
      if FUsnBroken then s := s + 'n相,';
      AText.Add(Copy( s, 1, Length( s ) - 1 ) + '二次失压');





    end;
    if FGroundBroken then  AText.Add('电压接地断开');
    if FPT1Reverse or FPT2Reverse or FPT3Reverse then
    begin
      s := EmptyStr;

      if Fptct then
      begin
        if FPT1Reverse then s := s + 'PT1,';
        if FPT2Reverse then s := s + 'PT2,';
        if FPT3Reverse then s := s + 'PT3,';
      end
      else
      begin
        if FPT1Reverse then s := s + 'TV1,';
        if FPT2Reverse then s := s + 'TV2,';
        if FPT3Reverse then s := s + 'TV3,';
      end;

      AText.Add( Copy( s, 1, Length( s ) - 1 ) + '极性反' );
    end;
  end;

  if (FPhaseType = ptFour ) or (FPhaseType = ptFourPT )then
  begin
    s := EmptyStr;

    // 简化接线
    if FIsClearLinke then
    begin
      GetClearLinkeISequence(sI1, sI2, sI3,bI1Reverse, bI2Reverse, bI3Reverse);

      if sI1 + sI2 + sI3 <> 'ABC' then
        s := 'I' + LowerCase(sI1 + sI2 + sI3)
    end
    else
    begin
      if FISequence <> stABC then
        s := 'I' + GetSequenceTypeStr( FISequence, Fabc )
    end;

    if FI1Reverse then
      s := s + ' 元件1反接';

    if FI2Reverse then
      s := s + ' 元件2反接';

    if FI3Reverse then
      s := s + ' 元件3反接';

    if s <> EmptyStr then
      AText.Add( s );



  end
  else
  begin
    s := EmptyStr;

    // 三相三相三线简化接线
    if FIsClearLinke then
    begin

      // 表尾电流接线
      if (I1In in [plA, plN]) and (I1Out in [plA, plN]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]) then
      begin
//        s := GetPhaseValue(1) +GetPhaseValue(3);
        s := '';
        if (I1In = plN) and (I1Out = plA) then
          s := s + ' 元件1反接';

        if (I2In = plN) and (I2Out = plC) then
          s := s + ' 元件2反接';
      end;

      if (I1In in [plC, plN]) and (I1Out in [plC, plN]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]) then
      begin
        s := GetPhaseValue(3) +GetPhaseValue(1);
        if (I1In = plN) and (I1Out = plC) then
          s := s + ' 元件1反接';

        if (I2In = plN) and (I2Out = plA) then
          s := s + ' 元件2反接';
      end;
      if (I1In in [plA, plC]) and (I1Out in [plA, plC]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]) then
      begin
        s := GetPhaseValue(1) +GetPhaseValue(2);
        if (I1In = plC) and (I1Out = plA) then
          s := s + ' 元件1反接';

        if (I2In = plC) and (I2Out = plN) then
          s := s + ' 元件2反接';
      end;

      if (I1In in [plC, plA]) and (I1Out in [plC, plA]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]) then
      begin
        s := GetPhaseValue(3) +GetPhaseValue(2);
        if (I1In = plA) and (I1Out = plC) then
          s := s + ' 元件1反接';

        if (I2In = plA) and (I2Out = plN) then
          s := s + ' 元件2反接';
      end;

      if (I1In in [plN, plA]) and (I1Out in [plN, plA]) and (I2In in [plC, plA]) and (I2Out in [plC, plA]) then
      begin
        s := GetPhaseValue(2) +GetPhaseValue(3);
        if (I1In = plA) and (I1Out = plN) then
          s := s + ' 元件1反接';

        if (I2In = plA) and (I2Out = plC) then
          s := s + ' 元件2反接';
      end;

      if (I1In in [plN, plC]) and (I1Out in [plN, plC]) and (I2In in [plA, plC]) and (I2Out in [plA, plC]) then
      begin
        s := GetPhaseValue(2) +GetPhaseValue(1);
        if (I1In = plC) and (I1Out = plN) then
          s := s + ' 元件1反接';

        if (I2In = plC) and (I2Out = plA) then
          s := s + ' 元件2反接';
      end;
    end
    else
    begin
      if ( FI1In <> plA ) and ( FI1Out <> plA ) then
      begin
        case Fabc of
          0: s := 'IcIa';
          1: s := 'IwIu';
          2: s := 'I3I1';
        end;
      end;

      if FI1In = plN then
      begin
        s := s + ' 元件1反接';
      end;

      if FI2In = plN then
      begin
        s := s + ' 元件2反接';
      end;
    end;



//    if ( FI1In <> plA ) or ( FI1Out <> plN ) then
//      s := s + Format( '一件进I%s出I%s,', [ GetPhaseLineTypeStr( FI1In, Fabc ),
//        GetPhaseLineTypeStr( FI1Out ) ] );
//
//    if ( FI2In <> plC ) or ( FI2Out <> plN ) then
//      s := s + Format( '二元件进I%s出I%s,', [ GetPhaseLineTypeStr( FI2In, Fabc ),
//        GetPhaseLineTypeStr( FI2Out ) ] );

    if s <> EmptyStr then
      AText.Add(  s );
  end;

  if FCT1Reverse or FCT2Reverse or FCT3Reverse then
  begin
    s := EmptyStr;
    if Fptct then
    begin
      if FCT1Reverse then s := s + 'CT1,';
      if FCT2Reverse then s := s + 'CT2,';
      if FCT3Reverse then s := s + 'CT3,';
    end
    else
    begin
      if FCT1Reverse then s := s + 'TA1,';
      if FCT2Reverse then s := s + 'TA2,';
      if FCT3Reverse then s := s + 'TA3,';
    end;


    AText.Add(Copy( s, 1, Length( s ) - 1 ) + '极性反');
  end;

  if FCT1Short or FCT2Short or FCT3Short then
  begin
    s := EmptyStr;
    if Fptct then
    begin
      if FCT1Short then s := s + 'CT1,';
      if FCT2Short then s := s + 'CT2,';
      if FCT3Short then s := s + 'CT3,';
    end
    else
    begin
      if FCT1Short then s := s + 'TA1,';
      if FCT2Short then s := s + 'TA2,';
      if FCT3Short then s := s + 'TA3,';
    end;


    AText.Add(Copy( s, 1, Length( s ) - 1 ) + '短路');
  end;

  if FIaBroken or FIbBroken or FIcBroken or FInBroken then
  begin
    s := EmptyStr;
   { if Fabc then
    BEGIN
      if FIaBroken then s := s + 'Ia,';
      if FIbBroken then s := s + 'Ib,';
      if FIcBroken then s := s + 'Ic,';
      if FInBroken then s := s + 'In,';
    END
    ELSE
    begin
      if FIaBroken then s := s + 'Iu,';
      if FIbBroken then s := s + 'Iv,';
      if FIcBroken then s := s + 'Iw,';
      if FInBroken then s := s + 'In,';
    end;   }
    case Fabc of
      0:
      begin
        if FIaBroken then s := s + 'Ia,';
        if FIbBroken then s := s + 'Ib,';
        if FIcBroken then s := s + 'Ic,';
        if FInBroken then s := s + 'In,';
      end;
      1:
      begin
        if FIaBroken then s := s + 'Iu,';
        if FIbBroken then s := s + 'Iv,';
        if FIcBroken then s := s + 'Iw,';
        if FInBroken then s := s + 'In,';
      end;
      2:
      begin
        if FIaBroken then s := s + 'I1,';
        if FIbBroken then s := s + 'I2,';
        if FIcBroken then s := s + 'I3,';
        if FInBroken then s := s + 'In,';
      end;  
    end;

    AText.Add( Copy( s, 1, Length( s ) - 1 ) + '开路' );
  end;

  if FIaGroundBroken or FIbGroundBroken or FIcGroundBroken or FIGroundBroken then
  begin
    s := EmptyStr;

    if FIaGroundBroken then s := s + 'Ia,';
    if FIbGroundBroken then s := s + 'Ib,';
    if FIcGroundBroken then s := s + 'Ic,';
//    if FIcGroundBroken then s := s + '全部,';

    AText.Add( Copy( s, 1, Length( s ) - 1 ) + '接地断开' );
  end;

end;

function TWIRING_ERROR.GetErrorCount: Integer;
  function CalErrorCount3(AErrCode: Integer): Integer;
  var
    i: Integer;
  begin
    Result := 0;

    for i := 0 to 7 do
      if AErrCode and ( 1 shl i ) = 1 shl i then
        Inc( Result );

    if (AErrCode and ( 1 shl 8 )+AErrCode and ( 1 shl 9 )) >0 then
      Inc( Result );

    for i := 16 to 23 do
      if AErrCode and ( 1 shl i ) = 1 shl i then
        Inc( Result );

    if AErrCode and ( 7 shl 24 ) <> 0 then
      Inc( Result );
  end;

  function CalErrorCount4(AErrCode: Integer): Integer;
  var
    i: Integer;
  begin
    Result := 0;

    for i := 0 to 10 do
      if AErrCode and ( 1 shl i ) = 1 shl i then
        Inc( Result );

    for i := 16 to 19 do
      if AErrCode and ( 1 shl i ) = 1 shl i then
        Inc( Result );

    if AErrCode and ( 7 shl 12 ) <> 0 then
      Inc( Result );

    if AErrCode and ( 7 shl 24 ) <> 0 then
      Inc( Result );
  end;

begin
  if FPhaseType = ptThree then
    Result := CalErrorCount3(ID)
  else
    Result := CalErrorCount4(ID);
end;

function TWIRING_ERROR.GetID: Cardinal;
begin
  if FPhaseType = ptThree then
    Result := GetIDThree
  else if FPhaseType = ptFour then
    Result := GetIDFour
  else
    Result := GetIDFourPT;
end;

function TWIRING_ERROR.GetIDFour: Cardinal;
begin
  // 四线编码, 此处参考编码规则
  Result := C_WE_ID_BASE_FOUR;

  Result := Result + Cardinal( FUSequence ) shl 24;
  Result := Result + Cardinal( FUaBroken ) shl 16;
  Result := Result + Cardinal( FUbBroken ) shl 17;
  Result := Result + Cardinal( FUcBroken ) shl 18;
  Result := Result + Cardinal( FUnBroken ) shl 19;

  Result := Result + Cardinal( FISequence ) shl 12;
  Result := Result + Cardinal( FCT1Reverse ) shl 8;
  Result := Result + Cardinal( FCT2Reverse ) shl 9;
  Result := Result + Cardinal( FCT3Reverse ) shl 10;
  Result := Result + Cardinal( FCT1Short ) shl 4;
  Result := Result + Cardinal( FCT2Short ) shl 5;
  Result := Result + Cardinal( FCT3Short ) shl 6;
  Result := Result + Cardinal( FIaBroken ) shl 0;
  Result := Result + Cardinal( FIbBroken ) shl 1;
  Result := Result + Cardinal( FIcBroken ) shl 2;
end;

function TWIRING_ERROR.GetIDFourPT: Cardinal;
begin
  // 四线编码, 此处参考编码规则
  Result := C_WE_ID_BASE_FOUR_PT;

  Result := Result + Cardinal( FUSequence ) shl 24;
  Result := Result + Cardinal( FUaBroken ) shl 16;
  Result := Result + Cardinal( FUbBroken ) shl 17;
  Result := Result + Cardinal( FUcBroken ) shl 18;
  Result := Result + Cardinal( FUnBroken ) shl 19;
  //带PT  胡红明2013.5.28 位置不够，用了剩下的几个空位
  Result  := Result + Cardinal(FUsaBroken) shl 20;
  Result  := Result + Cardinal(FUsbBroken) shl 21;
  Result  := Result + Cardinal(FUscBroken) shl 22;
  Result  := Result + Cardinal(FUsnBroken) shl 23;

  Result :=  Result + Cardinal(FGroundBroken) shl 15;
  Result  := Result + Cardinal(FPT1Reverse) shl 3;
  Result  := Result + Cardinal(FPT2Reverse) shl 7;
  Result  := Result + Cardinal(FPT3Reverse) shl 11;

  Result := Result + Cardinal( FISequence ) shl 12;
  Result := Result + Cardinal( FCT1Reverse ) shl 8;
  Result := Result + Cardinal( FCT2Reverse ) shl 9;
  Result := Result + Cardinal( FCT3Reverse ) shl 10;
  Result := Result + Cardinal( FCT1Short ) shl 4;
  Result := Result + Cardinal( FCT2Short ) shl 5;
  Result := Result + Cardinal( FCT3Short ) shl 6;
  Result := Result + Cardinal( FIaBroken ) shl 0;
  Result := Result + Cardinal( FIbBroken ) shl 1;
  Result := Result + Cardinal( FIcBroken ) shl 2;

  // 新加表尾电流反接
  Result  := Result + Cardinal(FI1Reverse) shl 28;
  Result  := Result + Cardinal(FI2Reverse) shl 29;
  Result  := Result + Cardinal(FI3Reverse) shl 30;
end;

function TWIRING_ERROR.GetIDThree: Cardinal;
begin
  // 三线编码, 此处参考编码规则
  Result := C_WE_ID_BASE_THREE;

  Result := Result + Cardinal( FUSequence ) shl 24;
  Result := Result + Cardinal( FUaBroken ) shl 16;
  Result := Result + Cardinal( FUbBroken ) shl 17;
  Result := Result + Cardinal( FUcBroken ) shl 18;
  Result := Result + Cardinal( FUsaBroken ) shl 19;
  Result := Result + Cardinal( FUsbBroken ) shl 20;
  Result := Result + Cardinal( FUscBroken ) shl 21;
  Result := Result + Cardinal( FPT1Reverse ) shl 22;
  Result := Result + Cardinal( FPT2Reverse ) shl 23;

  /////////???????????////
  case FI1In of
    plA: Result := Result + 0 shl 8;
    plN: Result := Result + 1 shl 8;
    plC: Result := Result + 2 shl 8;
  end;

  case FI1Out of
    plN : Result := Result + 0 shl 10;
    plA : Result := Result + 1 shl 10;
    plC : Result := Result + 2 shl 10;
  end;

  case FI2In of
    plC: Result := Result + 0 shl 12;
    plN: Result := Result + 1 shl 12;
    plA: Result := Result + 2 shl 12;
  end;

  case FI2Out of
    plN: Result := Result + 0 shl 14;
    plA: Result := Result + 1 shl 14;
    plC: Result := Result + 2 shl 14;
  end;

  Result := Result + Cardinal( FCT1Reverse ) shl 6;
  Result := Result + Cardinal( FCT2Reverse ) shl 7;
  Result := Result + Cardinal( FCT1Short ) shl 4;
  Result := Result + Cardinal( FCT2Short ) shl 5;
  Result := Result + Cardinal( FIaBroken ) shl 0;
  Result := Result + Cardinal( FIcBroken ) shl 2;
  Result := Result + Cardinal( FInBroken ) shl 3;
end;

function TWIRING_ERROR.IDInStr: string;
begin
  Result := IntToHex( GetID, 8 );
end;

function TWIRING_ERROR.IsCorrect: Boolean;
begin
  if FPhaseType = ptThree then
    Result := GetID = C_WE_ID_CORRECT_THREE
  else if FPhaseType = ptFour then
    Result := GetID = C_WE_ID_CORRECT_FOUR
  else
    Result := GetID = C_WE_ID_CORRECT_FOUR_PT;
end;

procedure TWIRING_ERROR.SetClearLinkeISequence(sI1, sI2, sI3: string;
  bI1Reverse, bI2Reverse, bI3Reverse : Boolean);
  procedure SetInOut(AI1In, AI1Out, AI2In, AI2Out, AI3In, AI3Out : TWE_PHASE_LINE_TYPE);
  begin
    if bI1Reverse then
    begin
      I1In := AI1Out;
      I1Out := AI1In;
    end
    else
    begin
      I1In := AI1In;
      I1Out := AI1Out;
    end;

    if bI2Reverse then
    begin
      I2In := AI2Out;
      I2Out := AI2In;
    end
    else
    begin
      I2In := AI2In;
      I2Out := AI2Out;
    end;

    if bI3Reverse then
    begin
      I3In := AI3Out;
      I3Out := AI3In;
    end
    else
    begin
      I3In := AI3In;
      I3Out := AI3Out;
    end;
  end;
var
  s : string;
begin
  s := UpperCase(sI1) + UpperCase(sI2) + UpperCase(sI3);

  if s = 'ABC' then SetInOut(plA, plN, plB, plN,plC, plN);
  if s = 'ACB' then SetInOut(plA, plN, plC, plN,plB, plN);
  if s = 'ABN' then SetInOut(plA, plC, plB, plC,plN, plC);
  if s = 'ACN' then SetInOut(plA, plB, plC, plB,plN, plB);
  if s = 'ANB' then SetInOut(plA, plC, plN, plC,plB, plC);
  if s = 'ANC' then SetInOut(plA, plB, plN, plB,plC, plB);
  if s = 'BAN' then SetInOut(plB, plC, plA, plC,plN, plC);
  if s = 'BCN' then SetInOut(plB, plA, plC, plA,plN, plA);
  if s = 'BNA' then SetInOut(plB, plC, plN, plC,plA, plC);
  if s = 'BNC' then SetInOut(plB, plA, plN, plA,plC, plA);
  if s = 'BAC' then SetInOut(plB, plN, plA, plN,plC, plN);
  if s = 'BCA' then SetInOut(plB, plN, plC, plN,plA, plN);
  if s = 'CBN' then SetInOut(plC, plA, plB, plA,plN, plA);
  if s = 'CAN' then SetInOut(plC, plB, plA, plB,plN, plB);
  if s = 'CNB' then SetInOut(plC, plA, plN, plA,plB, plA);
  if s = 'CNA' then SetInOut(plC, plB, plN, plB,plA, plB);
  if s = 'CAB' then SetInOut(plC, plN, plA, plN,plB, plN);
  if s = 'CBA' then SetInOut(plC, plN, plB, plN,plA, plN);
  if s = 'NBC' then SetInOut(plN, plA, plB, plA,plC, plA);
  if s = 'NBA' then SetInOut(plN, plC, plB, plC,plA, plC);
  if s = 'NCB' then SetInOut(plN, plA, plC, plA,plB, plA);
  if s = 'NCA' then SetInOut(plN, plB, plC, plB,plA, plB);
  if s = 'NAB' then SetInOut(plN, plC, plA, plC,plB, plC);
  if s = 'NAC' then SetInOut(plN, plB, plA, plB,plC, plB);
end;

procedure TWIRING_ERROR.SetI1In(const Value: TWE_PHASE_LINE_TYPE);
begin
  FI1In := Value;
end;

procedure TWIRING_ERROR.SetIaGroundBroken(const Value: Boolean);
begin
  FIaGroundBroken := Value;
end;

procedure TWIRING_ERROR.SetID(const Value: Cardinal);
begin
  Clear;  // 清空错误
  if Value and C_WE_ID_BASE_FOUR_PT = C_WE_ID_BASE_FOUR_PT then
    SetIDFourPT(Value)
  else
  if Value >= C_WE_ID_BASE_FOUR then
    SetIDFour( Value )
  else if Value >= C_WE_ID_BASE_THREE then
    SetIDThree( Value )
  else
    raise Exception.Create( '错误编码无效！' );
end;

procedure TWIRING_ERROR.SetIDFour(const Value: Cardinal);
begin
  FPhaseType := ptFour;
  // 四线编码, 此处参考编码规则
  FUSequence := TWE_SEQUENCE_TYPE( ( Value shr 24 ) and 7 );
  FUaBroken   := BitIsOne( Value, 16 );
  FUbBroken   := BitIsOne( Value, 17 );
  FUcBroken   := BitIsOne( Value, 18 );
  FUnBroken   := BitIsOne( Value, 19 );

  FISequence := TWE_SEQUENCE_TYPE( ( Value shr 12 ) and 7 );
  FCT1Reverse := BitIsOne( Value, 8 );
  FCT2Reverse := BitIsOne( Value, 9 );
  FCT3Reverse := BitIsOne( Value, 10 );
  FCT1Short   := BitIsOne( Value, 4 );
  FCT2Short   := BitIsOne( Value, 5 );
  FCT3Short   := BitIsOne( Value, 6 );
  FIaBroken   := BitIsOne( Value, 0 );
  FIbBroken   := BitIsOne( Value, 1 );
  FIcBroken   := BitIsOne( Value, 2 );
end;

procedure TWIRING_ERROR.SetIDFourPT(const Value: Cardinal);
begin
  FPhaseType := ptFourPT;
  // 四线编码, 此处参考编码规则
  FUSequence := TWE_SEQUENCE_TYPE( ( Value shr 24 ) and 7 );
  FUaBroken   := BitIsOne( Value, 16 );
  FUbBroken   := BitIsOne( Value, 17 );
  FUcBroken   := BitIsOne( Value, 18 );
  FUnBroken   := BitIsOne( Value, 19 );
  //带PT  胡红明2013.5.28
  FUsaBroken  := BitIsOne( Value, 20);
  FUsbBroken  := BitIsOne( Value, 21);
  FUscBroken  := BitIsOne( Value, 22);
  FUsnBroken  := BitIsOne( Value, 23);
  FGroundBroken :=  BitIsOne(Value, 15);
  FPT1Reverse  := BitIsOne(Value, 3);
  FPT2Reverse  := BitIsOne(Value, 7);
  FPT3Reverse  := BitIsOne(Value, 11);


  FISequence := TWE_SEQUENCE_TYPE( ( Value shr 12 ) and 7 );
  FCT1Reverse := BitIsOne( Value, 8 );
  FCT2Reverse := BitIsOne( Value, 9 );
  FCT3Reverse := BitIsOne( Value, 10 );
  FCT1Short   := BitIsOne( Value, 4 );
  FCT2Short   := BitIsOne( Value, 5 );
  FCT3Short   := BitIsOne( Value, 6 );
  FIaBroken   := BitIsOne( Value, 0 );
  FIbBroken   := BitIsOne( Value, 1 );
  FIcBroken   := BitIsOne( Value, 2 );

  FI1Reverse   := BitIsOne( Value, 28 );
  FI2Reverse   := BitIsOne( Value, 29 );
  FI3Reverse   := BitIsOne( Value, 30 );
end;

procedure TWIRING_ERROR.SetIDThree(const Value: Cardinal);
begin
  FPhaseType := ptThree;

  // 三线编码, 此处参考编码规则
  FUSequence := TWE_SEQUENCE_TYPE( ( Value shr 24 ) and 7 );
  FPT1Reverse := BitIsOne( Value, 22 );
  FPT2Reverse := BitIsOne( Value, 23 );
  FUsaBroken  := BitIsOne( Value, 19 );
  FUsbBroken  := BitIsOne( Value, 20 );
  FUscBroken  := BitIsOne( Value, 21 );
  FUaBroken   := BitIsOne( Value, 16 );
  FUbBroken   := BitIsOne( Value, 17 );
  FUcBroken   := BitIsOne( Value, 18 );

  //////////////????????????
  case ( Value shr 8 ) and 3 of
    0 : FI1In := plA;
    1 : FI1In := plN;
    2 : FI1In := plC;
  end;

  case ( Value shr 10 ) and 3 of
    0 : FI1Out := plN;
    1 : FI1Out := plA;
    2 : FI1Out := plC;
  end;

  case ( Value shr 12 ) and 3 of
    0 : FI2In := plC;
    1 : FI2In := plN;
    2 : FI2In := plA;
  end;

  case ( Value shr 14 ) and 3 of
    0 : FI2Out := plN;
    1 : FI2Out := plA;
    2 : FI2Out := plC;
  end;

  FCT1Reverse := BitIsOne( Value, 6 );
  FCT2Reverse := BitIsOne( Value, 7 );
  FCT1Short   := BitIsOne( Value, 4 );
  FCT2Short   := BitIsOne( Value, 5 );
  FIaBroken   := BitIsOne( Value, 0 );
  FIcBroken   := BitIsOne( Value, 2 );
  FInBroken   := BitIsOne( Value, 3 );
end;

procedure TWIRING_ERROR.SetPhaseType(const Value: TWE_PHASE_TYPE);
begin
  if Value <> FPhaseType then
  begin
    FPhaseType := Value;
    Clear;
  end;
end;

procedure TWIRING_ERROR.SetToCorrect;
begin
  if FPhaseType = ptThree then
    SetID( C_WE_ID_CORRECT_THREE )
  else if FPhaseType = ptFour then
    SetID( C_WE_ID_CORRECT_FOUR )
  else
    SetID( C_WE_ID_CORRECT_FOUR_PT )

end;

function TWIRING_ERROR.UHasBroken: Boolean;
begin
  Result := ( GetID shr 16 ) and $3F > 0;
end;

end.



