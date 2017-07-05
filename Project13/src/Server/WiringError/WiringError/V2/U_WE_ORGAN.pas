{===============================================================================
  Copyright(c) 2012, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，解析元件进出电压电流单元
  + TWE_DIAGRAM     接线图

===============================================================================}
unit U_WE_ORGAN;

interface

uses U_DIAGRAM_TYPE, SysUtils, Classes, U_WIRING_ERROR, xFunction;

type
  /// <summary>
  /// 向量线
  /// </summary>
  TVECTOR_LINE = class
  private
    FLineName: string;
    FLineAngle: Double;
    FIsVirtualLine: Boolean;
  public
    /// <summary>
    /// 线名称
    /// </summary>
    property LineName : string read FLineName write FLineName;

    /// <summary>
    /// 线角度0-360°，0°为竖直向上方向
    /// </summary>
    property LineAngle: Double read FLineAngle write FLineAngle;

    /// <summary>
    /// 是否为虚拟线
    /// </summary>
    property IsVirtualLine: Boolean read FIsVirtualLine write FIsVirtualLine;

    constructor Create( sLineName: string=''; dLineAngle: Double = 0 );

  end;

type
  /// <summary>
  /// 电压线
  /// </summary>
  TVECTOR_LINES = class
  private
    FLines: TStringList;

  public
    /// <summary>
    /// 向量线列表
    /// </summary>
    property Lines : TStringList read FLines write FLines;

    /// <summary>
    /// 根据线名称获取线对象
    /// </summary>
    function GetLine(sLineName:string) : TVECTOR_LINE;

    constructor Create;
    destructor Destroy; override;

  end;
var
  VectorLines : TVECTOR_LINES;

type
  /// <summary>
  /// 进出元件电流类型
  /// </summary>
  TI_TYPE = (itNot,itIa, itIb, itIc, itIn);

type
  /// <summary>
  /// 参数类型
  /// </summary>
  TPARAM_TYPE = (ptNot,     // 无参数
                 pt1_2     // 二分之一
                 );

type
  /// <summary>
  /// 元件类
  /// </summary>
  TWE_ORGAN = class
  private
    FUParam: TPARAM_TYPE;
    FIParam: TPARAM_TYPE;
    FUOutType: string;
    FIOutType: string;
    FUInType: string;
    FIInType: string;
    FUAngle: Double;
    FIAngle: Double;
    function GetItype: string;
    function GetUType: string;
    procedure SetIAngle(const Value: Double);
    procedure SetUAngle(const Value: Double);
    function ResetAngle( dAngle : Double ) : Double;
  public
    /// <summary>
    /// 电压参数
    /// </summary>
    property UParam : TPARAM_TYPE read FUParam write FUParam;
    /// <summary>
    /// 元件电压进端类型
    /// </summary>
    property UInType : string read FUInType write FUInType;
    /// <summary>
    /// 元件电压出端类型
    /// </summary>
    property UOutType : string read FUOutType write FUOutType;
    /// <summary>
    /// 电流参数
    /// </summary>
    property IParam : TPARAM_TYPE read FIParam write FIParam;
    /// <summary>
    /// 元件电流进端类型
    /// </summary>
    property IInType : string read FIInType write FIInType;
    /// <summary>
    /// 元件电流出端类型
    /// </summary>
    property IOutType : string read FIOutType write FIOutType;

    /// <summary>
    /// 电压类型
    /// </summary>
    property UType : string read GetUType;

    /// <summary>
    /// 电流类型
    /// </summary>
    property IType : string read GetItype;

    /// <summary>
    /// 电压线角度（画向量图时计算）
    /// </summary>
    property UAngle : Double read FUAngle write SetUAngle;

    /// <summary>
    /// 电流线角度（画向量图时计算，不包括φ角）
    /// </summary>
    property IAngle : Double read FIAngle write SetIAngle;

    /// <summary>
    /// 电流反向
    /// </summary>
    procedure IReverse;

    constructor Create;
  end;

/// <summary>
/// 解析错误类型到元件进出
/// </summary>
/// <param name="ADtype">接线类型</param>
/// <param name="WiringError">设置的错误</param>
/// <param name="AOrgans">有功/多功能元件列表</param>
/// <param name="ROrgans">无功元件列表</param>
procedure AnalysisOrgan( ADtype:TDiagramType; WiringError: TWIRING_ERROR;
  AOrgans, ROrgans : TStringList );


implementation

procedure AnalysisOrgan( ADtype:TDiagramType; WiringError: TWIRING_ERROR;
  AOrgans, ROrgans : TStringList );
  // ABC断相数量
  function GetUBreakCount : Integer;
  begin
    Result := 0;
    if WiringError.UaBroken or WiringError.UsaBroken then
      Inc(Result);
    if WiringError.UbBroken or WiringError.UsbBroken then
      Inc(Result);
    if WiringError.UcBroken or WiringError.UscBroken then
      Inc(Result);
  end;
  procedure Analysis3L4;
    function GetCurInOut(AInOut : TWE_PHASE_LINE_TYPE) : string;
    begin
      case AInOut of
        plA: Result := 'I_a';
        plB: Result := 'I_b';
        plC: Result := 'I_c';
        plN: Result := 'I_n';
      end;
    end;
    function IsBroken(nPhase:Integer ):Boolean;
    begin
      Result := False;
      case nPhase of
        1: Result := WiringError.UaBroken or WiringError.UsaBroken;
        2: Result := WiringError.UbBroken or WiringError.UsbBroken;
        3: Result := WiringError.UcBroken or WiringError.UscBroken;
      end;
    end;
   // 找第三边
    function GetLastPhase( s: string ):string;
    begin
      if (Pos('a', s) = 0) then
        Result := 'U_a'
      else if (Pos('b', s) = 0) then
        Result := 'U_b'
      else
        Result := 'U_c'
    end;

    // 断一相
    procedure BreakOne;
      function GetStr : string;
      begin
        if WiringError.UbBroken or WiringError.UsbBroken then
          Result := 'U_b'
        else if WiringError.UcBroken or WiringError.UscBroken then
          Result := 'U_c'
        else
          Result := 'U_a';
      end;
    var
      k : Integer;
      sPhase : string;
    begin
      for k := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[k]) do
        begin
          sPhase := GetLastPhase(UInType+UOutType);
          if sPhase = GetStr then
          begin
            UParam   := ptNot;
          end
          else
          begin
            if UInType = GetStr then
            begin
              UParam := ptNot;
              UInType := UOutType;
            end
            else
            begin
              UParam := pt1_2;
              UOutType := sPhase;
            end;
          end;
        end;
      end;
    end;
  var
    AOrgan : TWE_ORGAN;
    j: Integer;
  begin
    ClearStringList(ROrgans);

    for j := 0 to 1 do
    begin
      AOrgan := TWE_ORGAN.Create;
      ROrgans.AddObject(IntToStr(j+1), AOrgan);
    end;

    {电压部分}
    // 默认
    TWE_ORGAN(ROrgans.Objects[0]).UParam   := ptNot;
    TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_b';
    TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_c';

    TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
    TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_a';
    TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_c';

    // PT极性反

    if WiringError.PT1Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          UInType  := StringReplace(UInType, 'b','temp', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'b','temp', [rfReplaceAll]);

          UInType  := StringReplace(UInType, 'c','b', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'c','b', [rfReplaceAll]);

          UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
        end;
      end;
    end;

    if WiringError.PT2Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);

          UInType  := StringReplace(UInType, 'c','a', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'c','a', [rfReplaceAll]);

          UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
          UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
        end;
      end;
    end;
    // 电压相序
    case WiringError.USequence of
      stABC: ;
      stACB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'b','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','temp', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'c','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','b', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stBAC:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'b','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','a', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'temp','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stBCA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'c','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','a', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'b','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','c', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'temp','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stCAB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'b','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','a', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'c','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','b', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stCBA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'c','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','a', [rfReplaceAll]);
            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
    end;

    // ABC断一相
    if GetUBreakCount = 1 then
    begin
      BreakOne;
    end
    // ABC断两相或断三相
    else if GetUBreakCount > 1 then
    begin
      with WiringError do
      begin
        if IsBroken(1) and IsBroken(2) and (not IsBroken(3)) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_c';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_c';
          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_c';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_c';
        end
        else if IsBroken(1) and IsBroken(3) and (not IsBroken(2)) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_b';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_b';
          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_b';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_b';
        end
        else if IsBroken(2) and IsBroken(3) and (not IsBroken(1)) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_a';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_a';
          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_a';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_a';
        end
        else
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := '';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := '';
          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := '';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := '';
        end;
      end;
    end;


    {电流部分}
    // 默认
    TWE_ORGAN(ROrgans.Objects[0]).IInType  := 'I_a';
    TWE_ORGAN(ROrgans.Objects[0]).IOutType := 'I_n';
    TWE_ORGAN(ROrgans.Objects[1]).IInType  := 'I_c';
    TWE_ORGAN(ROrgans.Objects[1]).IOutType := 'I_n';

    // 电流元件进出
    TWE_ORGAN(ROrgans.Objects[0]).IInType  := GetCurInOut(WiringError.I1In);
    TWE_ORGAN(ROrgans.Objects[0]).IOutType := GetCurInOut(WiringError.I1Out);
    TWE_ORGAN(ROrgans.Objects[1]).IInType  := GetCurInOut(WiringError.I2In);
    TWE_ORGAN(ROrgans.Objects[1]).IOutType := GetCurInOut(WiringError.I2Out);

    // CT反接
    if WiringError.CT1Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('a', IType) > 0 then
            IReverse;
        end;
      end;
    end;

    if WiringError.CT2Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('c', IType) > 0 then
            IReverse;
        end;
      end;
    end;

    // 电流短路
    if WiringError.IaBroken or WiringError.CT1Short then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('a', IType) > 0 then
          begin
            IInType  := 'I_n';
            IOutType := 'I_n';
          end;
        end;
      end;
    end;

    if WiringError.IbBroken or WiringError.CT2Short then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('c', IType) > 0 then
          begin
            IInType  := 'I_n';
            IOutType := 'I_n';
          end;
        end;
      end;
    end;
  end;

  procedure Analysis4_NoPT_L6;
    // 找第三边
    function GetLastPhase( s: string ):string;
    begin
      if (Pos('a', s) = 0) then
        Result := 'U_a'
      else if (Pos('b', s) = 0) then
        Result := 'U_b'
      else
        Result := 'U_c'
    end;

    // 断一相
    procedure BreakOne;
      function GetStr : string;
      begin
        if (WiringError.UbBroken)  or (WiringError.UsbBroken) then
          Result := 'U_b'
        else if (WiringError.UcBroken) or (WiringError.UscBroken) then
          Result := 'U_c'
        else
          Result := 'U_a';
      end;
    var
      k : Integer;
      sPhase : string;
    begin
      for k := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[k]) do
        begin
          sPhase := GetLastPhase(UInType+UOutType);
          if sPhase = GetStr then
          begin
            UParam   := ptNot;
          end
          else
          begin
            UParam := pt1_2;

            if UInType = GetStr then
              UInType := sPhase
            else
              UOutType := sPhase;
          end;
        end;
      end;
    end;
  var
    AOrgan : TWE_ORGAN;
    j: Integer;
    
  begin
    ClearStringList(ROrgans);

    for j := 0 to 2 do
    begin
      AOrgan := TWE_ORGAN.Create;
      ROrgans.AddObject(IntToStr(j+1), AOrgan);
    end;

    {电压部分}
    // 默认
    TWE_ORGAN(ROrgans.Objects[0]).UParam   := ptNot;
    TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_b';
    TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_c';

    TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
    TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_c';
    TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_a';

    TWE_ORGAN(ROrgans.Objects[2]).UParam := ptNot;
    TWE_ORGAN(ROrgans.Objects[2]).UInType  := 'U_a';
    TWE_ORGAN(ROrgans.Objects[2]).UOutType := 'U_b';

    // 电压相序
    case WiringError.USequence of
      stABC: ;
      stACB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
//            s := UInType
            UInType  := StringReplace(UInType, 'b','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','temp', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'c','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','b', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stBAC:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'b','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','a', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'temp','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stBCA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'c','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','a', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'b','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','c', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'temp','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stCAB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'b','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'b','a', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'c','b', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','b', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stCBA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            UInType  := StringReplace(UInType, 'a','temp', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'a','temp', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'c','a', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'c','a', [rfReplaceAll]);

            UInType  := StringReplace(UInType, 'temp','c', [rfReplaceAll]);
            UOutType := StringReplace(UOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
    end;

    // ABC断一相
    if GetUBreakCount = 1 then
    begin
      BreakOne;
    end
    // ABC断两相或断三相
    else if GetUBreakCount > 1 then
    begin
      with WiringError do
      begin
        if UaBroken and UbBroken and (not UcBroken) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_c';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_c';

          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_c';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_c';

          TWE_ORGAN(ROrgans.Objects[2]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[2]).UInType  := 'U_c';
          TWE_ORGAN(ROrgans.Objects[2]).UOutType := 'U_c';
        end
        else if UaBroken and UcBroken and (not UbBroken) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_b';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_b';

          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_b';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_b';

          TWE_ORGAN(ROrgans.Objects[2]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[2]).UInType  := 'U_b';
          TWE_ORGAN(ROrgans.Objects[2]).UOutType := 'U_b';
        end
        else if UbBroken and UcBroken and (not UaBroken) then
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := 'U_a';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := 'U_a';

          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := 'U_a';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := 'U_a';

          TWE_ORGAN(ROrgans.Objects[2]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[2]).UInType  := 'U_a';
          TWE_ORGAN(ROrgans.Objects[2]).UOutType := 'U_a';
        end
        else
        begin
          TWE_ORGAN(ROrgans.Objects[0]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[0]).UInType  := '';
          TWE_ORGAN(ROrgans.Objects[0]).UOutType := '';

          TWE_ORGAN(ROrgans.Objects[1]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[1]).UInType  := '';
          TWE_ORGAN(ROrgans.Objects[1]).UOutType := '';

          TWE_ORGAN(ROrgans.Objects[2]).UParam := ptNot;
          TWE_ORGAN(ROrgans.Objects[2]).UInType  := '';
          TWE_ORGAN(ROrgans.Objects[2]).UOutType := '';
        end;
      end;
    end;

    //PT极性反
    if WiringError.PT1Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if (Pos('a',UInType) > 0) and ((Pos('-',UInType) <= 0))  then
            UInType := '-U_a';
          if (Pos('a',UOutType) > 0 ) and (Pos(' - ',UOutType) <= 0 ) then
            UOutType := '-U_a';
        end;
      end;
    end
    else
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('a',UInType) > 0   then
            UInType :=  'U_a';
          if (Pos('a',UOutType) > 0 ) then
            UOutType := 'U_a';
        end;
      end;
    end;

    if WiringError.PT2Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if (Pos('b',UInType) > 0) and (Pos('-',UInType) <= 0)   then
            UInType := '-U_b';
          if (Pos('b',UOutType) > 0 ) and (Pos('-',UOutType) <= 0) then
            UOutType := '-U_b';
        end;
      end;
    end
    else
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('b',UInType) > 0   then
            UInType :=  'U_b';
          if (Pos('b',UOutType) > 0 ) then
            UOutType := 'U_b';
        end;
      end;
    end;

    if WiringError.PT3Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if (Pos('c',UInType) > 0 ) and (Pos('-',UInType) <= 0)  then
            UInType := '-U_c';
          if (Pos('c',UOutType) > 0 ) and (Pos('-',UOutType) <= 0 ) then
            UOutType := '-U_c';
        end;
      end
    end
    else
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('c',UInType) > 0   then
            UInType := 'U_c';
          if (Pos('c',UOutType) > 0 ) then
            UOutType := 'U_c';
        end;
      end;
    end;

    {电流部分}
    // 默认
    TWE_ORGAN(ROrgans.Objects[0]).IInType  := 'I_a';
    TWE_ORGAN(ROrgans.Objects[0]).IOutType := 'I_n';
    TWE_ORGAN(ROrgans.Objects[1]).IInType  := 'I_b';
    TWE_ORGAN(ROrgans.Objects[1]).IOutType := 'I_n';
    TWE_ORGAN(ROrgans.Objects[2]).IInType  := 'I_c';
    TWE_ORGAN(ROrgans.Objects[2]).IOutType := 'I_n';

   // 电流相序
    case WiringError.ISequence of
      stACB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            IInType  := StringReplace(IInType, 'b','temp', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'b','temp', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'c','b', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'c','b', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'temp','c', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stBAC:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            IInType  := StringReplace(IInType, 'a','temp', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'a','temp', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'b','a', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'b','a', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'temp','b', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stBCA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            IInType  := StringReplace(IInType, 'a','temp', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'a','temp', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'c','a', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'c','a', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'b','c', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'b','c', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'temp','b', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'temp','b', [rfReplaceAll]);
          end;
        end;
      end;
      stCAB:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            IInType  := StringReplace(IInType, 'a','temp', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'a','temp', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'b','a', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'b','a', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'c','b', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'c','b', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'temp','c', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
      stCBA:
      begin
        for j := 0 to ROrgans.Count - 1 do
        begin
          with TWE_ORGAN(ROrgans.Objects[j]) do
          begin
            IInType  := StringReplace(IInType, 'a','temp', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'a','temp', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'c','a', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'c','a', [rfReplaceAll]);

            IInType  := StringReplace(IInType, 'temp','c', [rfReplaceAll]);
            IOutType := StringReplace(IOutType, 'temp','c', [rfReplaceAll]);
          end;
        end;
      end;
    end;

    // CT反接
    if WiringError.CT1Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('a', IType) > 0 then
            IReverse;
        end;
      end;
    end;

    if WiringError.CT2Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('b', IType) > 0 then
            IReverse;
        end;
      end;
    end;

    if WiringError.CT3Reverse then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('c', IType) > 0 then
            IReverse;
        end;
      end;
    end;

    // 电流短路
    if WiringError.IaBroken or WiringError.CT1Short then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('a', IType) > 0 then
          begin
            IInType  := 'I_n';
            IOutType := 'I_n';
          end;
        end;
      end;
    end;


    if WiringError.IbBroken or WiringError.CT2Short then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('b', IType) > 0 then
          begin
            IInType  := 'I_n';
            IOutType := 'I_n';
          end;
        end;
      end;
    end;
    if WiringError.IcBroken or WiringError.CT3Short then
    begin
      for j := 0 to ROrgans.Count - 1 do
      begin
        with TWE_ORGAN(ROrgans.Objects[j]) do
        begin
          if Pos('c', IType) > 0 then
          begin
            IInType  := 'I_n';
            IOutType := 'I_n';
          end;
        end;
      end;
    end;


  end;
begin
  if not Assigned(WiringError) then
    Exit;

  // 解析有功元件状态
  if Assigned(AOrgans) then
  begin



  end;

  // 解析无功元件状态
  if Assigned(ROrgans) then
  begin
    case ADtype of
      dt3CTClear: ;
      dt3M,
      dt3L4: Analysis3L4;
      dt4M_NoPT,
      dt4M_PT: Analysis4_NoPT_L6;
      dt4_PT_CT_CLear: ;
      dt4_PT_L6,
      dt4_NoPT_L6: Analysis4_NoPT_L6;
      dt4Direct: ;
    end;
  end;
  
end;

{ TWE_ORGAN }

constructor TWE_ORGAN.Create;
begin
  FUParam  := ptNot;
  FIParam  := ptNot;
end;

function TWE_ORGAN.GetItype: string;
begin
  if FIInType = 'I_n' then
    Result := FIOutType
  else
    Result := FIInType;
end;

function TWE_ORGAN.GetUType: string;
var
  sUOut: string;
begin
  if FUInType = 'U_n' then
    Result := '-'
  else
    Result :=  StringReplace(FUInType, '-', '', []);
  if Pos('U', FUOutType)> 0 then
    sUOut := StringReplace(FUOutType, '-', '', []);

  Result := Result + StringReplace(sUOut,'U','',[rfReplaceAll]);
end;

procedure TWE_ORGAN.IReverse;
var
  s : string;
begin
  s := IInType;
  FIInType := FIOutType;
  FIOutType := s;
end;

function TWE_ORGAN.ResetAngle(dAngle: Double): Double;
begin
  if dAngle >= 360 then
  begin
    dAngle := dAngle - 360;
    Result := ResetAngle(dAngle);
  end
  else if dAngle < 0 then
  begin
    dAngle := dAngle + 360;
    Result := ResetAngle(dAngle);
  end
  else
    Result := dAngle;
end;

procedure TWE_ORGAN.SetIAngle(const Value: Double);
begin
  FIAngle := ResetAngle(Value);
end;

procedure TWE_ORGAN.SetUAngle(const Value: Double);
begin
  FUAngle := ResetAngle(Value);
end;

{ TVECTOR_LINE }

constructor TVECTOR_LINE.Create(sLineName: string; dLineAngle: Double);
begin
  FLineName     := sLineName;
  FLineAngle    := dLineAngle;
  FIsVirtualLine:= False;
end;

{ TVECTOR_LINES }

constructor TVECTOR_LINES.Create;
  function CreateLine( sLName: string; dLineAngle: Double):TVECTOR_LINE;
  begin
    Result := TVECTOR_LINE.Create(sLName, dLineAngle);
  end;
begin
  FLines := TStringList.Create;
  FLines.AddObject('', CreateLine('U_a', 90));
  FLines.AddObject('', CreateLine('U_b', 330));
  FLines.AddObject('', CreateLine('U_c', 210));

  FLines.AddObject('', CreateLine('-U_a', 270));
  FLines.AddObject('', CreateLine('-U_b', 150));
  FLines.AddObject('', CreateLine('-U_c', 30));

  FLines.AddObject('', CreateLine('I_a', 90));
  FLines.AddObject('', CreateLine('I_b', 330));
  FLines.AddObject('', CreateLine('I_c', 210));
end;

destructor TVECTOR_LINES.Destroy;
begin
  ClearStringList(FLines);
  FLines.Free;

  inherited;
end;

function TVECTOR_LINES.GetLine(sLineName: string): TVECTOR_LINE;
var
  i: Integer;
begin
  Result := nil;
  
  for i := 0 to FLines.Count - 1 do
  begin
    if TVECTOR_LINE(FLines.Objects[i]).LineName = Trim(sLineName) then
    begin
      Result := TVECTOR_LINE(FLines.Objects[i]);
      Break;
    end;
  end;
end;

end.
