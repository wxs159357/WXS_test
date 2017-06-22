unit xElecBox;

interface

uses xElecLine, xElecPoint, xElecBusLine, xWiringError, xElecTV, xElecTA,
  xElecLineBox, xElecMeter
;

Type
  /// <summary>
  /// 电表箱类型
  /// </summary>
  TElecBoxType = (ebtNot,   // 未定义
                  ebtThree, // 三相三线
                  ebtFour   // 三相四线

                  );

type
  /// <summary>
  /// 计量箱
  /// </summary>
  TElecBox = class
  private
    FElecBoxName: string;
    FElecTA2: TElecTA;
    FElecTA3: TElecTA;
    FElecTA1: TElecTA;
    FElecTV2: TElecTV;
    FElecTV3: TElecTV;
    FElecTV1: TElecTV;
    FElecBoxType: TElecBoxType;
    FWiringError: TWIRINGF_ERROR;
    FElecLineBox: TElecLineBox;
    FElecBusLine: TElecBusLine;
    FElecMeter: TElecMeter;
    procedure SetElecBoxType(const Value: TElecBoxType);


    /// <summary>
    /// 需要重新计算电压电流的事件
    /// </summary>
    procedure VCChangeEvent(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;



    /// <summary>
    /// 计量箱名称
    /// </summary>
    property ElecBoxName : string read FElecBoxName write FElecBoxName;

    /// <summary>
    /// 计量箱类型
    /// </summary>
    property ElecBoxType : TElecBoxType read FElecBoxType write SetElecBoxType;

    /// <summary>
    /// 一次电缆线（母线）
    /// </summary>
    property ElecBusLine : TElecBusLine read FElecBusLine write FElecBusLine;

    /// <summary>
    /// 电压互感器1 （三线时用前两个互感器）
    /// </summary>
    property ElecTV1 : TElecTV read FElecTV1 write FElecTV1;

    /// <summary>
    /// 电压互感器2  （三线时用前两个互感器）
    /// </summary>
    property ElecTV2 : TElecTV read FElecTV2 write FElecTV2;

    /// <summary>
    /// 电压互感器3  （三线时用前两个互感器）
    /// </summary>
    property ElecTV3 : TElecTV read FElecTV3 write FElecTV3;

    /// <summary>
    /// 电流互感器1  （三线时用前两个互感器）
    /// </summary>
    property ElecTA1 : TElecTA read FElecTA1 write FElecTA1;

    /// <summary>
    /// 电流互感器2  （三线时用前两个互感器）
    /// </summary>
    property ElecTA2 : TElecTA read FElecTA2 write FElecTA2;

    /// <summary>
    /// 电流互感器3  （三线时用前两个互感器）
    /// </summary>
    property ElecTA3 : TElecTA read FElecTA3 write FElecTA3;

    /// <summary>
    /// 联合接线盒
    /// </summary>
    property ElecLineBox : TElecLineBox read FElecLineBox write FElecLineBox;

    /// <summary>
    /// 电能表
    /// </summary>
    property ElecMeter : TElecMeter read FElecMeter write FElecMeter;


  public
    /// <summary>
    /// 接线错误
    /// </summary>
    property WiringError : TWIRINGF_ERROR read FWiringError write FWiringError;

    /// <summary>
    /// 刷新值
    /// </summary>
    procedure RefurshValue;

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
  end;

implementation


{ TElecBox }

procedure TElecBox.ClearCurrentList;
begin
  FElecTA2.FirstClearCurrentList;
  FElecTA3.FirstClearCurrentList;
  FElecTA1.FirstClearCurrentList;
  FElecTA2.SecondClearCurrentList;
  FElecTA3.SecondClearCurrentList;
  FElecTA1.SecondClearCurrentList;

  FElecTV2.ClearCurrentList;
  FElecTV3.ClearCurrentList;
  FElecTV1.ClearCurrentList;
  FElecLineBox.ClearCurrentList;
  FElecBusLine.ClearCurrentList;
  FElecMeter.ClearCurrentList;
end;

procedure TElecBox.ClearVolVlaue;
begin

end;

procedure TElecBox.ClearWValue;
begin
  FElecTA2.FirstClearWValue;
  FElecTA3.FirstClearWValue;
  FElecTA1.FirstClearWValue;
  FElecTA2.SecondClearWValue;
  FElecTA3.SecondClearWValue;
  FElecTA1.SecondClearWValue;


  FElecTV2.ClearWValue;
  FElecTV3.ClearWValue;
  FElecTV1.ClearWValue;
  FElecLineBox.ClearWValue;
  FElecMeter.ClearWValue;
end;

constructor TElecBox.Create;
begin
  FElecBoxName:= '计量箱';
  FElecTA2:= TElecTA.Create;
  FElecTA3:= TElecTA.Create;
  FElecTA1:= TElecTA.Create;
  FElecTV2:= TElecTV.Create;
  FElecTV3:= TElecTV.Create;
  FElecTV1:= TElecTV.Create;
  FWiringError:= TWIRINGF_ERROR.Create;
  FElecLineBox:= TElecLineBox.Create;
  FElecBusLine:= TElecBusLine.Create;
  FElecMeter:= TElecMeter.Create;
  FElecBoxType:= ebtNot;

  FElecLineBox.OnChange := VCChangeEvent;
  FWiringError.OnChanged := VCChangeEvent;

  FElecTA2.TAName := 'TA2';
  FElecTA3.TAName := 'TA3';
  FElecTA1.TAName := 'TA1';
  FElecTV2.TVName := 'TV2';
  FElecTV3.TVName := 'TV3';
  FElecTV1.TVName := 'TV1';

  FElecTA1.SecondCurrentL.Current.WeightValue[100].WValue := 0;
  FElecTA2.SecondCurrentL.Current.WeightValue[101].WValue := 0;
  FElecTA3.SecondCurrentL.Current.WeightValue[102].WValue := 0;

  FElecTA1.SecondCurrentH.WID := 100;
  FElecTA2.SecondCurrentH.WID := 101;
  FElecTA3.SecondCurrentH.WID := 102;


  ElecBoxType:= TElecBoxType.ebtFour;

  // 刷新数据值
  RefurshValue;
end;

destructor TElecBox.Destroy;
begin
  FElecTA2.Free;
  FElecTA3.Free;
  FElecTA1.Free;
  FElecTV2.Free;
  FElecTV3.Free;
  FElecTV1.Free;
  FWiringError.Free;
  FElecLineBox.Free;
  FElecBusLine.Free;
  FElecMeter.Free;


  inherited;
end;

procedure TElecBox.RefurshValue;
begin
  FElecBusLine.RefurshValue;

  // 电流值传递（所有电流都流到电流低端）
  // 清空权值
  ClearWValue;

  // 递归所有电流低端节点
  // 赋值权值
  FElecBusLine.BusLineN.CalcCurrWValue;

  // 清空原件电流列表
  ClearCurrentList;

  // 初始化电流原件电流列表
  FElecBusLine.BusLineA.SendCurrentValue;
  FElecBusLine.BusLineB.SendCurrentValue;
  FElecBusLine.BusLineC.SendCurrentValue;

  FElecTA1.RefurshValue;
  FElecTA2.RefurshValue;
  FElecTA3.RefurshValue;

end;

procedure TElecBox.SetElecBoxType(const Value: TElecBoxType);
begin
  if FElecBoxType <> Value then
  begin
    FElecBoxType := Value;

    case FElecBoxType of
      ebtThree :
      begin

      end;
      ebtFour :
      begin
        // 电缆到电压互感器
        FElecBusLine.BusLineA.ConnPointAdd(FElecTV1.FirstVolH);
        FElecBusLine.BusLineB.ConnPointAdd(FElecTV2.FirstVolH);
        FElecBusLine.BusLineC.ConnPointAdd(FElecTV3.FirstVolH);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTV1.FirstVolL);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTV2.FirstVolL);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTV3.FirstVolL);

        // 电缆到电流互感器
        FElecBusLine.BusLineA.ConnPointAdd(FElecTA1.FirstCurrentH);
        FElecBusLine.BusLineB.ConnPointAdd(FElecTA2.FirstCurrentH);
        FElecBusLine.BusLineC.ConnPointAdd(FElecTA3.FirstCurrentH);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTA1.FirstCurrentL);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTA2.FirstCurrentL);
        FElecBusLine.BusLineN.ConnPointAdd(FElecTA3.FirstCurrentL);

        // 电压互感器到联合接线盒
        FElecTV1.SecondVolH.ConnPointAdd(FElecLineBox.BoxUA.InLineVol);
        FElecTV2.SecondVolH.ConnPointAdd(FElecLineBox.BoxUB.InLineVol);
        FElecTV3.SecondVolH.ConnPointAdd(FElecLineBox.BoxUC.InLineVol);
        FElecTV1.SecondVolL.ConnPointAdd(FElecLineBox.BoxUN.InLineVol);
        FElecTV2.SecondVolL.ConnPointAdd(FElecLineBox.BoxUN.InLineVol);
        FElecTV3.SecondVolL.ConnPointAdd(FElecLineBox.BoxUN.InLineVol);

        // 电流互感器到联合接线盒
        FElecTA1.SecondCurrentH.ConnPointAdd(FElecLineBox.BoxIA.InLineCurrent1);
        FElecTA2.SecondCurrentH.ConnPointAdd(FElecLineBox.BoxIB.InLineCurrent1);
        FElecTA3.SecondCurrentH.ConnPointAdd(FElecLineBox.BoxIC.InLineCurrent1);
        FElecTA1.SecondCurrentL.ConnPointAdd(FElecLineBox.BoxIA.InLineCurrent2);
        FElecTA2.SecondCurrentL.ConnPointAdd(FElecLineBox.BoxIB.InLineCurrent2);
        FElecTA3.SecondCurrentL.ConnPointAdd(FElecLineBox.BoxIC.InLineCurrent2);

        // 联合接线盒到电表
        FElecMeter.ElecPhase := epFour;

        FElecLineBox.BoxUA.OutLineVol.ConnPointAdd(FElecMeter.TerminalInfoByName['Ua']);
        FElecLineBox.BoxUB.OutLineVol.ConnPointAdd(FElecMeter.TerminalInfoByName['Ub']);
        FElecLineBox.BoxUC.OutLineVol.ConnPointAdd(FElecMeter.TerminalInfoByName['Uc']);
        FElecLineBox.BoxUN.OutLineVol.ConnPointAdd(FElecMeter.TerminalInfoByName['Un']);

        FElecLineBox.BoxIA.OutLineCurrent1.ConnPointAdd(FElecMeter.TerminalInfoByName['Ia+']);
        FElecLineBox.BoxIB.OutLineCurrent1.ConnPointAdd(FElecMeter.TerminalInfoByName['Ib+']);
        FElecLineBox.BoxIC.OutLineCurrent1.ConnPointAdd(FElecMeter.TerminalInfoByName['Ic+']);

        FElecLineBox.BoxIA.OutLineCurrent3.ConnPointAdd(FElecMeter.TerminalInfoByName['Ia-']);
        FElecLineBox.BoxIB.OutLineCurrent3.ConnPointAdd(FElecMeter.TerminalInfoByName['Ib-']);
        FElecLineBox.BoxIC.OutLineCurrent3.ConnPointAdd(FElecMeter.TerminalInfoByName['Ic-']);
      end;
    end;
  end;
end;

procedure TElecBox.VCChangeEvent(Sender: TObject);
begin
  RefurshValue;
end;

end.
