unit xElecTA;

interface

uses xElecLine, xElecPoint, System.Classes, System.SysUtils, xElecFunction ;

type
  /// <summary>
  /// 电流互感器
  /// </summary>
  TElecTA = class
  private
    FOnValueChnage: TNotifyEvent;
    FTAFirstValue: Double;
    FTASecondValue: Double;
    FTAName: string;
    FFirstCurrentL: TElecLine;
    FSecondCurrentH: TElecLine;
    FSecondCurrentL: TElecLine;
    FFirstCurrentH: TElecLine;

    procedure ValueChangeCurrent(Sender : TObject);
    procedure ValueChange(Sender : TObject);
    procedure SetTAName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 电流互感器名称
    /// </summary>
    property TAName : string read FTAName write SetTAName;

    /// <summary>
    /// 一次电压 高端
    /// </summary>
    property FirstCurrentH : TElecLine read FFirstCurrentH write FFirstCurrentH;

    /// <summary>
    /// 一次电压 低端
    /// </summary>
    property FirstCurrentL : TElecLine read FFirstCurrentL write FFirstCurrentL;

    /// <summary>
    /// 二次电压 高端
    /// </summary>
    property SecondCurrentH : TElecLine read FSecondCurrentH write FSecondCurrentH;

    /// <summary>
    /// 二次电压 低端
    /// </summary>
    property SecondCurrentL : TElecLine read FSecondCurrentL write FSecondCurrentL;

    /// <summary>
    /// TA变比一次侧值
    /// </summary>
    property TAFirstValue : Double read FTAFirstValue write FTAFirstValue;

    /// <summary>
    /// TA变比二次侧值
    /// </summary>
    property TASecondValue : Double read FTASecondValue write FTASecondValue;

    /// <summary>
    /// 值改变事件
    /// </summary>
    property OnValueChnage : TNotifyEvent read FOnValueChnage write FOnValueChnage;

  public
    /// <summary>
    /// 清空电压值
    /// </summary>
    procedure ClearVolVlaue;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure FirstClearWValue;

    /// <summary>
    /// 清空电流列表
    /// </summary>
    procedure FirstClearCurrentList;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure SecondClearWValue;

    /// <summary>
    /// 清空电流列表
    /// </summary>
    procedure SecondClearCurrentList;

    /// <summary>
    /// 刷新值
    /// </summary>
    procedure RefurshValue;
  end;

implementation

{ TElecTA }

procedure TElecTA.FirstClearCurrentList;
begin
  FFirstCurrentH.ClearCurrentList;
  FFirstCurrentL.ClearCurrentList;


end;

procedure TElecTA.ClearVolVlaue;
begin

end;

procedure TElecTA.FirstClearWValue;
begin
  FFirstCurrentH.ClearWValue;
  FFirstCurrentL.ClearWValue;


end;

procedure TElecTA.RefurshValue;
begin
  if Assigned(FFirstCurrentH) then
  begin

    GetTwoPointCurrent(FFirstCurrentH, FFirstCurrentL, FFirstCurrentH.Current);
    FSecondCurrentH.Current.Value := FFirstCurrentH.Current.Value/FTAFirstValue*FTASecondValue;
    FSecondCurrentH.Current.Angle := FFirstCurrentH.Current.Angle;


    // 递归所有电流低端节点
    // 赋值权值
    FSecondCurrentL.CalcCurrWValue;

    // 初始化电流原件电流列表
    FSecondCurrentH.SendCurrentValue;



  end;
end;

constructor TElecTA.Create;
begin
  FFirstCurrentL:= TElecLine.Create;
  FSecondCurrentH:= TElecLine.Create;
  FSecondCurrentL:= TElecLine.Create;
  FFirstCurrentH:= TElecLine.Create;

  FFirstCurrentL.LineName:= 'TA一次低端';
  FSecondCurrentH.LineName:= 'TA二次高端';
  FSecondCurrentL.LineName:= 'TA二次低端';
  FFirstCurrentH.LineName:= 'TA一次高端';

  FFirstCurrentH.ConnPointAdd(FFirstCurrentL);

  FSecondCurrentL.Current.IsLowPoint:= True;
  FSecondCurrentL.Current.IsHighPoint:= False;
  FSecondCurrentH.Current.IsLowPoint:= False;
  FSecondCurrentH.Current.IsHighPoint:= True;

  FFirstCurrentH.OnChangeCurrent := ValueChangeCurrent;
  FSecondCurrentH.OnChange := ValueChange;

  FTAFirstValue:= 100;
  FTASecondValue:= 5;
  FTAName := '电流互感器';
end;

destructor TElecTA.Destroy;
begin
  FFirstCurrentL.Free;
  FSecondCurrentH.Free;
  FSecondCurrentL.Free;
  FFirstCurrentH.Free;

  inherited;
end;

procedure TElecTA.SecondClearCurrentList;
begin
  FSecondCurrentH.ClearCurrentList;
  FSecondCurrentL.ClearCurrentList;
end;

procedure TElecTA.SecondClearWValue;
begin
  FSecondCurrentH.ClearWValue;
//  FSecondCurrentL.ClearWValue;
end;

procedure TElecTA.SetTAName(const Value: string);
begin
  FTAName := Value;

  FFirstCurrentL.LineName:= FTAName + '一次低端';
  FSecondCurrentH.LineName:= FTAName + '二次高端';
  FSecondCurrentL.LineName:= FTAName + '二次低端';
  FFirstCurrentH.LineName:= FTAName + '一次高端';
end;

procedure TElecTA.ValueChange(Sender: TObject);
begin
  if Assigned(FOnValueChnage) then
  begin
    FOnValueChnage(Self);
  end;
end;

procedure TElecTA.ValueChangeCurrent(Sender: TObject);
begin
//  if Assigned(FFirstCurrentH) then
//  begin
//    FSecondCurrentH.Current.Value := FFirstCurrentH.Current.Value/FTAFirstValue*FTASecondValue;
//    FSecondCurrentH.Current.Angle := FFirstCurrentH.Current.Angle;
//
////
////    // 递归所有电流低端节点
////    // 赋值权值
////    FSecondCurrentL.CalcCurrWValue;
//
//    // 清空原件电流列表
//    SecondClearCurrentList;
//
//    // 初始化电流原件电流列表
//    FSecondCurrentH.SendCurrentValue;
//
//
//
//  end;
end;


end.






