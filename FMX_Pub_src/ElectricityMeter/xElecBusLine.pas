unit xElecBusLine;

interface

uses xElecLine, xElecPoint, System.Classes, System.SysUtils;

type
  /// <summary>
  /// 母线
  /// </summary>
  TElecBusLine = class
  private
    FOnValueChnage: TNotifyEvent;
    FBusLineN: TElecLine;
    FBusLineB: TElecLine;
    FBusLineC: TElecLine;
    FBusLineA: TElecLine;

    procedure ValueChange(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 母线A相
    /// </summary>
    property BusLineA : TElecLine read FBusLineA write FBusLineA;

    /// <summary>
    /// 母线B相
    /// </summary>
    property BusLineB : TElecLine read FBusLineB write FBusLineB;

    /// <summary>
    /// 母线C相
    /// </summary>
    property BusLineC : TElecLine read FBusLineC write FBusLineC;

    /// <summary>
    /// 母线N相
    /// </summary>
    property BusLineN : TElecLine read FBusLineN write FBusLineN;

    /// <summary>
    /// 值改变事件
    /// </summary>
    property OnValueChnage : TNotifyEvent read FOnValueChnage write FOnValueChnage;

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

{ TElecBusLine }

procedure TElecBusLine.ClearCurrentList;
begin
  FBusLineN.ClearCurrentList;
  FBusLineB.ClearCurrentList;
  FBusLineC.ClearCurrentList;
  FBusLineA.ClearCurrentList;
end;

procedure TElecBusLine.ClearVolVlaue;
begin

end;

procedure TElecBusLine.ClearWValue;
begin
  FBusLineN.ClearWValue;
  FBusLineB.ClearWValue;
  FBusLineC.ClearWValue;
  FBusLineA.ClearWValue;
end;

constructor TElecBusLine.Create;
begin
  FBusLineN:= TElecLine.Create;
  FBusLineB:= TElecLine.Create;
  FBusLineC:= TElecLine.Create;
  FBusLineA:= TElecLine.Create;

  FBusLineA.SetValue('BusLineA', 220, 90, 100, 70, False, True, True);
  FBusLineB.SetValue('BusLineB', 220, 330, 100, 310, False, True, True);
  FBusLineC.SetValue('BusLineC', 220, 210, 100, 190, False, True, True);
  FBusLineN.SetValue('BusLineN', 0, 0, 0, 0, True, False, False);

  FBusLineN.Current.WeightValue[100].WValue := 0;
  FBusLineN.Current.WeightValue[101].WValue := 0;
  FBusLineN.Current.WeightValue[102].WValue := 0;


  FBusLineA.WID := 100;
  FBusLineB.WID := 101;
  FBusLineC.WID := 102;

  FBusLineN.OnChange := ValueChange;
  FBusLineB.OnChange := ValueChange;
  FBusLineC.OnChange := ValueChange;
  FBusLineA.OnChange := ValueChange;


end;

destructor TElecBusLine.Destroy;
begin
  FBusLineN.Free;
  FBusLineB.Free;
  FBusLineC.Free;
  FBusLineA.Free;

  inherited;
end;

procedure TElecBusLine.RefurshValue;
begin
  FBusLineA.SendVolValue;
  FBusLineB.SendVolValue;
  FBusLineC.SendVolValue;
  FBusLineN.SendVolValue;

end;

procedure TElecBusLine.ValueChange(Sender: TObject);
begin
  if Assigned(FOnValueChnage) then
  begin
    FOnValueChnage(Self);
  end;
end;

end.
