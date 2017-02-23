unit xElecPower;

interface

uses xElecLine, xElecPoint;

type
  TElecPower = class
  private
    FVolA: TElecLine;
    FVolB: TElecLine;
    FVolC: TElecLine;
    FVolN: TElecLine;
    FCurrAIn: TElecLine;
    FCurrAOut: TElecLine;
    FCurrBIn: TElecLine;
    FCurrBOut: TElecLine;
    FCurrCIn: TElecLine;
    FCurrCOut: TElecLine;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 电压A
    /// </summary>
    property VolA : TElecLine read FVolA write FVolA;

    /// <summary>
    /// 电压B
    /// </summary>
    property VolB : TElecLine read FVolB write FVolB;

    /// <summary>
    /// 电压C
    /// </summary>
    property VolC : TElecLine read FVolC write FVolC;

    /// <summary>
    /// 电压N
    /// </summary>
    property VolN : TElecLine read FVolN write FVolN;

    /// <summary>
    /// 电流A进
    /// </summary>
    property CurrAIn : TElecLine read FCurrAIn write FCurrAIn;

    /// <summary>
    /// 电流A出
    /// </summary>
    property CurrAOut : TElecLine read FCurrAOut write FCurrAOut;

    /// <summary>
    /// 电流B进
    /// </summary>
    property CurrBIn : TElecLine read FCurrBIn write FCurrBIn;

    /// <summary>
    /// 电流B出
    /// </summary>
    property CurrBOut : TElecLine read FCurrBOut write FCurrBOut;

    /// <summary>
    /// 电流C进
    /// </summary>
    property CurrCIn : TElecLine read FCurrCIn write FCurrCIn;

    /// <summary>
    /// 电流C出
    /// </summary>
    property CurrCOut : TElecLine read FCurrCOut write FCurrCOut;

    /// <summary>
    /// 初始化电源(三相四线)
    /// </summary>
    procedure INIPower4;

    /// <summary>
    /// 初始化电源(三相三线)
    /// </summary>
    procedure INIPower3;
  end;

implementation

{ TElecPower }

constructor TElecPower.Create;
begin
  FVolA:= TElecLine.Create;
  FVolB:= TElecLine.Create;
  FVolC:= TElecLine.Create;
  FVolN:= TElecLine.Create;
  FCurrAIn:= TElecLine.Create;
  FCurrAOut:= TElecLine.Create;
  FCurrBIn:= TElecLine.Create;
  FCurrBOut:= TElecLine.Create;
  FCurrCIn:= TElecLine.Create;
  FCurrCOut:= TElecLine.Create;
end;

destructor TElecPower.Destroy;
begin
  FVolA.Free;
  FVolB.Free;
  FVolC.Free;
  FVolN.Free;
  FCurrAIn.Free;
  FCurrAOut.Free;
  FCurrBIn.Free;
  FCurrBOut.Free;
  FCurrCIn.Free;
  FCurrCOut.Free;

  inherited;
end;

procedure TElecPower.INIPower3;
  procedure SetValue(AElecLine: TElecLine; sLineName: string; dVolValue, dVolAngle, dCurrValue,
    dCurrAngle : Double; bIsLowPoint, bIsHighPoint, bVolRoot : Boolean);
  begin
    AElecLine.LineName := sLineName;
    AElecLine.Voltage.Value := dVolValue;
    AElecLine.Voltage.Angle := dVolAngle;
    AElecLine.Current.Value := dCurrValue;
    AElecLine.Current.Angle := dCurrAngle;
    AElecLine.Current.IsLowPoint := bIsLowPoint;
    AElecLine.Current.IsHighPoint := bIsHighPoint;
    AElecLine.Current.ClearWValue;
    AElecLine.Voltage.ClearWValue;
    AElecLine.Voltage.IsVolRoot := bVolRoot;
  end;
begin
  SetValue(FVolA, 'PowerUa', 57.7, 90, 0, 0, False, False, True);
  SetValue(FVolB, 'PowerUb', 57.7, 330, 0, 0, False, False, True);
  SetValue(FVolC, 'PowerUc', 57.7, 210, 0, 0, False, False, True);
  SetValue(FVolN, 'PowerUn', 0, 0, 0, 0, False, False, False);

  SetValue(FCurrAIn, 'PowerIa+', 0, 0, 5, 70, False, True, False);
  SetValue(FCurrAOut, 'PowerIa-', 0, 0, 0, 0, True, False, False);
  SetValue(FCurrBIn, 'PowerIb+', 0, 0, 0, 0, False, True, False);
  SetValue(FCurrBOut, 'PowerIb-', 0, 0, 0, 0, True, False, False);
  SetValue(FCurrCIn, 'PowerIc+', 0, 0, 5, 190, False, True, False);
  SetValue(FCurrCOut, 'PowerIc-', 0, 0, 0, 0, True, False, False);

  FCurrAOut.Current.WeightValue[100].WValue := 0;
  FCurrBOut.Current.WeightValue[101].WValue := 0;
  FCurrCOut.Current.WeightValue[102].WValue := 0;
  FCurrAIn.WID := 100;
  FCurrBIn.WID := 101;
  FCurrCIn.WID := 102;
end;

procedure TElecPower.INIPower4;
  procedure SetValue(AElecLine: TElecLine; sLineName: string; dVolValue, dVolAngle, dCurrValue,
    dCurrAngle : Double; bIsLowPoint, bIsHighPoint, bVolRoot : Boolean);
  begin
    AElecLine.LineName := sLineName;
    AElecLine.Voltage.Value := dVolValue;
    AElecLine.Voltage.Angle := dVolAngle;
    AElecLine.Current.Value := dCurrValue;
    AElecLine.Current.Angle := dCurrAngle;
    AElecLine.Current.IsLowPoint := bIsLowPoint;
    AElecLine.Current.IsHighPoint := bIsHighPoint;
    AElecLine.Current.ClearWValue;
    AElecLine.Voltage.ClearWValue;
    AElecLine.Voltage.IsVolRoot := bVolRoot;
  end;
begin
  SetValue(FVolA, 'PowerUa', 220, 90, 0, 0, False, False, True);
  SetValue(FVolB, 'PowerUb', 220, 330, 0, 0, False, False, True);
  SetValue(FVolC, 'PowerUc', 220, 210, 0, 0, False, False, True);
  SetValue(FVolN, 'PowerUn', 0, 0, 0, 0, False, False, False);

  SetValue(FCurrAIn, 'PowerIa+', 0, 0, 5, 70, False, True, False);
  SetValue(FCurrAOut, 'PowerIa-', 0, 0, 0, 0, True, False, False);
  SetValue(FCurrBIn, 'PowerIb+', 0, 0, 5, 310, False, True, False);
  SetValue(FCurrBOut, 'PowerIb-', 0, 0, 0, 0, True, False, False);
  SetValue(FCurrCIn, 'PowerIc+', 0, 0, 5, 190, False, True, False);
  SetValue(FCurrCOut, 'PowerIc-', 0, 0, 0, 0, True, False, False);

  FCurrAOut.Current.WeightValue[100].WValue := 0;
  FCurrBOut.Current.WeightValue[101].WValue := 0;
  FCurrCOut.Current.WeightValue[102].WValue := 0;
  FCurrAIn.WID := 100;
  FCurrBIn.WID := 101;
  FCurrCIn.WID := 102;
end;

end.
