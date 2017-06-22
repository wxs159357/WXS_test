unit xElecOrgan;

interface

uses SysUtils, Classes, xElecLine, xElecFunction, Math, xElecPoint;

type
  /// <summary>
  /// 电表计量元件类
  /// </summary>
  TElecOrgan = class
  private
    FOrganName: string;
    FVolPointOut: TElecLine;
    FVolPointIn: TElecLine;
    FVolOrgan : TElecPoint;
    FOnChange: TNotifyEvent;
    FCurrentPointOut: TElecLine;
    FCurrentPointIn: TElecLine;
    FCurrentPoint: TElecPoint;
    FIsReactive: Boolean;

    /// <summary>
    /// 电压或电流值改变
    /// </summary>
    procedure ValueChange(Sender: TObject);
    function GetActivePower: Double;
    function GetReactivePower: Double;
    function GetPowerFactor: Double;
    function GetPositiveActivePower: Double;
    function GetReverseActivePower: Double;
    function GetPositiveReactivePower: Double;
    function GetReverseReactivePower: Double;
    function GetQuadrantReactivePower(nSN: Integer): Double;
    function GetAngle: Double;
    function GetIsIBreak: Boolean;
    function GetIsIReverse: Boolean;
    function GetIsUBreak: Boolean;
    function GetVolOrgan: TElecPoint;
    procedure SetOrganName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 元件名称
    /// </summary>
    property OrganName : string read FOrganName write SetOrganName;

    /// <summary>
    /// 电压线进
    /// </summary>
    property VolPointIn : TElecLine read FVolPointIn write FVolPointIn;

    /// <summary>
    /// 电压线出
    /// </summary>
    property VolPointOut : TElecLine read FVolPointOut write FVolPointOut;

    /// <summary>
    /// 元件电压合向量
    /// </summary>
    property VolOrgan : TElecPoint read GetVolOrgan;

    /// <summary>
    ///  电流进线
    /// </summary>
    property CurrentPointIn : TElecLine read FCurrentPointIn write FCurrentPointIn;

    /// <summary>
    ///  电流出线
    /// </summary>
    property CurrentPointOut : TElecLine read FCurrentPointOut write FCurrentPointOut;

    /// <summary>
    /// 获取原件电流值
    /// </summary>
    function GetOrganCurrent: TElecPoint;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure ClearWValue;

    /// <summary>
    /// 清空电压值
    /// </summary>
    procedure ClearVolVlaue;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 相角
    /// </summary>
    property Angle : Double read GetAngle;

    /// <summary>
    /// 是否是无功表 无功表需要把电压合向量移动60度
    /// </summary>
    property IsReactive : Boolean read FIsReactive write FIsReactive;

    {电能相关}
    /// <summary>
    /// 有功功率
    /// </summary>
    property ActivePower : Double read GetActivePower;

    /// <summary>
    /// 无功功率
    /// </summary>
    property ReactivePower : Double read GetReactivePower;

    /// <summary>
    /// 正向有功功率
    /// </summary>
    property PositiveActivePower : Double read GetPositiveActivePower;

    /// <summary>
    /// 反向有功功率
    /// </summary>
    property ReverseActivePower : Double read GetReverseActivePower;

    /// <summary>
    /// 正向无功功率
    /// </summary>
    property PositiveReactivePower : Double read GetPositiveReactivePower;

    /// <summary>
    /// 反向无功功率
    /// </summary>
    property ReverseReactivePower : Double read GetReverseReactivePower;

    /// <summary>
    /// 四象限无功功率
    /// </summary>
    property QuadrantReactivePower[nSN : Integer] : Double read GetQuadrantReactivePower;

    /// <summary>
    /// 功率因数
    /// </summary>
    property PowerFactor : Double read GetPowerFactor;

    {故障相关}

    /// <summary>
    /// U失压
    /// </summary>
    property IsUBreak : Boolean read GetIsUBreak;

    /// <summary>
    /// I失流
    /// </summary>
    property IsIBreak : Boolean read GetIsIBreak;

    /// <summary>
    /// I反向
    /// </summary>
    property IsIReverse : Boolean read GetIsIReverse;
  end;

implementation

{ TElecOrgan }

procedure TElecOrgan.ClearVolVlaue;
begin
  FVolPointIn.Voltage.ClearValue;
  FVolPointOut.Voltage.ClearValue;
end;

procedure TElecOrgan.ClearWValue;
begin
  FCurrentPointOut.ClearWValue;
  FCurrentPointIn.ClearWValue;
end;

constructor TElecOrgan.Create;
begin
  FVolPointIn:=    TElecLine.Create;
  FVolPointOut:=    TElecLine.Create;
  FVolOrgan:=     TElecPoint.Create;
  FCurrentPointOut:=     TElecLine.Create;
  FCurrentPointIn:=     TElecLine.Create;

  FCurrentPoint:= TElecPoint.Create;

  FVolPointIn.Onwner := Self;
  FVolPointOut.Onwner := Self;
  FCurrentPointIn.Onwner := Self;
  FCurrentPointOut.Onwner := Self;

  FIsReactive := False;

  FCurrentPointOut.ConnPointAdd(FCurrentPointIn);



  FOrganName := '默认元件名';

  FVolPointIn.OnChange    := ValueChange;
  FVolPointOut.OnChange    := ValueChange;
  FCurrentPointIn.OnChange    := ValueChange;
  FCurrentPointOut.OnChange    := ValueChange;
end;

destructor TElecOrgan.Destroy;
begin
  FVolPointOut.Free;
  FVolPointIn.Free;
  FVolOrgan.Free;
  FCurrentPointOut.Free;
  FCurrentPointIn.Free;
  FCurrentPoint.Free;

  inherited;
end;

function TElecOrgan.GetActivePower: Double;
begin
  Result := VolOrgan.Value*GetOrganCurrent.Value* PowerFactor;
end;

function TElecOrgan.GetAngle: Double;
begin
  result := AdjustAngle(VolOrgan.Angle - GetOrganCurrent.Angle);
end;

function TElecOrgan.GetIsIBreak: Boolean;
begin
  Result := Abs(GetOrganCurrent.Value) < 0.1;
end;

function TElecOrgan.GetIsIReverse: Boolean;
begin
  Result := GetOrganCurrent.Value < -0.1;
end;

function TElecOrgan.GetIsUBreak: Boolean;
begin
  Result := VolOrgan.Value < 30;
end;

function TElecOrgan.GetOrganCurrent: TElecPoint;
begin
  GetTwoPointCurrent(FCurrentPointIn, FCurrentPointOut, FCurrentPoint);
  Result := FCurrentPoint;
end;

function TElecOrgan.GetPositiveActivePower: Double;
var
  dValue : Double;
begin
  dValue := Angle;

  case GetQuadrantSN(dValue) of
    1 :
    begin
      Result := GetActivePower;
    end;
    2 :
    begin
      Result := 0;
    end;
    3 :
    begin
      Result := 0;
    end;
  else
    begin
      Result := GetActivePower;
    end;
  end;

  Result := Abs(Result);
end;

function TElecOrgan.GetPositiveReactivePower: Double;
var
  dValue : Double;
begin
  dValue := Angle;

  case GetQuadrantSN(dValue) of
    1 :
    begin
      Result := GetReactivePower;
    end;
    2 :
    begin
      Result := GetReactivePower;
    end;
    3 :
    begin
      Result := 0;
    end;
  else
    begin
      Result := 0;
    end;
  end;

  Result := Abs(Result);
end;

function TElecOrgan.GetPowerFactor: Double;
begin
//  //  Cosφ=1/(1+(无功力调电量/有功力调电量)^2)^0.5
//  Result := 1 / sqrt( 1 + sqr( ReactivePower / ActivePower ));
  Result := Cos(DegToRad(Angle))
end;

function TElecOrgan.GetQuadrantReactivePower(nSN: Integer): Double;
var
  dValue : Double;
begin
  dValue := Angle;

  if GetQuadrantSN(dValue) = nSN then
    Result := GetReactivePower
  else
    Result := 0;

  Result := Abs(Result);
end;

function TElecOrgan.GetReactivePower: Double;
begin
  Result := VolOrgan.Value*GetOrganCurrent.Value*
    Sin(DegToRad(Angle))
end;

function TElecOrgan.GetReverseActivePower: Double;
var
  dValue : Double;
begin
  dValue := Angle;

  case GetQuadrantSN(dValue) of
    1 :
    begin
      Result := 0;
    end;
    2 :
    begin
      Result := GetActivePower;
    end;
    3 :
    begin
      Result := GetActivePower;
    end;
  else
    begin
      Result := 0;
    end;
  end;
  Result := Abs(Result);
end;

function TElecOrgan.GetReverseReactivePower: Double;
var
  dValue : Double;
begin
  dValue := Angle;

  case GetQuadrantSN(dValue) of
    1 :
    begin
      Result := 0;
    end;
    2 :
    begin
      Result := 0;
    end;
    3 :
    begin
      Result := GetReactivePower;
    end;
  else
    begin
      Result := GetReactivePower;
    end;
  end;

  Result := Abs(Result);
end;

function TElecOrgan.GetVolOrgan: TElecPoint;
begin
  // 计算电压 和向量
  GetOtherValue(FVolPointIn.Voltage, FVolPointOut.Voltage, FVolOrgan);

  // 六十度无功需要移动60度
  if FIsReactive then
    FVolOrgan.Angle := AdjustAngle(FVolOrgan.Angle + 30);

  Result := FVolOrgan;
end;

procedure TElecOrgan.SetOrganName(const Value: string);
begin
  FOrganName := Value;

  FVolPointIn.LineName := FOrganName + 'VolIn';
  FVolPointOut.LineName := FOrganName + 'VolOut';

  FCurrentPointIn.LineName := FOrganName + 'CurrentIn';
  FCurrentPointOut.LineName := FOrganName + 'CurrentOut';
end;

//procedure TElecOrgan.SetIsIBreak(const Value: Boolean);
//begin
//  if Value then
//  begin
//    if FCurrentPointIn.CurrentReal.Current.Value <> 0 then
//      FCurTemp := FCurrentPointIn.CurrentReal.Current.Value;
//
//    FCurrentPointIn.CurrentReal.Current.Value := 0;
//  end
//  else
//  begin
//    if FCurTemp <> 0 then
//      FCurrentPointIn.CurrentReal.Current.Value := FCurTemp;
//  end;
//end;

//procedure TElecOrgan.SetIsIReverse(const Value: Boolean);
//begin
//  if Value then
//  begin
//    CurrentOrgan.Current.Value := -abs(FCurrentPointIn.CurrentReal.Current.Value);
//  end
//  else
//  begin
//    CurrentOrgan.Current.Value := abs(FCurrentPointIn.CurrentReal.Current.Value);
//  end;
//
//end;

//procedure TElecOrgan.SetIsUBreak(const Value: Boolean);
//begin
//  if Value then
//  begin
//    if FVolPointIn.Voltage.Value <> 0 then
//      FVolTemp := FVolPointIn.Voltage.Value;
//    FVolPointIn.Voltage.Value := 0;
//
//  end
//  else
//  begin
//    if FVolTemp <> 0 then
//      FVolPointIn.Voltage.Value := FVolTemp;
//  end;
//end;

procedure TElecOrgan.ValueChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.





