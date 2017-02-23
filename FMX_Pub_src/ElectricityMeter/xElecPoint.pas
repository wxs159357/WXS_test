unit xElecPoint;

interface

uses SysUtils, Classes;

const
  /// <summary>
  /// 无线权值
  /// </summary>
  C_WEIGHT_VALUE_INVALID = 9999;

type
  /// <summary>
  /// 权值类
  /// </summary>
  TWeightValue = class
  private
    FWID: Integer;
    FWValue: Integer;

  public
    constructor Create;

    /// <summary>
    /// 权值大小（相同ID，权值大的往权值小的地方流）
    /// </summary>
    property WValue : Integer read FWValue write FWValue;

    /// <summary>
    /// 权值ID （只有相同权值才有比较权值大小的意义）
    /// </summary>
    property WID : Integer read FWID write FWID;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure ClearWValue;

  end;

type
  /// <summary>
  /// 电的点对象
  /// </summary>
  TElecPoint = class
  private
    FOwner: TObject;
    FAngle: Double;
    FValue: Double;
    FPointName: string;
    FPointSN: string;
    FOnChange: TNotifyEvent;
    FIsLowPoint: Boolean;
    FIsVolRoot: Boolean;
    FIsHighPoint: Boolean;
    FWValueList: TStringList;
    FIsClear: Boolean;
    procedure SetValue(const Value: Double);
    procedure SetAngle(const Value: Double);

    procedure Changed;
    function GetTWeightValue(nWID : Integer): TWeightValue;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 所有者
    /// </summary>
    property Owner  : TObject read FOwner write FOwner;

    /// <summary>
    /// 点编号
    /// </summary>
    property PointSN : string read FPointSN write FPointSN;

    /// <summary>
    /// 向量点名称
    /// </summary>
    property PointName : string read FPointName write FPointName;

    /// <summary>
    /// 角度   水平向右为0度 竖直向上为90度
    /// </summary>
    property Angle : Double read FAngle write SetAngle;

    /// <summary>
    /// 向量值
    /// </summary>
    property Value : Double read FValue write SetValue;

    /// <summary>
    /// 清空值
    /// </summary>
    procedure ClearValue;

    /// <summary>
    /// 清除权值
    /// </summary>
    procedure ClearWValue;

    /// <summary>
    /// 权值列表
    /// </summary>
    property WValueList : TStringList read FWValueList write FWValueList;

    /// <summary>
    /// 根据权值ID获取权值对象（没有回自动创建）
    /// </summary>
    property WeightValue[nWID:Integer] : TWeightValue read GetTWeightValue;

    /// <summary>
    /// 是否是电流的低端或者是地线
    /// </summary>
    property IsLowPoint : Boolean read FIsLowPoint write FIsLowPoint;

    /// <summary>
    /// 是否是电流高端(在权值大于零的情况可以传递电流值)
    /// </summary>
    property IsHighPoint : Boolean read FIsHighPoint write FIsHighPoint;

    /// <summary>
    /// 是否是电压源
    /// </summary>
    property IsVolRoot : Boolean read FIsVolRoot write FIsVolRoot;

    /// <summary>
    /// 是否清空值
    /// </summary>
    property IsClear : Boolean read FIsClear write FIsClear;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 值赋值
    /// </summary>
    procedure AssignValue(APoint : TElecPoint);
  end;

implementation

{ TElecPoint }

procedure TElecPoint.AssignValue(APoint: TElecPoint);
begin
  if Assigned(APoint) then
  begin
    Angle := APoint.Angle;
    Value := APoint.Value;
  end;
end;

procedure TElecPoint.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TElecPoint.ClearValue;
begin
  FAngle := 0;
  FValue := 0;
  FIsClear := True;
end;

procedure TElecPoint.ClearWValue;
var
  i : Integer;
begin
  for i := 0 to FWValueList.Count - 1 do
  begin
    FWValueList.Objects[i].Free;
  end;

  FWValueList.Clear;
end;

constructor TElecPoint.Create;
begin
  FWValueList := TStringList.Create;
  FAngle := 0;
  FValue := 0;
  FPointName:= '';
  FPointSN:= '';
  FIsLowPoint:= False;
  FIsVolRoot := False;
  FIsHighPoint := False;
  FIsClear := True;
end;

destructor TElecPoint.Destroy;
var
  i : Integer;
begin
  for i := 0 to FWValueList.Count - 1 do
    FWValueList.Objects[i].Free;

  FWValueList.Clear;
  FWValueList.Free;

  inherited;
end;

function TElecPoint.GetTWeightValue(nWID: Integer): TWeightValue;
var
  nIndex : Integer;
begin
  nIndex := FWValueList.IndexOf(IntToStr(nWID));

  if nIndex = -1 then
  begin
    Result := TWeightValue.Create;
    Result.WID := nWID;
    FWValueList.AddObject(IntToStr(nWID), Result);
  end
  else
  begin
    Result := TWeightValue(FWValueList.Objects[nIndex]);
  end;
end;

procedure TElecPoint.SetAngle(const Value: Double);
begin
  if Abs(FAngle - Value) > 0.001 then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TElecPoint.SetValue(const Value: Double);
begin
  if Abs(FValue - Value) > 0.001 then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TWeightValue }

procedure TWeightValue.ClearWValue;
begin
  FWID    := C_WEIGHT_VALUE_INVALID;
  FWValue := C_WEIGHT_VALUE_INVALID;
end;

constructor TWeightValue.Create;
begin
  ClearWValue;
end;

end.

