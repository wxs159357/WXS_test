unit xElecBreak;

interface

uses xElecLine, xElecPoint, System.Classes, System.SysUtils;

type
  /// <summary>
  /// 断路器
  /// </summary>
  TElecBreak = class
  private
    FInLineN: TElecLine;
    FOutLineA: TElecLine;
    FInLineB: TElecLine;
    FInLineC: TElecLine;
    FInLineA: TElecLine;
    FOnBreakChange: TNotifyEvent;
    FIsBreakOn: Boolean;
    FOutLineN: TElecLine;
    FOutLineB: TElecLine;
    FOutLineC: TElecLine;
    FBreakName: string;
    procedure SetIsBreakOn(const Value: Boolean);

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 断路器名称
    /// </summary>
    property BreakName : string read FBreakName write FBreakName;

    /// <summary>
    /// A相进线
    /// </summary>
    property InLineA : TElecLine read FInLineA write FInLineA;

    /// <summary>
    /// B相进线
    /// </summary>
    property InLineB : TElecLine read FInLineB write FInLineB;

    /// <summary>
    /// C相进线
    /// </summary>
    property InLineC : TElecLine read FInLineC write FInLineC;

    /// <summary>
    /// N相进线
    /// </summary>
    property InLineN : TElecLine read FInLineN write FInLineN;

    /// <summary>
    /// A相进线
    /// </summary>
    property OutLineA : TElecLine read FOutLineA write FOutLineA;

    /// <summary>
    /// B相进线
    /// </summary>
    property OutLineB : TElecLine read FOutLineB write FOutLineB;

    /// <summary>
    /// C相进线
    /// </summary>
    property OutLineC : TElecLine read FOutLineC write FOutLineC;

    /// <summary>
    /// N相进线
    /// </summary>
    property OutLineN : TElecLine read FOutLineN write FOutLineN;

    /// <summary>
    /// 是否闭合
    /// </summary>
    property IsBreakOn : Boolean read FIsBreakOn write SetIsBreakOn;

    /// <summary>
    /// 开关状态改变
    /// </summary>
    property OnBreakChange : TNotifyEvent read FOnBreakChange write FOnBreakChange;

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

{ TElecBreak }

procedure TElecBreak.ClearCurrentList;
begin
  FInLineA.ClearCurrentList;
  FInLineB.ClearCurrentList;
  FInLineC.ClearCurrentList;
  FInLineN.ClearCurrentList;
  FOutLineA.ClearCurrentList;
  FOutLineB.ClearCurrentList;
  FOutLineC.ClearCurrentList;
  FOutLineN.ClearCurrentList;
end;

procedure TElecBreak.ClearVolVlaue;
begin

end;

procedure TElecBreak.ClearWValue;
begin
  FInLineA.ClearWValue;
  FInLineB.ClearWValue;
  FInLineC.ClearWValue;
  FInLineN.ClearWValue;
  FOutLineA.ClearWValue;
  FOutLineB.ClearWValue;
  FOutLineC.ClearWValue;
  FOutLineN.ClearWValue;
end;

constructor TElecBreak.Create;
begin
  FInLineA  := TElecLine.Create;
  FInLineB  := TElecLine.Create;
  FInLineC  := TElecLine.Create;
  FInLineN  := TElecLine.Create;
  FOutLineA := TElecLine.Create;
  FOutLineB := TElecLine.Create;
  FOutLineC := TElecLine.Create;
  FOutLineN := TElecLine.Create;
  FIsBreakOn:= True;
  FBreakName:= '断路器';
end;

destructor TElecBreak.Destroy;
begin
  FInLineA.Free;
  FInLineB.Free;
  FInLineC.Free;
  FInLineN.Free;
  FOutLineA.Free;
  FOutLineB.Free;
  FOutLineC.Free;
  FOutLineN.Free;

  inherited;
end;

procedure TElecBreak.SetIsBreakOn(const Value: Boolean);
begin
  FIsBreakOn := Value;
end;

end.
