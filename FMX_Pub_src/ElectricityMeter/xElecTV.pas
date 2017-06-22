unit xElecTV;

interface

uses xElecLine, xElecPoint, System.Classes, System.SysUtils;

type
  /// <summary>
  /// 电压互感器
  /// </summary>
  TElecTV = class
  private
    FOnValueChnage: TNotifyEvent;
    FTVFirstValue: Double;
    FTVSecondValue: Double;
    FTVName: string;
    FFirstVolL: TElecLine;
    FFirstVolH: TElecLine;
    FSecondVolH: TElecLine;
    FSecondVolL: TElecLine;


    procedure ValueChangeVol(Sender : TObject);
    procedure ValueChange(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 电压互感器名称
    /// </summary>
    property TVName : string read FTVName write FTVName;

    /// <summary>
    /// 一次电压高端
    /// </summary>
    property FirstVolH : TElecLine read FFirstVolH write FFirstVolH;

    /// <summary>
    /// 一次电压低端
    /// </summary>
    property FirstVolL : TElecLine read FFirstVolL write FFirstVolL;

    /// <summary>
    /// 二次电压高端
    /// </summary>
    property SecondVolH : TElecLine read FSecondVolH write FSecondVolH;

    /// <summary>
    /// 二次电压低端
    /// </summary>
    property SecondVolL : TElecLine read FSecondVolL write FSecondVolL;

    /// <summary>
    /// TV变比一次侧值
    /// </summary>
    property TVFirstValue : Double read FTVFirstValue write FTVFirstValue;

    /// <summary>
    /// TV变比二次侧值
    /// </summary>
    property TVSecondValue : Double read FTVSecondValue write FTVSecondValue;

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
    procedure ClearWValue;

    /// <summary>
    /// 清空电流列表
    /// </summary>
    procedure ClearCurrentList;
  end;

implementation

{ TElecTV }

procedure TElecTV.ClearCurrentList;
begin
  FFirstVolL.ClearCurrentList;
  FFirstVolH.ClearCurrentList;
  FSecondVolH.ClearCurrentList;
  FSecondVolL.ClearCurrentList;
end;

procedure TElecTV.ClearVolVlaue;
begin

end;

procedure TElecTV.ClearWValue;
begin
  FFirstVolL.ClearWValue;
  FFirstVolH.ClearWValue;
  FSecondVolH.ClearWValue;
  FSecondVolL.ClearWValue;
end;

constructor TElecTV.Create;
begin
  FFirstVolL:= TElecLine.Create;
  FFirstVolH:= TElecLine.Create;
  FSecondVolH:= TElecLine.Create;
  FSecondVolL:= TElecLine.Create;

  FFirstVolH.OnChangeVol := ValueChangeVol;
  FSecondVolH.OnChange := ValueChange;

  FFirstVolL.LineName:= 'TV一次低端';
  FFirstVolH.LineName:= 'TV一次高端';
  FSecondVolH.LineName:= 'TV二次高端';
  FSecondVolL.LineName:= 'TV二次低端';


  FTVFirstValue:= 1;
  FTVSecondValue:= 1;
  FTVName := '电压电压互感器';
end;

destructor TElecTV.Destroy;
begin
  FFirstVolL.Free;
  FFirstVolH.Free;
  FSecondVolH.Free;
  FSecondVolL.Free;

  inherited;
end;

procedure TElecTV.ValueChange(Sender: TObject);
begin
  if Assigned(FOnValueChnage) then
  begin
    FOnValueChnage(Self);
  end;
end;

procedure TElecTV.ValueChangeVol(Sender: TObject);
begin
  if Assigned(FFirstVolH) then
  begin
    FSecondVolH.Voltage.Value := FFirstVolH.Voltage.Value/FTVFirstValue*FTVSecondValue;
    FSecondVolH.Voltage.Angle := FFirstVolH.Voltage.Angle;

    FSecondVolH.WID := FFirstVolH.WID;
    FSecondVolH.SendVolValue;

  end;
end;

end.
