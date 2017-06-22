unit xElecLineBox;

interface

uses xElecLine, xElecPoint, System.Classes, System.SysUtils;


type
  /// <summary>
  /// 联合接线盒-电压
  /// </summary>
  TLineBoxVol = class
  private
    FIsON: Boolean;
    FInLineVol: TElecLine;
    FOutLineVol: TElecLine;
    FOnChange: TNotifyEvent;
    procedure SetIsON(const Value: Boolean);

    procedure OnOffChange(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// 进线
    /// </summary>
    property InLineVol : TElecLine read FInLineVol write FInLineVol;

    /// <summary>
    /// 出线
    /// </summary>
    property OutLineVol : TElecLine read FOutLineVol write FOutLineVol;

    /// <summary>
    /// 进线出线是否连接
    /// </summary>
    property IsON : Boolean read FIsON write SetIsON;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

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

type
  /// <summary>
  /// 联合接线盒-电流
  /// </summary>
  TLineBoxCurrent = class
  private
    FIsLine23On: Boolean;
    FIsLine12On: Boolean;
    FOutLineCurrent2: TElecLine;
    FOutLineCurrent3: TElecLine;
    FOutLineCurrent1: TElecLine;
    FInLineCurrent2: TElecLine;
    FInLineCurrent3: TElecLine;
    FInLineCurrent1: TElecLine;
    FOnChange: TNotifyEvent;
    procedure SetIsLine12On(const Value: Boolean);
    procedure SetIsLine23On(const Value: Boolean);
    procedure OnOffChange(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// 进线1
    /// </summary>
    property InLineCurrent1 : TElecLine read FInLineCurrent1 write FInLineCurrent1;

    /// <summary>
    /// 进线2
    /// </summary>
    property InLineCurrent2 : TElecLine read FInLineCurrent2 write FInLineCurrent2;

    /// <summary>
    /// 进线3
    /// </summary>
    property InLineCurrent3 : TElecLine read FInLineCurrent3 write FInLineCurrent3;

    /// <summary>
    /// 出线1
    /// </summary>
    property OutLineCurrent1 : TElecLine read FOutLineCurrent1 write FOutLineCurrent1;

    /// <summary>
    /// 出线2
    /// </summary>
    property OutLineCurrent2 : TElecLine read FOutLineCurrent2 write FOutLineCurrent2;

    /// <summary>
    /// 出线2
    /// </summary>
    property OutLineCurrent3 : TElecLine read FOutLineCurrent3 write FOutLineCurrent3;


    /// <summary>
    /// 线路1和线路2是否闭合
    /// </summary>
    property IsLine12On : Boolean read FIsLine12On write SetIsLine12On;

    /// <summary>
    /// 线路2和线路3是否闭合
    /// </summary>
    property IsLine23On : Boolean read FIsLine23On write SetIsLine23On;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
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


type
  /// <summary>
  /// 联合接线盒
  /// </summary>
  TElecLineBox = class
  private
    FLineBoxName: string;
    FBoxUA: TLineBoxVol;
    FBoxUB: TLineBoxVol;
    FBoxUC: TLineBoxVol;
    FBoxUN: TLineBoxVol;
    FBoxIA: TLineBoxCurrent;
    FBoxIB: TLineBoxCurrent;
    FBoxIC: TLineBoxCurrent;
    FOnChange: TNotifyEvent;

    procedure OnOffChange(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 联合接线盒名称
    /// </summary>
    property LineBoxName : string read FLineBoxName write FLineBoxName;

    /// <summary>
    /// A相电压
    /// </summary>
    property BoxUA : TLineBoxVol read FBoxUA write FBoxUA;

    /// <summary>
    /// B相电压
    /// </summary>
    property BoxUB : TLineBoxVol read FBoxUB write FBoxUB;

    /// <summary>
    /// C相电压
    /// </summary>
    property BoxUC : TLineBoxVol read FBoxUC write FBoxUC;

    /// <summary>
    /// N相电压
    /// </summary>
    property BoxUN : TLineBoxVol read FBoxUN write FBoxUN;

    /// <summary>
    /// A相电流
    /// </summary>
    property BoxIA : TLineBoxCurrent read FBoxIA write FBoxIA;

    /// <summary>
    /// B相电流
    /// </summary>
    property BoxIB : TLineBoxCurrent read FBoxIB write FBoxIB;

    /// <summary>
    /// C相电流
    /// </summary>
    property BoxIC : TLineBoxCurrent read FBoxIC write FBoxIC;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

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

{ TLineBoxVol }

procedure TLineBoxVol.ClearCurrentList;
begin
  FInLineVol.ClearCurrentList;
  FOutLineVol.ClearCurrentList;
end;

procedure TLineBoxVol.ClearVolVlaue;
begin

end;

procedure TLineBoxVol.ClearWValue;
begin
  FInLineVol.ClearWValue;
  FOutLineVol.ClearWValue;
end;

constructor TLineBoxVol.Create;
begin
  FInLineVol:= TElecLine.Create;
  FOutLineVol:= TElecLine.Create;
  FInLineVol.ConnPointAdd(FOutLineVol);
  FIsON:= True;
end;

destructor TLineBoxVol.Destroy;
begin
  FInLineVol.Free;
  FOutLineVol.Free;

  inherited;
end;

procedure TLineBoxVol.OnOffChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Sender);

    if FIsON then
    begin
      FInLineVol.ConnPointAdd(FOutLineVol);
      FInLineVol.SendVolValue;
    end
    else
    begin
      FInLineVol.ConnPointDel(FOutLineVol);
      FOutLineVol.Voltage.Value := 0;
      FOutLineVol.SendVolValue;
    end;

  end;
end;

procedure TLineBoxVol.SetIsON(const Value: Boolean);
begin
  if FIsON <> Value then
  begin
    FIsON := Value;
    OnOffChange(Self);
  end;

end;

{ TLineBoxCurrent }

procedure TLineBoxCurrent.ClearCurrentList;
begin
  FOutLineCurrent1.ClearCurrentList;
  FOutLineCurrent2.ClearCurrentList;
  FOutLineCurrent3.ClearCurrentList;
  FInLineCurrent1.ClearCurrentList;
  FInLineCurrent2.ClearCurrentList;
  FInLineCurrent3.ClearCurrentList;
end;

procedure TLineBoxCurrent.ClearVolVlaue;
begin

end;

procedure TLineBoxCurrent.ClearWValue;
begin
  FOutLineCurrent1.ClearWValue;
  FOutLineCurrent2.ClearWValue;
  FOutLineCurrent3.ClearWValue;
  FInLineCurrent1.ClearWValue;
  FInLineCurrent2.ClearWValue;
  FInLineCurrent3.ClearWValue;
end;

constructor TLineBoxCurrent.Create;
begin
  FOutLineCurrent1:= TElecLine.Create;
  FOutLineCurrent2:= TElecLine.Create;
  FOutLineCurrent3:= TElecLine.Create;
  FInLineCurrent1:= TElecLine.Create;
  FInLineCurrent2:= TElecLine.Create;
  FInLineCurrent3:= TElecLine.Create;

  FOutLineCurrent1.ConnPointAdd(FInLineCurrent1);
  FOutLineCurrent2.ConnPointAdd(FInLineCurrent2);
  FOutLineCurrent3.ConnPointAdd(FInLineCurrent3);


  FIsLine12On:= False;
  FIsLine23On:= True;
  FInLineCurrent2.ConnPointAdd(FOutLineCurrent3);
end;

destructor TLineBoxCurrent.Destroy;
begin
  FOutLineCurrent1.Free;
  FOutLineCurrent2.Free;
  FOutLineCurrent3.Free;
  FInLineCurrent1.Free;
  FInLineCurrent2.Free;
  FInLineCurrent3.Free;


  inherited;
end;


procedure TLineBoxCurrent.OnOffChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Sender);
  end;
end;

procedure TLineBoxCurrent.SetIsLine12On(const Value: Boolean);
begin
  if FIsLine12On <> Value then
  begin
    FIsLine12On := Value;
    OnOffChange(Self);
  end;

end;

procedure TLineBoxCurrent.SetIsLine23On(const Value: Boolean);
begin
  if FIsLine23On <> Value then
  begin
    FIsLine23On := Value;
    OnOffChange(Self);
  end;
end;

{ TElecLineBox }

procedure TElecLineBox.ClearCurrentList;
begin
  FBoxUA.ClearCurrentList;
  FBoxUB.ClearCurrentList;
  FBoxUC.ClearCurrentList;
  FBoxUN.ClearCurrentList;
  FBoxIA.ClearCurrentList;
  FBoxIB.ClearCurrentList;
  FBoxIC.ClearCurrentList;
end;

procedure TElecLineBox.ClearVolVlaue;
begin

end;

procedure TElecLineBox.ClearWValue;
begin
  FBoxUA.ClearWValue;
  FBoxUB.ClearWValue;
  FBoxUC.ClearWValue;
  FBoxUN.ClearWValue;
  FBoxIA.ClearWValue;
  FBoxIB.ClearWValue;
  FBoxIC.ClearWValue;
end;

constructor TElecLineBox.Create;
begin
  FLineBoxName:= '联合接线盒';
  FBoxUA:= TLineBoxVol.Create;
  FBoxUB:= TLineBoxVol.Create;
  FBoxUC:= TLineBoxVol.Create;
  FBoxUN:= TLineBoxVol.Create;
  FBoxIA:= TLineBoxCurrent.Create;
  FBoxIB:= TLineBoxCurrent.Create;
  FBoxIC:= TLineBoxCurrent.Create;

  FBoxUA.OnChange := OnOffChange;
  FBoxUB.OnChange := OnOffChange;
  FBoxUC.OnChange := OnOffChange;
  FBoxUN.OnChange := OnOffChange;
  FBoxIA.OnChange := OnOffChange;
  FBoxIB.OnChange := OnOffChange;
  FBoxIC.OnChange := OnOffChange;

  FBoxUA.InLineVol.LineName := '联合接线盒UA进线';
  FBoxUB.InLineVol.LineName := '联合接线盒UB进线';
  FBoxUC.InLineVol.LineName := '联合接线盒UC进线';
  FBoxUN.InLineVol.LineName := '联合接线盒UN进线';

  FBoxUA.OutLineVol.LineName := '联合接线盒UA出线';
  FBoxUB.OutLineVol.LineName := '联合接线盒UB出线';
  FBoxUC.OutLineVol.LineName := '联合接线盒UC出线';
  FBoxUN.OutLineVol.LineName := '联合接线盒UN出线';

  FBoxIA.InLineCurrent1.LineName := '联合接线盒IA进线1';
  FBoxIA.InLineCurrent2.LineName := '联合接线盒IA进线2';
  FBoxIA.InLineCurrent3.LineName := '联合接线盒IA进线3';

  FBoxIB.InLineCurrent1.LineName := '联合接线盒IB进线1';
  FBoxIB.InLineCurrent2.LineName := '联合接线盒IB进线2';
  FBoxIB.InLineCurrent3.LineName := '联合接线盒IB进线3';

  FBoxIC.InLineCurrent1.LineName := '联合接线盒IC进线1';
  FBoxIC.InLineCurrent2.LineName := '联合接线盒IC进线2';
  FBoxIC.InLineCurrent3.LineName := '联合接线盒IC进线3';

  FBoxIA.OutLineCurrent1.LineName := '联合接线盒IA出线1';
  FBoxIA.OutLineCurrent2.LineName := '联合接线盒IA出线2';
  FBoxIA.OutLineCurrent3.LineName := '联合接线盒IA出线3';

  FBoxIB.OutLineCurrent1.LineName := '联合接线盒IB出线1';
  FBoxIB.OutLineCurrent2.LineName := '联合接线盒IB出线2';
  FBoxIB.OutLineCurrent3.LineName := '联合接线盒IB出线3';

  FBoxIC.OutLineCurrent1.LineName := '联合接线盒IC出线1';
  FBoxIC.OutLineCurrent2.LineName := '联合接线盒IC出线2';
  FBoxIC.OutLineCurrent3.LineName := '联合接线盒IC出线3';

end;

destructor TElecLineBox.Destroy;
begin
  FBoxUA.Free;
  FBoxUB.Free;
  FBoxUC.Free;
  FBoxUN.Free;
  FBoxIA.Free;
  FBoxIB.Free;
  FBoxIC.Free;

  inherited;
end;

procedure TElecLineBox.OnOffChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

end.
