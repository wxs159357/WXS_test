unit xProtocolDL645;

interface

uses xProtocolBase, xDL645Type, xProtocolPacksDL645_07, xProtocolPacksDL645_97,
  system.SysUtils, xProtocolPacksDl645, System.Classes;

type
  TProtocolDL645 = class(TProtocolBase)
  private
    FProrocolType: TDL645_PROTOCOL_TYPE;
    FPack97 : TProtocolPacksDL645_97;
    fPack07 : TProtocolPacksDL645_07;
    FOnRev645Data: TGet645Data;

    function GetDL645Pack : TProtocolPacksDl645;

    /// <summary>
    /// 接收数据
    /// </summary>
    procedure Rve645Data( A645data : TStringList );

  protected
    /// <summary>
    /// 生成数据包
    /// </summary>
    function CreatePacks: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    /// 协议类型
    /// </summary>
    property ProrocolType : TDL645_PROTOCOL_TYPE read FProrocolType write FProrocolType;

    /// <summary>
    /// 接收数据包
    /// </summary>
    procedure ReceivedData(aPacks: TBytes; sParam1, sParam2 : string); override;

    /// <summary>
    /// 接收数据事件
    /// </summary>
    property OnRev645Data : TGet645Data read FOnRev645Data write FOnRev645Data;

  end;

implementation

{ TProtocolDL645 }

constructor TProtocolDL645.Create;
begin
  inherited;
  FPack97 := TProtocolPacksDL645_97.Create;
  fPack07 := TProtocolPacksDL645_07.Create;

  FPack97.OnRev645Data := Rve645Data;
  fPack07.OnRev645Data := Rve645Data;

end;

function TProtocolDL645.CreatePacks: TBytes;
begin
  if Assigned(FDev) then
  begin
    if FDev is TDL645_DATA then
    begin
      case TDL645_DATA(FDev).MeterProtocolType of
        dl645pt1997 :
        begin
          Result := FPack97.GetPacks(FOrderType, FDev);
        end;
        dl645pt2007:
        begin
          Result := FPack07.GetPacks(FOrderType, FDev);
        end
        else
          Result := nil;
      end;
    end
    else
    begin
      Result := nil;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

destructor TProtocolDL645.Destroy;
begin

  inherited;
end;

function TProtocolDL645.GetDL645Pack: TProtocolPacksDl645;
begin
  if FProrocolType = dl645pt2007 then
  begin
    Result := fPack07;
  end
  else if FProrocolType = dl645pt1997 then
  begin
    Result := FPack97;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TProtocolDL645.ReceivedData(aPacks: TBytes; sParam1, sParam2: string);
var
  APack : TProtocolPacksDl645;
begin
  inherited;
  APack := GetDL645Pack;

  if Assigned(APack) then
  begin

    APack.RevPacks( FOrderType, FDev, aPacks);

  end;
end;

procedure TProtocolDL645.Rve645Data(A645data: TStringList);
begin
  if Assigned (FOnRev645Data) then
  begin
    FOnRev645Data(A645data);
  end;
end;

end.

