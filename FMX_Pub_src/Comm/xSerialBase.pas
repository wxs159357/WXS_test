{===============================================================================
  串口通讯基类

===============================================================================}
unit xSerialBase;

interface

uses xCommBase, System.Types, xTypes, System.Classes, xFunction,

{$IFDEF MSWINDOWS}
  SPComm,
{$ENDIF}
  system.SysUtils, FMX.Forms;

type
  TSerialBase = class(TCommBase)
  private
    // 串口对象
  {$IFDEF MSWINDOWS}
    FCommPort: TComm;
  {$ENDIF}
    FBaudRate: string;
    FParity: string;
    FPortSN: Byte;
    FStopBits: string;
    FPortName: string;
    FFlowControl: string;
    FDataBits: string;

    procedure SetBaudRate(const Value: string);
    procedure SetDataBits(const Value: string);
    procedure SetFlowControl(const Value: string);
    procedure SetParity(const Value: string);
    procedure SetPortName(const Value: string);
    procedure SetPortSN(const Value: Byte);
    procedure SetStopBits(const Value: string);
{$IFDEF MSWINDOWS}
    /// <summary>
    /// 串口接收
    /// </summary>
    procedure ReceiveData(Sender: TObject; Buffer: Pointer; BufferLength: Word);
{$ENDIF}
  protected

    /// <summary>
    ///真实发送 串口或以太网发送
    /// </summary>
    function RealSend(APacks: TArray<Byte>): Boolean; override;

    /// <summary>
    /// 真实连接
    /// </summary>
    function RealConnect : Boolean; override;

    /// <summary>
    /// 真实断开连接
    /// </summary>
    procedure RealDisconnect; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 串口名称
    /// </summary>
    property PortName: string read FPortName write SetPortName;

    /// <summary>
    /// 串口号
    /// </summary>
    property PortSN: Byte read FPortSN write SetPortSN;

    /// <summary>
    /// 波特率
    /// </summary>
    property BaudRate: string read FBaudRate write SetBaudRate;

    /// <summary>
    /// 数据位    5 6 7 8
    /// </summary>
    property DataBits: string read FDataBits write SetDataBits;

    /// <summary>
    /// 停止位    1 1.5 2
    /// </summary>
    property StopBits: string read FStopBits write SetStopBits;

    /// <summary>
    /// 校验位      None Odd Even Mark Space
    /// </summary>
    property Parity: string read FParity write SetParity;

    /// <summary>
    /// 数据流控制  Hardware Software None Custom
    /// </summary>
    property FlowControl : string read FFlowControl write SetFlowControl;
  end;

implementation

{ TSerialBase }

constructor TSerialBase.Create;
begin
  inherited;
//  Coinitialize(nil);
{$IFDEF MSWINDOWS}
  FCommPort:= TComm.Create(application);
  FCommPort.OnReceiveData := ReceiveData;
{$ENDIF}
  BaudRate := '9600';
  DataBits := '8';
  StopBits := '1';
  Parity := 'None';

  PortSN := 0;
end;

destructor TSerialBase.Destroy;
begin

  inherited;
end;

function TSerialBase.RealConnect: Boolean;
begin
  inherited;
  Result := False;
  try
    if (FPortSN > 0) then
    begin
    {$IFDEF MSWINDOWS}
      FCommPort.StartComm;
    {$ENDIF}
      Result := True;
    end;
  except
    CommError(PortSNToName(FPortSN) + '打开失败！');
  end;
end;

procedure TSerialBase.RealDisconnect;
begin
  inherited;
{$IFDEF MSWINDOWS}
  FCommPort.StopComm; //关闭端口
{$ENDIF}
end;

function TSerialBase.RealSend(APacks: TArray<Byte>): Boolean;
var
  sStr : string;
begin
  Result := False;
  try
    if Active then
    begin
      sStr := PacksToStr(aPacks);
    {$IFDEF MSWINDOWS}
      Result := FCommPort.WriteCommData(PAnsiChar(AnsiString(sStr)), Length(aPacks));
//      Result := FCommPort.WriteCommData(@aPacks[0], Length(aPacks));
    {$ENDIF}

    end
    else
    begin
      Result := False;
      CommError('串口未打开，发送失败！');
    end;
  except
  end;
end;
{$IFDEF MSWINDOWS}
procedure TSerialBase.ReceiveData(Sender: TObject; Buffer: Pointer;
  BufferLength: Word);
var
  sData : string;
begin
  sData := string(PAnsiChar(Buffer));
  RevStrData(sData);
  RevPacksData(StrToPacks(sData))
end;
{$ENDIF}
procedure TSerialBase.SetBaudRate(const Value: string);
var
  s : string;
  n : Integer;
begin
  s := Trim(Value);

  if (s = '110') or
    (s = '300') or
    (s = '600') or
    (s = '1200') or
    (s = '2400') or
    (s = '4800') or
    (s = '9600') or
    (s = '14400') or
    (s = '19200') or
    (s = '38400') or
    (s = '56000') or
    (s = '57600') or
    (s = '115200') or
    (s = '128000') or
    (s = '256000') then
  begin
    FBaudRate := Value;
    TryStrToInt(FBaudRate, n);
  {$IFDEF MSWINDOWS}
    FCommPort.BaudRate := n;
  {$ENDIF}
  end
  else
    CommError('设置波特率不合法！');
end;

procedure TSerialBase.SetDataBits(const Value: string);
{$IFDEF MSWINDOWS}
  function GetByteSize(sData : string) : TByteSize;
  begin
    if sData = '5' then
      Result := _5
    else if sData = '6' then
      Result := _6
    else if sData = '7' then
      Result := _7
    else
      Result := _8;
  end;
{$ENDIF}

var
  s: string;
begin
  s := Trim(Value);

  if (s = '5') or (s = '6') or (s = '7') or (s = '8') then
  begin
    FDataBits := Value;
  {$IFDEF MSWINDOWS}
    FCommPort.ByteSize := GetByteSize(FDataBits);
  {$ENDIF}
  end
  else
    CommError('设置数据位不合法！');
end;

procedure TSerialBase.SetFlowControl(const Value: string);
var
  s: string;
begin
  s := Trim(Value);

  if (s='None') or (s='Hardware') or (s='Software') or (s='Custom') then
  begin
    FFlowControl := Value;
  end
  else
    CommError('设置数据流控制不合法！');
end;

procedure TSerialBase.SetParity(const Value: string);
{$IFDEF MSWINDOWS}
  function GetParity(sParity : string) : TParity;
  begin
    if sParity = 'None' then
      Result := TParity.None
    else if sParity = 'Odd' then
      Result := Odd
    else if sParity = 'Even' then
      Result := Even
    else if sParity = 'Mark' then
      Result := Mark
    else
      Result := Space;
  end;
{$ENDIF}
var
  s: string;
begin
  s := Trim(Value);

  if (s='None') or (s='Odd') or (s='Even') or (s='Mark') or (s='Space') then
  begin
    FParity := s;
  {$IFDEF MSWINDOWS}
    FCommPort.Parity := GetParity(s);
  {$ENDIF}
  end
  else
    CommError('设置校验位不合法！');
end;

procedure TSerialBase.SetPortName(const Value: string);
begin
  if Pos('COM', Value) > 0 then
  begin
    FPortSN := PortNameToSN(Value);

    FPortName := PortSNToName(FPortSN);

  {$IFDEF MSWINDOWS}
    FCommPort.CommName := FPortName;
  {$ENDIF}
  end
  else
  begin
    CommError('串口名称不合法！');
  end;
end;

procedure TSerialBase.SetPortSN(const Value: Byte);
begin
  FPortSN := Value;
  PortName := PortSNToName(FPortSN);
end;

procedure TSerialBase.SetStopBits(const Value: string);
{$IFDEF MSWINDOWS}
  function GetStopBits(sStop : string) : TStopBits;
  begin
    if sStop = '1' then
      Result := _1
    else if sStop = '1.5' then
      Result := _1_5
    else
      Result := _2;
  end;
{$ENDIF}

var
  s: string;
begin
  s := Trim(Value);

  if (s = '1') or (s = '1.5') or (s = '2')then
  begin
    FStopBits := s;

  {$IFDEF MSWINDOWS}
    FCommPort.StopBits := GetStopBits(s);
  {$ENDIF}
  end
  else
    CommError('设置停止位不合法！');
end;

end.
