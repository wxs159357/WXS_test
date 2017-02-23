unit xProtocolPacks;

interface

uses System.Types, xTypes, xConsts, System.Classes, xFunction,
  system.SysUtils;

type
  /// <summary>
  /// 通讯协议数据包操作类
  /// </summary>
  TProtocolPacks = class
  private

  protected

    {**************************生成发送数据包******************************}
    function PackNoData: TBytes;                           // 生成没有数据命令包

//    /// <summary>
//    /// 生成数据包
//    /// </summary>
//    function CreatePacks: TBytes;

    function CreatePackRsetTime: TBytes; virtual;                //  广播校时
    function CreatePackReadData: TBytes; virtual;                //  读数据
    function CreatePackReadNextData: TBytes; virtual;abstract;   //  读后续数据
    function CreatePackReadAddr: TBytes; virtual;abstract;       //  读通信地址
    function CreatePackWriteData: TBytes;virtual;abstract;       //  写数据
    function CreatePackWriteAddr: TBytes;virtual;                //  写通信地址
    function CreatePackFreeze: TBytes; virtual;                  //  冻结命令
    function CreatePackChangeBaudRate: TBytes; virtual;          //  改波通讯速率
    function CreatePackChangePWD: TBytes;virtual;abstract;       //  改密码
    function CreatePackClearMaxDemand: TBytes;virtual;abstract;  //  最大需量清零
    function CreatePackClearData: TBytes;virtual;                //  电表清零
    function CreatePackClearEvent: TBytes;virtual;               //  事件清零
    function CreatePackSetWOutType: TBytes;virtual;abstract;     //  设置多功能口
    function CreatePackIdentity: TBytes;virtual;abstract;         //  费控
    function CreatePackOnOffControl: TBytes;virtual;abstract;         //  费控



    {**************************生成应答数据包******************************}
//    {S25-校验仪}
//    function ReplyPackS25_CALIBRATOR_GET_TIME   : TBytes; //获取时间









    {**************************解析数据包******************************}

//    procedure ParseVersion(ADevice: TObject; APack : TBytes);    // 解析版本


    {功率查询板}
//    procedure ParsePackSTATE_GET_VALUE(APowerStatus : TPOWER_STATUS; APack : TBytes);//解析获取电压电流角度测量值











//    /// <summary>
//    /// 刷新包头包尾
//    /// </summary>
//    procedure RefreshHeadEnd(nCmdType: Integer);

    /// <summary>
    /// 解析数据包
    /// </summary>
    procedure ParsePack(nCmdType: Integer; ADevice: TObject; APack : TBytes); virtual;


//    /// <summary>
//    /// 获取通讯地址
//    /// </summary>
//    function GetAddr(nCmdType: Integer; ADevice: TObject) : Byte;

    /// <summary>
    /// 生成数据包
    /// </summary>
    procedure CreatePacks(var aDatas: TBytes; nCmdType: Integer; ADevice: TObject);virtual;

    /// <summary>
    /// 生成回复数据包
    /// </summary>
    procedure CreateReplyPacks(var aDatas: TBytes; nCmdType: Integer; ADevice: TObject);virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

//    /// <summary>
//    /// 整理数据包
//    /// </summary>
//    function ReSetPacks(nCmdType: Integer; ADevice: TObject;aDatas: TBytes;
//      bIsReply : Boolean = False; bIsReplyRight : Boolean = True):TBytes;

    /// <summary>
    /// 获取发送回复命令的数据包
    /// </summary>
    function GetReplyPacks(nCmdType: Integer; ADevice: TObject; APacks : TBytes) : TBytes; virtual;

    /// <summary>
    /// 获取发送的数据包
    /// </summary>
    function GetPacks( nCmdType: Integer; ADevice: TObject ): tbytes; virtual;

    /// <summary>
    /// 解析接收的数据包
    /// </summary>
    function RevPacks(nCmdType: Integer; ADevice: TObject; APack: TBytes): Integer;virtual;

  end;
var
  AProtocolPacks : TProtocolPacks;

implementation

{ TProtocolPacks }

constructor TProtocolPacks.Create;
begin

end;

function TProtocolPacks.CreatePackChangeBaudRate: TBytes;
begin

end;

function TProtocolPacks.CreatePackClearData: TBytes;
begin

end;

function TProtocolPacks.CreatePackClearEvent: TBytes;
begin

end;

function TProtocolPacks.CreatePackFreeze: TBytes;
begin

end;

function TProtocolPacks.CreatePackReadData: TBytes;
begin

end;

function TProtocolPacks.CreatePackRsetTime: TBytes;
begin

end;

procedure TProtocolPacks.CreatePacks(var aDatas: TBytes; nCmdType: Integer;
  ADevice: TObject);
begin

end;

//function TProtocolPacks.CreatePacks: TBytes;
//begin
//
//end;

function TProtocolPacks.CreatePackWriteAddr: TBytes;
begin

end;

procedure TProtocolPacks.CreateReplyPacks(var aDatas: TBytes; nCmdType: Integer;
  ADevice: TObject);
begin

end;

destructor TProtocolPacks.Destroy;
begin

  inherited;
end;

function TProtocolPacks.GetPacks(nCmdType: Integer; ADevice: TObject): tbytes;
begin
  CreatePacks(Result, nCmdType, ADevice);
end;

function TProtocolPacks.GetReplyPacks(nCmdType: Integer; ADevice: TObject;
  APacks: TBytes): TBytes;
begin

end;

function TProtocolPacks.PackNoData: TBytes;
begin

end;

procedure TProtocolPacks.ParsePack(nCmdType: Integer; ADevice: TObject;
  APack: TBytes);
begin

end;

function TProtocolPacks.RevPacks(nCmdType: Integer; ADevice: TObject;
  APack: TBytes): Integer;
begin
  Result := 0;
end;

end.
