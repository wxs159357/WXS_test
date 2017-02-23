unit xClientType;

interface

uses System.Classes, System.SysUtils;

type
  /// <summary>
  /// 考生状态
  /// </summary>
  TClientState = (esDisconn,      // 未连接
                  esConned,       // 已经连接
                  esLogin,        // 已登录
                  esWorkReady,    // 准备好考试
                  esWorking,      // 正在考试
                  esWorkFinished, // 交卷
                  esPractise      // 正在练习
                  );
/// <summary>
/// 状态转换成字符串
/// </summary>
function ClientStateStr(AState : TClientState) : string;

/// <summary>
/// 组包
/// </summary>
function BuildData(aData: TBytes): TBytes;

/// <summary>
/// 解析软件
/// </summary>
function AnalysisRevData(aRevData : TBytes) : TBytes;

implementation

function BuildData(aData: TBytes): TBytes;

var
  nByte, nCode1, nCode2 : Byte;
  i : Integer;
  nFixCodeCount, nIndex : Integer; // 转译字符个数

  function IsFixCode(nCode : Integer) : Boolean;
  begin
    Result := nCode in [$7F, $7E];


    if Result then
    begin
      Inc(nFixCodeCount);
      nCode1 := $7F;
      if nCode = $7E then
        nCode2 := $01
      else
        nCode2 := $02;
    end;
  end;
begin
  nByte := 0;
  nFixCodeCount := 0;

  for i := 0 to Length(aData) - 1 do
  begin
    IsFixCode(aData[i]); // 计算转译字符个数
    nByte := (nByte + aData[i])and $FF; // 计算校验码
  end;
  IsFixCode(nByte); // 判断校验码是否是转译字符

  SetLength(Result, Length(aData) + nFixCodeCount + 3);
  Result[0] := $7E;
  Result[Length(Result)-1] := $7E;
  Result[Length(Result)-2] := nByte;

  nIndex := 1;

  for i := 0 to Length(aData) - 1 do
  begin
    if IsFixCode(aData[i]) then
    begin
      Result[nIndex] := nCode1;
      Result[nIndex+1] := nCode2;

      Inc(nIndex, 2);
    end
    else
    begin
      Result[nIndex] := aData[i];
      Inc(nIndex);
    end;
  end;
end;

function AnalysisRevData(aRevData : TBytes) : TBytes;
var
  aBuf : TBytes;
  i : Integer;
  nCS : Byte;

  procedure AddByte(nByte : Byte);
  var
    nLen : Integer;
  begin
    nLen := Length(aBuf);
    SetLength(aBuf, nLen + 1);
    aBuf[nLen] := nByte;
  end;
begin
  Result := nil;
  // 处理转译字符
  for i := 0 to Length(aRevData) - 1 do
  begin
    if (aRevData[i] = $7F) and (aRevData[i+1] = $01) then
    begin

      AddByte($7E);
    end
    else if (aRevData[i] = $7F) and (aRevData[i+1] = $02) then
    begin
      AddByte($7F);
    end
    else
    begin
      AddByte(aRevData[i]);
    end;
  end;

  // 检验校验和
  nCS := 0;
  for i := 1 to Length(aBuf) - 3 do
  begin
    nCS := (nCS + aBuf[i]) and $FF;
  end;

  if nCS = aBuf[Length(aBuf)-2] then
  begin
    Result := aBuf;
  end;
end;

function ClientStateStr(AState : TClientState) : string;
begin
  case AState of
   esDisconn      :   Result := '未连接';
   esConned       :   Result := '已经连接';
   esLogin        :   Result := '已登录';
   esWorkReady    :   Result := '准备好考试';
   esWorking      :   Result := '正在考试';
   esWorkFinished :   Result := '交卷';
   esPractise     :   Result := '正在练习';
  else
    Result := '未定义';
  end;
end;


end.
