unit xProtocolType;

interface

const

  /// <summary>
  /// 最大命令值， 下面命令不得超过这个值
  /// </summary>
  C_MAX_ORDER = 10000;

  {通用}
  C_GET_VERSION       = 1; // 获取版本
  C_SET_ADDR          = 2; // 获取版本

  {语音 时钟}
  C_VOICE_PLAY        = 10; // 播放语音
  C_TIME_SET          = 11; // 设置时钟


/// <summary>
/// 获取命令码
/// </summary>
function GetControlCode(const nCommType: Integer) : Integer;

/// <summary>
/// 获取设备码
/// </summary>
function GetDevCode(const nCommType: Integer) : Integer;

/// <summary>
/// 获取命令描述
/// </summary>
function GetCommTypeStr(const nCommType: Integer):string;

/// <summary>
/// 根据设备码和命令码获取命令类型
/// </summary>
function GetCommType(nDevCode, nControlCode : Integer) : Integer;

implementation

function GetCommType(nDevCode, nControlCode : Integer) : Integer;
var
  i: Integer;
  nC1,nC2 : Integer;
begin
  Result := -1;
  for i := 0 to C_MAX_ORDER do
  begin
    nC1 := GetDevCode(i);
    nC2 := GetControlCode(i);

    if (nDevCode = nC1) and (nControlCode = nC2) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function GetControlCode(const nCommType: Integer) : Integer;
begin
  case nCommType of
    C_GET_VERSION      : Result:= $01; // 获取版本
    C_SET_ADDR         : Result:= $02; // 获取版本

    {时钟语音}
    C_TIME_SET         : Result:= $01; // 设置时钟
    C_VOICE_PLAY       : Result:= $02; // 播放语音
  else
    Result := $00;
  end;
end;

function GetDevCode(const nCommType: Integer) : Integer;
begin
  {通用}
  if nCommType in [0..9] then
  begin
    Result := $00;
  end
  {语音时钟}
  else if nCommType in [10..19] then
  begin
    Result := $01;
  end
  else
  begin
    Result := 0;
  end;
end;


function GetCommTypeStr(const nCommType: Integer):string;
var
  i : Integer;
begin
  case nCommType of
    C_GET_VERSION : Result:= '获取版本';

    {时钟语音}
    C_TIME_SET         : Result:= '设置时钟';
    C_VOICE_PLAY        : Result:= '播放语音';

  else
    Result := '未定义';
  end;

  if Length(Result) < 25 then
  begin
    for i := Length(Result) to 25 do
    begin
      Result := Result + ' ' ;
    end;
  end;
end;

end.
