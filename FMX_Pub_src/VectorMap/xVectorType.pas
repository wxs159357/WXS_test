unit xVectorType;

interface

const
  /// <summary>
  /// 选择线的颜色
  /// </summary>
  C_COLOR_SELECT = $FFFF5580;
  C_COLOR_SELECT_MAIN = $FF000080;


  C_COLOR_A = $FFcece00;
  C_COLOR_B = $FF009B00;
  C_COLOR_C = $FFB70000;

  C_COLOR_D = $FFC0C0C0;


type
  /// <summary>
  /// 向量类型
  /// </summary>
  tTVectorType = ( vtVol,        // 电压
                   vtCurrent     // 电流
                  );

/// <summary>
/// 将自定义Vtype转换成字符串
/// </summary>
function GetVTStr(AVT : tTVectorType) : string;
function SetVTType(sStr : string):tTVectorType;
function GetVTAllStr : string;

implementation

/// <summary>
/// 将自定义Vtype转换成字符串
/// </summary>
function GetVTStr(AVT : tTVectorType) : string;
begin
  case AVT of
    vtVol: Result := '电压';
    vtCurrent: Result := '电流';
  end;
end;

function SetVTType(sStr : string):tTVectorType;
var
  i : tTVectorType;
begin
  Result := Low(tTVectorType);
  for i := Low(tTVectorType) to High(tTVectorType) do
  begin
    if GetVTStr(i) = sStr then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function GetVTAllStr : string;
var
  i : tTVectorType;
begin
  for i := Low(tTVectorType) to High(tTVectorType) do
  begin
    Result := Result + GetVTStr(i) + #13#10;
  end;
end;

end.

