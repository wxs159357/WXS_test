unit xConfig;

interface

uses System.SysUtils, IniFiles, xConsts;

var
  /// <summary>
  /// 数据库服务器IP地址
  /// </summary>
  sPubDBServerIP : string;

  /// <summary>
  /// 数据库服务器端口
  /// </summary>
  sPubDBServerPort : Integer;

implementation

initialization
  if sPubIniFileName <> '' then
  begin
    with TIniFile.Create(sPubIniFileName) do
    begin
      sPubDBServerIP   := ReadString('Option', 'DBServerIP', '');
      sPubDBServerPort := ReadInteger('Option', 'DBServerPort', 15000);

      free;
    end;
  end;

finalization

  if sPubSysIniFileName <> '' then
  begin
    with TIniFile.Create(sPubSysIniFileName) do
    begin
      WriteString('Option',  'DBServerIP',   sPubDBServerIP);
      WriteInteger('Option', 'DBServerPort', sPubDBServerPort);

      free;
    end;
  end;
end.
