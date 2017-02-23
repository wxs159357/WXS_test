{===============================================================================
  常量单元 

===============================================================================}
unit xConsts;

interface

uses System.SysUtils, IniFiles;

const
  /// <summary>
  /// 回复类型
  /// </summary>
  C_REPLY_NORESPONSE   = -1;  // 未返回
  C_REPLY_CS_ERROR     = 1;   // 校验位错误
  C_REPLY_CORRECT      = 2;   // 返回数据正确
  C_REPLY_PACK_ERROR   = 4;   // 数据包错误
  C_REPLY_CODE1_ERROR  = 5;   // 设备码1错误
  C_REPLY_CODE2_ERROR  = 6;   // 设备码2错误

var
{-------------项目信息-----------------}
  C_SYS_COMPANY      : string= '****公司';
  C_SYS_WEB          : string= '';
  C_SYS_OBJECT_MODEL : string= '****型号';
  C_SYS_OBJECT_NAME  : string= '****系统';

{-------------权限信息-----------------}
  /// <summary>
  /// 是否是调试模式
  /// </summary>
  bPubIsAdmin : Boolean = False;

{-------------公共信息-----------------}
  /// <summary>
  /// 程序路径+名称
  /// </summary>
  sPubExePathName : string;

  /// <summary>
  /// 程序文件夹路径
  /// </summary>
  spubFilePath : string;

  /// <summary>
  /// INI文件名称
  /// </summary>
  sPubIniFileName : string;

  /// <summary>
  /// 软件信息INI文件名称
  /// </summary>
  sPubSysIniFileName : string;

implementation

initialization
  sPubExePathName    := ParamStr(0);
  spubFilePath       := ExtractFilePath(sPubExePathName);
  sPubIniFileName    := ChangeFileExt( sPubExePathName, '.ini' );
  sPubSysIniFileName := spubFilePath + 'Config.ini';

  if sPubSysIniFileName <> '' then
  begin
    with TIniFile.Create(sPubSysIniFileName) do
    begin
      C_SYS_COMPANY      := ReadString('System', 'Company',     '****公司');
      C_SYS_WEB          := ReadString('System', 'Web',         '');
      C_SYS_OBJECT_MODEL := ReadString('System', 'ObjectModel', '****型号');
      C_SYS_OBJECT_NAME  := ReadString('System', 'ObjectName',  '****系统');
      bPubIsAdmin        := ReadBool('System',   'IsAdmin',      False);
      free;
    end;
  end;

finalization

  if sPubSysIniFileName <> '' then
  begin
    with TIniFile.Create(sPubSysIniFileName) do
    begin
      WriteString('System',  'Company',     C_SYS_COMPANY);
      WriteString('System',  'Web',         C_SYS_WEB);
      WriteString('System',  'ObjectModel', C_SYS_OBJECT_MODEL);
      WriteString('System',  'ObjectName',  C_SYS_OBJECT_NAME);
      WriteBool('System',    'IsAdmin',     bPubIsAdmin);
      free;
    end;
  end;
end.
