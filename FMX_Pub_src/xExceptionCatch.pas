{===============================================================================
  异常处理单元

===============================================================================}

unit xExceptionCatch;

interface

uses System.Classes, System.SysUtils, Winapi.Windows;

type
  /// <summary>
  /// 信息类型
  /// </summary>
  TMessageType = (mtNote = 0,                         //  无
                  mtInformation = MB_ICONINFORMATION, //  提示
                  mtQuestion = MB_ICONQUESTION,       //  询问
                  mtWARNING = MB_ICONWARNING,         //  警告
                  mtSTOP = MB_ICONSTOP                //  错误
                  );

/// <summary>
/// 读取文件
/// </summary>
function ExceptionReadFile:string;

procedure WriteMsg(sUnitName, sCode, sMsg : string );
procedure WriteSql( sSQL: string );

/// <summary>
/// 消息异常（显示异常信息给用户看）
/// </summary>
/// <param name="sUnitName">单元名称</param>
/// <param name="MsgType">提示信息类型</param>
/// <param name="msgStr">提示信息</param>
procedure MsgException(sUnitName:string; MsgType : TMessageType;
  sMsgStr : string );

/// <summary>
/// 记录异常（异常信息记录到文件中，不提示给用户看）
/// </summary>
/// <param name="sUnitName">单元名称</param>
/// <param name="sCode">异常代码</param>
/// <param name="sExcStr">异常信息</param>
procedure RecordException(sUnitName,sCode, sExcStr : string );

/// <summary>
/// Sql异常（异常信息记录到文件中，不提示给用户看）
/// </summary>
/// <param name="sUnitName">单元名称</param>
/// <param name="sCode">异常代码</param>
/// <param name="sExcStr">异常信息</param>
/// <param name="sSqlStr">执行的Sql语句</param>
procedure SQLException(sUnitName,sCode, sExcStr, sSqlStr : string );

implementation

{ TEXCEPTION_CATCH }

function ExceptionReadFile:string;
var
  s,s2:string;
  FFile : TextFile;
begin
  s:='';
  Result:='';
  AssignFile(FFile,'Exception Code.txt');
  if FileExists('Exception Code.txt') then
  begin
    try
      Reset(FFile);
      while not Eof(FFile) do
      begin
        Readln(FFile, s2);
        s:=s + s2 + #13#10;
      end;
      Result:=s;
    finally
      CloseFile(FFile);
    end;
  end;    
end;

procedure MsgException(sUnitName:string;MsgType: TMessageType; sMsgStr: string);
begin
  WriteMsg(sUnitName, '-1', sMsgStr);
  MessageBox(0, PWideChar(sMsgStr), '', MB_OK + Integer(MsgType));
//  Application.MessageBox(PWideChar(sMsgStr), '', MB_OK + Integer(MsgType));
end;

procedure RecordException(sUnitName,sCode, sExcStr: string);
begin
  WriteMsg(sUnitName,sCode, sExcStr);
end;

procedure SQLException(sUnitName,sCode, sExcStr, sSqlStr: string);
begin
  WriteMsg(sUnitName,sCode, sExcStr);
  WriteSql(sSqlStr);
  MessageBox(0,PWideChar(sExcStr), '', MB_OK + MB_ICONWARNING);
end;

procedure WriteMsg(sUnitName, sCode, sMsg: string);
const
  C_SIGN = '------------------------------------------------';
var
  s : string;
  FFile : TextFile;
begin
  AssignFile(FFile,'Exception Code.txt');
  s := ExceptionReadFile;
  try
    ReWrite(FFile);

    write(FFile, s);
    Writeln( FFile, C_SIGN + C_SIGN );
    Writeln( FFile, '异常时间：' + FormatDateTime('YYYY-MM-DD hh:mm:ss', Now));
    Writeln( FFile, '异常文件：' + sUnitName );
    Writeln( FFile, '异常编码：' + sCode );
    Writeln( FFile, '异常信息：' + sMsg );
  finally
    CloseFile(FFile);
  end;
end;

procedure WriteSql(sSQL: string);
var
  s : string;
  FFile : TextFile;
begin
  AssignFile(FFile,'Exception Code.txt');
  s := ExceptionReadFile;

  try
    ReWrite(FFile);
    write(FFile, s);
    Write( FFile, 'SQL 信息：' + sSQL );
  finally
    CloseFile(FFile);
  end;
end;

end.

