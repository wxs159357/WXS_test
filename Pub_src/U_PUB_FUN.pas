{===============================================================================
  Copyright(c) 2013, 
  All rights reserved.

  公用函数
  + function GetSpecialFolder  获取Windows系统文件夹
  + function GetTempFolder     获取Windows临时文件夹
  + procedure WaitForSeconds   延时等待函数，等待时处理其它事件
  + function IntToBCD          整型数据转换成BCD码
  + function IntToByte         从整型数据中取某一个字节
  + function CalCS             计算CS校验码
  + function BCDToDouble       BCD码转换Double, nDigital为小数位数
  + procedure WriteRunLog      保存运行记录
  + function CheckAndCreateDir 检查文件夹是否存在，不存在创建
  + function GetFileFullVersion 获取完整文件版本信息
  + function CheckPath         检查目录的完整性
  + function GetFloatIntLength 获取浮点数据整数部分长度，不包含负号
  + function FileCopy          文件拷贝，如果显示进度，文件将被分成100段进行拷贝
  + procedure ClearStringList  清除 TStringList / TStrings 及 内部对象
  + function ApplicationExists 判断程序是否已经运行
  + function GetPartValue      获取用spart分割的值列表
  + function BytesToASC        Tbytes转换成string $31 32 ---> '12'
  + function BytesToPackStr    Tbytes转换成string $31 32 ---> '31 32'
  + procedure PcBeep           PC喇叭蜂鸣
  + function CalCRC16          计算CRC16校验码
  + function IntToLenStr       整型转化成固定长度的字符串，如果整型值过大则显示所有整型值
  + function GetIPFromURL      根据域名获取IP地址


===============================================================================}

unit U_PUB_FUN;

interface

uses SysUtils, Windows, Forms, Math, ShlObj, Classes, CommCtrl, Messages,Dialogs,
  StrUtils, WinSock;

type
   P8byte  = ^Byte;

type
  /// <summary>
  /// Windows系统文件夹类型
  /// </summary>
  TSpecialFolder = (
    sfDesktop,                // <desktop>
    sfInternet,               // Internet Explorer (icon on desktop)
    sfPrograms,               // Start Menu\Programs
    sfControls,               // My Computer\Control Panel
    sfPrinters,               // My Computer\Printers
    sfPersonal,               // My Documents
    sfFavorites,              // <user name>\Favorites
    sfStartup,                // Start Menu\Programs\Startup
    sfRecent,                 // <user name>\Recent
    sfSendTo,                 // <user name>\SendTo
    sfBitBucket,              // <desktop>\Recycle Bin
    sfStartMenu,              // <user name>\Start Menu
    sfMyDocuments,            // logical "My Documents" desktop icon
    sfMyMusic,                // "My Music" folder
    sfMyVideo,                // "My Videos" folder
    sfDesktopDirectory,       // <user name>\Desktop
    sfDrives,                 // My Computer
    sfNetwork,                // Network Neighborhood (My Network Places)
    sfNethood,                // <user name>\nethood
    sfFonts,                  // windows\fonts
    sfTemplates,              // <user name>\Templates
    sfCommonStartMenu,        // All Users\Start Menu
    sfCommonPrograms,         // All Users\Start Menu\Programs
    sfCommonStartup,          // All Users\Startup
    sfCommonDesktopDirectory, // All Users\Desktop
    sfAppData,                // <user name>\Application Data
    sfPrinthood,              // <user name>\PrintHood
    sfLocalAppData,           // <user name>\Local Settings\Applicaiton Data (non roaming)
    sfALTStartup,             // non localized startup
    sfCommonALTStartup,       // non localized common startup
    sfCommonFavorites,        // All Users\Favorites
    sfInternetCache,          // <user name>\Local Settings\Temporary Internet Files
    sfCookies,                // <user name>\Cookies
    sfHistory,                // <user name>\Local Settings\History
    sfCommonAppData,          // All Users\Application Data
    sfWindows,                // GetWindowsDirectory()
    sfSystem,                 // GetSystemDirectory()
    sfProgramFiles,           // C:\Program Files
    sfMyPictures,             // C:\Program Files\My Pictures
    sfProfile,                // USERPROFILE
    sfSystemX86,              // x86 system directory on RISC
    sfProgramFilesX86,        // x86 C:\Program Files on RISC
    sfProgramFilesCommon,     // C:\Program Files\Common
    sfProgramFilesCommonX86,  // x86 Program Files\Common on RISC
    sfCommonTemplates,        // All Users\Templates
    sfCommonDocuments,        // All Users\Documents
    sfCommonAdminTools,       // All Users\Start Menu\Programs\Administrative Tools
    sfAdminTools,             // <user name>\Start Menu\Programs\Administrative Tools
    sfConnections,            // Network and Dial-up Connections
    sfCommonMusic,            // All Users\My Music
    sfCommonPictures,         // All Users\My Pictures
    sfCommonVideo,            // All Users\My Video
    sfResources,              // Resource Direcotry
    sfResourcesLocalized,     // Localized Resource Direcotry
    sfCommonOEMLinks,         // Links to All Users OEM specific apps
    sfCDBurnArea,             // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    sfComputersNearMe,        // Computers Near Me (computered from Workgroup membership)
    sfTemp
  );

/// <summary>
/// 获取Windows系统文件夹
/// </summary>
/// <param name="SpecialFolder">Windows系统文件夹类型</param>
/// <returns>Windows系统文件夹</returns>
function GetSpecialFolder(SpecialFolder: TSpecialFolder): string;

/// <summary>
/// 获取Windows临时文件夹
/// </summary>
/// <returns>Windows临时文件夹</returns>
function GetTempFolder : string;

/// <summary>
/// 延时等待函数，等待时处理其它事件
/// </summary>
/// <param name="nMSeconds">毫秒</param>
procedure WaitForSeconds( nMSeconds : Cardinal );

/// <summary>
/// 整型数据转换成BCD码
/// </summary>
/// <param name="nNum">整型数据</param>
/// <param name="nLen">BCD码字节长度</param>
/// <returns>BCD码字节数组</returns>
function IntToBCD( nNum, nLen : Integer): TBytes; overload;
function IntToBCD( nNum : Int64; nLen : Integer): TBytes; overload;

/// <summary>
/// 从整型数据中取某一个字节
/// </summary>
/// <param name="n">整型数据</param>
/// <param name="nPos">从低位开始的第*个字节, nPos = 0, 1, 2, 3...</param>
/// <returns>字节数据</returns>
function IntToByte( const n, nPos : Integer ) : Byte; overload;
function IntToByte( const n : Int64; const nPos : Integer ) : Byte; overload;

/// <summary>
/// 计算CS校验码
/// </summary>
/// <param name="AData">字节数组</param>
/// <param name="AFrom">开始字节</param>
/// <param name="ATo">终止字节，-1为最大长度</param>
function CalCS( AData : TBytes; AFrom : Integer = 0; ATo : Integer = -1 ) : Byte;

/// <summary>
/// BCD转换Double, nDigital为小数位数
/// </summary>
/// <param name="aBCD"></param>
/// <param name="nDigital"></param>
/// <returns></returns>
function BCDToDouble( aBCD : TBytes; nDigital : Integer ) : Double;

/// <summary>
/// 保存运行记录
/// </summary>
/// <param name="sLogFileName">运行记录文件</param>
/// <param name="sRunMsg">运行记录</param>
procedure WriteRunLog( sLogFileName : string; sRunMsg : string );

/// <summary>
/// 检查文件夹是否存在，不存在创建
/// </summary>
/// <param name="sDirPath">文件夹路径</param>
/// <returns>是否存在</returns>
function CheckAndCreateDir( sDirPath : string ) : Boolean;

/// <summary>
/// 获取完整文件版本信息
/// </summary>
/// <param name="sFileName">文件名称</param>
/// <returns>完整版本信息 例如:1.0.0.0</returns>
function GetFileFullVersion( sFileName : string ) : string;

/// <summary>
/// 检查目录的完整性
/// 例如 c:\windows 的完整路径为 c:\windows\
/// </summary>
/// <param name="sPath">原路径</param>
/// <param name="sDelim">分隔符 \ 或 /</param>
/// <returns>完整路径</returns>
function CheckPath( sPath, sDelim : string ) : string;

/// <summary>
/// 获取浮点数据整数部分长度，不包含负号
/// </summary>
/// <param name="d">浮点数据</param>
/// <returns>整数部分长度</returns>
function GetFloatIntLength( d : Double ) : Integer;

/// <summary>
/// 文件拷贝，如果显示进度，文件将被分成100段进行拷贝
/// </summary>
/// <param name="sFileFrom">源文件名</param>
/// <param name="sFileTo">目标文件名</param>
/// <param name="hPB">进度条TProgressBar句柄</param>
/// <param name="sError">错误信息</param>
/// <returns>拷贝是否成功</returns>
function FileCopy( const sFileFrom, sFileTo : string; hPB : THandle;
  var sError : string ) : Boolean;

/// <summary>
/// 清除 TStringList / TStrings 及 内部对象
/// </summary>
/// <param name="sl">TStringList / TStrings</param>
procedure ClearStringList( sl : TStrings );

/// <summary>
/// 判断程序是否已经运行
/// </summary>
/// <param name="SpecialFolder"></param>
/// <returns></returns>
function ApplicationExists( AClass, ATitle : string;
  AShowIfExists : Boolean = True ) : Boolean;

/// <summary>
/// 获取用spart分割的值列表
/// </summary>
/// <param name="slValue">值列表</param>
/// <param name="sText">值字符串</param>
/// <param name="sPart">分隔符</param>
/// <returns>是否成功</returns>
function GetPartValue( slValue : TStringList; const sText : string;
  const sPart : Char ): Boolean;

/// <summary>
/// 字符串转换成Tbytes '12' ---> $12      （U_PUB_COMM_FUN 中函数替代）
/// </summary>
function StrToBytes( sData : string ) : TBytes;

/// <summary>
/// 字符串转换成Tbytes '12' ---> $31 32  （U_PUB_COMM_FUN 中函数替代）
/// </summary>
function StrToBytes1( sData : string ) : TBytes;

/// <summary>
/// Tbytes转换成string $31 32 ---> '12' （U_PUB_COMM_FUN 中函数替代）
/// </summary>
function BytesToASC( aBytes : TBytes ) : string;

/// <summary>
/// Tbytes转换成string $31 32 ---> '31 32' （U_PUB_COMM_FUN 中函数替代）
/// </summary>
function BytesToPackStr( aPack: TBytes ) : string;

/// <summary>
/// PC喇叭蜂鸣
/// </summary>
/// <param name="nFreq">频率</param>
/// <param name="nDura">声音持续时间（毫秒）</param>
procedure PcBeep(nFreq : Word = 500; nDura: Word = 1000);

/// <summary>
/// 计算CRC16校验码
/// </summary>
function CalCRC16(AData: array of Byte; AStart,AEnd: Integer): Word;

function iMask_Crc16(AData: TBytes; AStart, AEnd: Integer; Mask, Init: Word): Word;

/// <summary>
/// 整型转化成固定长度的字符串，如果整型值过大则显示所有整型值
/// </summary>
/// <param name="nValue">整型值</param>
/// <param name="nStrLen">转换后长度</param>
/// <param name="sStr">不够长度补齐的字符 默认为'0'</param>
/// <returns></returns>
function IntToLenStr(nValue, nStrLen : Integer; sStr : string = '0'): string;

/// <summary>
/// 计算Xor校验码
/// </summary>
/// <param name="AData">字节数组</param>
/// <param name="AFrom">开始字节</param>
/// <param name="ATo">终止字节，-1为最大长度</param>
function CalXor( AData : TBytes; AFrom : Integer = 0; ATo : Integer = -1 ) : Byte;


///// <summary>
///// 根据域名获取IP地址
///// </summary>
//function GetIPFromURL(sURL: string): string;

implementation

//function GetIPFromURL(sURL: string): string;
//var
//  WSAData: TWSAData;
//  HostEnt: PHostEnt;
//begin
//  WSAStartup(2, WSAData);
//  HostEnt := gethostbyname(PChar(sURL));
//
//  if Assigned(HostEnt) then
//  begin
//    with HostEnt^ do
//      Result := Format('%d.%d.%d.%d', [Byte(h_addr^[0]),
//        Byte(h_addr^[1]), Byte(h_addr^[2]), Byte(h_addr^[3])]);
//  end;
//
//  WSACleanup;
//end;

function CalXor( AData : TBytes; AFrom : Integer = 0; ATo : Integer = -1 ) : Byte;
var
  nFrom, nTo : Integer;
  i: Integer;
begin
  if Length( AData ) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // 确定开始和终止位置
  if AFrom < Low( AData ) then
    nFrom := Low( AData )
  else if AFrom > High( AData ) then
    nFrom := High( AData )
  else
    nFrom := AFrom;

  if ATo = -1 then
    nTo := High( AData )
  else if ATo < Low( AData ) then
    nTo := Low( AData )
  else if ATo > High( AData ) then
    nTo := High( AData )
  else
    nTo := ATo;
  Result := 0;
  for i := nFrom to nTo do
    Result := Result xor AData[ i ];
end;

function IntToLenStr(nValue, nStrLen : Integer; sStr : string): string;
var
  s : string;
  nLen : Integer;
begin
  s := IntToStr(nValue);

  nLen := nStrLen - Length(s);
  if nLen > 0 then
  begin
    Result := DupeString(sStr, nLen) + s;
  end
  else
    Result := s;
end;

function CalCRC16(AData: array of Byte; AStart,AEnd: Integer): Word;
const
  GENP = $A001; //多项式公式X16+X15+X2+1（1100 0000 0000 0101）
var
  crc:Word;
  i:Integer;
  tmp:Byte;
  procedure CalOneByte(AByte:Byte); //计算1个字节的校验码
  var
    j:Integer;
  begin
    crc:=crc xor AByte; //将数据与CRC寄存器的低8位进行异或

    for j := 0 to 7 do //对每一位进行校验
    begin
      tmp:=crc and 1; //取出最低位
      crc:=crc shr 1; //寄存器向右移一位
      crc:=crc and $7FFF; //将最高位置0

      if tmp=1 then //检测移出的位，如果为1，那么与多项式异或
        crc:=crc xor GENP;

      crc:=crc and $FFFF;
    end;
  end;
begin
  crc := $FFFF; //将余数设定为FFFF

  for i:=AStart to AEnd do //对每一个字节进行校验
    CalOneByte(AData[i]);

  Result:=crc;
end;

function iMask_Crc16(AData: TBytes; AStart,AEnd: Integer; Mask, Init: Word): Word;
var
  i, t : Integer;
  crc : Word;
  i_crc : Cardinal;
  b1 : Byte;
begin
  crc := init;

  for i := AStart to AEnd do
  begin
    b1 := AData[i];
    t := b1;
    t:= t*256;
    crc := crc xor t;
    for t := 1 to 8 do
    begin
      if ((crc and $8000) = $8000) then
      begin
        i_crc := (crc * 2) xor Mask;
        crc := word(i_crc);
      end
      else
      begin
        crc := crc *2;
      end;
    end;
  end;
  Result := crc;
end;

procedure PcBeep(nFreq, nDura: Word);
var
  VerInfo: TOSVersionInfo;
  nStart: DWord;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx(VerInfo);

  if VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    Windows.Beep(nFreq, nDura)
  else
  begin 
    Asm
      push bx
      in al,$61 
      mov bl,al 
      and al,3
      jne @@Skip 
      mov al,bl 
      or al,3
      out $61,al 
      mov al,$b6 
      out $43,al
      @@Skip: 
      mov ax,nFreq 
      out $42,al
      mov al,ah 
      out $42,al 
      pop bx
    end; 
    nStart:=GetTickCount;
    repeat
      Application.ProcessMessages;
    Until GetTickCount > nStart + nDura;
    asm
      IN AL,$61
      AND AL,$FC
      OUT $61,AL
    end;
  end;
end;

function BytesToPackStr( aPack: TBytes ) : string;
var
  i : Integer;
begin
  Result := EmptyStr;
  
  for i := 0 to High( aPack ) do
    Result := Result + ' ' + IntToHex( aPack[ i ], 2 );
end;

function BytesToASC( aBytes : TBytes ) : string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(aBytes) - 1 do
    Result := Result + Char(aBytes[i]);
end;

function StrToBytes( sData : string ) : TBytes;
  function GetIndex( sValue, sFormat : string ) : Integer;
  begin
    Result := Pos( sFormat, sValue );
  end;
var
  s, sValue : string;
  i: Integer;
  nValue : Integer;
begin
  try
    SetLength( Result, 0 );

    //  删除空格
    s := StringReplace(sData ,' ','',[rfReplaceAll]);

    //  取值
    for i := 1 to Length(s) do
    begin
      if s[i] <> #0 then
        sValue := sValue + s[i];

      if Length(sValue) = 2 then
      begin
        SetLength( Result, Length( Result ) + 1 );
        TryStrToInt( '$' + sValue, nValue );
        Result[ Length( Result ) - 1 ] := nValue;
        sValue := '';
      end;
    end;
  except

  end;
end;

function StrToBytes1( sData : string ) : TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(sData));

  for i := 0 to Length(Result) - 1 do
    Result[i] := ord(sData[i+1]);
end;

function GetSpecialFolder(SpecialFolder: TSpecialFolder): string;
const
  SpecialFolderValues: array[TSpecialFolder] of Integer = ($0000, $0001, $0002,
    $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000a, $000b, $000c, $000d,
    $000e, $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019,
    $001a, $001b, $001c, $001d, $001e, $001f, $0020, $0021, $0022, $0023, $0024,
    $0025, $0026, $0027, $0028, $0029, $002a, $002b, $002c, $002d, $002e, $002f,
    $0030, $0031, $0035, $0036, $0037, $0038, $0039, $003a, $003b, $003d, $FFFF);
var
  ItemIDList: PItemIDList;
  Buffer: array [0..MAX_PATH] of Char;
begin
  if SpecialFolder = sfTemp then
    Result := GetTempFolder
  else
  begin
    SHGetSpecialFolderLocation( 0, SpecialFolderValues[ SpecialFolder ],
      ItemIDList );
    SHGetPathFromIDList(ItemIDList, Buffer);
    Result := StrPas(Buffer);
  end;
end;

function GetTempFolder : string;
var
  Buffer : array[0..MAX_PATH] of Char;
begin
  GetTempPath( MAX_PATH, Buffer );
  Result := StrPas( Buffer );
end;

procedure WaitForSeconds( nMSeconds : Cardinal );
var
  nTick : Cardinal;
begin
  nTick := GetTickCount;

  repeat
    Application.ProcessMessages;
    Sleep(5);
  until GetTickCount - nTick  > nMSeconds;
end;

function IntToBCD( nNum, nLen : Integer): TBytes;
var
  i : Integer;
begin
  SetLength( Result, nLen );

  for i := 0 to nLen - 1 do
  begin
    Result[i] := (((nNum mod 100) div 10) shl 4) + (nNum mod 10);
    nNum := nNum div 100;
  end;
end;

function IntToBCD( nNum : Int64; nLen : Integer): TBytes;
var
  i : Integer;
begin
  SetLength( Result, nLen );

  for i := 0 to nLen - 1 do
  begin
    Result[i] := (((nNum mod 100) div 10) shl 4) + (nNum mod 10);
    nNum := nNum div 100;
  end;
end;  

function IntToByte( const n, nPos : Integer ) : Byte;
begin
  if nPos in [ 0..3 ] then
    Result := n shr ( 8 * nPos ) and $FF
  else
    Result := $00;
end;

function IntToByte( const n : Int64; const nPos : Integer ) : Byte;
begin
  if nPos in [ 0..7 ] then
    Result := n shr ( 8 * nPos ) and $FF
  else
    Result := $00;
end;

function CalCS( AData : TBytes; AFrom : Integer = 0; ATo : Integer = -1 ) : Byte;
var
  nFrom, nTo : Integer;
  nSum : Integer;
  i: Integer;
begin
  if Length( AData ) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // 确定开始和终止位置
  if AFrom < Low( AData ) then
    nFrom := Low( AData )
  else if AFrom > High( AData ) then
    nFrom := High( AData )
  else
    nFrom := AFrom;

  if ATo = -1 then
    nTo := High( AData )
  else if ATo < Low( AData ) then
    nTo := Low( AData )
  else if ATo > High( AData ) then
    nTo := High( AData )
  else
    nTo := ATo;

  // 计算CS
  nSum := 0;

  for i := nFrom to nTo do
    nSum := nSum + AData[ i ];

  Result := nSum and $FF;
end;

function BCDToDouble( aBCD : TBytes; nDigital : Integer ) : Double;
var
  i : Integer;
  nInt : Integer;
begin
  Result := 0;
  nInt := 2 * Length( aBCD ) - nDigital; // 整数长度

  for i := 0 to High( aBCD ) do
    Result := Result + ( ( aBCD[ i ] shr 4 ) * 10 + ( aBCD[ i ] and $F ) ) *
      Power( 10, nInt - ( i + 1 ) * 2 );
end;

procedure WriteRunLog( sLogFileName : string; sRunMsg : string );
var
  fileRunLog : TextFile;
begin
  AssignFile( fileRunLog, sLogFileName );

  if not FileExists( sLogFileName ) then
    Rewrite(fileRunLog);

  try
    Append( fileRunLog );
//    Writeln( fileRunLog, PChar( FormatDateTime( 'YYYY-MM-DD hh:mm:ss:zzz',Now )
//      + #9 + sRunMsg ));

    Writeln( fileRunLog, PChar( DateTimeToStr(Now)
      + #9 + sRunMsg ));
    CloseFile( fileRunLog );
  except
  end;
end;

function CheckAndCreateDir( sDirPath : string ) : Boolean;
begin
  Result := DirectoryExists( sDirPath );

  // 文件夹不存在则创建
  if not Result then
    Result := CreateDir( sDirPath );
end;

function GetFileFullVersion( sFileName : string ) : string;
var
  VerValue : PVSFixedFileInfo;
  VerInfoSize, VerValueSize, Dummy : Dword;
  VerInfo : Pointer;
  V1, V2, V3, V4 : word;
begin
  Result := EmptyStr;

  if FileExists( sFileName ) then
  begin
    VerInfoSize := GetFileVersionInfoSize( Pchar( sFileName ), Dummy );

    if VerInfoSize > 0 then
    begin
      GetMem( VerInfo, VerInfoSize );
      GetFileVersionInfo( PChar( sFileName ), 0, VerInfoSize, VerInfo );
      VerQueryValue( VerInfo, '\', Pointer(VerValue), VerValueSize );

      if Assigned( VerValue ) then
      begin
        With VerValue^ do
        begin
          V1 := dwFileVersionMS shr 16;
          V2 := dwFileVersionMS and $FFFF;
          V3 := dwFileVersionLS shr 16;
          V4 := dwFileVersionLS and $FFFF;
        end;

        FreeMem(VerInfo,VerInfoSize);
        Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' +
          IntToStr(V4);
      end;
    end
    else
      Result := EmptyStr;
  end
  else
    Result := EmptyStr;
end;

function CheckPath( sPath, sDelim : string ) : string;
begin
  if sPath <> EmptyStr then
  begin
    if sPath[ Length( sPath ) ] <> sDelim then
      Result := sPath + sDelim
    else
      Result := sPath;
  end;
end;

function GetFloatIntLength( d : Double ) : Integer;
var
  n : Int64;
begin
  // 获得整数, 并去掉正负号
  n := Abs( Trunc( d ) );

  if n > 0 then
    Result := ( n - 1 )
  else
    Result := Length( IntToStr( n ) );
end;

function FileCopy( const sFileFrom, sFileTo : string; hPB : THandle;
  var sError : string ) : Boolean;
const
  C_COPY_DIV = 100;  // 拷贝分段数
var
  fsFrom, fsTo : TFileStream; // 文件流
  nCopyLen : Int64;  // 已拷贝文件的长度
  nDivLen  : Int64;  // 分段拷贝的长度
begin
  try
    fsFrom := TFileStream.Create( sFileFrom, fmOpenRead or fmShareDenyWrite );

    try
      // 如果目标文件存在，删除
      if FileExists( sFileTo ) then
        if not DeleteFile( PWideChar( sFileTo + #0 ) ) then
          raise Exception.Create( '目标文件已存在, 而且不能覆盖' );

      fsTo := TFileStream.Create( sFileTo, fmCreate or fmOpenWrite );

      // 如果不显示进度，直接拷贝
      if hPB = 0 then
      begin
        fsTo.CopyFrom( fsFrom, fsFrom.Size );
      end
      else
      begin
        // 将文件分成100段进行拷贝
        nDivLen := fsFrom.Size div C_COPY_DIV;
        nCopyLen := 0;

        // 进度条默认使用默认设置
        // 设置进度条初始位置
        SendMessage( hPB, PBM_SETPOS, 0, 0 );

        // 分段拷贝文件
        repeat
          if nCopyLen + nDivLen < fsFrom.Size then
          begin
            nCopyLen := nCopyLen + fsTo.CopyFrom( fsFrom, nDivLen );
            SendMessage( hPB, PBM_STEPIT, 0, 0 );     // 进度+1
            Application.ProcessMessages;
          end
          else
          begin
            fsTo.CopyFrom( fsFrom, fsFrom.Size - nCopyLen );
            nCopyLen := fsFrom.Size;
            SendMessage( hPB, PBM_SETPOS, C_COPY_DIV, 0 );  // 进度显示完成
            Application.ProcessMessages;
          end;
        until nCopyLen = fsFrom.Size;
      end;

      fsFrom.Free;
      fsTo.Free;
      Result := True;
    except
      on e: Exception do
      begin
        sError := e.Message;
        fsFrom.Free;
        Result := False;
      end;
    end;
  except
    on e: Exception do
    begin
      sError := e.Message;
      Result := False;
    end;
  end;
end;

procedure ClearStringList( sl : TStrings );
var
  i : Integer;
begin
  if not Assigned( sl ) then
    Exit;


  for i := 0 to sl.Count - 1 do
    if Assigned( sl.Objects[ i ] ) then
      sl.Objects[ i ].Free;

  sl.Clear;  
end;

function ApplicationExists( AClass, ATitle : string;
  AShowIfExists : Boolean = True ) : Boolean;
var
  h : THandle;
  pClass, pTitle : PWideChar;
begin
  if Trim( AClass ) <> EmptyStr then
    pClass := PWideChar( AClass )
  else
    pClass := nil;

  if Trim( ATitle ) <> EmptyStr then
    pTitle := PWideChar( ATitle )
  else
    pTitle := nil;

  h := FindWindow( pClass, pTitle );

  Result := h <> 0;

  if Result and AShowIfExists then
  begin
    //用原来的位置和大小恢复窗口并激活
    windows.ShowWindow( h, SW_RESTORE );
    windows.SetForegroundWindow( h );
    windows.SetActiveWindow( h );
  end;
end;

function GetPartValue( slValue : TStringList; const sText : string;
  const sPart : Char ): Boolean;
var
  s : string;
  nIndex : Integer;
begin
  Result := False;
  slValue.Clear;
  if not Assigned( slValue ) then
    Exit;

  if sText = '' then
    Exit;

  s := sText;
  nIndex := Pos( sPart, s );
  while not (nIndex = 0) do
  begin
    if nindex <> 1 then
      slValue.Add(Copy(s, 0, nindex-1))
    else
      slValue.Add('');

    s := Copy(s, nIndex + 1, Length(s)- nindex + 1);
    nIndex := Pos( sPart, s );
  end;
  if (s <> '') and (s <>sPart ) then
    slValue.Add(s);

  Result := True;
end;

end.
