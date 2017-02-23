unit xVCL_FMX;

interface

uses FMX.Dialogs, System.Classes, System.SysUtils, FMX.Forms, FMX.Types;

type
  TMySaveDialog = class(TSaveDialog)
  end;

type
  TMyOpenDialog = class(TOpenDialog)
  end;

type
  TMyTimer = class(TTimer)
  end;


/// <summary>
/// 延时等待函数，等待时处理其它事件
/// </summary>
/// <param name="nMSeconds">毫秒</param>
procedure MyWaitForSeconds( nMSeconds : Cardinal );

/// <summary>
/// 处理界面事件
/// </summary>
procedure MyProcessMessages;


implementation

procedure MyProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure MyWaitForSeconds( nMSeconds : Cardinal );
var
  nTick : Cardinal;
begin
  nTick := TThread.GetTickCount;

  repeat
    Application.ProcessMessages;
    Sleep(1);
  until TThread.GetTickCount - nTick  > nMSeconds;
end;

end.
