unit FrmRevScreenMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, xUDPRevScreen, Vcl.Imaging.jpeg, System.Types,
  Vcl.ExtCtrls, Vcl.StdCtrls, xFunction, System.IniFiles, xConsts, System.DateUtils;

type
  TfRevScreenMain = class(TForm)
    img1: TImage;
    mmo1: TMemo;
    tmr1: TTimer;
    scrlbx1: TScrollBox;
    spltr1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure img1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure img1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure img1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FTempTime : Integer;
    FRevScreen : TUDPRevScreen;
    FShowLog : Boolean;
    FCaption : string;
    pBefore : TPoint;
    procedure ReadINI;
    procedure WriteINI;

    procedure RevJpg(Sender: TObject);
    procedure UDPPacksLog1( sIP : string; nPort: Integer; aPacks: TBytes; bSend : Boolean);
  public
    { Public declarations }
  end;

var
  fRevScreenMain: TfRevScreenMain;

implementation

{$R *.dfm}

procedure TfRevScreenMain.FormCreate(Sender: TObject);
begin
  FRevScreen := TUDPRevScreen.Create;
  ReadINI;
  FRevScreen.Connect;
  FRevScreen.OnRevJpg := RevJpg;
  if FShowLog then
    FRevScreen.OnIPSendRev := UDPPacksLog1;

  mmo1.Visible := FShowLog;
  pBefore.X := -1;
  pBefore.Y := -1;
end;
procedure TfRevScreenMain.FormDestroy(Sender: TObject);
begin
  WriteINI;
  FRevScreen.Free;
end;

procedure TfRevScreenMain.img1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pBefore.X := X;
  pbefore.Y := Y;
  Screen.Cursor := crSize;
end;

procedure TfRevScreenMain.img1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  nX, nY : Integer;
begin
  if ( pBefore.X > 0 ) and ( pBefore.Y > 0 ) then
  begin
    nX := pBefore.X - X;
    nY := pBefore.Y - Y;
    scrlbx1.VertScrollBar.Position := scrlbx1.VertScrollBar.Position + nY;
    scrlbx1.HorzScrollBar.Position := scrlbx1.HorzScrollBar.Position + nX;
  end;
end;

procedure TfRevScreenMain.img1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pBefore := Point( -1, -1 );
  Screen.Cursor := crDefault;
end;

procedure TfRevScreenMain.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    FRevScreen.ListenPort := ReadInteger('OptionRevScreen', 'ListenPort', 16101);
    Caption := '接收屏幕端口：' + IntToStr(FRevScreen.ListenPort);

    FShowLog := ReadBool('OptionRevScreen', 'ShowLog', False);
    Free;
  end;
end;

procedure TfRevScreenMain.RevJpg(Sender: TObject);
var
  nTemp : Integer;
begin
  if Sender is TJPEGImage then
  begin
    if FShowLog then
      mmo1.Lines.Add('刷新图片' + IntToStr(TJPEGImage(Sender).Width) + ';' + IntToStr(TJPEGImage(Sender).Height));

    img1.Width := TJPEGImage(Sender).Width;
    img1.Height := TJPEGImage(Sender).Height;

    try
      // 有时候会报错 jpg Error #61，报错后会一直报错，所以加下面处理
      img1.Picture.Bitmap.Assign(TJPEGImage(Sender));
    except
      img1.Picture.Bitmap.FreeImage;
    end;

    nTemp := MilliSecondOfTheHour(Now);
    FCaption := FormatFloat('0.00', 1000/(nTemp - FTempTime))+'侦/秒    ';
    FTempTime:=nTemp;
  end;
end;
procedure TfRevScreenMain.tmr1Timer(Sender: TObject);
begin
  if FCaption <> '' then
    Caption := FCaption;
end;

procedure TfRevScreenMain.UDPPacksLog1(sIP: string; nPort: Integer;
  aPacks: TBytes; bSend: Boolean);
var
  s : string;
  aBuf : TBytes;
begin
  if mmo1.Lines.Count > 5000 then
  begin
    mmo1.Lines.Clear;
    mmo1.Lines.Add('===========删除前5000行通讯记录=========')
  end;


  if bsend then
    s := '发送'
  else
    s := '接收';

  SetLength(aBuf, 10);

  aBuf[0] := aPacks[0];
  aBuf[1] := aPacks[1];
  aBuf[2] := aPacks[2];
  aBuf[3] := aPacks[3];
  aBuf[4] := aPacks[4];
  aBuf[5] := aPacks[5];
  aBuf[6] := aPacks[6];
  aBuf[7] := aPacks[7];
  aBuf[8] := aPacks[Length(aPacks)-2];
  aBuf[9] := aPacks[Length(aPacks)-1];

  // 通讯记录太长只取头和尾记录
  mmo1.Lines.Add(FormatDateTime('hh:mm:ss:zzz', Now) + s + sIP +':' + IntToStr(nPort) + ' ' + BCDPacksToStr(aBuf) );
end;

procedure TfRevScreenMain.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    WriteInteger('OptionRevScreen', 'ListenPort', FRevScreen.ListenPort);
    WriteBool('OptionRevScreen', 'ShowLog',FShowLog);
    Free;
  end;
end;

end.
