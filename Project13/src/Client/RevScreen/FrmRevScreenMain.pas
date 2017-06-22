unit FrmRevScreenMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, xUDPRevScreen, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, Vcl.StdCtrls, xFunction, System.IniFiles, xConsts;

type
  TfRevScreenMain = class(TForm)
    img1: TImage;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FRevScreen : TUDPRevScreen;
    FShowLog : Boolean;
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
end;
procedure TfRevScreenMain.FormDestroy(Sender: TObject);
begin
  WriteINI;
  FRevScreen.Free;
end;

procedure TfRevScreenMain.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    FRevScreen.ListenPort := ReadInteger('Option', 'ListenPort', 16101);
    Caption := '接收屏幕端口：' + IntToStr(FRevScreen.ListenPort);

    FShowLog := ReadBool('Option', 'ShowLog', False);
    Free;
  end;
end;

procedure TfRevScreenMain.RevJpg(Sender: TObject);
begin
  if Sender is TJPEGImage then
  begin
    mmo1.Lines.Add('刷新图片' + IntToStr(TJPEGImage(Sender).Width) + ';' + IntToStr(TJPEGImage(Sender).Height));

    Self.Width := TJPEGImage(Sender).Width-11;
    Self.Height := TJPEGImage(Sender).Height-31;

    img1.Picture.Bitmap.Assign(TJPEGImage(Sender));
  end;
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
    WriteInteger('Option', 'ListenPort', FRevScreen.ListenPort);
    WriteBool('Option', 'ShowLog',FShowLog);
    Free;
  end;
end;

end.
