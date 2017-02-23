unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uAbout,
  FMX.Controls.Presentation, FMX.StdCtrls, xConsts, xDBConn, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.ScrollBox, FMX.Memo,
  xDBUpdate,  xSerialBase, xTypes, xFunction, xTCPClientBase,xTCPServerBase,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext, IdGlobal,

{$IFDEF MSWINDOWS}
  SPComm,
{$ENDIF}
  IdUDPServer, IdUDPBase, IdUDPClient, IdSocketHandle, xUDPServerBase,
  xExceptionCatch, xProtocolDev, FMX.ListBox, FMX.DateTimeCtrls, xHttpServer;

type
  TForm2 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    fdqry1: TFDQuery;
    btn3: TButton;
    mmo1: TMemo;
    btn4: TButton;
    grpbx1: TGroupBox;
    btn5: TButton;
    btn6: TButton;
    btn7: TButton;
    grpbx2: TGroupBox;
    btn8: TButton;
    btn9: TButton;
    btn10: TButton;
    grpbx3: TGroupBox;
    btn11: TButton;
    btn12: TButton;
    btn13: TButton;
    grpbx4: TGroupBox;
    btn15: TButton;
    btn16: TButton;
    btn17: TButton;
    tmr1: TTimer;
    btn14: TButton;
    grpbx5: TGroupBox;
    btn18: TButton;
    btn19: TButton;
    btn20: TButton;
    btn21: TButton;
    cbb1: TComboBox;
    tmdt1: TTimeEdit;
    tmdt2: TTimeEdit;
    grpbx6: TGroupBox;
    btn22: TButton;
    btn23: TButton;
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure btn8Click(Sender: TObject);
    procedure btn9Click(Sender: TObject);
    procedure btn10Click(Sender: TObject);
    procedure btn11Click(Sender: TObject);
    procedure btn12Click(Sender: TObject);
    procedure btn13Click(Sender: TObject);
    procedure idpsrvr1UDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure btn15Click(Sender: TObject);
    procedure btn16Click(Sender: TObject);
    procedure btn17Click(Sender: TObject);
    procedure btn14Click(Sender: TObject);
    procedure btn18Click(Sender: TObject);
    procedure btn19Click(Sender: TObject);
    procedure btn20Click(Sender: TObject);
    procedure btn21Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btn22Click(Sender: TObject);
    procedure btn23Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FComm : TSerialBase;
    FTCPClent : TTCPClientBase;
    FTCPServer : TTCPServerBase;
    FUDPServer : TUDPServerBase;

    FDevTimer : TPDTimer;

    FHS : THttpServer;

    procedure SecLeft(nSecLeft: Integer);
    procedure SendRev( aPacks: TArray<Byte>; bSend : Boolean);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btn10Click(Sender: TObject);
begin
  FTCPClent.SendPacksData('123123');
end;

procedure TForm2.btn11Click(Sender: TObject);
begin
  FTCPServer.Connect;
end;

procedure TForm2.btn12Click(Sender: TObject);
begin
  FTCPServer.Disconnect;
end;

procedure TForm2.btn13Click(Sender: TObject);
begin
  FTCPServer.SendPacksData('456567')
end;

procedure TForm2.btn14Click(Sender: TObject);
begin
  MsgException(ClassName, mtWARNING, '提示测试');
end;

procedure TForm2.btn15Click(Sender: TObject);
begin
  FUDPServer.Connect;
end;

procedure TForm2.btn16Click(Sender: TObject);
begin
  FUDPServer.Disconnect;
end;

procedure TForm2.btn17Click(Sender: TObject);
begin
  FUDPServer.SendPacksData( 'GetTCPServerIPPort')
end;

procedure TForm2.btn18Click(Sender: TObject);
begin
  FDevTimer.CurrentTime := tmdt2.Time;
  FDevTimer.WorkType := TWorkTimerType(cbb1.ItemIndex);

  FDevTimer.Enabled := True;
end;

procedure TForm2.btn19Click(Sender: TObject);
begin
  FDevTimer.Enabled := False;
end;

procedure TForm2.btn1Click(Sender: TObject);
begin
  with TFrmAbout.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TForm2.btn20Click(Sender: TObject);
begin
  FDevTimer.Paused := True;
end;

procedure TForm2.btn21Click(Sender: TObject);
begin
  FDevTimer.Paused := False;
end;

procedure TForm2.btn22Click(Sender: TObject);
begin
  FHS.Active := True;
end;

procedure TForm2.btn23Click(Sender: TObject);
begin
  FHS.Active := False;
end;

procedure TForm2.btn2Click(Sender: TObject);
begin
  showmessage(spubFilePath);
end;

procedure TForm2.btn3Click(Sender: TObject);
begin
  fdqry1.Connection := ADBConn.Connection;

  fdqry1.SQL.Text := 'SELECT * FROM E_QUESTION';

  fdqry1.Open;

  while not fdqry1.Eof do
  begin
    mmo1.Lines.Add(fdqry1.FieldByName('QName').AsString);
    fdqry1.Next;
  end;

end;

procedure TForm2.btn4Click(Sender: TObject);
begin
  with TDBUpdate.Create do
  begin
    Update;
    Free;
  end;
end;

procedure TForm2.btn5Click(Sender: TObject);
begin
  FComm.PortSN := 1;
  FComm.Connect;
end;

procedure TForm2.btn6Click(Sender: TObject);
begin
  FComm.Disconnect;
end;

procedure TForm2.btn7Click(Sender: TObject);
begin
  FComm.SendPacksData('1234');
end;

procedure TForm2.btn8Click(Sender: TObject);
begin
  FTCPClent.Connect;
end;

procedure TForm2.btn9Click(Sender: TObject);
begin
  FTCPClent.Disconnect;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSAcc;Database=C:\Temp\dbdemos.mdb';
  FComm := TSerialBase.Create;
  FComm.OnSendRevPack := SendRev;

  FTCPClent := TTCPClientBase.Create;
  FTCPClent.OnSendRevPack := SendRev;
  FTCPClent.ServerIP := '127.0.0.1';
  FTCPClent.ServerPort := 10000;

  FTCPServer := TTCPServerBase.Create;
  FTCPServer.OnSendRevPack := SendRev;

  FUDPServer := TUDPServerBase.Create;
  FUDPServer.OnSendRevPack := SendRev;

  FDevTimer := TPDTimer.Create;
  FDevTimer.OnSecLeft := SecLeft;

  FHS := THttpServer.Create;

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
  FComm.Free;
  FTCPClent.Free;
  FTCPServer.Free;
  FUDPServer.Free;
  FDevTimer.Free;
  FHS.Free;
end;

procedure TForm2.idpsrvr1UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  s : string;
  i : Integer;
begin
  s := '';

  for i := 0 to Length(AData) - 1 do
    s := s + Char(AData[i]);

  mmo1.Lines.Add(FormatDateTime('[hh:mm:ss] ', Now) + s);
end;

procedure TForm2.SecLeft(nSecLeft: Integer);
begin
  ShowMessage(IntToStr(nSecLeft));
end;

procedure TForm2.SendRev(aPacks: TArray<Byte>; bSend: Boolean);
var
  s : string;
begin
  if bSend then
    s := '发送:'
  else
    s := '接收:';

  mmo1.Lines.Add(FormatDateTime('[hh:mm:ss] ', Now) + s + PacksToStr(aPacks));
end;

procedure TForm2.tmr1Timer(Sender: TObject);
begin
  tmdt1.Time := FDevTimer.CurrentTime;
//  tmdt1.Text := FormatDateTime('hh:mm:ss', FDevTimer.CurrentTime );
end;

end.
