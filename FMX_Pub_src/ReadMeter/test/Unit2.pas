unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, xDL645Thread,
  xThreadBase, xTCPClientBase, FMX.Controls.Presentation, FMX.StdCtrls,
  xProtocolPacksDl645;

type
  TForm2 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
    FReadMeterThread : TDL645Thread;

    procedure RevData( A645data : TStringList );
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btn1Click(Sender: TObject);
begin
  FReadMeterThread.ReadMeterAddr;
end;

procedure TForm2.btn2Click(Sender: TObject);
begin
  FReadMeterThread.ReadMeterData(0, 6, '000000.00');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FReadMeterThread := TDL645Thread.Create(False);
  FReadMeterThread.ProType := ctTCPClient;
  FReadMeterThread.OnRev645Data := RevData;

  TTCPClientBase(FReadMeterThread.CommBase).ServerIP := '127.0.0.1';
  TTCPClientBase(FReadMeterThread.CommBase).ServerPort := 10001;
  FReadMeterThread.CommBase.Connect;

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FReadMeterThread.Free;
end;

procedure TForm2.RevData(A645data: TStringList);
begin
  ShowMessage('');
end;

end.
