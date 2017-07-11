unit FrmTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, xSimpleInfoControl, FrmSimpleInfoList, xDBConn,
  Vcl.StdCtrls;

type
  TfTestMain = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTestMain: TfTestMain;

implementation

{$R *.dfm}

procedure TfTestMain.btn1Click(Sender: TObject);
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfTestMain.btn2Click(Sender: TObject);
var
  AInfo : TSimpleInfo;
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    AInfo := SelectInfo;

    if Assigned(AInfo) then
    begin
      ShowMessage('选择到的信息为' + AInfo.SIName);
    end
    else
    begin
      ShowMessage('没有选择信息');
    end;
    Free;
  end;
end;

procedure TfTestMain.FormCreate(Sender: TObject);
begin

  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=TS_C04;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';

  ASimpleInfoControl := TSimpleInfoControl.Create('SimpleInfo');
  ASimpleInfoControl.SimpleInfoName := '考试信息'
end;

procedure TfTestMain.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
  ASimpleInfoControl.Free;
end;

end.
