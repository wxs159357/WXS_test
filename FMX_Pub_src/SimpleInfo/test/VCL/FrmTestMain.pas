unit FrmTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, xSimpleInfoControl, uSimpleInfoList, xDBConn,
  Vcl.StdCtrls;

type
  TfTestMain = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
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
    SimpleInfoName := '题库';
    SimpleInfoDBName := 'SimpleInfo';
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
    SimpleInfoName := '题库';
    SimpleInfoDBName := 'SimpleInfo';

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

procedure TfTestMain.btn3Click(Sender: TObject);
var
  AInfo : TSimpleInfo;
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    SimpleInfoName := '章节';
    SimpleInfoDBName := 'SimpleInfo2';

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

procedure TfTestMain.btn4Click(Sender: TObject);
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    SimpleInfoName := '章节';
    SimpleInfoDBName := 'SimpleInfo2';
    ShowModal;
    Free;
  end;
end;

procedure TfTestMain.FormCreate(Sender: TObject);
begin

  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=TS_C04;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';


end;

procedure TfTestMain.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
end;

end.
