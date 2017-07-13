unit xTestMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Menus, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.TMSListView,
  xSimpleInfoControl, uSimpleInfoList, xDBConn, FMX.StdCtrls;

type
  TForm8 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.btn1Click(Sender: TObject);
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    SimpleInfoName := '题库';
    SimpleInfoDBName := 'SimpleInfo';
    ShowModal;
    Free;
  end;
end;

procedure TForm8.btn2Click(Sender: TObject);
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

procedure TForm8.btn3Click(Sender: TObject);
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

procedure TForm8.btn4Click(Sender: TObject);
begin
  with TfSimpleInfoList.Create(nil) do
  begin
    SimpleInfoName := '章节';
    SimpleInfoDBName := 'SimpleInfo2';
    ShowModal;
    Free;
  end;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin

  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=TS_C04;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';


end;

procedure TForm8.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
end;

end.
