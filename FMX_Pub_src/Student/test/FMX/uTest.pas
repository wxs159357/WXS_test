unit uTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  xStudentControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    mmo1: TMemo;
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
  Form2: TForm2;

implementation

uses
  xDBConn, uStudentList, xStudentInfo;

{$R *.fmx}

procedure TForm2.btn1Click(Sender: TObject);
begin
  with TfStudentList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TForm2.btn2Click(Sender: TObject);
var
  AStuInfo : TStudentInfo;
begin
  with TfStudentList.Create(nil) do
  begin
    AStuInfo := SelOneStu;
    if Assigned(AStuInfo) then
    begin
      mmo1.Lines.Clear;
      mmo1.Lines.Add('姓名:' + AStuInfo.stuName);
      mmo1.Lines.Add('性别:' + AStuInfo.stuSex);
    end;
    Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
const
  sConnStr = 'DriverID=MSAcc;Database=%s\UserDB.mdb';
var
  sFilePath : string;
begin
  sFilePath := ExtractFilePath(ParamStr(0));

  ADBConn := TDBConn.Create;
  //注意:64位系统下:  数据库文件不用全路径会报错(找不到文件)
  ADBConn.ConnStr := Format(sConnStr, [sFilePath]);

  StudentControl := TStudentControl.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
  StudentControl.Free;
end;

end.
