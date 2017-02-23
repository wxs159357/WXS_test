unit TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uSortList, uQuestionList,
  xQuestionInfo;

type
  TfTestMain = class(TForm)
    btn1: TButton;
    btn2: TButton;
    mmo1: TMemo;
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
  with TfQuestionList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfTestMain.btn2Click(Sender: TObject);
var
  AInfo : TQuestionInfo;
begin
  with TfQuestionList.Create(nil) do
  begin
    AInfo := SelOneQuestion;
    if Assigned(AInfo) then
    begin
      mmo1.Lines.Add('考题编号：' + IntToStr(AInfo.QID) + #13#10 +
                     '考题名称：' + AInfo.QName + #13#10 +
                     '考题描述：' + AInfo.QDescribe + #13#10 +
                     '考题备注：' + AInfo.QRemark1 + #13#10
                     );
    end;
    Free;
  end;
end;

end.
