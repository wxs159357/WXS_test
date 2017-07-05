unit TestMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uAbout,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm2 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btn1Click(Sender: TObject);
begin
  with TfAbout.Create(nil) do
  begin
    SetCopyRight('2000');
    SetWebSite('http://www.beiyanxing.com');
    SetCompanyName('北京易亨电子dfdfdfd仪表有限公司');
//    LoadPicture('d:1.bmp');
    SetVersion('');
    ShowCode2('d:\2.png');
    AddStrInfo('2234234234234');
    AddStrInfo('2234234234234');
    AddStrInfo('2234234234234');
    AddStrInfo('2234234234234');

    ShowAboutInfo('CKM-2008sdddddddddddddddddddddddddfsdfsdfsdfsdfD', '三相电能表检验管sdfsdfsdfsdfsdfsdfsdf理系统');
    Free;
  end;
end;

end.
