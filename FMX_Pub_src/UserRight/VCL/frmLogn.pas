unit frmLogn;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg;

type
  TfLogn = class(TForm)
    imgbanner: TImage;
    ImgLogin: TImage;
    lblUser: TLabel;
    lblPW: TLabel;
    edtPassWord: TEdit;
    edtUser: TEdit;
    Bevel1: TBevel;
    btnLogin: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fLogn: TfLogn;

implementation

uses
  uUserControl;

{$R *.dfm}

procedure TfLogn.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfLogn.btnLoginClick(Sender: TObject);
var
  i : Integer;
begin
  try
    i := Ord(UserControl.Login( PChar(edtUser.Text), PChar(edtPassWord.Text) ));
    case i of
      0: ShowMessage('用户不存在');
      1: ShowMessage('密码错误');
      2: ModalResult := mrOk;
    end;
  finally

  end;
end;

end.
