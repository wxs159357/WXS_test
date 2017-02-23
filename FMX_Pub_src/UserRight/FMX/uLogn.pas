unit uLogn;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TfLogn = class(TForm)
    Image1: TImage;
    Image2: TImage;
    lbl1: TLabel;
    Label1: TLabel;
    edtUser: TEdit;
    edtPassWord: TEdit;
    Line1: TLine;
    btnOk: TButton;
    btnCancle: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

{$R *.fmx}

procedure TfLogn.btnCancleClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfLogn.btnOkClick(Sender: TObject);
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

procedure TfLogn.FormCreate(Sender: TObject);
begin
  edtUser.SetFocus;
end;

end.
