unit frmUpUserPass;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  uUserInfo;

type
  TfUpUserPass = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    edtConfirm: TEdit;
    lblConfirm: TLabel;
    edtNew: TEdit;
    lblNew: TLabel;
    Bevel1: TBevel;
    lblOld: TLabel;
    edtOld: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FUser : TUser;
  public
    { Public declarations }
    procedure ShowInfo(AUser : TUser);
    procedure SaveInfo;
  end;

var
  fUpUserPass: TfUpUserPass;

implementation

uses
  xFunction;

{$R *.dfm}

{ TfUpUserPass }

procedure TfUpUserPass.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfUpUserPass.btnOKClick(Sender: TObject);
begin
  if Assigned(FUser) then
  begin
    if UpperCase(GetMD5(Trim(edtOld.Text))) <> UpperCase(FUser.Password) then
    begin
      MessageBox(0, '旧密码输入错误，请重新输入!', '提示', MB_OK);
      edtOld.SetFocus;
    end
    else
    begin
      if Trim(edtNew.Text) <> Trim(edtConfirm.Text) then
      begin
        MessageBox(0, '两次输入的密码不一致，请重新输入!', '提示', MB_OK);
        edtNew.SetFocus;
      end
      else
      begin
        ModalResult := mrOk;
      end;
    end;
  end;
end;

procedure TfUpUserPass.SaveInfo;
begin
  if Assigned(FUser) then
  begin
    FUser.Password := UpperCase(GetMD5(Trim(edtNew.Text)));
  end;
end;

procedure TfUpUserPass.ShowInfo(AUser: TUser);
begin
  FUser := AUser;
end;

end.
