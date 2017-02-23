unit uUpUsPass;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, uUserInfo;

type
  TfUpUsPass = class(TForm)
    edtConfirm: TEdit;
    Label4: TLabel;
    edtOld: TEdit;
    edtNew: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    btn1: TButton;
    Button1: TButton;
    Line1: TLine;
    procedure Button1Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FUser : TUser;
  public
    { Public declarations }
    procedure ShowInfo(AUser : TUser);
    procedure SaveInfo;
  end;

var
  fUpUsPass: TfUpUsPass;

implementation

uses
  xFunction, FMX.DialogService;

{$R *.fmx}

procedure TfUpUsPass.btn1Click(Sender: TObject);
begin
  if Assigned(FUser) then
  begin
    if UpperCase(GetMD5(Trim(edtOld.Text))) <> UpperCase(FUser.Password) then
    begin
      TDialogService.MessageDialog('旧密码输入错误，请重新输入!',TMsgDlgType.mtInformation,
                                    [], TMsgDlgBtn.mbOK, 0, nil);
      edtOld.SetFocus;
    end
    else
    begin
      if Trim(edtNew.Text) <> Trim(edtConfirm.Text) then
      begin
        TDialogService.MessageDialog('两次输入的密码不一致，请重新输入!',TMsgDlgType.mtInformation,
                                       [], TMsgDlgBtn.mbOK, 0, nil);
        edtNew.SetFocus;
      end
      else
      begin
        ModalResult := mrOk;
      end;
    end;
  end;
end;

procedure TfUpUsPass.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfUpUsPass.SaveInfo;
begin
  if Assigned(FUser) then
  begin
    FUser.Password := UpperCase(GetMD5(Trim(edtNew.Text)));
  end;
end;

procedure TfUpUsPass.ShowInfo(AUser: TUser);
begin
  FUser := AUser;
end;

end.
