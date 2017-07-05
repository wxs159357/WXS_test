unit FrmLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,IniFiles, xFunction, xConsts,
  xStudentAction, xStudentInfo, xClientControl, xClientType, xTCPClient,
  xUDPClient1;

const
  /// <summary>
  /// 客户端登陆超时时间
  /// </summary>
  C_LOGIN_TIMEOUT  = 1500;

type
  TfStudentLogin = class(TForm)
    lbl2: TLabel;
    edtName: TEdit;
    lbl3: TLabel;
    edtPassword: TEdit;
    btnLogin: TBitBtn;
    bvl1: TBevel;
    btnCancel: TButton;
    chk1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
    FStuAction : TStudentAction;
    procedure ReadINI;
    procedure WriteINI;

  public
    { Public declarations }
  end;

var
  fStudentLogin: TfStudentLogin;

implementation

{$R *.dfm}

procedure TfStudentLogin.btnLoginClick(Sender: TObject);
var
  sIP : string;
  AStudentInfo : TStudentInfo;
begin
  if edtName.Text = '' then
  begin
    Application.MessageBox(PChar(lbl2.Caption + '不能为空！'), '',
      MB_OK + MB_ICONINFORMATION);
    edtName.SetFocus;
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  btnLogin.Enabled := False;

  try
    AStudentInfo := TStudentInfo.create;

    if FStuAction.GetStuInfo(Trim(edtName.Text), Trim(edtPassword.Text), AStudentInfo) then
    begin

      sIP := UDPClient.CheckLogin(AStudentInfo.stuNumber);

      // 本学员已经在其他服务器上登录
      if sIP <> '' then
      begin
        Application.MessageBox(PChar('用户已经在' + sIP + '上登录，不允许重复登录！'),
         '', MB_OK + MB_ICONINFORMATION);
      end
      else
      begin
        ClientControl.StudentInfo.Assign(AStudentInfo);

        ClientControl.LoginState := lsLogin;

        ModalResult := mrOk;
      end;
    end
    else
    begin
      Application.MessageBox(PChar('用户名或密码错误！'),
         '', MB_OK + MB_ICONINFORMATION);
    end;
    AStudentInfo.Free;
  finally
    Screen.Cursor := crDefault;
    btnLogin.Enabled := True;
  end;
end;

procedure TfStudentLogin.FormCreate(Sender: TObject);
begin
  FStuAction := TStudentAction.Create;
  ReadINI;
end;

procedure TfStudentLogin.FormDestroy(Sender: TObject);
begin
  FStuAction.free;

  WriteINI;
end;

procedure TfStudentLogin.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    edtName.Text           := ReadString('Client', 'LogName', '');
    edtPassword.Text       := '';
    chk1.Checked := ReadBool('Option', 'InShowLoagin', True);
    Free;
  end;
end;

procedure TfStudentLogin.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    WriteString( 'Client', 'LogName', edtName.Text);

    WriteBool('Option', 'InShowLoagin', chk1.Checked);
    Free;
  end;
end;

end.

