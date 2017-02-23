unit FrmLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,IniFiles, xFunction, xConsts,
  xStudentAction, xStudentInfo, xClientControl, xClientType, xTCPClient;

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
begin
  if edtName.Text = '' then
  begin
    Application.MessageBox(PChar(lbl2.Caption + '不能为空！'), '',
      MB_OK + MB_ICONINFORMATION);
    edtName.SetFocus;
    Exit;
  end;

  if FStuAction.GetStuInfo(Trim(edtName.Text), Trim(edtPassword.Text),
    ClientControl.StudentInfo) then
  begin
    TCPClient.StuLogin(ClientControl.StudentInfo.stuNumber);
//    if  then
//    begin
      ClientControl.ClientState := esLogin;
      ModalResult := mrOk;
//    end
//    else
//    begin
//      Application.MessageBox(PChar('服务器不允许登录，可能不在考试列表中，请联系老师！'),
//       '', MB_OK + MB_ICONINFORMATION);
//    end;
  end
  else
  begin
    Application.MessageBox(PChar('用户名或密码错误！'),
       '', MB_OK + MB_ICONINFORMATION);
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

