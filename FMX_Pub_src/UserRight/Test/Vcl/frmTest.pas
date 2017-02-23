unit frmTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uUserControl, xDBConn;

type
  TfMainTest = class(TForm)
    btn1: TButton;
    btn5: TButton;
    btn2: TButton;
    lbl2: TLabel;
    btn7: TButton;
    lbl1: TLabel;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMainTest: TfMainTest;

implementation

uses
  frmLogn, uUserInfo, frmUserOption, uUpUsPass;

{$R *.dfm}

procedure TfMainTest.btn1Click(Sender: TObject);
  procedure ShowInfo;
  var
    user : TUser;
  begin
    user := UserControl.UserInfo;

    with mmo1.Lines do
    begin
      Clear;
      Add( 'LoginName: ' + user.LoginName );
      Add( 'FullName: ' + user.FullName );
    end;
    btn2.Enabled := user.ChangePwd;
    btn7.Enabled := UserControl.RightExist( PChar( '用户帐号管理' ) );

  end;
begin
  with TfLogn.Create(nil) do
  begin
    if ShowModal = mrOk then
    begin
      ShowInfo;
    end
    else
    begin
      btn5Click(nil);
    end;
    free;
  end;
end;

procedure TfMainTest.btn2Click(Sender: TObject);
begin
  if Assigned(UserControl) then
  begin
    UserControl.UserUpass;
  end;
end;

procedure TfMainTest.btn5Click(Sender: TObject);
begin
  UserControl.LogOut;
  mmo1.Clear;
  btn7.Enabled := False;
  btn2.Enabled := False;
end;

procedure TfMainTest.btn7Click(Sender: TObject);
begin
  with TfUserOption.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMainTest.FormCreate(Sender: TObject);
const
  sConnStr = 'DriverID=MSAcc;Database=%s';
begin
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := Format(sConnStr, [ExtractFilePath( Application.ExeName ) + 'ua_test.mdb']);
  UserControl := TUserControl.Create;
end;

procedure TfMainTest.FormDestroy(Sender: TObject);
begin
  UserControl.Free;
  ADBConn.Free;
end;

procedure TfMainTest.FormShow(Sender: TObject);
begin
  btn5Click(nil);
end;

end.
