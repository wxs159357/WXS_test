unit uMainTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TfMainTest = class(TForm)
    btnLogn: TButton;
    btnOutLogn: TButton;
    btnUpPass: TButton;
    btnUserGroup: TButton;
    mmoUser: TMemo;
    Label1: TLabel;
    procedure btnLognClick(Sender: TObject);
    procedure btnOutLognClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnUserGroupClick(Sender: TObject);
    procedure btnUpPassClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMainTest: TfMainTest;

implementation

uses
  uUserInfo, uUserControl, uLogn, xDBConn, uUserOption, uUpUsPass,
  FMX.DialogService;

{$R *.fmx}

procedure TfMainTest.btnLognClick(Sender: TObject);
procedure ShowInfo;
  var
    user : TUser;
  begin
    user := UserControl.UserInfo;

    with mmoUser.Lines do
    begin
      Clear;
      Add( 'LoginName: ' + user.LoginName );
      Add( 'FullName: ' + user.FullName );
    end;
    btnUpPass.Enabled := user.ChangePwd;
    btnUserGroup.Enabled := UserControl.RightExist( PChar( '用户帐号管理' ) );

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
      btnOutLognClick(nil);
    end;
    free;
  end;
end;

procedure TfMainTest.btnOutLognClick(Sender: TObject);
begin
  UserControl.LogOut;
  mmoUser.Lines.Clear;
  btnUpPass.Enabled := False;
  btnUserGroup.Enabled := False;
end;

procedure TfMainTest.btnUpPassClick(Sender: TObject);
begin
  if Assigned(UserControl) then
  begin
    UserControl.UserUpass;
  end;
end;

procedure TfMainTest.btnUserGroupClick(Sender: TObject);
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
  ADBConn.ConnStr := Format(sConnStr, [ExtractFilePath(ParamStr(0)) + 'ua_test.mdb']);
  UserControl := TUserControl.Create;
end;

procedure TfMainTest.FormDestroy(Sender: TObject);
begin
  UserControl.Free;
  ADBConn.Free;
end;

procedure TfMainTest.FormShow(Sender: TObject);
begin
  btnOutLognClick(nil);
end;

end.
