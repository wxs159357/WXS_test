unit uStudentInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,xStudentInfo, Buttons, ExtCtrls;

type
  TfStudentInfo = class(TForm)
    lbl5: TLabel;
    btn1: TBitBtn;
    btn2: TBitBtn;
    pnlStuInfo: TPanel;
    edtName: TEdit;
    edtLogin: TEdit;
    edtPassWord: TEdit;
    edtTel: TEdit;
    edtIDcard: TEdit;
    mmoRemark: TMemo;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    cbbArea: TComboBox;
    rbMan: TRadioButton;
    rbWoMan: TRadioButton;
    lbl10: TLabel;
    lbl11: TLabel;

    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FStuInfo : TStudentInfo;

    /// <summary>
    /// 检查界面信息
    /// </summary>
    function CheckStuInfo : Boolean;
  public
    { Public declarations }
    /// <summary>
    /// 将学员信息显示出来
    /// </summary>
    procedure ShowStu(AStu : TStudentInfo);

    /// <summary>
    /// 将新加的或修改的学员信息保存
    /// </summary>
    procedure SaveStu;

  end;

implementation

uses
  xStudentControl, xFunction;

{$R *.dfm}

procedure TfStudentInfo.ShowStu(AStu : TStudentInfo);
var
  i: Integer;
begin
  FStuInfo := AStu;

  if Assigned(FStuInfo) then
  begin
    if FStuInfo.stuSex = '女' then
      rbWoman.Checked := True
    else
      rbMan.Checked := True;

    if Assigned(StudentControl) then
    begin
      for i := 0 to StudentControl.StuList.Count - 1 do
      begin
        if  cbbArea.Items.IndexOf(TStudentInfo(StudentControl.StuList.Objects[i]).stuArea) = -1 then
          cbbArea.Items.Add(TStudentInfo(StudentControl.StuList.Objects[i]).stuArea);
      end;
    end;


    with FStuInfo do
    begin
      edtName.Text         := stuName;
      edtIDcard.Text       := stuIDcard;
      edtLogin.Text        := stuLogin;
      edtPassword.Text     := stuPwd;
      cbbArea.Text         := stuArea;
      edtTel.Text          := stuTel;
      mmoRemark.Text       := stuNote1;
    end;
  end;

end;

procedure TfStudentInfo.SaveStu;
begin

  if Assigned(FStuInfo) then
  begin
    if rbMan.Checked then
      FStuInfo.stuSex := '男'
    else
      FStuInfo.stuSex := '女';
    with FStuInfo do
    begin
      stuName              :=  edtName.Text;
      stuIDcard            :=  edtIDcard.Text;
      stuLogin             :=  edtLogin.Text;
      stuPwd          :=  edtPassword.Text;
      stuArea              :=  cbbArea.Text;
      stuTel               :=  edtTel.Text;
      stuNote1           :=  mmoRemark.Text;
      //stuRemark2           :=  edtRemark2.Text;
    end;
  end;

end;

procedure TfStudentInfo.btn1Click(Sender: TObject);
begin

  if CheckStuInfo then
    ModalResult := mrOk
  else
  begin
    Application.MessageBox('*必填项不能为空', '警告', MB_OK +
    MB_ICONINFORMATION);
    ModalResult := mrCancel;
  end;

end;

function TfStudentInfo.CheckStuInfo: Boolean;
begin

  if edtName.Text ='' then
    Result := false
  else
    Result := True;

end;


end.



