unit uStudentInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Memo, FMX.ListBox,
  FMX.ComboEdit, xStudentInfo, FMX.ScrollBox, FMX.DialogService,
  System.ImageList, FMX.ImgList;

type
  TFrmStudentInfo = class(TForm)
    pnl1: TPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel2: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    edtStuName: TEdit;
    edtStuLogin: TEdit;
    edtStuTel: TEdit;
    edtStuPWD: TEdit;
    edtStuIdCard: TEdit;
    mmoStuNote: TMemo;
    rbSex1: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    rbSex2: TRadioButton;
    edtStuArea: TComboEdit;
    lbl9: TLabel;
    il1: TImageList;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FStuInfo : TStudentInfo;
  public
    { Public declarations }
    /// <summary>
    /// 显示学员信息
    /// </summary>
    procedure ShowStu(AStu : TStudentInfo);

    /// <summary>
    /// 保存学员信息
    /// </summary>
    procedure SaveStu;
  end;

var
  FrmStudentInfo: TFrmStudentInfo;

implementation

uses
  xStudentControl, xFunction;

{$R *.fmx}

{ TFrmStudentInio }

procedure TFrmStudentInfo.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmStudentInfo.btnOKClick(Sender: TObject);
  function CheckStuInfo : Boolean;
  begin
    if edtStuName.Text = '' then
      Result := False
    else
      Result := True;
  end;
begin
  if CheckStuInfo then
  begin
    ModalResult := mrOk;
  end
  else
  begin
    if HintMessage(1, '*必填项不能为空!您确定取消吗?', '提示', hbtYesNo) = hrtYes then
    begin
      ModalResult := mrCancel;
    end
    else
    begin
      edtStuName.SetFocus;
    end;
  end;
end;

procedure TFrmStudentInfo.SaveStu;
begin
  if Assigned(FStuInfo) then
  begin
    with FStuInfo do
    begin
      stuName   := edtStuName.Text;

      if rbSex1.IsChecked then
        stuSex  := '男'
      else
        stuSex  := '女';

      stuLogin  := edtStuLogin.Text;
      stuPwd    := edtStuPWD.Text;
      stuArea   := edtStuArea.Text;
      stuIdCard := edtStuIdCard.Text;
      stuTel    := edtStuTel.Text;
      stuNote1  := mmoStuNote.Text;
    end;
  end;
end;

procedure TFrmStudentInfo.ShowStu(AStu: TStudentInfo);
var
  i : Integer;
  sStuArea : string;
begin
  FStuInfo := AStu;

  edtStuName.SetFocus;
  if Assigned(FStuInfo) then
  begin
    with FStuInfo do
    begin
      edtStuName.Text     := stuName;

      if stuSex = '女' then
        rbSex2.IsChecked  := True
      else
        rbSex1.IsChecked  := True;

      for i := 0 to StudentControl.StuList.Count - 1 do
      begin
        sStuArea := TStudentInfo(StudentControl.StuList.Objects[i]).stuArea;
        if edtStuArea.Items.IndexOf(sStuArea) = -1 then
        begin
          edtStuArea.Items.Add(sStuArea);
        end;
      end;

      edtStuLogin.Text    := stuLogin;
      edtStuPWD.Text      := stuPwd;
      edtStuArea.Text     := stuArea;
      edtStuIdCard.Text   := stuIdCard;
      edtStuTel.Text      := stuTel;
      mmoStuNote.Text     := stuNote1;
    end;
  end;
end;

end.
