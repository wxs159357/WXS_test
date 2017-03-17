unit FrmExamineeList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, xExamControl, xStudentControl, xStudentInfo, uStudentList,
  System.ImageList, Vcl.ImgList, uStudentInfo;

type
  TfExamineeList = class(TForm)
    pnl1: TPanel;
    grpbx1: TGroupBox;
    lvStuList: TListView;
    btnadd: TButton;
    btnDel: TButton;
    actnmngr1: TActionManager;
    actadd: TAction;
    actDel: TAction;
    btn1: TButton;
    btn2: TButton;
    imglst1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure actaddExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure lvStuListDblClick(Sender: TObject);
  private
    { Private declarations }
    procedure RefurshList;
  public
    { Public declarations }
  end;

var
  fExamineeList: TfExamineeList;

implementation

{$R *.dfm}

{ TfExamineeList }

procedure TfExamineeList.actaddExecute(Sender: TObject);
var
  slList : TStringList;
  i : Integer;
  AStuInfo : TStudentInfo;
begin
  slList := TStringList.Create;
  with TfStudentList.Create(nil) do
  begin
    SelectStu(slList);


    for i := 0 to slList.Count - 1 do
    begin
      AStuInfo := TStudentInfo(slList.Objects[i]);

      if ExamControl.ExamStuList.IndexOf(IntToStr(AStuInfo.stuNumber)) = -1 then
      begin
        ExamControl.AddStu(AStuInfo);
      end;
    end;
  end;

  slList.Free;

  RefurshList;
end;

procedure TfExamineeList.actDelExecute(Sender: TObject);
var
  i: Integer;
  Astu: TStudentInfo;
  nIndex : Integer;
begin
  if lvStuList.ItemIndex = -1 then
  begin
    Application.MessageBox('请选择要删除的记录！','警告', MB_OK +MB_ICONINFORMATION);
    Exit;

  end;

  if Application.MessageBox('确定要删除所选记录吗？', '警告', MB_OKCANCEL +
    MB_ICONQUESTION) = IDOK then
  begin
    for I := lvStuList.Items.Count  - 1 downto 0 do
    begin
      if lvStuList.Items[I].Selected then
      begin
        Astu := TStudentInfo(lvStuList.Items[I].Data);
        nIndex := ExamControl.ExamStuList.IndexOf(IntToStr(Astu.stuNumber));

        if nIndex <> -1 then
        begin
          ExamControl.ExamStuList.Delete(nIndex);
        end;
      end;
    end;

    RefurshList;
  end;
end;

procedure TfExamineeList.FormCreate(Sender: TObject);
begin
  RefurshList;
end;

procedure TfExamineeList.lvStuListDblClick(Sender: TObject);
begin
  if Assigned(lvStuList.Selected) then
  begin
    with TfStudentInfo.Create(nil) do
    begin
      ShowStu(TStudentInfo(lvStuList.Selected.Data));
      ShowModal;
      free;
    end;
  end;
end;

procedure TfExamineeList.RefurshList;
var
  i : Integer;
begin
  lvStuList.Items.Clear;

  for i := 0 to ExamControl.ExamStuList.Count -1 do
  begin
    with lvStuList.Items.Add do
    begin
      Caption := TStudentInfo(ExamControl.ExamStuList.Objects[i]).stuName;

      Data := TStudentInfo(ExamControl.ExamStuList.Objects[i])
    end;
  end;
end;

end.
