unit FrmExercise;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ImgList, Vcl.ComCtrls,
  Vcl.StdCtrls, System.ImageList, xExerciseControl, xExerciseInfo,
  System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan,
  Vcl.Menus, FrmQuestionListC, xQuestionInfo, FrmQInfoC, Vcl.Buttons;

type
  TfExercise = class(TForm)
    grp2: TGroupBox;
    lvExercises: TListView;
    imglstil1: TImageList;
    actnmngr1: TActionManager;
    actAddPath: TAction;
    pmn1: TPopupMenu;
    actReName: TAction;
    mntmReName: TMenuItem;
    actSelectQuestion: TAction;
    actUpPath: TAction;
    btnUpPath: TSpeedButton;
    actDel: TAction;
    mntmDel: TMenuItem;
    mntmAddPath: TMenuItem;
    mntmSelectQuestion: TMenuItem;
    mntmN1: TMenuItem;
    procedure actAddPathExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actSelectQuestionExecute(Sender: TObject);
    procedure lvExercisesDblClick(Sender: TObject);
    procedure actUpPathExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure lvExercisesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure pmn1Popup(Sender: TObject);
  private
    { Private declarations }
    FQuestionList : TfQuestionListC;
    FIsClientExercise: Boolean;
    /// <summary>
    /// 刷新目录
    /// </summary>
    procedure RefurshExercise;

    /// <summary>
    /// 进入答题环境
    /// </summary>
    procedure AnswerQuestion(AQuesiton : TQuestionInfo);
  public
    { Public declarations }
    /// <summary>
    /// 是否是客户端练习模式
    /// </summary>
    property IsClientExercise : Boolean read FIsClientExercise write FIsClientExercise;
  end;

var
  fExercise: TfExercise;

implementation

{$R *.dfm}

{ TfExercise }

procedure TfExercise.actAddPathExecute(Sender: TObject);
var
  sName : string;
  AExerciseInfo : TExerciseInfo;
begin
  sName := InputBox('输入','目录名称', '新建目录');

  if not ExerciseControl.IsExist(sName) then
  begin
    AExerciseInfo := TExerciseInfo.Create;
    AExerciseInfo.Imageindex := 0;
    AExerciseInfo.Path := ExerciseControl.CurrentPath;
    AExerciseInfo.Ename := sName;

    ExerciseControl.AddExercise(AExerciseInfo);

    RefurshExercise;
  end
  else
  begin
    Application.MessageBox('目录已存在，请重新建立目录！', '', MB_OK + MB_ICONINFORMATION);
  end;

end;

procedure TfExercise.actDelExecute(Sender: TObject);
var
  i : Integer;
  AExerciseInfo : TExerciseInfo;
begin
  for i := 0 to lvExercises.Items.Count - 1 do
  begin
    if lvExercises.Items[i].Selected then
    begin
      AExerciseInfo := TExerciseInfo(lvExercises.Items[i].Data);
      ExerciseControl.DelExercise(AExerciseInfo, True);
    end;
  end;

  ExerciseControl.LoadExercise(ExerciseControl.CurrentPath);
  RefurshExercise;
end;

procedure TfExercise.actSelectQuestionExecute(Sender: TObject);
var
  AQInfo : TQuestionInfo;
  AExerciseInfo : TExerciseInfo;
begin
  AQInfo := FQuestionList.SelOneQuestion;

  if Assigned(AQInfo) then
  begin
    if not ExerciseControl.IsExist(AQInfo.QName) then
    begin
      AExerciseInfo := TExerciseInfo.Create;
      AExerciseInfo.Imageindex := 1;
      AExerciseInfo.Ptype := 1;
      AExerciseInfo.Path := ExerciseControl.CurrentPath;
      AExerciseInfo.Ename := AQInfo.QName;
      AExerciseInfo.Code1 := AQInfo.QCode;
      AExerciseInfo.Code2 := AQInfo.QRemark2;

      ExerciseControl.AddExercise(AExerciseInfo);
    end
    else
    begin
      Application.MessageBox('习题已经存在，请重新选择！', '', MB_OK + MB_ICONINFORMATION);
    end;

    RefurshExercise;
  end;

end;

procedure TfExercise.actUpPathExecute(Sender: TObject);
begin
  ExerciseControl.LoadPreviousPath;
  RefurshExercise;
end;

procedure TfExercise.AnswerQuestion(AQuesiton: TQuestionInfo);
begin

end;

procedure TfExercise.FormCreate(Sender: TObject);
begin
  FQuestionList := TfQuestionListC.Create(nil);
  FIsClientExercise := False;
  ExerciseControl.LoadExercise();
  RefurshExercise;
end;

procedure TfExercise.FormDestroy(Sender: TObject);
begin
  FQuestionList.Free;
end;

procedure TfExercise.lvExercisesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if FIsClientExercise then
  begin
    lvExercises.PopupMenu := nil;
    Handled := False;
  end
  else
  begin
    if Assigned(lvExercises.Selected) then
    begin
      actReName.Enabled := TExerciseInfo(lvExercises.Selected.Data).Ptype = 0
    end;
  end;

end;

procedure TfExercise.lvExercisesDblClick(Sender: TObject);
var
  AExerciseInfo : TExerciseInfo;
  AQInfo : TQuestionInfo;
begin
  if Assigned(lvExercises.Selected) then
  begin
    AExerciseInfo := TExerciseInfo(lvExercises.Selected.Data);

    case AExerciseInfo.Ptype of
      0 :
      begin
        ExerciseControl.LoadExercise(ExerciseControl.CurrentPath + '\' + AExerciseInfo.Ename);
        RefurshExercise;
      end;
      1 :
      begin
        AQInfo := TQuestionInfo.Create;
        AQInfo.QCode := AExerciseInfo.Code1;
        AQInfo.QName := AExerciseInfo.Ename;
        AQInfo.QRemark2 := AExerciseInfo.Code2;

        if FIsClientExercise then
        begin
          AnswerQuestion(AQInfo);
        end
        else
        begin
          with TfQInfo.Create(nil) do
          begin
            ShowInfoReadOnly(AQInfo);
            ShowModal;
          end;
        end;


        AQInfo.Free;
      end;
    end;
  end;
end;

procedure TfExercise.pmn1Popup(Sender: TObject);
begin
  Caption := '1';
end;

procedure TfExercise.RefurshExercise;
var
  i : Integer;
  AExerciseInfo : TExerciseInfo;
begin
  lvExercises.Clear;

  for i := 0 to ExerciseControl.ExerciseList.Count - 1 do
  begin
    AExerciseInfo := ExerciseControl.ExerciseInfo[i];

    if Assigned(AExerciseInfo) then
    begin
      with lvExercises.Items.Add do
      begin
        Caption := AExerciseInfo.Ename;
        ImageIndex := AExerciseInfo.Imageindex;
        Data := AExerciseInfo;
      end;
    end;
  end;

  actUpPath.Enabled := ExerciseControl.CurrentPath <> '';
end;

end.
