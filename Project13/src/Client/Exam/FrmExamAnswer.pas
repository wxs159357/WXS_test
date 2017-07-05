unit FrmExamAnswer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  xDataDictionary, xQuestionInfo, xSortControl, uQuestionList, xClientControl;

type
  TfExamAnswer = class(TForm)
    pnl1: TPanel;
    pnl2: TPanel;
    spltr1: TSplitter;
    lvSubList: TListView;
    pnlLastTime: TPanel;
    pnl3: TPanel;
    btnPostPaper: TButton;
    tmr1: TTimer;
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure RefurshQuestionList;

  public
    { Public declarations }
    procedure StartExam(Sender: TObject);
    procedure StopExam(Sender: TObject);

  end;

var
  fExamAnswer: TfExamAnswer;

implementation

{$R *.dfm}

{ TfExamAnswer }

procedure TfExamAnswer.RefurshQuestionList;
var
  i : Integer;
  AQInfo : TQuestionInfo;
  nID : Integer;
begin
  lvSubList.Clear;

  for i := 0 to ClientControl.SubList.Count - 1 do
  begin
    TryStrToInt(ClientControl.SubList[i], nID);
    AQInfo := SortControl.GetQInfo(nID);

    if Assigned(AQInfo) then
    begin
      with lvSubList.Items.Add do
      begin
        Caption := IntToStr(AQInfo.QID);
        SubItems.Add(AQInfo.QName);
        Data := AQInfo;
      end;
    end;
  end;
end;

procedure TfExamAnswer.StartExam(Sender: TObject);
begin
  RefurshQuestionList;
  tmr1Timer(nil);
  tmr1.Enabled := True;
end;

procedure TfExamAnswer.StopExam(Sender: TObject);
begin
  tmr1.Enabled := False;
  ShowMessage('øº ‘Ω· ¯');
end;

procedure TfExamAnswer.tmr1Timer(Sender: TObject);
var
  dtTime : TDateTime;
begin
  dtTime := ClientControl.StartTime + ClientControl.ExamTimes/MinsPerDay - now;

  pnlLastTime.Caption := FormatDateTime('hh:mm:ss', dtTime);
end;

end.
