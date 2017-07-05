unit FrmExamReady;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, xClientControl;

type
  TfExamReady = class(TForm)
    prgrsbr1: TProgressBar;
    btnReady: TButton;
    lbl1: TLabel;
    procedure btnReadyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    /// <summary>
    /// ×¼±¸
    /// </summary>
    procedure ShowReady(nTotalCount : Integer);

    procedure ShowReadyProgress(nReadyCount, nTotalCount : Integer);
  end;

var
  fExamReady: TfExamReady;

implementation

{$R *.dfm}

{ TfExamReady }

procedure TfExamReady.btnReadyClick(Sender: TObject);
begin
  ClientControl.StuReadyExam;
  btnReady.Enabled := False;
end;

procedure TfExamReady.ShowReady(nTotalCount: Integer);
begin
  btnReady.Enabled := True;
  prgrsbr1.Max := nTotalCount;
  prgrsbr1.Position := 0;
end;

procedure TfExamReady.ShowReadyProgress(nReadyCount, nTotalCount: Integer);
begin
  prgrsbr1.Max := nTotalCount;
  prgrsbr1.Position := nReadyCount;
  lbl1.Caption := IntToStr(nReadyCount) + '/' + IntToStr(nTotalCount);
end;

end.
