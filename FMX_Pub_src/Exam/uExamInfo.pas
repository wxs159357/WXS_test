unit uExamInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfExamInfo = class(TForm)
    edtExamName: TLabeledEdit;
    edtTime: TLabeledEdit;
    lbl1: TLabel;
    pnl1: TPanel;
    btn1: TButton;
    btn2: TButton;
    bvl1: TBevel;
    procedure edtTimeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EditInfo(var sExamName : string; var nExamTime: Integer);
  end;

var
  fExamInfo: TfExamInfo;

implementation

{$R *.dfm}

{ TfExamInfo }

procedure TfExamInfo.EditInfo(var sExamName: string; var nExamTime: Integer);
begin
  edtExamName.Text := sExamName;
  edtTime.Text := IntToStr(nExamTime);

  if ShowModal = mrOk then
  begin
    sExamName := edtExamName.Text;
    TryStrToInt(edtTime.Text, nExamTime);
  end;
end;

procedure TfExamInfo.edtTimeKeyPress(Sender: TObject; var Key: Char);
begin
  if not ( Ord(Key) in [8, 13, 45, 48..57] ) then
    Key := #0;
end;

end.
