unit uPaperInfoBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, xPaperAction;

type
  TfPaperInfoBase = class(TForm)
    pnl1: TPanel;
    btnCancel: TButton;
    btnSave: TButton;
    btnPrint: TButton;
    pnlMain: TPanel;
    lbl3: TLabel;
    edtScore: TEdit;
    lvQuestList: TListView;
    spltr1: TSplitter;
    lbl1: TLabel;
    procedure edtScoreKeyPress(Sender: TObject; var Key: Char);
  private
  { Private declarations }

  public
  { Public declarations }

    /// <summary>
    /// œ‘ æøºæÌ–≈œ¢
    /// </summary>
    procedure ShowPaperInfo( nPaperID, nStuID: Integer; dScore: Double );
  end;


implementation

{$R *.dfm}

procedure TfPaperInfoBase.edtScoreKeyPress(Sender: TObject; var Key: Char);
begin
  if not ( Ord(Key) in [8, 13, 45, 46, 48..57] ) then Key := #0;
end;

procedure TfPaperInfoBase.ShowPaperInfo(nPaperID, nStuID: Integer; dScore : Double);
begin
  edtScore.Text := FloatToStr(dScore);



  if ShowModal = mrOk then
  begin
    TryStrToFloat(edtScore.Text, dScore);
    APaperAction.EditStuScore(nPaperID, nStuID, dScore);
  end;
end;

end.
