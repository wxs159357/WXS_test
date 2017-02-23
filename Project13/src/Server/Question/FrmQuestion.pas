unit FrmQuestion;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfQuestion = class(TForm)
    pnl1: TPanel;
    grp1: TGroupBox;
    lv1: TListView;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fQuestion: TfQuestion;

implementation

{$R *.dfm}

end.
