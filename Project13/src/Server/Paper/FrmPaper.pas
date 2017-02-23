unit FrmPaper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.ComCtrls, Data.DB, System.ImageList;

type
  TfPaper = class(TForm)
    grp1: TGroupBox;
    pnl1: TPanel;
    spltr1: TSplitter;
    dbgrd1: TDBGrid;
    grp2: TGroupBox;
    grp3: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    chk1: TCheckBox;
    cbb1: TComboBox;
    cbb2: TComboBox;
    dtmpckr1: TDateTimePicker;
    dtmpckr2: TDateTimePicker;
    btn1: TButton;
    imglstil1: TImageList;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fPaper: TfPaper;

implementation

{$R *.dfm}

end.
