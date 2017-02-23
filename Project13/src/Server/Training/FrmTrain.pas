unit FrmTrain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ImgList, System.ImageList;

type
  TfTrain = class(TForm)
    pnl1: TPanel;
    pnl2: TPanel;
    spltr1: TSplitter;
    grp1: TGroupBox;
    grp2: TGroupBox;
    stsbr1: TStatusBar;
    pnl4: TPanel;
    btn1: TButton;
    pnl3: TPanel;
    btn2: TButton;
    btn3: TButton;
    tvDirectory: TTreeView;
    btn4: TButton;
    btn5: TButton;
    pgcntrl1: TPageControl;
    tbsht1: TTabSheet;
    tbsht2: TTabSheet;
    btn6: TButton;
    imglstil1: TImageList;
    lvStudents: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTrain: TfTrain;

implementation

{$R *.dfm}

end.
