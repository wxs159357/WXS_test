unit FrmRanking;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TfRanking = class(TForm)
    pgcntrl1: TPageControl;
    tbsht1: TTabSheet;
    tbsht2: TTabSheet;
    tbsht3: TTabSheet;
    lv1: TListView;
    lv2: TListView;
    lv3: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fRanking: TfRanking;

implementation

{$R *.dfm}

end.
