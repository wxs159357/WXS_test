unit testQ;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.TMSListView, FMX.Header,
  FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData,
  FMX.TMSCustomGrid, FMX.TMSGrid, uSortList, uQuestionList, FMX.Memo, xQuestionInfo,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TForm6 = class(TForm)
    btn2: TButton;
    btn1: TButton;
    mmo1: TMemo;
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.btn1Click(Sender: TObject);
var
  AInfo : TQuestionInfo;
begin
  with TfQuestionList.Create(nil) do
  begin
    AInfo := SelOneQuestion;
    if Assigned(AInfo) then
    begin
      mmo1.Lines.Add('øºÃ‚±‡∫≈£∫' + IntToStr(AInfo.QID) + #13#10 +
                     'øºÃ‚√˚≥∆£∫' + AInfo.QName + #13#10 +
                     'øºÃ‚√Ë ˆ£∫' + AInfo.QDescribe + #13#10 +
                     'øºÃ‚±∏◊¢£∫' + AInfo.QRemark1 + #13#10
                     );
    end;
    Free;
  end;
end;

procedure TForm6.btn2Click(Sender: TObject);
begin
  with TfQuestionList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

end.
