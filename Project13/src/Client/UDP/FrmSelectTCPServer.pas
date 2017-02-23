unit FrmSelectTCPServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient;

procedure GetTCPIPPort(sIPPort : string; var sIP: string; var nPort : Integer);

type
  TfSelectTCPServer = class(TForm)
    lv1: TListView;
    pnl1: TPanel;
    lbl1: TLabel;
    pnl2: TPanel;
    btn1: TButton;
    btn2: TButton;
    idtcpclnt1: TIdTCPClient;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SelectTCPServer(slList : TStringList; var sIP: string; var nPort : Integer);
  end;

var
  fSelectTCPServer: TfSelectTCPServer;

implementation

{$R *.dfm}
procedure GetTCPIPPort(sIPPort : string; var sIP: string; var nPort : Integer);
var
  nIndex : Integer;
begin
  nIndex := Pos(',', sIPPort);

  if nIndex > 0 then
  begin
    sIP := Copy(sIPPort, 1, nIndex-1);
    TryStrToInt(Copy(sIPPort, nIndex+1, Length(sIPPort) - nIndex), nPort);
  end
  else
  begin
    sIP := '';
    nPort := 0;
  end;
end;

{ TfSelectTCPServer }

procedure TfSelectTCPServer.SelectTCPServer(slList: TStringList;
  var sIP: string; var nPort: Integer);
var
  i: Integer;

  sP : string;
  nP : Integer;
begin
  sIP := '';
  nPort := 0;
  if not Assigned(slList) then
    Exit;

  lv1.Items.Clear;

  for i := 0 to slList.Count - 1 do
  begin
    GetTCPIPPort(slList[i], sp, np);
    with lv1.Items.Add do
    begin
      Caption := IntToStr(i+1);
      SubItems.Add(sp);
      SubItems.Add(IntToStr(np));
    end;
  end;

  lv1.ItemIndex := 0;

  if ShowModal = mrOk then
  begin
    if lv1.ItemIndex <> -1 then
    begin
      sIP := lv1.Items[lv1.ItemIndex].SubItems[0];

      TryStrToInt(lv1.Items[lv1.ItemIndex].SubItems[1], nPort);
    end;
  end;
end;

end.
