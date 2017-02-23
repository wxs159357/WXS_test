unit FrmPosState;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.StdCtrls,
  System.IniFiles, xExamControl, xClientInfo, Vcl.Menus;

type
  TfPosState = class(TForm)
    imglst1: TImageList;
    actnmngr1: TActionManager;
    lvList: TListView;
    pmn1: TPopupMenu;
    actChangeIP: TAction;
    mntmChangeIP: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actChangeIPExecute(Sender: TObject);
  private
    { Private declarations }
    FWidth : Integer;
    procedure ReadINI;
    procedure WriteINI;
    procedure ClinetChanged(Sender: TObject);

    procedure RefurshClient(AClinetInfo : TClientInfo);
  public
    { Public declarations }
  end;

var
  fPosState: TfPosState;

implementation

{$R *.dfm}

procedure TfPosState.actChangeIPExecute(Sender: TObject);
begin
  if Assigned(lvList.Selected) then
  begin
    with TClientInfo(lvList.Selected.Data) do
    begin
      ClientIP := InputBox('更改信息', '客户端IP地址', ClientIP);
      RefurshClient(TClientInfo(lvList.Selected.Data));
    end;
  end;
end;

procedure TfPosState.ClinetChanged(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TClientInfo) then
  begin
    RefurshClient(TClientInfo(Sender));
  end;
end;

procedure TfPosState.FormCreate(Sender: TObject);
var
  i : Integer;
  AClinetInfo : TClientInfo;
begin
  ReadINI;
  ExamControl.OnClinetChanged := ClinetChanged;

  Width := 132 + 105 * (ExamControl.ColCount - 1);

  lvList.Clear;
  for i := 0 to ExamControl.ClinetCount - 1 do
  begin
    AClinetInfo := ExamControl.ClientInfo[i];

    if Assigned(AClinetInfo) then
    begin
      with lvList.Items.Add do
      begin
        Data := AClinetInfo;
        RefurshClient(AClinetInfo);
      end;
    end;
  end;
end;

procedure TfPosState.FormDestroy(Sender: TObject);
begin
  WriteINI;
end;

procedure TfPosState.ReadINI;
begin
  with TIniFile.Create('ClientPos.ini') do
  begin
    FWidth := ReadInteger('Option', 'ClientPos', 132 + 105 * 5);
    Free;
  end;
end;

procedure TfPosState.RefurshClient(AClinetInfo: TClientInfo);
var
  AItem : TListItem;
begin
  if Assigned(AClinetInfo) and (lvList.Items.Count >= AClinetInfo.ClientSN) then
  begin
    AItem := lvList.Items[AClinetInfo.ClientSN-1];

    AItem.Caption := AClinetInfo.ClientName;
    AItem.ImageIndex := Integer(AClinetInfo.ClientState);
  end;
end;

procedure TfPosState.WriteINI;
begin
  with TIniFile.Create('ClientPos.ini') do
  begin
    WriteInteger('Option', 'ClientPos', FWidth);
    Free;
  end;
end;

end.
