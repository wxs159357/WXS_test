unit uLog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TfLog = class(TForm)
    mmoLog: TMemo;
    pnl1: TPanel;
    spltr1: TSplitter;
    chkSend: TCheckBox;
    chkRev: TCheckBox;
    chkHex: TCheckBox;
    btnClear: TButton;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    procedure PackLog(aPacks: TArray<Byte>; bSend : Boolean);

  end;

var
  fLog: TfLog;

implementation

uses xFunction;

{$R *.fmx}
{ TfLog }

procedure TfLog.btnClearClick(Sender: TObject);
begin
  mmoLog.Lines.Clear;
end;

procedure TfLog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfLog.PackLog(aPacks: TArray<Byte>; bSend : Boolean);
var
  s : string;
begin
  if Visible then
  begin
    if chkHex.IsChecked then
      s := BCDPacksToStr(aPacks)
    else
      s := PacksToStr(aPacks);

    if mmoLog.Lines.Count > 1000 then
    begin
      mmoLog.Lines.Clear;
      mmoLog.Lines.Add('删除前1000条记录');
    end;

    if bSend then
    begin
      if chkSend.IsChecked then
        mmoLog.Lines.Add(FormatDateTime('hh:mm:ss:zzz ', Now) + '发送 ' + s);
    end
    else
    begin
      if chkRev.IsChecked then
        mmoLog.Lines.Add(FormatDateTime('hh:mm:ss:zzz ', Now) + '接收 ' + s);
    end;
  end;
end;

end.
