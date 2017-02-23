unit FrmLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TfLog = class(TForm)
    redt1: TRichEdit;
    pnl2: TPanel;
    chkRev: TCheckBox;
    chkSend: TCheckBox;
    btnClear: TButton;
    chkHex: TCheckBox;
    spl1: TSplitter;
    btn1: TBitBtn;
    procedure btnClearClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure PackLog(SendPack : TBytes;bSend : Boolean; aObject : TObject);
  end;

var
  fLog: TfLog;

implementation

{$R *.dfm}

{ TfEquLog }

procedure TfLog.btn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfLog.btnClearClick(Sender: TObject);
begin
  redt1.Lines.Clear;
end;

procedure TfLog.PackLog(SendPack: TBytes; bSend: Boolean; aObject : TObject);
//var
//  s : string;
begin
//  if Visible then
//  begin
//    if chkHex.Checked then
//      s := BytesToPackStr(SendPack)
//    else
//      s := BytesToASC(SendPack);
//
//    if bSend then
//    begin
//      if chkSend.Checked then
//        redt1.Lines.Add(FormatDateTime('hh:mm:ss:zzz ', Now) + '∑¢ÀÕ ' + s);
//    end
//    else
//    begin
//      if chkRev.Checked then
//        redt1.Lines.Add(FormatDateTime('hh:mm:ss:zzz ', Now) + 'Ω” ’ ' + s);
//    end;
//  end;
end;

end.
