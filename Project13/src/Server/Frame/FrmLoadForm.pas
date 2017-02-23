unit FrmLoadForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, xConsts, ExtCtrls;

type
  TfLoadform = class(TForm)
    grp1: TGroupBox;
    img1: TImage;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    bvl1: TBevel;
    lblLoadInfo: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    /// <summary>
    /// 加载信息描述
    /// </summary>
    procedure LoadInfo( sInfo : string );

  end;

var
  fLoadform: TfLoadform;

implementation

{$R *.dfm}

procedure TfLoadform.FormShow(Sender: TObject);
begin
  lbl1.Caption := C_SYS_OBJECT_MODEL;
  lbl3.Caption := C_SYS_COMPANY;
  lbl5.Caption := C_SYS_OBJECT_NAME;
//  lbl4.Caption := '版本：V' + GetFileFullVersion( Application.ExeName );

  if FileExists('res\logo\Load.png') then
    img1.Picture.LoadFromFile('res\logo\Load.png');
end;

procedure TfLoadform.LoadInfo(sInfo: string);
begin
  lblLoadInfo.Caption := sInfo;
//  Application.ProcessMessages;
//  Sleep(5);
end;

end.
