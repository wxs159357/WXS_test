unit uLoadForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, xConsts, xFunction;

type
  TfLoadForm = class(TForm)
    pnl1: TPanel;
    lblModel: TLabel;
    lblCOMPANY: TLabel;
    lblVersion: TLabel;
    lblObjectName: TLabel;
    lblLoadInfo: TLabel;
    img1: TImage;
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
  fLoadForm: TfLoadForm;

implementation

{$R *.fmx}

procedure TfLoadForm.FormShow(Sender: TObject);
begin
  lblModel.Text      := C_SYS_OBJECT_MODEL;
  lblCOMPANY.Text    := C_SYS_COMPANY;
  lblObjectName.Text := C_SYS_OBJECT_NAME;
  lblVersion.Text    := '版本：V' + GetFileVersion;

  if FileExists('res\logo\Load.bmp') then
    img1.Bitmap.LoadFromFile('res\logo\Load.bmp');
end;

procedure TfLoadForm.LoadInfo(sInfo: string);
begin
  lblLoadInfo.Text := sInfo;
end;

end.
