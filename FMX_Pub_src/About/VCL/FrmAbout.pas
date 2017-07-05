unit FrmAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  dxGDIPlusClasses, xFunction;

type
  TfAbout = class(TForm)
    lstInfo: TListBox;
    pnl1: TPanel;
    pnlCode: TPanel;
    lblName: TLabel;
    lblNumbers: TLabel;
    img2: TImage;
    imgMoreInfo: TImage;
    pnl3: TPanel;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure lstInfoDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    /// <summary>
    /// 版权信息
    /// </summary>
    /// <param name="FormYear">版权起始年份,如1998</param>
    /// <param name="ToYear">版权终止年份，如2000</param>
    procedure SetCopyRight(FromYear: string; ToYear: string = '');

    /// <summary>
    /// 设置公司名称
    /// </summary>
    procedure SetCompanyName(sName: string);

    /// <summary>
    /// 设置公司网址
    /// </summary>
    procedure SetWebSite(aWebSite: string);

    /// <summary>
    /// 导入图片
    /// </summary>
    /// <param name="sPicName">图片的完整名称，包括完整路径和扩展名,大小为470x130</param>
    procedure LoadPicture(sPicName: string);

    /// <summary>
    /// 添加显示版本信息
    /// </summary>
    procedure SetVersion(sFileName: string);

    /// <summary>
    /// 添加字符串信息
    /// </summary>
    procedure AddStrInfo(sStr : string);

    /// <summary>
    /// 显示版本信息
    /// </summary>
    /// <param name="sModel">型号 如CKM-S21</param>
    /// <param name="sName">程序全名 如10/0.4KV 配电仿真系统</param>
    procedure ShowAboutInfo( sModel, sName : string);

    /// <summary>
    /// 显示二维码信息
    /// </summary>
    procedure ShowCode2(sFileName : string);

  end;

var
  fAbout: TfAbout;

implementation

{$R *.dfm}

{ TfAbout }

procedure TfAbout.SetVersion(sFileName: string);
begin
  lstInfo.Items.Add('版本：' + GetFileVersion);
end;

procedure TfAbout.AddStrInfo(sStr: string);
begin
  if sStr <> '' then
    lstInfo.Items.Add(sStr);
end;


procedure TfAbout.btn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfAbout.FormCreate(Sender: TObject);
begin
  lstInfo.Items.Clear;
end;

procedure TfAbout.LoadPicture(sPicName: string);
begin
  if FileExists( sPicName ) then
    img2.Picture.LoadFromFile(sPicName);
end;

procedure TfAbout.lstInfoDblClick(Sender: TObject);
begin
  if lstInfo.ItemIndex <> -1 then
    ShowMessage(lstInfo.Items[lstInfo.ItemIndex]);
end;

procedure TfAbout.SetCompanyName(sName: string);
begin
  if sName <> '' then
    lstInfo.Items.Add('公司：' + sName);
end;

procedure TfAbout.SetCopyRight(FromYear, ToYear: string);
var
  sFrom, sTo : string;
begin
  if FromYear = '' then
    sFrom := FormatDateTime('YYYY', Now)
  else
    sFrom := FromYear;

  if ToYear = '' then
    sTo := FormatDateTime('YYYY', Now)
  else
    sTo := ToYear;

  if sFrom = sTo then
    lstInfo.Items.Add('版权所有：'+ sFrom)
  else
    lstInfo.Items.Add('版权所有：'+ sFrom + ' - ' + sTo);
end;

procedure TfAbout.SetWebSite(aWebSite: string);
begin
  if aWebSite <> '' then
    lstInfo.Items.Add('网址：' + aWebSite);
end;
procedure TfAbout.ShowAboutInfo(sModel, sName: string);
begin
  lblNumbers.Caption := sModel;
  lblName.Caption := sName;
  ShowModal;
end;

procedure TfAbout.ShowCode2(sFileName: string);
begin
  pnlCode.Visible := FileExists(sFileName);

  if pnlCode.Visible then
  begin
    imgMoreInfo.Picture.LoadFromFile(sFileName);
  end;
end;

end.
