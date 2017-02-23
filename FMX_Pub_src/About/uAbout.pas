unit uAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, xFunction,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, xBaseForm;

type
  TfAbout = class(TfBaseForm)
    img1: TImage;
    lstInfo: TListBox;
    btnClose: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
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
    /// 显示版本信息
    /// </summary>
    /// <param name="sModel">型号 如CKM-S21</param>
    /// <param name="sName">程序全名 如10/0.4KV 配电仿真系统</param>
    procedure ShowAboutInfo( sModel, sName : string; aFontColor: TAlphaColor = TAlphaColorRec.White );


  end;

var
  fAbout: TfAbout;

implementation

{$R *.fmx}

{ TForm2 }

procedure TfAbout.SetVersion(sFileName: string);
begin
  lstInfo.Items[0] := '版本：' + GetFileVersion;
end;

procedure TfAbout.LoadPicture(sPicName: string);
begin
  if FileExists( sPicName ) then
    img1.Bitmap.LoadFromFile(sPicName);
end;

procedure TfAbout.SetCompanyName(sName: string);
begin
  if sName <> '' then
    lstInfo.Items[1] := '公司：' + sName
  else
    lstInfo.Items[1] := '';
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
    lstInfo.Items[3] := '版权所有：'+ sFrom
  else
    lstInfo.Items[3] := '版权所有：'+ sFrom + ' - ' + sTo;
end;

procedure TfAbout.SetWebSite(aWebSite: string);
begin
  if aWebSite <> '' then
    lstInfo.Items[2] := '网址：' + aWebSite
  else
    lstInfo.Items[2] := '';
end;
procedure TfAbout.ShowAboutInfo(sModel, sName: string;
  aFontColor: TAlphaColor);
begin
  lbl1.Text := sModel;
  lbl2.Text := sName;
  lbl1.FontColor := aFontColor;
  lbl2.FontColor := aFontColor;
  ShowModal;
end;

end.
