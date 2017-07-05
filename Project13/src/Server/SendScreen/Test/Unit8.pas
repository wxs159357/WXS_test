unit Unit8;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, xThreadUDPSendScreen, jpeg,
  Vcl.ExtCtrls;

type
  TForm8 = class(TForm)
    mmo1: TMemo;
    rdgrp1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdgrp1Click(Sender: TObject);
  private
    { Private declarations }
    dc : HDC;
    fullCanvas : TCanvas;
    Fullscreen:Tbitmap;
    AJpeg : TJPEGImage;
    /// <summary>
    /// 界面截屏
    /// </summary>
    procedure GetSreen(Sender: TObject);
    /// <summary>
    /// 窗口截屏
    /// </summary>
    procedure GetSreen1(Sender: TObject);
  public
    { Public declarations }

  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.FormCreate(Sender: TObject);
begin
  UDPSendScreen := TThreadUDPSendScreen.Create(False);
  rdgrp1Click(nil);
  dc := GetDC(0);
  fullCanvas := TCanvas.Create;
  fullCanvas.Handle := dc;

  Fullscreen:=TBitmap.Create;
  AJpeg := TJPEGImage.Create;

end;

procedure TForm8.FormDestroy(Sender: TObject);
begin
  UDPSendScreen.Free;

  fullCanvas.Free;
  Fullscreen.Free;
  AJpeg.Free;

  ReleaseDC(0, dc);
end;

procedure TForm8.GetSreen(Sender: TObject);
var

  SrcRect, DstRect : TRect;




begin
//  fullCanvas := TCanvas.Create;
//  fullCanvas.Handle := dc;

//  Fullscreen:=TBitmap.Create;

  Fullscreen.Width:=Self.Width;
  Fullscreen.Height:=Self.Height;

  SrcRect := Rect(Self.Left,Self.Top,Self.Left+Fullscreen.Width,Self.Top+Fullscreen.Height);
  DstRect := Rect(0,0,Fullscreen.Width,Fullscreen.Height);

//  AJpeg := TJPEGImage.Create;
  Fullscreen.Canvas.CopyRect(DstRect, fullCanvas, SrcRect); //把整个屏幕复制到BITMAP中
  Fullscreen.PixelFormat := pfDevice;
  AJpeg.Assign( Fullscreen );

  TMemoryStream(Sender).Clear;
  AJpeg.SaveToStream(TMemoryStream(Sender));

//  fullscreen.free;
//  AJpeg.Free;
//  fullCanvas.Free;



//  fullCanvas := TCanvas.Create;
//  fullCanvas.Handle := dc;
//
//  Fullscreen:=TBitmap.Create;
//
//  Fullscreen.Width:=Self.Width;
//  Fullscreen.Height:=Self.Height;
//
//  SrcRect := Rect(Self.Left,Self.Top,Self.Left+Fullscreen.Width,Self.Top+Fullscreen.Height);
//  DstRect := Rect(0,0,Fullscreen.Width,Fullscreen.Height);
//
//  AJpeg := TJPEGImage.Create;
//  Fullscreen.Canvas.CopyRect(DstRect, fullCanvas, SrcRect); //把整个屏幕复制到BITMAP中
//  Fullscreen.PixelFormat := pfDevice;
//  AJpeg.Assign( Fullscreen );
//
//  TMemoryStream(Sender).Clear;
//  AJpeg.SaveToStream(TMemoryStream(Sender));
//
//  fullscreen.free;
//  AJpeg.Free;
//  fullCanvas.Free;


end;

procedure TForm8.GetSreen1(Sender: TObject);
var
//  Fullscreen:Tbitmap;
//  AJpeg : TJPEGImage;
  SrcRect, DstRect : TRect;
begin

//  Fullscreen:=TBitmap.Create;

  Fullscreen.Width:=Self.Width-15;
  Fullscreen.Height:=Self.Height-36;

  SrcRect := Rect(0, 0, Fullscreen.Width,Fullscreen.Height);
  DstRect := Rect(0, 0, Fullscreen.Width,Fullscreen.Height);

//  AJpeg := TJPEGImage.Create;
  Fullscreen.Canvas.CopyRect(DstRect, Self.Canvas, SrcRect); //把整个屏幕复制到BITMAP中
  Fullscreen.PixelFormat := pfDevice;
  AJpeg.Assign( Fullscreen );

  TMemoryStream(Sender).Clear;
  AJpeg.SaveToStream(TMemoryStream(Sender));

//  fullscreen.free;
//  AJpeg.Free;
end;

procedure TForm8.rdgrp1Click(Sender: TObject);
begin
  if rdgrp1.ItemIndex = 0 then
  begin
    UDPSendScreen.OnGetScreen :=  GetSreen;
  end
  else
  begin
    UDPSendScreen.OnGetScreen :=  GetSreen1;
  end;
end;

end.
