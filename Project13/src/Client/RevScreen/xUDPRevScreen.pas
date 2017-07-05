unit xUDPRevScreen;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xUDPServerBase,
  xConsts, System.IniFiles, Winapi.WinInet, winsock, Graphics,
  Vcl.Imaging.jpeg;

const
  C_SEND_PACKS_COUNT = 8000; // 每条命令发送包长度
type
  /// <summary>
  /// 接收屏幕
  /// </summary>
  TUDPRevScreen = class(TUDPServerBase)

  private
    FOnRevJpg: TNotifyEvent;
    FJPG : TJPEGImage;
    FRevPacks : TBytes;
    FRandom : Byte;


//    procedure ReadINI;
//    procedure WriteINI;

  protected
    procedure RevPacksData(sIP: string; nPort :Integer;aPacks: TArray<Byte>); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 获取到图片
    /// </summary>
    property OnRevJpg : TNotifyEvent read FOnRevJpg write FOnRevJpg;

  end;


implementation

{ TUDPRevScreen }

constructor TUDPRevScreen.Create;
begin
  inherited;
  FJPG := TJPEGImage.Create;
end;

destructor TUDPRevScreen.Destroy;
begin
  FJPG.Free;
  inherited;
end;

procedure TUDPRevScreen.RevPacksData(sIP: string; nPort: Integer;
  aPacks: TArray<Byte>);
  /// <summary>
  /// 接收界面图片
  /// </summary>
  procedure DataTojpg( ajpgPack : TBytes );
  var
    ASCreen : TMemoryStream;
    nLen : Integer;
  begin
    nLen := Length(ajpgPack);

    if (nLen >= 4) and
      (ajpgPack[nLen - 2] = 255) and
      (ajpgPack[nLen - 1] = 217) and
      (ajpgPack[0] = 255) and
      (ajpgPack[1] = 216) then
    begin
      ASCreen := TMemoryStream.Create;
      ASCreen.Position := 0;
      ASCreen.Write( ajpgPack[0], nLen );

      ASCreen.Position := 0;
      FJPG.LoadFromStream(ASCreen);
      FreeAndNil(ASCreen);
    end;
  end;

var
  nRandom, nPackSign : Integer;

//  nPackIndex nPackCount : Integer;

  nLen1, nLen2 : Integer;
  i : Integer;
begin
  inherited;

  if Length(aPacks) > 7 then
  begin
    if (aPacks[0] = $FF) and (aPacks[1] = $AA) and (aPacks[2] = $55) then
    begin
      nRandom    := aPacks[3];
      nPackSign  := aPacks[4];
//      nPackIndex := aPacks[5];
//      nPackCount := aPacks[6];

      if FRandom <> nRandom then
      begin

      end;

      nLen1 := Length(aPacks)-7;
      nLen2 := Length(FRevPacks);

      SetLength(FRevPacks, nLen1 + nLen2);

      for i := 0 to nLen1 - 1 do
        FRevPacks[nLen2 + i] := aPacks[i+7];

      // 起始包
      if nPackSign and 1 = 1 then
      begin

      end;

      // 中间包
      if nPackSign and 2 = 2 then
      begin

      end;

      // 结尾包
      if nPackSign and 4 = 4 then
      begin

        if Assigned(FOnRevJpg)then
        begin
          DataTojpg(FRevPacks);

          FOnRevJpg(FJPG);

        end;


        SetLength(FRevPacks, 0);
      end;
    end;
  end;
end;

end.
