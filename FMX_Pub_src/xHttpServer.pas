unit xHttpServer;

interface

uses
  System.SysUtils, System.Types,  System.Classes, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext;

type
  THttpServer = class
  private
    FHttpServer: TIdHTTPServer;
    FRootDir: string;
    FFileText : TStringList;
    FDefaultMainPage: string;
    FDefaultPort: Integer;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 服务器根目录 （默认 程序根目录下的www文件夹）
    /// </summary>
    property RootDir : string read FRootDir write FRootDir;

    /// <summary>
    /// 服务器端口 (默认80)
    /// </summary>
    property DefaultPort : Integer read FDefaultPort write FDefaultPort;

    /// <summary>
    /// 是否启动
    /// </summary>
    property Active : Boolean read GetActive write SetActive;

    /// <summary>
    /// 默认主页 (相对目录)
    /// </summary>
    property DefaultMainPage : string read FDefaultMainPage write FDefaultMainPage;

  end;

implementation

{ THttpServer }

procedure THttpServer.CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LFilename: string;
  LPathname: string;
  sFileExt : string;
begin
//浏览器请求http://127.0.0.1:8008/index.html?a=1&b=2
  //ARequestInfo.Document  返回    /index.html
  //ARequestInfo.QueryParams 返回  a=1b=2
  //ARequestInfo.Params.Values['name']   接收get,post过来的数据
  ////webserver发文件
  LFilename := ARequestInfo.Document;

  if LFilename = '/' then
    LFilename := '/'+DefaultMainPage;

  LPathname := RootDir + LFilename;

  if FileExists(LPathname) then
  begin
    sFileExt := UpperCase(ExtractFileExt(LPathname));

    if (sFileExt = '.HTML') or (sFileExt = '.HTM')then
    begin
      FFileText.LoadFromFile(LPathname);
      AResponseInfo.ContentType :='text/html;Charset=UTF-8'; // 中文不乱码
      AResponseInfo.ContentText:=FFileText.Text;
    end
    else if sFileExt = '.XML'then
    begin
      FFileText.LoadFromFile(LPathname);
      AResponseInfo.ContentType :='text/xml;Charset=UTF-8';
      AResponseInfo.ContentText:=FFileText.Text;
    end
    else
    begin
      AResponseInfo.ContentStream := TFileStream.Create(LPathname, fmOpenRead + fmShareDenyWrite);//发文件
      if (sFileExt = '.RAR') or (sFileExt = '.ZIP')or (sFileExt = '.EXE') then
      begin
        //下载文件时，直接从网页打开而没有弹出保存对话框的问题解决
        AResponseInfo.CustomHeaders.Values['Content-Disposition'] :='attachment; filename="'+extractfilename(LPathname)+'"';
      end;
    end;
  end
  else
  begin
    AResponseInfo.ContentType :='text/html;Charset=UTF-8';
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '找不到' + ARequestInfo.Document;
  end;
//替换 IIS
  {AResponseInfo.Server:='IIS/6.0';
  AResponseInfo.CacheControl:='no-cache';
  AResponseInfo.Pragma:='no-cache';
  AResponseInfo.Date:=Now;}
end;

constructor THttpServer.Create;
begin
  FHttpServer:= TIdHTTPServer.Create;
  FFileText := TStringList.Create;
  FHttpServer.OnCommandGet := CommandGet;
  FRootDir := ExtractFilePath(ParamStr(0)) + 'www';
  FDefaultMainPage := 'Index.html';
  FDefaultPort := 80;
end;

destructor THttpServer.Destroy;
begin
  FHttpServer.Free;
  FFileText.Free;
  inherited;
end;

function THttpServer.GetActive: Boolean;
begin
  Result := FHttpServer.Active;
end;

procedure THttpServer.SetActive(const Value: Boolean);
begin
  if FHttpServer.Active <> Value then
  begin
    if Value then
    begin
      FHttpServer.Bindings.Clear;
      FHttpServer.DefaultPort:= FDefaultPort;
    end;

    FHttpServer.Active := Value;
  end;
end;

end.
