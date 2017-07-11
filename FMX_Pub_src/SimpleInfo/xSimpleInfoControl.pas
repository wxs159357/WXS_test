unit xSimpleInfoControl;

interface

uses SysUtils, Classes, xDBActionBase, FireDAC.Stan.Param, xFunction ;

type
  /// <summary>
  /// 信息
  /// </summary>
  TSimpleInfo = class
  private
    FSIRemark1: string;
    FSIID: Integer;
    FSIName: string;
    FSIRemark2: string;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TSimpleInfo); virtual;

    /// <summary>
    /// 编号
    /// </summary>
    property SIID   : Integer    read FSIID write FSIID;

    /// <summary>
    /// 名称
    /// </summary>
    property SIName : string read FSIName write FSIName;

    /// <summary>
    /// 备注1
    /// </summary>
    property SIRemark1 : string read FSIRemark1 write FSIRemark1;

    /// <summary>
    /// 备注2
    /// </summary>
    property SIRemark2 : string read FSIRemark2 write FSIRemark2;

  public
    /// <summary>
    /// 清除信息
    /// </summary>
    procedure Clear;


  end;

type
  TSimpleInfoAction = class(TDBActionBase)
  private
    FDBTableName : string;

  public
    constructor Create(sDBTableName : string); overload;
    destructor Destroy; override;

    /// <summary>
    /// 添加
    /// </summary>
    procedure AddInfo(AInfo : TSimpleInfo);

    /// <summary>
    /// 删除
    /// </summary>
    procedure DelInfo(AInfoID : Integer);

    /// <summary>
    /// 编辑
    /// </summary>
    procedure EditInfo(AInfo : TSimpleInfo);

    /// <summary>
    /// 清空
    /// </summary>
    procedure ClearInfo;

    /// <summary>
    /// 加载信息
    /// </summary>
    procedure LoadInfo(slList : TStringList);
  end;

type
  TSimpleInfoControl = class

  private
    FAction : TSimpleInfoAction;
    FSimpleInfoList: TStringList;
    FSimpleInfoName: string;
    function GetSimpleInfo(nIndex: Integer): TSimpleInfo;

    /// <summary>
    /// 获取最大编号
    /// </summary>
    function GetMaxID : Integer;

  public
    constructor Create(sDBTableName : string);
    destructor Destroy; override;

  public
    /// <summary>
    /// 信息列表
    /// </summary>
    property SimpleInfoList : TStringList read FSimpleInfoList write FSimpleInfoList;

    property SimpleInfo[nIndex:Integer] : TSimpleInfo read GetSimpleInfo;

    /// <summary>
    /// 根据名称获取信息
    /// </summary>
    /// <param name="sInfoName">信息名称</param>
    /// <param name="aNotInfo">排除的信息</param>
    /// <returns>获取的信息，不等于排除信息</returns>
    function GetInfoByName(sInfoName : string; aNotInfo : TSimpleInfo = nil) : TSimpleInfo;

    /// <summary>
    /// 添加
    /// </summary>
    procedure AddInfo(AInfo : TSimpleInfo);

    /// <summary>
    /// 删除
    /// </summary>
    procedure DelInfo(nInfoID : Integer);

    /// <summary>
    /// 编辑
    /// </summary>
    procedure EditInfo(AInfo : TSimpleInfo);

    /// <summary>
    /// 清空
    /// </summary>
    procedure ClearInfo;

    /// <summary>
    /// 简单信息名称
    /// </summary>
    property SimpleInfoName : string read FSimpleInfoName write FSimpleInfoName;

  end;
var
  ASimpleInfoControl : TSimpleInfoControl;


implementation

{ TSimpleInfo }

procedure TSimpleInfo.Assign(Source: TSimpleInfo);
begin
  if Assigned(Source) then
  begin
    FSIID      := Source.SIID     ;
    FSIName    := Source.SIName   ;
    FSIRemark1 := Source.SIRemark1;
    FSIRemark2 := Source.SIRemark2;
  end;
end;

procedure TSimpleInfo.Clear;
begin
  FSIID      := -1;
  FSIName    := '';
  FSIRemark1 := '';
  FSIRemark2 := '';
end;

constructor TSimpleInfo.Create;
begin
  Clear;
end;

destructor TSimpleInfo.Destroy;
begin

  inherited;
end;


{ TSimpleInfoAction }

procedure TSimpleInfoAction.AddInfo(AInfo: TSimpleInfo);
var
  sSQL : string;
begin
  if not Assigned(AInfo) then
    Exit;

  sSQL := 'insert into ' + FDBTableName + ' (SIID, SIName, SIRemark1, SIRemark2)' +
          ' values (:SIID, :SIName, :SIRemark1, :SIRemark2)';

  FQuery.SQL.Text := sSQL;

  with FQuery.Params, AInfo do
  begin
    ParamByName( 'SIID'      ).Value := FSIID     ;
    ParamByName( 'SIName'    ).Value := FSIName   ;
    ParamByName( 'SIRemark1' ).Value := FSIRemark1;
    ParamByName( 'SIRemark2' ).Value := FSIRemark2;
  end;

  ExecSQL;
end;

procedure TSimpleInfoAction.ClearInfo;
begin
  FQuery.SQL.Text := 'delete from ' + FDBTableName;
  ExecSQL;
end;

constructor TSimpleInfoAction.Create(sDBTableName: string);
begin
  inherited Create;
  FDBTableName := sDBTableName;

end;

procedure TSimpleInfoAction.DelInfo(AInfoID: Integer);
var
  sSQL : string;
begin
  sSQL := 'delete from ' + FDBTableName +' where SIID = %d';
  FQuery.SQL.Text := Format(sSQL, [AInfoID]);
  ExecSQL;
end;

destructor TSimpleInfoAction.Destroy;
begin

  inherited;
end;

procedure TSimpleInfoAction.EditInfo(AInfo: TSimpleInfo);
var
  sSQL : string;
begin
  if not Assigned(AInfo) then
    Exit;

  sSQL := 'update ' + FDBTableName + ' set SIName = :SIName, ' +
          'SIRemark1 = :SIRemark1, SIRemark2 = :SIRemark2 where SIID = :SIID';

  FQuery.SQL.Text := sSQL;
  with FQuery.Params, AInfo do
  begin
    ParamByName( 'SIID'      ).Value := FSIID     ;
    ParamByName( 'SIName'    ).Value := FSIName   ;
    ParamByName( 'SIRemark1' ).Value := FSIRemark1;
    ParamByName( 'SIRemark2' ).Value := FSIRemark2;
  end;
  ExecSQL;
end;

procedure TSimpleInfoAction.LoadInfo(slList: TStringList);
var
  sSQL : string;
  AInfo : TSimpleInfo;
begin
  if not Assigned(slList) then
    Exit;

  sSQL := 'select * from ' + FDBTableName;

  FQuery.SQL.Text := sSQL;
  FQuery.Open;

  while not FQuery.Eof do
  begin
    AInfo := TSimpleInfo.Create;

    AInfo.SIID      := FQuery.FieldByName('SIID').AsInteger;
    AInfo.SIName    := FQuery.FieldByName('SIName').AsString;
    AInfo.SIRemark1 := FQuery.FieldByName('SIRemark1').AsString;
    AInfo.SIRemark2 := FQuery.FieldByName('SIRemark2').AsString;

    slList.AddObject('', AInfo);
    FQuery.Next;
  end;

  FQuery.Close;

end;

{ TSimpleInfoControl }

procedure TSimpleInfoControl.AddInfo(AInfo : TSimpleInfo);
begin
  if Assigned(AInfo) then
  begin
    AInfo.SIID := GetMaxID + 1;
    FAction.AddInfo(AInfo);
    FSimpleInfoList.AddObject('', AInfo);
  end;
end;

procedure TSimpleInfoControl.ClearInfo;
begin
  FAction.ClearInfo;
  ClearStringList(FSimpleInfoList);
end;

constructor TSimpleInfoControl.Create(sDBTableName: string);
begin
  FAction := TSimpleInfoAction.Create(sDBTableName);
  FSimpleInfoList:= TStringList.Create;

  FAction.LoadInfo(FSimpleInfoList);
  FSimpleInfoName := '基本信息';
end;

procedure TSimpleInfoControl.DelInfo(nInfoID: Integer);
var
   i : Integer;
begin
  for i := FSimpleInfoList.Count - 1 downto 0 do
  begin
    with TSimpleInfo(FSimpleInfoList.Objects[i]) do
    begin
      if SIID = nInfoID then
      begin
        TSimpleInfo(FSimpleInfoList.Objects[i]).Free;
        FSimpleInfoList.Delete(i);
        Break;
      end;
    end;
  end;

  FAction.DelInfo(nInfoID);
end;

destructor TSimpleInfoControl.Destroy;
begin
  ClearStringList(FSimpleInfoList);
  FSimpleInfoList.Free;
  FAction.Free;

  inherited;
end;

procedure TSimpleInfoControl.EditInfo(AInfo: TSimpleInfo);
begin
  FAction.EditInfo(AInfo);
end;

function TSimpleInfoControl.GetInfoByName(sInfoName: string; aNotInfo : TSimpleInfo): TSimpleInfo;
var
   i : Integer;
  AInfo : TSimpleInfo;
begin
  Result := nil;
  for i := 0 to FSimpleInfoList.Count - 1 do
  begin
    AInfo := TSimpleInfo(FSimpleInfoList.Objects[i]);

    if (Trim(AInfo.SIName) = Trim(sInfoName)) and (AInfo <> aNotInfo) then
    begin
      Result := AInfo;
      Break;
    end;
  end;
end;

function TSimpleInfoControl.GetMaxID: Integer;
var
   i : Integer;
begin
  Result := 0;
  for i := 0 to FSimpleInfoList.Count - 1 do
  begin
    with TSimpleInfo(FSimpleInfoList.Objects[i]) do
    begin
      if SIID > Result then
      begin
        Result := SIID;
      end;
    end;
  end;
end;

function TSimpleInfoControl.GetSimpleInfo(nIndex: Integer): TSimpleInfo;
begin
  if (nIndex >= 0) and (FSimpleInfoList.Count > nIndex) then
  begin
    Result := TSimpleInfo(FSimpleInfoList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

end.
