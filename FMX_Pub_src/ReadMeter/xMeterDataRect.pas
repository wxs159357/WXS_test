{===============================================================================
  Copyright(c) 2006-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  公用电表数据单元

  + TMeterDataItem  电表数据-数据项
  + TMeterDataGroup 电表数据-组数据
  + TMeterData       电表数据
  + TMETER_DATA_GROUP_DL645 基于标准DL645协议的组数据

===============================================================================}

unit xMeterDataRect;

interface

uses SysUtils, Classes, xFunction, xDL645Type;

type
  /// <summary>
  /// 电表数据-数据项
  /// </summary>
  TMeterDataItem = class(TPersistent)
  private
    FSign   : Int64;
    FNote   : string;
    FFormat : string;
    FUnits  : string;
    FLength : Integer;
    FValue  : string;
    procedure SetValue( Val : string );
  published
    /// <summary>
    /// 标识
    /// </summary>
    property Sign : Int64 read FSign write FSign;

    /// <summary>
    /// 说明
    /// </summary>
    property Note : string read FNote write FNote;

    /// <summary>
    /// 格式
    /// </summary>
    property Format : string read FFormat write FFormat;

    /// <summary>
    /// 单位
    /// </summary>
    property Units : string read FUnits write FUnits;

    /// <summary>
    /// 长度
    /// </summary>
    property Length : Integer read FLength write FLength;

    /// <summary>
    /// 值
    /// </summary>
    property Value : string read FValue write SetValue;

    /// <summary>
    /// 对象赋值
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  public
    function SignInHex : string;
  end;

type
  /// <summary>
  /// 电表数据-组数据
  /// </summary>
  TMeterDataGroup = class( TPersistent )
  private
    FItems : TStringList;
    FGroupName: string;
    FVerifyValue : Boolean;
    function GetItem( nSign : Int64 ) : TMeterDataItem;
    function GetItemValue( nSign : Int64 ) : string;
    procedure SetItemValue( nSign : Int64; const Value : string );
    procedure SetGroupName(const Value: string);
  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TPersistent);

    /// <summary>
    /// 是否校验数据
    /// </summary>
    property VerifyValue : Boolean read FVerifyValue write FVerifyValue;

    /// <summary>
    /// 组名称
    /// </summary>
    property GroupName : string read FGroupName write SetGroupName;

    /// <summary>
    /// 所有数据项
    /// </summary>
    property Items : TStringList read FItems;

    /// <summary>
    /// 数据项
    /// </summary>
    /// <param name="nSign">数据标识</param>
    property Item[ nSign : Int64 ] : TMeterDataItem read GetItem;

    /// <summary>
    /// 数据项的值
    /// </summary>
    property ItemValue[ nSign : Int64 ] : string read GetItemValue write
      SetItemValue;

    /// <summary>
    /// 新建一个数据项, 如果已存在，则返回的已有数据项
    /// </summary>
    /// <param name="nSign">数据标识</param>
    /// <returns>数据项</returns>
    function NewItem( nSign : Int64 ) : TMeterDataItem; overload;virtual;

    /// <summary>
    /// 删除数据项
    /// </summary>
    procedure DelItem( nSign : Int64 );

    /// <summary>
    /// 新建一个数据项, 如果已存在，则返回的已有数据项
    /// </summary>
    /// <param name="nSign">标识</param>
    /// <param name="sFormat">格式</param>
    /// <param name="nLen">长度</param>
    /// <param name="sUnit">单位</param>
    /// <param name="sNote">说明</param>
    /// <returns>数据项</returns>
    function NewItem(nSign: Int64; sFormat: string; nLen: integer;
      sUnit, sNote: string) : TMeterDataItem; overload;virtual;

    /// <summary>
    /// 删除所有数据项
    /// </summary>
    procedure DeleteAllItems;

    /// <summary>
    /// 清空所有数据项的值
    /// </summary>
    procedure ClearAllItemsValue;
  end;

type
  /// <summary>
  /// 电表数据
  /// </summary>
  TMeterData = class( TPersistent )
  private
    FAutoCreateGroup : Boolean;
    FAutoCreateItem : Boolean;
    FDefaultGroupName : string;
    FGroups : TStringList;     // 所有分组和数据项
    FGroupStructure : TMeterDataGroup;
    function GetDefaultGroupName : string;
    procedure SetDefaultGroupName( const Value : string );
    function GetGroup( sGroupName : string  ) : TMeterDataGroup;
    function GetGroupItem( sGroupName : string; nSign : Int64 ) :
      TMeterDataItem;
    function GetDefaultGroupItem( nSign : Int64 ) : TMeterDataItem;
    function GetGroupItemValue( sGroupName : string; nSign : Int64 ) : string;
    procedure SetGroupItemValue( sGroupName : string; nSign : Int64;
      const Value: string);
    function GetDefaultGroupItemValue( nSign : Int64 ) : string;
    procedure SetDefaultGroupItemValue( nSign : Int64; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 复制MeterData对象
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    /// 新的组数据
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    /// <returns>组数据</returns>
    function NewGroup( sGroupName : string ) : TMeterDataGroup;virtual;

    /// <summary>
    /// 删除某一组数据
    /// </summary>
    procedure DeleteGroup( sGroupName : string );

    /// <summary>
    /// 删除所有组数据
    /// </summary>
    procedure DeleteAllGroups;

    /// <summary>
    /// 清空某一组所有数据项的值
    /// </summary>
    procedure ClearGroupItemsValue( sGroupName : string );

    /// <summary>
    /// 清空所有数据
    /// </summary>
    procedure ClearAllGroupItemsValue;

    /// <summary>
    /// 清空默认组所有数据项的值
    /// </summary>
    procedure ClearItemsValue;

    /// <summary>
    /// 是否自动创建分组(读取组是如果没有组则先创建组再返回)
    /// </summary>
    property AutoCreateGroup : Boolean read FAutoCreateGroup
      write FAutoCreateGroup;

    /// <summary>
    /// 是否自动创建数据项(读取数据项时如果没有数据项则先创建数据项再返回)
    /// </summary>
    property AutoCreateItem : Boolean read FAutoCreateItem
      write FAutoCreateItem;

    /// <summary>
    /// 所有组数据
    /// </summary>
    property Groups : TStringList read FGroups;

    /// <summary>
    /// 默认的组结构(组中应包含的数据项)
    /// </summary>
    property GroupStructure : TMeterDataGroup read FGroupStructure
      write FGroupStructure;

    /// <summary>
    /// 默认的组名称
    /// </summary>
    property DefaultGroupName : string read GetDefaultGroupName
      write SetDefaultGroupName;

    /// <summary>
    /// 组数据
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    property Group[ sGroupName : string ] : TMeterDataGroup read GetGroup;

    /// <summary>
    /// 分组中的数据项
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    /// <param name="nSign">数据标识</param>
    property GroupItem[ sGroupName : string; nSign : Int64 ]: TMeterDataItem
      read GetGroupItem;

    /// <summary>
    /// 默认组中的数据项
    /// </summary>
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    /// <param name="nSign">数据标识</param>
    property Item[ nSign : Int64 ] : TMeterDataItem
      read GetDefaultGroupItem;

    /// <summary>
    /// 分组中数据项的值
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    /// <param name="nSign">数据标识</param>
    property GroupItemValue[ sGroupName : string; nSign : Int64 ]: string
       read GetGroupItemValue write SetGroupItemValue;

    /// <summary>
    /// 默认组中数据项的值
    /// </summary>
    /// <param name="sGroupName">组名称</param>
    /// <param name="nSign">数据标识</param>
    property ItemValue[ nSign : Int64 ]: string read GetDefaultGroupItemValue
      write SetDefaultGroupItemValue;
  end;

type
  /// <summary>
  /// 基于标准DL645协议的组数据
  /// </summary>
  TMETER_DATA_GROUP_DL645 = class( TMeterDataGroup )
  private
    /// <summary>
    /// 添加一般数据
    /// </summary>
    /// <param name="sType">数据类型</param>
    /// <param name="nBaseSign">起始数据标识</param>
    /// <param name="nLen">长度</param>
    /// <param name="sFormat">格式</param>
    /// <param name="sUnitA">有功单位</param>
    /// <param name="sUnitU">无功单位</param>
    procedure AddNormalData( sType: string; nBaseSign : Integer;
      nLen : Integer; sFormat, sUnitA, sUnitU : string );

    /// <summary>
    /// 添加时段信息数据
    /// </summary>
    procedure AddTimePeriodData;

    /// <summary>
    /// 添加变量和参变量
    /// </summary>
    procedure AddVariableData;
  public
    constructor Create;
  end;

/// <summary>
/// 校验电表数据项
/// </summary>
procedure VerifyMeterDataItem( AItem : TMeterDataItem );

/// <summary>
/// 核对数据格式
/// </summary>
function VerifiedStr( const sData, sFormat : string; nLen : Integer ) : string;

/// <summary>
/// 修正数据用于显示
/// </summary>
function FixDataForShow( AData: string; AFormat: string; ALen: Integer) : string; overload;
function FixDataForShow( AData: TBytes; AFormat: string; ALen: Integer) : string; overload;

implementation

{ TMETER_READING }

procedure TMeterDataItem.Assign(Source: TPersistent);
begin
  Assert( Source is TMeterDataItem );

  FSign   := TMeterDataItem( Source ).Sign  ;
  FNote   := TMeterDataItem( Source ).Note  ;
  FFormat := TMeterDataItem( Source ).Format;
  FUnits  := TMeterDataItem( Source ).Units ;
  FLength := TMeterDataItem( Source ).Length;
  FValue  := TMeterDataItem( Source ).Value ;
end;

{ TMeterData }

procedure TMeterData.Assign(Source: TPersistent);
var
  i : Integer;
begin
  Assert( Source is TMeterData );
  ClearStringList( Groups );

  AutoCreateGroup  := TMeterData( Source ).AutoCreateGroup;
  AutoCreateItem   := TMeterData( Source ).AutoCreateItem;
  DefaultGroupName := TMeterData( Source ).DefaultGroupName;

  GroupStructure.Assign( TMeterData( Source ).GroupStructure );
  Groups.Text := TMeterData( Source ).Groups.Text;

  for i := 0 to TMeterData( Source ).Groups.Count -1 do
  begin
    Groups.Objects[ i ] := TMeterDataGroup.Create;
    TMeterDataGroup( Groups.Objects[ i ] ).Assign( TMeterDataGroup(
      TMeterData( Source ).Groups.Objects[ i ] ) );
  end;
end;

procedure TMeterData.ClearAllGroupItemsValue;
var
  i: Integer;
begin
  for i := 0 to FGroups.Count - 1 do
    ClearGroupItemsValue( FGroups[ i ] );
end;

procedure TMeterData.ClearGroupItemsValue(sGroupName: string);
var
  mdGroup : TMeterDataGroup;
begin
  mdGroup := GetGroup( sGroupName );

  if Assigned( mdGroup ) then
    mdGroup.ClearAllItemsValue;
end;

procedure TMeterData.ClearItemsValue;
begin
  ClearGroupItemsValue( FDefaultGroupName );
end;

constructor TMeterData.Create;
begin
  FGroups := TStringList.Create;
  FGroupStructure := TMeterDataGroup.Create;
  FAutoCreateGroup := False;
  FAutoCreateItem := False;
end;

destructor TMeterData.Destroy;
begin
  FGroups.Free;
  FGroupStructure.Free;
  inherited;
end;

procedure TMeterData.DeleteAllGroups;
var
  i : Integer;
begin
  for i := 0 to fgroups.Count - 1 do
    FGroups.Objects[ i ].Free;

  FGroups.Clear;
end;

procedure TMeterData.DeleteGroup( sGroupName : string );
var
  nIndex : Integer;
begin
  nIndex := FGroups.IndexOf( sGroupName );

  if nIndex <> -1 then
  begin
    FGroups.Objects[ nIndex ].Free;
    FGroups.Delete( nIndex );
  end;
end;

function TMeterData.GetDefaultGroupItem(nSign: Int64): TMeterDataItem;
begin
  Result := GetGroupItem( GetDefaultGroupName, nSign );
end;

function TMeterData.GetDefaultGroupItemValue(nSign: Int64): string;
begin
  Result := GetGroupItemValue( GetDefaultGroupName, nSign );
end;

function TMeterData.GetDefaultGroupName: string;
begin
  if FDefaultGroupName <> EmptyStr then
    Result := FDefaultGroupName
  else
  begin
    if FGroups.Count > 0 then
      Result := FGroups[ 0 ]
    else
      Result := EmptyStr;
  end;
end;

function TMeterData.GetGroup(sGroupName: string): TMeterDataGroup;
var
  nIndex : Integer;
begin
  nIndex := FGroups.IndexOf( sGroupName );

  if nIndex = -1 then
  begin
    // 如果可以自动创建，创建新的组数据
    if AutoCreateGroup then
    begin
      Result := TMeterDataGroup.Create;
      FGroups.AddObject( sGroupName, Result );
    end
    else
      Result := nil
  end
  else
    Result := TMeterDataGroup( FGroups.Objects[ nIndex ] );
end;

function TMeterData.GetGroupItem(sGroupName: string; nSign: Int64):
  TMeterDataItem;
var
  mdGroup : TMeterDataGroup;
begin
  mdGroup := GetGroup( sGroupName );

  if not Assigned( mdGroup ) then
    Result := nil
  else
  begin
    Result := mdGroup.Item[ nSign ];

    // 如果可以自动创建，创建新的数据项
    if ( not Assigned( Result ) ) and AutoCreateItem then
      Result := mdGroup.NewItem( nSign );
  end;
end;

function TMeterData.GetGroupItemValue(sGroupName: string; nSign: Int64):
  string;
var
  mdItem : TMeterDataItem;
begin
  mdItem := GetGroupItem( sGroupName, nSign );

  if Assigned( mdItem ) then
    Result := mdItem.Value
  else
    Result := EmptyStr;
end;

function TMeterData.NewGroup(sGroupName: string): TMeterDataGroup;
var
  mdGroup : TMeterDataGroup;
begin
  if sGroupName = EmptyStr then
  begin
    Result := nil;
    Exit;
  end;

  mdGroup := GetGroup( sGroupName );

  if not Assigned( mdGroup ) then
  begin
    mdGroup := TMeterDataGroup.Create;

    // 使用组数据结构
    mdGroup.Assign( GroupStructure );

    mdGroup.GroupName := sGroupName;

    FGroups.AddObject( sGroupName, mdGroup );
  end;

  Result := mdGroup;
end;

procedure TMeterData.SetDefaultGroupItemValue(nSign: Int64;
  const Value: string);
begin
  SetGroupItemValue( GetDefaultGroupName, nSign, Value );
end;

procedure TMeterData.SetDefaultGroupName(const Value: string);
begin
  if FGroups.IndexOf( Value ) <> -1 then
    FDefaultGroupName := Value;
end;

procedure TMeterData.SetGroupItemValue(sGroupName: string; nSign: Int64;
  const Value: string);
var
  mdItem : TMeterDataItem;
begin
  mdItem := GetGroupItem( sGroupName, nSign );

  if Assigned( mdItem ) then
    mdItem.Value := Value;
end;

{ TMeterDataGroup }

procedure TMeterDataGroup.Assign(Source: TPersistent);
var
  mdGroup : TMeterDataGroup;
  mdItem : TMeterDataItem;
  i: Integer;
begin
  Assert( Source is TMeterDataGroup );

  DeleteAllItems;

  GroupName := TMeterDataGroup( Source ).GroupName;

  mdGroup := TMeterDataGroup( Source );

  FVerifyValue := mdGroup.VerifyValue;

  for i := 0 to mdGroup.Items.Count - 1 do
  begin
    mdItem := TMeterDataItem.Create;
    mdItem.Assign( TMeterDataItem( mdGroup.Items.Objects[ i ] ) );
    FItems.AddObject( mdGroup.Items[ i ], mdItem );
  end;
end;

procedure TMeterDataGroup.AssignValue(Source: TPersistent);
var
  i: Integer;
  AItem : TMeterDataItem;
begin
  Assert( Source is TMeterDataGroup );

  ClearAllItemsValue;

  for i := 0 to Items.Count - 1 do
  begin
    with TMeterDataItem(Items.Objects[i]) do
    begin
      AItem := TMeterDataGroup(Source).Item[Sign];
      if Assigned(AItem) then
        Value :=  AItem.Value;
    end;
  end;
end;

procedure TMeterDataGroup.ClearAllItemsValue;
var
  mdItem : TMeterDataItem;
  i : Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    mdItem := TMeterDataItem( FItems.Objects[ i ] );
    mdItem.Value := EmptyStr;
  end;
end;

constructor TMeterDataGroup.Create;
begin
  FItems := TStringList.Create;
  FVerifyValue := False;
end;

procedure TMeterDataGroup.DeleteAllItems;
var
  i : Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems.Objects[ i ].Free;

  FItems.Clear;
end;

procedure TMeterDataGroup.DelItem(nSign: Int64);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    if TMeterDataItem(FItems.Objects[i]).Sign = nSign then
    begin
      FItems.Objects[ i ].Free;
      FItems.Delete(i);
      Exit;
    end;
  end;
end;

destructor TMeterDataGroup.Destroy;
begin
  DeleteAllItems;
  FItems.Free;
  inherited;
end;

function TMeterDataGroup.GetItem(nSign: Int64): TMeterDataItem;
var
  nIndex : Integer;
  sSign : string;
begin
//  if nSign > $FFFF then
    sSign := IntToHex( nSign and $FFFFFFFF, 8 );
//  else
//    sSign := IntToHex( nSign and C_METER_DATA_SIGN_MAX, 4 );

  nIndex := FItems.IndexOf( sSign );
  if nIndex = -1 then
  begin
    nIndex := FItems.IndexOf( IntToHex( Sign97To07(nSign) and $FFFFFFFF, 8 ) );
  end;

  if nIndex = -1 then
    Result := nil
  else
    Result := TMeterDataItem( FItems.Objects[ nIndex ] );
end;

function TMeterDataGroup.GetItemValue(nSign: Int64): string;
var
  mdItem : TMeterDataItem;
begin
  mdItem := GetItem( nSign );

  if Assigned( mdItem ) then
    Result := mdItem.Value
  else
    Result := EmptyStr;
end;

function TMeterDataGroup.NewItem(nSign: Int64; sFormat: string;
  nLen: integer; sUnit, sNote: string): TMeterDataItem;
begin
  Result := NewItem( nSign );

  with Result do
  begin
    Note   := sNOTE;
    Format := sFORMAT;
    Units  := sUNIT;
    Length := nLEN;
    Value  := EmptyStr;
  end;
end;

function TMeterDataGroup.NewItem(nSign: Int64): TMeterDataItem;
var
  nIndex : Integer;
  sSign : string;
begin
//  if nSign > $FFFF then
    sSign := IntToHex( nSign and $FFFFFFFF, 8 );
//  else
//    sSign := IntToHex( nSign and C_METER_DATA_SIGN_MAX, 4 );

  nIndex := FItems.IndexOf( sSign );

  if nIndex = -1 then
  begin
    Result := TMeterDataItem.Create;
    Result.Sign := nSign;
    FItems.AddObject( sSign, Result );
  end
  else
    Result := TMeterDataItem( FItems.Objects[ nIndex ] );
end;

procedure TMeterDataGroup.SetGroupName(const Value: string);
begin
  FGroupName := Value;
end;

procedure TMeterDataGroup.SetItemValue(nSign: Int64; const Value: string);
var
  mdItem : TMeterDataItem;
begin
  mdItem := GetItem( nSign );

  if Assigned( mdItem ) then
  begin
    mdItem.Value := Value;

    if VerifyValue then
      VerifyMeterDataItem( mdItem );
  end;
end;

{ TMETER_DATA_GROUP_DL645 }

procedure TMETER_DATA_GROUP_DL645.AddNormalData( sType: string;
  nBaseSign : Integer; nLen : Integer; sFormat, sUnitA, sUnitU : string );
  function GetNote(n1, n2, n3: Integer; sType : string ): string;
  begin
    if n1 < 2 then
      Result := '（当前）'
    else if n1 < 4 then
      Result := '（上月）'
    else
      Result := '（上上月）';

    if n3 in [ 1..$E ] then
    begin
      Result := Result + '费率' + IntToStr( n3 );
    end;

    case n2 of
      1 : Result := Result + '正向';
      2 : Result := Result + '反向';
      3 : Result := Result + '一象限';
      4 : Result := Result + '四象限';
      5 : Result := Result + '二象限';
      6 : Result := Result + '三象限';
    end;

    if n1 in [ 0, 2, 4 ] then
      Result := Result + '有功'
    else
      Result := Result + '无功';

    if n3 = 0 then
      result := result + '总' + sType
    else if n3 = $f then
      result := result + sType + '数据块'
    else
      result := result + sType;
  end;
var
  i, j, k : Integer;
  sUnit, sNote : string;
  nSign : Int64;
begin
  for i := 0 to 5 do       // 月份 , 0, 2, 4有功, 1,3,5无功
  begin
    for j := 1 to 6 do     // 象限
    begin
      for k := 0 to 15 do  // 费率
      begin
        if ( i in [ 0, 2, 4 ] ) and ( j > 2 ) then   // 有功没有多象限
          Break;

        nSign := nBaseSign;

        if i in [ 0, 2, 4 ] then     // 有功
        begin
          nSign := nSign + ( i * 2 ) shl 8 + j shl 4 + k;
          sUnit := sUnitA;
        end
        else
        begin
          nSign := nSign + ( ( i - 1 ) * 2 + 1 ) shl 8 + j shl 4 + k;
          sUnit := sUnitU;
        end;

        sNote := GetNote( i, j, k, sType );

        if k < 15 then
          NewItem( nSign, sFormat, nLen, sUnit, sNote )
        else
          NewItem( nSign, sFormat, nLen * 15, sUnit, sNote );
      end;
    end;
  end;
end;


procedure TMETER_DATA_GROUP_DL645.AddTimePeriodData;
var
  i, j : Integer;
  nSign : Int64;
  sNote : string;
begin
  for i := 1 to 15 do
  begin
    nSign := $C320 + i;
    sNote := Format( '%d时区起始日期及日时段表号', [ i ] );

    NewItem( nSign, 'MMDDNN', 3, '', sNote );
  end;

  for i := 1 to 8 do
    for j := 1 to 15 do
    begin
      nSign := $C300 + ( i + 2 ) shl 4 + j;
      sNote := Format( '第%d日时段表第%d时段起始时间及费率号', [ i, j ] );

      NewItem( nSign, 'hhmmNN', 3, '', sNote );
    end;

  for i := 1 to $D do
  begin
    nSign := $C410 + i;
    sNote := Format( '第%d公共假日日期及日时段表号', [ i ] );

    NewItem( nSign, 'MMDDNN', 3, '', sNote );
  end;
end;

procedure TMETER_DATA_GROUP_DL645.AddVariableData;
begin
  // 变量
  NewItem( $B210, 'MMDDhhmm'    , 4, '月日时分', '最近一次编程时间' );
  NewItem( $B211, 'MMDDhhmm'    , 4, '月日时分', '最近一次最大需量清零时间' );
  NewItem( $B212, '0000'        , 2, ''        , '编程次数' );
  NewItem( $B213, '0000'        , 2, ''        , '最大需量清零次数' );
  NewItem( $B214, '000000'      , 3, '分钟'    , '电池工作时间' );
  NewItem( $B310, '0000'        , 2, ''        , '总断相次数' );
  NewItem( $B311, '0000'        , 2, ''        , 'A相断相次数' );
  NewItem( $B312, '0000'        , 2, ''        , 'B相断相次数' );
  NewItem( $B313, '0000'        , 2, ''        , 'C相断相次数' );
  NewItem( $B320, '000000'      , 3, '分钟'    , '断相时间累计值' );
  NewItem( $B321, '000000'      , 3, '分钟'    , 'A断相时间累计值' );
  NewItem( $B322, '000000'      , 3, '分钟'    , 'B断相时间累计值' );
  NewItem( $B323, '000000'      , 3, '分钟'    , 'C断相时间累计值' );
  NewItem( $B330, 'MMDDhhmm'    , 4, '月日时分', '最近一次断相起始时刻' );
  NewItem( $B331, 'MMDDhhmm'    , 4, '月日时分', 'A相最近断相起始时刻' );
  NewItem( $B332, 'MMDDhhmm'    , 4, '月日时分', 'B相最近断相起始时刻' );
  NewItem( $B333, 'MMDDhhmm'    , 4, '月日时分', 'C相最近断相起始时刻' );
  NewItem( $B340, 'MMDDhhmm'    , 4, '月日时分', '最近一次断相的结束时刻' );
  NewItem( $B341, 'MMDDhhmm'    , 4, '月日时分', 'A相最近一次断相的结束时刻' );
  NewItem( $B342, 'MMDDhhmm'    , 4, '月日时分', 'B相最近一次断相的结束时刻' );
  NewItem( $B343, 'MMDDhhmm'    , 4, '月日时分', 'C相最近一次断相的结束时刻' );
  NewItem( $B611, '000'         , 2, 'V'       , 'A相电压' );
  NewItem( $B612, '000'         , 2, 'V'       , 'B相电压' );
  NewItem( $B613, '000'         , 2, 'V'       , 'C相电压' );
  NewItem( $B621, '00.00'       , 2, 'A'       , 'A相电流' );
  NewItem( $B622, '00.00'       , 2, 'A'       , 'B相电流' );
  NewItem( $B623, '00.00'       , 2, 'A'       , 'C相电流' );
  NewItem( $B630, '00.0000'     , 3, 'kW'      , '瞬时有功功率' );
  NewItem( $B631, '00.0000'     , 3, 'kW'      , 'A相有功功率' );
  NewItem( $B632, '00.0000'     , 3, 'kW'      , 'B相有功功率' );
  NewItem( $B633, '00.0000'     , 3, 'kW'      , 'C相有功功率' );
  NewItem( $B634, '00.00'       , 2, 'kW'      , '正向有功功率上限值' );
  NewItem( $B635, '00.00'       , 2, 'kW'      , '反向有功功率上限值' );
  NewItem( $B640, '00.00'       , 2, 'kvarh'   , '瞬时无功功率' );
  NewItem( $B641, '00.00'       , 2, 'kvarh'   , 'A相无功功率' );
  NewItem( $B642, '00.00'       , 2, 'kvarh'   , 'B相无功功率' );
  NewItem( $B643, '00.00'       , 2, 'kvarh'   , 'C相无功功率' );
  NewItem( $B650, '0.000'       , 2, ''        , '总功率因数' );
  NewItem( $B651, '0.000'       , 2, ''        , 'A相功率因数' );
  NewItem( $B652, '0.000'       , 2, ''        , 'B相功率因数' );
  NewItem( $B653, '0.000'       , 2, ''        , 'C相功率因数' );

  // 参变量
  NewItem( $C010, 'YYMMDDWW'    , 4, '年月日周', '日期及周次' );
  NewItem( $C011, 'hhmmss'      , 3, '时分秒'  , '时间' );
  NewItem( $C020, ''            , 1, ''        , '电表运行状态字' );
  NewItem( $C021, ''            , 1, ''        , '电网状态字' );
  NewItem( $C022, ''            , 1, ''        , '周休日状态字' );
  NewItem( $C030, '000000'      , 3, 'p/(kWh)' , '电表常数（有功）' );
  NewItem( $C031, '000000'      , 3, 'p/(kvarh)','电表常数（无功）' );
  NewItem( $C032, '000000000000', 6, ''        , '表号' );
  NewItem( $C033, '000000000000', 6, ''        , '用户号' );
  NewItem( $C034, '000000000000', 6, ''        , '设备码' );
  NewItem( $C111, '00'          , 1, '分钟'    , '最大需量周期' );
  NewItem( $C112, '00'          , 1, '分钟'    , '滑差时间' );
  NewItem( $C113, '00'          , 1, '秒'      , '循显时间' );
  NewItem( $C114, '00'          , 1, '秒'      , '停显时间 ' );
  NewItem( $C115, '00'          , 1, ''        , '显示电能小数位数 ' );
  NewItem( $C116, '00'          , 1, ''        , '显示功率（最大需量）小数位数' );
  NewItem( $C117, 'DDhh'        , 2, '日时'    , '自动抄表日期' );
  NewItem( $C118, '00'          , 1, ''        , '负荷代表日' );
  NewItem( $C119, '000000.0'    , 4, 'kWh'     , '有功电能起始读数' );
  NewItem( $C11A, '000000.0'    , 4, 'kvarh'   , '无功电能起始读数' );
  NewItem( $C211, '0000'        , 2, 'ms'      , '输出脉冲宽度' );
  NewItem( $C212, '00000000'    , 4, ''        , '密码权限及密码' );
  NewItem( $C310, '00'          , 1, ''        , '年时区数P' );
  NewItem( $C311, '00'          , 1, ''        , '日时段表数q' );
  NewItem( $C312, '00'          , 1, ''        , '日时段（每日切换数）m≤10' );
  NewItem( $C313, '00'          , 1, ''        , '费率数k≤14' );
  NewItem( $C314, '00'          , 1, ''        , '公共假日数n' );
  NewItem( $C41E, '00'          , 1, ''        , '周休日采用的日时段表号' );
  NewItem( $C510, 'MMDDhhmm'    , 4, '月日时分', '负荷记录起始时间' );
  NewItem( $C511, '0000'        , 2, '分钟'    , '负荷记录间隔时间' );
end;

constructor TMETER_DATA_GROUP_DL645.Create;
begin
  inherited;

  AddNormalData( '电能', $9000, 4, '000000.00', 'kWh', 'kvarh' );
  AddNormalData( '最大需量', $A000, 3, '00.0000', 'kW', 'kvar' );
  AddNormalData( '最大需量发生时间', $B000, 4, 'MMDDhhmm',
    '月日时分', '月日时分' );

  AddVariableData;
  AddTimePeriodData;
end;

procedure TMeterDataItem.SetValue(Val: string);
begin
  if Val <> FValue then
    FValue := Val;
end;

function TMeterDataItem.SignInHex: string;
begin
  Result := IntToHex( Sign, 8 );
end;

procedure VerifyMeterDataItem( AItem : TMeterDataItem );
begin
  AItem.Value := VerifiedStr( AItem.Value, AItem.Format, AItem.Length );
end;

function VerifiedStr( const sData, sFormat : string; nLen : Integer ) : string;
  // 修正字符串长度
  procedure ChangeStrSize( var s : string; nLen : Integer );
  var
    i : Integer;
  begin
    if Length( s ) < nLen then
    begin
      for i := 1 to nLen - Length( s ) do
        s := '0' + s;
    end
    else if Length( s ) > nLen then
    begin
      s := Copy( s, Length( s ) - nLen + 1, nLen );
    end;
  end;
var
  nLenStr : Integer;
  dTemp : Double;
  s : string;
  aBuf : TBytes;
begin
  s := Trim( sData );
  s := StringReplace( s, ' ', '', [rfReplaceAll] );
  s := StringReplace( s, '-', '', [rfReplaceAll] );
  s := StringReplace( s, ':', '', [rfReplaceAll] );

  if (sFormat <> EmptyStr) and (sFormat <> '00.0000YYMMDDhhmm') then   // 有数据格式的
  begin
    // 整理浮点数据
    if ( Pos( '0.0', sFormat ) > 0 ) then
    begin
      TryStrToFloat( s, dTemp );
      s := FormatFloat( sFormat, dTemp );
    end
    else
    begin
      if Pos('X', sFormat) > 0 then
      begin
        nLenStr := Round(Length( sFormat )/2);
        ChangeStrSize( s, nLenStr );
        aBuf := StrToPacks( s );
        s := BCDPacksToStr(aBuf);

        s := StringReplace(s, ' ', '', [rfReplaceAll]);
      end
      else
      begin
        nLenStr := Length( sFormat );
        ChangeStrSize( s, nLenStr );
      end;

    end;
  end
  else
  begin
    s := StringReplace( s, '.', '', [rfReplaceAll] );
    nLenStr := nLen * 2;    // 字节个数×2，因为是十六进制
    ChangeStrSize( s, nLenStr );

    Insert('.',s, 3);
  end;

  Result := s;
end;

function FixDataForShow( AData: string; AFormat: string; ALen: Integer) : string;
const
  C_DATETIME_STR = '%s-%s %s:%s';
var
  nRStrLen : Integer; // .右边的字符串长度
  s : string;
begin
  if Pos( '.', AFormat ) > 0 then
  begin
    nRStrLen := Length( AFormat ) - Pos( '.', AFormat );
    s := Copy( AData, 1, Length( AData ) - nRStrLen );
    s := s + '.';
    s := s + Copy( AData, Length( AData ) - nRStrLen + 1, nRStrLen );
    Result := s;
  end
  else if ( UpperCase( AFormat ) = 'MMDDHHMM' ) and ( Length( AData ) = 8 ) then
  begin
    s := AData;
    Result := Format( C_DATETIME_STR, [ Copy( s, 1, 2 ), Copy( s, 3, 2 ),
        Copy( s, 5, 2 ), Copy( s, 7, 2 ) ] );
  end;
end;

function FixDataForShow( AData: TBytes; AFormat: string; ALen: Integer) : string;
var
  s : string;
  i: Integer;
begin
  s := '';
  for i := 0 to Length(AData) - 1 do
    s :=  s + IntToHex(adata[i], 2);
  Result := FixDataForShow(s, AFormat, ALen);
end;

end.





