unit xDataDictionary;

interface

uses xDBActionBase,  System.Classes, System.SysUtils, FireDAC.Stan.Param, xFunction;

type
  /// <summary>
  /// 数据字典类
  /// </summary>
  TDataDictionary = class(TDBActionBase)

  private
    FDictionaries: TStringList;
    function GetDictionary(sName: string): TStringList;
    procedure SetDictionary(sName: string; const Value: TStringList);


  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 数据字典列表
    /// </summary>
    property Dictionaries : TStringList read FDictionaries ;

    /// <summary>
    /// 数据字典的数据项
    /// </summary>
    /// <param name=" sName "> 数据项名称 </param>
    property Dictionary[ sName : string ] : TStringList read GetDictionary write SetDictionary;

    /// <summary>
    /// 获取数据字典列表
    /// </summary>
    /// <returns>字典名称列表</returns>
    procedure LoadFromDB;

    /// <summary>
    /// 保存数据字典列表
    /// </summary>
    procedure SaveToDB;
  end;
var
  DataDict : TDataDictionary;

implementation

{ TDataDictionary }

constructor TDataDictionary.Create;
begin
  inherited;
  FDictionaries := TStringList.Create;

  try
    LoadFromDB;
  except
  end;
end;

destructor TDataDictionary.Destroy;
begin
  ClearStringList( FDictionaries );
  FDictionaries.Free;

  inherited;
end;

function TDataDictionary.GetDictionary(sName: string): TStringList;
var
  nIndex : Integer;
begin
  nIndex := FDictionaries.IndexOf( sName );
  if nIndex < 0 then
  begin
    Result := TStringList.Create;
    Dictionaries.AddObject( sName, Result );
  end
  else
    Result := TStringList( FDictionaries.Objects[ nIndex ] );
end;

procedure TDataDictionary.LoadFromDB;
const
  C_SEL = ' select * from UserDictionary order by DicSN ';
var
  Items : TStringList;
begin

  FQuery.SQL.Text := C_SEL;

  try
    FQuery.Open;
    while  not FQuery.Eof do
    begin
      Items := TStringList.Create;
      Items.Text := FQuery.FieldByName( 'DicValue' ).AsString;
      FDictionaries.AddObject( FQuery.FieldByName( 'DicName'  ).AsString, Items );
      FQuery.Next;
    end;
    FQuery.Close;
  finally

  end;
end;

procedure TDataDictionary.SaveToDB;
const
  C_DEL = ' delete from  UserDictionary ' ;

  C_INS = ' insert into UserDictionary ( DicSN, DicName, DicValue )' +
    ' values (  :DicSN, :DicName, :DicValue ) ';
var
  i : Integer;
begin

  try
    //清空表中记录
    FQuery.SQL.Text := C_DEL;
    ExecSQL;

    // 插入新的数据
    for i := 0 to FDictionaries.Count - 1 do
    begin
      FQuery.SQL.Text := C_INS;
      with FQuery.Params, FDictionaries do
      begin
        ParamByName( 'DicSN' ).Value := i;
        ParamByName( 'DicName'  ).Value := Strings[ i ] ;
        ParamByName( 'DicValue' ).Value := TStringList(
          FDictionaries.Objects[ i ] ).Text;
      end;
      ExecSQL;
    end;
  finally

  end;
end;

procedure TDataDictionary.SetDictionary(sName: string;
  const Value: TStringList);
var
  nIndex : Integer;
begin
  // 数据项在字典中的位置
  nIndex := FDictionaries.IndexOf( sName );

  // 如果没有对应的数据，退出
  if nIndex < 0 then
    Exit;

  // 给字典数据项赋值
  TStringList( FDictionaries.Objects[ nIndex ] ).Text := Value.Text ;
end;

end.
