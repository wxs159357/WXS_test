unit xSortControl;

interface

uses xSortInfo, System.SysUtils, System.Classes, xFunction, xSortAction, xQuestionInfo;

type
  /// <summary>
  /// 题库控制类
  /// </summary>
  TSortControl = class
  private
    FSortList: TStringList;
    FSortAction : TSortAction;
    function GetSortInfo(nIndex: Integer): TSortInfo;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 题库列表
    /// </summary>
    property SortList : TStringList read FSortList write FSortList;
    property SortInfo[nIndex:Integer] : TSortInfo read GetSortInfo;

    /// <summary>
    /// 添加题库
    /// </summary>
    procedure AddSort(ASortInfo : TSortInfo);

    /// <summary>
    /// 删除题库
    /// </summary>
    procedure DelSort(nSortID : Integer);

    /// <summary>
    /// 编辑题库
    /// </summary>
    procedure EditSort(ASortInfo : TSortInfo);

    /// <summary>
    /// 加载题库
    /// </summary>
    procedure LoadSort;

    /// <summary>
    /// 清空题库
    /// </summary>
    procedure ClearSrot;

    /// <summary>
    /// 获取考题
    /// </summary>
    function GetQInfo(nQID : Integer) : TQuestionInfo;

  end;
var
  SortControl : TSortControl;

implementation

{ TSortControl }

procedure TSortControl.AddSort(ASortInfo: TSortInfo);
begin
  if Assigned(ASortInfo) then
  begin
    ASortInfo.SortID := FSortAction.GetMaxSN + 1;

    FSortList.AddObject('', ASortInfo);
    FSortAction.AddSort(ASortInfo);
  end;
end;

procedure TSortControl.ClearSrot;
var
  i : Integer;
begin
  for i := FSortList.Count - 1 downto 0 do
  begin
    TSortInfo(FSortList.Objects[i]).ClearQuestion;
    FSortAction.DelSort(TSortInfo(FSortList.Objects[i]).SortID);
    TSortInfo(FSortList.Objects[i]).Free;
    FSortList.Delete(i);
  end;
end;

constructor TSortControl.Create;
begin
  FSortList:= TStringList.Create;
  FSortAction := TSortAction.Create;

  LoadSort;
end;

procedure TSortControl.DelSort(nSortID: Integer);
var
  i : Integer;
begin
  for i := FSortList.Count - 1 downto 0 do
  begin
    if TSortInfo(FSortList.Objects[i]).Sortid = nSortID then
    begin
      TSortInfo(FSortList.Objects[i]).ClearQuestion;
      FSortAction.DelSort(nSortID);
      TSortInfo(FSortList.Objects[i]).Free;
      FSortList.Delete(i);
      Break;
    end;
  end;
end;

destructor TSortControl.Destroy;
begin
  ClearStringList(FSortList);
  FSortList.Free;
  FSortAction.Free;
  inherited;
end;

procedure TSortControl.EditSort(ASortInfo: TSortInfo);
begin
  FSortAction.EditSort(ASortInfo);
end;

function TSortControl.GetQInfo(nQID: Integer): TQuestionInfo;
var
  i : Integer;
  ASort : TSortInfo;
begin
  Result := nil;
  for i := FSortList.Count - 1 downto 0 do
  begin
    ASort := TSortInfo(FSortList.Objects[i]);
    ASort.LoadQuestion;
    Result := ASort.GetQInfo(nQID);

    if Assigned(Result) then
      Break;
  end;
end;

function TSortControl.GetSortInfo(nIndex: Integer): TSortInfo;
begin
  if (nIndex >= 0) and (nIndex < FSortList.Count) then
  begin
    Result := TSortInfo(FSortList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TSortControl.LoadSort;
begin
  FSortAction.LoadSort(FSortList);
end;

end.
