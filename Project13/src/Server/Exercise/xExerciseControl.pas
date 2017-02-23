unit xExerciseControl;

interface

uses xExerciseInfo, System.SysUtils, System.Classes, xFunction, xExerciseAction;

type
  /// <summary>
  /// 控制类
  /// </summary>
  TExerciseControl = class
  private
    FExerciseList: TStringList;
    FExerciseAction : TExerciseAction;
    FCurrentPath: string;
    function GetExerciseInfo(nIndex: Integer): TExerciseInfo;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 当前目录
    /// </summary>
    property CurrentPath : string read FCurrentPath write FCurrentPath;

    /// <summary>
    /// 列表
    /// </summary>
    property ExerciseList : TStringList read FExerciseList write FExerciseList;
    property ExerciseInfo[nIndex:Integer] : TExerciseInfo read GetExerciseInfo;

    /// <summary>
    /// 添加
    /// </summary>
    procedure AddExercise(AExerciseInfo : TExerciseInfo);

    /// <summary>
    /// 删除
    /// </summary>
    procedure DelExercise(nExerciseID : Integer); overload;
    /// <summary>
    /// 删除
    /// </summary>
    /// <param name="bIsDelPath">如果是目录，是否删除目录</param>
    procedure DelExercise(AExercise : TExerciseInfo; bIsDelPath: Boolean); overload;


    /// <summary>
    /// 编辑
    /// </summary>
    procedure EditExercise(AExerciseInfo : TExerciseInfo);

    /// <summary>
    /// 加载
    /// </summary>
    procedure LoadExercise(sPath : string = '');

    /// <summary>
    /// 上一层目录
    /// </summary>
    procedure LoadPreviousPath;

    /// <summary>
    /// 当前目录先是否存在名
    /// </summary>
    function IsExist(sEName : string) : Boolean;

    /// <summary>
    /// 清空
    /// </summary>
    procedure ClearExercise;

  end;
var
  ExerciseControl : TExerciseControl;
implementation

{ TExerciseControl }

procedure TExerciseControl.AddExercise(AExerciseInfo: TExerciseInfo);
begin
  if Assigned(AExerciseInfo) then
  begin
    AExerciseInfo.ID := FExerciseAction.GetMaxSN + 1;

    FExerciseList.AddObject('', AExerciseInfo);
    FExerciseAction.AddExercise(AExerciseInfo);
  end;
end;

procedure TExerciseControl.ClearExercise;
var
  i : Integer;
begin
  for i := FExerciseList.Count - 1 downto 0 do
  begin
    FExerciseAction.DelExercise(TExerciseInfo(FExerciseList.Objects[i]).ID);
    TExerciseInfo(FExerciseList.Objects[i]).Free;
    FExerciseList.Delete(i);
  end;
end;

constructor TExerciseControl.Create;
begin
  FExerciseList:= TStringList.Create;
  FExerciseAction := TExerciseAction.Create;

  LoadExercise;
end;

procedure TExerciseControl.DelExercise(nExerciseID: Integer);
var
  i : Integer;
begin
  for i := FExerciseList.Count - 1 downto 0 do
  begin
    if TExerciseInfo(FExerciseList.Objects[i]).id = nExerciseID then
    begin
      FExerciseAction.DelExercise(nExerciseID);
      TExerciseInfo(FExerciseList.Objects[i]).Free;
      FExerciseList.Delete(i);
      Break;
    end;
  end;
end;

procedure TExerciseControl.DelExercise(AExercise: TExerciseInfo;
  bIsDelPath: Boolean);
var
  i : Integer;
begin
  for i := FExerciseList.Count - 1 downto 0 do
  begin
    if TExerciseInfo(FExerciseList.Objects[i]).id = AExercise.Id then
    begin
      FExerciseAction.DelExercise(AExercise, bIsDelPath);
      TExerciseInfo(FExerciseList.Objects[i]).Free;
      FExerciseList.Delete(i);
      Break;
    end;
  end;
end;

destructor TExerciseControl.Destroy;
begin
  ClearStringList(FExerciseList);
  FExerciseList.Free;
  FExerciseAction.Free;
  inherited;
end;

procedure TExerciseControl.EditExercise(AExerciseInfo: TExerciseInfo);
begin
  FExerciseAction.EditExercise(AExerciseInfo);
end;

function TExerciseControl.GetExerciseInfo(nIndex: Integer): TExerciseInfo;
begin
  if (nIndex >= 0) and (nIndex < FExerciseList.Count) then
  begin
    Result := TExerciseInfo(FExerciseList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TExerciseControl.IsExist(sEName: string): Boolean;
var
  i : Integer;
  AInfo : TExerciseInfo;
begin
  Result := False;

  if sEName = '' then
  begin
    Result := True;
  end
  else
  begin
    for i := 0 to FExerciseList.Count -1 do
    begin
      AInfo := TExerciseInfo(FExerciseList.Objects[i]);

      if AInfo.Ename = sEName then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TExerciseControl.LoadExercise(sPath : string);
begin
  FCurrentPath := sPath;
  FExerciseAction.LoadExercise(sPath, FExerciseList);
end;

procedure TExerciseControl.LoadPreviousPath;
var
  nIndex : Integer;
begin
  nIndex := FCurrentPath.LastIndexOf('\');
  LoadExercise(Copy(FCurrentPath, 1, nIndex));
end;

end.


