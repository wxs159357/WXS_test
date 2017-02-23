unit xVectorMap;

interface

uses Classes, SysUtils, xVectorLine, XMLIntf,
  XMLDoc,Variants, FMX.Graphics, System.UITypes, system.Types, FMX.Types,
  FMX.Controls, FMX.StdCtrls, FMX.Objects, xVectorType, xVectorArc;

type
  TVectorMap = class
  private
    FVectorList: TStringList;
    FOwner : TComponent;
    XMLDocument1: TXMLDocument;
    FRect: TRectF;
    FText : TText;
    FCanvas: TControl;
    FBackColor: TAlphaColor;
    FRectangle: TRectangle;
    FTempVectorA, FTempVectorB, FTempVectorC : TVectorLineInfo;
    FLineH, FLineV : TLine;
    FArcList: TStringList;
    FVectorMapName: string;
    FIsCanSelect: Boolean;
    FOnSelectVector: TNotifyEvent;
    FOnSelectArc: TNotifyEvent;
    function GetVectorInfo(nIndex: Integer): TVectorLineInfo;
    procedure SetVectorStr(const Value: string);
    function GetVectorStr: string;
    function GetArcInfo(nIndex: Integer): TVectorArc;
    procedure ClearList(slList : TStringList);
    function GetMaxWidth : Single;

    /// <summary>
    /// 获取向量最大长度
    /// </summary>
    function GetMaxVectorLen(AVType: tTVectorType) : Single;

    /// <summary>
    /// 根据ID获取向量
    /// </summary>
    function GetVectorInfoByID(nVID : Integer) : TVectorLineInfo;
    procedure SetIsCanSelect(const Value: Boolean);

    /// <summary>
    /// 向量双击事件
    /// </summary>
    procedure VectorDblClick(Sender: TObject);

    /// <summary>
    /// 向量鼠标按下事件
    /// </summary>
    procedure VectorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);

    /// <summary>
    /// 背景图鼠标按下事件
    /// </summary>
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SetCanvas(const Value: TControl);


    /// <summary>
    /// 弧线鼠标按下事件
    /// </summary>
    procedure ArcMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  public
    /// <summary>
    /// 向量列表
    /// </summary>
    property VectorList : TStringList read FVectorList write FVectorList;
    property VectorInfo[nIndex : Integer] : TVectorLineInfo read GetVectorInfo;

    /// <summary>
    /// 设置全部向量处在不选择状态
    /// </summary>
    procedure SetNoSelect;
    procedure SetNoMainSelect;

    /// <summary>
    /// 添加向量
    /// </summary>
    function AddVector : TVectorLineInfo;

    /// <summary>
    /// 删除向量
    /// </summary>
    procedure DelVector(nVectorID : Integer);

    /// <summary>
    /// 背景图
    /// </summary>
    property BackColor : TAlphaColor read FBackColor write FBackColor;

    /// <summary>
    /// 向量图描述
    /// </summary>
    property VectorMapName : string read FVectorMapName write FVectorMapName;

    /// <summary>
    /// 删除选择向量
    /// </summary>
    procedure DelSelect;

    /// <summary>
    /// 获取选择的向量图个数
    /// </summary>
    function GetSelectVectorCount : Integer;

    /// <summary>
    /// 弧线列表
    /// </summary>
    property ArcList : TStringList read FArcList write FArcList;
    property ArcInfo[nIndex : Integer] : TVectorArc read GetArcInfo;

    /// <summary>
    /// 添加弧线
    /// </summary>
    function AddArc : TVectorArc;
    function AddVectorArc: TVectorArc;

    /// <summary>
    /// 删除弧线
    /// </summary>
    procedure DelArc(nArcID : Integer);

    /// <summary>
    /// 删除向量相关的弧线
    /// </summary>
    procedure DelVectorArc(nVectorID : Integer);

    /// <summary>
    /// 设置全部弧线处在不选择状态
    /// </summary>
    procedure SetArcNoSelect;

  public
    constructor Create(Owner : TComponent);
    destructor Destroy; override;

    /// <summary>
    /// 最大电压值
    /// </summary>
    function GetMaxVolValue : Double;

    /// <summary>
    /// 最大电流值
    /// </summary>
    function GetMaxCurValue : Double;

    /// <summary>
    /// 向量图描述
    /// </summary>
    property VectorStr : string read GetVectorStr write SetVectorStr;

    /// <summary>
    /// *画布 必须赋值
    /// </summary>
    property Canvas : TControl read FCanvas write SetCanvas;

    /// <summary>
    /// *绘制区域 必须赋值
    /// </summary>
    property Rect : TRectF read FRect write FRect;

    /// <summary>
    /// 绘制
    /// </summary>
    procedure Draw;

    /// <summary>
    /// 是否允许选择 默认 允许
    /// </summary>
    property IsCanSelect : Boolean read FIsCanSelect write SetIsCanSelect;

    /// <summary>
    /// 选择向量事件
    /// </summary>
    property OnSelectVector : TNotifyEvent read FOnSelectVector write FOnSelectVector;

    /// <summary>
    /// 选择弧线事件
    /// </summary>
    property OnSelectArc : TNotifyEvent read FOnSelectArc write FOnSelectArc;
  end;
var
  AVectorControl : TVectorMap;


implementation

{ TVectorMap }

function TVectorMap.AddArc: TVectorArc;
  function GetMaxID : Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to FArcList.Count - 1 do
    begin
      if ArcInfo[i].VID > Result then
        Result := ArcInfo[i].VID;
    end;
  end;
begin
  Result := TVectorArc.Create;
  Result.VID := GetMaxID + 1;
  Result.ArcR := GetMaxWidth/15*Result.VID+GetMaxWidth/10;
  Result.OnMouseDown := ArcMouseDown;

  FArcList.AddObject('', Result);
end;

function TVectorMap.AddVectorArc:TVectorArc;
  function GetMainVercor(bMain : Boolean) : TVectorLineInfo;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to FVectorList.Count - 1 do
    begin
      if (VectorInfo[i].IsMainSelect = bMain) and VectorInfo[i].IsSelected then
      begin
        Result := VectorInfo[i];
        Break;
      end;
    end;
  end;
var
  AMain, AOther : TVectorLineInfo;
  ACenterPoint : TPointF;
begin
  Result := nil;
  AMain := GetMainVercor(True);
  AOther := GetMainVercor(False);

  if Assigned(AMain) and Assigned(AOther) then
  begin
    ACenterPoint.X := FRect.Width/2;
    ACenterPoint.y := FRect.Height/2;

    Result := AddArc;
    Result.StartVector := AMain;
    Result.EndVector := AOther;
    Result.CenterPoint := ACenterPoint;

    Result.Canvas := FCanvas;

    Result.Draw;
    Result.FArc.SendToBack;
    FRectangle.SendToBack;
  end;
end;

function TVectorMap.AddVector: TVectorLineInfo;
  function GetMaxID : Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to FVectorList.Count - 1 do
    begin
      if VectorInfo[i].VID > Result then
        Result := VectorInfo[i].VID;
    end;
  end;
var
  ACenterPoint : TPointF;
begin
  ACenterPoint.X := FRect.Width/2;
  ACenterPoint.y := FRect.Height/2;

  Result := TVectorLineInfo.Create;
  Result.VID := GetMaxID + 1;
  Result.OnDblClick := VectorDblClick;
  Result.OnMouseDown := VectorMouseDown;
  Result.IsCanSelect := FIsCanSelect;
  Result.CenterPoint := ACenterPoint;
  Result.Canvas := FCanvas;
  if Result.VType = vtVol then
    Result.VMaxValue := GetMaxVolValue
  else
    Result.VMaxValue := GetMaxCurValue;

  FVectorList.AddObject('', Result);
end;

procedure TVectorMap.ArcMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SetArcNoSelect;

  TVectorArc(Sender).IsSelected := True;

  if Assigned(FOnSelectArc) then
    FOnSelectArc(Sender);
end;

procedure TVectorMap.ClearList(slList : TStringList);
var
  i: Integer;
begin
  for i := slList.Count - 1 downto 0 do
    slList.Objects[i].Free;

  slList.Clear;
end;

constructor TVectorMap.Create(Owner : TComponent);
begin
  FOwner := Owner;
  XMLDocument1:= TXMLDocument.Create(FOwner);
  FVectorList:= TStringList.Create;
  FBackColor := $FFFFFFFF;;
  FArcList := TStringList.Create;
  FIsCanSelect := True;
end;

procedure TVectorMap.DelArc(nArcID: Integer);
var
  i: Integer;
  AArc : TVectorArc;
begin
  for i := FArcList.Count - 1 downto 0 do
  begin
    AArc := ArcInfo[i];

    if Assigned(AArc) then
    begin
      if AArc.VID = nArcID then
      begin
        AArc.Free;
        FArcList.Delete(i);
      end;
    end;
  end;
end;

procedure TVectorMap.DelSelect;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  for i := FVectorList.Count - 1 downto 0 do
  begin
    AVLInfo := VectorInfo[i];
    if AVLInfo.IsSelected then
    begin
      // 删除向量相关的弧线
      DelVectorArc(AVLInfo.VID);

      // 删除向量图
      AVLInfo.Free;
      FVectorList.Delete(i);
    end;
  end;
end;

procedure TVectorMap.DelVector(nVectorID: Integer);
var
  i: Integer;
  AVLineInfo : TVectorLineInfo;
begin
  for i := FVectorList.Count - 1 downto 0 do
  begin
    AVLineInfo := VectorInfo[i];

    if Assigned(AVLineInfo) then
    begin
      if AVLineInfo.VID = nVectorID then
      begin
        AVLineInfo.Free;
        FVectorList.Delete(i);
      end;
    end;
  end;
end;

procedure TVectorMap.DelVectorArc(nVectorID: Integer);
var
  i: Integer;
  AArc : TVectorArc;
begin
  for i := FArcList.Count - 1 downto 0 do
  begin
    AArc := ArcInfo[i];

    if Assigned(AArc) then
    begin
      if (AArc.StartVector.VID = nVectorID) or (AArc.EndVector.VID = nVectorID) then
      begin
        AArc.Free;
        FArcList.Delete(i);
      end;
    end;
  end;
end;

destructor TVectorMap.Destroy;
begin
  ClearList(FVectorList);
  FVectorList.Free;
  ClearList(FArcList);
  FArcList.Free;

  XMLDocument1.Free;
  if Assigned(FText) then
    FText.Free;
  if Assigned(FLineV) then
    FLinev.Free;
  if Assigned(FLineH) then
    FLineH.Free;
  if Assigned(FRectangle) then
    FRectangle.Free;

  if Assigned(FTempVectorA) then
    FTempVectorA.Free;
  if Assigned(FTempVectorB) then
    FTempVectorB.Free;
  if Assigned(FTempVectorC) then
    FTempVectorC.Free;

  inherited;
end;

procedure TVectorMap.Draw;
var
  AVector : TVectorLineInfo;
  AArc : TVectorArc;
  i: Integer;
  ACenterPoint : TPointF;
  dMaxVolValue, dMaxCurrentValue : Single;

  procedure DrawVector( AVec: TVectorLineInfo; dAngle : Double; sName : string);
  begin
    if not Assigned(AVec) then
      AVec := TVectorLineInfo.Create;
    AVec.VAngle := dAngle;
    AVec.CenterPoint := ACenterPoint;
    AVec.Canvas := FCanvas;
    AVec.VMaxValue := dMaxVolValue;
    AVec.VMaxWidth := GetMaxWidth;
    AVec.VColor := $3FC0C0C0;
    AVec.VDash := TStrokeDash.Solid;
    AVec.VName := sName;
    AVec.IsCanSelect := False;
    AVec.IsDrawPoint := False;
    AVec.Draw;

  end;
begin
  if not Assigned(FCanvas) then
    Exit;

  ACenterPoint.X := FRect.Width/2;
  ACenterPoint.y := FRect.Height/2;

  // 画背景
  if not Assigned(FRectangle) then
    FRectangle:= TRectangle.Create(FCanvas);
  FRectangle.Parent := FCanvas;
  FRectangle.OnMouseDown := MouseDown;

  FRectangle.Stroke.Dash := TStrokeDash.Dash;
  FRectangle.Position.X := 0;
  FRectangle.Position.Y := 0;
  FRectangle.Width := FRect.Width;
  FRectangle.Height := FRect.Height;
  FRectangle.Fill.Color := FBackColor;
  FRectangle.Stroke.Color := $FFC0C0C0;
  FRectangle.Stroke.Thickness := 1;

  // 画背景交叉线
  if not Assigned(FLineH) then
    FLineH := TLine.Create(FRectangle);
  FLineH.Parent := FRectangle;

  FLineH.Position.X := 0;
  FLineH.Position.y := FRectangle.Height/2-1;
  FLineH.Width := FRectangle.Width;
  FLineH.Height := 1;

  FLineH.LineType := TLineType.Top;
  FLineH.Stroke.Color := $FFC0C0C0;
  FLineH.Stroke.Dash := TStrokeDash.Dash;

  if not Assigned(FLineV) then
    FLineV := TLine.Create(FRectangle);
  FLineV.Parent := FRectangle;

  FLineV.Position.X := FRectangle.Width/2;
  FLineV.Position.y := 0;
  FLineV.Width := 1;
  FLineV.Height := FRectangle.Height;

  FLineV.LineType := TLineType.Left;
  FLineV.Stroke.Color := $FFC0C0C0;
  FLineV.Stroke.Dash := TStrokeDash.Dash;

  dMaxVolValue := GetMaxVolValue;
  dMaxCurrentValue := GetMaxCurValue;

  // 画描述
  if not Assigned(FText) then
    FText := TText.Create(FRectangle);
  FText.Parent := FRectangle;
  FText.Align := TAlignLayout.Bottom;
  FText.Color := FBackColor xor $00C0C0C0;
  FText.Text := FVectorMapName;

  // 画辅助向量
  DrawVector(FTempVectorA, 90, 'Ua');
  DrawVector(FTempVectorB, 330, 'Ub');
  DrawVector(FTempVectorC, 210, 'Uc');

  // 画弧线
  for i := 0 to FArcList.Count - 1 do
  begin
    AArc := ArcInfo[i];
    AArc.CenterPoint := ACenterPoint;
    AArc.Canvas := FCanvas;

    AArc.Draw;
    AArc.FArc.SendToBack;
  end;

  // 画向量
  for i := 0 to FVectorList.Count - 1 do
  begin
    AVector := VectorInfo[i];
    AVector.CenterPoint := ACenterPoint;
    AVector.Canvas := FCanvas;
    if AVector.VType = vtVol then
      AVector.VMaxValue := dMaxVolValue
    else
      AVector.VMaxValue := dMaxCurrentValue;

    AVector.VMaxWidth := GetMaxWidth;
    AVector.Draw;
  end;
  FRectangle.SendToBack;
end;

function TVectorMap.GetArcInfo(nIndex: Integer): TVectorArc;
begin
  if (nIndex >= 0) and (nIndex < FArcList.Count) then
  begin
    Result := TVectorArc(FArcList.Objects[nIndex]);
  end
  else
    Result := nil;
end;

function TVectorMap.GetMaxCurValue: Double;
begin
  Result := GetMaxVectorLen(vtCurrent)+2;
end;

function TVectorMap.GetMaxVectorLen(AVType: tTVectorType): Single;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  Result := 0;

  for i := 0 to FVectorList.Count - 1 do
  begin
    AVLInfo := VectorInfo[i];

    if AVLInfo.VType = AVType then
    begin
      if AVLInfo.VValue > Result then
        Result := AVLInfo.VValue;
    end;
  end;
end;

function TVectorMap.GetMaxVolValue: Double;
begin
  Result := GetMaxVectorLen(vtVol);

end;

function TVectorMap.GetMaxWidth: Single;
begin
  if FRect.Height > FRect.Width then
  begin
    Result := FRect.Width/2-35;
  end
  else
  begin
    Result := FRect.Height/2-35;
  end;

end;

function TVectorMap.GetSelectVectorCount: Integer;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  Result := 0;

  for i := 0 to FVectorList.Count - 1 do
  begin
    AVLInfo := VectorInfo[i];

    if AVLInfo.IsSelected then
    begin
      Inc(Result);
    end;
  end;
end;

function TVectorMap.GetVectorInfo(nIndex: Integer): TVectorLineInfo;
begin
  if (nIndex >= 0) and (nIndex < FVectorList.Count) then
  begin
    Result := TVectorLineInfo(FVectorList.Objects[nIndex]);
  end
  else
    Result := nil;
end;

function TVectorMap.GetVectorInfoByID(nVID: Integer): TVectorLineInfo;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  Result := nil;

  for i := 0 to FVectorList.Count - 1 do
  begin
    AVLInfo := VectorInfo[i];

    if AVLInfo.VID = nVID then
    begin
      Result := AVLInfo;
      Break;
    end;
  end;
end;

function TVectorMap.GetVectorStr: string;
const
  C_XML = '<LineInfo VID ="%d" VName ="%s" VType="%s" VColor="%d" VValue="%f" VAngle="%f" VDrawPoint="%s">%s</LineInfo>';

  C_XML_ARC = '<ArcInfo VID ="%d" VName ="%s" StartVectorName="%s" EndVectorName="%s" StartVectorID="%d" EndVectorID="%d"> </ArcInfo>';
var
  i: Integer;
begin
  Result := '<?xml version="1.0" encoding="gb2312"?>' + #13#10;
  Result := Result + '<VectorMap>' + #13#10;
  Result := Result + '<VectorLine>' + #13#10;
  for i := 0 to FVectorList.Count - 1 do
  begin
    with VectorInfo[i] do
    begin
      Result := Result + Format(C_XML, [VID, VName, VTypeStr, VColor, VValue, VAngle,BoolToStr(IsDrawPoint), VName ]) + #13#10;
    end;
  end;

  Result := Result + '</VectorLine>' + #13#10;

  Result := Result + '<VectorArc>' + #13#10;
  for i := 0 to FArcList.Count - 1 do
  begin
    with ArcInfo[i] do
    begin
      if Assigned(StartVector) and Assigned(EndVector) then
      begin
        Result := Result + Format(C_XML_ARC, [VID, VName, StartVector.VName, EndVector.VName, StartVector.VID, EndVector.VID]) + #13#10;
      end;
    end;
  end;

  Result := Result + '</VectorArc>' + #13#10;

  Result := Result + '</VectorMap>' + #13#10;
end;

procedure TVectorMap.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SetNoSelect;
  SetArcNoSelect;

  if Assigned(FOnSelectVector) then
    FOnSelectVector(nil);

  if Assigned(FOnSelectArc) then
    FOnSelectArc(nil);
end;

procedure TVectorMap.VectorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
  procedure SetVectorMain;
  var
    i: Integer;
    AVLInfo : TVectorLineInfo;
  begin
    for i := 0 to FVectorList.Count - 1 do
    begin
      AVLInfo := VectorInfo[i];

      if AVLInfo.IsSelected then
      begin
        AVLInfo.IsMainSelect := True;
        Break;
      end;
    end;
  end;
var
  nCount : Integer;
begin
  nCount := GetSelectVectorCount;

  if ssCtrl in Shift then
  begin
    if nCount = 0 then
    begin
      TVectorLineInfo(Sender).IsSelected := true;
      TVectorLineInfo(Sender).IsMainSelect := true;
    end
    else
    begin
      TVectorLineInfo(Sender).IsSelected := not TVectorLineInfo(Sender).IsSelected;

      if not TVectorLineInfo(Sender).IsSelected then
      begin
        TVectorLineInfo(Sender).IsMainSelect := False;
        SetVectorMain;
      end;
    end;
    TVectorLineInfo(Sender).Draw;
  end
  else
  begin
    if TVectorLineInfo(Sender).IsSelected then
    begin
      SetNoMainSelect;
      TVectorLineInfo(Sender).IsMainSelect := true;
    end
    else
    begin
      SetNoSelect;
      TVectorLineInfo(Sender).IsSelected := true;
      TVectorLineInfo(Sender).IsMainSelect := true;
    end;

    TVectorLineInfo(Sender).Draw;
  end;

  if Assigned(FOnSelectVector) then
    FOnSelectVector(Sender);
end;

procedure TVectorMap.SetArcNoSelect;
var
  i: Integer;
begin
  for i := 0 to FArcList.Count - 1 do
  begin
    if  ArcInfo[i].IsSelected then
    begin
      ArcInfo[i].IsSelected := False;
      ArcInfo[i].Draw;
    end;
  end;

end;

procedure TVectorMap.SetCanvas(const Value: TControl);
begin
  if FCanvas <> Value then
  begin
    FCanvas := Value;
  end;
end;

procedure TVectorMap.SetIsCanSelect(const Value: Boolean);
var
  i: Integer;
begin
  FIsCanSelect := Value;
  for i := 0 to FVectorList.Count - 1 do
    VectorInfo[i].IsCanSelect := Value;
end;

procedure TVectorMap.SetNoMainSelect;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  for i := 0 to FVectorList.Count - 1 do
  begin
    AVLInfo := VectorInfo[i];
    AVLInfo.IsMainSelect := False;
    AVLInfo.Draw;
  end;
end;

procedure TVectorMap.SetNoSelect;
var
  i: Integer;
  AVLInfo : TVectorLineInfo;
begin
  for i := 0 to FVectorList.Count - 1 do
  begin
    AVLInfo := VectorInfo[i];

    AVLInfo.IsSelected := False;
    AVLInfo.IsMainSelect := False;
    AVLInfo.Draw;
  end;
end;

procedure TVectorMap.SetVectorStr(const Value: string);
  function GetStrValue(sValue : string; Anode:IXMLNode) : string;
  var
    oValue : OleVariant;
  begin
    oValue := Anode.Attributes[sValue];
    if oValue <> null then
      Result := oValue
    else
      Result := '';
  end;

  function GetIntValue(sValue : string; Anode:IXMLNode) : Integer;
  var
    oValue : OleVariant;
  begin
    oValue := Anode.Attributes[sValue];
    if oValue <> null then
      Result := oValue
    else
      Result := 0;
  end;

  function GetFloatValue(sValue : string; Anode:IXMLNode) : Double;
  var
    oValue : OleVariant;
  begin
    oValue := Anode.Attributes[sValue];
    if oValue <> null then
      Result := oValue
    else
      Result := 0;
  end;

  function GetBoolValue(sValue : string; Anode:IXMLNode) : Boolean;
  var
    oValue : OleVariant;
  begin
    oValue := Anode.Attributes[sValue];
    if oValue <> null then
      Result := oValue
    else
      Result := True;
  end;
var
  node, nodeInfo: IXMLNode;
  i: Integer;
  j: Integer;
  AVLInfo : TVectorLineInfo;
  AArcInfo : TVectorArc;
begin
  ClearList(FVectorList);
  ClearList(FArcList);
  XMLDocument1.XML.Text := Value;
  XMLDocument1.Active := True;

  for i := 0 to XMLDocument1.DocumentElement.ChildNodes.Count - 1 do
  begin
    node := XMLDocument1.DocumentElement.ChildNodes[i];

    if node.NodeName = 'VectorLine' then
    begin
      for j := 0 to node.ChildNodes.Count - 1 do
      begin
        nodeInfo := node.ChildNodes[j];
        AVLInfo := AddVector;
        AVLInfo.VID := GetIntValue('VID', nodeInfo);
        AVLInfo.VName := GetStrValue('VName', nodeInfo);
        AVLInfo.VTypeStr := GetStrValue('VType', nodeInfo);
        AVLInfo.VColor := GetIntValue('VColor', nodeInfo);
        AVLInfo.VValue := GetFloatValue('VValue', nodeInfo);
        AVLInfo.VAngle := GetFloatValue('VAngle', nodeInfo);
        AVLInfo.IsDrawPoint := GetBoolValue('VDrawPoint', nodeInfo);
      end;
    end;
  end;

  for i := 0 to XMLDocument1.DocumentElement.ChildNodes.Count - 1 do
  begin
    node := XMLDocument1.DocumentElement.ChildNodes[i];

    if node.NodeName = 'VectorArc' then
    begin
      for j := 0 to node.ChildNodes.Count - 1 do
      begin
        nodeInfo := node.ChildNodes[j];
        AArcInfo := AddArc;
        AArcInfo.VID := GetIntValue('VID', nodeInfo);
        AArcInfo.VName := GetStrValue('VName', nodeInfo);
        AArcInfo.StartVector := GetVectorInfoByID(GetIntValue('StartVectorID', nodeInfo));
        AArcInfo.EndVector := GetVectorInfoByID(GetIntValue('EndVectorID', nodeInfo));
      end;
    end;
  end;

  Draw;
end;

procedure TVectorMap.VectorDblClick(Sender: TObject);
begin

end;

end.


