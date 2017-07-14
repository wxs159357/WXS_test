unit FrmTrain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ImgList, System.ImageList, xTrainQuestionControl, xTrainQuestionInfo,
  xQuestionInfo, uQuestionList, FrmQuestionListC, FrmQInfoC, Vcl.Buttons,
  xThreadUDPSendScreen, jpeg, FrmPosState, xExamControl;

type
  TfTrain = class(TForm)
    pnl2: TPanel;
    spltr1: TSplitter;
    grp1: TGroupBox;
    grp2: TGroupBox;
    pnl3: TPanel;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    pgcntrl1: TPageControl;
    tbsht1: TTabSheet;
    tbsht2: TTabSheet;
    imglstil1: TImageList;
    imglst1: TImageList;
    tv1: TTreeView;
    btn7: TButton;
    spltr2: TSplitter;
    btn6: TSpeedButton;
    btn1: TButton;
    lbl1: TLabel;
    img1: TImage;
    tmr1: TTimer;
    tbsht3: TTabSheet;
    procedure btn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure tv1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
    FTCControl : TTrainQuestionControl;
    FQInfo : TQuestionInfo;
    FFormQInfo : TfQInfo;
    FSelectNode : TTreeNode;

    fullCanvas : TCanvas;
    Fullscreen, Fullscreen1 :Tbitmap;
    AJpeg : TJPEGImage;

    FromPosState : TfPosState;

    /// <summary>
    /// 窗口截屏
    /// </summary>
    procedure GetSreen1(Sender: TObject);


    procedure LoadInfo;
    /// <summary>
    /// 节点是否存在子节点
    /// </summary>
    function HasNoteStr(ANode: TTreeNode; sStr: string): TTreeNode;
    /// <summary>
    /// 树是否存在根节点
    /// </summary>
    function HasRootNoteStr(sStr: string): TTreeNode;

    /// <summary>
    /// 添加子节点
    /// </summary>
    function AddChildNote(ANode: TTreeNode; sStr: string): TTreeNode;
  public
    { Public declarations }
  end;

var
  fTrain: TfTrain;

implementation

{$R *.dfm}

{ TfTrain }

function TfTrain.AddChildNote(ANode: TTreeNode; sStr: string): TTreeNode;
var
  ANoteTemp : TTreeNode;
begin
  Result := nil;

  if Assigned(ANode) then
    ANoteTemp := HasNoteStr(ANode, sStr)
  else
    ANoteTemp := HasRootNoteStr(sStr);

  if Assigned(ANoteTemp) then
  begin
    Application.MessageBox('目录已经存在！', '提示', MB_OK + MB_ICONINFORMATION);
  end
  else
  begin
    Result := tv1.Items.AddChild(ANode, sStr);
  end;
end;

procedure TfTrain.btn1Click(Sender: TObject);
begin
  if btn1.Caption = '开始培训' then
  begin
    btn1.Caption := '停止培训';

//    UDPSendScreen.OnGetScreen := GetSreen1;

  end
  else
  begin
    btn1.Caption := '开始培训';
//    UDPSendScreen.OnGetScreen := nil;
//    UDPSendScreen.
  end;



end;

procedure TfTrain.btn2Click(Sender: TObject);
var
  ANote, ANoteTemp : TTreeNode;
  AInfo : TTrainQuestion;
  sName : string;
begin
  ANote := tv1.Selected;

  if Assigned(ANote) then
  begin
    if TTrainQuestion(ANote.Data).Tqtype = 1 then
    begin
      Application.MessageBox('考题不允许建立子目录！', '警告', MB_OK + MB_ICONWARNING);

      Exit;
    end;
  end;


  sName := InputBox('输入','目录名称', '新建目录');
  ANoteTemp := AddChildNote(ANote, sName);

  if Assigned(ANoteTemp) then
  begin
    AInfo := TTrainQuestion.Create;
    AInfo.Tqtype := 0;
    ANoteTemp.ImageIndex := AInfo.Tqtype;
    ANoteTemp.SelectedIndex := AInfo.Tqtype;
    if Assigned(ANote) then
      AInfo.Tqpath := TTrainQuestion(ANote.Data).Tqpath + '\' + TTrainQuestion(ANote.Data).Tqqname
    else
      AInfo.Tqpath := '';
    AInfo.Tqqname := sName;
    ANoteTemp.Data := AInfo;
    FTCControl.AddTrainQuestion(AInfo);
  end;
end;

procedure TfTrain.btn3Click(Sender: TObject);
var
  ANode, ANoteTemp : TTreeNode;
  AInfo : TTrainQuestion;
  sName : string;
begin
  ANode := tv1.Selected;

  if Assigned(ANode) then
    ANode := ANode.Parent;


  sName := InputBox('输入','目录名称', '新建目录');
  ANoteTemp := AddChildNote(ANode, sName);

  if Assigned(ANoteTemp) then
  begin
    AInfo := TTrainQuestion.Create;
    AInfo.Tqtype := 0;
    ANoteTemp.ImageIndex := AInfo.Tqtype;
    ANoteTemp.SelectedIndex := AInfo.Tqtype;


    if Assigned(ANode) then
      AInfo.Tqpath := TTrainQuestion(ANode.Data).Tqpath + '\' + TTrainQuestion(ANode.Data).Tqqname
    else
      AInfo.Tqpath := '';
    AInfo.Tqqname := sName;
    ANoteTemp.Data := AInfo;
    FTCControl.AddTrainQuestion(AInfo);
  end;

end;

procedure TfTrain.btn4Click(Sender: TObject);
var
  ANote : TTreeNode;
begin
  ANote := tv1.Selected;

  if not Assigned(ANote) then
  begin
    Application.MessageBox('请选择要删除的记录！', '提示', MB_OK + MB_ICONINFORMATION);
    Exit;
  end;

  if Application.MessageBox('确定要删除选择记录（记录下的内容全部删除）！', '提示', MB_OKCANCEL +
    MB_ICONQUESTION) = IDOK then
  begin
    FTCControl.DelInfo(TTrainQuestion(ANote.Data));
    LoadInfo;
  end;





end;

procedure TfTrain.btn5Click(Sender: TObject);
var
  AQInfo : TQuestionInfo;
  AInfo : TTrainQuestion;
  ANoteTemp,ANode : TTreeNode;
begin

  with TfQuestionListC.Create(nil) do
  begin
    AQInfo := SelOneQuestion;
    Free;
  end;

  if Assigned(AQInfo) then
  begin

    ANode := tv1.Selected;


    if Assigned(ANode) then
      if TTrainQuestion(ANode.Data).Tqtype = 1 then //  考题
        ANode := ANode.Parent;

    ANoteTemp := AddChildNote(ANode, AQInfo.QName);

    if Assigned(ANoteTemp) then
    begin
      AInfo := TTrainQuestion.Create;
      AInfo.Tqtype := 1;

      ANoteTemp.ImageIndex := AInfo.Tqtype;
      ANoteTemp.SelectedIndex := AInfo.Tqtype;

      if ANoteTemp.Level > 0 then
      begin
        AInfo.Tqpath := TTrainQuestion(ANode.Data).Tqpath + '\' + TTrainQuestion(ANode.Data).Tqqname
      end
      else
      begin
        AInfo.Tqpath := '';
      end;

      AInfo.Tqqname := AQInfo.QName;
      AInfo.Tqcode1 := AQInfo.QCode;
      AInfo.Tqcode2 := AQInfo.QRemark2;
      AInfo.Tqremark := IntToStr(AQInfo.QID);
      ANoteTemp.Data := AInfo;
      FTCControl.AddTrainQuestion(AInfo);
    end;
  end;
end;

procedure TfTrain.btn6Click(Sender: TObject);
begin
  if btn6.Caption = '︽' then
  begin
    pnl3.Height := 100;
    btn6.Caption := '︾';
  end
  else
  begin
    pnl3.Height := btn6.Height;
    btn6.Caption := '︽';
  end;
end;

procedure TfTrain.btn7Click(Sender: TObject);
var
  ANode, AParentNode, ANodeTemp : TTreeNode;
  sName : string;
begin
  ANode := tv1.Selected;

  if Assigned(ANode) then
    AParentNode := ANode.Parent
  else
    AParentNode := nil;

  if not Assigned(ANode) then
  begin
    Application.MessageBox('请选择记录！', '提示', MB_OK + MB_ICONINFORMATION);
    Exit;
  end;
  sName := TTrainQuestion(ANode.Data).Tqqname;
  sName := InputBox('输入','名称', sName);

  if Assigned(AParentNode) then
    ANodeTemp := HasNoteStr(AParentNode, sName)
  else
    ANodeTemp := HasRootNoteStr(sName);

  if Assigned(ANodeTemp) then
  begin
    Application.MessageBox('名称已存在！', '', MB_OK + MB_ICONINFORMATION);
    Exit;
  end;

  FTCControl.Rename(TTrainQuestion(ANode.Data), sName);
  LoadInfo;
end;

procedure TfTrain.FormCreate(Sender: TObject);
begin
  FTCControl := TTrainQuestionControl.Create;
  FQInfo := TQuestionInfo.Create;

  FromPosState := TfPosState.Create(nil);
  FromPosState.Align := alClient;
  FromPosState.Parent := tbsht3;
  FromPosState.BorderStyle := bsNone;

  FFormQInfo := TfQInfo.Create(nil);
  FFormQInfo.Parent := tbsht1;
  FFormQInfo.BorderStyle := bsNone;
  FFormQInfo.Align := alClient;
  FFormQInfo.pnlBottom.Visible := False;
  FFormQInfo.ShowInfoReadOnly(nil);

  UDPSendScreen := TThreadUDPSendScreen.Create(False);

  fullCanvas := TCanvas.Create;
  Fullscreen:=TBitmap.Create;
  Fullscreen1:=TBitmap.Create;
  AJpeg := TJPEGImage.Create;

  Fullscreen1.Assign(img1.Picture.Bitmap);
  UDPSendScreen.OnGetScreen := GetSreen1;


  LoadInfo;
end;

procedure TfTrain.FormDestroy(Sender: TObject);
begin
  UDPSendScreen.Free;
  FromPosState.Free;

  fullCanvas.Free;
  Fullscreen.Free;
  Fullscreen1.free;
  AJpeg.Free;


  FFormQInfo.Free;
  FQInfo.Free;
  FTCControl.Free;
end;

procedure TfTrain.FormShow(Sender: TObject);
begin
  FFormQInfo.Show;
  FromPosState.Show;
end;

procedure TfTrain.GetSreen1(Sender: TObject);
var
  SrcRect, DstRect : TRect;

begin
  if btn1.Caption = '开始培训' then
  begin
    Fullscreen1.PixelFormat := pfDevice;
    AJpeg.Assign( Fullscreen1 );
  end
  else
  begin
    Fullscreen.Width:=pgcntrl1.Width-15;
    Fullscreen.Height:=pgcntrl1.Height-36;

    SrcRect := Rect(0, 0, Fullscreen.Width,Fullscreen.Height);
    DstRect := Rect(0, 0, Fullscreen.Width,Fullscreen.Height);

    Fullscreen.Canvas.CopyRect(DstRect, pgcntrl1.Canvas, SrcRect); //把整个屏幕复制到BITMAP中
    Fullscreen.PixelFormat := pfDevice;
    AJpeg.Assign( Fullscreen );
  end;

  TMemoryStream(Sender).Clear;
  AJpeg.SaveToStream(TMemoryStream(Sender));
end;

function TfTrain.HasRootNoteStr( sStr : string) : TTreeNode;
var
  k : Integer;
  ANode : TTreeNode;
begin
Result := nil;
  for k := 0 to tv1.Items.Count - 1 do
  begin
    ANode := tv1.Items.Item[k];

    if ANode.Level = 0 then
    begin
      if ANode.Text =  sStr then
      begin
        Result := ANode;
        Break;
      end;
    end;

  end;
end;


function TfTrain.HasNoteStr( ANode : TTreeNode; sStr : string) : TTreeNode;
var
  AFistNode, ALastNode, ACurNode : TTreeNode;
begin
  Result := nil;

  if not Assigned(ANode) then
    Exit;


  AFistNode := ANode.getFirstChild;
  ALastNode := ANode.GetLastChild;

  if not Assigned(AFistNode) then
    Exit;


  if Assigned(AFistNode) and Assigned(ALastNode) then
  begin
    if AFistNode.Text = sStr then
      Result := AFistNode;

    if not Assigned(AFistNode) then
    begin
      repeat
        ACurNode := AFistNode.GetNext;

        if ACurNode.Text = sStr then
          Result := ACurNode;

      until Assigned(AFistNode) or (ACurNode = ALastNode);
    end;
  end;
end;

procedure TfTrain.LoadInfo;
  function AddNote(sParent, sName : string) : TTreeNode;
  var
    nIndex : Integer;
    sPath, sNewName : string;
    ANode : TTreeNode;
  begin
    nIndex := LastDelimiter('\', sParent);

    if nIndex >= 1 then
    begin
      sPath := Copy(sParent, 1, nIndex-1);
      sNewName := Copy(sParent, nIndex+ 1, Length(sParent) - nIndex);
      ANode := AddNote(sPath, sNewName);

      // 是否存在节点
      Result := HasNoteStr(ANode, sName);

      //  不存在则创建
      if not Assigned(Result) then
        Result := tv1.Items.AddChild(ANode, sName)
    end
    else
    begin

      // 是否存在节点
      Result := HasRootNoteStr(sName);
      if not Assigned(Result) then
        Result := tv1.Items.AddChild(nil, sName);
    end;

  end;

  procedure AddInfo(ATQInfo : TTrainQuestion);
  var
    ANode : TTreeNode;
  begin
    ANode := AddNote(ATQInfo.Tqpath, ATQInfo.Tqqname);

    if Assigned(ANode) then
    begin
      ANode.Data := ATQInfo;
      ANode.ImageIndex := ATQInfo.Tqtype;
      ANode.SelectedIndex := ATQInfo.Tqtype;
    end;
  end;
var
  AInfo : TTrainQuestion;
  i : Integer;
begin
  tv1.Items.Clear;

  for i := 0  to FTCControl.TrainQuestionList.Count - 1 do
  begin
    AInfo := FTCControl.TrainQuestionInfo[i];

    if Assigned(AInfo) then
    begin
      AddInfo(AInfo);
    end;
  end;
end;

procedure TfTrain.tmr1Timer(Sender: TObject);
begin
  lbl1.Caption := '培训人数' + IntToStr(ExamControl.TrainClientCount) + '/' +
    IntToStr(ExamControl.ClinetCount);
end;

procedure TfTrain.tv1Click(Sender: TObject);
var
  ANode : TTreeNode;
begin
  ANode := tv1.Selected;

  if FSelectNode <> ANode then
  begin
    FSelectNode := ANode;

    if Assigned(FSelectNode) then
    begin
      if TTrainQuestion(FSelectNode.Data).Tqtype = 1 then
      begin

        FQInfo.QCode := TTrainQuestion(FSelectNode.Data).Tqcode1;
        FQInfo.QName := TTrainQuestion(FSelectNode.Data).Tqqname;
        FQInfo.QRemark2 := TTrainQuestion(FSelectNode.Data).Tqcode2;

        FFormQInfo.ShowInfoReadOnly(FQInfo);

      end;
    end;
  end;
end;

end.

