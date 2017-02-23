unit xQuestionAction;

interface

uses xDBActionBase, xQuestionInfo, System.Classes, System.SysUtils, xFunction,
  FireDAC.Stan.Param;

type
  TQuestionAction = class(TDBActionBase)

  public
    /// <summary>
    /// 获取最大编号
    /// </summary>
    function GetMaxSN : Integer;

    /// <summary>
    /// 添加考题
    /// </summary>
    procedure AddQuestion(AQuestion : TQuestionInfo);

    /// <summary>
    /// 删除考题
    /// </summary>
    procedure DelQuestion(nSortID, nQID : Integer); overload;
    procedure DelQuestion(AQuestion : TQuestionInfo); overload;
    procedure DelQuestion(nSortID : Integer); overload;

    /// <summary>
    /// 修改考题
    /// </summary>
    procedure EditQuestion(AQuestion : TQuestionInfo);

    /// <summary>
    /// 清空考题
    /// </summary>
    procedure ClearQuestion; overload;
    procedure ClearQuestion(nSortID : Integer); overload;
    /// <summary>
    /// 加载考题
    /// </summary>
    procedure LoadQuestion(nSortID : Integer; slList :TStringList);
  end;

implementation

{ TQuestionAction }

procedure TQuestionAction.AddQuestion(AQuestion: TQuestionInfo);
const
  C_SQL = 'insert into QuestionInfo ( SortID, QID, QType, QName,' +
     'QDescribe, QCode, QAnswer, QExplain, QRemark1, QRemark2' +
     ') values (  %d, %d, %d, :QName, :QDescribe, :QCode, :QAnswer,' +
     ':QExplain, :QRemark1, :QRemark2 )';
begin
  if Assigned(AQuestion) then
  begin
    with AQuestion, FQuery.Params do
    begin
      FQuery.Sql.Text := Format( C_SQL, [ Sortid, Qid, Qtype ] );

      ParamByName( 'QName'     ).Value := Qname    ;
      ParamByName( 'QCode'     ).Value := QCode;
      ParamByName( 'QDescribe' ).Value := Qdescribe;
      ParamByName( 'QAnswer'   ).Value := Qanswer  ;
      ParamByName( 'QExplain'  ).Value := Qexplain ;
      ParamByName( 'QRemark1'  ).Value := Qremark1 ;
      ParamByName( 'QRemark2'  ).Value := Qremark2 ;
    end;

    ExecSQL;
  end;
end;

procedure TQuestionAction.ClearQuestion;
begin
  FQuery.Sql.Text := 'delete from QuestionInfo';
  ExecSQL;
end;

procedure TQuestionAction.DelQuestion(nSortID, nQID: Integer);
const
  C_SQL = 'delete from QuestionInfo where SortID = %d and QID = %d';
begin
  FQuery.Sql.Text := Format( C_SQL, [ nSortID, nQID ] );
  ExecSQL;
end;

procedure TQuestionAction.DelQuestion(AQuestion: TQuestionInfo);
begin
  if Assigned(AQuestion) then
  begin
    DelQuestion(AQuestion.SortID, AQuestion.QID);
  end;
end;

procedure TQuestionAction.ClearQuestion(nSortID: Integer);
const
  C_SQL = 'delete from QuestionInfo where SortID = %d';
begin
  FQuery.Sql.Text := Format( C_SQL, [ nSortID ] );
  ExecSQL;
end;

procedure TQuestionAction.DelQuestion(nSortID: Integer);
const
  C_SQL = 'delete from QuestionInfo where SortID = %d';
begin
  FQuery.Sql.Text := Format( C_SQL, [ nSortID ] );
  ExecSQL;
end;

procedure TQuestionAction.EditQuestion(AQuestion: TQuestionInfo);
const
  C_SQL ='update QuestionInfo set QType = %d,' +
         'QName = :QName, QDescribe = :QDescribe, QCode = :QCode, QAnswer = :QAnswer,' +
         'QExplain = :QExplain, QRemark1 = :QRemark1,' +
         'QRemark2 = :QRemark2  where SortID = %d and QID = %d';
begin
  if Assigned(AQuestion) then
  begin
    with AQuestion, FQuery.Params do
    begin
      FQuery.Sql.Text := Format( C_SQL, [ Qtype, Sortid, Qid ] );

      ParamByName( 'QName'     ).Value := Qname    ;
      ParamByName( 'QDescribe' ).Value := Qdescribe;
      ParamByName( 'QCode'     ).Value := QCode;
      ParamByName( 'QAnswer'   ).Value := Qanswer  ;
      ParamByName( 'QExplain'  ).Value := Qexplain ;
      ParamByName( 'QRemark1'  ).Value := Qremark1 ;
      ParamByName( 'QRemark2'  ).Value := Qremark2 ;
    end;

    ExecSQL;
  end;
end;

function TQuestionAction.GetMaxSN: Integer;
const
  C_SQL = 'select max(QID) as MaxSN from QuestionInfo';
begin
  FQuery.Open(C_SQL);

  if FQuery.RecordCount = 1 then
    Result := FQuery.FieldByName('MaxSN').AsInteger
  else
    Result := 0;

  FQuery.Close;
end;

procedure TQuestionAction.LoadQuestion(nSortID: Integer; slList: TStringList);
const
  C_SQL = 'select * from QuestionInfo where SortID = %d';
var
  AQuestion : TQuestionInfo;
begin
  if Assigned(slList) then
  begin
    ClearStringList(slList);
    FQuery.Open(Format( C_SQL, [nSortID] ));
    while not FQuery.Eof do
    begin
      AQuestion := TQuestionInfo.Create;
      with AQuestion, FQuery do
      begin

        Sortid    := FieldByName( 'SortID'    ).AsInteger;
        Qid       := FieldByName( 'QID'       ).AsInteger;
        Qtype     := FieldByName( 'QType'     ).AsInteger;
        Qname     := FieldByName( 'QName'     ).AsString;
        Qdescribe := FieldByName( 'QDescribe' ).AsString;
        QCode     := FieldByName( 'QCode' ).AsString;
        Qanswer   := FieldByName( 'QAnswer'   ).AsString;
        Qexplain  := FieldByName( 'QExplain'  ).AsString;
        Qremark1  := FieldByName( 'QRemark1'  ).AsString;
        Qremark2  := FieldByName( 'QRemark2'  ).AsString;
      end;
      slList.AddObject('', AQuestion);
      FQuery.Next;
    end;
    FQuery.Close;
  end;
end;

end.
