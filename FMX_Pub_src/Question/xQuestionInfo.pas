unit xQuestionInfo;

interface

uses System.SysUtils, System.Classes;

type
  TQuestionInfo = class
  private
    FQid       : Integer   ;
    FQtype     : Integer   ;
    FQname     : String    ;
    FQCode     : string    ;
    FQdescribe : String    ;
    FQanswer   : String    ;
    FQexplain  : String    ;
    FQremark1  : String    ;
    FQremark2  : String    ;
    FSortID: Integer;
  public
    constructor Create;
    /// <summary>
    /// 复制对象
    /// </summary>
    procedure Assign(Source : TQuestionInfo);

    /// <summary>
    /// 题库ID
    /// </summary>
    property SortID : Integer read FSortID write FSortID;
    /// <summary>
    /// 考题编号
    /// </summary>
    property QID       : Integer    read FQid       write FQid      ;
    /// <summary>
    /// 考题类型
    /// </summary>
    property QType     : Integer    read FQtype     write FQtype    ;

    /// <summary>
    /// 类型名称
    /// </summary>
    function QTypeString : string; virtual;
    /// <summary>
    /// 考题名称
    /// </summary>
    property QName     : String     read FQname     write FQname    ;

    /// <summary>
    /// 考题编码
    /// </summary>
    property QCode : String     read FQCode write FQCode;

    /// <summary>
    /// 考题描述
    /// </summary>
    property QDescribe : String     read FQdescribe write FQdescribe;
    /// <summary>
    /// 考题答案
    /// </summary>
    property QAnswer   : String     read FQanswer   write FQanswer  ;
    /// <summary>
    /// 考题详解
    /// </summary>
    property QExplain  : String     read FQexplain  write FQexplain ;
    /// <summary>
    /// 考题备注1
    /// </summary>
    property QRemark1  : String     read FQremark1  write FQremark1 ;
    /// <summary>
    /// 考题备注2
    /// </summary>
    property QRemark2  : String     read FQremark2  write FQremark2 ;
  end;

implementation

{ TQuestionInfo }

procedure TQuestionInfo.Assign(Source: TQuestionInfo);
begin
  FSortID    := Source.SortID   ;
  FQid       := Source.Qid      ;
  FQtype     := Source.Qtype    ;
  FQname     := Source.Qname    ;
  FQCode     := Source.FQCode   ;
  FQdescribe := Source.Qdescribe;
  FQanswer   := Source.Qanswer  ;
  FQexplain  := Source.Qexplain ;
  FQremark1  := Source.Qremark1 ;
  FQremark2  := Source.Qremark2 ;
end;

constructor TQuestionInfo.Create;
begin
  FQid       := -1;
  FQtype     := -1;
  FQname     := '';
  FQCode     := '';
  FQdescribe := '';
  FQanswer   := '';
  FQexplain  := '';
  FQremark1  := '';
  FQremark2  := '';
end;

function TQuestionInfo.QTypeString: string;
begin
  Result := '类型' + IntToStr(FQtype);
end;

end.
