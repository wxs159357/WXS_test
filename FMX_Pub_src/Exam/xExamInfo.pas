unit xExamInfo;

interface

type
  TExamInfo = class

  public
    constructor Create;
    destructor Destroy; override;
  private
    FExamTime: Integer;
    FExamName: string;

  public
    /// <summary>
    /// 考试名称
    /// </summary>
    property ExamName : string read FExamName write FExamName;


    /// <summary>
    /// 考试时间 (分)
    /// </summary>
    property ExamTime : Integer read FExamTime write FExamTime;
  end;

implementation

{ TExamInfo }

constructor TExamInfo.Create;
begin
  FExamName := '全国技能竞赛';
  FExamTime := 45;
end;

destructor TExamInfo.Destroy;
begin

  inherited;
end;

end.
