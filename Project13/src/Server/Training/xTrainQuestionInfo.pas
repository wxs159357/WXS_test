unit xTrainQuestionInfo;

interface

type
  /// <summary>
  /// 培训题信息
  /// </summary>
  TTrainQuestion = class
  private
    FTqid     : Integer   ;
    FTqtype   : Integer   ;
    FTqpath   : String    ;
    FTqqname  : String    ;
    FTqcode1  : String    ;
    FTqcode2  : String    ;
    FTqremark : String    ;
  public
    property Tqid     : Integer    read FTqid     write FTqid    ;
    /// <summary>
    /// 0：目录 1：文件
    /// </summary>
    property Tqtype   : Integer    read FTqtype   write FTqtype  ;
    property Tqpath   : String     read FTqpath   write FTqpath  ;
    property Tqqname  : String     read FTqqname  write FTqqname ;
    property Tqcode1  : String     read FTqcode1  write FTqcode1 ;
    property Tqcode2  : String     read FTqcode2  write FTqcode2 ;
    property Tqremark : String     read FTqremark write FTqremark;
  end;


implementation

end.
