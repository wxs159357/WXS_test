unit xExerciseInfo;

interface
type
  TExerciseInfo = class
  private
    FId         : Integer   ;
    FPath       : String    ;
    FPtype      : Integer    ;
    FEname      : String    ;
    FImageindex : Integer   ;
    FCode1      : String    ;
    FCode2      : String    ;
    FRemark     : String    ;
  public
    /// <summary>
    /// 编号
    /// </summary>
    property Id         : Integer    read FId         write FId        ;
    /// <summary>
    /// 目录
    /// </summary>
    property Path       : String     read FPath       write FPath      ;
    /// <summary>
    /// 类型
    /// </summary>
    property Ptype      : Integer     read FPtype      write FPtype     ;
    /// <summary>
    /// 目录名或考题名
    /// </summary>
    property Ename      : String     read FEname      write FEname     ;
    /// <summary>
    /// 图标序号
    /// </summary>
    property Imageindex : Integer    read FImageindex write FImageindex;
    /// <summary>
    /// 编码1
    /// </summary>
    property Code1      : String     read FCode1      write FCode1     ;
    /// <summary>
    /// 编码2
    /// </summary>
    property Code2      : String     read FCode2      write FCode2     ;
    /// <summary>
    /// 备注
    /// </summary>
    property Remark     : String     read FRemark     write FRemark    ;
  end;


implementation

end.
