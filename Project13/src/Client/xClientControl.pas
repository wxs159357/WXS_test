unit xClientControl;

interface

uses System.Classes, System.SysUtils, xClientType, xStudentInfo, xTCPClient,
  xUDPClient1, xFunction;

type
  TClientControl = class
  private
    FStudentInfo: TStudentInfo;
    FStartTime: TDateTime;
    FClientState: TClientState;
    FEndTime: TDateTime;
    FSubList: TStringList;
    FOnStateChange: TNotifyEvent;

    procedure ReadINI;
    procedure WriteINI;

    procedure StateChange;
    procedure SetClientState(const Value: TClientState);


  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 客户端状态
    /// </summary>
    property ClientState : TClientState read FClientState write SetClientState;

    /// <summary>
    /// 考生信息
    /// </summary>
    property StudentInfo : TStudentInfo read FStudentInfo write FStudentInfo;

    /// <summary>
    /// 考生的考题列表
    /// </summary>
    property SubList : TStringList read FSubList write FSubList;

    /// <summary>
    /// 开始考试时间
    /// </summary>
    property StartTime : TDateTime read FStartTime write FStartTime;

    /// <summary>
    /// 结束考试时间
    /// </summary>
    property EndTime : TDateTime read FEndTime write FEndTime;

  public
    /// <summary>
    /// 状态改变
    /// </summary>
    property OnStateChange : TNotifyEvent read FOnStateChange write FOnStateChange;

  end;
var
  ClientControl : TClientControl;

implementation

{ TClientControl }


constructor TClientControl.Create;
begin
  FClientState := esDisconn;
  FStudentInfo:= TStudentInfo.Create

end;

destructor TClientControl.Destroy;
begin
  FStudentInfo.Free;

  inherited;
end;

procedure TClientControl.ReadINI;
begin

end;

procedure TClientControl.SetClientState(const Value: TClientState);
begin
  if FClientState <> Value then
  begin
    FClientState := Value;
    StateChange;
    TCPClient.SendStuState(FClientState);
  end;
end;

procedure TClientControl.StateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TClientControl.WriteINI;
begin

end;

end.
