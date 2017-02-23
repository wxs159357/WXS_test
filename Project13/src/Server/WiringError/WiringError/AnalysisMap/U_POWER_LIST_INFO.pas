unit U_POWER_LIST_INFO;

interface

type
  /// <summary>
  /// 三相四线功率信息
  /// </summary>
  TFourPower = class
  private
    FErrorcode  : String    ;
    FErrorcount : Integer   ;
    FU1         : Double    ;
    FU2         : Double    ;
    FU3         : Double    ;
    FI1         : Double    ;
    FI2         : Double    ;
    FI3         : Double    ;
    FU1i1       : Double    ;
    FU2i2       : Double    ;
    FU3i3       : Double    ;
    FU1u2       : Double    ;
    FU1u3       : Double    ;
    FU2u3       : Double    ;
    FAngle: Double;
    FUIPower: Double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TFourPower);

    property Errorcode  : String     read FErrorcode  write FErrorcode ;
    property Errorcount : Integer    read FErrorcount write FErrorcount;
    property U1         : Double     read FU1         write FU1        ;
    property U2         : Double     read FU2         write FU2        ;
    property U3         : Double     read FU3         write FU3        ;
    property I1         : Double     read FI1         write FI1        ;
    property I2         : Double     read FI2         write FI2        ;
    property I3         : Double     read FI3         write FI3        ;
    property U1i1       : Double     read FU1i1       write FU1i1      ;
    property U2i2       : Double     read FU2i2       write FU2i2      ;
    property U3i3       : Double     read FU3i3       write FU3i3      ;
    property U1u2       : Double     read FU1u2       write FU1u2      ;
    property U1u3       : Double     read FU1u3       write FU1u3      ;
    property U2u3       : Double     read FU2u3       write FU2u3      ;
    property Angle      : Double     read FAngle      write FAngle    ;
    property UIPower    : Double     read FUIPower    write FUIPower    ;
  end;

type
  /// <summary>
  /// 三相三线功率信息
  /// </summary>
  TThreePower = class
  private
    FErrorcode  : String    ;
    FErrorcount : Integer   ;
    FU12        : Double    ;
    FU32        : Double    ;
    FI1         : Double    ;
    FI3         : Double    ;
    FU12i1      : Double    ;
    FU32i3      : Double    ;
    FU12u32     : Double    ;
    FAngle: Double;
    FUIPower: Double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TThreePower);

    property Errorcode  : String     read FErrorcode  write FErrorcode ;
    property Errorcount : Integer    read FErrorcount write FErrorcount;
    property U12        : Double     read FU12        write FU12       ;
    property U32        : Double     read FU32        write FU32       ;
    property I1         : Double     read FI1         write FI1        ;
    property I3         : Double     read FI3         write FI3        ;
    property U12i1      : Double     read FU12i1      write FU12i1     ;
    property U32i3      : Double     read FU32i3      write FU32i3     ;
    property U12u32     : Double     read FU12u32     write FU12u32    ;
    property Angle      : Double     read FAngle      write FAngle    ;
    property UIPower    : Double     read FUIPower    write FUIPower    ;
  end;


implementation

{ TFourPower }

procedure TFourPower.Assign(Source: TFourPower);
begin
  if Assigned(Source) then
  begin
    FErrorcode  := Source.Errorcode ;
    FErrorcount := Source.Errorcount;
    FU1         := Source.U1        ;
    FU2         := Source.U2        ;
    FU3         := Source.U3        ;
    FI1         := Source.I1        ;
    FI2         := Source.I2        ;
    FI3         := Source.I3        ;
    FU1i1       := Source.U1i1      ;
    FU2i2       := Source.U2i2      ;
    FU3i3       := Source.U3i3      ;
    FU1u2       := Source.U1u2      ;
    FU1u3       := Source.U1u3      ;
    FU2u3       := Source.U2u3      ;
    FAngle      := Source.Angle    ;
    FUIPower    := Source.UIPower    ;
  end;
end;

constructor TFourPower.Create;
begin
  FErrorcode  := '';
  FErrorcount := 0;
  FU1         := -1;
  FU2         := -1;
  FU3         := -1;
  FI1         := -1;
  FI2         := -1;
  FI3         := -1;
  FU1i1       := -1;
  FU2i2       := -1;
  FU3i3       := -1;
  FU1u2       := -1;
  FU1u3       := -1;
  FU2u3       := -1;
  FUIPower    := -1;
end;

destructor TFourPower.Destroy;
begin

  inherited;
end;

{ TThreePower }

procedure TThreePower.Assign(Source: TThreePower);
begin
  if Assigned(Source) then
  begin
    FErrorcode  := Source.Errorcode ;
    FErrorcount := Source.Errorcount;
    FU12        := Source.U12       ;
    FU32        := Source.U32       ;
    FI1         := Source.I1        ;
    FI3         := Source.I3        ;
    FU12i1      := Source.U12i1     ;
    FU32i3      := Source.U32i3     ;
    FU12u32     := Source.U12u32    ;
    FAngle      := Source.Angle    ;
    FUIPower    := Source.UIPower    ;
  end;
end;          

constructor TThreePower.Create;
begin
  FErrorcode   := '';
  FErrorcount  := 0;
  FU12         := -1;
  FU32         := -1;
  FI1          := -1;
  FI3          := -1;
  FU12i1       := -1;
  FU32i3       := -1;
  FU12u32      := -1;
  FUIPower    := -1;
end;

destructor TThreePower.Destroy;
begin

  inherited;
end;

end.
