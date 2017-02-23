{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，自动生成考题单元
  + TWE_MAKE_LIST     自动生成考题

===============================================================================}
unit U_WE_MAKE_LIST;

interface

uses SysUtils, Classes, U_WIRING_ERROR;

type
  TIntArray = array of Integer;


type
  TWE_MAKE_LIST = class
  private
    FPhaseType: TWE_PHASE_TYPE;
    FErrorTypeSet: TWE_ERROR_TYPE_SET;
    FErrorCount: Integer;
    procedure SetErrorTypeSet(const Value: TWE_ERROR_TYPE_SET);
    procedure SetPhaseType(const Value: TWE_PHASE_TYPE);
  protected
    /// <summary>
    /// 清理错误类型
    /// </summary>
    procedure CleanErrorTypeSet;

    /// <summary>
    /// 相序列表
    /// </summary>
    function GetUSequenceArray : TIntArray;
    function GetUBrokenArray : TIntArray;
    function GetUsBrokenArray : TIntArray;
    function GetPTReverseArray : TIntArray;
    function GetISequenceArray : TIntArray;
    function GetIBrokenArray : TIntArray;
    function GetCTShortArray : TIntArray;
    function GetCTReverseArray : TIntArray;

    /// <summary>
    /// 创建序列
    /// </summary>
    function CreateNewArray( AAry1, AAry2 : TIntArray ) : TIntArray;

    /// <summary>
    /// 计算三线错误个数
    /// </summary>
    function CalErrorCount3( AErrCode : Integer ) : Integer;

    /// <summary>
    /// 计算四线错误个数
    /// </summary>
    function CalErrorCount4( AErrCode : Integer ) : Integer;

    /// <summary>
    /// 依据错误个数过滤序列
    /// </summary>
    function FilterArray( AErrArray : TIntArray ) : TIntArray;
  public
    /// <summary>
    /// 相线类型
    /// </summary>
    property PhaseType : TWE_PHASE_TYPE read FPhaseType write SetPhaseType;

    /// <summary>
    /// 包含的错误类型
    /// </summary>
    property ErrorTypeSet : TWE_ERROR_TYPE_SET read FErrorTypeSet write SetErrorTypeSet;

    /// <summary>
    /// 错误个数
    /// </summary>
    property ErrorCount : Integer read FErrorCount write FErrorCount;
  public
    /// <summary>
    /// 创建列表
    /// </summary>
    procedure MakeWEList( AList : TList );

    /// <summary>
    /// 创建ID列表
    /// </summary>
    function GetIDList : TIntArray;
  end;

implementation

{ TWE_MAKE_LIST }

function TWE_MAKE_LIST.CalErrorCount3(AErrCode: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to 7 do
    if AErrCode and ( 1 shl i ) = 1 shl i then
      Inc( Result );

  if (AErrCode and ( 1 shl 8 )+AErrCode and ( 1 shl 9 )) >0 then
    Inc( Result );
//  if (AErrCode and ( 1 shl 10 )+AErrCode and ( 1 shl 11 )) >0 then
//    Inc( Result );
//  if (AErrCode and ( 1 shl 12 )+AErrCode and ( 1 shl 13 )) >0 then
//    Inc( Result );
//  if (AErrCode and ( 1 shl 14 )+AErrCode and ( 1 shl 15 )) >0 then
//    Inc( Result );

//  for i := 0 to 4 - 1 do
//    if AErrCode and ( 3 shl ( i * 2 + 8 ) ) <> 0 then
//      Inc( Result );

  for i := 16 to 23 do
    if AErrCode and ( 1 shl i ) = 1 shl i then
      Inc( Result );

  if AErrCode and ( 7 shl 24 ) <> 0 then
    Inc( Result );
end;

function TWE_MAKE_LIST.CalErrorCount4(AErrCode: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to 10 do
    if AErrCode and ( 1 shl i ) = 1 shl i then
      Inc( Result );

  for i := 16 to 19 do
    if AErrCode and ( 1 shl i ) = 1 shl i then
      Inc( Result );

  if AErrCode and ( 7 shl 12 ) <> 0 then
    Inc( Result );

  if AErrCode and ( 7 shl 24 ) <> 0 then
    Inc( Result );
end;

procedure TWE_MAKE_LIST.CleanErrorTypeSet;
begin
  if FPhaseType = ptFour then
  begin
    Exclude( FErrorTypeSet, etUsBroken );
    Exclude( FErrorTypeSet, etPTReverse );
  end;  
end;

function TWE_MAKE_LIST.CreateNewArray(AAry1, AAry2: TIntArray): TIntArray;
var
  i, j: Integer;
  nLen1, nLen2 : Integer;
begin
  nLen1 := Length( AAry1 );
  nLen2 := Length( AAry2 );

  if ( nLen1 = 0 ) and ( nLen2 = 0 ) then
    Result := nil
  else if nLen1 = 0 then
    Result := AAry2
  else if nLen2 = 0 then
    Result := AAry1
  else
  begin
    SetLength( Result,  nLen1 * nLen2 );
    for i := 0 to nLen1 - 1 do
      for j := 0 to nLen2 - 1 do
        Result[ j * nLen1 + i ] := AAry2[ j ] or AAry1[ i ];
  end;
end;

function TWE_MAKE_LIST.FilterArray(AErrArray: TIntArray): TIntArray;
var
  i, nIndex : Integer;
  nFilterCount : Integer;
begin
  if FErrorCount = 0 then
  begin
    Result := AErrArray;
    Exit;
  end;

  nFilterCount := 0;

  if FPhaseType = ptThree then
  begin
    for i := 0 to Length( AErrArray ) - 1 do
      if CalErrorCount3( AErrArray[ i ] ) <> FErrorCount then
      begin
        AErrArray[ i ] := -1;
        Inc( nFilterCount );
      end;
  end
  else
  begin
    for i := 0 to Length( AErrArray ) - 1 do
      if CalErrorCount4( AErrArray[ i ] ) <> FErrorCount then
      begin
        AErrArray[ i ] := -1;
        Inc( nFilterCount );
      end;
  end;   

  // 取最终结果
  SetLength( Result, Length( AErrArray ) - nFilterCount );
  nIndex := 0;

  for i := 0 to Length( AErrArray ) - 1 do
    if AErrArray[ i ] > 0 then
    begin
      Result[ nIndex ] := AErrArray[ i ];
      Inc( nIndex );
    end;  
end;

function TWE_MAKE_LIST.GetCTReverseArray: TIntArray;
var
  i : Integer;
begin
  if etCTReverse in FErrorTypeSet then
  begin
    if FPhaseType = ptThree then
    begin
      SetLength( Result , 4 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 6;
    end
    else
    begin
      SetLength( Result , 8 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 8;
    end;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetCTShortArray: TIntArray;
var
  i : Integer;
begin
  if etCTShort in FErrorTypeSet then
  begin
    if FPhaseType = ptThree then
    begin
      SetLength( Result , 4 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 4;
    end
    else
    begin
      SetLength( Result , 8 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 4;
    end;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetIBrokenArray: TIntArray;
var
  i : Integer;
  j: Integer;
  k: Integer;
begin
  if etIBroken in FErrorTypeSet then
  begin
    if FPhaseType = ptThree then
    begin
      SetLength( Result , 8 );

      for i := 0 to 2 - 1 do
        for j := 0 to 2 - 1 do
          for k := 0 to 2 - 1 do
            Result[ 4 * k + 2 * j + i ] := k shl 3 + j shl 2 + i;
    end
    else
    begin
      SetLength( Result , 8 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 8;
    end;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetISequenceArray: TIntArray;
var
  i : Integer;
  aInt : array[ 0..3 ] of TIntArray;

begin
  if etISequence in FErrorTypeSet then
  begin
    if FPhaseType = ptThree then
    begin
      for i := 0 to 4 - 1 do
      begin
        SetLength( aInt[ i ], 3 );
        aInt[ i, 0 ] := 0 shl ( 8 + i * 2 );
        aInt[ i, 1 ] := 1 shl ( 8 + i * 2 );
        aInt[ i, 2 ] := 2 shl ( 8 + i * 2 );
      end;
      //Result := CreateNewArray( CreateNewArray( aInt[ 0 ], aInt[ 1 ] ),
        //CreateNewArray( aInt[ 2 ], aInt[ 3 ] ) );
//      SetLength( Result,  8 );
//      Result[0] :=aInt[ 0 ][1-1]+aInt[ 1 ][1-1 ]+aInt[ 2 ][1-1]+aInt[ 3 ][ 1-1 ] ;
//      Result[1] :=aInt[ 0 ][1-1]+aInt[ 1 ][1-1 ]+aInt[ 2 ][2-1]+aInt[ 3 ][ 3-1 ] ;
//      Result[2] :=aInt[ 0 ][3-1]+aInt[ 1 ][1-1 ]+aInt[ 2 ][3-1]+aInt[ 3 ][ 1-1 ] ;
//      Result[3] :=aInt[ 0 ][3-1]+aInt[ 1 ][1-1 ]+aInt[ 2 ][2-1]+aInt[ 3 ][ 2-1 ] ;
//      Result[4] :=aInt[ 0 ][2-1]+aInt[ 1 ][2-1 ]+aInt[ 2 ][1-1]+aInt[ 3 ][ 1-1 ] ;
//      Result[5] :=aInt[ 0 ][2-1]+aInt[ 1 ][2-1 ]+aInt[ 2 ][2-1]+aInt[ 3 ][ 3-1 ] ;
//      Result[6] :=aInt[ 0 ][2-1]+aInt[ 1 ][3-1 ]+aInt[ 2 ][3-1]+aInt[ 3 ][ 1-1 ] ;
//      Result[7] :=aInt[ 0 ][2-1]+aInt[ 1 ][3-1 ]+aInt[ 2 ][2-1]+aInt[ 3 ][ 2-1 ] ;
      SetLength( Result,  2 );
      Result[0] :=aInt[ 0 ][ 0 ]+aInt[ 1 ][ 0 ]+aInt[ 2 ][ 0 ]+aInt[ 3 ][ 0 ] ;
      Result[1] :=aInt[ 0 ][ 2 ]+aInt[ 1 ][ 0 ]+aInt[ 2 ][ 2 ]+aInt[ 3 ][ 0 ] ;

    end
    else
    begin
      SetLength( Result , 6 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 12;
    end;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetPTReverseArray: TIntArray;
var
  i : Integer;
begin
  if etPTReverse in FErrorTypeSet then
  begin
    SetLength( Result , 4 );

    for i := 0 to Length( Result ) - 1 do
      Result[ i ] := i shl 22;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetUBrokenArray: TIntArray;
var
  i : Integer;
begin
  if etUBroken in FErrorTypeSet then
  begin
    if FPhaseType = ptThree then
    begin
      SetLength( Result , 8 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 16;
    end
    else
    begin
      SetLength( Result , 16 );

      for i := 0 to Length( Result ) - 1 do
        Result[ i ] := i shl 16;
    end;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetUsBrokenArray: TIntArray;
var
  i : Integer;
begin
  if etUsBroken in FErrorTypeSet then
  begin
    SetLength( Result , 8 );

    for i := 0 to Length( Result ) - 1 do
      Result[ i ] := i shl 19;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetUSequenceArray: TIntArray;
var
  i : Integer;
begin
  if etUSequence in FErrorTypeSet then
  begin
    SetLength( Result , 6 );

    for i := 0 to Length( Result ) - 1 do
      Result[ i ] := i shl 24;
  end
  else
    Result := nil;
end;

function TWE_MAKE_LIST.GetIDList : TIntArray;
begin
  SetLength( Result, 1 );
  
  if FPhaseType = ptThree then
    Result[ 0 ] := C_WE_ID_CORRECT_THREE
  else
    Result[ 0 ] := C_WE_ID_CORRECT_FOUR;

  Result := CreateNewArray( Result, GetUSequenceArray );
  Result := CreateNewArray( Result, GetUBrokenArray );
  Result := CreateNewArray( Result, GetUsBrokenArray );
  Result := CreateNewArray( Result, GetPTReverseArray );
  Result := CreateNewArray( Result, GetISequenceArray );
  Result := CreateNewArray( Result, GetIBrokenArray );
  Result := CreateNewArray( Result, GetCTShortArray );
  Result := CreateNewArray( Result, GetCTReverseArray );

  if FErrorCount > 0 then
    Result := FilterArray( Result );
end;

procedure TWE_MAKE_LIST.MakeWEList(AList: TList);
begin

end;

procedure TWE_MAKE_LIST.SetErrorTypeSet(const Value: TWE_ERROR_TYPE_SET);
begin
  FErrorTypeSet := Value;
  CleanErrorTypeSet;
end;

procedure TWE_MAKE_LIST.SetPhaseType(const Value: TWE_PHASE_TYPE);
begin
  FPhaseType := Value;
  CleanErrorTypeSet;
end;

end.
