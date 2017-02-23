{===============================================================================
  Copyright(c) 2010, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  电能表 通讯类型单元

===============================================================================}
unit xDL645Type;

interface

const
  C_645_RESET_TIME        = 1; //  广播校时
  C_645_READ_DATA         = 2; //  读数据
  C_645_READ_NEXTDATA     = 3; //  读后续数据
  C_645_READ_ADDR         = 4; //  读通信地址
  C_645_WRITE_DATA        = 5; //  写数据
  C_645_WRITE_ADDR        = 6; //  写通信地址
  C_645_FREEZE            = 7; //  冻结命令
  C_645_CHANGE_BAUD_RATE  = 8; //  改波通讯速率
  C_645_CHANGE_PWD        = 9; //  改密码
  C_645_CLEAR_MAX_DEMAND  = 10;//  最大需量清零
  C_645_CLEAR_RDATA       = 11;//  电表清零
  C_645_CLEAR_REVENT      = 12;//  事件清零
  C_SET_WOUT_TYPE         = 13;//  设置多功能口
  C_645_IDENTITY          = 14;//  身份认证
  C_645_ONOFF_CONTROL     = 15;//  拉合闸
  C_645_CLEAR_RDATA_13    = 16;//  电表清零
  C_645_CLEAR_REVENT_13   = 17;//  事件清零
  C_645_DATA_COPY         = 18;//  数据回抄

type
  /// <summary>
  /// 协议类型
  /// </summary>
  TDL645_PROTOCOL_TYPE = ( dl645ptNone, //  未知类型
                           dl645pt1997, //  DL645-1997
                           dl645pt2007  //  DL645-2007
                          );

const
  /// <summary>
  /// 命令等待时间
  /// </summary>
  C_COMMAND_TIMEOUT = 1500;
type
  /// <summary>
  /// 多功能口输出
  /// </summary>
  TMOUT_TYPE = (
                 motSecond,    //  秒脉冲
                 motCycDemand, //  需量周期
                 motSwitch     //  时段投切
  );


//type
//  /// <summary>
//  /// 控制命令类型
//  /// </summary>
//  TCOMMAND_DL645_TYPE = ( cmd645None,
//                          C_645_RESET_TIME,      //  广播校时
//                          C_645_READ_DATA,       //  读数据
//                          C_645_READ_NEXTDATA,   //  读后续数据
//                          C_645_READ_ADDR,       //  读通信地址
//                          C_645_WRITE_DATA,      //  写数据
//                          C_645_WRITE_ADDR,      //  写通信地址
//                          C_645_FREEZE,         //  冻结命令
//                          C_645_CHANGE_BAUD_RATE, //  改波通讯速率
//                          C_645_CHANGE_PWD,      //  改密码
//                          C_645_CLEAR_MAX_DEMAND, //  最大需量清零
//                          C_645_CLEAR_RDATA,      //  电表清零
//                          C_645_CLEAR_REVENT,     //  事件清零
//                          C_SET_WOUT_TYPE,       //  设置多功能口输出类型
//                          C_645_IDENTITY,       //  身份认证
//                          C_645_ONOFF_CONTROL   //  拉合闸控制
//
//                       );


//  C_645_RESET_TIME        = 1; //  广播校时
//  C_645_READ_DATA         = 2; //  读数据
//  C_645_READ_NEXTDATA     = 3; //  读后续数据
//  C_645_READ_ADDR         = 4; //  读通信地址
//  C_645_WRITE_DATA        = 5; //  写数据
//  C_645_WRITE_ADDR        = 6; //  写通信地址
//  C_645_FREEZE            = 7; //  冻结命令
//  C_645_CHANGE_BAUD_RATE  = 8; //  改波通讯速率
//  C_645_CHANGE_PWD        = 9; //  改密码
//  C_645_CLEAR_MAX_DEMAND  = 10;//  最大需量清零
//  C_645_CLEAR_RDATA       = 11;//  电表清零
//  C_645_CLEAR_REVENT      = 12;//  事件清零
//  C_SET_WOUT_TYPE         = 13;//  设置多功能口
//  C_645_IDENTITY          = 14;//  身份认证
//  C_645_ONOFF_CONTROL     = 15;//  拉合闸
//  C_645_CLEAR_RDATA_13    = 16;//  电表清零
//  C_645_CLEAR_REVENT_13   = 17;//  事件清零
//  C_645_DATA_COPY         = 18;//  数据回抄








//function GetDL645TypeStr( ADL645Type : TCOMMAND_DL645_TYPE ) :string;
type
  /// <summary>
  /// DL645_07错误类型
  /// </summary>
  TDL645_07_ERR = ( de645_07None,
                    de645_07OverRate,      // 费率数超
                    de645_07OverDayTime,   // 日时段数超
                    de645_07OverYearTme,   // 年时区数超
                    de645_07BaudNotChange, // 通讯速率不能改
                    de645_07PwdError,      // 密码错误或未授权
                    de645_07NoneData,      // 无请求数据
                    de645_07OtherError,     // 其他错误
                    de645_07RepeatTopUp,     // 重复充值
                    de645_07ESAMError,         // ESAM验证失败
                    de645_07IdentityError,     // 身份认证失败
                    de645_07ClientSNError,     // 客户编号不匹配
                    de645_07TopUpTimesError,   // 充值次数错误
                    de645_07BuyOverError       // 购电超囤积

                  );



  /// <summary>
  /// 冻结类型
  /// </summary>
  TDL645_07_FREEZE_TYPE = ( dft_07None,    // 未知类型
                            dft_07Month,   // 月冻结
                            dft_07Day,     // 日冻结
                            dft_07Hour,    // 时冻结
                            dft_07Now      // 瞬时冻结
                           );

  /// <summary>
  /// 事件清零类型
  /// </summary>
  TDL645_07_CLEAREVENT_TYPE = ( dct_07None, // 未知类型
                                dct_07All,  // 全部清零
                                dct_07Item  // 分项清零
  );

function Get07ErrStr( AErrType : TDL645_07_ERR ) : string;


/// <summary>
/// 97的数据标识转换成07的数据标识
/// </summary>
function Sign97To07(nSign:int64):int64;

implementation
//
//function GetDL645TypeStr( ADL645Type : TCOMMAND_DL645_TYPE ) :string;
//begin
//  Result := '';
//  case ADL645Type of
//    C_645_RESET_TIME:      Result := '广播校时';
//    C_645_READ_DATA:       Result := '读数据';
//    C_645_READ_NEXTDATA:   Result := '读后续数据';
//    C_645_READ_ADDR:       Result := '读通信地址';
//    C_645_WRITE_DATA:      Result := '写数据';
//    C_645_WRITE_ADDR:      Result := '写通信地址';
//    C_645_FREEZE:         Result := '冻结命令';
//    C_645_CHANGE_BAUD_RATE: Result := '改波通讯速率';
//    C_645_CHANGE_PWD:      Result := '改密码';
//    C_645_CLEAR_MAX_DEMAND: Result := '最大需量清零';
//    C_645_CLEAR_RDATA:      Result := '电表清零';
//    C_645_CLEAR_REVENT:     Result := '事件清零';
//    C_SET_WOUT_TYPE:       Result := '多功能口设置';
//    C_645_IDENTITY:       Result := '身份认证';
//    C_645_ONOFF_CONTROL :  Result := '费控功能';
//  end;
//end;

function Get07ErrStr( AErrType : TDL645_07_ERR ) : string;
begin
  case AErrType of
    de645_07None:          Result := '无错误';
    de645_07OverRate:      Result := '费率数超';
    de645_07OverDayTime:   Result := '日时段数超';
    de645_07OverYearTme:   Result := '年时区数超';
    de645_07BaudNotChange: Result := '通讯速率不能改';
    de645_07PwdError:      Result := '密码错误或未授权';
    de645_07NoneData:      Result := '无请求数据';
    de645_07OtherError:    Result := '其他错误';
    de645_07RepeatTopUp:    Result := '重复充值';
    de645_07ESAMError:    Result := 'ESAM验证失败';
    de645_07IdentityError:    Result := '身份认证失败';
    de645_07ClientSNError:    Result := '客户编号不匹配';
    de645_07TopUpTimesError:    Result := '充值次数错误';
    de645_07BuyOverError:    Result := '购电超囤积';
  end;
end;

function Sign97To07(nSign:int64):int64;
begin
  if nSign = $9010 then
    Result := $00000000
  else if nSign = $9011 then
    Result := $00000100
  else if nSign = $9012 then
    Result := $00000200
  else if nSign = $9013 then
    Result := $00000300
  else if nSign = $9014 then
    Result := $00000400
  else if nSign = $9110 then
    Result := $00030000
  else if nSign = $9120 then
    Result := $00040000
  else if nSign = $A010 then
    Result := $01010000
  else if nSign = $B010 then
    Result := $01010000
  else if nSign = $A110 then
    Result := $01020000
  else if nSign = $B110 then
    Result := $01020000

  else if nSign = $B611 then Result := $02010100
  else if nSign = $B612 then Result := $02010200
  else if nSign = $B613 then Result := $02010300
  else if nSign = $B621 then Result := $02020100
  else if nSign = $B622 then Result := $02020200
  else if nSign = $B623 then Result := $02020300
  else if nSign = $B630 then Result := $02030000
  else if nSign = $B631 then Result := $02030100
  else if nSign = $B632 then Result := $02030200
  else if nSign = $B633 then Result := $02030300
  else if nSign = $B640 then Result := $02040000
  else if nSign = $B641 then Result := $02040100
  else if nSign = $B642 then Result := $02040200
  else if nSign = $B643 then Result := $02040300
  else if nSign = $C030 then Result := $04000409
  else if nSign = $C031 then Result := $0400040A
  else if nSign = $C032 then Result := $04000402
  else if nSign = $C010 then Result := $04000101
  else if nSign = $C011 then Result := $04000102
  else
    Result := $00010000


//$C033
//$C034
//$C035






end;

end.















