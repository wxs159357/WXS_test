unit U_ERRORF_TO_WIRING;

interface

uses Classes, SysUtils, U_CKM_DEVICE, U_WIRING_ERROR, U_WIRINGF_ERROR;

/// <summary>
/// 2002F错误转换成两表位错误
/// </summary>
/// <param name="AErrorF">2002F错误对象</param>
/// <param name="AOldError">表位1错误对象</param>
/// <param name="AErrorMeter2">表位2错误对象</param>
/// <returns>是否转成功</returns>
function ErrorFToWiring(AErrorF : TWIRINGF_ERROR; AErrorMeter : TWIRING_ERROR): Boolean;

implementation

function ErrorFToWiring(AErrorF : TWIRINGF_ERROR; AErrorMeter : TWIRING_ERROR): Boolean;
  function GetSequence( ASequence : TORGAN_LINK; bIsVol : Boolean) : TWE_SEQUENCE_TYPE;
  var
    s : string;
  begin
    s := ASequence.OrganStr;

    if (AErrorF.PhaseType = ptfThree) and (not bIsVol) then
    begin
      if s = 'ac' then Result := stABC
      else if s = 'ca' then Result := stCBA
      else
        Result := stABC;
    end
    else
    begin
      if s = 'abc' then Result := stABC
      else if s = 'acb' then Result := stACB
      else if s = 'bac' then Result := stBAC
      else if s = 'bca' then Result := stBCA
      else if s = 'cab' then Result := stCAB
      else if s = 'cba' then Result := stCBA
      else
        Result := stABC;
    end;
  end;

  procedure SetMeterError(AOldError: TWIRING_ERROR; ANewError: TMETER_ERROR);
  var
    sSequenceStr : string;
//    a : Boolean;
  begin
    // 电表1错误
    with AErrorF, ANewError do
    begin


      if AErrorF.PhaseType = ptfFour then
      begin
        if AErrorF.IsTrans then
        begin
          AOldError.PhaseType := ptFourPT
        end
        else
        begin
          AOldError.PhaseType := ptFour;
        end;
      end
      else
        AOldError.PhaseType := ptThree;

      // 一次电压断相
      AOldError.UaBroken := FirError.AValue;
      AOldError.UbBroken := FirError.BValue;
      AOldError.UcBroken := FirError.CValue;

      // 接地断开
      AOldError.GroundBroken := SenError.SenUGroundBroken;

      // 二次极性反
      AOldError.PT1Reverse := SenError.SenUReverse.AValue;
      AOldError.PT2Reverse := SenError.SenUReverse.BValue;
      AOldError.PT3Reverse := SenError.SenUReverse.CValue;


      // 电压相序
      AOldError.USequence := GetSequence(ANewError.USequence, True);

      // 电流开路 电流短路  电压断相  反接
      sSequenceStr := ANewError.ISequence.OrganStr;

      if sSequenceStr = 'abc' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.AValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.BValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.CValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.AValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.BValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.CValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.AValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.BValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.CValue;
      end
      else if sSequenceStr = 'acb' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.AValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.CValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.BValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.AValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.CValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.BValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.AValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.CValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.BValue;
      end
      else if sSequenceStr = 'bac' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.BValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.AValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.CValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.BValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.AValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.CValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.BValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.AValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.CValue;
      end
      else if sSequenceStr = 'bca' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.CValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.AValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.BValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.CValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.AValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.BValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.CValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.AValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.BValue;
      end
      else if sSequenceStr = 'cab' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.BValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.CValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.AValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.BValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.CValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.AValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.BValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.CValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.AValue;
      end
      else if sSequenceStr = 'cba' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.CValue;
        AOldError.IbBroken := SenError.SenIBroken.BValue or IBroken.BValue;
        AOldError.IcBroken := SenError.SenIBroken.CValue or IBroken.AValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.CValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.BValue;
        AOldError.CT3Short := SenError.SenIShort.CValue or IShort.AValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue xor IReverse.CValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue xor IReverse.BValue;
        AOldError.CT3Reverse := SenError.SenIReverse.CValue xor IReverse.AValue;
      end
      else if sSequenceStr = 'ac' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.AValue;
        AOldError.IcBroken := SenError.SenIBroken.BValue or IBroken.CValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.AValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.CValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue;
      end
      else if sSequenceStr = 'ca' then
      begin
        AOldError.IaBroken := SenError.SenIBroken.AValue or IBroken.AValue;
        AOldError.IcBroken := SenError.SenIBroken.BValue or IBroken.CValue;

        AOldError.CT1Short := SenError.SenIShort.AValue or IShort.AValue;
        AOldError.CT2Short := SenError.SenIShort.BValue or IShort.CValue;

        AOldError.CT1Reverse := SenError.SenIReverse.AValue;
        AOldError.CT2Reverse := SenError.SenIReverse.BValue;
      end;

      sSequenceStr := ANewError.USequence.OrganStr;
      if sSequenceStr = 'abc' then
      begin
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.AValue;
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.BValue;
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.CValue;
      end
      else if sSequenceStr = 'acb' then
      begin
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.AValue;
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.BValue;
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.CValue;
      end
      else if sSequenceStr = 'bac' then
      begin
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.AValue;
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.BValue;
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.CValue;
      end
      else if sSequenceStr = 'bca' then
      begin
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.CValue;
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.AValue;
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.BValue;
      end
      else if sSequenceStr = 'cab' then
      begin
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.BValue;
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.CValue;
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.AValue;
      end
      else if sSequenceStr = 'cba' then
      begin
        AOldError.UscBroken := SenError.SenUBroken.CValue or UBroken.AValue;
        AOldError.UsbBroken := SenError.SenUBroken.BValue or UBroken.BValue;
        AOldError.UsaBroken := SenError.SenUBroken.AValue or UBroken.CValue;
      end;

      AOldError.InBroken := False;

      // 三相四线表尾电流反
//      if AErrorF.PhaseType = ptfFour  then
//      begin
//        AOldError.I1Reverse := IReverse.AValue;
//        AOldError.I2Reverse := IReverse.BValue;
//        AOldError.I3Reverse := IReverse.CValue;
//      end;


      // 接地断开
      if AErrorF.PhaseType = ptfFour  then
      begin
        AOldError.IaGroundBroken := SenError.SenIGroundBroken.AValue ;
        AOldError.IbGroundBroken := SenError.SenIGroundBroken.BValue ;
        AOldError.IcGroundBroken := SenError.SenIGroundBroken.CValue ;
      end
      else
      begin
        AOldError.IaGroundBroken := SenError.SenIGroundBroken.AValue ;
        AOldError.IcGroundBroken := SenError.SenIGroundBroken.bValue ;
      end;


      // 表尾 电流相序
      if AErrorF.PhaseType = ptfFour  then
      begin

        AOldError.ISequence := GetSequence(ANewError.ISequence, False);
//        case ANewError.ISequence of
//          stfABC, stfAC:
//          begin
//            AOldError.I1In := plA;
//            AOldError.I2In := plB;
//            AOldError.I3In := plC;
//          end;
//          stfACB:
//          begin
//            AOldError.I1In := plA;
//            AOldError.I2In := plC;
//            AOldError.I3In := plB;
//          end;
//          stfBAC:
//          begin
//            AOldError.I1In := plB;
//            AOldError.I2In := plA;
//            AOldError.I3In := plC;
//          end;
//          stfBCA:
//          begin
//            AOldError.I1In := plB;
//            AOldError.I2In := plC;
//            AOldError.I3In := plA;
//          end;
//          stfCAB:
//          begin
//            AOldError.I1In := plC;
//            AOldError.I2In := plA;
//            AOldError.I3In := plB;
//          end;
//          stfCBA, stfCA:
//          begin
//            AOldError.I1In := plC;
//            AOldError.I2In := plB;
//            AOldError.I3In := plA;
//          end;
//        end;
//        AOldError.I1Out := plN;
//        AOldError.I2Out := plN;
//        AOldError.I3Out := plN;
      end
      else
      begin
        sSequenceStr := ANewError.ISequence.OrganStr;
        if (sSequenceStr = 'abc') or (sSequenceStr = 'ac') then
        begin
          if IReverse.AValue then
          begin
            AOldError.I1In  := plN;
            AOldError.I1Out := plA;
          end
          else
          begin
            AOldError.I1In  := plA;
            AOldError.I1Out := plN;
          end;

          if IReverse.CValue then
          begin
            AOldError.I2In  := plN;
            AOldError.I2Out := plC;
          end
          else
          begin
            AOldError.I2In  := plC;
            AOldError.I2Out := plN;
          end;
        end
        else
        begin
          if IReverse.AValue then
          begin
            AOldError.I1In  := plN;
            AOldError.I1Out := plC;
          end
          else
          begin
            AOldError.I1In  := plC;
            AOldError.I1Out := plN;

          end;

          if IReverse.CValue then
          begin
            AOldError.I2In  := plN;
            AOldError.I2Out := plA;
          end
          else
          begin
            AOldError.I2In  := plA;
            AOldError.I2Out := plN;
          end;
        end;
      end;
    end;
  end;
begin
  if Assigned(AErrorF) and Assigned(AErrorMeter) then
  begin
    Result := True;
    // 转换表位故障
    SetMeterError(AErrorMeter, AErrorF.MeterError);
  end
  else
  begin
    Result := False;
  end;


end;

end.

