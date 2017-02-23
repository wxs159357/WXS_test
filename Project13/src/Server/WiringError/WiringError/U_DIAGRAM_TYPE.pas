unit U_DIAGRAM_TYPE;

interface

uses SysUtils;

type
  /// <summary> 接线图类型 </summary>
  TDiagramType = (
    dt3CTClear,           //三相三线CT简化
    dt3M,                 //三相三线多功能
    dt3L4,                //三相三线四线制

    dt4M_NoPT,            //三相四线多功能无PT
    dt4Direct,            //三相四线直通
    dt4_NoPT_L6,          //三相四线无PT六线制

    dt4M_PT,              //三相四线多功能有PT
    dt4_PT_CT_CLear,      //三相四线经PT，CT简化
    dt4_PT_L6             //三相四线经PT，六线制
    
    );

function DiagramTypeToStr(ADiagramType: TDiagramType): string;
function DiagramStrToType(sDiagramStr: string): TDiagramType;


implementation

function DiagramTypeToStr(ADiagramType: TDiagramType): string;
begin
  case ADiagramType of
    dt3CTClear:      Result := '三相三线CT简化';
    dt3M:            Result := '三相三线多功能';
    dt3L4:           Result := '三相三线四线制';

    dt4M_NoPT:       Result := '三相四线多功能无PT';
    dt4M_PT:         Result := '三相四线多功能有PT';
    dt4_PT_CT_CLear: Result := '三相四线经PT,CT简化';
    dt4_PT_L6:       Result := '三相四线经PT六线制';
    dt4_NoPT_L6:     Result := '三相四线无PT六线制';
    dt4Direct:       Result := '三相四线直通';
  else
    raise Exception.Create('Unrecognized');
  end;
end;
function DiagramStrToType(sDiagramStr: string): TDiagramType;
begin
  if sDiagramStr = '三相三线CT简化' then
    Result := dt3CTClear
  else if sDiagramStr = '三相三线多功能' then
    Result := dt3M
  else if sDiagramStr = '三相三线四线制' then
    Result := dt3L4
  else if sDiagramStr = '三相四线多功能无PT' then
    Result := dt4M_NoPT
  else if sDiagramStr = '三相四线多功能有PT' then
    Result := dt4M_PT
  else if sDiagramStr = '三相四线经PT,CT简化' then
    Result := dt4_PT_CT_CLear
  else if sDiagramStr = '三相四线经PT六线制' then
    Result := dt4_PT_L6
  else if sDiagramStr = '三相四线无PT六线制' then
    Result := dt4_NoPT_L6
  else if sDiagramStr = '三相四线直通' then
    Result := dt4Direct
  else
    Result := dt3L4;
end;

end.
