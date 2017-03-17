{===============================================================================
  Copyright(c) 2013, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  + 考卷数据操作类

===============================================================================}
unit xPaperAction;

interface

uses Classes, SysUtils, ADODB, xDBActionBase;

type
  /// <summary>
  /// 考卷数据操作类
  /// </summary>
  TPaperAction = class(TDBActionBase)
  private

  public
    /// <summary>
    /// 编辑考生分数
    /// </summary>
    procedure EditStuScore(nPaperID, nStuID : Integer; dScore : Double);

    /// <summary>
    /// 删除考卷
    /// </summary>
    procedure DelPaper(nPaperID, nStuID : Integer);

  end;
var
  APaperAction : TPaperAction;

implementation

{ TPaperAction }

procedure TPaperAction.DelPaper(nPaperID, nStuID: Integer);
const
  C_SQL = 'delete from PSTUInfo where PaperID = %d and STUNumber = %d';
  C_SQL1 = 'delete from PSTU_ANSWER where PaperID = %d and STUNumber = %d';
  C_SQL2 = 'seledt * from PSTUInfo where PaperID = %d and STUNumber = %d';
  C_SQL3 = 'delete from PExamInfo where PaperID = %d';
begin
  FQuery.SQL.Text := Format(C_SQL, [nPaperID, nStuID]);
  ExecSQL;

  FQuery.SQL.Text := Format(C_SQL1, [nPaperID, nStuID]);
  ExecSQL;

  FQuery.Open(Format(C_SQL, [nPaperID, nStuID]));
  if FQuery.RecordCount = 0 then
  begin
    FQuery.Close;
    FQuery.SQL.Text := Format(C_SQL3, [nPaperID]);
    ExecSQL;
  end
  else
  begin
    FQuery.Close;
  end;


end;

procedure TPaperAction.EditStuScore(nPaperID, nStuID: Integer; dScore: Double);
const
  C_SQL = 'update PSTUInfo set StuScore = %f where PaperID = %d and STUNumber = %d';
begin
  FQuery.SQL.Text := Format(C_SQL, [dScore, nPaperID, nStuID]);
  ExecSQL;
end;

end.
