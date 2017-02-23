{===============================================================================
  Copyright(c) 2014, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  学员信息导入导出单元

  + TStudentTableOption  学员信息导入导出类

===============================================================================}
unit xStudentTabelOutOrIn;

interface

uses
  System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}
  System.Win.ComObj,
  {$ENDIF}
  xVCL_FMX;

type
  /// <summary>
  /// 学员信息导入导出类
  /// </summary>
  TStudentTableOption = class
  private
    {$IFDEF MSWINDOWS}
    FDlgSaveExecl : TMySaveDialog;
    FDlgOpenExecl : TMyOpenDialog;

    /// <summary>
    /// 导出学员时，对Excel设置合适的列宽，并初始化表头
    /// </summary>
    procedure SetExcelWideIn(AExcel: Variant);
    {$ENDIF}

  public
    /// <summary>
    /// 将将Excel表格中的学员信息导入
    /// </summary>
    /// <returns>0表示成功，1表示失败，2表示操作者在 OpenDialog 控件中点击了取消，没有导入Excel</returns>
    function ImportStu(slStus : TStringList) : Integer;

    /// <summary>
    /// 将学生信息导出到Excel表格中
    /// </summary>
    /// <returns>0表示成功，1表示失败，2表示操作者在 SaveDialog 控件中点击了取,没有导出记录</returns>
    function ExportStu(slStus:TStringList ):Integer;
  end;
implementation

uses
  xStudentInfo;

{ TStudentTableOption }

function TStudentTableOption.ExportStu(slStus: TStringList): Integer;
{$IFDEF MSWINDOWS}
var
  sExeclFile : string;
  AObjExecl : Variant;
  i : integer;
  ASheet: Variant;
{$ENDIF}
begin
  result := 1;
  {$IFDEF MSWINDOWS}
  if  Assigned(slStus) then
  begin
    FDlgSaveExecl := TMySaveDialog.Create(nil);

    //设置文件格式
    FDlgSaveExecl.Filter := 'Excel files (*.xls)|.xls|所有文件(*.*)|*.*';

    //设置后缀并名
    FDlgSaveExecl.DefaultExt:='.xls';

    if FDlgSaveExecl.Execute then
    begin
      sExeclFile := FDlgSaveExecl.FileName;
      try
        // 新建Excel 初始化
        AObjExecl := CreateOleObject('Excel.application');
        AObjExecl.Visible := False;

        //初始化单元格
        AObjExecl.WorkBooks.Add(-4167);
        AObjExecl.WorkBooks[1].Sheets[1].name :='学员信息';
        ASheet := AObjExecl.WorkBooks[1].Sheets['学员信息'];
        SetExcelWideIn(AObjExecl);

        //对象信息导入Excel
        for i := slStus.Count - 1 downto 0  do
        begin
          with TStudentInfo(slStus.Objects[i]) do
          begin
            AObjExecl.Cells[i+2,1].Value := stuName;
            AObjExecl.Cells[i+2,2].Value := stuSex;
            AObjExecl.Cells[i+2,3].Value := stuIDcard;
            AObjExecl.Cells[i+2,4].Value := stuLogin;
            AObjExecl.Cells[i+2,5].Value := stuPwd;
            AObjExecl.Cells[i+2,6].Value := stuArea;
            AObjExecl.Cells[i+2,7].Value := stuTel;
            AObjExecl.Cells[i+2,8].Value := stuNote1;
          end;
        end;

        //存储新建Excel
        ASheet.SaveAs( sExeclFile );
        AObjExecl.WorkBooks.Close;
        AObjExecl.Quit;
        FDlgSaveExecl.Free;
        VarClear(AObjExecl);
        Result := 0;
      except
        AObjExecl.WorkBooks.Close;
        AObjExecl.Quit;
        AObjExecl.Free;
        VarClear(AObjExecl);
      end;
    end
    else
    begin
      FDlgSaveExecl.Free;
      Result := 2;
    end;
  end;
  {$ENDIF}
end;

function TStudentTableOption.ImportStu(slStus: TStringList): Integer;
{$IFDEF MSWINDOWS}
var
  sExeclFile : string;
  AObjExecl : Variant;
  i : Integer;
  AStuInfo : TStudentInfo;
{$ENDIF}
begin
  Result := 1;

  {$IFDEF MSWINDOWS}
  if Assigned(slStus) then
  begin
    FDlgOpenExecl := TMyOpenDialog.Create(nil);
    FDlgOpenExecl.Filter :='Excel files (*.xls)|*.xls';

    if FDlgOpenExecl.Execute then
    begin
      sExeclFile := FDlgOpenExecl.FileName;
      AObjExecl :=  CreateOleObject('Excel.application');
      AObjExecl.WorkBooks.Open( sExeclFile );
      AObjExecl.Visible := False;
      try
        if AObjExecl.WorkSheets[1].UsedRange.Columns.Count <> 8 then
        begin
          AObjExecl.WorkBooks.Close;
          AObjExecl.Quit;
          varclear(AObjExecl);
          result := 1;
          Exit;
        end;

        //读取Excel中的记录到学员对象
        for i := 2 to AObjExecl.WorkSheets[1].UsedRange.Rows.Count do
        begin
          AStuInfo := TStudentInfo.Create;
          with AStuInfo do
          begin
            stuName          := AObjExecl.Cells[i,1].Value;
            stuSex           := AObjExecl.Cells[i,2].Value;
            stuIDcard        := AObjExecl.Cells[i,3].Value;
            stuLogin         := AObjExecl.Cells[i,4].Value;
            stuPwd           := AObjExecl.Cells[i,5].Value;
            stuArea          := AObjExecl.Cells[i,6].Value;
            stuTel           := AObjExecl.Cells[i,7].Value;
            stuNote1         := AObjExecl.Cells[i,8].Value;
            stuNote2         := AObjExecl.Cells[i,9].Value;
            slStus.AddObject('' ,AStuInfo);
          end;
        end;
        AObjExecl.WorkBooks.Close;
        AObjExecl.Quit;
        VarClear(AObjExecl);
        FDlgOpenExecl.free;
        Result := 0;
      except
        AObjExecl.WorkBooks.Close;
        AObjExecl.Quit;
        VarClear(AObjExecl);
      end;
    end
    else
    begin
      FDlgOpenExecl.Free;
      Result := 2;
    end;
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TStudentTableOption.SetExcelWideIn(AExcel: Variant);
begin
  // 初始化表头
  AExcel.Cells[1,1].Value := '姓名';
  AExcel.Cells[1,2].Value := '性别';
  AExcel.Cells[1,3].Value := '身份证号';
  AExcel.Cells[1,4].Value := '登陆名';
  AExcel.Cells[1,5].Value := '密码';
  AExcel.Cells[1,6].Value := '所在地';
  AExcel.Cells[1,7].Value := '联系电话';
  AExcel.Cells[1,8].Value := '备注';

  //初始化列宽
  AExcel.ActiveSheet.Columns[1].ColumnWidth := 10;
  AExcel.ActiveSheet.Columns[2].ColumnWidth := 8;
  AExcel.ActiveSheet.Columns[3].ColumnWidth := 20;
  AExcel.ActiveSheet.Columns[4].ColumnWidth := 20;
  AExcel.ActiveSheet.Columns[5].ColumnWidth := 15;
  AExcel.ActiveSheet.Columns[6].ColumnWidth := 8;
  AExcel.ActiveSheet.Columns[7].ColumnWidth := 12;
  AExcel.ActiveSheet.Columns[8].ColumnWidth := 15;

  //将指定列设置成文本格式
  AExcel.ActiveSheet.Columns[3].NumberFormatLocal := '@';
  AExcel.ActiveSheet.Columns[4].NumberFormatLocal := '@';
  AExcel.ActiveSheet.Columns[5].NumberFormatLocal := '@';
  AExcel.ActiveSheet.Columns[7].NumberFormatLocal := '@';
  AExcel.ActiveSheet.Columns[8].NumberFormatLocal := '@';
end;
{$ENDIF}

end.
