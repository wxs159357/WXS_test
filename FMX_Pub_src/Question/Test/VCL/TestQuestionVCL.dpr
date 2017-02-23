program TestQuestionVCL;

uses
  Vcl.Forms,
  TestMain in 'TestMain.pas' {fTestMain},
  xQuestionAction in '..\..\xQuestionAction.pas',
  xQuestionInfo in '..\..\xQuestionInfo.pas',
  xSortAction in '..\..\xSortAction.pas',
  xSortControl in '..\..\xSortControl.pas',
  xSortInfo in '..\..\xSortInfo.pas',
  xConsts in '..\..\..\xConsts.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBConn in '..\..\..\VCL\xDBConn.pas',
  uSortList in '..\..\VCL\uSortList.pas' {fSortList},
  uQuestionInfo in '..\..\VCL\uQuestionInfo.pas' {fQuestionInfo},
  uQuestionList in '..\..\VCL\uQuestionList.pas' {fQuestionList},
  uSortInfo in '..\..\VCL\uSortInfo.pas' {fSortInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';
  SortControl := TSortControl.Create;

  Application.CreateForm(TfTestMain, fTestMain);
  Application.Run;

  SortControl.Free;
  ADBConn.Free;





end.
