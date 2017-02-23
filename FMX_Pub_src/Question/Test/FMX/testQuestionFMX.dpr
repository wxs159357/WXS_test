program testQuestionFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  testQ in 'testQ.pas' {Form6},
  xFunction in '..\..\..\xFunction.pas',
  xConsts in '..\..\..\xConsts.pas',
  xQuestionInfo in '..\..\xQuestionInfo.pas',
  xSortInfo in '..\..\xSortInfo.pas',
  xSortControl in '..\..\xSortControl.pas',
  xQuestionAction in '..\..\xQuestionAction.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xSortAction in '..\..\xSortAction.pas',
  uQuestionList in '..\..\FMX\uQuestionList.pas' {fQuestionList},
  uSortList in '..\..\FMX\uSortList.pas' {fSortList},
  uSortInfo in '..\..\FMX\uSortInfo.pas' {fSortInfo},
  uQuestionInfo in '..\..\FMX\uQuestionInfo.pas' {fQuestionInfo},
  xDBConn in '..\..\..\FMX\xDBConn.pas';

{$R *.res}

begin
  Application.Initialize;

  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';
  SortControl := TSortControl.Create;
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TfQuestionInfo, fQuestionInfo);
  Application.Run;

  SortControl.Free;
  ADBConn.Free;

end.
