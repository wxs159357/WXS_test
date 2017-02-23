program TestStudentFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  uStudentInfo in '..\..\FMX\uStudentInfo.pas' {FrmStudentInfo},
  uStudentList in '..\..\FMX\uStudentList.pas' {fStudentList},
  xStudentAction in '..\..\xStudentAction.pas',
  xStudentControl in '..\..\xStudentControl.pas',
  xStudentInfo in '..\..\xStudentInfo.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xFunction in '..\..\..\xFunction.pas',
  uTest in 'uTest.pas' {Form2},
  xStudentTabelOutOrIn in '..\..\xStudentTabelOutOrIn.pas',
  xDBConn in '..\..\..\FMX\xDBConn.pas',
  xVCL_FMX in '..\..\..\FMX\xVCL_FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
