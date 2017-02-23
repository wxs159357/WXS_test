program TestStudentVCL;

uses
  Vcl.Forms,
  FrmTestMain in 'FrmTestMain.pas' {fTestMain},
  xStudentAction in '..\..\xStudentAction.pas',
  xStudentControl in '..\..\xStudentControl.pas',
  xStudentInfo in '..\..\xStudentInfo.pas',
  xStudentTabelOutOrIn in '..\..\xStudentTabelOutOrIn.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  uStudentInfo in '..\..\VCL\uStudentInfo.pas' {fStudentInfo},
  xFunction in '..\..\..\xFunction.pas',
  uStudentList in '..\..\VCL\uStudentList.pas' {fStudentList},
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xDBConn in '..\..\..\VCL\xDBConn.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfTestMain, fTestMain);
  Application.Run;
end.
