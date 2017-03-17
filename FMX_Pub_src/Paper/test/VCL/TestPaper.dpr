program TestPaper;

uses
  Vcl.Forms,
  TestMain in 'TestMain.pas' {fTestMain},
  uPaperBase in '..\..\VCL\uPaperBase.pas' {fPaperBase},
  uPaperInfoBase in '..\..\VCL\uPaperInfoBase.pas' {fPaperInfoBase},
  xDBConn in '..\..\..\VCL\xDBConn.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xFunction in '..\..\..\xFunction.pas',
  xPaperAction in '..\..\xPaperAction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfTestMain, fTestMain);
  Application.Run;
end.
