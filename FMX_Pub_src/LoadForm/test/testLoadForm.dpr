program testLoadForm;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestMain in 'TestMain.pas' {Form6},
  uLoadForm in '..\uLoadForm.pas' {fLoadForm},
  xConsts in '..\..\xConsts.pas',
  xFunction in '..\..\xFunction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TfLoadForm, fLoadForm);
  Application.Run;
end.
