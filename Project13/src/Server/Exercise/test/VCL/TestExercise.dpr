program TestExercise;

uses
  Vcl.Forms,
  TestMain in 'TestMain.pas' {Form8},
  xExerciseAction in '..\..\xExerciseAction.pas',
  xExerciseControl in '..\..\xExerciseControl.pas',
  xExerciseInfo in '..\..\xExerciseInfo.pas',
  FrmExercise in '..\..\VCL\FrmExercise.pas' {fExercise};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TfExercise, fExercise);
  Application.Run;
end.
