program testAboutFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestMain in 'TestMain.pas' {Form2},
  uAbout in '..\..\FMX\uAbout.pas' {fAbout},
  xFunction in '..\..\..\xFunction.pas',
  xVCL_FMX in '..\..\..\FMX\xVCL_FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TfAbout, fAbout);
  Application.Run;
end.
