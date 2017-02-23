program testAbout;

uses
  System.StartUpCopy,
  FMX.Forms,
  uAbout in '..\uAbout.pas' {fAbout},
  xFileVersion in '..\xFileVersion.pas',
  TestMain in 'TestMain.pas' {Form2},
  xFunction in '..\..\xFunction.pas',
  xBaseForm in '..\..\xBaseForm.pas' {fBaseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
