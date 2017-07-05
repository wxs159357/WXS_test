program TestAboutVCL;

uses
  Vcl.Forms,
  FrmTestMain in 'FrmTestMain.pas' {Form8},
  xFunction in '..\..\..\xFunction.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  FrmAbout in '..\..\VCL\FrmAbout.pas' {fAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TfAbout, fAbout);
  Application.Run;
end.
