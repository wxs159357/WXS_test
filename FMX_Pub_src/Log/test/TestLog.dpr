program TestLog;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestLogMain in 'TestLogMain.pas' {Form6},
  uLog in '..\uLog.pas' {fLog},
  xFunction in '..\..\xFunction.pas',
  xTypes in '..\..\xTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
