program Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  xConsts in '..\xConsts.pas',
  xFunction in '..\xFunction.pas',
  uAbout in '..\About\uAbout.pas' {FrmAbout},
  xDBConn in '..\xDBConn.pas',
  xDBActionBase in '..\xDBActionBase.pas',
  xDBUpdate in '..\xDBUpdate.pas',
  xCommBase in '..\Comm\xCommBase.pas',
  xTypes in '..\xTypes.pas',
  xSerialBase in '..\Comm\xSerialBase.pas',
  xTCPClientBase in '..\Comm\xTCPClientBase.pas',
  xThreadBase in '..\xThreadBase.pas',
  xTCPServerBase in '..\Comm\xTCPServerBase.pas',
  xUDPServerBase in '..\Comm\xUDPServerBase.pas',
  xProtocolBase in '..\xProtocolBase.pas',
  xProtocolType in '..\xProtocolType.pas',
  xProtocolDev in '..\xProtocolDev.pas',
  xExceptionCatch in '..\xExceptionCatch.pas',
  xHttpServer in '..\xHttpServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
