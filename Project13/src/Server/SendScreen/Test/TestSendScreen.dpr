program TestSendScreen;

uses
  Vcl.Forms,
  Unit8 in 'Unit8.pas' {Form8},
  xProtocolSendScreen in '..\xProtocolSendScreen.pas',
  xThreadUDPSendScreen in '..\xThreadUDPSendScreen.pas',
  xCommBase in '..\..\..\..\..\FMX_Pub_src\Comm\xCommBase.pas',
  xSerialBase in '..\..\..\..\..\FMX_Pub_src\Comm\xSerialBase.pas',
  xTCPClientBase in '..\..\..\..\..\FMX_Pub_src\Comm\xTCPClientBase.pas',
  xTCPServerBase in '..\..\..\..\..\FMX_Pub_src\Comm\xTCPServerBase.pas',
  xUDPServerBase in '..\..\..\..\..\FMX_Pub_src\Comm\xUDPServerBase.pas',
  xVCL_FMX in '..\..\..\..\..\FMX_Pub_src\VCL\xVCL_FMX.pas',
  xConsts in '..\..\..\..\..\FMX_Pub_src\xConsts.pas',
  xFunction in '..\..\..\..\..\FMX_Pub_src\xFunction.pas',
  xThreadBase in '..\..\..\..\..\FMX_Pub_src\xThreadBase.pas',
  xTypes in '..\..\..\..\..\FMX_Pub_src\xTypes.pas',
  xProtocolBase in '..\..\..\..\..\FMX_Pub_src\xProtocolBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
