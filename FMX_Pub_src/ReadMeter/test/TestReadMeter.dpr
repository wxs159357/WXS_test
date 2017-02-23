program TestReadMeter;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  xDL645Type in '..\xDL645Type.pas',
  xCommBase in '..\..\Comm\xCommBase.pas',
  xSerialBase in '..\..\Comm\xSerialBase.pas',
  xTCPClientBase in '..\..\Comm\xTCPClientBase.pas',
  xTCPServerBase in '..\..\Comm\xTCPServerBase.pas',
  xUDPServerBase in '..\..\Comm\xUDPServerBase.pas',
  xFunction in '..\..\xFunction.pas',
  xTypes in '..\..\xTypes.pas',
  xMeterDataRect in '..\xMeterDataRect.pas',
  xProtocolPacks in '..\..\xProtocolPacks.pas',
  xProtocolPacksDl645 in '..\xProtocolPacksDl645.pas',
  xConsts in '..\..\xConsts.pas',
  xProtocolPacksDL645_97 in '..\xProtocolPacksDL645_97.pas',
  xProtocolPacksDL645_07 in '..\xProtocolPacksDL645_07.pas',
  xDL645Thread in '..\xDL645Thread.pas',
  xThreadBase in '..\..\xThreadBase.pas',
  xProtocolBase in '..\..\xProtocolBase.pas',
  xProtocolDL645 in '..\xProtocolDL645.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
