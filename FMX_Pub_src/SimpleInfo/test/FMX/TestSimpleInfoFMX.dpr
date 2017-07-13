program TestSimpleInfoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  xTestMain in 'xTestMain.pas' {Form8},
  xDBConn in '..\..\..\FMX\xDBConn.pas',
  xVCL_FMX in '..\..\..\FMX\xVCL_FMX.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xSimpleInfoControl in '..\..\xSimpleInfoControl.pas',
  uSImpleInfo in '..\..\FMX\uSImpleInfo.pas' {fSimpleInfo},
  uSimpleInfoList in '..\..\FMX\uSimpleInfoList.pas' {fSimpleInfoList};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
