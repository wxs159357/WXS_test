program TestSimpleInfoVCL;

uses
  Vcl.Forms,
  FrmTestMain in 'FrmTestMain.pas' {fTestMain},
  xSimpleInfoControl in '..\..\xSimpleInfoControl.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBConn in '..\..\..\VCL\xDBConn.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  uSimpleInfoList in '..\..\VCL\uSimpleInfoList.pas' {fSimpleInfoList},
  uSimpleInfo in '..\..\VCL\uSimpleInfo.pas' {fSimpleInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfTestMain, fTestMain);
  Application.Run;
end.
