program TestSimpleInfoVCL;

uses
  Vcl.Forms,
  FrmTestMain in 'FrmTestMain.pas' {fTestMain},
  xSimpleInfoControl in '..\..\xSimpleInfoControl.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xConsts in '..\..\..\xConsts.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBConn in '..\..\..\VCL\xDBConn.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  FrmSimpleInfoList in '..\..\VCL\FrmSimpleInfoList.pas' {fSimpleInfoList},
  FrmSimpleInfo in '..\..\VCL\FrmSimpleInfo.pas' {fSimpleInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfTestMain, fTestMain);
  Application.Run;
end.
