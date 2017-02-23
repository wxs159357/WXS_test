program ProUserOptionVcl;

uses
  Vcl.Forms,
  frmTest in 'frmTest.pas' {fMainTest},
  uUserAction in '..\..\uUserAction.pas',
  uUserControl in '..\..\uUserControl.pas',
  uUserInfo in '..\..\uUserInfo.pas',
  frmLogn in '..\..\VCL\frmLogn.pas' {fLogn},
  uUpUsPass in '..\..\VCL\uUpUsPass.pas' {fUpUsPass},
  frmUserGroupInfo in '..\..\VCL\frmUserGroupInfo.pas' {fUserGroupInfo},
  frmUserGroupList in '..\..\VCL\frmUserGroupList.pas' {fUserGroupList},
  frmUserList in '..\..\VCL\frmUserList.pas' {fUserList},
  frmUserNew in '..\..\VCL\frmUserNew.pas' {fUserNew},
  frmUserOption in '..\..\VCL\frmUserOption.pas' {fUserOption},
  xDBConn in '..\..\..\VCL\xDBConn.pas',
  xVCL_FMX in '..\..\..\VCL\xVCL_FMX.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBActionBase in '..\..\..\xDBActionBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMainTest, fMainTest);
  Application.CreateForm(TfLogn, fLogn);
  Application.CreateForm(TfUpUsPass, fUpUsPass);
  Application.CreateForm(TfUserGroupInfo, fUserGroupInfo);
  Application.CreateForm(TfUserGroupList, fUserGroupList);
  Application.CreateForm(TfUserList, fUserList);
  Application.CreateForm(TfUserNew, fUserNew);
  Application.CreateForm(TfUserOption, fUserOption);
  Application.Run;
end.
