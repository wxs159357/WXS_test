program ProGroupRightFmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainTest in 'uMainTest.pas' {fMainTest},
  uUserAction in '..\..\uUserAction.pas',
  uUserControl in '..\..\uUserControl.pas',
  uUserInfo in '..\..\uUserInfo.pas',
  uFramGroupList in '..\..\FMX\uFramGroupList.pas' {famGroupList: TFrame},
  uFramUserList in '..\..\FMX\uFramUserList.pas' {famUserList: TFrame},
  uLogn in '..\..\FMX\uLogn.pas' {fLogn},
  uNewGroup in '..\..\FMX\uNewGroup.pas' {fNewGroup},
  uNewUser in '..\..\FMX\uNewUser.pas' {fNewUser},
  uUpUsPass in '..\..\FMX\uUpUsPass.pas' {fUpUsPass},
  uUserOption in '..\..\FMX\uUserOption.pas' {fUserOption},
  xDBActionBase in '..\..\..\xDBActionBase.pas',
  xFunction in '..\..\..\xFunction.pas',
  xDBConn in '..\..\..\FMX\xDBConn.pas',
  xVCL_FMX in '..\..\..\FMX\xVCL_FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMainTest, fMainTest);
  Application.CreateForm(TfLogn, fLogn);
  Application.CreateForm(TfNewGroup, fNewGroup);
  Application.CreateForm(TfNewUser, fNewUser);
  Application.CreateForm(TfUpUsPass, fUpUsPass);
  Application.CreateForm(TfUserOption, fUserOption);
  Application.Run;
end.
