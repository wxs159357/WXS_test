program Project4;

uses
  Vcl.Forms,
  FrmRevScreenMain in '..\FrmRevScreenMain.pas' {fRevScreenMain},
  xUDPRevScreen in '..\xUDPRevScreen.pas',
  xCommBase in '..\..\..\..\..\FMX_Pub_src\Comm\xCommBase.pas',
  xUDPServerBase in '..\..\..\..\..\FMX_Pub_src\Comm\xUDPServerBase.pas',
  xConsts in '..\..\..\..\..\FMX_Pub_src\xConsts.pas',
  xFunction in '..\..\..\..\..\FMX_Pub_src\xFunction.pas',
  xTypes in '..\..\..\..\..\FMX_Pub_src\xTypes.pas',
  xVCL_FMX in '..\..\..\..\..\FMX_Pub_src\VCL\xVCL_FMX.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Emerald Light Slate');
  Application.CreateForm(TfRevScreenMain, fRevScreenMain);
  Application.Run;
end.
