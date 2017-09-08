program TestDBUpdate;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  xDBActionBase in '..\xDBActionBase.pas',
  xDBUpdate in '..\xDBUpdate.pas',
  xFunction in '..\xFunction.pas',
  xConsts in '..\xConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
