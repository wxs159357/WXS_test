program TestMeterElec;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  xElecFunction in '..\xElecFunction.pas',
  xElecPoint in '..\xElecPoint.pas',
  xElecLine in '..\xElecLine.pas',
  xElecOrgan in '..\xElecOrgan.pas',
  xElecMeter in '..\xElecMeter.pas',
  xElecPower in '..\xElecPower.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
