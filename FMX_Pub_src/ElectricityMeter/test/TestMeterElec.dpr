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
  xElecPower in '..\xElecPower.pas',
  xElecBusLine in '..\xElecBusLine.pas',
  xElecTV in '..\xElecTV.pas',
  xElecTA in '..\xElecTA.pas',
  xElecBreak in '..\xElecBreak.pas',
  xElecLineBox in '..\xElecLineBox.pas',
  xElecBox in '..\xElecBox.pas',
  xWiringError in '..\xWiringError.pas',
  xFunction in '..\..\xFunction.pas',
  xBaseForm in '..\..\FMX\xBaseForm.pas' {fBaseForm},
  xDBConn in '..\..\FMX\xDBConn.pas',
  xVCL_FMX in '..\..\FMX\xVCL_FMX.pas',
  Unit3 in 'Unit3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
