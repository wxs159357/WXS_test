program Project2;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  U_WE_DIAGRAM in 'U_WE_DIAGRAM.pas',
  FrmWESelect2 in '..\FrmWESelect2.pas' {fWESelect2},
  U_WIRING_ERROR in '..\U_WIRING_ERROR.pas',
  U_WE_DiagramElement in 'U_WE_DiagramElement.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TfWESelect2, fWESelect2);
  Application.Run;
end.
