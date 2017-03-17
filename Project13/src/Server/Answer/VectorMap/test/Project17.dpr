program Project17;

uses
  Forms,
  FrmTest in 'FrmTest.pas' {Form17},
  U_VECTOR_LIEN_INFO in '..\U_VECTOR_LIEN_INFO.pas',
  U_VECTOR_CONTROL in '..\U_VECTOR_CONTROL.pas',
  FrmVectorMapInfo in '..\FrmVectorMapInfo.pas' {fVectorMapInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm17, Form17);
  Application.CreateForm(TfVectorMapInfo, fVectorMapInfo);
  Application.Run;
end.
