program Project2;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  xVectorLine in '..\xVectorLine.pas',
  xVectorMap in '..\xVectorMap.pas',
  xVectorArc in '..\xVectorArc.pas',
  xVectorType in '..\xVectorType.pas',
  uVectorMap in '..\uVectorMap.pas' {fVectorMap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TfVectorMap, fVectorMap);
  Application.Run;
end.
