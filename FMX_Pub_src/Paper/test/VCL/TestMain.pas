unit TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, xDBConn, uPaperBase, Vcl.StdCtrls,
  xPaperAction;

type
  TfTestMain = class(TForm)
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTestMain: TfTestMain;

implementation

{$R *.dfm}

procedure TfTestMain.btn1Click(Sender: TObject);
begin
  with TfPaperBase.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfTestMain.FormCreate(Sender: TObject);
begin
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa';

  APaperAction := TPaperAction.Create;
end;

procedure TfTestMain.FormDestroy(Sender: TObject);
begin
  APaperAction.Free;
  ADBConn.Free;
end;

end.
