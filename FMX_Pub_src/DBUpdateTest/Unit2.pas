unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, xDBUpdate, xDBConn, xConsts;

type
  TForm2 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btn1Click(Sender: TObject);
begin
  with TDBUpdate.Create do
  begin
    Update;
    Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSAcc;Database='+spubFilePath+'dbdemos.mdb';
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ADBConn.Free;
end;

end.
