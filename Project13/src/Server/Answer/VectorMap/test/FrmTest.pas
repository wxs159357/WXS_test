unit FrmTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GDIPAPI, ExtCtrls,
  U_VECTOR_CONTROL, FrmVectorMapInfo;

type
  TForm17 = class(TForm)
    btn6: TButton;
    btn7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form17: TForm17;

implementation

{$R *.dfm}

procedure TForm17.btn6Click(Sender: TObject);
begin
  with TfVectorMapInfo.Create(nil) do
  begin
    NewVMap(AVectorControl);
    ShowModal;
  end;

  AVectorControl.Canvas := Self.Canvas;
  AVectorControl.Rect := Rect(50,50,730,550);
  AVectorControl.Draw;
end;

procedure TForm17.btn7Click(Sender: TObject);
begin
  with TfVectorMapInfo.Create(nil) do
  begin
    EditVMap(AVectorControl);
    ShowModal;
  end;

  AVectorControl.Canvas := Self.Canvas;
  AVectorControl.Rect := Rect(50,50,730,550);
  AVectorControl.Draw;
end;

procedure TForm17.FormCreate(Sender: TObject);
begin
  AVectorControl := TVECTOR_CONTROL.Create;
  AVectorControl.Canvas := Self.Canvas;
  AVectorControl.Rect := Rect(50,50,730,550);
end;

procedure TForm17.FormDestroy(Sender: TObject);
begin
  AVectorControl.Free;
end;

end.
