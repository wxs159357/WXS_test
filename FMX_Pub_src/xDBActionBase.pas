{===============================================================================
  数据库操作基类

===============================================================================}
unit xDBActionBase;

interface

uses xDBConn, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TDBActionBase = class
  private

  protected
    FQuery :TFDQuery;


  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Query : TFDQuery read FQuery write FQuery;

    /// <summary>
    /// 执行SQL 语句
    /// </summary>
    procedure ExecSQL;

  end;

implementation

{ TDBAction }

constructor TDBActionBase.Create;
begin
  FQuery :=TFDQuery.Create(nil);
  FQuery.Connection := ADBConn.Connection;
end;

destructor TDBActionBase.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TDBActionBase.ExecSQL;
begin
  try
    FQuery.ExecSQL;
  except

  end;
end;

end.

