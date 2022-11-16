unit Notify.Response;

interface

uses
  Notify.Response.Contract;

type
  TNotifyResonse = class sealed(TInterfacedObject, INotifyResponse)
  strict private
    FStatusCode: Integer;
    FContent: String;
  public
    function Content: string; overload;
    function Content(const PValue: String): INotifyResponse; overload;
    function StatusCode: Integer; overload;
    function StatusCode(const PValue: Integer): INotifyResponse; overload;
  end;

implementation

{ TNotifyResonse }

function TNotifyResonse.Content(const PValue: String): INotifyResponse;
begin
  Result := Self;
  FContent := PValue;
end;

function TNotifyResonse.StatusCode(const PValue: Integer): INotifyResponse;
begin
  Result := Self;
end;

function TNotifyResonse.StatusCode: Integer;
begin

end;

function TNotifyResonse.Content: string;
begin
  Result := Content;
end;

end.
