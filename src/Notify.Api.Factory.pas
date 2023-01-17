unit Notify.Api.Factory;

interface

uses
  Notify.Api.Contract;

type
  TNotifyApiFactory = class(TInterfacedObject, INotifyApiFactory)
  public
    class function New: INotifyApiFactory;
    function Api: INotifyApi;
  end;

implementation

uses
  Notify.Api.NetHTTP,
  Notify.Api.Indy;

{ TNotifyApiFactory }

function TNotifyApiFactory.Api: INotifyApi;
begin
  {$IFDEF NTFY_HTTP_INDY}
  Result := TNotifyApiIndy.New
  {$ELSE}
  Result := TNotifyApiNetHTTP.New;
  {$IFEND}
end;

class function TNotifyApiFactory.New: INotifyApiFactory;
begin
  Result := Self.Create;
end;

end.
