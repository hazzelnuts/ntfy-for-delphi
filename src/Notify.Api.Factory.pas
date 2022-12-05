unit Notify.Api.Factory;

interface

uses
  Notify.Api.Contract;

type
  TNotifyApiFactory = class(TInterfacedObject, INotifyApiFactory)
  public
    class function New: INotifyApiFactory;
    function Api: INotifyApi; overload;
  end;

implementation

uses
  Notify.Config,
  Notify.Types,
  Notify.Api.Indy;

{ TNotifyApiFactory }

class function TNotifyApiFactory.New: INotifyApiFactory;
begin
  Result := Self.Create;
end;

function TNotifyApiFactory.Api: INotifyApi;
begin
  {$IFDEF INDY}
  Result := TNotityApiIndy.New;
  {$ELSE }
  Result := TNotityApiIndy.New;
  {$IFEND}
end;

end.
