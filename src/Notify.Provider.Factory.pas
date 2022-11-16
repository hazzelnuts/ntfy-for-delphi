unit Notify.Provider.Factory;

interface

uses
  Notify.Provider.Contract;

type
  TNotifyProviderFactory = class(TInterfacedObject, INotifyProviderFactory)
  public
    class function New: INotifyProviderFactory;
    function Provider: INotifyProvider; overload;
  end;

implementation

uses
  Notify.Types,
  Notify.Provider.Indy;

{ TNotifyProviderFactory }

class function TNotifyProviderFactory.New: INotifyProviderFactory;
begin
  Result := Self.Create;
end;

function TNotifyProviderFactory.Provider: INotifyProvider;
begin
  {$IF INDY}
  Result := TNotityProviderIndy.New;
  {$ELSEIF NETHTTP}
  Result := TNotityProviderIndy.New; //have to implement NetHttp
  {$IFEND}
end;

end.
