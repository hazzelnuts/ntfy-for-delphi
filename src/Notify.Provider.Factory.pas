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
  Notify.Config,
  Notify.Types,
  Notify.Provider.Indy;

{ TNotifyProviderFactory }

class function TNotifyProviderFactory.New: INotifyProviderFactory;
begin
  Result := Self.Create;
end;

function TNotifyProviderFactory.Provider: INotifyProvider;
begin
  {$IFDEF INDY}
  Result := TNotityProviderIndy.New;
  {$ELSE }
  Result := TNotityProviderIndy.New;
  {$IFEND}
end;

end.
