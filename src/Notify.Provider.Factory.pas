unit Notify.Provider.Factory;

interface

uses
  Notify.Provider.Contract;

type
  TNotifyProviderFactory = class(TInterfacedObject, INotifyProviderFactory)
  strict private
    const BaseURL = 'https://ntfy.sh/';
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
  {$IFDEF INDY}
  Result := TNotityProviderIndy.New(BaseURL);
  {$ELSE }
  Result := TNotityProviderIndy.New(BaseURL);
  {$IFEND}
end;

end.
