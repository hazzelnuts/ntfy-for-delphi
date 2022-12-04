unit Notify.Config.Factory;

interface

uses
  Notify.Config.Contract;

type
  TNotifyConfigFactory = class sealed(TInterfacedObject, INotifyConfigFactory)
  public
    class function New: INotifyConfigFactory;
    function Config: INotifyConfig;
  end;

implementation

uses
  Notify.Config;

{ TNotifyConfigFactory }

function TNotifyConfigFactory.Config: INotifyConfig;
begin
  Result := TNotifyConfig.New;
end;

class function TNotifyConfigFactory.New: INotifyConfigFactory;
begin
  Result := Self.Create;
end;

end.
