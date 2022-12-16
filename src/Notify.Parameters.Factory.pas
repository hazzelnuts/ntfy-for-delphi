unit Notify.Parameters.Factory;

interface

uses
  Notify.Parameters.Contract;

type
  TNotifyParametersFactory = class(TInterfacedObject, INotifyParametersFactory)
  public
    class function New: INotifyParametersFactory;
    function Parameters: INotifyParameters;
  end;

implementation

uses
  Notify.Parameters;

{ TNotifyParametersFactory }

class function TNotifyParametersFactory.New: INotifyParametersFactory;
begin
  Result := Self.Create;
end;

function TNotifyParametersFactory.Parameters: INotifyParameters;
begin
  Result := TNotifyParameters.New;
end;

end.
