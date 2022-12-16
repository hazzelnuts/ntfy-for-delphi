unit Notify.Parameters;

interface

uses
  Notify.Parameters.Contract;

type
  TNotifyParameters = class(TInterfacedObject, INotifyParameters)
  strict private
    FPoll: Boolean;
    FSince: String;
  public
    class function New: INotifyParameters;
    function Poll: Boolean; overload;
    function Poll(const AValue: Boolean): INotifyParameters; overload;
  end;

implementation

{ TNotifyParameters }

class function TNotifyParameters.New: INotifyParameters;
begin
  Result := Self.Create;
end;

function TNotifyParameters.Poll(const AValue: Boolean): INotifyParameters;
begin
  Result := Self;
  FPoll := AValue;
end;

function TNotifyParameters.Poll: Boolean;
begin
  Result := FPoll;
end;

end.
