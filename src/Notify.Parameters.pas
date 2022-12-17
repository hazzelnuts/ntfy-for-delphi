unit Notify.Parameters;

interface

uses
  Notify.Parameters.Contract;

type
  TNotifyParameters = class(TInterfacedObject, INotifyParameters)
  strict private
    FPoll: Boolean;
    FSince: String;
    FScheduled: Boolean;
  public
    class function New: INotifyParameters;
    function Poll: Boolean; overload;
    function Poll(const AValue: Boolean): INotifyParameters; overload;
    function Since: String; overload;
    function Since(const AValue: String): INotifyParameters; overload;
    function Scheduled: Boolean; overload;
    function Scheduled(const AValue: Boolean): INotifyParameters; overload;
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

function TNotifyParameters.Scheduled(const AValue: Boolean): INotifyParameters;
begin
  Result := Self;
  FScheduled := AValue;
end;

function TNotifyParameters.Scheduled: Boolean;
begin
  Result := FScheduled;
end;

function TNotifyParameters.Since(const AValue: String): INotifyParameters;
begin
  Result := Self;
  FSince := AValue;
end;

function TNotifyParameters.Since: String;
begin
  Result := FSince;
end;

function TNotifyParameters.Poll: Boolean;
begin
  Result := FPoll;
end;

end.
