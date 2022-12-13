unit Notify.Event.Factory;

interface

uses
  Notify.Event.Contract;

type
  TNotifyMessageFactory = class(TInterfacedObject, INotifyEventFactory)
  public
    class function New: INotifyEventFactory;
    function Event: INotifyEvent;
  end;

implementation

uses
  Notify.Event;


{ TNotifyMessageFactory }

class function TNotifyMessageFactory.New: INotifyEventFactory;
begin
  Result := Self.Create;
end;

function TNotifyMessageFactory.Event: INotifyEvent;
begin
  Result := TNotifyMessage.New;
end;

end.
