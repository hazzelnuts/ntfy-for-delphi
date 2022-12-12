unit Notify.Event.Factory;

interface

uses
  Notify.Event.Contract;

type
  TNotifyMessageFactory = class(TInterfacedObject, INotifyMessageFactory)
  public
    class function New: INotifyMessageFactory;
    function NotifyMessage: INotifyMessage;
  end;

implementation

uses
  Notify.Event;


{ TNotifyMessageFactory }

class function TNotifyMessageFactory.New: INotifyMessageFactory;
begin
  Result := Self.Create;
end;

function TNotifyMessageFactory.NotifyMessage: INotifyMessage;
begin
  Result := TNotifyMessage.New;
end;

end.
