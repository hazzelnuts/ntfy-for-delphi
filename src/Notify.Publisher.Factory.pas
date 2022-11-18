unit Notify.Publisher.Factory;

interface

uses
  Notify.Publisher.Contract;

type
  TNotifyPublisherFactory = class sealed(TInterfacedObject, INotifyPublisherFactory)
  public
    class function New: INotifyPublisherFactory;
    function Publisher: INotifyPublisher;
  end;

implementation

uses
  Notify.Publisher;

{ TNotifyPublisherFactory }

class function TNotifyPublisherFactory.New: INotifyPublisherFactory;
begin
  Result := Self.Create;
end;

function TNotifyPublisherFactory.Publisher: INotifyPublisher;
begin
  Result := TNotifyPublisher.New;
end;

end.
