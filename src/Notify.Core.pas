unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Contract,
  Notify.Provider.Contract,
  Notify.Publisher.Contract;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FProvider: INotifyProvider;
    FPublisher: INotifyPublisher;
  public
    constructor Create;
    class function New: INotifyCore;
    function Poll: INotifyCore;
    function Scheduled: INotifyCore;
    function Since(const PValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const PValue: Integer): INotifyCore; overload;
    function Listen: INotifyCore;
    function Publish: INotifyCore;
    function Notification(const PPublisher: INotifyPublisher): INotifyCore;
  end;

implementation

uses
  Notify.Publisher.Factory,
  Notify.Provider.Factory;

{ TNotifyCore }

constructor TNotifyCore.Create;
begin
  FProvider := TNotifyProviderFactory.New.Provider;
  FPublisher := TNotifyPublisherFactory.New.Publisher;
end;

function TNotifyCore.Listen: INotifyCore;
begin

end;

class function TNotifyCore.New: INotifyCore;
begin
  Result := Self.Create;
end;

function TNotifyCore.Notification(const PPublisher: INotifyPublisher): INotifyCore;
begin
  Result := Self;
  FPublisher := PPublisher;
end;

function TNotifyCore.Poll: INotifyCore;
begin

end;

function TNotifyCore.Publish: INotifyCore;
begin
  Result := Self;
  FProvider.Publisher(FPublisher).Post;
end;

function TNotifyCore.Scheduled: INotifyCore;
begin

end;

function TNotifyCore.Since(const PValue: Integer): INotifyCore;
begin

end;

function TNotifyCore.Since(const PValue: String;
  const PFeytchType: TNotifyFectchType): INotifyCore;
begin

end;

end.
