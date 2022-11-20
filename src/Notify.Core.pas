unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Core.Contract,
  Notify.Provider.Contract,
  Notify.Notification.Contract;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FProvider: INotifyProvider;
    FPublisher: INotifyNotification;
  public
    constructor Create;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Poll: INotifyCore;
    function Scheduled: INotifyCore;
    function Since(const PValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const PValue: Integer): INotifyCore; overload;
    function Listen: INotifyCore;
    function Publish: INotifyCore;
    function Notification(const PPublisher: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  Notify.Notification.Factory,
  Notify.Provider.Factory;

{ TNotifyCore }

constructor TNotifyCore.Create;
begin
  FProvider := TNotifyProviderFactory.New.Provider;
  FPublisher := TNotifyNotificationFactory.New.Notification;
end;

function TNotifyCore.Listen: INotifyCore;
begin

end;

class function TNotifyCore.New: INotifyCore;
begin
  Result := Self.Create;
end;

class function TNotifyCore.NewInstance: TObject;
begin
  if not (Assigned(NotifyCore)) then
    NotifyCore := TNotifyCore(inherited NewInstance);
  Result := NotifyCore;
end;

function TNotifyCore.Notification(const PPublisher: INotifyNotification): INotifyCore;
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
