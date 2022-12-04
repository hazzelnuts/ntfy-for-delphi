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
    FNotification: INotifyNotification;
  public
    constructor Create;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Publish: INotifyCore;
    function Poll: INotifyCore;
    function Listen: INotifyCore;
  private
    function Since(const AValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const AValue: Integer): INotifyCore; overload;
    function Icon(const AValue: String): INotifyCore; overload;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  System.SysUtils,
  Notify.Notification.Factory,
  Notify.Provider.Factory;

{ TNotifyCore }

constructor TNotifyCore.Create;
begin
  FProvider := TNotifyProviderFactory.New.Provider;
  FNotification := TNotifyNotificationFactory.New.Notification;
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

function TNotifyCore.Notification(const ANotification: INotifyNotification): INotifyCore;
begin
  Result := Self;
  FNotification := ANotification;
  FProvider.Notification(FNotification);
end;

function TNotifyCore.Poll: INotifyCore;
begin

end;

function TNotifyCore.Publish: INotifyCore;
begin
  Result := Self;
  FProvider.Post;
end;

function TNotifyCore.Icon(const AValue: String): INotifyCore;
begin
  Result := Self;
  FProvider.AddHeader('Icon', AValue);
end;

function TNotifyCore.Since(const AValue: Integer): INotifyCore;
begin

end;

function TNotifyCore.Since(const AValue: String;
  const PFeytchType: TNotifyFectchType): INotifyCore;
begin

end;

end.
