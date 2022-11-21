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
  public
    constructor Create;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Poll: INotifyCore;
    function Delay(const AValue: String): INotifyCore;
    function Since(const AValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const AValue: Integer): INotifyCore; overload;
    function Listen: INotifyCore;
    function Publish: INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  System.SysUtils,
  Notify.Provider.Factory;

{ TNotifyCore }

constructor TNotifyCore.Create;
begin
  FProvider := TNotifyProviderFactory.New.Provider;
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
  FProvider.Notification(ANotification);
end;

function TNotifyCore.Poll: INotifyCore;
begin

end;

function TNotifyCore.Publish: INotifyCore;
begin
  Result := Self;
  FProvider.Post;
end;

function TNotifyCore.Delay(const AValue: String): INotifyCore;
begin
  Result := Self;
  FProvider.AddHeader('Delay', AValue);
end;

function TNotifyCore.Since(const AValue: Integer): INotifyCore;
begin

end;

function TNotifyCore.Since(const AValue: String;
  const PFeytchType: TNotifyFectchType): INotifyCore;
begin

end;

end.
