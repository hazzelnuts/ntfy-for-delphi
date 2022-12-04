unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Core.Contract,
  Notify.Provider.Contract,
  Notify.Config.Contract,
  Notify.Notification.Contract;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FProvider: INotifyProvider;
    FNotification: INotifyNotification;
    FConfig: INotifyConfig;
  public
    constructor Create;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Publish: INotifyCore;
    function SendFile: INotifyCore;
  private
    function Config(const AConfig: INotifyConfig): INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  System.SysUtils,
  Notify.Facade,
  System.Classes;

{ TNotifyCore }

function TNotifyCore.Config(const AConfig: INotifyConfig): INotifyCore;
begin
  Result := Self;
end;

constructor TNotifyCore.Create;
begin
  FProvider := TNotifyCoreFacade.New.Provider;
  FNotification := TNotifyCoreFacade.New.Notification;
  FConfig := TNotifyCoreFacade.New.Config;
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
end;

function TNotifyCore.Publish: INotifyCore;
begin
  Result := Self;

  FProvider.Config(FConfig);

  if FNotification.FileName <> '' then
  begin
    SendFile;
    Exit;
  end;

  if FNotification.Icon <> '' then
    FProvider.AddHeader('Icon', FNotification.Icon);

  FProvider
    .AddBody(FNotification.AsJSONString)
    .Post;

end;

function TNotifyCore.SendFile: INotifyCore;
begin
  Result := Self;
  FProvider
    .AddBody(TFileStream.Create(FNotification.FilePath, fmOpenRead))
    .AddHeader('Filename', FNotification.FileName)
    .AddHeader('Title', FNotification.Title)
    .AddHeader('Message', FNotification.MessageContent)
    .AddHeader('Priority', IntToStr(Ord(FNotification.Priority)))
    .AddHeader('Tags', FNotification.Tags)
    .AddHeader('Icon', FNotification.Icon)
    .AddHeader('Email', FNotification.Email)
    .AddHeader('Delay', FNotification.Delay)
    .AddURLSegment(FNotification.Topic)
    .Put;
end;

end.
