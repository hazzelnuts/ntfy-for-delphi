unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Core.Contract,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.Notification.Contract,
  Notify.Subscription.Thread;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FApi: INotifyApi;
    FNotification: INotifyNotification;
    FConfig: INotifyConfig;
    FSubcriptionThread: TNotifySubcriptionThread;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Publish: INotifyCore;
    function Subscribe: INotifyCore;
    function Unsubscribe: INotifyCore;
  private
    procedure OpenConnection;
    function SendFile: INotifyCore;
    function SaveLog(const AValue: Boolean): INotifyCore;
    function LogPath(const AValue: String): INotifyCore;
    function Cache(const AValue: Boolean): INotifyCore;
    function UserName(const AValue: String): INotifyCore;
    function Password(const AValue: String): INotifyCore;
    function BaseURL(const AValue: String): INotifyCore;
    function Topic(const AValue: String): INotifyCore;
    function DisableFireBase(const AValue: Boolean): INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  System.NetEncoding,
  System.SysUtils,
  Notify.Facade,
  Notify.SmartPointer,
  System.Classes;

{ TNotifyCore }

function TNotifyCore.BaseURL(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.BaseURL(AValue);
end;

function TNotifyCore.Cache(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.Cache(AValue);
end;

constructor TNotifyCore.Create;
begin
  FApi := TNotifyCoreFacade.New.Api;
  FNotification := TNotifyCoreFacade.New.Notification;
  FConfig := TNotifyCoreFacade.New.Config;
end;

destructor TNotifyCore.Destroy;
begin
  if Assigned(FSubcriptionThread) and (not FSubcriptionThread.Finished) then
    FSubcriptionThread.Terminate;
  inherited;
end;

function TNotifyCore.DisableFireBase(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.DisableFireBase(AValue);
end;

function TNotifyCore.LogPath(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.LogPath(AValue);
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

procedure TNotifyCore.OpenConnection;
begin
  FApi
    .Config(FConfig)
    .AddEndPoint(FNotification.Topic + '/json')
    .Get;
end;

function TNotifyCore.Password(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.Password(AValue);
end;

function TNotifyCore.Publish: INotifyCore;
var
  LUserNamePassword: String;
  LBasicAuth: String;
begin
  Result := Self;

  FApi.Config(FConfig).ClearHeaders.ClearBody;

  if (FConfig.Cache = False) then
    FApi.AddHeader('Cache', 'no');

  if (FConfig.DisableFireBase) then
    FApi.AddHeader('Firebase', 'no');

  if (FConfig.Password <> '') and (FConfig.UserName <> '') then
  begin
    LUserNamePassword := Format('%s:%s', [FConfig.UserName, FConfig.Password]);
    LUserNamePassword := TNetEncoding.Base64.Encode(LUserNamePassword);
    LBasicAuth := Format('Basic %s', [LUserNamePassword]);
    FApi.AddHeader('Authorization', LBasicAuth);
  end;

  if FNotification.FileName <> '' then
  begin
    SendFile;
    Exit;
  end;

  if FNotification.Icon <> '' then
    FApi.AddHeader('Icon', FNotification.Icon);

  FApi
    .AddBody(FNotification.AsJSONString)
    .Post;

end;

function TNotifyCore.SaveLog(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.SaveLog(AValue);
end;

function TNotifyCore.SendFile: INotifyCore;
var
  LFileStream: TSmartPointer<TFileStream>;
begin
  Result := Self;

  LFileStream := TFileStream.Create(FNotification.FilePath, fmOpenRead);

  FApi
    .AddBody(LFileStream.Value)
    .AddHeader('Filename', FNotification.FileName)
    .AddHeader('Title', FNotification.Title)
    .AddHeader('Message', FNotification.MessageContent)
    .AddHeader('Priority', IntToStr(Ord(FNotification.Priority)))
    .AddHeader('Tags', FNotification.Tags)
    .AddHeader('Icon', FNotification.Icon)
    .AddHeader('Email', FNotification.Email)
    .AddHeader('Delay', FNotification.Delay)
    .AddHeader('Topic', FNotification.Topic)
    .AddEndPoint(FNotification.Topic)
    .Put;

end;

function TNotifyCore.Subscribe: INotifyCore;
begin
  Result := Self;

  {$IFDEF CONSOLE}
    OpenConnection;
    Exit;
  {$ENDIF}

  if Assigned(FSubcriptionThread) then
    Exit;

  FSubcriptionThread := TNotifySubcriptionThread.Create(OpenConnection);
  FSubcriptionThread.FreeOnTerminate := True;
  FSubcriptionThread.Start;

end;

function TNotifyCore.Topic(const AValue: String): INotifyCore;
begin
  Result := Self;
  FNotification.Topic(AValue);
end;

function TNotifyCore.Unsubscribe: INotifyCore;
begin
  Result := Self;

  if Assigned(FSubcriptionThread) then
  begin
    FSubcriptionThread.Destroy;
  end;

end;

function TNotifyCore.UserName(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.UserName(AValue);
end;

end.
