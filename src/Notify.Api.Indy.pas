unit Notify.Api.Indy;

interface

uses
  System.Classes, IdBaseComponent, IdComponent, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdTCPConnection,
  IdTCPClient, IdHTTP, IdStream, IdGlobal,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.SimpleWebsocket.Indy,
  NX.Horizon,
  Notify.Subscription.Event,
  System.Threading;

type

  TSSEThread = class(TThread)
  private
    FUrl: String;
    FIdHttp: TIdHTTP;
    FIdEventStream: TMemoryStream;
    const FCloseConnectionMessage = '/\/\';
    procedure DoOnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    constructor Create(AUrl: String; var AIdHttp: TIdHTTP);
    procedure Execute; override;
    destructor Destroy; override;
    procedure AbortStream;
  end;

  TNotityApiIndy = class(TInterfacedObject, INotifyApi)
  strict private
    FIOHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdHTTP: TIdHTTP;
    FBodyStream: TMemoryStream;
    FNotifyConfig: INotifyConfig;
    FEndPoint: String;
    FConnectionThread: TSSEThread;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyApi;
  private
    function Config(const AValue: INotifyConfig): INotifyApi;
    function ClearHeaders: INotifyApi;
    function ClearBody: INotifyApi;
    function AddHeader(const AName: String; AValue: String): INotifyApi; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyApi; overload;
    function AddBody(const AValue: String): INotifyApi; overload;
    function AddBody(const AValue: TFileStream): INotifyApi; overload;
    function AddEndPoint(const AValue: String): INotifyApi; overload;
    function Get: INotifyApi;
    function Post: INotifyApi;
    function Put: INotifyApi;
    function AbortStream: INotifyApi;
    procedure Destroythread;
  end;

implementation

uses
  Notify.SmartPointer,
  System.SysUtils,
  Notify.Logs, Winapi.Windows;

{ TNotityApiIndy }

function TNotityApiIndy.AddBody(const AValue: String): INotifyApi;
var
  LBodyStream: TSmartPointer<TStringStream>;
begin
  Result := Self;
  LBodyStream := TStringStream.Create(AValue);
  FBodyStream.CopyFrom(LBodyStream.Value, LBodyStream.Value.Size);
end;

function TNotityApiIndy.AbortStream: INotifyApi;
begin
  Destroythread;
end;

function TNotityApiIndy.AddBody(const AValue: TFileStream): INotifyApi;
begin
  Result := Self;
  FBodyStream.CopyFrom(AValue, AValue.Size);
end;

function TNotityApiIndy.AddEndPoint(const AValue: String): INotifyApi;
begin
  Result := Self;
  FEndPoint := AValue;
end;

function TNotityApiIndy.AddHeader(const AName: String; AValues: array of String): INotifyApi;
var
  LString, LValue: String;
begin
  Result := Self;
  for LString in AValues do
    if LValue = '' then
      LValue := LString
    else
      LValue := Format('%s, %s', [LValue, LString]);
  FIdHTTP.Request.CustomHeaders.AddValue(AName, LValue);
end;

function TNotityApiIndy.AddHeader(const AName: String; AValue: String): INotifyApi;
begin
  Result := Self;
  FIdHTTP.Request.CustomHeaders.AddValue(AName, AValue);
end;

function TNotityApiIndy.ClearBody: INotifyApi;
begin
  Result := Self;
  FBodyStream.Clear;
end;

function TNotityApiIndy.ClearHeaders: INotifyApi;
begin
  Result := Self;
  FIdHTTP.Request.CustomHeaders.Clear;
end;

function TNotityApiIndy.Config(const AValue: INotifyConfig): INotifyApi;
begin
  Result := Self;
  FNotifyConfig := AValue;
end;

constructor TNotityApiIndy.Create;
begin
  FIdHTTP := TIdHTTP.Create(nil);
  FIOHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FBodyStream := TMemoryStream.Create;
  FIOHandlerSSL.SSLOptions.Method := sslvTLSv1_2;
  FIdHTTP.IOHandler := FIOHandlerSSL;
  FIdHTTP.HTTPOptions := [hoNoReadMultipartMIME];
end;

destructor TNotityApiIndy.Destroy;
begin
  try
    Destroythread;
  finally
    FIdHTTP.Free;
    FIOHandlerSSL.Free;
    FBodyStream.Free;
  end;
  inherited;
end;

procedure TNotityApiIndy.Destroythread;
begin
  if Assigned(FConnectionThread) then
  begin
    try
      FConnectionThread.AbortStream;
      FConnectionThread.Terminate;
      FConnectionThread.WaitFor;
    finally
      FConnectionThread.Free;
      FConnectionThread := nil;
    end;
  end;
end;

function TNotityApiIndy.Get: INotifyApi;
var
  LUrl: String;
begin
  Result := Self;
  LUrl := Format('%s/%s', [FNotifyConfig.BaseURL, FEndPoint]);

  try
    if Assigned(FConnectionThread) then
      Destroythread;
  finally
    FConnectionThread := TSSEThread.Create(LUrl, FIdHTTP);
    FConnectionThread.Start;
  end;

end;

class function TNotityApiIndy.New: INotifyApi;
begin
  Result := Self.Create;
end;

function TNotityApiIndy.Post: INotifyApi;
begin
  Result := Self;
  FIdHTTP.Post(FNotifyConfig.BaseURL, FBodyStream);
  if FNotifyConfig.SaveLog then
    TNotifyLogs.Log(FNotifyConfig.LogPath, FIdHTTP.ResponseText);
end;

function TNotityApiIndy.Put: INotifyApi;
var
  LUrl: String;
begin
  Result := Self;
  LUrl := Format('%s/%s', [FNotifyConfig.BaseURL, FEndPoint]);
  FIdHTTP.Put(LUrl, FBodyStream);
  if FNotifyConfig.SaveLog then
    TNotifyLogs.Log(FNotifyConfig.LogPath, FIdHTTP.ResponseText);
end;

{ TSSEThread }

procedure TSSEThread.AbortStream;
var
  Cmd: String;
begin
  try
    Cmd := FCloseConnectionMessage;
    FIdEventStream.WriteData(ToBytes(Cmd), Length(Cmd));
    FIdEventStream.Position := 0;
    DoOnWork(Self, wmRead, 1000);
  except on E: Exception do
    begin
      Exit;
    end;
  end;
end;

constructor TSSEThread.Create(AUrl: String; var AIdHttp: TIdHTTP);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FUrl := AUrl;
  FIdHttp := AIdHttp;
end;

destructor TSSEThread.Destroy;
begin

  inherited;
end;

procedure TSSEThread.DoOnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
  LEventString: String;
begin

  if Terminated then
    Exit;

  FIdEventStream.Position := 0;
  SetString(LEventString, PAnsiChar(FIdEventStream.Memory), FIdEventStream.Size);

  if LEventString = '' then
    Exit;

  if LEventString = FCloseConnectionMessage then
    FIdHttp.Socket.Close;

  NxHorizon.Instance.Post<TNotifySubscriptionEvent>(LEventString);
end;

procedure TSSEThread.Execute;
begin
  inherited;
  try
    FIdEventStream := TMemoryStream.Create;
    FIdHttp.OnWork := DoOnWork;
    FIdHttp.Request.CacheControl := 'no-cache';
    FIdHttp.Request.Accept := 'text/event-stream';
    FIdHttp.Response.KeepAlive := True;
    while not Terminated do
    begin
      try
        FIdHttp.Get(FUrl, FIdEventStream);
      except on E: Exception do
        begin
          Exit;
        end;
      end;
    end;

  finally
    FIdEventStream.Free;
  end;
end;

end.
