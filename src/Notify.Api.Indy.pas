unit Notify.Api.Indy;

interface

uses
  System.Classes,
  System.Threading,
  IdBaseComponent,
  IdComponent,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  IdStream,
  IdGlobal,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.Subscription.Event,
  NX.Horizon,
  Notify.Api.Response;

type

  TSSEThread = class(TThread)
  private
    FUrl: String;
    FIdHttp: TIdHTTP;
    FIdEventStream: TMemoryStream;
    FResponse: TNotifyApiResponse;
    const FCloseConnectionMessage = '/\/\';
    procedure DoOnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    constructor Create(AUrl: String; var AIdHttp: TIdHTTP);
    procedure Execute; override;
    destructor Destroy; override;
    procedure AbortStream;
    property Response: TNotifyApiResponse read FResponse write FResponse;
  end;

  TNotifyApiIndy = class(TInterfacedObject, INotifyApi)
  strict private
    FIOHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdHTTP: TIdHTTP;
    FBodyStream: TMemoryStream;
    FNotifyConfig: INotifyConfig;
    FEndPoint: String;
    FConnectionThread: TSSEThread;
    FURLParametersList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyApi;
  private
    FResponse: TNotifyApiResponse;
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
    function AddURLParameter(const AName: String; AValue: String): INotifyApi;
    procedure Destroythread;
    function CreateURL: String;
    function ClearURLParameters: INotifyApi;
    function Response: TNotifyApiResponse;
  end;

implementation

uses
  IdURI,
  Notify.SmartPointer,
  System.SysUtils,
  Notify.Logs,
  Notify.Notification.DTO;

{ TNotityApiIndy }

function TNotifyApiIndy.AddBody(const AValue: String): INotifyApi;
var
  LBodyStream: TSmartPointer<TStringStream>;
begin
  Result := Self;
  LBodyStream := TStringStream.Create(AValue);
  FBodyStream.CopyFrom(LBodyStream.Value, LBodyStream.Value.Size);
end;

function TNotifyApiIndy.AbortStream: INotifyApi;
begin
  Destroythread;
end;

function TNotifyApiIndy.AddBody(const AValue: TFileStream): INotifyApi;
begin
  Result := Self;
  FBodyStream.CopyFrom(AValue, AValue.Size);
end;

function TNotifyApiIndy.AddEndPoint(const AValue: String): INotifyApi;
begin
  Result := Self;
  FEndPoint := AValue;
end;

function TNotifyApiIndy.AddHeader(const AName: String; AValues: array of String): INotifyApi;
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

function TNotifyApiIndy.AddURLParameter(const AName: String; AValue: String): INotifyApi;
var
  LParam: String;
begin
  Result := Self;
  LParam := Format('%s=%s', [AName, AValue]);
  FURLParametersList.Add(LParam);
end;

function TNotifyApiIndy.AddHeader(const AName: String; AValue: String): INotifyApi;
begin
  Result := Self;
  FIdHTTP.Request.CustomHeaders.AddValue(AName, AValue);
end;

function TNotifyApiIndy.ClearBody: INotifyApi;
begin
  Result := Self;
  FBodyStream.Clear;
end;

function TNotifyApiIndy.ClearHeaders: INotifyApi;
begin
  Result := Self;
  FIdHTTP.Request.CustomHeaders.Clear;
end;

function TNotifyApiIndy.ClearURLParameters: INotifyApi;
begin
  Result := Self;
  FURLParametersList.Clear;
end;

function TNotifyApiIndy.Config(const AValue: INotifyConfig): INotifyApi;
begin
  Result := Self;
  FNotifyConfig := AValue;
end;

constructor TNotifyApiIndy.Create;
begin
  FIdHTTP := TIdHTTP.Create(nil);
  FIOHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FBodyStream := TMemoryStream.Create;
  FIOHandlerSSL.SSLOptions.Method := sslvTLSv1_2;
  FIdHTTP.IOHandler := FIOHandlerSSL;
  FIdHTTP.HTTPOptions := [hoNoReadMultipartMIME];
  FURLParametersList := TStringList.Create;
  FResponse.Notification := TNotifyNotificationDTO.Create;
end;

function TNotifyApiIndy.CreateURL: String;
var
  I: Integer;
begin
  Result := Format('%s/%s', [FNotifyConfig.BaseURL, FEndPoint]);
  if FURLParametersList.Count > 0 then
  begin
    Result := Result + '?';
    for I := 0 to Pred(FURLParametersList.Count) do
    begin
      if I > 0 then
        Result := Result + '&';
      Result := Result + FURLParametersList.Strings[I];
    end;
  end;
end;

destructor TNotifyApiIndy.Destroy;
begin
  try
    Destroythread;
  finally
    FResponse.Notification.Free;
    FreeAndNil(FIdHTTP);
    FreeAndNil(FIOHandlerSSL);
    FreeAndNil(FBodyStream);
    FreeAndNil(FURLParametersList);
  end;
  inherited;
end;

procedure TNotifyApiIndy.Destroythread;
begin
  if Assigned(FConnectionThread) then
  begin
    try
      if not FConnectionThread.Terminated then
      begin
        FConnectionThread.AbortStream;
        FConnectionThread.Terminate;
        FConnectionThread.WaitFor;
      end;
    finally
      FConnectionThread.Free;
      FConnectionThread := nil;
    end;
  end;
end;

function TNotifyApiIndy.Get: INotifyApi;
begin
  Result := Self;
  try
    if Assigned(FConnectionThread) then
      Destroythread;
  finally
    FConnectionThread := TSSEThread.Create(TIdURI.URLEncode(CreateURL), FIdHTTP);
    FResponse := FConnectionThread.Response;
    FConnectionThread.Start;
  end;

  {$IFDEF CONSOLE}
  FConnectionThread.WaitFor;
  {$ENDIF}

end;

class function TNotifyApiIndy.New: INotifyApi;
begin
  Result := Self.Create;
end;

function TNotifyApiIndy.Post: INotifyApi;
begin
  Result := Self;
  FResponse.Content := FIdHTTP.Post(TIdURI.URLEncode(CreateURL), FBodyStream);
  FResponse.Notification.AsJson := FResponse.Content;
  FResponse.StatusCode := FIdHttp.ResponseCode;
  if FNotifyConfig.SaveLog then
    TNotifyLogs.Log(FNotifyConfig.LogPath, FIdHTTP.ResponseText);
end;

function TNotifyApiIndy.Put: INotifyApi;
begin
  Result := Self;
  FResponse.Content := FIdHTTP.Put(TIdURI.URLEncode(CreateURL), FBodyStream);
  FResponse.Notification.AsJson := FResponse.Content;
  FResponse.StatusCode := FIdHttp.ResponseCode;
  if FNotifyConfig.SaveLog then
    TNotifyLogs.Log(FNotifyConfig.LogPath, FIdHTTP.ResponseText);
end;

function TNotifyApiIndy.Response: TNotifyApiResponse;
begin
  Result := FResponse;
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
  LEventString: UTF8String;
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
  FResponse.Content :=  UnicodeString(LEventString);
  FResponse.StatusCode := FIdHttp.ResponseCode;

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
        Terminate;
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
