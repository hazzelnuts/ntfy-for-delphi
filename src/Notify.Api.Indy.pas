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
    constructor Create(AUrl: String; var AIdHttp: TIdHTTP; var AResponse: TNotifyApiResponse);
    procedure Execute; override;
    destructor Destroy; override;
    procedure AbortStream;
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
    function ClearURLParameters: INotifyApi;
    function ClearEndPoint: INotifyApi;
    function AddHeader(const AName: String; AValue: String): INotifyApi; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyApi; overload;
    function AddBody(const AValue: String): INotifyApi; overload;
    function AddBody(const AValue: TFileStream): INotifyApi; overload;
    function AddEndPoint(const AValue: String): INotifyApi; overload;
    function AddURLParameter(const AName: String; AValue: String): INotifyApi;
    function Get: INotifyApi;
    function Post: INotifyApi;
    function Put: INotifyApi;
    function AbortStream: INotifyApi;
    procedure Destroythread;
    function CreateURL: String;
    function Response: TNotifyApiResponse;
    procedure LogRequest;
  end;

implementation

uses
  IdURI,
  Notify.SmartPointer,
  System.SysUtils,
  System.StrUtils,
  System.Types,
  Notify.Logs,
  Notify.Notification.DTO,
  Notify.Error,
  Notify.Response.Data;

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

function TNotifyApiIndy.ClearEndPoint: INotifyApi;
begin
  Result := Self;
  FEndPoint := '';
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

  if AValue.ProxyServer <> '' then
  begin
    FIdHttp.ProxyParams.ProxyServer := AValue.ProxyServer;
    FIdHttp.ProxyParams.ProxyUsername := AValue.ProxyUser;
    FIdHttp.ProxyParams.ProxyPassword := AValue.ProxyPassword;
    FIdHttp.ProxyParams.ProxyPort := AValue.ProxyPort;
  end;

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
  FResponse := TNotifyApiResponse.Create;
end;

function TNotifyApiIndy.CreateURL: String;
var
  I: Integer;
begin

  if FEndPoint <> '' then
    Result := Format('%s/%s', [FNotifyConfig.BaseURL, FEndPoint])
  else
    Result := FNotifyConfig.BaseURL;

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
    FreeAndNil(FResponse);
    FreeAndNil(FIdHTTP);
    FreeAndNil(FIOHandlerSSL);
    FreeAndNil(FURLParametersList);
    FreeAndNil(FBodyStream);
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
    FConnectionThread := TSSEThread.Create(TIdURI.URLEncode(CreateURL), FIdHTTP, FResponse);
    FConnectionThread.Start;
  end;

  {$IFDEF CONSOLE OR CONSOLE_TESTRUNNER}
  FConnectionThread.WaitFor;
  {$ENDIF}

end;

procedure TNotifyApiIndy.LogRequest;
begin
  if FNotifyConfig.SaveLog then
  begin
    TNotifyLogs.Log(FNotifyConfig.LogPath, FIdHTTP.URL.GetFullURI());
    TNotifyLogs.Log(FNotifyConfig.LogPath, FBodyStream);
    TNotifyLogs.Log(FNotifyConfig.LogPath, FResponse.StatusCode.ToString);
    TNotifyLogs.Log(FNotifyConfig.LogPath, FResponse.ResponseStream);
  end;
end;

class function TNotifyApiIndy.New: INotifyApi;
begin
  Result := Self.Create;
end;

function TNotifyApiIndy.Post: INotifyApi;
begin
  Result := Self;
  try
    FIdHTTP.Post(TIdURI.URLEncode(CreateURL), FBodyStream, FResponse.ResponseStream);
  finally
    FResponse.StatusCode := FIdHttp.ResponseCode;
    LogRequest;
  end;
end;

function TNotifyApiIndy.Put: INotifyApi;
begin
  Result := Self;
  try
    FIdHTTP.Put(TIdURI.URLEncode(CreateURL), FBodyStream, FResponse.ResponseStream);
  finally
    FResponse.StatusCode := FIdHttp.ResponseCode;
    LogRequest;
  end;
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

constructor TSSEThread.Create(AUrl: String; var AIdHttp: TIdHTTP; var AResponse: TNotifyApiResponse);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FUrl := AUrl;
  FIdHttp := AIdHttp;
  FResponse := AResponse;
end;

destructor TSSEThread.Destroy;
begin

  inherited;
end;

procedure TSSEThread.DoOnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
  LEventString: UTF8String;

  {$IFDEF VER310}
  LStrings: TStringDynArray;
  {$ELSE}
  LStrings: TArray<string>;
  {$ENDIF}

  LString: UTF8String;
begin

  if Terminated then
    Exit;

  FIdEventStream.Position := 0;
  SetString(LEventString, PAnsiChar(FIdEventStream.Memory), FIdEventStream.Size);

  if LEventString = '' then
    Exit;

  if LEventString = FCloseConnectionMessage then
    FIdHttp.Socket.Close;

  LStrings := SplitString(LEventString, #$A);

  for LString in LStrings do
    NxHorizon.Instance.Post<TNotifySubscriptionEvent>(LString);

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
        try
          FIdHttp.Get(FUrl, FIdEventStream);
        finally
          Terminate;
        end;
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
