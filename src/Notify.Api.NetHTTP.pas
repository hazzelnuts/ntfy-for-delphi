unit Notify.Api.NetHTTP;

interface

uses
  System.Classes,
  System.Threading,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.Subscription.Event,
  Notify.Api.Response,
  NX.Horizon,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type

  TSSEThread = class(TThread)
  private
    FUrl: String;
    FNetHTTPResquest: TNetHTTPRequest;
    FEventStream: TMemoryStream;
    FResponse: TNotifyApiResponse;
    procedure NetHTTPRequestReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
  public
    constructor Create(AUrl: String; var ANetHTTPRequest: TNetHTTPRequest; var AResponse: TNotifyApiResponse);
    procedure Execute; override;
    destructor Destroy; override;
    procedure AbortStream;
  end;

  TNotifyApiNetHTTP = class(TInterfacedObject, INotifyApi)
  strict private
    FNetClient: TNetHTTPClient;
    FNetHTTPRequest: TNetHTTPRequest;
    FBodyStream: TStringStream;
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
  Notify.SmartPointer,
  Rest.Types,
  System.SysUtils,
  System.StrUtils,
  System.NetEncoding,
  System.Types,
  Notify.Logs,
  Notify.Notification.DTO,
  Notify.Error,
  Notify.Response.Data;


{ TNotifyApiNetHTTP }

function TNotifyApiNetHTTP.AbortStream: INotifyApi;
begin
  Destroythread;
end;

function TNotifyApiNetHTTP.AddBody(const AValue: String): INotifyApi;
var
  LBodyStream: TSmartPointer<TStringStream>;
begin
  Result := Self;
  LBodyStream := TStringStream.Create(AValue);
  FBodyStream.CopyFrom(LBodyStream.Value, LBodyStream.Value.Size);
end;

function TNotifyApiNetHTTP.AddBody(const AValue: TFileStream): INotifyApi;
begin
  Result := Self;
  FBodyStream.CopyFrom(AValue, AValue.Size);
end;

function TNotifyApiNetHTTP.AddEndPoint(const AValue: String): INotifyApi;
begin
  Result := Self;
  FEndPoint := AValue
end;

function TNotifyApiNetHTTP.AddHeader(const AName: String; AValues: array of String): INotifyApi;
var
  LString, LValue: String;
begin
  Result := Self;
  for LString in AValues do
    if LValue = '' then
      LValue := LString
    else
      LValue := Format('%s, %s', [LValue, LString]);
  FNetClient.CustomHeaders[AName] :=  LValue;
end;

function TNotifyApiNetHTTP.AddHeader(const AName: String; AValue: String): INotifyApi;
begin
  Result := Self;
  FNetClient.CustomHeaders[AName] := AValue;
end;

function TNotifyApiNetHTTP.AddURLParameter(const AName: String; AValue: String): INotifyApi;
var
  LParam: String;
begin
  Result := Self;
  LParam := Format('%s=%s', [AName, AValue]);
  FURLParametersList.Add(LParam);
end;

function TNotifyApiNetHTTP.ClearBody: INotifyApi;
begin
  Result := Self;
  FBodyStream.Clear;
end;

function TNotifyApiNetHTTP.ClearEndPoint: INotifyApi;
begin
  Result := Self;
  FEndPoint := '';
end;

function TNotifyApiNetHTTP.ClearHeaders: INotifyApi;
begin
  Result := Self;
  FNetHTTPRequest.CustHeaders.Clear;
end;

function TNotifyApiNetHTTP.ClearURLParameters: INotifyApi;
begin
  Result := Self;
  FURLParametersList.Clear;
end;

function TNotifyApiNetHTTP.Config(const AValue: INotifyConfig): INotifyApi;
begin
  Result := Self;
  FNotifyConfig := AValue;
end;

constructor TNotifyApiNetHTTP.Create;
begin
  FNetClient := TNetHTTPClient.Create(nil);
  FNetHTTPRequest := TNetHTTPRequest.Create(nil);
  FNetHTTPRequest.Client := FNetClient;
  FNetClient.ContentType := CONTENTTYPE_APPLICATION_JSON;
  FBodyStream := TStringStream.Create;
  FURLParametersList := TStringList.Create;
  FResponse := TNotifyApiResponse.Create;
end;

function TNotifyApiNetHTTP.CreateURL: String;
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

destructor TNotifyApiNetHTTP.Destroy;
begin
  try
    Destroythread;
  finally
    FResponse.Free;
    FNetClient.Free;
    FNetHTTPRequest.Free;
    FURLParametersList.Free;
    FBodyStream.Free;
  end;
  inherited;
end;

procedure TNotifyApiNetHTTP.Destroythread;
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

function TNotifyApiNetHTTP.Get: INotifyApi;
begin
  Result := Self;
  try
    if Assigned(FConnectionThread) then
      Destroythread;
  finally
    FConnectionThread := TSSEThread.Create(CreateURL, FNetHTTPRequest, FResponse);
    FConnectionThread.Start;
  end;

  {$IFDEF CONSOLE}
  FConnectionThread.WaitFor;
  {$ENDIF}

end;

procedure TNotifyApiNetHTTP.LogRequest;
begin
  if FNotifyConfig.SaveLog then
  begin
    TNotifyLogs.Log(FNotifyConfig.LogPath, FNetHTTPRequest.URL);
    TNotifyLogs.Log(FNotifyConfig.LogPath, FBodyStream);
    TNotifyLogs.Log(FNotifyConfig.LogPath, FResponse.StatusCode.ToString);
    TNotifyLogs.Log(FNotifyConfig.LogPath, FResponse.ResponseStream);
  end;
end;

class function TNotifyApiNetHTTP.New: INotifyApi;
begin
  Result := Self.Create;
end;

function TNotifyApiNetHTTP.Post: INotifyApi;
var
  LHTTPResponse: IHTTPResponse;
  LBodyStringStream: TStringStream;
begin
  Result := Self;
  try
    LBodyStringStream := TStringStream.Create(UTF8Encode(FBodyStream.DataString));
    try
      LHTTPResponse := FNetHTTPRequest.Post(CreateURL, LBodyStringStream, FResponse.ResponseStream);
    finally
      LBodyStringStream.Free;
    end;
  finally
    FResponse.StatusCode := LHTTPResponse.StatusCode;
    LogRequest;
  end;
end;

function TNotifyApiNetHTTP.Put: INotifyApi;
var
  LHTTPResponse: IHTTPResponse;
  LBodyStringStream: TStringStream;
begin
  Result := Self;
  try
    LBodyStringStream := TStringStream.Create(FBodyStream.DataString);
    try
      LHTTPResponse := FNetHTTPRequest.Put(CreateURL, LBodyStringStream, FResponse.ResponseStream);
    finally
      LBodyStringStream.Free;
    end;
  finally
    FResponse.StatusCode := LHTTPResponse.StatusCode;
    LogRequest;
  end;
end;

function TNotifyApiNetHTTP.Response: TNotifyApiResponse;
begin
  Result := FResponse;
end;

{ TSSEThread }

procedure TSSEThread.AbortStream;
begin
  FNetHTTPResquest.Cancel;
end;

constructor TSSEThread.Create(AUrl: String; var ANetHTTPRequest: TNetHTTPRequest;
  var AResponse: TNotifyApiResponse);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FUrl := AUrl;
  FNetHTTPResquest := ANetHTTPRequest;
  FResponse := AResponse;
end;

destructor TSSEThread.Destroy;
begin
  inherited;
end;

procedure TSSEThread.Execute;
begin
  inherited;
  try
    FEventStream := TMemoryStream.Create;
    FNetHTTPResquest.OnReceiveData := NetHTTPRequestReceiveData;
    FNetHTTPResquest.Accept := 'text/event-stream';
    while not Terminated do
    begin
      try
        try
          FNetHTTPResquest.Get(FUrl, FEventStream);
        finally
          Terminate;
        end;
      except
        on E: Exception do
        begin
          Terminate;
          Exit;
        end;
      end;
    end;

  finally
    FEventStream.Free;
  end;
end;

procedure TSSEThread.NetHTTPRequestReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LEventString: UTF8String;
  LStrings: TArray<String>;
  LString: String;
begin

  if Terminated then
    Exit;

  SetString(LEventString, PAnsiChar(FEventStream.Memory), FEventStream.Size);

  if LEventString = '' then
    Exit;

  LStrings := SplitString(UTF8ToString(LEventString) , #$A);

  for LString in LStrings do
    NxHorizon.Instance.Post<TNotifySubscriptionEvent>(Utf8String(LString));

end;

end.
