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
    FHeaders: TNetHeaders;
    FConfig: INotifyConfig;
    procedure ReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
  public
    constructor Create(const AUrl: String; const AHeaders: TNetHeaders; var ANetHTTPRequest: TNetHTTPRequest; var AResponse: TNotifyApiResponse; var AConfig: INotifyConfig);
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
    FNetHeaders: TNetHeaders;
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
    function CreateURL: String;
    function Response: TNotifyApiResponse;
    procedure LogRequest;
    function Disconnect: INotifyApi;
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
  FBodyStream.Clear;
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
  LIndex: Integer;
  LHeaderPair: TNameValuePair;
begin
  Result := Self;
  for LString in AValues do
    if LValue = '' then
      LValue := LString
    else
      LValue := Format('%s, %s', [LValue, LString]);

  if LValue <> '' then
  begin
    LIndex := Length(FNetHeaders);
    SetLength(FNetHeaders, LIndex + 1);
    FNetHeaders[LIndex] := LHeaderPair.Create(AName, StringReplace(LValue, sLineBreak, '', [rfReplaceAll]));
  end;
end;

function TNotifyApiNetHTTP.AddHeader(const AName: String; AValue: String): INotifyApi;
var
  LHeaderPair: TNameValuePair;
  LIndex: Integer;
begin
  Result := Self;
  if AValue <> '' then
  begin
    LIndex := Length(FNetHeaders);
    SetLength(FNetHeaders, LIndex + 1);
    FNetHeaders[LIndex] := LHeaderPair.Create(AName, StringReplace(AValue, sLineBreak, '', [rfReplaceAll]));
  end;
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
  SetLength(FNetHeaders, 0);
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
    AbortStream;
  finally
    FResponse.Free;
    FNetClient.Free;
    FNetHTTPRequest.Free;
    FURLParametersList.Free;
    FBodyStream.Free;
  end;
  inherited;
end;

function TNotifyApiNetHTTP.Disconnect: INotifyApi;
begin
  Result := Self;
  {$IF CompilerVersion >= 34.0}
  FNetHTTPRequest.Cancel;
  {$ENDIF}
end;

function TNotifyApiNetHTTP.Get: INotifyApi;
begin
  Result := Self;
  try
    if Assigned(FConnectionThread) then
      AbortStream;
  finally
    FConnectionThread := TSSEThread.Create(CreateURL, FNetHeaders, FNetHTTPRequest, FResponse, FNotifyConfig);
    FConnectionThread.Start;
  end;

  {$IF DEFINED(CONSOLE) OR DEFINED(CONSOLE_TESTRUNNER)}
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
      LHTTPResponse := FNetHTTPRequest.Post(CreateURL, LBodyStringStream, FResponse.ResponseStream, FNetHeaders);
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
      LHTTPResponse := FNetHTTPRequest.Put(CreateURL, LBodyStringStream, FResponse.ResponseStream, FNetHeaders);
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
  {$IF CompilerVersion >= 34.0}
    FNetHTTPResquest.Cancel;
  {$ENDIF}
end;

constructor TSSEThread.Create(const AUrl: String; const AHeaders: TNetHeaders;
  var ANetHTTPRequest: TNetHTTPRequest; var AResponse: TNotifyApiResponse; var AConfig: INotifyConfig);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FUrl := AUrl;
  FNetHTTPResquest := ANetHTTPRequest;
  FResponse := AResponse;
  FHeaders := AHeaders;
  FConfig := AConfig;
end;

destructor TSSEThread.Destroy;
begin
  inherited;
end;

procedure TSSEThread.Execute;
begin
  inherited;
  FEventStream := TMemoryStream.Create;
  try
    FNetHTTPResquest.OnReceiveData := ReceiveData;
    FNetHTTPResquest.Accept := 'text/event-stream';
    while not Terminated do
    begin
      try
        try
          FNetHTTPResquest.Get(FUrl, FEventStream, FHeaders);
          {$IF DEFINED(CONSOLE)}
            if FConfig.SaveLog then
              WriteLn(DateTimeToStr(Now) + ' Exiting Get HTTP Call');
          {$IFEND}

          if not FConfig.Poll then
            TThread.Sleep(10000);

        finally
          if FConfig.Poll then
            Terminate;
        end;
      except
        if FConfig.Poll then
          Exit;
      end;
    end;
  finally
    FEventStream.Free;
  end;
end;

procedure TSSEThread.ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LEventString: UTF8String;
  LStrings: TArray<String>;
  LString: String;
begin

  if Terminated then
    Exit;

  FEventStream.Position := 0;
  SetString(LEventString, PAnsiChar(FEventStream.Memory), FEventStream.Size);

  if LEventString = '' then
    Exit;

  LStrings := SplitString(UTF8ToString(LEventString) , #$A);

  for LString in LStrings do
  {$IF DEFINED(ANDROID)}
    NxHorizon.Instance.Send<TNotifySubscriptionEvent>(Utf8String(LString), Sync);
  {$ELSE}
    NxHorizon.Instance.Post<TNotifySubscriptionEvent>(Utf8String(LString));
  {$ENDIF}

end;

end.
