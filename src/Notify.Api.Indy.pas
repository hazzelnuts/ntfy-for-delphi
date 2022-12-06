unit Notify.Api.Indy;

interface

uses
  System.Classes, IdBaseComponent, IdComponent, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdTCPConnection,
  IdTCPClient, IdHTTP, IdStream, IdGlobal,
  Notify.Api.Contract, Notify.Config.Contract;

type
  TNotityApiIndy = class(TInterfacedObject, INotifyApi)
  strict private
    FIOHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdHTTP: TIdHTTP;
    FIdEventStream: TIdEventStream;
    FBodyStream: TMemoryStream;
    FNotifyConfig: INotifyConfig;
    FEndPoint: String;
    procedure OnWriteEvent(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
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
  end;

implementation

uses
  Notify.SmartPointer,
  System.SysUtils;

{ TNotityApiIndy }

function TNotityApiIndy.AddBody(const AValue: String): INotifyApi;
var
  LBodyStream: TSmartPointer<TStringStream>;
begin
  Result := Self;
  LBodyStream := TStringStream.Create(AValue);
  FBodyStream.CopyFrom(LBodyStream.Value, LBodyStream.Value.Size);
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
  FIdEventStream := TIdEventStream.Create;
  FBodyStream := TMemoryStream.Create;

  FIOHandlerSSL.SSLOptions.Method := sslvTLSv1_2;
  FIdHTTP.IOHandler := FIOHandlerSSL;
  FIdHTTP.Request.Accept := 'text/event-stream';
  FIdHTTP.Request.CacheControl := 'no-store';
  FIdHTTP.HTTPOptions := [hoNoReadMultipartMIME, hoNoReadChunked];
  FIdEventStream.OnWrite := OnWriteEvent;

end;

destructor TNotityApiIndy.Destroy;
begin
  FIdHTTP.Disconnect;
  FIdHTTP.Free;
  FIOHandlerSSL.Free;
  FIdEventStream.Free;
  FBodyStream.Free;
  inherited;
end;

function TNotityApiIndy.Get: INotifyApi;
begin
  Result := Self;
  FIdHTTP.Get('');
end;

class function TNotityApiIndy.New: INotifyApi;
begin
  Result := Self.Create;
end;

procedure TNotityApiIndy.OnWriteEvent(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
begin
  //Will be used when subscribing....
end;

function TNotityApiIndy.Post: INotifyApi;
begin
  Result := Self;
  FIdHTTP.Post(FNotifyConfig.BaseURL, FBodyStream);
end;

function TNotityApiIndy.Put: INotifyApi;
var
  LUrl: String;
begin
  Result := Self;
  LUrl := Format('%s/%s', [FNotifyConfig.BaseURL, FEndPoint]);
  FIdHTTP.Put(LUrl, FBodyStream);
end;

end.
