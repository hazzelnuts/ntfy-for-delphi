unit Notify.Provider.Indy;

interface

uses
  System.Classes, IdBaseComponent, IdComponent, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdTCPConnection,
  IdTCPClient, IdHTTP, IdStream, IdGlobal,
  Notify.Provider.Contract, Notify.Config.Contract;

type
  TNotityProviderIndy = class(TInterfacedObject, INotifyProvider)
  strict private
    FIOHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdHTTP: TIdHTTP;
    FIdEventStream: TIdEventStream;
    FBodyStream: TMemoryStream;
    FNotifyConfig: INotifyConfig;
    procedure OnWriteEvent(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyProvider;
  private
    function Config(const AValue: INotifyConfig): INotifyProvider;
    function AddHeader(const AName: String; AValue: String): INotifyProvider; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyProvider; overload;
    function AddBody(const AValue: String): INotifyProvider; overload;
    function AddBody(const AValue: TFileStream): INotifyProvider; overload;
    function AddURLSegment(const AValue: String): INotifyProvider; overload;
    function Get: INotifyProvider;
    function Post: INotifyProvider;
    function Put: INotifyProvider;
  end;

implementation

uses
  Notify.SmartPointer,
  System.SysUtils;

{ TNotityProviderIndy }

function TNotityProviderIndy.AddBody(const AValue: String): INotifyProvider;
var
  LBodyStream: TSmartPointer<TStringStream>;
begin
  Result := Self;
  LBodyStream := TStringStream.Create(AValue);
  FBodyStream.CopyFrom(LBodyStream.Value, LBodyStream.Value.Size);
end;

function TNotityProviderIndy.AddBody(const AValue: TFileStream): INotifyProvider;
begin
  Result := Self;
  FBodyStream.CopyFrom(AValue, AValue.Size);
end;

function TNotityProviderIndy.AddHeader(const AName: String; AValues: array of String): INotifyProvider;
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

function TNotityProviderIndy.AddHeader(const AName: String; AValue: String): INotifyProvider;
begin
  Result := Self;
  FIdHTTP.Request.CustomHeaders.AddValue(AName, AValue);
end;

function TNotityProviderIndy.AddURLSegment(const AValue: String): INotifyProvider;
begin
  Result := Self;
  FNotifyConfig.BaseURL(Format('%s/%s', [FNotifyConfig.BaseURL, AValue]));
end;

function TNotityProviderIndy.Config(const AValue: INotifyConfig): INotifyProvider;
begin
  Result := Self;
  FNotifyConfig := AValue;
end;

constructor TNotityProviderIndy.Create;
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

destructor TNotityProviderIndy.Destroy;
begin
  FIdHTTP.Disconnect;
  FIdHTTP.Free;
  FIOHandlerSSL.Free;
  FIdEventStream.Free;
  FBodyStream.Free;
  inherited;
end;

function TNotityProviderIndy.Get: INotifyProvider;
begin
  Result := Self;
  FIdHTTP.Get('');
end;

class function TNotityProviderIndy.New: INotifyProvider;
begin
  Result := Self.Create;
end;

procedure TNotityProviderIndy.OnWriteEvent(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint);
begin
  //Will be used when subscribing....
end;

function TNotityProviderIndy.Post: INotifyProvider;
begin
  Result := Self;
  FIdHTTP.Post(FNotifyConfig.BaseURL, FBodyStream);
end;

function TNotityProviderIndy.Put: INotifyProvider;
begin
  Result := Self;
  FIdHTTP.Put(FNotifyConfig.BaseURL, FBodyStream);
end;

end.
