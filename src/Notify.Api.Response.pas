unit Notify.Api.Response;

interface

  ///
  ///  Response is only used for publishing
  ///

uses
  Notify.Error,
  Notify.Response.Data,
  Notify.Notification.DTO,
  System.Classes;

type

  {$M+}

  TNotifyApiResponse = class
  private
  {$IFDEF WIN32}
    FStatusCode: Integer;
  {$ELSE}
    FStatusCode: Int64;
  {$ENDIF}
    FResponseStream: TMemoryStream;
    FResponseData: TNotifyResponseData;
    FResponseErrors: TNotifyErrors;
    function GetErros: TNotifyErrors;
    function GetData: TNotifyResponseData;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Erros: TNotifyErrors read GetErros;
    property Data: TNotifyResponseData read GetData;
    property {$IFDEF WIN32} StatusCode: Integer {$ELSE} StatusCode: Int64 {$ENDIF} read FStatusCode write FStatusCode;
    property ResponseStream: TMemoryStream read FResponseStream write FResponseStream;
  end;

implementation

uses
  System.SysUtils;

{ TNotifyApiResponse }

constructor TNotifyApiResponse.Create;
begin
  FResponseData := TNotifyResponseData.Create;
  FResponseErrors := TNotifyErrors.Create;
  FResponseStream := TMemoryStream.Create
end;

destructor TNotifyApiResponse.Destroy;
begin
  FreeAndNil(FResponseData);
  FreeAndNil(FResponseErrors);
  FreeAndNil(FResponseStream);
  inherited
end;

function TNotifyApiResponse.GetData: TNotifyResponseData;
var
  LRawString: String;
begin
  Result := FResponseData;

  if not Assigned(FResponseData) then
    Exit;

  FResponseStream.Position := 0;
  SetString(LRawString, PAnsiChar(FResponseStream.Memory), FResponseStream.Size);
  FResponseData.AsJson := LRawString;
end;

function TNotifyApiResponse.GetErros: TNotifyErrors;
var
  LRawString: String;
begin
  Result := FResponseErrors;

  if not Assigned(FResponseData) then
    Exit;

  FResponseStream.Position := 0;
  SetString(LRawString, PAnsiChar(FResponseStream.Memory), FResponseStream.Size);
  FResponseErrors.AsJson := LRawString;
end;

end.
