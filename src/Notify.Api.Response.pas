unit Notify.Api.Response;

interface

uses
  Notify.Error,
  Notify.Response.Data,
  Notify.Notification.DTO,
  System.Classes;

type
  TNotifyApiResponse = record
    StatusCode: Integer;
    ResponseStream: TMemoryStream;
    ResponseData: TNotifyResponseData;
    ResponseErrors: TNotifyErrors;
    RawContent: String;
    function GetErros: TNotifyErrors;
    function GetData: TNotifyResponseData;
    property Erros: TNotifyErrors read GetErros;
    property Data: TNotifyResponseData read GetData;
  end;

implementation

{ TNotifyApiResponse }

function TNotifyApiResponse.GetData: TNotifyResponseData;
var
  LRawString: String;
begin
  Result := ResponseData;

  if not Assigned(ResponseData) then
    Exit;

  ResponseStream.Position := 0;
  SetString(LRawString, PAnsiChar(ResponseStream.Memory), ResponseStream.Size);
  ResponseData.AsJson := LRawString;
end;

function TNotifyApiResponse.GetErros: TNotifyErrors;
var
  LRawString: String;
begin
  Result := ResponseErrors;

  if not Assigned(ResponseData) then
    Exit;

  ResponseStream.Position := 0;
  SetString(LRawString, PAnsiChar(ResponseStream.Memory), ResponseStream.Size);
  ResponseErrors.AsJson := LRawString;
end;

end.
