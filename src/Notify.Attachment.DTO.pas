unit Notify.Attachment.DTO;

interface

uses
  Rest.Json.Types,
  Notify.JSON.Parser;

type
  TNotifyAttachmentDTO = class(TJsonDTO)
  private
    [JSONName('name')]
    FName: String;
    [JSONName('url')]
    FUrl: String;
    [JSONName('type')]
    FMimeType: String;
    [JSONName('size')]
    FSize: Integer;
    [JSONName('expires')]
    FExpires: Integer;
  published
    property Name: String read FName write FName;
    property Url: String read FUrl write FUrl;
    property MimeType: String read FMimeType write FMimeType;
    property Size: Integer read FSize write FSize;
    property Expires: Integer read FExpires write FExpires;
  end;


implementation

end.
