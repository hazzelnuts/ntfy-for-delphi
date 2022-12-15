unit Notify.Attachment;

interface

uses
  Notify.Attachment.Contract;

type
  TNotifyAttachment = class(TInterfacedObject, INotifyAttachment)
  strict private
    FName: String;
    FUrl: String;
    FMimeType: String;
    FSize: Integer;
    FExpires: Integer;
  public
    class function New: INotifyAttachment;
  private
    function Name: String; overload;
    function Name(const AValue: String): INotifyAttachment; overload;
    function Url: String; overload;
    function Url(const AValue:String): INotifyAttachment; overload;
    function MimeType: String; overload;
    function MimeType(const AValue: String): INotifyAttachment; overload;
    function Size: Integer; overload;
    function Size(const AValue: Integer): INotifyAttachment; overload;
    function Expires: Integer; overload;
    function Expires(const AValue: Integer): INotifyAttachment; overload;
  end;

implementation

{ TNotifyAttachment }

function TNotifyAttachment.Expires: Integer;
begin
  Result := FExpires;
end;

function TNotifyAttachment.Expires(const AValue: Integer): INotifyAttachment;
begin
  Result := Self;
  FExpires := AValue;
end;

function TNotifyAttachment.MimeType: String;
begin
  Result := FMimeType;
end;

function TNotifyAttachment.MimeType(const AValue: String): INotifyAttachment;
begin
  Result := Self;
  FMimeType := AValue;
end;

function TNotifyAttachment.Name(const AValue: String): INotifyAttachment;
begin
  Result := Self;
  FName := AValue;
end;

function TNotifyAttachment.Name: String;
begin
  Result := FName;
end;

class function TNotifyAttachment.New: INotifyAttachment;
begin
  Result := Self.Create;
end;

function TNotifyAttachment.Size: Integer;
begin
  Result := FSize;
end;

function TNotifyAttachment.Size(const AValue: Integer): INotifyAttachment;
begin
  Result := Self;
  FSize := AValue;
end;

function TNotifyAttachment.Url: String;
begin
  Result := FUrl;
end;

function TNotifyAttachment.Url(const AValue: String): INotifyAttachment;
begin
  Result := Self;
  FUrl := AValue;
end;

end.
