unit Notify.Api.Response;

interface

uses
  Notify.Notification.DTO;

type
  TNotifyApiResponse = record
    StatusCode: Integer;
    Content: String;
    Notification: TNotifyNotificationDTO;
  end;

implementation

end.
