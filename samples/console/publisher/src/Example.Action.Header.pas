unit Example.Action.Header;

interface

  ///
  ///  Example using ation headers
  ///

procedure UseActionHeader;

implementation

uses
  Notify,
  Notify.Custom.Types,
  REST.Json.Types;

procedure UseActionHeader;
var
  Headers: TNotifyActionHeaders;
begin
  Headers := TNotifyActionHeaders.Create;
  try
    Headers.Auth := 'Bearer 982j38sdfhj2181jcznxc81234b9as-i34';
    Headers.Cmd := 'OPEN XOAUTH .\crs\main\lib';

    Ntfy.Notification(New.Notification
      .Action(New.Action
        .&Type(TNotifyActionType.HTTP)
        .&Label('Action Headers')
        .Url('http://someurl.com')
        .Body('"message":"hello"')
        .Headers(Headers))
    );

  finally
    Headers.Free;
  end;
end;

end.
