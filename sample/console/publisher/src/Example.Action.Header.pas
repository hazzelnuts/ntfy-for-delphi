unit Example.Action.Header;

interface

  ///
  ///  Example using ation headers
  ///

procedure UseActionHeader;

implementation

uses
  Notify,
  REST.Json.Types;

type
  THeaders = class(TJsonDTO)
  private
    [JSONName('auth')]
    FAuth: String;
    [JSONName('cmd')]
    FCmd: String;
  published
    property Auth: String read FAuth write FAuth;
    property Cmd: String read FCmd write FCmd;
  end;

procedure UseActionHeader;
var
  Headers: THeaders;
begin
  Headers := THeaders.Create;
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
