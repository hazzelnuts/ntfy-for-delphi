unit Action.Header;

interface

  ///
  ///  Example using ation headers
  ///

uses
  NotifyDelphi,
  REST.Json.Types;

type
  TActionHeader = class(TJsonDTO)
  private
    [JSONName('auth')]
    FAuth: String;
    [JSONName('cmd')]
    FCmd: String;
  published
    property Auth: String read FAuth write FAuth;
    property Cmd: String read FCmd write FCmd;
  end;

implementation

var
  ActionHeaders: TActionHeader;

initialization

  ActionHeaders := TActionHeader.Create;
  try
    ActionHeaders.Auth := 'Bearer 982j38sdfhj2181jcznxc81234b9as-i34';
    ActionHeaders.Cmd := 'OPEN XOAUTH .\crs\main\lib';
    Ntfy.Notification(New.Notification
      .Action(New.Action
        .&Type(TNotifyActionType.HTTP)
        .Url('http://someurl.com')
        .Body('"message":"hello"')
        .Headers(ActionHeaders))
    );
  finally
    ActionHeaders.Free;
  end;

finalization


end.
