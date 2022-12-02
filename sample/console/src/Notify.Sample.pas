unit Notify.Sample;

interface

uses
  REST.Json.Types,
  Notify;

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
  ActionHeaders.Auth := 'Bearer 982j38sdfhj2181jcznxc81234b9as-i34';
  ActionHeaders.Cmd := 'OPEN XOAUTH .\crs\main\lib';

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .Title('Work time')
      .MessageContent('Ending for today...')
      .Priority(TNotifyPriority.MIN)
      .Tags(['ice_cream'])
      .Action(
        New.Action
          .&Type(TNotifyActionType.HTTP)
          .&Label('Open Mail')
          .Method('GET')
          .Url('https://viacep.com.br/ws/01001000/json/'))
  );

  Ntfy.Publish;

  ActionHeaders.Free;

finalization

end.
