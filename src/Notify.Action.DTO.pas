unit Notify.Action.DTO;

interface

uses
  Notify.Custom.Types,
  Notify.JSON.Parser,
  System.Generics.Collections,
  REST.Json.Types;

type

  TNotifyActionDTO = class(TJsonDTO)
  private
    [JSONName('action')]
    FAction: String;
    [JSONName('clear')]
    FClear: Boolean;
    [JSONName('label')]
    FLabel: String;
    [JSONName('url')]
    FUrl: String;
    [JSONName('method')]
    FMethod: String;
    [JSONName('body')]
    FBody: String;
    [JSONName('headers'), JSONMarshalled]
    FHeaders: TNotifyActionHeaders;
  published
    property Action: String read FAction write FAction;
    property Clear: Boolean read FClear write FClear;
    property &Label: String read FLabel write FLabel;
    property Url: String read FUrl write FUrl;
    property Method: String read FMethod write FMethod;
    property Body: String read FBody write FBody;
    property Headers: TNotifyActionHeaders read FHeaders write FHeaders;
  end;


implementation

end.
