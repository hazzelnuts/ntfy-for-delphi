unit Notify.Action.DTO;

interface

uses
  Notify.JSON.Parser, System.Generics.Collections, REST.Json.Types;

type
  TNotifyActionsDTO = class(TJsonDTO)
  private
    [JSONName('action')]
    FAction: String;
    [JSONName('clear')]
    FClear: Boolean;
    [JSONName('label')]
    FLabel: String;
    [JSONName('url')]
    FUrl: String;
    [JSONName('intent')]
    FIntent: String;
  published
    property Action: String read FAction write FAction;
    property Clear: Boolean read FClear write FClear;
    property &Label: String read FLabel write FLabel;
    property Url: String read FUrl write FUrl;
    property Intent: String read FIntent write FIntent;
  end;


implementation

end.
