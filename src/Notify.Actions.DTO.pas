unit Notify.Actions.DTO;

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
  published
    property Action: String read FAction write FAction;
    property Clear: Boolean read FClear write FClear;
    property &Label: String read FLabel write FLabel;
    property Url: String read FUrl write FUrl;
  end;


implementation

end.
