unit Notify.Subscription.DTO;

interface

uses
  Rest.Json.Types,
  Notify.JSON.Parser;

type
  TNotifySubscriptionDTO = class(TJsonDTO)
  private
    [JSONName('id')]
    FId: String;
    [JSONName('time')]
    FTime: Integer;
    [JSONName('event')]
    FEvent: String;
    [JSONName('topic')]
    FTopic: String;
    [JSONName('priority')]
    FPriority: Integer;
    [JSONName('click')]
    FClick: String;
    [JSONName('title')]
    FTitle: String;
    [JSONName('message')]
    FMessage: String;
  published
    property Id: String read FId write FId;
    property Time: Integer read FTime write FTime;
    property Event: String read FEvent write FEvent;
    property Topic: String read FTopic write FTopic;
    property Priority: Integer read FPriority write FPriority;
    property Click: String read FClick write FClick;
    property Title: String read FTitle write FTitle;
    property Message: String read FMessage write FMessage;
  end;


implementation

end.
