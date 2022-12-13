unit Notify.Event.DTO;

interface

uses
  Rest.Json.Types,
  Notify.JSON.Parser,
  Notify.Action.DTO,
  Notify.Attachment.DTO,
  System.Generics.Collections;

type
  TNotifyEventDTO = class(TJsonDTO)
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
    [JSONName('tags')]
    FTagsArray: TArray<string>;
    [JSONMarshalled(False)]
    FTags: TList<string>;
    [GenericListReflect]
    FActions: TObjectList<TNotifyActionDTO>;
    [JSONName('actions'), JSONMarshalled(False)]
    FActionsArray: TArray<TNotifyActionDTO>;
    [JSONName('attachment')]
    FAttachment: TNotifyAttachmentDTO;
    function GetTags: TList<string>;
    function GetActions: TObjectList<TNotifyActionDTO>;
  protected
    function GetAsJson: string; override;
  published
    property Id: String read FId write FId;
    property Time: Integer read FTime write FTime;
    property Event: String read FEvent write FEvent;
    property Topic: String read FTopic write FTopic;
    property Priority: Integer read FPriority write FPriority;
    property Click: String read FClick write FClick;
    property Title: String read FTitle write FTitle;
    property Message: String read FMessage write FMessage;
    property Tags: TList<string> read GetTags;
    property Actions: TObjectList<TNotifyActionDTO> read GetActions;
    property Attachment: TNotifyAttachmentDTO read FAttachment write FAttachment;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TNotifyEventDTO }

constructor TNotifyEventDTO.Create;
begin
  inherited;
end;

destructor TNotifyEventDTO.Destroy;
begin
  GetTags.Free;
  GetActions.Free;
  inherited;
end;

function TNotifyEventDTO.GetActions: TObjectList<TNotifyActionDTO>;
begin
  Result := ObjectList<TNotifyActionDTO>(FActions, FActionsArray);
end;

function TNotifyEventDTO.GetAsJson: string;
begin
  RefreshArray<string>(FTags, FTagsArray);
  RefreshArray<TNotifyActionDTO>(FActions, FActionsArray);
  Result := inherited;
end;

function TNotifyEventDTO.GetTags: TList<string>;
begin
  Result := List<string>(FTags, FTagsArray);
end;

end.
