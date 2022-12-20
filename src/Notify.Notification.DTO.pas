unit Notify.Notification.DTO;

interface

uses
  Notify.JSON.Parser,
  System.Generics.Collections,
  REST.Json.Types,
  Notify.Action.DTO;

type
  TNotifyNotificationDTO = class(TJsonDTO)
  private
    [JSONName('id')]
    FId: String;
    [JSONName('time')]
    FTime: Integer;
    [JSONName('attach')]
    FAttach: string;
    [JSONName('click')]
    FClick: string;
    [JSONName('filename')]
    FFilename: string;
    [JSONName('message')]
    FMessage: string;
    [JSONName('priority')]
    FPriority: Integer;
    [JSONName('tags')]
    FTagsArray: TArray<string>;
    [JSONMarshalled(False)]
    FTags: TList<string>;
    [JSONName('title')]
    FTitle: string;
    [JSONName('topic')]
    FTopic: string;
    [JSONName('delay')]
    FDelay: String;
    [JSONName('email')]
    FEmail: String;
    [GenericListReflect]
    FActions: TObjectList<TNotifyActionDTO>;
    [JSONName('actions'), JSONMarshalled(False)]
    FActionsArray: TArray<TNotifyActionDTO>;
    function GetTags: TList<string>;
    function GetActions: TObjectList<TNotifyActionDTO>;
  protected
    function GetAsJson: string; override;
  published
    property Attach: string read FAttach write FAttach;
    property Click: string read FClick write FClick;
    property Filename: string read FFilename write FFilename;
    property MessageContent: string read FMessage write FMessage;
    property Priority: Integer read FPriority write FPriority;
    property Tags: TList<string> read GetTags;
    property Title: string read FTitle write FTitle;
    property Topic: string read FTopic write FTopic;
    property Actions: TObjectList<TNotifyActionDTO> read GetActions;
    property Delay: String read FDelay write FDelay;
    property Email: String read FEmail write FEmail;
    property Id: String read Fid write FId;
    property Time: Integer read FTime write FTime;
  public
    destructor Destroy; override;
  end;

implementation

{ TNotifyNotificationDTO }

destructor TNotifyNotificationDTO.Destroy;
begin
  GetTags.Free;
  GetActions.Free;
  inherited;
end;

function TNotifyNotificationDTO.GetTags: TList<string>;
begin
  Result := List<string>(FTags, FTagsArray);
end;

function TNotifyNotificationDTO.GetActions: TObjectList<TNotifyActionDTO>;
begin
  Result := ObjectList<TNotifyActionDTO>(FActions, FActionsArray);
end;

function TNotifyNotificationDTO.GetAsJson: string;
begin
  RefreshArray<string>(FTags, FTagsArray);
  RefreshArray<TNotifyActionDTO>(FActions, FActionsArray);
  Result := inherited;
end;

end.
