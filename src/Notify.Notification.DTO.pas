unit Notify.Notification.DTO;

interface

uses
  Notify.JSON.Parser, System.Generics.Collections, REST.Json.Types;

type
  TNotifyNotificationDTO = class(TJsonDTO)
  private
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
    function GetTags: TList<string>;
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
  public
    destructor Destroy; override;
  end;

implementation

{ TNotifyNotificationDTO }

destructor TNotifyNotificationDTO.Destroy;
begin
  GetTags.Free;
  inherited;
end;

function TNotifyNotificationDTO.GetTags: TList<string>;
begin
  Result := List<string>(FTags, FTagsArray);
end;

function TNotifyNotificationDTO.GetAsJson: string;
begin
  RefreshArray<string>(FTags, FTagsArray);
  Result := inherited;
end;

end.
