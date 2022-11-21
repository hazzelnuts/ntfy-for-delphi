unit RootUnit;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TActions = class
  private
    [JSONName('action')]
    FAction: string;
    [JSONName('clear')]
    FClear: Boolean;
    [JSONName('label')]
    FLabel: string;
    [JSONName('url')]
    FUrl: string;
  published
    property Action: string read FAction write FAction;
    property Clear: Boolean read FClear write FClear;
    property &Label: string read FLabel write FLabel;
    property Url: string read FUrl write FUrl;
  end;
  
  TRoot = class(TJsonDTO)
  private
    [JSONName('actions'), JSONMarshalled(False)]
    FActionsArray: TArray<TActions>;
    [GenericListReflect]
    FActions: TObjectList<TActions>;
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
    function GetActions: TObjectList<TActions>;
    function GetTags: TList<string>;
  protected
    function GetAsJson: string; override;
  published
    property Actions: TObjectList<TActions> read GetActions;
    property Attach: string read FAttach write FAttach;
    property Click: string read FClick write FClick;
    property Filename: string read FFilename write FFilename;
    property Message: string read FMessage write FMessage;
    property Priority: Integer read FPriority write FPriority;
    property Tags: TList<string> read GetTags;
    property Title: string read FTitle write FTitle;
    property Topic: string read FTopic write FTopic;
  public
    destructor Destroy; override;
  end;
  
implementation

{ TRoot }

destructor TRoot.Destroy;
begin
  GetTags.Free;
  GetActions.Free;
  inherited;
end;

function TRoot.GetActions: TObjectList<TActions>;
begin
  Result := ObjectList<TActions>(FActions, FActionsArray);
end;

function TRoot.GetTags: TList<string>;
begin
  Result := List<string>(FTags, FTagsArray);
end;

function TRoot.GetAsJson: string;
begin
  RefreshArray<TActions>(FActions, FActionsArray);
  RefreshArray<string>(FTags, FTagsArray);
  Result := inherited;
end;

end.