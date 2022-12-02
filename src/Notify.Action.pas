unit Notify.Action;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  System.Generics.Collections;

type
  TNofifyAction = class sealed(TInterfacedObject, INotifyAction)
  private
    FType: TNotifyActionType;
    FLabel: String;
    FUrl: String;
    FClear: Boolean;
    FMethod: String;
    FBody: String;
    FHeaders: TDictionary<String, String>;
    FHeader: TJsonDTO;
  public
    class function New: INotifyAction;
    constructor Create;
    destructor Destroy; override;
    function &Type: TNotifyActionType; overload;
    function &Type(const AValue: TNotifyActionType): INotifyAction; overload;
    function &Label: String; overload;
    function &Label(const AValue: String): INotifyAction; overload;
    function Url: String; overload;
    function Url(const AValue: String): INotifyAction; overload;
    function Clear: Boolean; overload;
    function Clear(const AValue: Boolean): INotifyAction; overload;
    function Method: String; overload;
    function Method(const AValue: String): INotifyAction; overload;
    function Body: String; overload;
    function Body(const AValue: String): INotifyAction; overload;
    function Headers: TJsonDTO; overload;
    function Headers(const AValue: TJsonDTO): INotifyAction; overload;
  end;

implementation

{ TNofifyAction }

function TNofifyAction.&Type: TNotifyActionType;
begin
  Result := FType;
end;

function TNofifyAction.&Type(const AValue: TNotifyActionType ): INotifyAction;
begin
  Result := Self;
  FType := AValue;
end;

function TNofifyAction.Body: String;
begin
  Result := FBody;
end;

function TNofifyAction.Body(const AValue: String): INotifyAction;
begin
  Result := Self;
  FBody := AValue;
end;

function TNofifyAction.Clear(const AValue: Boolean): INotifyAction;
begin
  Result := Self;
  FClear := AValue;
end;

constructor TNofifyAction.Create;
begin
  FHeaders := TDictionary<String, String>.Create;
  FMethod := 'POST';
end;

destructor TNofifyAction.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TNofifyAction.Headers: TJsonDTO;
begin
  Result := FHeader;
end;

function TNofifyAction.Clear: Boolean;
begin
  Result := FClear;
end;

function TNofifyAction.&Label(const AValue: String): INotifyAction;
begin
  Result := Self;
  FLabel := AValue;
end;

function TNofifyAction.Method: String;
begin
  Result := FMethod;
end;

function TNofifyAction.Method(const AValue: String): INotifyAction;
begin
  Result := Self;
  FMethod := AValue;
end;

function TNofifyAction.&Label: String;
begin
  Result := FLabel;
end;

class function TNofifyAction.New: INotifyAction;
begin
  Result := Self.Create;
end;

function TNofifyAction.Url: String;
begin
  Result := FUrl;
end;

function TNofifyAction.Url(const AValue: String): INotifyAction;
begin
  Result := Self;
  FUrl := AValue;
end;

function TNofifyAction.Headers(const AValue: TJsonDTO): INotifyAction;
begin
  Result := Self;
  FHeader := AValue;
end;

end.
