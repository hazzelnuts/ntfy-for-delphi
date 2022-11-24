unit Notify.Action;

interface

uses
  Notify.Types,
  Notify.Action.Contract;

type
  TNofifyAction = class sealed(TInterfacedObject, INotifyAction)
  private
    FType: TNotifyActionType;
    FLabel: String;
    FUrl: String;
    FClear: Boolean;
  public
    class function New: INotifyAction;
    function &Type: TNotifyActionType; overload;
    function &Type(const AValue: TNotifyActionType): INotifyAction; overload;
    function &Label: String; overload;
    function &Label(const AValue: String): INotifyAction; overload;
    function Url: String; overload;
    function Url(const AValue: String): INotifyAction; overload;
    function Clear: Boolean; overload;
    function Clear(const AValue: Boolean): INotifyAction; overload;
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

function TNofifyAction.Clear(const AValue: Boolean): INotifyAction;
begin
  Result := Self;
  FClear := AValue;
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

end.
