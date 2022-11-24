unit Notify.Action.Factory;

interface

uses
  Notify.Action.Contract;

type
  TNotifyActionFactory = class sealed(TInterfacedObject, INotifyActionFactory)
  public
    class function New: INotifyActionFactory;
    function Action: INotifyAction;
  end;

implementation

uses
  Notify.Action;

{ TNotifyActionFactory }

function TNotifyActionFactory.Action: INotifyAction;
begin
  Result := TNofifyAction.New;
end;

class function TNotifyActionFactory.New: INotifyActionFactory;
begin
  Result := Self.Create;
end;

end.
