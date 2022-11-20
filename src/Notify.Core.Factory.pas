unit Notify.Core.Factory;

interface

uses
  Notify.Core.Contract;

type
  TNotifyCoreFactory = class sealed(TInterfacedObject, INotifyCoreFactory)
  public
    class function New: INotifyCoreFactory;
    function Core: INotifyCore;
  end;

implementation

uses
  Notify.Core;

{ TNotifyCoreFactory }

function TNotifyCoreFactory.Core: INotifyCore;
begin
  Result := TNotifyCore.New;
end;

class function TNotifyCoreFactory.New: INotifyCoreFactory;
begin
  Result := Self.Create;
end;

end.
