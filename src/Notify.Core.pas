unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Contract,
  Notify.Provider.Contract,
  Notify.Publisher.Contract;

type
  TNotifyCore = class sealed(TInterfacedObject, INotify)
  strict private
    FProvider: INotifyProvider;
    FPublisher: INotifyPublisher;
  public
    constructor Create;
    function Poll: INotify;
    function Scheduled: INotify;
    function Since(const PValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotify; overload;
    function Since(const PValue: Integer): INotify; overload;
    function Listen: INotify;
    function Publish: INotify;
    function Notification(const PPublisher: INotifyPublisher): INotify;
  end;

implementation

uses
  Notify.Provider.Factory;

{ TNotifyCore }

constructor TNotifyCore.Create;
begin

end;

function TNotifyCore.Listen: INotify;
begin

end;

function TNotifyCore.Poll: INotify;
begin

end;

function TNotifyCore.Publish: INotify;
begin

end;

function TNotifyCore.Scheduled: INotify;
begin

end;

function TNotifyCore.Since(const PValue: Integer): INotify;
begin

end;

function TNotifyCore.Since(const PValue: String;
  const PFeytchType: TNotifyFectchType): INotify;
begin

end;

function TNotifyCore.Subject(const PValue: String): INotify;
begin

end;

end.
