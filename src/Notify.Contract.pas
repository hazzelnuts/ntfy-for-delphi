unit Notify.Contract;

interface

uses
  System.SysUtils,
  Notify.Types,
  Notify.Publisher.Contract;

type
  INotifyCore = interface
    ['{AEDB3C31-D45F-4469-9427-9CEA5427A4E3}']
    function Poll: INotifyCore;
    function Scheduled: INotifyCore;
    function Since(const PValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const PValue: Integer): INotifyCore; overload;
    function Listen: INotifyCore;
    function Publish: INotifyCore;
    function Notification(const PPublisher: INotifyPublisher): INotifyCore;
  end;

implementation

end.
