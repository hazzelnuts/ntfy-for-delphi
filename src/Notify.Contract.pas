unit Notify.Contract;

interface

uses
  System.SysUtils,
  Notify.Types;

type
  INotify = interface
    ['{AEDB3C31-D45F-4469-9427-9CEA5427A4E3}']
    function Poll: INotify;
    function Scheduled: INotify;
    function Since(const PValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotify; overload;
    function Since(const PValue: Integer): INotify; overload;
    function Listen: INotify;
    function Publish: INotify;
  end;

implementation

end.
