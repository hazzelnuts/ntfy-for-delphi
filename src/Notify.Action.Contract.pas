unit Notify.Action.Contract;

interface

type
  INotifyAction = interface
    ['{C7C7E46E-A4BA-440A-8A48-3AD485825186}']
    function Action: String; overload;
    function Action(const PValue: String): INotifyAction; overload;
    function ActionLabel: String; overload;
    function ActionLabel(const PValue: String): INotifyAction; overload;
    function Url: String; overload;
    function Url(const PValue: String): INotifyAction; overload;
    function Clear: Boolean; overload;
    function Clear(const PValue: Boolean): INotifyAction; overload;
    function Body: String; overload;
    function Body(const PValue: String): INotifyAction; overload;
  end;

implementation

end.
