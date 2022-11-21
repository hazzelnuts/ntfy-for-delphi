unit Notify.Action.Contract;

interface

type
  INotifyAction = interface
    ['{C7C7E46E-A4BA-440A-8A48-3AD485825186}']
    function Action: String; overload;
    function Action(const AValue: String): INotifyAction; overload;
    function &Label: String; overload;
    function &Label(const AValue: String): INotifyAction; overload;
    function Url: String; overload;
    function Url(const AValue: String): INotifyAction; overload;
    function Clear: Boolean; overload;
    function Clear(const AValue: Boolean): INotifyAction; overload;
    function Body: String; overload;
    function Body(const AValue: String): INotifyAction; overload;
  end;

implementation

end.
