unit Notify.Action.Contract;

interface

uses
  Notify.Types,
  System.Generics.Collections;

type
  INotifyAction = interface
    ['{C7C7E46E-A4BA-440A-8A48-3AD485825186}']
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

end.
