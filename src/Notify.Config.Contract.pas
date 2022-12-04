unit Notify.Config.Contract;

interface

type
  INotifyConfig = interface
    ['{8CFCA0D0-3637-4367-9F56-B420D5441659}']
    function BaseURL: String; overload;
    function BaseURL(const AValue: String): INotifyConfig; overload;
  end;

  INotifyConfigFactory = interface
    ['{E3FFED5C-4F3D-4C7F-BD80-BD13EFE528CF}']
    function Config: INotifyConfig;
  end;

implementation

end.
