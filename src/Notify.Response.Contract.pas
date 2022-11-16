unit Notify.Response.Contract;

interface

type
  INotifyResponse = interface
    ['{8B34E4D6-A130-4EA4-8BA2-411702A90B33}']
    function Content: String; overload;
    function Content(const PValue: String): INotifyResponse; overload;
    function StatusCode: Integer; overload;
    function StatusCode(const PValue: Integer): INotifyResponse; overload;
  end;

implementation

end.
