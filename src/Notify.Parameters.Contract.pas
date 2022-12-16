unit Notify.Parameters.Contract;

interface

type
  INotifyParameters = interface
    ['{212F2991-844B-41C2-9418-0B31B74C47D5}']
    function Poll: Boolean; overload;
    function Poll(const AValue: Boolean): INotifyParameters; overload;
  end;

  INotifyParametersFactory = interface
    ['{2E4DE030-5261-43AD-B13D-4D2F506F86B6}']
    function Parameters: INotifyParameters;
  end;

implementation

end.
