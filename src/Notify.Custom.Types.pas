unit Notify.Custom.Types;

interface

uses
  REST.Json.Types;

  /// Custom types defined by the user. You can modify the classes members
  /// on this unit adjusting it for your needs. Do not rename the classes'
  /// declarations as it's being used underneath the library. You only need
  /// renaming field names.

type

  {$M+}

  TNotifyActionHeaders = class
  private
    [JSONName('cmd')]
    FCmd: String;
    [JSONName('parameter')]
    FParameter: String;
    [JSONName('systemdate')]
    FSystemDate: String;
    [JSONName('auth')]
    FAuth: String;
  published
    property Cmd: String read FCmd write FCmd;
    property Parameter: String read FParameter write FParameter;
    property SystemDate: String read FSystemDate write FSystemDate;
    property Auth: String read FAuth write FAuth;
  end;

implementation

end.
