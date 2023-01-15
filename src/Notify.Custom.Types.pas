unit Notify.Custom.Types;

interface

uses
  REST.Json.Types;

  /// Custom types defined by the user. You can modify the classes members
  /// on this unit adjusting it for your needs. Do not rename the class
  /// declarations because it's being used underneath the library. You only need
  /// to rename fields/properties.

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
