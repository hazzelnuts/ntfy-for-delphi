unit Notify.Error;

interface

uses
  Notify.JSON.Parser, REST.Json.Types;

type
  TNotifyErrors = class(TJsonDTO)
  private
    [JSONName('code')]
    FCode: Integer;
    [JSONName('http')]
    FHttp: Integer;
    [JSONName('error')]
    FError: String;
  published
    property Code: Integer read FCode write FCode;
    property Http: Integer read FHttp write FHttp;
    property Error: String read FError write FError;
  end;

implementation

end.
