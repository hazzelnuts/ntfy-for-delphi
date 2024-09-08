unit Notify.Response.Data;

interface

uses
  REST.Json.Types, Notify.JSON.Parser;

type
  TNotifyResponseDTO = class(TJsonDTO)
  private
    [JSONName('id')]
    FId: String;
    [JSONName('time')]
    FTime: Integer;
  published
    property Id: String read FId write FId;
    property Time: Integer read FTime write FTime;
  end;

implementation

end.
