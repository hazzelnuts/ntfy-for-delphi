unit Example.Action.HTTP;

interface

  ///
  ///  Example using action view
  ///

procedure UseActionHTTP;

implementation

uses
  Notify;

procedure UseActionHTTP;
begin

  Ntfy.Notification(New.Notification
    .Topic('your-very-secret-topic')
    .Action(New.Action
      .&Type(TNotifyActionType.HTTP)
      .Url('https://viacep.com.br/ws/01001000/json/')
      .&Label('Send GET request to viacep')
      .Method('GET'))
  );

end;

end.
