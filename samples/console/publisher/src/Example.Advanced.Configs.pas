unit Example.Advanced.Configs;

interface

  ///
  ///  Example for self-hosted apis
  ///

procedure UseAdvancedConfigs;

implementation

uses
  Notify;

procedure UseAdvancedConfigs;
begin

  // Username and password for protected topics
  // Be careful! Username and password are not encrypted, only encoded! Use HTTPS
  Ntfy.UserName('username').Password('password');

  // Disabling Firebase will significantly increase the amount of time messages
  // are delivered in Android
  Ntfy.DisableFireBase(True);

  //Disabling cache will cause messages not to be delivered to whom is not subscribed
  Ntfy.Cache(False);

end;

end.
