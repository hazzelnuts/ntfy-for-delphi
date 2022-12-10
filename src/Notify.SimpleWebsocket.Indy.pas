unit Notify.SimpleWebsocket.Indy;

//  * Simple WebSocket client for Delphi
//  * http://www.websocket.org/echo.html
//  * Author: Lucas Rubian Schatz
//  * Copyright 2018, Indy Working Group.

interface

uses
  System.Classes, System.SysUtils, IdSSLOpenSSL, IdTCPClient, IdGlobal, IdCoderMIME,
     IdHash, IdHashSHA, math, System.threading, DateUtils, System.SyncObjs, IdURI;
Type
  TSWSCDataEvent = procedure(Sender: TObject; const Text: string) of object;
  TSWSCErrorEvent = procedure(Sender: TObject; exception:Exception;const Text: string; var forceDisconnect) of object;
  TOpCode = (TOContinuation, TOTextFrame, TOBinaryFrame, TOConnectionClose, TOPing, TOPong);
  Const
  TOpCodeByte: array[TopCode] of Byte = ($0, $1, $2, $8, $9, $A);

  Type
  TIdSimpleWebSocketClient = class(TIdTCPClient)
  private
    SecWebSocketAcceptExpectedResponse: string;
    FHeartBeatInterval: Cardinal;
    FAutoCreateHandler: Boolean;
    FURL: String;
    FOnUpgrade: TnotifyEvent;
    FonHeartBeatTimer: TNotifyEvent;
    FonError: TSWSCErrorEvent;
    FonPing: TSWSCDataEvent;
    FonConnectionDataEvent: TSWSCDataEvent;
    FonDataEvent: TSWSCDataEvent;
    FUpgraded: Boolean;

  protected

    lInternalLock:TCriticalSection;
    lClosingEventLocalHandshake:Boolean;
    //Sync Event
    lSyncFunctionEvent:TSimpleEvent;
    lSyncFunctionTrigger:TFunc<String,Boolean>;
    //Sync Event

    //get if a particular bit is 1
    function Get_a_Bit(const aValue: Cardinal; const Bit: Byte): Boolean;
    //set a particular bit as 1
    function Set_a_Bit(const aValue: Cardinal; const Bit: Byte): Cardinal;
    //set a particular bit as 0
    function Clear_a_Bit(const aValue: Cardinal; const Bit: Byte): Cardinal;

    procedure readFromWebSocket;virtual;
    function encodeFrame(pMsg:String; pOpCode:TOpCode=TOpCode.TOTextFrame):TIdBytes;
    function verifyHeader(pHeader:TStrings):boolean;
    procedure startHeartBeat;
    procedure sendCloseHandshake;
    function generateWebSocketKey:String;
  published
    property onDataEvent: TSWSCDataEvent read FonDataEvent write FonDataEvent;
    property onConnectionDataEvent: TSWSCDataEvent read FonConnectionDataEvent write FonConnectionDataEvent;
    property onPing: TSWSCDataEvent read FonPing write FonPing;
    property onError: TSWSCErrorEvent read FonError write FonError;
    property onHeartBeatTimer: TNotifyEvent read FonHeartBeatTimer write FonHeartBeatTimer;
    property OnUpgrade: TnotifyEvent read FOnUpgrade write FOnUpgrade;
    property HeartBeatInterval: Cardinal read FHeartBeatInterval write FHeartBeatInterval;
    property AutoCreateHandler: Boolean read FAutoCreateHandler write FAutoCreateHandler;
    property URL: String read FURL write FURL;
  public
    procedure Connect(pURL:String);overload;
    procedure Close;
    function Connected: Boolean; overload;
    property Upgraded: Boolean read FUpgraded;
    procedure writeText(pMsg:String);
    procedure writeTextSync(pMsg:String;pTriggerFunction:TFunc<String,Boolean>);
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
end;

implementation

{ TIdSimpleWebSocketClient }

function TIdSimpleWebSocketClient.Clear_a_Bit(const aValue: Cardinal;
  const Bit: Byte): Cardinal;
begin
  Result := aValue and not (1 shl Bit);
end;

procedure TIdSimpleWebSocketClient.Close;
begin
  self.lInternalLock.Enter;
  try
    if self.Connected then
    begin
      self.sendCloseHandshake;
      self.IOHandler.InputBuffer.Clear;
      self.IOHandler.CloseGracefully;
      self.Disconnect;
      if assigned(self.OnDisconnected) then
        self.OnDisconnected(self);
    end;
  finally
    self.lInternalLock.Leave;
  end
end;

function TIdSimpleWebSocketClient.generateWebSocketKey():String;
var rand:TidBytes;
  I: Integer;
begin
  SetLength(rand, 16);
  for I := low(rand) to High(rand) do
    rand[i] := byte(random(255));

  result := TIdEncoderMIME.EncodeBytes(rand);  //generates a random Base64String
  self.SecWebSocketAcceptExpectedResponse := Result + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'; //fixed string, see: https://tools.ietf.org/html/rfc6455#section-1.3

  with TIdHashSHA1.Create do
  try
    SecWebSocketAcceptExpectedResponse := TIdEncoderMIME.EncodeBytes(HashString( self.SecWebSocketAcceptExpectedResponse ));
  finally
    Free;
  end;
end;

function TIdSimpleWebSocketClient.Connected: Boolean;
begin
  result := false;  //for some reason, if its not connected raises an error after connection lost!
  try
    result := inherited;
  except
  end
end;

procedure TIdSimpleWebSocketClient.Connect(pURL: String);
var  URI      : TIdURI;
     lSecure  : Boolean;
begin
  uri := nil;
  try
    lClosingEventLocalHandshake := false;
    URI           := TIdURI.Create(pURL);
    self.URL      := pURL;
    self.Host     := URI.Host;
    URI.Protocol  := ReplaceOnlyFirst(URI.Protocol.ToLower, 'ws', 'http'); //replaces wss to https too, as apparently indy does not support ws(s) yet

    if URI.Path='' then
      URI.Path := '/';
    lSecure := uri.Protocol='https';

    if URI.Port.IsEmpty then
    begin
      if lSecure then
        self.Port := 443
      else
        self.Port := 80;
    end
    else
      self.Port := StrToInt(URI.Port);


    if lSecure and (self.IOHandler=nil) then
    begin
      if self.AutoCreateHandler then  //for simple life
      begin
        self.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(self);
        (self.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLOptions.Mode := TIdSSLMode.sslmClient;
        (self.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLOptions.SSLVersions := [TIdSSLVersion.sslvTLSv1, TIdSSLVersion.sslvTLSv1_1, TIdSSLVersion.sslvTLSv1_2]; //depending on your server, change this at your code;
      end
      else
        raise Exception.Create('Please, inform a TIdSSLIOHandlerSocketOpenSSL descendant');
    end;

    if self.Connected then
      raise Exception.Create('Already connected, verify');


    inherited Connect;
    if not URI.Port.IsEmpty then
      URI.Host := URI.Host+':'+URI.Port;
    self.Socket.WriteLn(format('GET %s HTTP/1.1', [uri.path+uri.Document]));
    self.Socket.WriteLn(format('Host: %s', [URI.Host]));
    self.Socket.WriteLn('User-Agent: Delphi WebSocket Simple Client');
    self.Socket.WriteLn('Connection: keep-alive, Upgrade');
    self.Socket.WriteLn('Upgrade: WebSocket');
    self.Socket.WriteLn('Sec-WebSocket-Version: 13');
    self.Socket.WriteLn(format('Sec-WebSocket-Key: %s', [generateWebSocketKey()]));
    self.Socket.WriteLn('');

    readFromWebSocket;
    startHeartBeat;
  finally
    URI.Free;
  end;
end;

procedure TIdSimpleWebSocketClient.sendCloseHandshake;
begin
  self.lClosingEventLocalHandshake := true;
  self.Socket.Write(self.encodeFrame('', TOpCode.TOConnectionClose));
  TThread.Sleep(200);
end;

constructor TIdSimpleWebSocketClient.Create(AOwner: TComponent);
begin
  inherited;
  lInternalLock := TCriticalSection.Create;
  Randomize;
  self.AutoCreateHandler := false;
  self.HeartBeatInterval := 30000;
end;

destructor TIdSimpleWebSocketClient.Destroy;
begin
  lInternalLock.Free;
  if self.AutoCreateHandler and Assigned(self.IOHandler) then
    self.IOHandler.Free;
  inherited;
end;

function TIdSimpleWebSocketClient.encodeFrame(pMsg:String; pOpCode:TOpCode): TIdBytes;
var FIN, MASK: Cardinal;
    MaskingKey:array[0..3] of cardinal;
    EXTENDED_PAYLOAD_LEN:array[0..3] of Cardinal;
    buffer:Tidbytes;
    I: Integer;
    xor1, xor2:char ;
    ExtendedPayloadLength:Integer;
begin
  FIN:=0;
  FIN := Set_a_bit(FIN,7) or TOpCodeByte[pOpCode];

  MASK  := set_a_bit(0,7);

  ExtendedPayloadLength:= 0;
  if pMsg.Length<=125 then
    MASK := MASK+pMsg.Length
  else
  if pMsg.Length<intPower(2,16) then
  begin
    MASK := MASK+126;
    ExtendedPayloadLength := 2;
    //https://stackoverflow.com/questions/13634240/delphi-xe3-integer-to-array-of-bytes
    //converts an integer to two bytes array
    EXTENDED_PAYLOAD_LEN[1] := byte(pmsg.Length);
    EXTENDED_PAYLOAD_LEN[0] := byte(pmsg.Length shr 8);
  end
  else
  begin
    mask := mask+127;
    ExtendedPayloadLength := 4;
    EXTENDED_PAYLOAD_LEN[3] := byte(pmsg.Length);
    EXTENDED_PAYLOAD_LEN[2] := byte(pmsg.Length shr 8);
    EXTENDED_PAYLOAD_LEN[1] := byte(pmsg.Length shr 16);
    EXTENDED_PAYLOAD_LEN[0] := byte(pmsg.Length shr 32);
  end;
  MaskingKey[0] := random(255);
  MaskingKey[1] := random(255);
  MaskingKey[2] := random(255);
  MaskingKey[3] := random(255);

  SetLength(buffer, 1+1+ExtendedPayloadLength+4+pMsg.Length);
  buffer[0] := FIN;
  buffer[1] := MASK;
  for I := 0 to ExtendedPayloadLength-1 do
    buffer[1+1+i] := EXTENDED_PAYLOAD_LEN[i];
  //payload mask:
  for I := 0 to 3 do
    buffer[1+1+ExtendedPayloadLength+i] := MaskingKey[i];
  for I := 0 to pMsg.Length-1 do
  begin
    {$IF DEFINED(iOS) or DEFINED(ANDROID)}
    xor1 := pMsg[i];
    {$ELSE}
    xor1 := pMsg[i+1];
    {$ENDIF}
    xor2 :=  chr(MaskingKey[((i) mod 4)]);
    xor2 := chr(ord(xor1) xor ord(xor2));
    buffer[1+1+ExtendedPayloadLength+4+i] := ord(xor2);
  end;
  result := buffer;
end;

function TIdSimpleWebSocketClient.Get_a_Bit(const aValue: Cardinal;
  const Bit: Byte): Boolean;
begin
  Result := (aValue and (1 shl Bit)) <> 0;
end;

procedure TIdSimpleWebSocketClient.readFromWebSocket;
var
  lSpool: string;
  b:Byte;
  T: ITask;
  lPos:Integer;
  lSize:int64;
  lOpCode:Byte;
  linFrame:Boolean;
  lMasked:boolean;
  lForceDisconnect:Boolean;
  lHeader:TStringlist;
//  lClosingRemoteHandshake:Boolean;
//  lPing:Boolean;
begin
  lSpool := '';
  lPos  := 0;
  lSize := 0;
  lOpCode := 0;
  lMasked    := false;
  FUpgraded  := false;
//  lPing     := false;
//  pingByte  := Set_a_Bit(0,7); //1001001//PingByte
//  pingByte  := Set_a_Bit(pingByte,3);
//  pingByte  := Set_a_Bit(pingByte,0);
//  closeByte := Set_a_Bit(0,7);//1001000//CloseByte
//  closeByte := Set_a_Bit(closeByte,3);

  lHeader := TStringList.Create;
  linFrame := false;

  try
    while Connected and not FUpgraded do //First, we guarantee that this is an valid Websocket
    begin
      b := self.Socket.ReadByte;

      lSpool := lSpool+chr(b);
      if (not FUpgraded and (b=ord(#13))) then
      begin
        if lSpool=#10#13 then
        begin

          //verifies header
          try
            if not verifyHeader(lHeader) then
            begin
              raise Exception.Create('URL is not from an valid websocket server, not a valid response header found');
            end;
          finally
            lHeader.Free;
          end;

          FUpgraded := true;
          lSpool := '';
          lPos := 0;
        end
        else
        begin
          if assigned(onConnectionDataEvent) then
            onConnectionDataEvent(self, lSpool);

          lHeader.Add(lSpool.Trim);
          lSpool := '';
        end;
      end;
    end;
  except
  on e:Exception do
  begin
    lForceDisconnect := true;
    if assigned(self.onError) then
      self.onError(self, e, e.Message, lForceDisconnect);
    if lForceDisconnect then
      self.Close;
    exit;
  end;
  end;


  if Connected then
    T := TTask.Run(
      procedure
      var
        extended_payload_length: cardinal;
      begin

        try
          while Connected do
          begin

            b := self.Socket.ReadByte;


            if FUpgraded and (lPos=0) and Get_a_Bit(b, 7) then //FIN
            begin
              linFrame  := true;
              lOpCode := Clear_a_Bit(b, 7);

              inc(lPos);


              if lOpCode=TOpCodeByte[TOpCode.TOConnectionClose] then
            end
            else if FUpgraded and (lPos=1) then
            begin
              lMasked  := Get_a_Bit(b, 7);
              lSize    := b;
              if lMasked then
                lSize    := b-set_a_bit(0,7);
              if lSize=0 then
                lPos := 0
              else
              if lSize=126 then // get size from 2 next bytes
              begin
                lsize := self.socket.ReadUInt16;
              end
              else if lSize=127 then begin
                lsize := self.socket.ReadUInt64;
              end;

              inc(lPos);
            end
            else
            if linFrame then
            begin
              lSpool := lSpool+chr(b);

              if (FUpgraded and (Length(lSpool)=lSize)) then
              begin
                lPos   := 0;
                linFrame := false;

                if lOpCode=TOpCodeByte[TOpCode.TOPing] then
                begin
                  try
                    lInternalLock.Enter;
                    self.Socket.Write(encodeFrame(lSpool, TOpCode.TOPong));
                  finally
                    lInternalLock.Leave;
                  end;

                if assigned(onPing) then
                  onPing(self, lSpool);
                end
                else
                begin
                  if FUpgraded and assigned(FonDataEvent) and (not (lOpCode=TOpCodeByte[TOpCode.TOConnectionClose]))  then
                    onDataEvent(self, lSpool);
                  if assigned(self.lSyncFunctionTrigger) then
                  begin
                    if self.lSyncFunctionTrigger(lSpool) then
                    begin
                      self.lSyncFunctionEvent.SetEvent;
                    end;
                  end;
                end;

                lSpool := '';
                if lOpCode=TOpCodeByte[TOpCode.TOConnectionClose] then
                begin
                  if not Self.lClosingEventLocalHandshake then
                  begin
                    self.Close;
                    if assigned(self.OnDisconnected) then
                      self.OnDisconnected(self);
                  end;
                  break
                end;

              end;
            end;
          end;
        except
        on e:Exception do
        begin
          lForceDisconnect := true;
          if assigned(self.onError) then
            self.onError(self, e, e.Message, lForceDisconnect);
          if lForceDisconnect then
            self.Close;
        end;
        end;
      end);

  if ((not Connected) or (not FUpgraded))and (not (( lOpCode=TOpCodeByte[TOpCode.TOConnectionClose]) or lClosingEventLocalHandshake))then
  begin

    raise Exception.Create('Websocket not connected or timeout'+QuotedStr(lSpool));
  end
  else
     if assigned(self.OnUpgrade) then
       self.OnUpgrade(self);

end;

procedure TIdSimpleWebSocketClient.startHeartBeat;
var TimeUltimaNotif:TDateTime;
    lForceDisconnect:Boolean;
begin
  TThread.CreateAnonymousThread(procedure begin
    TimeUltimaNotif := Now;
    try
      while (self.Connected) and (self.HeartBeatInterval>0) do
      begin
        //HeartBeat:
        if (MilliSecondsBetween(TimeUltimaNotif, Now) >= Floor(self.HeartBeatInterval)) then
        begin
          if assigned(self.onHeartBeatTimer) then
            self.onHeartBeatTimer(self);
          TimeUltimaNotif := Now;
        end;
          TThread.Sleep(500);
      end;
    except
    on e:Exception do
    begin
      lForceDisconnect := true;
      if assigned(self.onError) then
        self.onError(self, e, e.Message, lForceDisconnect);
      if lForceDisconnect then
        self.Close;
    end;
    end;

  end).Start;
end;

function TIdSimpleWebSocketClient.verifyHeader(pHeader: TStrings): boolean;
begin
  pHeader.NameValueSeparator := ':';
  result := false;
  if (pos('HTTP/1.1 101', pHeader[0])=0) and (pos('HTTP/1.1', pHeader[0])>0) then
    raise Exception.Create(pHeader[0].Substring(9));

  if (pHeader.Values['Connection'].Trim.ToLower='upgrade') and (pHeader.Values['Upgrade'].Trim.ToLower='websocket') then
  begin
    if pHeader.Values['Sec-WebSocket-Accept'].Trim=self.SecWebSocketAcceptExpectedResponse then
      result := true
    else
    if pHeader.Values['Sec-WebSocket-Accept'].trim.IsEmpty then
      result := true
    else
      raise Exception.Create('Unexpected return key on Sec-WebSocket-Accept in handshake');

  end;
end;

function TIdSimpleWebSocketClient.Set_a_Bit(const aValue: Cardinal;
  const Bit: Byte): Cardinal;
begin
  Result := aValue or (1 shl Bit);
end;

procedure TIdSimpleWebSocketClient.writeText(pMsg: String);
begin
  try
    lInternalLock.Enter;
    self.Socket.Write(encodeFrame(pMSG));
  finally
    lInternalLock.Leave;
  end;
end;

procedure TIdSimpleWebSocketClient.writeTextSync(pMsg: String;
  pTriggerFunction: TFunc<String,Boolean>);
begin
  self.lSyncFunctionTrigger :=  pTriggerFunction;
  try
    self.lSyncFunctionEvent := TSimpleEvent.Create();
    self.lSyncFunctionEvent.ResetEvent;
    self.writeText(pMsg);
    self.lSyncFunctionEvent.WaitFor(self.ReadTimeout);

  finally
    self.lSyncFunctionTrigger:= nil;
    self.lSyncFunctionEvent.Free;
  end;


end;

end.
