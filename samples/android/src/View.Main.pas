unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText, System.Android.Service;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FNtfyService: TLocalServiceConnection;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils,
  System.Threading;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  LJIntent: JIntent;
  LTask: ITask;
begin
  /// Sets up a subscription to a push notification service using the Ntfy library.
  /// The BaseURL method sets the base URL for the service (in this case,
  /// https://ntfy.sh), and the Subscribe method subscribes to a specific
  /// channel (ntfy-android-test-delphi) and specifies a callback function to
  /// handle incoming notifications. The callback function is defined using an
  /// anonymous procedure that calls the PushNotification function with the
  /// incoming event as an argument.
  LTask := TTask.Create(
    procedure
    begin


      // You can use both options, broadcasting an intent...
      LJIntent := TJIntent.Create;
      LJIntent.setClassName(
        TAndroidHelper.Context,
        StringToJString('com.embarcadero.services.NtfyAndroidService'));
        TAndroidHelper.Context.startForegroundService(LJIntent);


      // Or starting the service...
      //FNtfyService := TLocalServiceConnection.Create;
      //FNtfyService.StartService('NtfyAndroidService');

    end);



  LTask.Start;
end;

end.
