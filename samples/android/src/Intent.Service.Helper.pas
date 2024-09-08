unit Intent.Service.Helper;

interface

uses
  AndroidApi.JNI.GraphicsContentViewText;

type
  TIntentServiceHelper = record
  private
    FIntent: JIntent;
    FCode: Integer;
    FData: string;
  public
    class function Create(const Intent: JIntent): TIntentServiceHelper; overload; static;
    class function Create(const AServiceName: string; Code: Integer; Data: string): TIntentServiceHelper; overload; static;
    property Code: Integer read FCode;
    property Data: string read FData;
    property Intent: JIntent read FIntent;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

{ TIntentServiceHelper }

class function TIntentServiceHelper.Create(const Intent: JIntent): TIntentServiceHelper;
begin
  Result.FCode := Intent.getIntExtra(TAndroidHelper.StringToJString('Code'), -1);
  Result.FData := TAndroidHelper.JStringToString(Intent.getStringExtra(TAndroidHelper.StringToJString('Data')));
end;

class function TIntentServiceHelper.Create(const AServiceName: string; Code: Integer; Data: string): TIntentServiceHelper;
var
  LService: string;
begin
  Result.FIntent := TJIntent.Create;
  LService := AServiceName;
  if not LService.StartsWith('com.embarcadero.services.') then
    LService := 'com.embarcadero.services.' + LService;
  Result.FIntent.setClassName(TAndroidHelper.Context.getPackageName(), TAndroidHelper.StringToJString(LService));
  Result.FIntent.putExtra(TAndroidHelper.StringToJString('Code'), Code);
  Result.FIntent.putExtra(TAndroidHelper.StringToJString('Data'), TAndroidHelper.StringToJString(Data));
end;

end.
