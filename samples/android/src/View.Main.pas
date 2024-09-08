unit View.Main;

interface

uses
  System.Skia, FMX.Skia, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  FMX.ExtCtrls, FMX.Edit, FMX.Ani;

type

  TForm1 = class(TForm)
    CornerButton1: TCornerButton;
    CornerButton2: TCornerButton;
    Text1: TText;
    Edit1: TEdit;
    SkSvg1: TSkSvg;
    Spin: TFloatAnimation;
    Text2: TText;
    procedure StartService;
    procedure CornerButton1Click(Sender: TObject);
    procedure CornerButton2Click(Sender: TObject);
  private
    procedure Toast(const Text: String);
  end;

var
  Form1: TForm1;

implementation

uses
  System.Android.Service,
  Intent.Service.Helper,
  AndroidAPI.Helpers,
  AndroidAPI.JNI.Widget,
  System.Threading,
  Notify, Androidapi.JNI.JavaTypes;

{$R *.fmx}

procedure TForm1.CornerButton1Click(Sender: TObject);
begin
  StartService;
end;

procedure TForm1.CornerButton2Click(Sender: TObject);
begin
  TTask.Create(procedure
  begin

    Ntfy.Notification(
      New.Notification
        .Topic(Edit1.Text)
        .Title('🧪 Android Test')
        .MessageContent('Should receive in this app')
    ).Publish;

    TThread.Synchronize(nil, procedure
    begin
      Spin.Start;
      Toast('Notification published successfully.');
    end);
  end).Start;
end;

procedure TForm1.StartService;
var
  LIntentService: TIntentServiceHelper;
begin
  try                                                {serice name}        { topic }
    LIntentService := TIntentServiceHelper.Create('NtfyServiceLocal', 0, Edit1.Text);
    TAndroidHelper.Activity.startForegroundService(LIntentService.Intent);
    TThread.Synchronize(nil, procedure
    begin
      Spin.Start;
      Toast('Intent sent successfully, you may close the app now.');
    end);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TForm1.Toast(const Text: String);
var
  JText: JString;
begin
  JText := StringToJString(Text);
  TJToast.JavaClass.makeText(
    TAndroidHelper.Context,
    TJCharSequence.Wrap(JText),
    TJToast.JavaClass.LENGTH_SHORT
  ).show;
end;

end.
