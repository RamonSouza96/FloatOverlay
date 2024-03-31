unit Main.View;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Effects, FMX.Colors, FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.FloatOverlay;

type
  TFormMain = class(TForm)
    BtnPermission: TButton;
    Layout1: TLayout;
    CbxTypeShape: TComboBox;
    ColorComboBox1: TColorComboBox;
    Button2: TButton;
    BtnSetText: TButton;
    EditText: TEdit;
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnPermissionClick(Sender: TObject);
    procedure BtnSetTextClick(Sender: TObject);
  private
    { Private declarations }
  public
    FFloatOverlay: TFMXFloatOverlay;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNI.Provider,
  Androidapi.Jni.Net;

procedure TFormMain.BtnPermissionClick(Sender: TObject);
var
  LCanDrawOverlays: Boolean;
  LIntent: JIntent;
  LUri : Jnet_Uri;
  LPackageName : string;
begin
  LCanDrawOverlays := TJSettings.JavaClass.canDrawOverlays(TAndroidHelper.Context);
  if not LCanDrawOverlays then
  begin
    LPackageName := 'package:' + JStringToString(TAndroidHelper.Context.getPackageName);
    LUri    := TJnet_Uri.JavaClass.parse(StringToJString(LPackageName));
    LIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_MANAGE_OVERLAY_PERMISSION, LUri);
    TAndroidHelper.Activity.startActivityForResult(LIntent, 0);
  end;
end;

procedure TFormMain.BtnSetTextClick(Sender: TObject);
begin
  if Assigned(FFloatOverlay) then
    FFloatOverlay.SetText(EditText.Text);
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  FFloatOverlay := TFMXFloatOverlay.Create;
  FFloatOverlay.CreateFloatObject(500, 500, 100, 100, ColorComboBox1.Color,
    CbxTypeShape.ItemIndex);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFloatOverlay.Free;
end;

end.
