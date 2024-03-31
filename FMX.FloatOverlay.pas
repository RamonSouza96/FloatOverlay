unit FMX.FloatOverlay;

interface

uses
  System.UITypes,
  Androidapi.Helpers,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type
  TMyClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure OnClick(v: JView); cdecl;
  end;

type
  TFMXFloatOverlay = class(TJavaLocal, JView_OnTouchListener)
  private
    // Set initial position of the floating object
    procedure SetPosition;
    // Limit the position of the floating object within the screen bounds
    procedure LimitPosition;
  public
    // Handle touch events on the floating object
    function OnTouch(AView: JView; AMotionEvent: JMotionEvent): Boolean; cdecl;
    // Create the floating object with specified parameters
    procedure CreateFloatObject(AWidth, AHeight, APositionX, APositionY: Integer;
      AColor: TAlphaColor; AType: Integer);
    // Delete the floating object from the screen
    procedure DeleteObject;
    // Set text to be displayed on the floating object
    procedure SetText(AText: string);
  end;

 var
    // WindowManager for managing the floating object
    FWindowManager: JWindowManager;
    // Layout parameters for the floating object
    FParams: JWindowManager_LayoutParams;
    // Layout view of the floating object
    FLayoutView: JRelativeLayout;
    // TextView for displaying text on the floating object
    LTextView: JTextView;
    // Touch listener for handling touch events on the floating object
    FTouchListener: TFMXFloatOverlay;
    // Variables to store position information of the floating object
    FPositionX,  FPositionY : Integer;
    FPositionIniX, FPositionIniY : Single;
    FPositionViewX, FPositionViewY : Integer;

implementation

// Event handler for button click
procedure TMyClickListener.OnClick(v: JView);
begin
  // Remove the floating object when the button is clicked
  if (FWindowManager <> nil) then
  begin
    if (FLayoutView <> nil) then
    begin
      FWindowManager.removeView(FLayoutView);
      FWindowManager := nil;
      FLayoutView := nil;
    end;
  end;
end;

// Set initial position of the floating object
procedure TFMXFloatOverlay.SetPosition;
var
  LDisplayMetrics : JDisplayMetrics;
begin
  LDisplayMetrics := TJDisplayMetrics.JavaClass.init;
  FWindowManager.getDefaultDisplay.getMetrics(LDisplayMetrics);
  FPositionX := LDisplayMetrics.widthPixels - FLayoutView.getWidth;
  FPositionY := LDisplayMetrics.heightPixels - FLayoutView.getHeight;
end;

// Handle touch events on the floating object
function TFMXFloatOverlay.OnTouch(AView: JView; AMotionEvent: JMotionEvent): Boolean;
var
  X, Y: Integer;
begin
  // Check if it's an initial touch event
  if AMotionEvent.getAction = TJMotionEvent.JavaClass.ACTION_DOWN then
  begin
    // If initial position is not set, set it
    if FPositionX = -1 then
      SetPosition;

    // Save initial touch coordinates and current coordinates of the floating rectangle
    FPositionIniX := AMotionEvent.getRawX();
    FPositionIniY := AMotionEvent.getRawY();
    FPositionViewX := FParams.X;
    FPositionViewY := FParams.Y;
  end;

  // Check if it's a finger movement event on the screen
  if AMotionEvent.getAction = TJMotionEvent.JavaClass.ACTION_MOVE then
  begin
    // Calculate the difference between current touch coordinates and initial coordinates
    X := Trunc(AMotionEvent.getRawX() - FPositionIniX);
    Y := Trunc(AMotionEvent.getRawY() - FPositionIniY);

    // Update the coordinates of the floating rectangle according to the finger movement
    FParams.X := FPositionViewX + X;
    FParams.Y := FPositionViewY + Y;

    // Limit the coordinates of the floating rectangle to ensure it does not go out of bounds
    LimitPosition;

    // Update the position of the floating rectangle on the screen
    FWindowManager.UpdateViewLayout(FLayoutView, FParams);
  end;
end;

// Limit the position of the floating object within the screen bounds
procedure TFMXFloatOverlay.LimitPosition;
begin
  // Limit the X coordinate
  if FParams.X < 0 then
    FParams.X := 0
  else if FParams.X > FPositionX then
    FParams.X := FPositionX;

  // Limit the Y coordinate
  if FParams.Y < 0 then
    FParams.Y := 0
  else if FParams.Y > FPositionY then
    FParams.Y := FPositionY;
end;

// Delete the floating object from the screen
procedure TFMXFloatOverlay.DeleteObject;
begin
  // Check if the floating object and window manager are initialized
  if (FWindowManager <> nil) then
  begin
    if (FLayoutView <> nil) then
    begin
      // Remove the floating object from the screen
      FWindowManager.removeView(FLayoutView);
      // Release the references
      FWindowManager := nil;
      FLayoutView := nil;
    end;
  end;
end;

procedure TFMXFloatOverlay.SetText(AText: string);
begin
  LTextView.setText(StrToJCharSequence(AText));
end;

// Create the floating object with specified parameters
procedure TFMXFloatOverlay.CreateFloatObject(AWidth, AHeight, APositionX,
  APositionY: Integer; AColor: TAlphaColor; AType: Integer);
var
  LObject : JObject;
  LGradientDrawable: JGradientDrawable;
  LButton: JButton;
  LButtonParams: JRelativeLayout_LayoutParams;
begin
  // Prevent creation of multiple floating objects
  if FLayoutView <> nil then
    Exit;

  // Set initial position
  FPositionX := -1;
  FPositionY := FPositionX;

  // Create touch listener
  FTouchListener := TFMXFloatOverlay.Create;

  // Create layout
  FLayoutView := TJRelativeLayout.JavaClass.init(SharedActivityContext);
  FLayoutView.setOnTouchListener(FTouchListener);

  // Layout parameters
  FParams := TJWindowManager_LayoutParams.JavaClass.init;
  FParams.&type := TJWindowManager_LayoutParams.JavaClass.TYPE_APPLICATION_OVERLAY;
  FParams.flags := TJWindowManager_LayoutParams.JavaClass.FLAG_NOT_FOCUSABLE;
  FParams.format := TJPixelFormat.JavaClass.TRANSLUCENT;
  FParams.gravity := TJGravity.JavaClass.LEFT or TJGravity.JavaClass.TOP;
  FParams.width := AWidth;
  FParams.height := AHeight;
  FParams.x := APositionX;
  FParams.y := APositionY;

  // Create shape
  LGradientDrawable := TJGradientDrawable.JavaClass.init;
  if AType = 0 then
  begin
    LGradientDrawable.setShape(TJGradientDrawable.JavaClass.RECTANGLE);
    LGradientDrawable.setCornerRadius(100);
  end
  else
    LGradientDrawable.setShape(TJGradientDrawable.JavaClass.OVAL);
  LGradientDrawable.setColor(TAndroidHelper.AlphaColorToJColor(AColor));
  FLayoutView.setBackground(LGradientDrawable);

  // Create TextView
  LTextView := TJTextView.JavaClass.init(SharedActivityContext);
  LTextView.setText(StrToJCharSequence('Firemonkey'));
  LTextView.setTextSize(TJTypedValue.JavaClass.COMPLEX_UNIT_SP, 14); // Text size 14sp
  LTextView.setTextColor(TJColor.JavaClass.WHITE); // Text color
  LTextView.setGravity(TJGravity.JavaClass.CENTER); // Set text alignment to top and center horizontally
  LTextView.setBackgroundColor(TJColor.JavaClass.TRANSPARENT); // Background color of the text view
  LTextView.setTextAlignment(TJView.JavaClass.TEXT_ALIGNMENT_CENTER);
  LTextView.setWidth(AWidth);
  LTextView.setHeight(Round(0.2 * AHeight)); // -80%
  FLayoutView.addView(LTextView); // Add to layout

  // Create Button
  LButton := TJButton.JavaClass.init(SharedActivityContext);
  LButton.setText(StrToJCharSequence('Delete'));
  LButton.setBackgroundColor(TJColor.JavaClass.BLUE);
  LButtonParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT, TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT);
  LButtonParams.addRule(TJRelativeLayout.JavaClass.CENTER_HORIZONTAL);
  LButtonParams.addRule(TJRelativeLayout.JavaClass.CENTER_VERTICAL);
  LButton.setLayoutParams(LButtonParams);
  LButton.setOnClickListener(TMyClickListener.Create);
  FLayoutView.addView(LButton);

  // Get WindowManager service
  LObject := SharedActivityContext.getSystemService(TJContext.JavaClass.WINDOW_SERVICE);
  FWindowManager := TJWindowManager.Wrap((LObject as ILocalObject).GetObjectID);

  // Add the layout view to the WindowManager
  FWindowManager.addView(FLayoutView, FParams);
end;

end.

