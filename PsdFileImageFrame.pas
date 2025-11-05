unit PsdFileImageFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,PsdImage,
  Vcl.StdCtrls;

// PSDファイル表示画面
type
  TFramePsdFileImage = class(TFrame)
    ImagePsd: TImage;
    scrBox: TScrollBox;
    procedure FrameResize(Sender: TObject);
    procedure ImagePsdMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImagePsdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImagePsdMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private 宣言 }
    FTimer       : TTimer;           // 遅延描画タイマー
    FPsdImage    : TPsdImage;        // PSDクラス
    FMouseDown   : Boolean;          //
    FMouseX      : Integer;
    FMouseY      : Integer;
    FScaleIndex  : Integer;
    FDrawing     : Boolean;          // True: 描画中
    FShowed      : Boolean;          // True : 描画処理を実施した
    FOnRectChange: TNotifyEvent;

    // 拡大
    procedure ProcBig();
    // 縮小
    procedure ProcSmall();
    procedure ProcMove(const X,Y : Integer);
    // 再描画
    procedure ReDraw();

    // ホイールスクロールイベント
    procedure WMMousewheel(var Msg: TMessage); message WM_MOUSEWHEEL;

    // 描画遅延タイマー
    procedure OnTimer(Sender: TObject);
    // 描画完了イベント
    procedure OnDrawFinish(Sender: TObject);
    function GetScale: Double;
  protected
    procedure DoRectChange();virtual;
  public
    { Public 宣言 }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure Show();
    procedure ShowImage();
    procedure ProcResize();
    property PsdImage : TPsdImage read FPsdImage write FPsdImage;
    // True : 描画中
    property Drawing : Boolean read FDrawing;
    //property ClipRect : TRect read FClipRect write FClipRect;
    // 現在の拡大率
    property Scale : Double read GetScale;

    property OnRectChange : TNotifyEvent read FOnRectChange write FOnRectChange;
  end;

implementation

uses PNGImage;

// 拡大率テーブル
const TBL_SCALE : array[0..4] of Double = (1,1.5,2,4,8);

// アスペクトル比を合わせた範囲を取得 r : 変形先としての範囲 aWidth,aHeight:元画像ファイル
procedure RectToStreachRect(var r : TRect;const aWidth,aHeight : Integer);
var
  xh,yh,xhr,yhr : Integer;
begin
  //if aWidth = aHeight then exit;
  if aWidth = 0 then exit;
  if aHeight = 0 then exit;

  xhr := r.Width;
  yhr := r.Height;
  if aWidth > aHeight then begin
    yh := r.Width * aHeight div aWidth;
    r.Top := (yhr - yh) div 2;
    r.Height := yh;
  end
  else begin
    xh := r.Height * aWidth div aHeight;
    r.Left := (xhr - xh) div 2;
    r.Width := xh;
  end;
end;


{$R *.dfm}

{ TFramePsdFileImage }

constructor TFramePsdFileImage.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 10;
  FTimer.OnTimer := OnTimer;
  scrBox.VertScrollBar.Tracking := True;
  scrBox.HorzScrollBar.Tracking := True;
end;

destructor TFramePsdFileImage.Destroy;
begin
  while Drawing do begin                        // スレッド終了まで待機
    Application.ProcessMessages;
    Sleep(10);
  end;

  FTimer.Free;
  inherited;
end;


procedure TFramePsdFileImage.DoRectChange;
begin
  if Assigned(FOnRectChange) then begin
    FOnRectChange(Self);
  end;
end;

procedure TFramePsdFileImage.FrameResize(Sender: TObject);
begin
  ProcResize();
  if FShowed then ShowImage
end;

function TFramePsdFileImage.GetScale: Double;
begin
  result := TBL_SCALE[FScaleIndex];
end;

procedure TFramePsdFileImage.ImagePsdMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit;

  FMouseDown := True;
  FMouseX    := X;
  FMouseY    := Y;
end;

procedure TFramePsdFileImage.ImagePsdMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin

  if not FMouseDown then exit;

  ProcMove(X,Y);

  DoRectChange();
end;

procedure TFramePsdFileImage.ImagePsdMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;

// 描画スレッド完了イベント
procedure TFramePsdFileImage.OnDrawFinish(Sender: TObject);
var
  cv : TCanvas;
  r : TRect;
begin
  r := Rect(0,0,ImagePsd.Width,ImagePsd.Height);
  cv := ImagePsd.Canvas;
  cv.Brush.Color := clWhite;
  cv.Brush.Style := bsSolid;
  cv.FillRect(r);
  RectToStreachRect(r,FPsdImage.Bitmap.Width,FPsdImage.Bitmap.Height);
  ImagePsd.Canvas.StretchDraw(r,FPsdImage.Bitmap);
  FDrawing := False;
end;

// 描画遅延タイマー　描画中なら彩度タイマーで待つ
procedure TFramePsdFileImage.OnTimer(Sender: TObject);
var
  r : TRect;
begin
  FTimer.Enabled := False;
  if FDrawing then begin
    FTimer.Enabled := True;
    exit;
  end;
  FDrawing := true;
  r := Rect(0,0,ImagePsd.Width,ImagePsd.Height);
end;


procedure TFramePsdFileImage.ProcBig;
var
  xh,yh,z1,z2 : Double;
  x,y : Integer;
begin
  z1 := TBL_SCALE[FScaleIndex];
  if FScaleIndex+1 > High(TBL_SCALE) then exit;
  Inc(FScaleIndex);
  z2 := TBL_SCALE[FScaleIndex];
  xh := scrBox.ClientWidth  * TBL_SCALE[FScaleIndex];
  yh := scrBox.ClientHeight  * TBL_SCALE[FScaleIndex];

  ImagePsd.Width := Round(xh);
  ImagePsd.Height := Round(yh);

  x := scrBox.HorzScrollBar.Position;
  y := scrBox.VertScrollBar.Position;

  x := Round(x * z2 / z1);
  y := Round(y * z2 / z1);

  scrBox.VertScrollBar.Position := y;
  scrBox.HorzScrollBar.Position := x;

  ReDraw();
end;

procedure TFramePsdFileImage.ProcSmall;
var
  xh,yh : Double;
begin
  if FScaleIndex <= 0 then exit;
  Dec(FScaleIndex);

  xh := scrBox.ClientWidth  * TBL_SCALE[FScaleIndex];
  yh := scrBox.ClientHeight  * TBL_SCALE[FScaleIndex];

  ImagePsd.Width := Round(xh);
  ImagePsd.Height := Round(yh);
  ReDraw();

  scrBox.VertScrollBar.Position := ImagePsd.Height div 4;
  scrBox.HorzScrollBar.Position := ImagePsd.Width div 4;

end;


procedure TFramePsdFileImage.ProcMove(const X, Y: Integer);
var
  sx,sy,xh,yh : Integer;
begin

  xh := X - FMouseX;
  yh := Y - FMouseY;

  sx :=  scrBox.HorzScrollBar.Position;
  sy :=  scrBox.VertScrollBar.Position;

  sx := sx - xh;
  sy := sy - yh;

  scrBox.HorzScrollBar.Position := sx;
  scrBox.VertScrollBar.Position := sy;

end;

procedure TFramePsdFileImage.ProcResize;
begin
  ImagePsd.Width := ClientWidth - 16;
  ImagePsd.Height := ClientHeight - 16;
  ImagePsd.Picture.Bitmap.Width := ImagePsd.Width;
  ImagePsd.Picture.Bitmap.Height := ImagePsd.Height;
end;

procedure TFramePsdFileImage.ReDraw;
var
  cv : TCanvas;
  r : TRect;
begin
  r := Rect(0,0,ImagePsd.Width,ImagePsd.Height);
  ImagePsd.Picture.Bitmap.Width := ImagePsd.Width;
  ImagePsd.Picture.Bitmap.Height := ImagePsd.Height;
  cv := ImagePsd.Canvas;
  cv.Brush.Color := clWhite;
  cv.Brush.Style := bsSolid;
  cv.FillRect(r);
  RectToStreachRect(r,FPsdImage.Bitmap.Width,FPsdImage.Bitmap.Height);
  ImagePsd.Canvas.StretchDraw(r,FPsdImage.Bitmap);
  DoRectChange();
end;



procedure TFramePsdFileImage.Show;
begin
  ImagePsd.Left:= 0;
  ImagePsd.Top := 0;
  ImagePsd.Width := scrBox.ClientWidth;
  ImagePsd.Height := scrBox.ClientHeight;
end;


procedure TFramePsdFileImage.ShowImage;
begin
  FShowed := True;
  OnTimer(Self);              // 直にタイマー処理を呼び出す
  OnDrawFinish(Self);
end;

procedure TFramePsdFileImage.WMMousewheel(var Msg: TMessage);
var
  d : Integer;
begin
  d := shortint(HiWord(Msg.wParam));
  if (d > 0) then begin                // ホイールを奥
    ProcBig();
  end
  else begin                           // ホイール手前
    ProcSmall();
  end;
end;

end.
