unit PSDImageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,PSDImage,
  PsdFileImageFrame,PsdFileTreeFrame;

type
  TFormPSDImage = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    PanelImage: TPanel;
    PanelTree: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private 宣言 }
    FPSDImage : TPsdImage;
    FFrameImage : TFramePsdFileImage;
    FFrameTree : TFramePsdFileTree;
    // ツリーで表情変更時のイベント
    procedure OnTreeClick(Sender: TObject);
  public
    { Public 宣言 }
  end;

var
  FormPSDImage: TFormPSDImage;

implementation

var
  GPerfCounter: array[0..9] of Int64;


procedure StopTimStart(const Index: Integer);
begin
  if (Index < Low(GPerfCounter)) or (Index > High(GPerfCounter)) then
    Exit;
  QueryPerformanceCounter(GPerfCounter[Index]);
end;

procedure StopTimStop(const Index: Integer; const Tag: string);
var
  StopCounter, Elapsed: Int64;
  Ms: Double;
  Msg: string;
  Freq: Int64;
begin
  if (Index < Low(GPerfCounter)) or (Index > High(GPerfCounter)) then
    Exit;
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(StopCounter);
  Elapsed := StopCounter - GPerfCounter[Index];
  Ms := (Elapsed / Freq) * 1000.0;
  Msg := Format('StopTim[%d] %s = %.3f ms', [Index, Tag, Ms]);
  OutputDebugString(PChar(Msg));
end;

{$R *.dfm}

procedure TFormPSDImage.FormCreate(Sender: TObject);
begin
  FPSDImage := TPSDImage.Create;

  FFrameImage := TFramePsdFileImage.Create(Self);
  FFrameImage.Parent := PanelImage;
  FFrameImage.Align := alClient;
  FFrameImage.PsdImage := FPSDImage;

  FFrameTree := TFramePsdFileTree.Create(Self);
  FFrameTree.Parent := PanelTree;
  FFrameTree.Align := alClient;
  FFrameTree.PsdImage := FPSDImage;
  FFrameTree.OnTreeClick := OnTreeClick;
end;

procedure TFormPSDImage.FormDestroy(Sender: TObject);
begin
  FFrameTree.Free;
  FFrameImage.Free;
  FPSDImage.Free;
end;


procedure TFormPSDImage.OnTreeClick(Sender: TObject);
begin
  FFrameImage.ShowImage;
end;

procedure TFormPSDImage.Button1Click(Sender: TObject);
begin
  // ファイル選択ダイアログを開く
  OpenDialog1.Filter := 'PSD files (*.psd)|*.psd';
  OpenDialog1.Title := 'PSDファイルを選択';
  if not OpenDialog1.Execute then
    Exit;

  // PSD読み込みクラスを生成
  // ファイルを読み込み
  StopTimStart(0);
  FPSDImage.LoadFromFile(OpenDialog1.FileName);
  StopTimStop(0,'Load');

  StopTimStart(0);
  FFrameTree.ShowTree;
  StopTimStop(0,'Tree');

  // 表示先の TImage に転送
  StopTimStart(0);
  FFrameImage.ShowImage;
  StopTimStop(0,'View');

  // フォームのタイトルにファイル名を表示
  Caption := Format('PSD表示テスト - %s', [ExtractFileName(OpenDialog1.FileName)]);
end;

end.
