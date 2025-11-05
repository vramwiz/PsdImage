unit PSDImageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,PSDImage,DebugStopwatch,
  PsdFileImageFrame;

type
  TFormPSDImage = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private 宣言 }
    FPSDImage : TPsdImage;
    FFrameImage : TFramePsdFileImage;
  public
    { Public 宣言 }
  end;

var
  FormPSDImage: TFormPSDImage;

implementation

{$R *.dfm}

procedure TFormPSDImage.FormCreate(Sender: TObject);
begin
  FPSDImage := TPSDImage.Create;

  FFrameImage := TFramePsdFileImage.Create(Self);
  FFrameImage.Parent := Self;
  FFrameImage.Align := alClient;
  FFrameImage.PsdImage := FPSDImage;
end;

procedure TFormPSDImage.FormDestroy(Sender: TObject);
begin
  FFrameImage.Free;
  FPSDImage.Free;
end;


procedure TFormPSDImage.Button1Click(Sender: TObject);
begin
  // ファイル選択ダイアログを開く
  OpenDialog1.Filter := 'PSD files (*.psd)|*.psd';
  OpenDialog1.Title := 'PSDファイルを選択';
  if not OpenDialog1.Execute then
    Exit;

  // PSD読み込みクラスを生成
  StopTimStart(0);
  // ファイルを読み込み
  FPSDImage.LoadFromFile(OpenDialog1.FileName);
  StopTimStop(0, 'Load');

  StopTimStart(0);
  // 表示先の TImage に転送
  FFrameImage.ShowImage;
  StopTimStop(0, 'Draw');

  // フォームのタイトルにファイル名を表示
  Caption := Format('PSD表示テスト - %s', [ExtractFileName(OpenDialog1.FileName)]);
end;

end.
