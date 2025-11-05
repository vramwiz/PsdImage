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
  FPSDImage.LoadFromFile(OpenDialog1.FileName);

  FFrameTree.ShowTree;
  // 表示先の TImage に転送
  FFrameImage.ShowImage;

  // フォームのタイトルにファイル名を表示
  Caption := Format('PSD表示テスト - %s', [ExtractFileName(OpenDialog1.FileName)]);
end;

end.
