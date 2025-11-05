unit PsdFileTreeFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ImgList, Vcl.ComCtrls,PsdImage, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.ImageList;

//--------------------------------------------------------------------------//
//  レイヤー表示非表示と反転のデータをリスト管理するクラス                  //
//--------------------------------------------------------------------------//
type
	TPsdFileTreeFrameExpands = class(TList)
	private
		{ Private 宣言 }
	public
		{ Public 宣言 }
    function IsExpand(tss : TPsdFileTrees) : Boolean;
    procedure AddExpand(tss : TPsdFileTrees);
	end;

  // スレッドを管理するリストがあれば複数スレッド可能だが必要かどうかは保留
type
  TPsdFileTreeFrameThread2 = class(TThread)
  private
    { Private 宣言 }
    FTreeView : TTreeView;          // 描画ツリービュー
    FNode     : TTreeNode;          // リストを追加するノード
    FLayer    : TPsdFileLayer;      // 処理中のレイヤー
    FExpands  : TPsdFileTreeFrameExpands;     // 展開したツリーを記憶

    FTrees    : TPsdFileTrees;      // 処理中のパーツツリーリスト
    FTree     : TPsdFileTree;       // 処理中のツリー
    FOnEvent  : TNotifyEvent;       // 終了時に発生させるイベント
    //FTrees    : TPsdFileTrees;
    function IsDrawLayer(dl : TPsdFileLayer) : Boolean;
    // 空白画像を登録
    procedure AddImageNil();
    procedure Addtree();
    // レイヤーの画像をイメージリストに追加
    procedure DrawLayer(dl : TPsdFileLayer;tn : TTreeNode);
  protected
    procedure Execute();override;
  public
    { Public 宣言 }
    constructor Create(tv : TTreeView;aNode: TTreeNode;aExpands  : TPsdFileTreeFrameExpands;aOn : TNotifyEvent);
    destructor Destroy;override;

  end;


// 表情ツリーフレーム
type
  TFramePsdFileTree = class(TFrame)
    tvPsd: TTreeView;
    ImagePsd: TImageList;
    ImageState: TImageList;
    procedure tvPsdExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvPsdMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvPsdCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvPsdExpanded(Sender: TObject; Node: TTreeNode);
  private
    { Private 宣言 }
    FPsdFile      :  TPsdImage;                // PSD解析クラス
    //FThread       : TPsdFileTreeFrameThread;  // スレッドクラス
    FExpanding    : Boolean;
    FSelectTrees  : TPsdFileTrees;
    FSelectTree   : TPsdFileTree;
    FExpands      : TPsdFileTreeFrameExpands;    // 展開したツリーを記憶
    FThreaded     : Boolean;
    FThread2       : TPsdFileTreeFrameThread2;  // スレッドクラス
    FOnTreeClick  : TNotifyEvent;

    // チェック状態を示す画像を作成して登録
    procedure DrawCheck();
    // 非表示状態にしたときは、その下の階層のデータも非表示に
    procedure TreeNodeHide(tss : TPsdFileTrees);

    procedure TreeReView();
    procedure TreeReViewSub(tn : TTreeNode);

    function GetSelectTreeOwner: TPsdFileTree;

    procedure OnSelfThreadFinish(Sender: TObject);
    //property OnThreadFinish : TNotifyEvent read FOnThreadFinish write FOnThreadFinish;
  protected
    procedure DoTreeClick();
  public
    { Public 宣言 }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure ShowTree();
    // チェックのみ更新
    procedure ShowTreeCheck();

    function IsThread() : Boolean;

    property PsdImage : TPsdImage read FPsdFile write FPsdFile;
    property SelectTrees  : TPsdFileTrees read FSelectTrees;
    // 選択中のツリーを取得
    property SelectTree  : TPsdFileTree read FSelectTree;
    // 選択中のツリーの1つ上のツリーを取得
    property SelectTreeOwner : TPsdFileTree read GetSelectTreeOwner;
    property OnTreeClick : TNotifyEvent read FOnTreeClick write FOnTreeClick;
  end;

implementation

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


//uses ExplorerVImageConvert;

{$R *.dfm}

{ TPsdFileTreeFrame }

constructor TFramePsdFileTree.Create(AOwner: TComponent);
//var
//  WindowStyles: Integer;
begin
  inherited;
  FExpands := TPsdFileTreeFrameExpands.Create;
  tvPsd.DoubleBuffered := True;
  //OnThreadFinish := OnSelfThreadFinish;
end;

destructor TFramePsdFileTree.Destroy;
begin
  if FThread2 <> nil then begin
    FThread2.Terminate;
    while IsThread do begin                     // スレッド終了まで待機
      Application.ProcessMessages;
      Sleep(10);
    end;
  end;
  FExpands.Free;
  inherited;
end;


procedure TFramePsdFileTree.DoTreeClick;
begin
  if Assigned(FOnTreeClick) then begin
    FOnTreeClick(Self);
  end;
end;

procedure TFramePsdFileTree.DrawCheck;
var
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(ImageState.Width,ImageState.Height);
    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Font.Name := 'MS UI Gothic';                 // 同じ大きさになるフォントを指定
    bmp.Canvas.Font.Height := -13;                            // 0 は無を表すので1: OFF 2:ONとする
    bmp.Canvas.TextOut(2,2,'○');                          // OFFの白丸を描画
    ImageState.Add(bmp,nil);                                // 無として登録 ※本当に無の方が良いかも
    ImageState.Add(bmp,nil);                                // OFFとして登録
    bmp.SetSize(ImagePsd.Width,ImagePsd.Height);            // ビットマップ初期化・・・できてないかも
    bmp.Canvas.Font.Height := -13;                            // 0 は無を表すので1: OFF 2:ONとする
    bmp.Canvas.TextOut(2,2,'●');                          // ONの黒丸を描画
    ImageState.Add(bmp,nil);                                // ONとして登録
  finally
    bmp.Free;
  end;
end;

function TFramePsdFileTree.GetSelectTreeOwner: TPsdFileTree;
var
  ts : TPsdFileTree;
  i : Integer;
begin
  result := nil;
  ts := FSelectTree;
  if ts = nil then exit;

  i := ts.Owners.Count;
  if i = 0 then exit;
  result := TPsdFileTree(ts.Owners[i-1]);

end;


function TFramePsdFileTree.IsThread: Boolean;
begin
  //result := FThreaded;
  //exit;
  result := False;
  if FThread2 = nil then exit;                   // スレッドクラスが存在している場合
  //FThread2.Terminate;
  //if not FThread2.Finished then exit;         // スレッド実行中の場合
  result := FThreaded;
  //result := True;
end;

procedure TFramePsdFileTree.OnSelfThreadFinish(Sender: TObject);
begin
  FThreaded := False;
end;

procedure TFramePsdFileTree.TreeNodeHide(tss: TPsdFileTrees);
var
  i : Integer;
  ts : TPsdFileTree;
  dn : TTreeNode;
begin
  for i := 0 to tss.Count-1 do begin
    ts := tss[i];
    dn := TTreeNode(ts.Node);
    if dn = nil then exit;                        // 展開前のツリーの場合は処理しない

    ts.Visible := False;
    dn.StateIndex := Integer(ts.Visible)+1;
    TreeNodeHide(TPsdFileTrees(ts.Trees));        // 下の階層も非表示に
  end;
end;


procedure TFramePsdFileTree.TreeReView;
var
  i: Integer;
  tn : TTreeNode;
  ts : TPsdFileTree;
begin
  for i := 0 to tvPsd.Items.Count-1 do begin
    tn := tvPsd.Items[i];
    ts := TPsdFileTree(tn.Data);
    tn.StateIndex := Integer(ts.Visible)+1;   // 表示に反映
    TreeReViewSub(tn);
  end;

end;

procedure TFramePsdFileTree.TreeReViewSub(tn: TTreeNode);
var
  i: Integer;
  tn2 : TTreeNode;
  ts : TPsdFileTree;
begin
  for i := 0 to tn.Count-1 do begin
    tn2 := tn.Item[i];
    ts := TPsdFileTree(tn2.Data);
    tn2.StateIndex := Integer(ts.Visible)+1;   // 表示に反映
  end;

end;

procedure TFramePsdFileTree.ShowTree;
var
  i : Integer;
  ts : TPsdFileTree;
  tv : TTreeView;
  tn : TTreeNode;
  dl : TPsdFileLayer;
begin
  //tvPsd.Hint := MemoHelp.Lines.Text;
  //tvPsd.ShowHint := True;

  tvPsd.Images.Clear;
  FExpands.Clear;
  DrawCheck();
  tv := tvPsd;
  tv.Items.Clear;
  for i := 0 to FPsdFile.Trees.Count-1 do begin
    ts := FPsdFile.Trees[i];
    dl := ts.Layer;
    tn := tv.Items.AddChildObject(nil,dl.Name,ts);
    tn.StateIndex := Integer(ts.Visible) + 1;
    tn.HasChildren := ts.IsChildren;
  end;

  if tv.Items.Count>0 then begin
    tn := tv.Items[0];
    tn.Expand(True);
  end;
end;

procedure TFramePsdFileTree.ShowTreeCheck;
begin
  TreeReView();
end;

procedure TFramePsdFileTree.tvPsdCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  FExpanding := True;                          // ツリーを閉じた後のクリックを無効にする
end;


procedure TFramePsdFileTree.tvPsdExpanded(Sender: TObject; Node: TTreeNode);
begin
  //TPsdFileTreeFrameThread2.Create(tvPsd,Node);
end;

procedure TFramePsdFileTree.tvPsdExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  dl : TPsdFileLayer;
  tss : TPsdFileTrees;
  ts : TPsdFileTree;
begin
  FExpanding := True;                          // 展開後のクリックを無効にする
  if FThread2<>nil then begin                   // スレッドクラスが存在している場合
    if not FThread2.Finished then begin         // スレッド実行中の場合
      //AllowExpansion := False;
      exit;                                    // 処理中断
    end;
  end;

  ts := TPsdFileTree(Node.Data);               // ノードに割り当てられたツリー情報を参照
  tss := TPsdFileTrees(ts.Trees);              // そのツリーの子ツリーを参照
  if tss = nil then exit;                      // 存在しない場合は処理しない

  if FExpands.IsExpand(tss) then begin         // 一度展開済みのツリーの場合
    AllowExpansion := True;                      // 展開可能と判断
    exit;
  end;
  //FExpands.AddExpand(FTrees);                              // 展開済みとする
  //if tss.Expanded then exit;                   // 一度展開済みのツリーの場合は描画しない
  AllowExpansion := True;                      // 展開可能と判断
  dl := ts.Layer;

  tvPsd.Items.AddChildObject(Node,dl.Name,ts);   // 子ツリーに追加
  FThreaded := True;
  FThread2 := TPsdFileTreeFrameThread2.Create(tvPsd,Node,FExpands,OnSelfThreadFinish);
end;


procedure TFramePsdFileTree.tvPsdMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ts : TPsdFileTree;
  tss : TPsdFileTrees;
  Node : TTreeNode;
begin

  Node :=  tvPsd.GetNodeAt(X,Y);         // カーソル上のノードを取得
  if Node = nil then exit;               // なければ未処理
  ts := TPsdFileTree(Node.Data);         // ツリークラスを参照

  tss := TPsdFileTrees(ts.Trees);        // 子ツリー一覧を参照

  FSelectTrees := tss;                   // 選択中のツリーとする
  FSelectTree  := ts;


  if FExpanding then begin               // ツリーの展開や閉じるの場合
    FExpanding := False;                 // 展開フラグをリセット
    exit;                                // 処理終了
  end;

  if Button <> mbLeft then exit;         // マウスの左クリックでは無い場合は未処理

  ts.Visible := not ts.Visible;          // 該当するデータの表示状態を反転
  TreeReView();                          // ツリー表示のチェックを再描画

  DoTreeClick();                         // クリックイベントを発生させる
end;

{ TPsdFileTreeFrameThread2 }

constructor TPsdFileTreeFrameThread2.Create(tv: TTreeView;aNode: TTreeNode;aExpands  : TPsdFileTreeFrameExpands;aOn : TNotifyEvent);
begin
  inherited create(False);
  FExpands := aExpands;
  FreeOnTerminate := False;
  FTreeView := tv;
  FNode := aNode;
  FOnEvent  := aOn;
  //FTrees := tss;
end;

destructor TPsdFileTreeFrameThread2.Destroy;
begin

  inherited;
end;

procedure TPsdFileTreeFrameThread2.DrawLayer(dl: TPsdFileLayer; tn: TTreeNode);
var
  r  : TRect;
  bmpFrom,bmpTo : TBitmap;
begin


  bmpFrom := TBitmap.Create;                               // PSD用ビットマップ生成
  bmpTo := TBitmap.Create;                                 // ツリー表示用ビットマップ生成
  bmpTo.SetSize(FTreeView.Images.Width,FTreeView.Images.Height);         // ツリー表示用イメージリスト合わせる

  try
    dl.ImageToBitmap(bmpFrom);
    r := Rect(0,0,FTreeView.Images.Width,FTreeView.Images.Height);
    RectToStreachRect(r,bmpFrom.Width,bmpFrom.Height);     // 描画先のアスペクト比を合わせる
    bmpTo.Canvas.StretchDraw(r,bmpFrom);                   // 拡大縮小して描画

    FTreeView.Images.Add(bmpTo,nil);                       // 画像を登録
  finally
    bmpTo.Free;
    bmpFrom.Free;
  end;
end;

procedure TPsdFileTreeFrameThread2.Execute;
var
    tn : TTreeNode;
  i : Integer;
begin
  FTree := TPsdFileTree(FNode.Data);               // ノードに割り当てられたツリー情報を参照
  FTrees := TPsdFileTrees(FTree.Trees);              // そのツリーの子ツリーを参照
  if FTrees = nil then exit;                      // 存在しない場合は処理しない
  if FExpands.IsExpand(FTrees) then exit;                  // 一度展開済みのツリーの場合は描画しない
  FExpands.AddExpand(FTrees);                              // 展開済みとする

  {
  if FTrees.Expanded then exit;                   // 一度展開済みのツリーの場合は描画しない
  FTrees.Expanded := True;                        // 展開済みとする
  }
  if FNode.Count>0 then begin
    tn := FNode.Item[0];
    FTreeView.Items.Delete(tn);
  end;
  for i := 0 to FTrees.Count-1 do begin
    if Terminated then break;
    if FTreeView = nil then break;

    FTree := FTrees[i];
    FLayer := FTree.Layer;
    FTreeView.Items.BeginUpdate();
    Synchronize(Addtree);
    FTreeView.Items.EndUpdate();
  end;
  if Assigned(FOnEvent) then begin             // スレッド終了イベントを発生させる
    FOnEvent(Self);
  end;
end;

procedure TPsdFileTreeFrameThread2.Addtree;
var
    tn : TTreeNode;
begin
  if Terminated then exit;
  if FTreeView = nil then exit;
  tn := FTreeView.Items.AddChildObject(FNode,FLayer.Name,FTree);   // 子ツリーに追加
  FTree.Node := tn;
  tn.HasChildren := FTree.IsChildren;
  if not IsDrawLayer(FLayer) then begin
    AddImageNil();
  end
  else begin
    DrawLayer(FLayer,tn);
  end;
  tn.ImageIndex := FTreeView.Images.Count-1;           // ダミー画像をツリー用画像として参照
  tn.SelectedIndex := FTreeView.Images.Count-1;        // 選択画像も同じとする
  tn.StateIndex := Integer(FTree.Visible) + 1;            // 状態表示用の画像を初期値に
end;



procedure TPsdFileTreeFrameThread2.AddImageNil;
var
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(FTreeView.Images.Width,FTreeView.Images.Height);
    FTreeView.Images.Add(bmp,nil);
  finally
    bmp.Free;
  end;
end;



function TPsdFileTreeFrameThread2.IsDrawLayer(dl: TPsdFileLayer): Boolean;
begin
  result := False;
  if dl.LayerType<>0 then exit;
  if dl.Channels.Width = 0 then exit;
  result := True;
end;

{ TPsdFileTreeFrameExpands }

procedure TPsdFileTreeFrameExpands.AddExpand(tss: TPsdFileTrees);
begin
  Add(tss);
end;

function TPsdFileTreeFrameExpands.IsExpand(tss: TPsdFileTrees): Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to Count-1 do begin
    if Items[i] <> tss then continue;
    result := True;
    exit;
  end;
end;

end.
