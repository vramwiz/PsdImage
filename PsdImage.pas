//------------------------------------------------------------------------------
//  クラス名 : TPSDImage
//------------------------------------------------------------------------------
//  【概要】
//    Photoshop(PSD) ファイルを読み込み、内部構造（レイヤー階層・リソース情報）
//    を解析して VCL 用の TBitmap にレンダリングするクラスです。
//    各レイヤーは TPsdFileLayer/Trees/Layers クラスを通じて管理され、
//    Render() により合成画像を生成します。
//
//  【主な役割】
//    ・PSDファイルヘッダの読み込みと基本情報の保持
//    ・カラーモード、チャネル構成、レイヤー情報の解析
//    ・レイヤー階層(TPsdFileTrees)の構築
//    ・各レイヤーの描画順に基づく合成処理(Render)
//    ・VCL向けの全体画像(TBitmap)生成
//
//  【内部構成】
//    FLayerRoot   : 仮想的な最上位レイヤー（PSDTool互換構造用）
//    FLayers      : レイヤー管理クラス（全レイヤーを一元管理）
//    FTrees       : レイヤー階層ツリー構造
//    FBitmap      : 合成後の最終出力ビットマップ
//    FResource    : PSD内の追加リソース情報（ICCプロファイル等）
//
//  【主なメソッド】
//    LoadFromFile()       : PSDファイルを読み込んで内部構造を解析
//    Render()             : 各レイヤーをα合成して最終ビットマップを生成
//    Invalidate()         : 再描画を要求（Renderを再実行）
//    VisibleInit()        : レイヤーの表示・非表示を初期化
//    GetBitmap()          : 合成済みのTBitmapを取得
//    GetBitmapThumbnail() : 縮小サムネイルを取得
//
//  【ファイル構造解析】
//    LoadFromFileHeader()     : ファイルヘッダおよび基本情報を読み込み
//    LoadFromFileColorMode()  : カラーモードデータを解析
//    LoadFromFileLayerInfo()  : レイヤー情報・チャネルデータを展開
//    LoadFromFileTree()       : フォルダ構造(グループ階層)を構築
//    LoadFromFileAnm()        : ANM連携用データ割り当て
//
//  【描画動作】
//    RenderSub() 内でレイヤーツリーを走査し、各レイヤーのブレンドモード
//    に従って FBitmap に対して順次合成を行います。
//    現在のVCL描画実装では αブレンドは疑似的な処理であり、
//    PNG保存時には完全な透明情報を維持します。
//
//  【注意点】
//    ・FBitmap は pf32bit + AlphaFormat=afDefined で管理されます。
//    ・VCL上では透明は白背景で表示されますが、保存時にαは保持されます。
//    ・Render() 実行後に FBitmap を直接操作する場合、ロック処理に注意してください。
//------------------------------------------------------------------------------
unit PSDImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,Jpeg,PngImage,System.StrUtils;

type TPsdFileBlandType = (btNil,   // ブレンドモードを示す物では無い
                          btPass,  //   パススルー、
                          btNorm,  //  通常、
                          btDiss,  //  ディゾルブ、
                          btDark,  //  暗く、
                          btMul,   //  乗算、
                          btIdiv,  //  カラーバーン、
                          btLbrn,  //  リニアバーン、
                          btDkCl,  //  ダークcolor、
                          btLite,  //  lighten、
                          btScrn,  //  screen、
                          btDiv,   //  color dodge
                          btLddg,  //  linear dodge、
                          btLgCl,  //  lighter color、
                          btOver,  //  overlay、
                          btSLit,  //  soft light、
                          btHLit,  //  ハードライト、
                          btVLit,  //  ビビッドライト、
                          btLLit,  //  リニアライト、
                          btPLit,  //  ピンライト、
                          btHMix,  //  ハードミックス、
                          btDiff,  //  差、
                          btSmud,  //  除外、
                          btFsub,  //  減算、
                          btFdiv,  //  除算
                          btHue,   //  色相、
                          btSat,   //  飽和、
                          btCol,   //  色、
                          btLum    //  明るさ、
                          );


const BLEND_KEY: array[0..27] of AnsiString = ('pass','norm','diss','dark','mul ',
                                               'idiv','lbrn','dkCl','lite','scrn',
                                               'div ','lddg','lgCl','over','sLit',
                                               'hLit','vLit','lLit','pLit','hMix',
                                               'diff','smud','fsub','fdiv','hue ',
                                               'sat ','col ','lum ');

type TPsdFileColorType = (ctNil,   // チャンネル毎の色モード
                          ctR,     // 赤
                          ctG,     // 緑
                          ctB,     // 青
                          ctAlpha, // アルファチャンネル透明度 0:不透明 255:透明
                          ctMask   // マスク
                          );

type  TFourth = packed record
    B,G,R,A : Byte;
  end;
TFourthArray = array[0..40000000] of TFourth;
PFourthArray = ^TFourthArray;


type  TTriple = packed record
    B,G,R : Byte;
  end;
TTripleArray = array[0..40000000] of TTriple;
PTripleArray = ^TTripleArray;

//--------------------------------------------------------------------------//
//  高速バッファ型ファイルストリームクラス（完全メモリ展開版）             //
//  既存の TFileStreamBuf と同名・同I/F互換で動作                        //
//--------------------------------------------------------------------------//
type
  TFileStreamBuf = class(TPersistent)
  private
    FBuffer: PByte;          // ファイル全体のメモリバッファ
    FSize: Integer;          // バッファ全体サイズ
    FPos: Integer;           // 現在の読み取り位置
    FFileName: string;       // デバッグ・確認用に保持
  public
    constructor Create(const AFileName: string); virtual;
    destructor Destroy; override;

    // 現在位置／サイズ取得
    function Position: Integer;
    function Size: Integer;

    // ポジション制御
    procedure Seek(NewPos: Integer);
    procedure Skip(Count: Integer);

    // バイト・数値読み込み
    function ReadByte: Byte;
    function ReadBin(const Length: Integer): Integer;     // BigEndianで読む
    function ReadInt(const Length: Integer): Integer;     // LittleEndianで読む

    // 文字列系（既存と同名）
    function ReadStr(const Length: Integer): AnsiString;
    function ReadStrPascal(const Length: Integer): AnsiString;
    function ReadStrPascal2(const Length: Integer): AnsiString;
    function ReadStrUnicodeSizeLenData: string;

    // 読み捨て
    procedure ReadDumy(const Length: Integer);
  end;
type
  TPsdFileBitmap = class(TPersistent)
  private
    { Private 宣言 }
    FWidth: Integer;
    FHeight: Integer;
    FPixcel : array of array of TTriple;
  public
    { Public 宣言 }
    procedure SetSize(aWidth,aHeight : Integer);

    property Width : Integer read FWidth;
    property Height : Integer read FHeight;

  end;

type
  TPsdFileBlend = class(TPersistent)
  private
    { Private 宣言 }
    FSource: Integer;
    FAddress: Integer;
  public
    { Public 宣言 }
    function LoadFromStream(fs : TFileStreamBuf) : Boolean;

    property Source : Integer read FSource;
    property Address : Integer read FAddress;
  end;

type
	TPsdFileBlends = class(TList)
	private
		{ Private 宣言 }
    function GetItems(Index: Integer): TPsdFileBlend;
	public
		{ Public 宣言 }
    destructor Destroy;override;
    function Add() : TPsdFileBlend;
    procedure Delete(i : Integer);
    procedure Clear();override;

		property Blends[Index: Integer] : TPsdFileBlend read GetItems ;default;

	end;


type
  TPsdFileChannel = class(TPersistent)
  private
    { Private 宣言 }
    FImageLength  : Integer;       // チャンネルに該当する画像データサイズ
    FTop          : Integer;       // レイヤーマスクを囲む長方形：上、左、下、右
    FLeft         : Integer;
    FRight        : Integer;
    FBottom       : Integer;
    FDefaultColor : Integer;       // デフォルトの色。0または255
    FRectTop      : Integer;       //
    FRectLeft     : Integer;
    FFlag         : Integer;       // ビットごとに意味があるフラグ 0=相対位置
    FFlag2        : Integer;       // 上記と同じ内容が入ると思われる
    FMask         : Integer;       // マスク フラグのビット4が設定されている場合にのみ
    FMask2        : Integer;
    FRectRight    : Integer;
    FRectBottom   : Integer;
    FMode         : Integer;       // 0:R 1:G 2:B FFFF:αチャンネル FFFE:マスク？
    FImageAdr     : Integer;       // 画像格納アドレス Dataには画像データ長が入っている
    FImage        :  array of array of Byte;
    FColorType: TPsdFileColorType; // y * x のサイズを持つ画像データ管理 8bit専用

    function LoadBitmap(fs : TFileStreamBuf;Pos : Integer) : Boolean;
    // 非圧縮データを展開
    function LoadBitmapCompressionNon(fs : TFileStreamBuf) : Boolean;
    // RLE圧縮データを展開
    function LoadBitmapCompressionRle(fs : TFileStreamBuf) : Boolean;


    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetColorType: TPsdFileColorType;
  public
    { Public 宣言 }
    function LoadFromStream(fs : TFileStreamBuf) : Boolean;

    property Left   : Integer read FLeft;
    property Top    : Integer read FTop;
    property Right  : Integer read FRight;
    property Bottom : Integer read FBottom;
    property Height : Integer read GetHeight;
    property Width  : Integer read GetWidth;

    property DefaultColor : Integer read FDefaultColor;
    property Flag         : Integer read FFlag;
    property Flag2        : Integer read FFlag2;
    property Mask         : Integer read FMask;
    property Mask2        : Integer read FMask2;
    property RectLeft     : Integer read FRectLeft;
    property RectTop      : Integer read FRectTop;
    property RectRight    : Integer read FRectRight;
    property RectBottom   : Integer read FRectBottom;

   // property ImageAdr2 : Integer read FImageAdr2;
    property ImageAdr : Integer read FImageAdr;
    property ImageLength   : Integer read FImageLength;
    property Mode : Integer read FMode;
    property ColorType : TPsdFileColorType read FColorType;
  end;


type
	TPsdFileChannels = class(TList)
	private
		{ Private 宣言 }
    function GetItems(Index: Integer): TPsdFileChannel;
    function GetHeight: Integer;
    function GetWidth: Integer;
	public
		{ Public 宣言 }
    destructor Destroy;override;
    function Add() : TPsdFileChannel;
    procedure Delete(i : Integer);
    procedure Clear();override;

    function IndexOfColorType(const ct : TPsdFileColorType) : Integer;

		property Channels[Index: Integer] : TPsdFileChannel read GetItems ;default;

    property Height : Integer read GetHeight;
    property Width  : Integer read GetWidth;
	end;

type
  TPsdFileLayer = class(TPersistent)
  private
    { Private 宣言 }
    FChannelCount : Integer;
    FChannels     : TPsdFileChannels;
    FTransParent  : Integer;
    FClipping     : Integer;
    FFlag         : Integer;
    FBlends       : TPsdFileBlends;
    FBlendGlay    : TPsdFileBlend;
    FNameShifJis  : AnsiString;
    FNameUnicode  : string;
    FLayerType    : Integer;          // 0:表情 1,2:開く 3:閉じる
    FProtectFlag  : Integer;
    FOwner        : TObject;
    FBlendMode    : TPsdFileBlandType;
    FPng          : tPngimage;
    FAnmText      : string;
    FAnmGroup     : string;
    FAnmGroup2    : string;
    FTree         : TObject;

    function LoadFromStreamInfo(fs : TFileStreamBuf) : Boolean;
    function LoadFromStream(fs : TFileStreamBuf) : Boolean;
    function LoadFromStreamNorm(fs : TFileStreamBuf;mode : TPsdFileBlandType) : Integer;
    function LoadFromStreamLuni(fs : TFileStreamBuf) : Boolean;
    function LoadFromStreamLsct(fs : TFileStreamBuf) : Boolean;
    function LoadFromStreamLspf(fs : TFileStreamBuf) : Boolean;
    function LoadFromStreamEtc(fs : TFileStreamBuf) : Boolean;

    // チャンネルごとの画像データをビットマップに変換
    // toLinesが示すScanLineにaWidth aHeightの範囲内描画
    procedure Draw(const toLines : array of Pointer;const aWidth,aHeight,aLeft,aTop : Integer);
    //procedure Draw3(const toLines : TPsdFileBitmap;const aWidth,aHeight,aLeft,aTop : Integer);
    // 標準描画
    function DrawNorm(const fromCol,toCol : TFourth) : TFourth;
    function DrawMul(const fromCol,toCol : TFourth) : TFourth;

    function GetName: string;
    // PSDデータ内の識別文字を描画エフェクタ型に変換
    function GetBlandMode(const str : AnsiString) : TPsdFileBlandType;
    function GetVisible: Boolean;
    function GetTreeVisible: Boolean;
    procedure SetTreeVisible(const Value: Boolean);
  public
    { Public 宣言 }
    constructor Create();
    destructor Destroy;override;

    function ImageToBitmap(aBitmap : TBitmap) : Boolean;

    property ChannelCount : Integer read FChannelCount;
    property Channels : TPsdFileChannels read FChannels;
    property TransParent : Integer read FTransParent;
    property Clipping : Integer read FClipping;
    property Flag : Integer read FFlag;
    //property NextPos : Integer read FNextPos;
    property Blends : TPsdFileBlends read FBlends;
    property BlendGlay : TPsdFileBlend read FBlendGlay;
    // ShiftJIS UNICODE存在する名称データを返す
    property Name : string read GetName;
    property NameShifJis :AnsiString read FNameShifJis;
    property NameUnicode :string read FNameUnicode;
    property LayerType : Integer read FLayerType;
    property ProtectFlag : Integer read FProtectFlag;
    property Owner : TObject read FOwner;
    property BlendMode : TPsdFileBlandType read FBlendMode;
    property AnmGroup : string read FAnmGroup;
    property AnmGroup2 : string read FAnmGroup2;
    property AnmText  : string read FAnmText;
    property Visible : Boolean read GetVisible;
    // 表情ツリー上での表示／非表示を設定　※親ツリーまで影響が及ぶ
    property TreeVisible : Boolean read GetTreeVisible write SetTreeVisible;
  end;

//--------------------------------------------------------------------------//
//  PSDから取り出したレイヤー情報リストを管理するクラス                     //
//--------------------------------------------------------------------------//
type
	TPsdFileLayers = class(TList)
	private
		{ Private 宣言 }
    function GetLayers(Index: Integer): TPsdFileLayer;
	public
		{ Public 宣言 }
    destructor Destroy;override;

    function Add() : TPsdFileLayer;
    procedure Delete(i : Integer);
    procedure Clear();override;

    function IndexOfAnmText(const AnmText : string) : Integer;

		property Layers[Index: Integer] : TPsdFileLayer read GetLayers ;default;

	end;


//--------------------------------------------------------------------------//
//  レイヤー情報リストのポインタだけを管理するクラス                        //
//--------------------------------------------------------------------------//
type
	TPsdFileLayerExs = class(TList)
	private
		{ Private 宣言 }
    function GetLayers(Index: Integer): TPsdFileLayer;
	public
		{ Public 宣言 }

    procedure  Add(d : TPsdFileLayer);

		property Layers[Index: Integer] : TPsdFileLayer read GetLayers ;default;

	end;

type
  TPsdFileTree = class(TPersistent)
  private
    { Private 宣言 }
    FLevel: Integer;                      // 階層レベル　※まだ未対応
    FLayer: TPsdFileLayer;                // 対応するレイヤー
    FTrees: TObject;                      // その下にあるツリー
    //FOwner: TObject;                      // その上にあるツリー
    FNode : TObject;
    FVisible: Boolean;
    FImageIndex: Integer;
    FOwners: TList;
    function GetIsChildren: Boolean;
    function GetIsMultiSelect: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetIsAlways: Boolean;
  public
    { Public 宣言 }
    constructor Create();
    destructor Destroy;override;

    property IsChildren : Boolean read  GetIsChildren;

    property Trees : TObject read FTrees;
    //property Owner : TObject read FOwner;
    property Owners : TList read FOwners;
    property Layer : TPsdFileLayer read FLayer;
    property Level : Integer read FLevel;
    property Node  : TObject read FNode write FNode;
    property Visible : Boolean read FVisible write SetVisible;
    property ImageIndex : Integer read FImageIndex write FImageIndex;
     // True : 複数選択可能な要素
    property IsMultiSelect : Boolean read GetIsMultiSelect;
    //  True : 常にVisibleでなければならない
    property IsAlways      : Boolean read GetIsAlways;
  end;

//--------------------------------------------------------------------------//
//  レイヤー情報をツリー形式で管理するクラス                                //
//--------------------------------------------------------------------------//
type
	TPsdFileTrees = class(TList)
	private
		{ Private 宣言 }
    //FExpanded: Boolean;
    function GetTrees(Index: Integer): TPsdFileTree;
	public
		{ Public 宣言 }
    destructor Destroy;override;
    function Add() : TPsdFileTree;
    procedure Delete(i : Integer);
    procedure Clear();override;

    function IndexOfLayer(dl : TPsdFileLayer) : Integer;
    // レイヤークラスからそのレイヤーが持つ子レイヤーリストを取得
    //function GetLayerTree(dl : TPsdFileLayer) : TPsdFileTree;
    //function Execute(psd :  TPsdFile) : Boolean;

		property Trees[Index: Integer] : TPsdFileTree read GetTrees ;default;

    //property Expanded : Boolean read FExpanded write FExpanded;
	end;

//--------------------------------------------------------------------------//
//  リソースデータのサムネイル管理クラス                                    //
//--------------------------------------------------------------------------//
type
  TPsdFileResourceThumbnail = class(TPersistent)
  private
    { Private 宣言 }
    FImageFormat : Integer;   // フォーマット。1=kJpegRGB。kRawRGB（0）もサポートします。
    FWidth       : Integer;
    FHeight      : Integer;
    FWidthbytes  : Integer;   // パディングされた行のバイト=（幅*ピクセルあたりのビット数+ 31）/ 32*4。
    FWidthHeight : Integer;   //合計サイズ=widthbytes*高さ*平面
    FDataLength  : Integer;   // データ長（圧縮後）
    FPixcelBit   : Integer;
    FPlaneCount  : Integer;
    FBitmap      : TBitmap;
    function LoadFromFile(fs : TFileStreamBuf) : Boolean;
    // 画像データを読み込み
    function LoadFromFileData(fs : TFileStreamBuf) : Boolean;
    // Jpegデータを読み込み
    function LoadFromFileDataJpeg(fs : TFileStreamBuf) : Boolean;
  public
    { Public 宣言 }
    constructor Create();
    destructor Destroy;override;
    property Bitmap : TBitmap read FBitmap;
    property ImageFormat : Integer read FImageFormat;
    property Height : Integer read FHeight;
    property Width  : Integer read FWidth;
    property Widthbytes : Integer read FWidthbytes;
    property WidthHeight : Integer read FWidthHeight;
    property DataLength : Integer read FDataLength;
    property PixcelBit : Integer read FPixcelBit;
    property PlaneCount : Integer read FPlaneCount;
  end;

//--------------------------------------------------------------------------//
//  リソースデータ管理クラス                                                //
//--------------------------------------------------------------------------//
type TPsdFileResource = class(TPersistent)
  private
    { Private 宣言 }
    FThumbnail: TPsdFileResourceThumbnail;
    FSize: Integer;
    function LoadFromFile(fs : TFileStreamBuf) : Boolean;
  public
    { Public 宣言 }
    constructor Create();
    destructor Destroy;override;
    property Thumbnail : TPsdFileResourceThumbnail read FThumbnail;
    property Size : Integer read FSize;
  end;



//--------------------------------------------------------------------------//
//  PSDファイルを読み込む                                                   //
//--------------------------------------------------------------------------//
type TPSDImage = class(TPersistent)
  private
    { Private 宣言 }
    FLayerRoot    : TPsdFileLayer;        // PSDtool用に使う存在しない親レイヤークラス
    FLayers       : TPsdFileLayers;       // レイヤー管理クラス
    FTrees        : TPsdFileTrees;        // PSDツリークラス
    FBitmap       : TBitmap;              // 全体表示用ビットマップ
    FResource     : TPsdFileResource;
    // ------------------------------------------------------------------------
    FVersion    : Integer;             // バージョン
    FChannel    : Integer;             // アルファチャネルを含む、画像内のチャネル数
    FHeight     : Integer;             // 画像全体の高さ
    FWidth      : Integer;             // 画像全体の幅
    FDataBit    : Integer;             // チャネルあたりのビット数。値は1、8、16、および32です。
    FColorMode  : Integer;             // ファイルのカラーモード。サポートされている値は次のとおりです。ビットマップ=0; グレースケール=1; インデックス付き=2; RGB = 3; CMYK = 4; マルチチャネル=7; Duotone = 8; ラボ=9。
    // ------------------------------------------------------------------------
    FLayerMaskLength : Integer;
    FLayerLength : Integer;
    FLayerCount  : Integer;            // レイヤー数
    // ------------------------------------------------------------------------
    FBitmapLines : array of Pointer;
    FFileName    : string;             // データ読み込み時に使用するファイル名
    FRendered    : Boolean;            // True:レンダー済み

    function LoadFromFileHeader(fs : TFileStreamBuf) : Boolean;
    function LoadFromFileColorMode(fs : TFileStreamBuf) : Boolean;
    function LoadFromFileLayerInfo(fs : TFileStreamBuf) : Boolean;
    function LoadFromFileAnm() : Boolean; // ANMファイルの作成に必要な情報を各レイヤーに割り当てる
    function LoadFromFileTree() : Boolean;

    procedure RenderSub(tss : TPsdFileTrees);
    procedure BitmapClear();

    function GetBitmap: TBitmap;
    function GetBitmapThumbnail: TBitmap;
  protected
  public
    { Public 宣言 }
    constructor Create();
    destructor Destroy;override;

    // レイヤーの表示非表示情報を初期状態に
    procedure VisibleInit();
    // 合成を行う
    procedure Render;
    // 再描画が必要なときに明示的に呼ぶ
    procedure Invalidate;

    property Layers  : TPsdFileLayers read FLayers;
    property Trees  : TPsdFileTrees read FTrees;
    property BitmapThumbnail : TBitmap read GetBitmapThumbnail;

    procedure LoadFromFile(const FileName : string);
    property Bitmap: TBitmap read GetBitmap;
  end;

implementation

function StrLeft(const str,key : string) : string;
var
  i : Integer;
begin
  result := '';
  i := Pos(key,str);
  if i = 0 then exit;
  result := Copy(str,i+Length(key),Length(str));
end;

// 指定した文字列に囲まれた部分を返す
function StrMid(const str,keyLeft,keyRight : string) : string;
var
  i,j : Integer;
  s : string;
begin
  result := '';
  i := Pos(keyLeft,str);
  if i = 0 then exit;

  s := Copy(str,i+Length(keyLeft),Length(str));

  j := Pos(keyRight,s);
  if j = 0 then exit;
  result := Copy(s,1,j-1);

end;

function StrMidCut(var str : string;const keyLeft,keyRight : string) : string;
var
  i,j : Integer;
  s : string;
begin
  result := '';
  i := Pos(keyLeft,str);
  if i = 0 then exit;

  s := Copy(str,i+Length(keyLeft),Length(str));

  j := Pos(keyRight,s);
  if j = 0 then exit;
  result := Copy(s,1,j-1);

  j := Pos(keyRight,str);
  str := Copy(str,j + Length(keyRight),Length(str));
end;

// 指定した右側の文字列を探し、左はその最短位置を探して囲まれた部分を返す
function StrMidRight(const str,keyLeft,keyRight : string) : string;
var
  i,j : Integer;
  s,ss : string;
begin
  result := str;

  s := str;
  j := Pos(keyRight,s);
  if j = 0 then exit;

  repeat
    ss := StrMid(s,keyLeft,keyRight);
    if ss = '' then break;

    i := Pos(keyLeft,s);
    if i = 0 then break;

    s := Copy(s,(i+Length(keyLeft)),Length(s));
    result := ss;
  until (False);


end;

function StrCutLeft(var str : string;const key : string) : Boolean;
var
  i : Integer;
begin
  result := False;
  i := Pos(key,str);
  if i = 0 then exit;
  str := Copy(str,i+Length(key),Length(str));
  result := True;
end;



{ TPsdImage }

procedure TPsdImage.BitmapClear;
var
  cv  : TCanvas;
begin
  FBitmap.AlphaFormat := afDefined;
  cv := FBitmap.Canvas;
  cv.Brush.Style := bsSolid;
  cv.FillRect(Rect(0,0,FBitmap.Width,FBitmap.Height));
end;

constructor TPsdImage.Create;
begin
  FLayerRoot   := TPsdFileLayer.Create;
  FLayers := TPsdFileLayers.Create;
  FTrees := TPsdFileTrees.Create;
  FBitmap := TBitmap.Create;
  FResource := TPsdFileResource.Create;

end;

destructor TPsdImage.Destroy;
begin
  FLayerRoot   := TPsdFileLayer.Create;
  FLayers := TPsdFileLayers.Create;
  FTrees := TPsdFileTrees.Create;
  FBitmap := TBitmap.Create;
  FResource := TPsdFileResource.Create;

  inherited;
end;

procedure TPsdImage.RenderSub(tss: TPsdFileTrees);
var
  i : Integer;
  ts : TPsdFileTree;
  dl : TPsdFileLayer;
begin
  for i := tss.Count-1 downto 0 do begin
    ts := tss[i];
    if not ts.Visible then continue;
    RenderSub(TPsdFileTrees(ts.Trees));

    dl := ts.FLayer;
    if dl.LayerType<>0 then continue;
    //dl.LoadFromBitmap();
    dl.Draw(FBitmapLines,FWidth,FHeight,dl.Channels[0].Left,dl.Channels[0].Top);
  end;
end;

function TPsdImage.GetBitmap: TBitmap;
begin
  if not FRendered then Render;
  Result := FBitmap;
end;

function TPsdImage.GetBitmapThumbnail: TBitmap;
begin
  result := FResource.FThumbnail.FBitmap;
end;

procedure TPSDImage.Invalidate;
begin
  FRendered := False;
end;

procedure TPsdImage.LoadFromFile(const FileName: string);
var
  i,j,pos : Integer;
  dl : TPsdFileLayer;
  dc : TPsdFileChannel;
  fs : TFileStreamBuf;
  se : string;
begin
  FLayers.Clear;
  FTrees.Clear;
  FRendered := False;
  se := ExtractFileExt(FileName);
  if CompareText(se,'.psd') <> 0 then exit;
  fs := TFileStreamBuf.Create(FileName);
  try
    FFileName := FileName;                        // ファイル名を保存　※要改良？
    LoadFromFileHeader(fs);                       // ヘッダを処理
    LoadFromFileColorMode(fs);                    // カラーモードを処理
    FResource.LoadFromFile(fs);
    LoadFromFileLayerInfo(fs);                    // レイヤー情報を処理

    pos := fs.Position;                           // 現在のファイル位置を画像データの先頭とする
    for j := 0 to FLayers.Count-1 do begin        // レイヤー数ループ
      dl := FLayers[j];                           // レイヤーのデータを参照
      for i := 0 to dl.Channels.Count-1 do begin  // チャンネル数分ループ
        dc := dl.Channels[i];                     // チャンネルのデータを参照
        dc.FImageAdr := pos;                      // 画像データのアドレスとする

        dc.LoadBitmap(fs,pos);

        pos := pos + dc.FImageLength;             // 画像サイズ分アドレスを増やす
      end;
    end;
    LoadFromFileAnm();                            // ANMファイルに必要な情報を作成
    LoadFromFileTree();                           // レイヤーツリー情報を作成
  finally
    fs.Free;
  end;
end;

function StringListToText(ts: TStringList): string;
var
  i: Integer;
  s : string;
begin
  s := '';
  for i := 0 to ts.Count-1 do begin
    if ts[i] = '' then continue;
    s := s + ts[i] + '/';
  end;
  result := s;
end;

// ANMトラック文字列の右側にある不要な「/」を削除
function AnmRightDelete(const str : string) : string;
var
  i: Integer;
  s : string;
begin
  result := str;
  if Pos('/',str) = 0 then exit;
  for i := Length(str) downto 1 do begin
    s := Copy(str,i,1);
    if s = '/' then continue;
      result := Copy(str,1,i);
      exit;
  end;
end;

function StringToData(const str: string): string;
var
  s,sf,st : string;
  i : Integer;
begin
  s := str;
  s := StringReplace(s,'表','%xQ', [rfReplaceAll]);
  i := Pos(' ',s);
  if i <> 0  then begin
    s := s;
  end;

  for i := $20 to $2f do begin
    if i = $21 then continue;         // 「!」は変換しない
    if i = $2A then continue;         // 「*」は変換しない
    if i = $2f then continue;         // 「/」は変換しない
    if i = $25 then continue;         // 「%」は変換しない
    if i = $28 then continue;         // 「(」は変換しない
    if i = $29 then continue;         // 「)」は変換しない
    st := '%' + IntToHex(i,2);
    sf := Char(i);
    s := ReplaceText(s,sf,st);
  end;
  result := s;
end;

function TPsdImage.LoadFromFileAnm: Boolean;
var
  j : Integer;
  dl : TPsdFileLayer;
  ts : TStringList;
  s : string;
begin
  ts := TStringList.Create;
  try
    for j := FLayers.Count-1 downto 0 do begin        // 逆順に読み込む
      dl := FLayers[j];                               // レイヤーのデータを参照
      case dl.LayerType of                            // レイヤーの種類で分岐
        0 : begin                                     // データの場合
              dl.FAnmGroup2 := StringListToText(ts);
              s := StringListToText(ts) + dl.Name;    //
              s := StringToData(s);
              s := AnmRightDelete(s);
              s := '  "v1.' + s + '",';               // ANMのトラック情報として加工
              dl.FAnmText := s;
              if ts.Count > 0 then begin
                dl.FAnmGroup := ts[ts.Count-1];
              end
              else begin
                dl.FAnmGroup := dl.Name;
              end;
            end;
        1,2 : begin                                   // フォルダを開くデータの場合
              dl.FAnmGroup2 := StringListToText(ts);
              s := StringListToText(ts) + dl.Name;    //
              s := StringToData(s);
              s := AnmRightDelete(s);
              s := '  "v1.' + s + '",';               // ANMのトラック情報として加工
              dl.FAnmText := s;
              ts.Add(dl.Name);                        // 文字列リストにグループ名として追加
            end;
        3 : begin                                     // フォルダを閉じるデータの場合
             if ts.Count > 0 then begin               //
                ts.Delete(ts.Count-1);                // 末端の分類名を削除
             end;
            end;
      end;
    end;
  finally
    ts.Free;
  end;
  result := True;
end;

function TPsdImage.LoadFromFileColorMode(fs: TFileStreamBuf): Boolean;
var
  size,i : Integer;
begin
  size :=  fs.ReadBin(4);
  for i := 0 to size-1 do begin
    fs.ReadDumy(1);
  end;
  result := True;
end;

function TPsdImage.LoadFromFileHeader(fs: TFileStreamBuf): Boolean;
var
  sa: AnsiString;
begin
  result := False;
  sa := fs.ReadStr(4);
  if sa <> '8BPS' then exit;
  FVersion := fs.ReadBin(2);
  fs.ReadDumy(6);
  FChannel   := fs.ReadBin(2);
  FHeight    := fs.ReadBin(4);
  FWidth     := fs.ReadBin(4);
  FDataBit   := fs.ReadBin(2);
  FColorMode := fs.ReadBin(2);
  result := True;
end;

function TPsdImage.LoadFromFileLayerInfo(fs: TFileStreamBuf): Boolean;
var
  i : Integer;
  d : TPsdFileLayer;
begin
  FLayers.Clear;
  FLayerMaskLength := fs.ReadBin(4);
  FLayerLength     := fs.ReadBin(4);
  FLayerCount      := fs.ReadBin(2);
  if (FLayerCount and $8000)<>0 then begin
    FLayerCount := (FLayerCount xor $FFFF) + 1;
  end;

  for i := 0 to FLayerCount-1 do begin
    d := FLayers.Add();
    d.FOwner := Self;
    d.LoadFromStreamInfo(fs);
    d.LoadFromStream(fs);
  end;

  result := True;
end;

function TPsdImage.LoadFromFileTree: Boolean;
var
  i : Integer;
  dl : TPsdFileLayer;
  tl,tl2 : TList;
  ts : TPsdFileTree;
  tss : TPsdFileTrees;
begin
  FTrees.Clear();
  tl := TList.Create;
  tl2 := TList.Create;
  try
    tl.Add(nil);                                       // ルートのツリー情報として追加
    //tl2.Add(nil);                                    // ルートのツリー情報として追加
    tss := FTrees;                                     // ルートのツリーリストを参照

    FLayerRoot.FNameShifJis := '!v1';                  // 親としてダミーのレイヤーを生成
    FLayerRoot.FLayerType := 1;
    ts := tss.Add();                                   // 親ツリーに追加
    ts.FLevel := -1;
    ts.FLayer := FLayerRoot;                           // レイヤーデータを反映
    FLayerRoot.FTree := ts;
    ts.FVisible := True;                               // 表示状態を反映
    ts.FOwners.Assign(tl2);                            // 子レイヤーにたどり着くまでのツリーを割り当て
    tl.Add(tss);                                       // 現在のツリーリストをストック
    tl2.Add(ts);
    tss := TPsdFileTrees(ts.FTrees);                   // 子ツリーを参照し以降の処理用とする


    for i := FLayers.Count-1 downto 0 do begin
      dl := FLayers[i];
      case dl.LayerType of
        0   : begin                                    // データレイヤーの場合
                ts := tss.Add();                       // 親ツリーリストに追加
                ts.FLayer := dl;                       // レイヤーデータ参照
                dl.FTree := ts;
                ts.FVisible := dl.Visible;              // 表示状態を反映
                ts.FOwners.Assign(tl2);                // 子レイヤーにたどり着くまでのツリーを割り当て
              end;
        1,2 : begin                                    // フォルダ開きレイヤーの場合
                ts := tss.Add();                       // 親ツリーに追加
                ts.FLayer := dl;                       // レイヤーデータを反映
                dl.FTree := ts;
                ts.FVisible := dl.Visible;              // 表示状態を反映
                ts.FOwners.Assign(tl2);                // 子レイヤーにたどり着くまでのツリーを割り当て
                tl.Add(tss);                           // 現在のツリーリストをストック
                tl2.Add(ts);
                tss := TPsdFileTrees(ts.FTrees);       // 子ツリーを参照し以降の処理用とする
              end;
        3   : begin                                    // フォルダ閉じるデータの場合
                tss := TPsdFileTrees(tl[tl.Count-1]);  // ストックしたツリーリストを取り出し
                tl.Delete(tl.Count-1);                 // ストックから削除
                tl2.Delete(tl2.Count-1);
              end;
      end;
    end;
  finally
    tl2.Free;
    tl.Free;
  end;
  result := True;
end;

procedure TPSDImage.Render;
var
  y : Integer;
begin
  FBitmap.SetSize(FWidth,FHeight);
  FBitmap.PixelFormat := pf32bit;
  BitmapClear();
  SetLength(FBitmapLines,FHeight);
  for y := 0 to FHeight-1 do begin
    FBitmapLines[y] := FBitmap.ScanLine[y];
  end;

  RenderSub(FTrees);
  FBitmap.AlphaFormat := afDefined;
  FRendered := True;
end;

procedure TPsdImage.VisibleInit;
var
 i  : Integer;
 dl : TPsdFileLayer;
 ts : TPsdFileTree;
begin
  for i := 0 to FLayers.Count-1 do begin
    dl := FLayers[i];
    ts := TPsdFileTree(dl.FTree);
    if ts = nil then continue;
    ts.FVisible := dl.Visible;
  end;
end;

{ TPsdFileLayers }

destructor TPsdFileLayers.Destroy;
begin
  Clear();
  inherited;
end;

function TPsdFileLayers.Add: TPsdFileLayer;
var
  d : TPsdFileLayer;
begin
  d := TPsdFileLayer.Create;
  inherited Add(d);
  result := d;
end;

procedure TPsdFileLayers.Clear;
var
  i : Integer;
begin
  for i := 0 to Count-1 do begin
    Layers[i].Free;
  end;
  inherited;
end;

procedure TPsdFileLayers.Delete(i: Integer);
begin
  Layers[i].Free;
  inherited Delete(i);
end;

function TPsdFileLayers.GetLayers(Index: Integer): TPsdFileLayer;
begin
  result := inherited Items[Index];
end;

function TPsdFileLayers.IndexOfAnmText(const AnmText: string): Integer;
var
  i: Integer;
begin
 result := -1;
 for i := 0 to Count-1 do begin
   if Layers[i].FAnmText = AnmText then begin
     result := i;
     exit;
   end;
 end;
end;

{ TPsdFileLayerExs }

procedure TPsdFileLayerExs.Add(d: TPsdFileLayer);
begin
  inherited Add(d);
end;

function TPsdFileLayerExs.GetLayers(Index: Integer): TPsdFileLayer;
begin
  result := inherited Items[Index];
end;

{ TPsdFileLayer }

constructor TPsdFileLayer.Create;
begin
  //FBitmap := TBitmap.Create;
  //FBitmapTransParent := TBitmap.Create;
  //FBitmapMask := TBitmap.Create;
  FPng := TPngImage.Create;
  FChannels := TPsdFileChannels.Create;
  FBlends := TPsdFileBlends.Create;
  FBlendGlay := TPsdFileBlend.Create;
end;

destructor TPsdFileLayer.Destroy;
begin
  FBlendGlay.Free;
  FBlends.Free;
  FChannels.Free;
  FPng.Free;
  inherited;
end;



procedure TPsdFileLayer.Draw(const toLines : array of Pointer;const aWidth,aHeight,aLeft,aTop : Integer);
var
  ch,y,x,d : Integer;
  dc : TPsdFileChannel;
  //fromLines : array of Pointer;
  fromCol,toCol,resCol : TFourth;
begin
  //SetLength(fromLines,FChannels.Height);                   // 高さ分の配列を作成
  for y := 0 to FChannels.Height-1 do begin                // 高さ分ループ
    if aTop+y < 0 then continue;                           // 範囲外の場合は描画しない
    if aTop+y > aHeight-1 then break;                        // 範囲外の場合は描画しない
    for x := 0 to FChannels.Width-1 do begin               // 横幅分ループ
      if aLeft+x < 0 then continue;                        // 範囲外の場合は描画しない
      if aLeft+x > aWidth-1 then break;                      // 範囲外の場合は描画しない
      for ch := 0 to FChannels.Count-1 do begin            // チャンネル数分ループ
        dc := FChannels[ch];                               // チャンネルクラス参照
        if y > dc.Height-1 then break;
        if x > dc.Width-1 then break;

        d := dc.FImage[y,x];                               // 画像データを取得
        case dc.FColorType of                              // チャンネルが持つ色指定で分岐
          ctR     : fromCol.R := d;   // R                 // 元のRGB画素データとする
          ctG     : fromCol.G := d;   // G
          ctB     : fromCol.B := d;   // B
          ctAlpha : fromCol.A := d;   // α
        end;
      end;
      toCol.R := PFourthArray(toLines[aTop+y])^[aLeft+x].R; // 描画先RGB画素データを取得
      toCol.G := PFourthArray(toLines[aTop+y])^[aLeft+x].G;
      toCol.B := PFourthArray(toLines[aTop+y])^[aLeft+x].B;
      toCol.A := PFourthArray(toLines[aTop+y])^[aLeft+x].A; // αチャンネルは手動処理のためないものとする

      case BlendMode of                                     // 描画エフェクトの種類で処理を分岐
        btNil: ;
        btNorm: resCol := DrawNorm(fromCol,toCol);          // 通常
        btMul : resCol := DrawMul(fromCol,toCol);            // 乗算
        {
        btPass: ;
        btDiss: ;
        btDark: ;
        btIdiv: ;
        btLbrn: ;
        btDkCl: ;
        btLite: ;
        btScrn: ;
        btDiv: ;
        btLddg: ;
        btLgCl: ;
        btOver: ;
        btSLit: ;
        btHLit: ;
        btVLit: ;
        btLLit: ;
        btPLit: ;
        btHMix: ;
        btDiff: ;
        btSmud: ;
        btFsub: ;
        btFdiv: ;
        btHue: ;
        btSat: ;
        btCol: ;
        btLum: ;
        }
      else begin                                            // その他のエフェクタもある場合
             //d := 0;                                        // ここをブレイクポイントとする
           end;
      end;
      if resCol.A > 0 then begin
        resCol.A := 255;
      end;

      PFourthArray(toLines[aTop+y])^[aLeft+x].R := resCol.R;  // 描画先の画素データとして反映
      PFourthArray(toLines[aTop+y])^[aLeft+x].G := resCol.G;
      PFourthArray(toLines[aTop+y])^[aLeft+x].B := resCol.B;
      PFourthArray(toLines[aTop+y])^[aLeft+x].A := resCol.A;


    end;
  end;

end;



// 標準描画
function TPsdFileLayer.DrawNorm(const fromCol, toCol: TFourth): TFourth;
var
  alpha : Integer;
begin
  alpha := 255 - fromCol.A;
  result.R := fromCol.R * fromCol.A div 255 + toCol.R * alpha div 255;
  result.G := fromCol.G * fromCol.A div 255 + toCol.G * alpha div 255;
  result.B := fromCol.B * fromCol.A div 255 + toCol.B * alpha div 255;
  result.A := fromCol.A * fromCol.A div 255 + toCol.A * alpha div 255;
end;

// 乗算描画
function TPsdFileLayer.DrawMul(const fromCol, toCol: TFourth): TFourth;
var
  alpha : Integer;
  fromCol2 : TFourth;
begin
  alpha := 255 - fromCol.A;
  fromCol2.R := fromCol.R  * toCol.R div 255;
  fromCol2.G := fromCol.G  * toCol.G div 255;
  fromCol2.B := fromCol.B  * toCol.B div 255;
  fromCol2.A := fromCol.A  * toCol.A div 255;

  result.R := fromCol2.R * fromCol.A div 255 + toCol.R * alpha div 255;
  result.G := fromCol2.G * fromCol.A div 255 + toCol.G * alpha div 255;
  result.B := fromCol2.B * fromCol.A div 255 + toCol.B * alpha div 255;
  result.A := fromCol2.A * fromCol.A div 255 + toCol.A * alpha div 255;
end;

// PSDデータ内の識別文字を描画エフェクタ型に変換
function TPsdFileLayer.GetBlandMode(const str: AnsiString): TPsdFileBlandType;
var
  i : Integer;
begin
  result := btNil;
  for i := 0 to High(BLEND_KEY) do begin
    if BLEND_KEY[i] = str then begin
      result := TPsdFileBlandType(i+1);
      exit;
    end;
  end;
end;

function TPsdFileLayer.GetName: string;
begin
  if FNameUnicode<>'' then begin
    result := FNameUnicode;
  end
  else begin
    result := string(FNameShifJis);
  end;
end;


function TPsdFileLayer.GetTreeVisible: Boolean;
var
  ts : TPsdFileTree;
begin
  result := False;
  ts := TPsdFileTree(FTree);
  if ts = nil then exit;

  result := ts.Visible;
end;


function TPsdFileLayer.LoadFromStream(fs: TFileStreamBuf): Boolean;
var
  sa : AnsiString;
  m : TPsdFileBlandType;
  PosNext : Integer;
begin
  result := False;
  PosNext := fs.Position + 1;             // Nomalタグを読みに行くまで不明のためダミー
  //while fs.Position< FNextPos do begin
  while fs.Position < PosNext do begin
    sa := fs.ReadStr(4);
    if sa <> '8BIM' then begin
      sa := sa;
      exit;
    end;
    sa := fs.ReadStr(4);
    m := GetBlandMode(sa);
    if m <> btNil then begin
      PosNext := LoadFromStreamNorm(fs,m);
    end
    else begin
      if sa= 'luni' then begin
        LoadFromStreamLuni(fs);
      end
      else if sa= 'lsct' then begin
        LoadFromStreamLsct(fs);
      end
      else if sa= 'lspf' then begin
        LoadFromStreamLspf(fs);
      end
      else if sa= 'tsly' then begin
        LoadFromStreamEtc(fs);
      end
      else begin
        LoadFromStreamEtc(fs);
      end;
    end;
  end;
  result := True;
end;

function TPsdFileLayer.LoadFromStreamEtc(fs: TFileStreamBuf): Boolean;
var
  len : Integer;
begin
  len := fs.ReadBin(4);
  fs.ReadDumy(len);
  result := True;
end;

function TPsdFileLayer.LoadFromStreamInfo(fs: TFileStreamBuf): Boolean;
var
  i,aTop,aLeft,ABottom,aRight : Integer;
  dc : TPsdFileChannel;
begin
  aTop   := fs.ReadBin(4);
  aLeft   := fs.ReadBin(4);
  aBottom := fs.ReadBin(4);
  aRight  := fs.ReadBin(4);
  FChannelCount := fs.ReadBin(2);
  FChannels.Clear;
  for i := 0 to FChannelCount-1 do begin
    dc := FChannels.Add();
    dc.LoadFromStream(fs);
    dc.FTop := aTop;
    dc.FLeft := aLeft;
    dc.FBottom := ABottom;
    dc.FRight := aRight;
  end;
  result := True;
end;

function TPsdFileLayer.LoadFromStreamLsct(fs: TFileStreamBuf): Boolean;
var
  len,i : Integer;
begin
  len := fs.ReadBin(4);
//  fs.ReadDumy(len);

  FLayerType := fs.ReadBin(4);
  for i := 4 to len-1 do begin
    fs.ReadDumy(1);
  end;

  result := True;
end;

function TPsdFileLayer.LoadFromStreamLspf(fs: TFileStreamBuf): Boolean;
var
  len : Integer;
begin
  len := fs.ReadBin(4);
  fs.ReadDumy(len);
  result := True;
end;

function TPsdFileLayer.LoadFromStreamLuni(fs: TFileStreamBuf): Boolean;
begin
  FNameUnicode := fs.ReadStrUnicodeSizeLenData();
  result := True;
end;

function TPsdFileLayer.LoadFromStreamNorm(fs: TFileStreamBuf;mode : TPsdFileBlandType): Integer;
var
  size : Integer;
  i: Integer;
  db : TPsdFileBlend;
  dc : TPsdFileChannel;
begin
  FBlendMode   := mode;
  FTransParent := fs.ReadBin(1);
  FClipping    := fs.ReadBin(1);
  FFlag        := fs.ReadBin(1);
  fs.ReadDumy(1);
  size         := fs.ReadBin(4);
  Result       := fs.Position + size;
  //FNextPos := fs.Position + size;
  size := fs.ReadBin(4);
  if size >0 then begin                    // レイヤーマスクデータが存在する場合
    dc := FChannels[4];                    // マスク？ RGBの次のチャンネルに割り当てる
    dc.FTop          := fs.ReadBin(4);     // Modeが-2 または -3であることを確かめた方が良い
    dc.FLeft         := fs.ReadBin(4);
    dc.FBottom       := fs.ReadBin(4);
    dc.FRight        := fs.ReadBin(4);
    dc.FDefaultColor := fs.ReadBin(1);
    dc.FFlag         := fs.ReadBin(1);
    dc.FMask         := fs.ReadBin(1);
    if size=20 then begin                  // 長さ20バイトのデータの場合
      fs.ReadDumy(1);                      // 2の倍数にするための予備データを読み飛ばす
    end
    else begin                             // 20以上の場合はさらにデータが存在する
      dc.FFlag2      := fs.ReadBin(1);
      dc.FMask2      := fs.ReadBin(1);
      dc.FRectTop    := fs.ReadBin(4);
      dc.FRectLeft   := fs.ReadBin(4);
      dc.FRectTop    := fs.ReadBin(4);
      dc.FRectBottom := fs.ReadBin(4);
    end;

  end;
  size := fs.ReadBin(4);
  FBlends.Clear;
  for i := 0 to size div 8-1 do begin
    if i = 0 then begin
      FBlendGlay.LoadFromStream(fs);
    end
    else begin
      db := FBlends.Add();
      db.LoadFromStream(fs);
    end;
  end;
  size := fs.ReadBin(1);
  FNameShifJis := fs.ReadStrPascal(size);
end;

procedure TPsdFileLayer.SetTreeVisible(const Value: Boolean);
var
  ts : TPsdFileTree;
begin
  ts := TPsdFileTree(FTree);
  ts.Visible := Value;
end;

function TPsdFileLayer.GetVisible: Boolean;
begin
  result := (FFlag and $02) <> $02;
end;

function TPsdFileLayer.ImageToBitmap(aBitmap: TBitmap): Boolean;
var
  y,aWidth,aHeight : Integer;
  sLines : array of Pointer;
  cv : TCanvas;
begin
  result := False;
  if ChannelCount=0 then exit;

  if FChannels.Count=5 then begin
    exit;
  end;
  aWidth := FChannels.Width;
  aHeight := FChannels.Height;
  //aWidth := aBitmap.Width;
  //aHeight := aBitmap.Height;

  aBitmap.SetSize(aWidth,aHeight);
  aBitmap.PixelFormat := pf32bit;
  cv := aBitmap.Canvas;
  cv.Brush.Color := clWhite;
  cv.Brush.Style := bsSolid;
  cv.FillRect(Rect(0,0,aBitmap.Width,aBitmap.Height));

  SetLength(sLines,aHeight);
  for y := 0 to FChannels.Height-1 do begin
    sLines[y] := aBitmap.ScanLine[y];
  end;

  if FChannels.Height=0 then exit;

  Draw(sLines,aWidth,aHeight,0,0);
  //Draw2(sLines,aWidth,aHeight,Channels[0].Left,Channels[0].Top);


end;


{ TPsdFileChannels }

destructor TPsdFileChannels.Destroy;
begin
  Clear();
  inherited;
end;

function TPsdFileChannels.Add: TPsdFileChannel;
var
  d : TPsdFileChannel;
begin
  d := TPsdFileChannel.Create;
  inherited Add(d);
  result := d;
end;

procedure TPsdFileChannels.Clear;
var
  i : Integer;
begin
  for i := 0 to Count-1 do begin
    Channels[i].Free;
  end;
  inherited;
end;

procedure TPsdFileChannels.Delete(i: Integer);
begin
  Channels[i].Free;
  inherited Delete(i);
end;

function TPsdFileChannels.GetItems(Index: Integer): TPsdFileChannel;
begin
  result := inherited Items[Index];
end;

function TPsdFileChannels.GetHeight: Integer;
var
  i,d : Integer;
  dc : TPsdFileChannel;
begin
  d := 0;
  for i := 0 to Count-1 do begin
    dc := Items[i];
    if dc.FMode >= 3 then continue;
    if dc.Height > d then d := dc.Height;
  end;
  result := d;
end;

function TPsdFileChannels.GetWidth: Integer;
var
  i,d : Integer;
  dc : TPsdFileChannel;
begin
  d := 0;
  for i := 0 to Count-1 do begin
    dc := Items[i];
    if dc = nil then continue;
    if dc.FMode >= 3 then continue;
    if dc.Width > d then d := dc.Width;
  end;
  result := d;
end;

function TPsdFileChannels.IndexOfColorType(const ct: TPsdFileColorType): Integer;
var
  i : Integer;
begin
  result := -1;
  for i := 0 to Count-1 do begin
    if Channels[i].FColorType = ct then begin
      result := i;
      exit;
    end;
  end;
end;

{ TPsdFileChannel }


function TPsdFileChannel.GetColorType: TPsdFileColorType;
begin
  result := ctNil;
  case FMode of                   // チャンネルが持つ色指定で分岐
    0     : result := ctR;        // 赤
    1     : result := ctG;        // 緑
    2     : result := ctB;        // 青
    $FFFF : result := ctAlpha;    // 透明度
    $FFFE : result := ctMask;     // マスク
  end;
end;

function TPsdFileChannel.GetHeight: Integer;
begin
  result := FBottom - FTop;
end;

function TPsdFileChannel.GetWidth: Integer;
begin
  result := FRight - FLeft;
end;

function TPsdFileChannel.LoadBitmap(fs: TFileStreamBuf; Pos: Integer): Boolean;
var
  mode : Integer;

begin

  SetLength(FImage,Height,Width);
  mode := fs.ReadBin(2);
  case mode of
    0: LoadBitmapCompressionNon(fs);
    1: LoadBitmapCompressionRle(fs);
  end;

  result := True;
end;

function TPsdFileChannel.LoadBitmapCompressionNon(fs: TFileStreamBuf): Boolean;
var
  y,x,d : Integer;
begin
  for y := 0 to Height-1 do begin        // 高さ分ループ
    for x := 0 to Width-1 do begin       // 幅分ループ
      d := fs.ReadBin(1);
      FImage[y,x] := d;
    end;
  end;
  result := True;
end;

function TPsdFileChannel.LoadBitmapCompressionRle(fs: TFileStreamBuf): Boolean;
var
  rLen :  array of Integer;
  y,x,len,i,NextPos : Integer;
  d : Byte;
begin
  SetLength(rLen,Height);                       // 高さ×チャンネル数の配列を準備
  for y := 0 to Height-1 do begin               // 各Ｙ座標ごとに読み込むバイト数を取得
    rLen[y] :=  fs.ReadBin(2);
  end;

  for y := 0 to Height-1 do begin              // 高さ分ループ
    NextPos := fs.Position + rLen[y];         // 読み込み完了位置を取得
    x := 0;
    while fs.Position <  NextPos do begin     // 横幅分データが揃うまでループ
      len := fs.ReadBin(1);                   // データを取得

      if len < 128 then begin                  // 最上位ビットが0の場合
        Inc(len);                              // 1増やす
        for i := 0 to len-1 do begin           // 取得した値をデータ長としてループ
          d := fs.ReadBin(1);                 // データを取得
          if x >= Width then continue;         // X座標が範囲を超える場合は未処理
          FImage[y,x] := d;                    // 値を書き込む
          x := x +1;                           // 次の座標へ
        end;
      end
      else if len >128 then begin              // 最上位ビットが1の場合
        len := len xor $ff;                    // 2の補数とする
        len := len +2;
        d := fs.ReadBin(1);                   // データを取得
        for i := 0 to len-1 do begin           // 2の補数にした長さデータ分ループ
          if x >= Width then continue;         // X座標が範囲を超える場合は未処理
          FImage[y,x] := d;                    // 値を書き込む
          x := x +1;                           // 次の座標へ
        end;
      end;
    end;
  end;
  result := True;
end;

function TPsdFileChannel.LoadFromStream(fs: TFileStreamBuf): Boolean;
begin
  FMode        := fs.ReadBin(2);
  FImageLength := fs.ReadBin(4);
  FColorType   := GetColorType();
  result := True;
end;

{ TPsdFileBlends }

destructor TPsdFileBlends.Destroy;
begin
  Clear();
  inherited;
end;

function TPsdFileBlends.Add: TPsdFileBlend;
var
  d : TPsdFileBlend;
begin
  d := TPsdFileBlend.Create;
  inherited Add(d);
  result := d;
end;

procedure TPsdFileBlends.Clear;
var
  i : Integer;
begin
  for i := 0 to Count-1 do begin
    Blends[i].Free;
  end;
  inherited;
end;

procedure TPsdFileBlends.Delete(i: Integer);
begin
  Blends[i].Free;
  inherited Delete(i);
end;

function TPsdFileBlends.GetItems(Index: Integer): TPsdFileBlend;
begin
  result := inherited Items[Index];
end;

{ TPsdFileBlend }

function TPsdFileBlend.LoadFromStream(fs: TFileStreamBuf): Boolean;
begin
  FSource := fs.ReadBin(4);
  FAddress := fs.ReadBin(4);
  result := True;
end;


{ TPsdFileBitmap }

procedure TPsdFileBitmap.SetSize(aWidth, aHeight: Integer);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  SetLength(FPixcel,FHeight,FWidth);
end;



{ TPsdFileTreeFrameTrees }

destructor TPsdFileTrees.Destroy;
begin
  Clear();
  inherited;
end;

function TPsdFileTrees.Add: TPsdFileTree;
var
  d : TPsdFileTree;
begin
  d := TPsdFileTree.Create;
  //d.FOwner := Self;
  inherited Add(d);
  result := d;
end;

procedure TPsdFileTrees.Clear;
var
  i : Integer;
begin
  for i := 0 to Count-1 do begin
    Trees[i].Free;
  end;
  inherited;
end;

procedure TPsdFileTrees.Delete(i: Integer);
begin
  Trees[i].Free;
  inherited Delete(i);
end;

function TPsdFileTrees.GetTrees(Index: Integer): TPsdFileTree;
begin
  result := inherited Items[Index];
end;

{
function TPsdFileTrees.GetLayerTree( dl: TPsdFileLayer): TPsdFileTree;
var
  i : Integer;
  ts : TPsdFileTree;
begin
  i := IndexOfLayer(dl);
  ts := Items[i];
  result := TPsdFileTree(ts.FTrees);
end;
}

function TPsdFileTrees.IndexOfLayer(dl: TPsdFileLayer): Integer;
var
  i : Integer;
begin
  result := -1;
  for i := 0 to Count-1 do begin
    if Trees[i].FLayer = dl then begin
      result := i;
      exit;
    end;
  end;

end;

{ TPsdFileTreeFrameTree }

constructor TPsdFileTree.Create;
begin
  FTrees := TPsdFileTrees.Create;
  FOwners := TList.Create;
end;

destructor TPsdFileTree.Destroy;
begin
  FOwners.Free;
   FTrees.Free;
  inherited;
end;

function TPsdFileTree.GetIsAlways: Boolean;
var
  dl : TPsdFileLayer;
begin
  result := False;
  dl := FLayer;
  if dl = nil then exit;
  result := Copy(dl.Name,1,1) = '!';
end;

function TPsdFileTree.GetIsChildren: Boolean;
var
  tss : TPsdFileTrees;
begin
  tss := TPsdFileTrees(FTrees);             // 子供が居るかどうかで判断
  result := tss.Count > 0;
end;


function TPsdFileTree.GetIsMultiSelect: Boolean;
var
  dl : TPsdFileLayer;
begin
  result := False;
  dl := FLayer;
  if dl = nil then exit;
  result := Copy(dl.Name,1,1) <> '*';
end;

procedure TPsdFileTree.SetVisible(const Value: Boolean);
var
  i,cnt: Integer;
  ts : TPsdFileTree;
  tss : TPsdFileTrees;
  s : string;
begin
  if IsMultiSelect then begin                // 複数選択可能な場合
    if not Value then begin                  // 非表示の指定の場合で
      if IsAlways then exit;                 // 常に表示のレイヤーの場合は処理終了
    end;

    FVisible := Value;                       // 反映させる
    cnt := FOwners.Count;
    if cnt < 2 then exit;                    // 可能なはずの選択肢が自分しか無い場合は消さない
    ts := TPsdFileTree(FOwners[cnt-1]);
    ts.Visible := True;
  end
  else begin
    //if FVisible = Value then exit;

    cnt := FOwners.Count;
    if cnt = 0 then begin
      FVisible := Value;
      exit;
    end;
    ts := TPsdFileTree(FOwners[cnt-1]);
    tss := TPsdFileTrees(ts.FTrees);
    for i := 0 to tss.Count-1 do begin   // 自分の親ツリーのデータ数ループ
      ts := tss[i];                      // ツリーデータ参照
      if ts = Self then continue;        // 自分自身の場合は処理しない
      if not ts.FVisible then continue;  // 非表示中なら処理しない
      if ts.Level=-1 then continue;
      s := ts.FLayer.Name;
      if ts.IsAlways then continue;      // 常に表示のレイヤーは消さない
      if ts.IsMultiSelect then continue;      // 常に表示のレイヤーは消さない

      ts.FVisible := False;              // 排他的処理とする
    end;
    if Value then begin                  // 複数選択不可能な場合　非表示指定は処理しない
      FVisible := Value;                 // 指定通り表示状態とする
    end;
    if cnt < 1 then exit;
    ts := TPsdFileTree(FOwners[cnt-1]);
    ts.Visible := True;
  end;
end;

{ TPsdFileResource }

constructor TPsdFileResource.Create;
begin
  FThumbnail := TPsdFileResourceThumbnail.Create;
end;

destructor TPsdFileResource.Destroy;
begin
  FThumbnail.Free;
  inherited;
end;

function TPsdFileResource.LoadFromFile(fs: TFileStreamBuf): Boolean;
var
  size,size2 : Integer;
  s: string;
  sa : AnsiString;
  mode,len : Integer;
begin
  FSize :=  fs.ReadBin(4);                // リソース全体のサイズを取得
  size := fs.Position + FSize;
  while fs.Position < size do begin          // キャッシュサイズまで読み込み処理を行う
    sa := fs.ReadStr(4);                    // シグネチャ 8BIMを取得
    if sa <> '8BIM' then break;             // 解析エラーとして処理終了
    mode := fs.ReadBin(2);                 // リソースの種類を取得
    s := IntToHex(mode,4);                 // デバッグ用
    len := fs.ReadBin(1);                  // リソースを識別する文字列の長さを取得
    sa := fs.ReadStrPascal2(len);          // リソースを識別する文字列を取得
    case mode of
      $040C : FThumbnail.LoadFromFile(fs); // サムネイルデータの場合サムネイル情報として読み込む
    else      begin
                size2 := fs.ReadBin(4);        // リソースのデータサイズを取得
                size2 := (size2+1) div 2 * 2;  // リソースデータは2の倍数サイズなので丸める
                fs.ReadDumy(size2);            // リソースサイズ分データを読み飛ばす
              end;
    end;
  end;

  result := True;

end;

{ TPsdFileResourceThumbnail }

constructor TPsdFileResourceThumbnail.Create;
begin
  FBitmap := TBitmap.Create;
end;

destructor TPsdFileResourceThumbnail.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TPsdFileResourceThumbnail.LoadFromFile(fs: TFileStreamBuf): Boolean;
var
  size,bufpos,phase : Integer;
begin
  size :=  fs.ReadBin(4);                     // リソース全体のサイズを取得
  size := (size+1) div 2 * 2;                 // もしかしたら必要／不要かも
  bufpos := fs.Position + size;                 // 最終データ位置を取得
  phase := 0;                                 // 処理経過状態を初期化
  while fs.Position < bufpos do begin           // 最終データまで読み込み処理を行う
    case phase of                             // 処理経過順序で分岐
      0 : FImageFormat := fs.ReadBin(4);
      1 : FWidth       := fs.ReadBin(4);
      2 : FHeight      := fs.ReadBin(4);
      3 : FWidthbytes  := fs.ReadBin(4);
      4 : FWidthHeight := fs.ReadBin(4);
      5 : FDataLength  := fs.ReadBin(4);
      6 : FPixcelBit   := fs.ReadBin(2);
      7 : FPlaneCount  := fs.ReadBin(2);
      8 : LoadFromFileData(fs);               // 画像データを読み込み
      else fs.ReadDumy(1);                    // あまりデータを処理
    end;
    inc(phase);
  end;
  result := True;
end;

function TPsdFileResourceThumbnail.LoadFromFileData(fs: TFileStreamBuf): Boolean;
begin
  FBitmap.SetSize(FWidth,FHeight);         // ビットマップサイズを設定
  FBitmap.PixelFormat := pf32bit;
  case FImageFormat of                     // データの種類で分岐
    1 : LoadFromFileDataJpeg(fs);          // Jpegデータの場合
  end;
  result := True;
end;

function TPsdFileResourceThumbnail.LoadFromFileDataJpeg(fs: TFileStreamBuf): Boolean;
var
  jpeg : TJPEGImage;
  ms : TMemoryStream;
  i: Integer;
  d : Byte;
begin
  jpeg := TJPEGImage.Create;
  ms := TMemoryStream.Create;
  try
    ms.Position := 0;                      // ストリームの先頭へ
    for i := 0 to DataLength-1 do begin    // データ長分ループ
      d := fs.ReadBin(1);                  // キャッシュから読み込み
      ms.WriteData(d);                     // ストリームへ書きだし
    end;
    ms.Position := 0;                      // ストリームの先頭へ
    jpeg.LoadFromStream(ms);               // Jpegデータとして読み込む
    FBitmap.Assign(jpeg);                  // ビットマップ変換
  finally
    ms.Free;
    jpeg.Free;
  end;
  result := True;
end;

//--------------------------------------------------------------------------//
//  高速バッファ型ファイルストリームクラス（完全メモリ展開版）             //
//  TFileStreamBuf : 旧版と同名・同I/F互換                                 //
//--------------------------------------------------------------------------//


{ TFileStreamBuf }

constructor TFileStreamBuf.Create(const AFileName: string);
var
  FS: TFileStream;
begin
  inherited Create;
  FFileName := AFileName;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    FSize := FS.Size;
    GetMem(FBuffer, FSize);
    FS.ReadBuffer(FBuffer^, FSize);
  finally
    FS.Free;
  end;

  FPos := 0;
end;

destructor TFileStreamBuf.Destroy;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  inherited;
end;

function TFileStreamBuf.Position: Integer;
begin
  Result := FPos;
end;

function TFileStreamBuf.Size: Integer;
begin
  Result := FSize;
end;

procedure TFileStreamBuf.Seek(NewPos: Integer);
begin
  if (NewPos < 0) or (NewPos > FSize) then
    raise Exception.CreateFmt('Seek position %d out of range', [NewPos]);
  FPos := NewPos;
end;

procedure TFileStreamBuf.Skip(Count: Integer);
begin
  Seek(FPos + Count);
end;

function TFileStreamBuf.ReadByte: Byte;
begin
  if FPos >= FSize then
    raise Exception.Create('Read beyond end of buffer');
  Result := FBuffer[FPos];
  Inc(FPos);
end;

//---------------------------------------------------------------------------
// BigEndian 読み込み
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadBin(const Length: Integer): Integer;
var
  i: Integer;
  b: Byte;
begin
  Result := 0;
  for i := 0 to Length - 1 do
  begin
    if FPos >= FSize then
      raise Exception.Create('Read beyond end of buffer');
    b := FBuffer[FPos];
    Inc(FPos);
    Result := (Result shl 8) or b;
  end;
end;

//---------------------------------------------------------------------------
// LittleEndian 読み込み
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadInt(const Length: Integer): Integer;
var
  i: Integer;
  b: Byte;
begin
  Result := 0;
  for i := 0 to Length - 1 do
  begin
    if FPos >= FSize then
      raise Exception.Create('Read beyond end of buffer');
    b := FBuffer[FPos];
    Inc(FPos);
    Result := Result or (b shl (i * 8));
  end;
end;

//---------------------------------------------------------------------------
// 読み捨て（高速）
//---------------------------------------------------------------------------
procedure TFileStreamBuf.ReadDumy(const Length: Integer);
begin
  if (Length <= 0) then Exit;
  if FPos + Length > FSize then
    raise Exception.Create('ReadDumy beyond end of buffer');
  Inc(FPos, Length);
end;

//---------------------------------------------------------------------------
// 文字列（ANSI）
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadStr(const Length: Integer): AnsiString;
var
  i: Integer;
begin
  SetLength(Result, Length);
  for i := 1 to Length do
  begin
    if FPos >= FSize then
      raise Exception.Create('ReadStr beyond end of buffer');
    Result[i] := AnsiChar(FBuffer[FPos]);
    Inc(FPos);
  end;
end;

//---------------------------------------------------------------------------
// Pascal形式文字列（4バイト境界調整付き）
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadStrPascal(const Length: Integer): AnsiString;
var
  m: Integer;
begin
  Result := ReadStr(Length);
  m := (Length + 1) mod 4;
  if m <> 0 then
    ReadDumy(4 - m);
end;

//---------------------------------------------------------------------------
// Pascal形式文字列（2バイト境界調整付き）
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadStrPascal2(const Length: Integer): AnsiString;
begin
  if Length = 0 then
  begin
    ReadDumy(1);
    Result := '';
    Exit;
  end;
  Result := ReadStr(Length);
  if (Length mod 2) = 0 then
    ReadDumy(1);
end;

//---------------------------------------------------------------------------
// Unicodeサイズ付き文字列（PSD用）
//---------------------------------------------------------------------------
function TFileStreamBuf.ReadStrUnicodeSizeLenData: string;
var
  s: string;
  i, size, len: Integer;
  Tbl: TBytes;
begin
  s := '';
  size := ReadBin(4); // 全体サイズ
  len  := ReadBin(4); // 実際の文字列長

  if size < 4 then
    Exit('');

  SetLength(Tbl, size);
  for i := 0 to size - 5 do
  begin
    if (i mod 2) = 0 then
      Tbl[i + 1] := ReadByte
    else
      Tbl[i - 1] := ReadByte;
  end;

  s := PChar(@Tbl[0]);
  SetLength(s, len);
  Result := s;
end;

end.

