# TPSDImage クラス

## 概要
`TPSDImage` は **Photoshop (PSD) ファイル** を読み込み、  
内部のレイヤー構造を解析して **VCL 用の `TBitmap`** にレンダリングする Delphi クラスです。  

各レイヤーは `TPsdFileLayer` / `TPsdFileLayers` / `TPsdFileTrees` で管理され、  
`Render()` によって αブレンド合成を行い、最終的なビットマップを生成します。  

---

## 主な機能

| 機能 | 説明 |
|------|------|
| PSDヘッダ解析 | 画像サイズ、ビット数、チャンネル数、カラーモードを読み込み |
| レイヤー解析 | 各レイヤーの画像データ・αチャンネル・ブレンドモードを展開 |
| ツリー構築 | PSDのフォルダ構造（グループ階層）を再現 |
| 合成処理 | レイヤー順にαブレンドし、VCL向けの `TBitmap` を生成 |
| PNG互換出力 | `TPngImage.Assign(Bitmap)` によって透過PNGとして保存可能 |

---

## 内部構成

| フィールド | 型 | 役割 |
|-------------|----|------|
| `FLayerRoot` | `TPsdFileLayer` | 仮想的な最上位レイヤー（PSDTool互換） |
| `FLayers` | `TPsdFileLayers` | レイヤーの一元管理クラス |
| `FTrees` | `TPsdFileTrees` | フォルダ階層（グループ構造） |
| `FBitmap` | `TBitmap` | 合成後の最終出力 |
| `FResource` | `TPsdFileResource` | ICCプロファイルなどPSD内の追加情報 |

---

## 主なメソッド

| メソッド | 説明 |
|-----------|------|
| `LoadFromFile()` | PSDファイルを読み込み、内部構造を解析 |
| `Render()` | 各レイヤーをブレンドモードに従って合成 |
| `Invalidate()` | 再描画要求（`Render()`を再実行） |
| `VisibleInit()` | レイヤーの表示・非表示状態を初期化 |
| `GetBitmap()` | 合成済みの `TBitmap` を取得 |
| `GetBitmapThumbnail()` | サムネイルサイズの `TBitmap` を取得 |

---

## ファイル解析処理の流れ

1. **ヘッダ解析**  
   `LoadFromFileHeader()` でバージョン、チャンネル数、画像サイズを取得  
2. **カラーモード解析**  
   `LoadFromFileColorMode()` によりカラーデータを読み込み  
3. **レイヤー展開**  
   `LoadFromFileLayerInfo()` でレイヤー構造とピクセルデータを展開  
4. **ツリー構築**  
   `LoadFromFileTree()` でフォルダ階層を構築  
5. **レンダリング**  
   `Render()` にて全レイヤーをα合成し `FBitmap` に出力  

---

## 描画動作の仕様

- 合成処理は `RenderSub()` にて行われます。  
- 各レイヤーのブレンドモード (`btNorm`, `btMul` など) に応じて合成。  
- 現在の VCL 実装では **αブレンドは擬似的処理** ですが、  
  内部では `pf32bit + AlphaFormat = afDefined` により  
  **完全な透過情報を保持** しています。  

そのため、VCL上では白背景に見えても、  
`TPngImage.Assign(FBitmap)` → `SaveToFile()` で  
**透明を維持したPNGファイル**を出力できます。

---

## 注意点

- `FBitmap` は `pf32bit` + `AlphaFormat=afDefined` で管理されています。  
- VCL Canvas (`StretchDraw`, `Draw`) では α が無視されるため、  
  プレビュー時は市松模様背景などで「見た目の透明」を再現します。  
- `Render()` 実行後に `FBitmap` を直接操作する場合はロックに注意してください。  

---

## 将来拡張（予定）

- 調整レイヤー／マスクレイヤー対応  
- GDI+ または Graphics32 による真のαブレンド描画  
- FireMonkey 対応版レンダラー  
- PSDToolKit互換シリアライズ（ptkl文字列生成）

---

## ライセンス
このクラスはMITとして提供されています。  
利用・改変は自己責任で行ってください。
