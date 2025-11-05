object FramePsdFileTree: TFramePsdFileTree
  Left = 0
  Top = 0
  Width = 450
  Height = 240
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  object tvPsd: TTreeView
    Left = 0
    Top = 0
    Width = 450
    Height = 240
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    Images = ImagePsd
    Indent = 27
    ReadOnly = True
    StateImages = ImageState
    TabOrder = 0
    OnCollapsing = tvPsdCollapsing
    OnExpanding = tvPsdExpanding
    OnExpanded = tvPsdExpanded
    OnMouseDown = tvPsdMouseDown
  end
  object ImagePsd: TImageList
    Height = 32
    Width = 32
    Left = 152
    Top = 88
  end
  object ImageState: TImageList
    Height = 20
    Width = 20
    Left = 176
    Top = 184
  end
end
