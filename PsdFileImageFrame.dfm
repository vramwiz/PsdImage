object FramePsdFileImage: TFramePsdFileImage
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnResize = FrameResize
  object scrBox: TScrollBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    TabOrder = 0
    object ImagePsd: TImage
      Left = 3
      Top = 3
      Width = 316
      Height = 236
      OnMouseDown = ImagePsdMouseDown
      OnMouseMove = ImagePsdMouseMove
      OnMouseUp = ImagePsdMouseUp
    end
  end
end
