program PSDImageSample;

uses
  Vcl.Forms,
  PsdImage in 'PsdImage.pas',
  DebugStopwatch in 'DebugStopwatch.pas',
  PSDImageForm in 'PSDImageForm.pas' {FormPSDImage},
  PsdFileImageFrame in 'PsdFileImageFrame.pas' {FramePsdFileImage: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPSDImage, FormPSDImage);
  Application.CreateForm(TFormPSDImage, FormPSDImage);
  Application.Run;
end.
