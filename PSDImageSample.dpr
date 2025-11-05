program PSDImageSample;

uses
  Vcl.Forms,
  PsdImage in 'PsdImage.pas',
  PSDImageForm in 'PSDImageForm.pas' {FormPSDImage},
  PsdFileImageFrame in 'PsdFileImageFrame.pas' {FramePsdFileImage: TFrame},
  PsdFileTreeFrame in 'PsdFileTreeFrame.pas' {FramePsdFileTree: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPSDImage, FormPSDImage);
  Application.CreateForm(TFormPSDImage, FormPSDImage);
  Application.Run;
end.
