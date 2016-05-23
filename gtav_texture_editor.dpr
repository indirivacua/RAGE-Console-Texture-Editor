program gtav_texture_editor;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  About in 'About.pas' {AboutForm},
  GTAIV.TextureResource.Xbox360 in 'GTAIV.TextureResource.Xbox360.pas',
  Compression.LZX in 'Compression.LZX.pas',
  Global.Endian in 'Global.Endian.pas',
  Console.Xbox360.Graphics in 'Console.Xbox360.Graphics.pas',
  Console.Xbox360.Swizzling in 'Console.Xbox360.Swizzling.pas',
  GTAIV.TextureResource.PS3 in 'GTAIV.TextureResource.PS3.pas',
  Compression.ZLib in 'Compression.ZLib.pas',
  Console.PS3.Graphics in 'Console.PS3.Graphics.pas',
  Global.DirectDrawSurface in 'Global.DirectDrawSurface.pas',
  TextureEditor.CPUSizeManual in 'TextureEditor.CPUSizeManual.pas' {FormCPUSizeManual},
  Intro in 'Intro.pas' {IntroForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TFormCPUSizeManual, FormCPUSizeManual);
  Application.CreateForm(TIntroForm, IntroForm);
  DictionaryName:=paramstr(1);
  if DictionaryName<>'' then
    begin
      FormCPUSizeManual.Position:=poScreenCenter;
      LoadFile(DictionaryName);
    end;
  Application.Run;
end.
