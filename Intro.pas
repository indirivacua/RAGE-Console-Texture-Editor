unit Intro;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Aero.Core, UI.Aero.Window, Vcl.StdCtrls,
  UI.Aero.Core.BaseControl, UI.Aero.Core.CustomControl.Animation,
  UI.Aero.Button.Custom, UI.Aero.Button.Theme, UI.Aero.black.GameButton,
  UI.Aero.Core.CustomControl, UI.Aero.Labels, ShellApi;

type
  TIntroForm = class(TForm)
    AeroWindow: TAeroWindow;
    BlackGameButton1: TBlackGameButton;
    BPPlabel: TAeroLabel;
    AeroLabel2: TAeroLabel;
    AeroLabel3: TAeroLabel;
    Label2: TAeroLabel;
    procedure BlackGameButton1Click(Sender: TObject);
    procedure Label15Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IntroForm: TIntroForm;

implementation

{$R *.dfm}

procedure TIntroForm.BlackGameButton1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PwideChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
  close;
end;

procedure TIntroForm.Label15Click(Sender: TObject);
begin
  If (Sender is TAeroLabel) then
  with (Sender as TAeroLabel) do
  ShellExecute(Application.Handle,PChar('open'),
  PChar(Hint),
  PChar(0),
  nil,
  SW_NORMAL);
end;

end.
