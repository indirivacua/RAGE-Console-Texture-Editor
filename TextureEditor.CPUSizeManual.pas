unit TextureEditor.CPUSizeManual;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TFormCPUSizeManual = class(TForm)
    CPUSegmentSize: TRadioGroup;
    btnOK: TButton;
    edtCustomSize: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure CPUSegmentSizeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCPUSizeManual: TFormCPUSizeManual;

implementation

uses MainUnit;

{$R *.dfm}

procedure TFormCPUSizeManual.btnOKClick(Sender: TObject);
begin
  case CPUSegmentSize.ItemIndex of
    0: dwCPUSize:=1024;
    1: dwCPUSize:=2048;
    2: dwCPUSize:=4096;
    3: dwCPUSize:=8192;
    4: dwCPUSize:=16348;
    5: dwCPUSize:=StrToInt(edtCustomSize.Text);
  end;
  Close;
end;

procedure TFormCPUSizeManual.CPUSegmentSizeClick(Sender: TObject);
begin
  if CPUSegmentSize.ItemIndex = 5 then edtCustomSize.Enabled:=true else edtCustomSize.Enabled:=false;
end;

procedure TFormCPUSizeManual.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  btnOKClick(Sender);
end;

end.
