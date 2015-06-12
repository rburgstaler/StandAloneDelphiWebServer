unit FormUnit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.AppEvnts,
  Vcl.StdCtrls,
  IdHTTPWebBrokerBridge,
  WebModuleUnit;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TWebServer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Winapi.ShellApi, Datasnap.DSService;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  ButtonStartClick(nil);
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0, nil, PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  fServer.Port:=StrToInt(EditPort.Text);
  fServer.StartServer;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  fServer.StopServer;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer:=TWebServer.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FServer.Free;
end;

end.
