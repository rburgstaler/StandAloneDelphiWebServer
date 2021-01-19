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
  WebServerUnit;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    EditSSLPrivateKeyFile: TEdit;
    EditSSLPrivateKeyPassword: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    EditSSLCertFile: TEdit;
    Label5: TLabel;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Winapi.ShellApi,
  WebServerSvc;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not WebServerService.Server.Active;
  ButtonStop.Enabled := WebServerService.Server.Active;
  EditPort.Enabled := not WebServerService.Server.Active;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
  lProtocol: String;
begin
  if WebServerService.Server.Secure then lProtocol:='https'
  else lProtocol:='http';
  ButtonStartClick(nil);
  LURL := Format('%s://localhost:%d', [lProtocol, WebServerService.Server.Port]);
  ShellExecute(0, nil, PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  WebServerService.Server.StartWebServer(StrToInt(EditPort.Text), EditSSLPrivateKeyFile.Text, EditSSLPrivateKeyPassword.Text, EditSSLCertFile.Text);
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  WebServerService.Server.StopWebServer;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EditPort.Text:=IntToStr(WebServerService.Prefs.Port);
  EditSSLPrivateKeyFile.Text:=WebServerService.Prefs.SSLPrivateKeyFile;
  EditSSLPrivateKeyPassword.Text:=WebServerService.Prefs.SSLPrivateKeyPassword;
  EditSSLCertFile.Text:=WebServerService.Prefs.SSLCertFile;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  WebServerService.Prefs.Port:=StrToIntDef(EditPort.Text, 0);
  WebServerService.Prefs.SSLPrivateKeyFile:=EditSSLPrivateKeyFile.Text;
  WebServerService.Prefs.SSLPrivateKeyPassword:=EditSSLPrivateKeyPassword.Text;
  WebServerService.Prefs.SSLCertFile:=EditSSLCertFile.Text;
end;

end.
