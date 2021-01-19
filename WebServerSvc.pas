unit WebServerSvc;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Vcl.Dialogs,
  Prefs,
  IOUtils,
  ActiveX,
  WebServerUnit;

type
  TWebServerService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TWebServer;
    fPrefs: TPrefs;
  public
    function GetServiceController: TServiceController; override;
    function PrefFilePath: String;
    { Public declarations }
    property Server: TWebServer read fServer;
    property Prefs: TPrefs read fPrefs;
  end;

var
  WebServerService: TWebServerService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  WebServerService.Controller(CtrlCode);
end;

function TWebServerService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function TWebServerService.PrefFilePath: String;
begin
  Result:=TPath.ChangeExtension(ParamStr(0), '.ini');
end;

procedure TWebServerService.ServiceCreate(Sender: TObject);
begin
  FPrefs:=TPrefs.Create;
  if FileExists(PrefFilePath) then FPrefs.LoadFromFile(PrefFilePath);

  FServer:=TWebServer.Create;;
end;

procedure TWebServerService.ServiceDestroy(Sender: TObject);
begin
  FPrefs.SaveToFile(PrefFilePath);
  FPrefs.Free;
  FServer.Free;
end;

procedure TWebServerService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  CoInitialize(nil);
  fServer.StartWebServer(fPrefs.Port, fPrefs.SSLPrivateKeyFile, fPrefs.SSLPrivateKeyPassword, fPrefs.SSLCertFile);
end;

procedure TWebServerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  fServer.StopWebServer;
  CoUninitialize;
end;

end.
