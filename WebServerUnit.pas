unit WebServerUnit;

interface

uses
  Classes,
  SysUtils,
  IdSSLOpenSSL,
  IdGlobal,
  IdSSL,
  IdHTTPWebBrokerBridge;


type
  TWebServer = class
  private
    FServer: TIdHTTPWebBrokerBridge;
    fSSLPrivateKeyPassword: String;
    fSSLEnabled: Boolean;
    procedure TerminateThreads;
    function getActive: Boolean;
    function getPort: Integer;
    procedure GetSSLPassWord(var Password: String);
    procedure OnHTTPQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
    function getSecure: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartServer(APort: Integer; ASSLPrivateKeyFile, ASSLPrivateKeyPassword, ASSLCertFile: String);
    procedure StopServer;
    property Port: Integer read getPort;
    property Secure: Boolean read getSecure;
    property Active: Boolean read getActive;
  end;

implementation

uses
  DataSnap.DSSession;

{ TWebServer }

constructor TWebServer.Create;
begin
  inherited;
  FServer:=TIdHTTPWebBrokerBridge.Create(nil);
  FServer.OnQuerySSLPort:=OnHTTPQuerySSLPort;
end;

destructor TWebServer.Destroy;
begin
  StopServer;
  FServer.Free;
  inherited;
end;

function TWebServer.getActive: Boolean;
begin
  Result:=fServer.Active;
end;

function TWebServer.getPort: Integer;
begin
  if FServer.Bindings.Count > 0 then
    Result := FServer.Bindings[0].Port
  else
    Result := -1;
end;

function TWebServer.getSecure: Boolean;
begin
  Result:=FServer.Active and (FServer.IOHandler is TIdServerIOHandlerSSLBase);
end;

procedure TWebServer.GetSSLPassWord(var Password: String);
begin
  Password:=fSSLPrivateKeyPassword;
end;

procedure TWebServer.OnHTTPQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  VUseSSL:=fSSLEnabled;
end;

procedure TWebServer.StartServer(APort: Integer; ASSLPrivateKeyFile, ASSLPrivateKeyPassword, ASSLCertFile: String);
var
  lHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  if FServer.Active and (FServer.DefaultPort<>APort) then StopServer;

  fSSLEnabled:=(ASSLPrivateKeyFile<>'') and (ASSLCertFile<>'');
  if not FServer.Active then
  begin
    lHandler := nil;
    if fSSLEnabled then
    begin
      lHandler:=TIdServerIOHandlerSSLOpenSSL.Create;
      lHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
      lHandler.OnGetPassword := GetSSLPassword;
      fSSLPrivateKeyPassword:=ASSLPrivateKeyPassword;
      lHandler.SSLOptions.CertFile:=ASSLCertFile;
      lHandler.SSLOptions.KeyFile:=ASSLPrivateKeyFile;
      FServer.IOHandler:=lHandler;
    end;

    fServer.Bindings.Clear;
    fServer.DefaultPort := APort;  // if we do not set the default port, and we only set fServer.Bindings.Add.Port := aPort, then it is 3 time slower for some reason.  I don't know why, it just is.

    FServer.Active := True;
  end;
end;

procedure TWebServer.StopServer;
begin
  TerminateThreads;
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TWebServer.TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

end.
