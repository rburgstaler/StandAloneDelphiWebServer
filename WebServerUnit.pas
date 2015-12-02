unit WebServerUnit;

interface

uses
  Classes,
  SysUtils,
  IdSSLOpenSSL,
  IdHTTPWebBrokerBridge;


type
  TWebServer = class
  private
    FServer: TIdHTTPWebBrokerBridge;
    fSSLPrivateKeyPassword: String;
    procedure TerminateThreads;
    function getActive: Boolean;
    function getPort: Integer;
    procedure GetSSLPassWord(var Password: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartServer(APort: Integer; ASSLPrivateKeyFile, ASSLPrivateKeyPassword, ASSLCertFile: String);
    procedure StopServer;
    property Port: Integer read getPort;
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

procedure TWebServer.GetSSLPassWord(var Password: String);
begin
  Password:=fSSLPrivateKeyPassword;
end;

procedure TWebServer.StartServer(APort: Integer; ASSLPrivateKeyFile, ASSLPrivateKeyPassword, ASSLCertFile: String);
var
  lHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  if FServer.Active and (FServer.DefaultPort<>APort) then StopServer;

  if not FServer.Active then
  begin
    if (ASSLPrivateKeyFile<>'') and (ASSLPrivateKeyPassword<>'') and (ASSLCertFile<>'') then
    begin
      lHandler:=TIdServerIOHandlerSSLOpenSSL.Create;
      lHandler.SSLOptions.Method := sslvSSLv23;
      lHandler.OnGetPassword := GetSSLPassword;
      fSSLPrivateKeyPassword:=ASSLPrivateKeyPassword;
      lHandler.SSLOptions.CertFile:=ASSLCertFile;
      lHandler.SSLOptions.KeyFile:=ASSLPrivateKeyFile;
      FServer.IOHandler:=lHandler;
    end;

    FServer.Bindings.Clear;
    FServer.Bindings.Add.Port:=APort;
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
