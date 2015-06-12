unit WebServerUnit;

interface

uses
  Classes,
  SysUtils,
  IdHTTPWebBrokerBridge;


type
  TWebServer = class
  private
    FServer: TIdHTTPWebBrokerBridge;
    fPort: Integer;
    procedure TerminateThreads;
    function getActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartServer;
    procedure StopServer;
    property Port: Integer read fPort write fPort;
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

procedure TWebServer.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := Port;
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
