unit WebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Datasnap.DSHTTPWebBroker,
  Datasnap.DSServer,
  Datasnap.DSCommonServer,
  IPPeerServer,
  Datasnap.DSHTTP,
  IdHTTPWebBrokerBridge;

type
  TWebModule1 = class(TWebModule)
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    DSServer1: TDSServer;
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1GoAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

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

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  Web.WebReq,
  DataSnap.DSSession;

{$R *.dfm}

procedure TWebModule1.WebModule1GoAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  //

  Response.Content:='Hello... from the server: '+Request.Content;
end;

procedure TWebModule1.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.SendRedirect('/Main.html')
  else
    Response.SendRedirect(Request.InternalScriptName + '/');
end;

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

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

