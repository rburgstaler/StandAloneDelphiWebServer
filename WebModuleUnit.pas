unit WebModuleUnit;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer,
  Web.WebFileDispatcher, Web.HTTPProd,
  DSAuth,
  Datasnap.DSProxyJavaScript, IndyPeerImpl, Datasnap.DSClientMetadata,
  Datasnap.DSCommonServer, IPPeerServer,
  Datasnap.DSMetadata, Datasnap.DSServerMetadata, Datasnap.DSHTTP;

type
  TWebModule1 = class(TWebModule)
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    DSServer1: TDSServer;
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  Web.WebReq;

{$R *.dfm}

procedure TWebModule1.WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.Content := 'Hello world'
  else
    Response.SendRedirect(Request.InternalScriptName + '/');
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.

