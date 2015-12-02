program StandAloneWebServer;
{$APPTYPE GUI}



uses
  Vcl.Forms,
  Web.WebReq,
  SvcMgr,
  ServiceUtils,
  FormUnit in 'FormUnit.pas' {Form1},
  WebModuleUnit in 'WebModuleUnit.pas' {WebModule1: TWebModule},
  WebServerUnit in 'WebServerUnit.pas',
  WebServerSvc in 'WebServerSvc.pas' {WebServerService: TService},
  Prefs in 'Prefs.pas';

{$R *.res}

begin
  // STANDARD SERVICE APPLICATION COMMENT:
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //

  // First initialize the service application, which needs to occur whether its running
  // as a service or a user application
  if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
    Vcl.SvcMgr.Application.Initialize;

  // Always create the service module because it's used even if the form is created
  if (not (ApplicationType in [TApplicationType.atService])) and (not Vcl.SvcMgr.Application.Installing) then
  begin
    // If being run as a user app and not with the -install/-uninstall flags then
    // run the Vcl.Forms.Appication object
    //Vcl.Forms.Application.MainFormOnTaskbar := True;
    Vcl.Forms.Application.CreateForm(TWebServerService, WebServerService);
  Vcl.Forms.Application.ShowMainForm := True;
    Vcl.Forms.Application.CreateForm(TForm1, Form1);
    Vcl.Forms.Application.Run;
  end
  else
  begin
    // Otherwise run the TServiceApplication object to start up the service
    Vcl.SvcMgr.Application.CreateForm(TWebServerService, WebServerService);
    Vcl.SvcMgr.Application.Run;
  end;
end.
