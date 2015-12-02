object WebServerService: TWebServerService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'WebServerService'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 327
  Width = 404
end
