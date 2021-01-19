unit ServiceUtilsU;

interface

uses
  System.Types, System.Classes, Winapi.Windows, System.UITypes, System.SysUtils, Winapi.WinSvc,

  Vcl.Forms,
  Vcl.SvcMgr,
  Winapi.ShellAPI,
  Vcl.Dialogs;

type
  TServiceStatus = (ssNotInstalled,ssStarting,ssRunning,ssStopping,ssStopped);
  TApplicationType = (atUnknown, atService, atDesktopUserInteractive, atDesktopNonUserInteractive);

function UserInteractive: Boolean;

function GetServiceStatus(AName: string) : TServiceStatus;
function GetServiceExecutablePath(strMachine: string; strServiceName: string): String;
function StopServiceAndWait(AName: string; ATimeout: Integer): Boolean;
function StartService(AName: string): Boolean;

  //Event Types
  //0x0000: EVENTLOG_SUCCESS - Information event
  //0x0010: EVENTLOG_AUDIT_FAILURE - Failure Audit event
  //0x0008: EVENTLOG_AUDIT_SUCCESS - Success Audit event
  //0x0001: EVENTLOG_ERROR_TYPE - Error event
  //0x0004: EVENTLOG_INFORMATION_TYPE - Information event
  //0x0002: EVENTLOG_WARNING_TYPE - Warning event
procedure LogWindowsEvent(AName: String; Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);

//Returns how the application is currently being run.  Can determine whether the application
//is running as a Windows service or not
function ApplicationType: TApplicationType;

const
  cParamNameServiceName = 'name';
  cParamNameServiceHelp = 'help';

var
  gServiceWasRunning: Boolean;

resourcestring
  strServiceRunning = 'The %s service is already running and must be stopped in order to use the application interactively.'#13#10#13#10'After closing the application, the service will be restarted.';
  strServiceRestarting = 'Restart the %s service now?';

  strServiceHelp = 'This application is designed to be both run as a regular interactive Windows program or installed as a Windows service that runs in the background.'#13#10#13#10+
                   'Use the following command-line parameters to set it up as a service.'#13#10#13#10+
                   '  -install - Register the application as a Windows service.'#13#10#13#10+
                   '  -uninstall - Unregister the application as a Windows service.'#13#10#13#10+
                   '  -name <Service Name> - Choose an alternate name for installing/uninstalling as a Windows service.  Default service name is "%s"'#13#10#13#10+
                   '  -skipprompts - Does not show dialog boxes for starting and stopping the service when trying to run the application interactively.'#13#10#13#10+
                   '%s';

implementation

uses
  Winapi.TlHelp32,
  Winapi.PsAPI,
  System.StrUtils,
  System.Win.Registry;


var
  gServiceName: String;
  gApplicationType: TApplicationType = TApplicationType.atUnknown;   //Cached / Lazy load of the application type for faster access

function QuoteIfNeeded(AStr: String): String;
begin
  Result:=AStr;
  if Pos(' ', AStr)>0 then Result:='"'+AStr+'"';
end;


function UserInteractive: Boolean;
var
  WinStation: HWINSTA;
  Flags: USEROBJECTFLAGS;
  Needed: Cardinal;
begin
  // Return whether the window station that this process is running under is
  // interactive (run by a user) or non-interactive (started as a service)
  WinStation := GetProcessWindowStation;
  GetUserObjectInformation(WinStation,UOI_FLAGS,@Flags,SizeOf(Flags),Needed);
  Result := Flags.dwFlags and WSF_VISIBLE = WSF_VISIBLE;
end;

function ApplicationType: TApplicationType;
var
  HandleSnapShot  : THandle;
  EntryProc : TProcessEntry32;
  CurrentProcessId: DWORD;
  ParentProcessId : DWORD;
  lProcesses: TStringList;
  lIdx: Integer;

begin
  //If we have already found the application type then we do not need to look it up again
  if gApplicationType=TApplicationType.atUnknown then
  begin
    //First determine if this app is being run as a service or not
    lProcesses:=TStringList.Create;
    try
      ParentProcessId:=0;
      HandleSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);   //enumerate the process
      if HandleSnapShot <> INVALID_HANDLE_VALUE then
      begin
        try
          EntryProc.dwSize := SizeOf(EntryProc);
          if Process32First(HandleSnapShot, EntryProc) then    //find the first process
          begin
            CurrentProcessId := GetCurrentProcessId(); //get the id of the current process
            repeat
              lProcesses.AddObject(EntryProc.szExeFile, TObject(EntryProc.th32ProcessID));
              if EntryProc.th32ProcessID = CurrentProcessId then ParentProcessId := EntryProc.th32ParentProcessID; //get the id of the parent process
            until not Process32Next(HandleSnapShot, EntryProc);
          end;
        finally
          CloseHandle(HandleSnapShot);
        end;
      end;

      //If we found the parent's process id, then determine whether it is the service
      //runner or not
      if ParentProcessId<>0 then
      begin
        lIdx:=lProcesses.IndexOfObject(TObject(ParentProcessId));
        if (lIdx>-1) and AnsiMatchText(lProcesses[lIdx], ['services.exe', 'winlogon.exe', 'wininit.exe']) then gApplicationType:=TApplicationType.atService;
      end;

      //If the type is still unknown, then we assume it is a desktop but the question is...
      //Is it user interactive or not???
      if gApplicationType=TApplicationType.atUnknown then
      begin
        if UserInteractive then gApplicationType:=TApplicationType.atDesktopUserInteractive
        else gApplicationType:=TApplicationType.atDesktopNonUserInteractive
      end;
    finally
      lProcesses.Free;
    end;

  end;
  Result:=gApplicationType;
end;

function GetServiceStatus(AName: string) : TServiceStatus;
var
  scm,svc: SC_HANDLE;
  stat: SERVICE_STATUS;
begin
  Result := ssNotInstalled;

  scm := openscmanager(nil,nil,SC_MANAGER_CONNECT);
  if scm <> 0 then
  begin
    try
      svc := openservice(scm,pchar(AName),SERVICE_QUERY_STATUS);
      if svc <> 0 then begin
        try
          if queryservicestatus(svc,stat) then
          begin
            case stat.dwCurrentState of
              SERVICE_STOPPED : result := ssStopped;
              SERVICE_START_PENDING : result := ssStarting;
              SERVICE_STOP_PENDING: result := ssStopping;
              SERVICE_RUNNING : result := ssRunning;
            end;
          end;
        finally
          CloseServiceHandle(svc);
        end;
      end;
    finally
      CloseServiceHandle(scm);
    end;
  end;
end;

function GetServiceExecutablePath(strMachine: string; strServiceName: string): String;
var
  hSCManager,hSCService: SC_Handle;
  lpServiceConfig: LPQUERY_SERVICE_CONFIGW;
  nSize, nBytesNeeded: DWord;
begin
  Result := '';
  hSCManager := OpenSCManager(PChar(strMachine), nil, SC_MANAGER_CONNECT);
  if (hSCManager > 0) then
  begin
    hSCService := OpenService(hSCManager, PChar(strServiceName), SERVICE_QUERY_CONFIG);
    if (hSCService > 0) then
    begin
      QueryServiceConfig(hSCService, nil, 0, nSize);
      lpServiceConfig := AllocMem(nSize);
      try
        if not QueryServiceConfig(
          hSCService, lpServiceConfig, nSize, nBytesNeeded) Then Exit;
          Result := String(lpServiceConfig^.lpBinaryPathName);
      finally
        Dispose(lpServiceConfig);
      end;
      CloseServiceHandle(hSCService);
    end;
  end;
end;

function StopServiceAndWait(AName: string; ATimeout: Integer): Boolean;
var
  scm,svc: SC_HANDLE;
  ph: Cardinal;
  stat: Winapi.WinSvc.TServiceStatus;
  info: SERVICE_STATUS_PROCESS;
  Size: Cardinal;
begin
  Result := False;

  scm := OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
  if (scm <> 0) then
  begin
    try
      svc := OpenService(scm,PChar(AName),SERVICE_STOP or SERVICE_QUERY_STATUS);
      if svc <> 0 then
      begin
        try
          if QueryServiceStatusEx(svc,SC_STATUS_PROCESS_INFO,@info,SizeOf(info),Size) then
          begin
            if (ControlService(svc,SERVICE_CONTROL_STOP,stat)) then
            begin
              // Wait for it to exit
              ph := OpenProcess(SYNCHRONIZE, FALSE, info.dwProcessId);
              if ph <> 0 then
              begin
                try
                  if WaitForSingleObject(ph,ATimeout) = WAIT_OBJECT_0 then
                    Result := True;
                finally
                  CloseHandle(ph);
                end;
              end;
            end;
          end;
        finally
          CloseServiceHandle(svc);
        end;
      end;
    finally
      CloseServiceHandle(scm);
    end;
  end;
end;

function StartService(AName: string): Boolean;
var
  scm,svc: SC_HANDLE;
  tmp: PChar;
begin
  Result := False;

  scm := OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
  if (scm <> 0) then
  begin
    try
      svc := OpenService(scm,PChar(AName),SERVICE_START);
      if svc <> 0 then
      begin
        try
          if Winapi.WinSvc.StartService(svc,0,tmp) then
          begin
            Result := True;
          end;
        finally
          CloseServiceHandle(svc);
        end;
      end;
    finally
      CloseServiceHandle(scm);
    end;
  end;
end;

procedure LogWindowsEvent(AName: String; Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);
begin
  with TEventLogger.Create(AName) do
  begin
    try
      LogMessage(Message, EventType, Category, ID);
    finally
      Free;
    end;
  end;
end;

end.
