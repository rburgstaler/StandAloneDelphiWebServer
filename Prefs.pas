unit Prefs;

interface

uses
  Classes,
  SysUtils,
  IniFiles;

type
  TPrefs = class
  private
    fSSLPrivateKeyPassword: String;
    fPort: Integer;
    fSSLPrivateKeyFile: String;
    fSSLCertFile: String;
  public
    procedure LoadFromFile(AFileName: String);
    procedure SaveToFile(AFileName: String);
  published
    property Port: Integer read fPort write fPort;
    property SSLPrivateKeyFile: String read fSSLPrivateKeyFile write fSSLPrivateKeyFile;
    property SSLPrivateKeyPassword: String read fSSLPrivateKeyPassword write fSSLPrivateKeyPassword;
    property SSLCertFile: String read fSSLCertFile write fSSLCertFile;
  end;

implementation

{ TPrefs }

procedure TPrefs.LoadFromFile(AFileName: String);
var
  lIniFile: TIniFile;
begin
  lIniFile:=TIniFile.Create(AFileName);
  try
    fPort:=lIniFile.ReadInteger('Config', 'Port', 0);
    fSSLPrivateKeyFile:=lIniFile.ReadString('Config', 'SSLPrivateKeyFile', '');
    fSSLPrivateKeyPassword:=lIniFile.ReadString('Config', 'SSLPrivateKeyPassword', '');
    fSSLCertFile:=lIniFile.ReadString('Config', 'SSLCertFile', '');
  finally
    lIniFile.Free;
  end;
end;

procedure TPrefs.SaveToFile(AFileName: String);
var
  lIniFile: TIniFile;
begin
  lIniFile:=TIniFile.Create(AFileName);
  try
    lIniFile.WriteInteger('Config', 'Port', fPort);
    lIniFile.WriteString('Config', 'SSLPrivateKeyFile', fSSLPrivateKeyFile);
    lIniFile.WriteString('Config', 'SSLPrivateKeyPassword', fSSLPrivateKeyPassword);
    lIniFile.WriteString('Config', 'SSLCertFile', fSSLCertFile);
  finally
    lIniFile.Free;
  end;
end;

end.
