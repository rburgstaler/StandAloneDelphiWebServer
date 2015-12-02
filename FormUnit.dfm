object Form1: TForm1
  Left = 271
  Top = 114
  Caption = 'Form1'
  ClientHeight = 235
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 50
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object Label3: TLabel
    Left = 24
    Top = 89
    Width = 94
    Height = 13
    Caption = 'SSL Private Key File'
  end
  object Label4: TLabel
    Left = 24
    Top = 125
    Width = 124
    Height = 13
    Caption = 'SSL Private Key Password'
  end
  object Label5: TLabel
    Left = 24
    Top = 158
    Width = 89
    Height = 13
    Caption = 'SSL Certificate File'
  end
  object ButtonStart: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 105
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object EditPort: TEdit
    Left = 24
    Top = 67
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '8282'
  end
  object ButtonOpenBrowser: TButton
    Left = 24
    Top = 202
    Width = 107
    Height = 25
    Caption = 'Open Browser'
    TabOrder = 3
    OnClick = ButtonOpenBrowserClick
  end
  object EditSSLPrivateKeyFile: TEdit
    Left = 24
    Top = 104
    Width = 345
    Height = 21
    TabOrder = 4
  end
  object EditSSLPrivateKeyPassword: TEdit
    Left = 24
    Top = 138
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object EditSSLCertFile: TEdit
    Left = 24
    Top = 171
    Width = 345
    Height = 21
    TabOrder = 6
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 288
    Top = 24
  end
end
