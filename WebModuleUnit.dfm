object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Name = 'ReverseStringAction'
      PathInfo = '/ReverseString'
    end
    item
      Name = 'ServerFunctionInvokerAction'
      PathInfo = '/ServerFunctionInvoker'
    end
    item
      Default = True
      Name = 'DefaultAction'
      PathInfo = '/'
      OnAction = WebModuleDefaultAction
    end>
  Height = 333
  Width = 414
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
    Server = DSServer1
    Filters = <>
    WebDispatch.PathInfo = 'datasnap*'
    Left = 96
    Top = 75
  end
  object WebFileDispatcher1: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end
      item
        MimeType = 'text/html'
        Extensions = 'htm;html'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpg;jpeg;jpe'
      end
      item
        MimeType = 'image/gif'
        Extensions = 'gif'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end>
    RootDirectory = '../web'
    Left = 96
    Top = 144
  end
end
