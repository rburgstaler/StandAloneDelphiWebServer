unit IdSSLOpenSSL_ECDH_Util;

interface

uses
  IdCTypes,
  IdSSLOpenSSLHeaders,
  IdSSLOpenSSL;

/// <summary>
///   Enable automatic curve selection for ECDH key exchange.
/// </summary>
/// <remarks>
///   aContext must be Active before calling.
/// </remarks>
function IdSSLSetECDHAuto(aContext: TIdSSLContext): Boolean;

implementation

type
  THackIdSSLContext = class (TIdSSLContext);

const
  SSL_CTRL_SET_ECDH_AUTO = 94;

function SSL_CTX_set_ecdh_auto(ctx : PSSL_CTX; m : TIdC_LONG) : TIdC_LONG;
 {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
	Result := SSL_CTX_ctrl(ctx,SSL_CTRL_SET_ECDH_AUTO,m,nil);
end;

function IDSSLSetECDHAuto(aContext: TIdSSLContext): Boolean;
var
  lContext: THackIdSSLContext;
begin
  lContext := THackIdSSLContext(aContext);
  Result := SSL_CTX_set_ecdh_auto(lContext.fContext, 1) = 1;
end;

end.
