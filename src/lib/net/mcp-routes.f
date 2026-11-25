\ mcp-routes.f - the route a host's MCP server has behind bearer tokens: POST
\ /mcp for the callers BEARER admits, and the protected-resource metadata
\ routes. Before HTTP:START the host states BEARER:ISSUER!, BEARER:METADATA!,
\ BEARER:APP! for each app that may call as itself and ORIGIN! for each origin
\ whose pages may call, then gives ROUTES the quotation that calls HANDLE in
\ the host's own context.
\ STORAGE CLASS. PROCESS-WIDE: the origins and the quotation, written before
\ HTTP serves.

require src/lib/string.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/reason.f
require src/lib/utf8-scalar.f
require src/lib/uri.f
require src/lib/net/http.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/net/http-router.f
require src/lib/net/bearer.f
require src/lib/net/mcp.f

package MCP
private
EXPORT BEARER:WORD-FROM?
EXPORT BEARER:WORD+

\ The origins ORIGIN! lists, one space apart; made at the first ORIGIN!, so a
\ built program keeps none.
TYPED-VARIABLE ORIGINS ^VEC:vector<u8> SCRATCH-ONLY

TYPED-VARIABLE ROUTE-BODY [ HTTP:request HTTP:response -- ] SCRATCH-ONLY

: ORIGINS$ ( -- str )
   ORIGINS @ dup NULL-PTR = if drop s" " exit then
   VEC:BYTES ;

: ORIGINS-READY ( -- )
   ORIGINS @ NULL-PTR <> if exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ORIGINS ! ;

: ORIGIN-ERROR ( str -- )
   {: s:str :}
   s REASON:QUOTE-MOST UTF8:FIT {: kept:n :}
   REASON:OPEN s" origin " REASON:APPEND
   s STR:DATA kept >str kept s STR:LEN < REASON:QUOTE
   s" : " REASON:APPEND REASON:EXPECTED
   s" http or https, a host and an optional port, alone" REASON:APPEND
   REASON:FIX
   s" write it as a browser's Origin header does, like `https://chat.example`"
   REASON:APPEND
   E-SCHEMA REASON:STOP ;

\ Whether the request's page, if it names one, may call. A browser sends one
\ Origin; a request with several is no browser's and names no one page.
: MAY-CALL? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" Origin" HTTP:HEADER-COUNT-OF {: count:n :}
   count 0= if true exit then
   count 1 > if false exit then
   asked s" Origin" HTTP:HEADER-OF$ drop {: origin:str :}
   ORIGINS$ origin 0 WORD-FROM? ;

: POSTED ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked MAY-CALL? 0= if
      answer 403 s" origin_rejected"
      s" a page of this origin may not call" HTTP:ERROR! exit
   then
   asked answer BEARER:CALLER? 0= if exit then
   asked answer ROUTE-BODY @ execute ;

public

\ Lists an origin whose pages may call, beside those listed before, as the
\ Origin header a browser sends spells it: they are compared byte for byte.
\ ROUTES answers no CORS preflight, so a browser calls from the server's own
\ origin alone. The text is copied. A built program keeps none: it states them
\ at run time. E-STATE while HTTP serves; E-SCHEMA for a text that is not an
\ http(s) origin alone.
: ORIGIN! ( str -- )
   {: origin:str :}
   HTTP:RUNNING? if E-STATE throw then
   origin URI:ORIGIN? 0= if origin ORIGIN-ERROR then
   ORIGINS-READY
   origin ORIGINS @ WORD+ ;


\ Adds the POST route of /mcp and BEARER:METADATA-ROUTES. A request with no
\ Origin, or one ORIGIN! lists, that BEARER:CALLER? admits runs the quotation,
\ which answers it through HANDLE; any other Origin is 403 origin_rejected. A
\ built program keeps no route: it adds them at run time. E-STATE while HTTP
\ serves; E-HTTP-STATE, nothing added, with no METADATA! URL stated.
: ROUTES ( [ HTTP:request HTTP:response -- ] -- )
   {: body :}
   HTTP:RUNNING? if E-STATE throw then
   BEARER:METADATA-ROUTES
   body ROUTE-BODY !
   s" POST" s" /mcp" [: POSTED ;] HTTP:ROUTE ;

;package
