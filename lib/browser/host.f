\ host.f - BROWSER-HOST: the browser host's page served by Habu's HTTP server
\ (docs/browser-host.md).
\
\ The page is three files beside this one: index.html, host.js and turn.js.
\ Each is read as this file loads, from the tree that resolved it, into a
\ dictionary buffer of the file's size, so an application image built from a
\ program that requires this file carries the page and serves it with no Habu
\ tree at run time. ROUTES registers GET /, /host.js and /turn.js on HTTP's
\ route table with their content types; the caller registers its own routes,
\ /module.wasm and every path its module fetches, and starts the server.
\
\ STORAGE CLASS. PROCESS-WIDE: the page's buffers, filled as this file loads
\ and only read after it, so every request worker serves them at once.

require lib/errors.f
require lib/fs.f
require lib/net/http.f

package BROWSER-HOST
private

\ A file's path in the tree that resolved this file: SOURCE-ROOT:CURRENT$
\ while it loads.
: PATH ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SOURCE-ROOT:CURRENT$ a u SOURCE-ROOT:JOIN ;

\ The file at the path into the buffer of its size; a file that changed size
\ since it was measured is E-FS-IO, or READ-ALL's E-FS-CAPACITY.
: READ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n dst:ptr cap:n :}
   a u PATH dst cap READ-ALL cap <> if E-FS-IO throw then ;

s" lib/browser/index.html" PATH FILE-SIZE constant INDEX-U
INDEX-U BUFFER: INDEX
s" lib/browser/index.html" INDEX INDEX-U READ

s" lib/browser/host.js" PATH FILE-SIZE constant HOST-U
HOST-U BUFFER: HOST
s" lib/browser/host.js" HOST HOST-U READ

s" lib/browser/turn.js" PATH FILE-SIZE constant TURN-U
TURN-U BUFFER: TURN
s" lib/browser/turn.js" TURN TURN-U READ

: SEND ( HTTP:response ptr u8 n ptr u8 n -- )
   {: r:HTTP:response a:ptr u:n type:ptr tu:n :}
   r s" Content-Type" type tu HTTP:HEADER!
   r a u HTTP:BODY! ;

: INDEX-PAGE ( HTTP:request HTTP:response -- )
   nip INDEX INDEX-U s" text/html; charset=utf-8" SEND ;

: HOST-SCRIPT ( HTTP:request HTTP:response -- )
   nip HOST HOST-U s" text/javascript; charset=utf-8" SEND ;

: TURN-SCRIPT ( HTTP:request HTTP:response -- )
   nip TURN TURN-U s" text/javascript; charset=utf-8" SEND ;

public

: ROUTES ( -- )
   s" GET" s" /" [: INDEX-PAGE ;] HTTP:ROUTE
   s" GET" s" /host.js" [: HOST-SCRIPT ;] HTTP:ROUTE
   s" GET" s" /turn.js" [: TURN-SCRIPT ;] HTTP:ROUTE ;

;package
