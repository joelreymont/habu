\ host.f - BROWSER-HOST: the browser host's page served by Habu's HTTP server; the
\ page-module turn protocol is the header of turn.js. The page's six files, beside
\ this one, are read into dictionary buffers as it loads, so an application image
\ serves them with no Habu tree at run time. ROUTES registers their GET routes; the
\ caller registers /module.wasm, every path its module fetches, and starts the server.
\ STORAGE CLASS. PROCESS-WIDE: buffers filled at load and only read after.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/build-input.f
require src/lib/net/http.f

package BROWSER-HOST
private

\ The path of the named file beside this one; it holds only while this file
\ loads.
: PATH ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop SOURCE-ROOT:DIRNAME
   a u SOURCE-ROOT:JOIN ;

\ The file at the path into the buffer of its size; a file that changed size
\ since it was measured is E-FS-IO, or FS:READ-ALL's E-FS-CAPACITY.
: READ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n dst:ptr cap:n :}
   a u PATH {: path:ptr pathu:n :}
   path pathu BUILD-INPUT:FILE
   path pathu dst cap FS:READ-ALL cap <> if E-FS-IO throw then ;

s" index.html" PATH FS:FILE-SIZE constant INDEX-U
INDEX-U BUFFER: INDEX
s" index.html" INDEX INDEX-U READ

s" host.js" PATH FS:FILE-SIZE constant HOST-JS-U
HOST-JS-U BUFFER: HOST-JS
s" host.js" HOST-JS HOST-JS-U READ

s" viewport.js" PATH FS:FILE-SIZE constant VIEWPORT-U
VIEWPORT-U BUFFER: VIEWPORT
s" viewport.js" VIEWPORT VIEWPORT-U READ

s" turn.js" PATH FS:FILE-SIZE constant TURN-U
TURN-U BUFFER: TURN
s" turn.js" TURN TURN-U READ

s" page.js" PATH FS:FILE-SIZE constant PAGE-U
PAGE-U BUFFER: PAGE
s" page.js" PAGE PAGE-U READ

s" mcp.js" PATH FS:FILE-SIZE constant MCP-U
MCP-U BUFFER: MCP
s" mcp.js" MCP MCP-U READ

: SEND ( HTTP:response ptr u8 n ptr u8 n -- )
   {: r:HTTP:response a:ptr u:n type:ptr tu:n :}
   r s" Content-Type" type tu HTTP:HEADER!
   r a u HTTP:BODY! ;

: INDEX-PAGE ( HTTP:request HTTP:response -- )
   nip INDEX INDEX-U s" text/html; charset=utf-8" SEND ;

: HOST-SCRIPT ( HTTP:request HTTP:response -- )
   nip HOST-JS HOST-JS-U s" text/javascript; charset=utf-8" SEND ;

: VIEWPORT-SCRIPT ( HTTP:request HTTP:response -- )
   nip VIEWPORT VIEWPORT-U s" text/javascript; charset=utf-8" SEND ;

: TURN-SCRIPT ( HTTP:request HTTP:response -- )
   nip TURN TURN-U s" text/javascript; charset=utf-8" SEND ;

: PAGE-SCRIPT ( HTTP:request HTTP:response -- )
   nip PAGE PAGE-U s" text/javascript; charset=utf-8" SEND ;

: MCP-SCRIPT ( HTTP:request HTTP:response -- )
   nip MCP MCP-U s" text/javascript; charset=utf-8" SEND ;

public

: ROUTES ( -- )
   s" GET" s" /" [: INDEX-PAGE ;] HTTP:ROUTE
   s" GET" s" /host.js" [: HOST-SCRIPT ;] HTTP:ROUTE
   s" GET" s" /viewport.js" [: VIEWPORT-SCRIPT ;] HTTP:ROUTE
   s" GET" s" /turn.js" [: TURN-SCRIPT ;] HTTP:ROUTE
   s" GET" s" /page.js" [: PAGE-SCRIPT ;] HTTP:ROUTE
   s" GET" s" /mcp.js" [: MCP-SCRIPT ;] HTTP:ROUTE ;

;package
