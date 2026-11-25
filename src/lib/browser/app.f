\ app.f - MCP-APP: an MCP App's view (ext-apps "2026-01-26") of a Habu Wasm
\ module, in two parts. The kept page, PAGE+, is app.html and app.js, all a
\ host keeps between its reads of the ui:// resource: app.html with the title
\ HTML-escaped at its <!--title--> marker, the page id at <!--page-id--> and
\ app.js as base64 at <!--app.js-->. Everything that changes with a release
\ comes from the server each time the page starts: after
\ ui/notifications/initialized the page calls the app-only tool CODE-TOOL$
\ with arguments {}, and CODE+ is that result's _meta, the page id, the code
\ id and view.html, view.js, turn.js, viewport.js, page.js, socket.js and the
\ module as base64. A page whose id is not the _meta's is older than its
\ server and does not start. The page id is the SHA-256, in lowercase hex, of
\ app.html's and app.js's bytes, so it changes exactly when app.html or
\ app.js does. The page fetches nothing and runs under ChatGPT's view CSP:
\ script-src 'self' 'wasm-unsafe-eval' 'unsafe-inline' 'unsafe-eval' blob:;
\ worker-src blob:; connect-src 'self'; img-src 'self' data:, so page.js
\ shows an SVG from a data: URL. A page's descriptor may list
\ connectDomains, which the host adds to connect-src. The eight files, beside
\ this one, are read into dictionary buffers as it loads. A server whose view
\ is its own view.js, not this one, answers CODE-TOOL$ with a _meta of its own
\ making, {"page":PAGE-ID$,"files":{FILE+ ...}}, whose view.js app.js starts.
\ STORAGE CLASS. PROCESS-WIDE: buffers filled at load and only read after;
\ PAGE+, FILE+, CODE-ID+ and CODE+ write only the caller's vector.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/arena.f
require src/lib/base64.f
require src/lib/xml.f
require src/lib/xml/write.f

package MCP-APP
private

\ The path of the named file beside this one; it holds only while this file
\ loads.
: PATH ( str -- str )
   {: s:str :}
   SOURCE-ROOT:ACTIVE-SOURCE drop drop SOURCE-ROOT:DIRNAME
   s SOURCE-ROOT:JOIN ;


\ A file that changed size since it was measured is E-FS-IO, or E-FS-CAPACITY.
: READ ( str ^u8 n -- )
   {: s:str dst:^ cap:n :}
   s PATH {: path:str :}
   path dst cap FS:READ-ALL cap <> if E-FS-IO throw then ;

s" app.html" PATH FS:FILE-SIZE constant HTML-U
HTML-U BUFFER: HTML
s" app.html" HTML HTML-U READ

s" app.js" PATH FS:FILE-SIZE constant APP-U
APP-U BUFFER: APP
s" app.js" APP APP-U READ

s" view.html" PATH FS:FILE-SIZE constant VIEW-HTML-U
VIEW-HTML-U BUFFER: VIEW-HTML
s" view.html" VIEW-HTML VIEW-HTML-U READ

s" view.js" PATH FS:FILE-SIZE constant VIEW-JS-U
VIEW-JS-U BUFFER: VIEW-JS
s" view.js" VIEW-JS VIEW-JS-U READ

s" viewport.js" PATH FS:FILE-SIZE constant VIEWPORT-U
VIEWPORT-U BUFFER: VIEWPORT
s" viewport.js" VIEWPORT VIEWPORT-U READ

s" turn.js" PATH FS:FILE-SIZE constant TURN-U
TURN-U BUFFER: TURN
s" turn.js" TURN TURN-U READ

s" page.js" PATH FS:FILE-SIZE constant PAGE-U
PAGE-U BUFFER: PAGE
s" page.js" PAGE PAGE-U READ

s" socket.js" PATH FS:FILE-SIZE constant SOCKET-U
SOCKET-U BUFFER: SOCKET
s" socket.js" SOCKET SOCKET-U READ

\ The page id: the lowercase hex SHA-256 of app.html's bytes, then app.js's.
$40 constant ID-U
ID-U BUFFER: ID
SHA256-CTX-BYTES SCRATCH-BUFFER: SHA
$20 SCRATCH-BUFFER: DIGEST
SHA SHA256-BEGIN
SHA HTML HTML-U >str SHA256-FEED
SHA APP APP-U >str SHA256-FEED
SHA DIGEST SHA256-END
DIGEST ID SHA256>HEX

\ The offset of the marker in app.html at or after the offset; each marker
\ follows the one before it.
: MARK ( str n -- n )
   {: m:str from:n :}
   HTML HTML-U >str m from STR:FIND-SUB-FROM MATCH option
      none OF
         s" hb: src/lib/browser/app.html lacks a marker in order" EXIT-STATUS:INTERNAL die
      ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;


: TITLE-MARK ( -- str )  s" <!--title-->" ;
: ID-MARK ( -- str )  s" <!--page-id-->" ;
: APP-MARK ( -- str )  s" <!--app.js-->" ;

TITLE-MARK 0 MARK constant TITLE-AT
TITLE-AT TITLE-MARK STR:LEN + constant TITLE-END
ID-MARK TITLE-END MARK constant ID-AT
ID-AT ID-MARK STR:LEN + constant ID-END
APP-MARK ID-END MARK constant APP-AT
APP-AT APP-MARK STR:LEN + constant APP-END

: GAP+ ( n n ^VEC:vector<u8> -- )
   {: from:n to:n v:^ :}
   HTML from + to from - >str v VEC:APPEND ;


\ The n zero bytes appended to the vector.
: TAIL+ ( ^VEC:vector<u8> n -- SPAN:span<u8> )
   {: v:^ k:n :}
   v VEC:COUNT {: old:n :}
   v old k + VEC:ENSURE
   v VEC:SPAN old SPAN:SKIP ;


\ The bytes are read after the vector grows, so it must not hold them.
: BASE64+ ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   v s STR:LEN 2 + 3 / 4 * TAIL+ {: out :}
   s out BASE64:ENCODE drop ;


\ The escaped title and the vector.
: COMPOSE ( str ^VEC:vector<u8> -- )
   {: t:str v:^ :}
   0 TITLE-AT v GAP+  t v VEC:APPEND
   TITLE-END ID-AT v GAP+  ID ID-U >str v VEC:APPEND
   ID-END APP-AT v GAP+  APP APP-U >str v BASE64+
   APP-END HTML-U v GAP+ ;


\ Escapes the title into a scratch vector before the page vector is touched,
\ so a title ESCAPE-TEXT rejects leaves the page vector as it was.
: ESCAPED ( str ^VEC:vector<u8> ^ARENA:arena -- )
   ARENA:ALLOCATOR {: t:str v:^ al:^ :}
   al 0 [type] u8 VEC:NEW {: s:^ :}
   t s XML:ESCAPE-TEXT
   s VEC:BYTES v COMPOSE ;


\ CODE-ID+'s files, in its order, fed to the SHA-256 context.
: FILES-FED ( ^u8 -- )
   {: sha:^ :}
   sha VIEW-HTML VIEW-HTML-U >str SHA256-FEED
   sha VIEW-JS VIEW-JS-U >str SHA256-FEED
   sha TURN TURN-U >str SHA256-FEED
   sha VIEWPORT VIEWPORT-U >str SHA256-FEED
   sha PAGE PAGE-U >str SHA256-FEED
   sha SOCKET SOCKET-U >str SHA256-FEED ;


\ The scratch bytes hold the SHA-256 context, then the digest; the vector
\ grows only once the digest is done.
: HASHED ( str ^VEC:vector<u8> ^u8 NUM:alloc-byte-len -- )
   drop {: m:str v:^ sha:^ :}
   sha SHA256-BEGIN  sha m SHA256-FEED  sha FILES-FED
   sha SHA256-CTX-BYTES + {: dg:^ :}
   sha dg SHA256-END
   v ID-U TAIL+ SPAN:$ STR:DATA {: hex:^ :}
   dg hex SHA256>HEX ;

public

\ The page id, PAGE+'s page's: the SHA-256 of app.html's bytes, then app.js's,
\ as 64 lowercase hex digits.
: PAGE-ID$ ( -- str )  ID ID-U >str ;

\ A file's name, its bytes and the vector a member of a CODE-TOOL$ result's
\ files is appended to: "<name>":"<the bytes as base64>". The name is
\ written as it is, so it must be printable ASCII with no " or \. The vector
\ must hold neither the name nor the bytes.
: FILE+ ( str str ^VEC:vector<u8> -- )
   {: name:str s:str v:^ :}
   s\" \"" v VEC:APPEND  name v VEC:APPEND
   s\" \":\"" v VEC:APPEND  s v BASE64+  s\" \"" v VEC:APPEND ;


\ The title and the vector the page is appended to; a title that is not XML
\ text is rejected and appends nothing.
: PAGE+ ( str ^VEC:vector<u8> -- )
   [: ESCAPED ;] ARENA:WITH ;


\ The app-only tool the page calls, with arguments {}, for its code.
: CODE-TOOL$ ( -- str )  s" view_code" ;

\ The app-only tool the view calls, with arguments {}, for its socket; its
\ result's _meta is {"url":"<the socket's ws: or wss: URL>","seconds":<n>},
\ the seconds after which the view asks for the URL again.
: SOCKET-TOOL$ ( -- str )  s" view_socket" ;

\ The module's bytes and the vector their code id is appended to: the SHA-256,
\ as 64 lowercase hex digits, of the module's bytes, then view.html's,
\ view.js's, turn.js's, viewport.js's, page.js's and socket.js's.
: CODE-ID+ ( str ^VEC:vector<u8> -- )
   SHA256-CTX-BYTES $20 + MEM:BYTES-ALLOC-LEN
   [: HASHED ;] MEM:WITH-BYTES ;

\ The module's bytes and the vector CODE-TOOL$'s result's _meta is appended
\ to: {"page":"<page id>","code":"<CODE-ID+>","files":{"view.html":"<base64>",
\ "view.js":…,"turn.js":…,"viewport.js":…,"page.js":…,"socket.js":…,
\ "module":…}}. The vector must not hold the module.
: CODE+ ( str ^VEC:vector<u8> -- )
   {: m:str v:^ :}
   s\" {\"page\":\"" v VEC:APPEND  ID ID-U >str v VEC:APPEND
   s\" \",\"code\":\"" v VEC:APPEND  m v CODE-ID+
   s\" \",\"files\":{" v VEC:APPEND
   s" view.html" VIEW-HTML VIEW-HTML-U >str v FILE+  s" ," v VEC:APPEND
   s" view.js" VIEW-JS VIEW-JS-U >str v FILE+  s" ," v VEC:APPEND
   s" turn.js" TURN TURN-U >str v FILE+  s" ," v VEC:APPEND
   s" viewport.js" VIEWPORT VIEWPORT-U >str v FILE+  s" ," v VEC:APPEND
   s" page.js" PAGE PAGE-U >str v FILE+  s" ," v VEC:APPEND
   s" socket.js" SOCKET SOCKET-U >str v FILE+  s" ," v VEC:APPEND
   s" module" m v FILE+  s" }}" v VEC:APPEND ;

;package
