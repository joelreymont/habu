\ app.f - MCP-APP: an MCP App's view (ext-apps "2026-01-26") of a Habu Wasm
\ module, in two parts. The kept page, PAGE+, is app.html and app.js, all a
\ host keeps between its reads of the ui:// resource: app.html with the title
\ HTML-escaped at its <!--title--> marker, the page id at <!--page-id--> and
\ app.js as base64 at <!--app.js-->. Everything that changes with a release
\ comes from the server each time the page starts: after
\ ui/notifications/initialized the page calls the app-only tool CODE-TOOL$
\ with arguments {}, and CODE+ is that result's _meta, the page id and
\ view.html, view.js, turn.js, viewport.js, page.js and the module as base64.
\ A page whose id is not the _meta's is older than its server and does not
\ start. The page id is the SHA-256, in lowercase hex, of app.html's and
\ app.js's bytes, so it changes exactly when app.html or app.js does.
\ The page fetches nothing and runs under ChatGPT's view CSP:
\ script-src 'self' 'wasm-unsafe-eval' 'unsafe-inline' 'unsafe-eval' blob:;
\ worker-src blob:; connect-src 'self'; img-src 'self' data:, so page.js
\ shows an SVG from a data: URL. The seven files, beside this one, are
\ read into dictionary buffers as it loads.
\ STORAGE CLASS. PROCESS-WIDE: buffers filled at load and only read after;
\ PAGE+ and CODE+ write only the caller's BUF.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/base64.f
require src/lib/xml.f

package MCP-APP
private

\ The path of the named file beside this one; it holds only while this file
\ loads.
: PATH ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop SOURCE-ROOT:DIRNAME
   a u SOURCE-ROOT:JOIN ;


\ A file that changed size since it was measured is E-FS-IO, or E-FS-CAPACITY.
: READ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n dst:ptr cap:n :}
   a u PATH {: path:ptr pathu:n :}
   path pathu dst cap FS:READ-ALL cap <> if E-FS-IO throw then ;

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

\ The page id: the lowercase hex SHA-256 of app.html's bytes, then app.js's.
$40 constant ID-U
ID-U BUFFER: ID
SHA256-CTX-BYTES SCRATCH-BUFFER: SHA
$20 SCRATCH-BUFFER: DIGEST
SHA SHA256-BEGIN
SHA HTML HTML-U SHA256-FEED
SHA APP APP-U SHA256-FEED
SHA DIGEST SHA256-END
DIGEST ID SHA256>HEX

\ The offset of the marker in app.html at or after the offset; each marker
\ follows the one before it.
: MARK ( ptr u8 n n -- n )
   {: m:ptr mu:n from:n :}
   HTML HTML-U m mu from FIND-SUB-FROM MATCH option
      none OF s" hb: src/lib/browser/app.html lacks a marker in order" 1 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;


: TITLE-MARK ( -- ptr u8 n )  s" <!--title-->" ;
: ID-MARK ( -- ptr u8 n )  s" <!--page-id-->" ;
: APP-MARK ( -- ptr u8 n )  s" <!--app.js-->" ;

TITLE-MARK 0 MARK constant TITLE-AT
TITLE-AT TITLE-MARK nip + constant TITLE-END
ID-MARK TITLE-END MARK constant ID-AT
ID-AT ID-MARK nip + constant ID-END
APP-MARK ID-END MARK constant APP-AT
APP-AT APP-MARK nip + constant APP-END

\ The longest escape ESCAPE-TEXT writes for one byte: &amp; and &#13;.
5 constant WIDEST

: SPAN+ ( ptr u8 n ptr a -- )
   {: a:ptr u:n buf:ptr :}
   a u BUF:N>BLEN buf BUF:APPEND-SPAN ;


: GAP+ ( n n ptr a -- )
   {: from:n to:n buf:ptr :}
   HTML from + to from - buf SPAN+ ;


: BASE64+ ( ptr u8 n ptr a -- )
   {: a:ptr u:n buf:ptr :}
   u 2 + 3 / 4 * {: len:n :}
   buf len BUF:N>BLEN BUF:GROW {: out :}
   a u out BASE64:ENCODE drop ;


\ The escaped title and the BUF.
: COMPOSE ( ptr u8 n ptr a -- )
   {: t:ptr tu:n buf:ptr :}
   0 TITLE-AT buf GAP+  t tu buf SPAN+
   TITLE-END ID-AT buf GAP+  ID ID-U buf SPAN+
   ID-END APP-AT buf GAP+  APP APP-U buf BASE64+
   APP-END HTML-U buf GAP+ ;


\ Escapes the title into the scratch bytes before the BUF is touched, so a
\ title ESCAPE-TEXT refuses leaves the BUF as it was.
: ESCAPED ( ptr u8 n ptr a n ptr u8 NUM:alloc-byte-len -- )
   drop {: t:ptr tu:n buf:ptr cap:n s:ptr :}
   t tu s cap XML:ESCAPE-TEXT {: n:n :}
   s n buf COMPOSE ;


\ A member of CODE+'s files: the file's name and its bytes as base64.
: MEMBER+ ( ptr u8 n ptr u8 n ptr a -- )
   {: name:ptr nu:n a:ptr u:n buf:ptr :}
   s\" \"" buf SPAN+  name nu buf SPAN+  s\" \":\"" buf SPAN+
   a u buf BASE64+  s\" \"" buf SPAN+ ;

public

\ The title and the BUF the page is appended to; a title that is not XML text
\ is refused.
: PAGE+ ( ptr u8 n ptr a -- )
   {: t:ptr tu:n buf:ptr :}
   \ An allocation is never empty, so an empty title takes one escape's room.
   tu 1 max WIDEST * {: cap:n :}
   t tu buf cap  cap MEM:BYTES-ALLOC-LEN [: ESCAPED ;] MEM:WITH-BYTES ;


\ The app-only tool the page calls, with arguments {}, for its code.
: CODE-TOOL$ ( -- ptr u8 n )  s" view_code" ;

\ The module's bytes and the BUF CODE-TOOL$'s result's _meta is appended to:
\ {"page":"<page id>","files":{"view.html":"<base64>","view.js":…,
\ "turn.js":…,"viewport.js":…,"page.js":…,"module":…}}. The BUF must not
\ hold the module.
: CODE+ ( ptr u8 n ptr a -- )
   {: m:ptr mu:n buf:ptr :}
   s\" {\"page\":\"" buf SPAN+  ID ID-U buf SPAN+
   s\" \",\"files\":{" buf SPAN+
   s" view.html" VIEW-HTML VIEW-HTML-U buf MEMBER+  s" ," buf SPAN+
   s" view.js" VIEW-JS VIEW-JS-U buf MEMBER+  s" ," buf SPAN+
   s" turn.js" TURN TURN-U buf MEMBER+  s" ," buf SPAN+
   s" viewport.js" VIEWPORT VIEWPORT-U buf MEMBER+  s" ," buf SPAN+
   s" page.js" PAGE PAGE-U buf MEMBER+  s" ," buf SPAN+
   s" module" m mu buf MEMBER+  s" }}" buf SPAN+ ;

;package
