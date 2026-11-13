\ app.f - MCP-APP: one self-contained UTF-8 HTML page that shows a Habu Wasm
\ module as an MCP App's view (ext-apps "2026-01-26"). PAGE+ appends app.html
\ with the title HTML-escaped at its <!--title--> marker and turn.js,
\ viewport.js, page.js, app.js and the module as base64 at theirs. Its inline
\ bootstrap imports each script from a blob: URL and starts app.js, so the page
\ fetches nothing and runs under ChatGPT's view CSP:
\ script-src 'self' 'wasm-unsafe-eval' 'unsafe-inline' 'unsafe-eval' blob:;
\ worker-src blob:; connect-src 'self'. The five files, beside this one, are
\ read into dictionary buffers as it loads.
\ STORAGE CLASS. PROCESS-WIDE: buffers filled at load and only read after;
\ PAGE+ writes only the caller's BUF.

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

s" viewport.js" PATH FS:FILE-SIZE constant VIEWPORT-U
VIEWPORT-U BUFFER: VIEWPORT
s" viewport.js" VIEWPORT VIEWPORT-U READ

s" turn.js" PATH FS:FILE-SIZE constant TURN-U
TURN-U BUFFER: TURN
s" turn.js" TURN TURN-U READ

s" page.js" PATH FS:FILE-SIZE constant PAGE-U
PAGE-U BUFFER: PAGE
s" page.js" PAGE PAGE-U READ

\ The offset of the marker in app.html at or after the offset; each marker
\ follows the one before it.
: MARK ( ptr u8 n n -- n )
   {: m:ptr mu:n from:n :}
   HTML HTML-U m mu from FIND-SUB-FROM MATCH option
      none OF s" hb: src/lib/browser/app.html lacks a marker in order" 1 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;


: TITLE-MARK ( -- ptr u8 n )  s" <!--title-->" ;
: TURN-MARK ( -- ptr u8 n )  s" <!--turn.js-->" ;
: VIEWPORT-MARK ( -- ptr u8 n )  s" <!--viewport.js-->" ;
: PAGE-MARK ( -- ptr u8 n )  s" <!--page.js-->" ;
: APP-MARK ( -- ptr u8 n )  s" <!--app.js-->" ;
: MODULE-MARK ( -- ptr u8 n )  s" <!--module-->" ;

TITLE-MARK 0 MARK constant TITLE-AT
TITLE-AT TITLE-MARK nip + constant TITLE-END
TURN-MARK TITLE-END MARK constant TURN-AT
TURN-AT TURN-MARK nip + constant TURN-END
VIEWPORT-MARK TURN-END MARK constant VIEWPORT-AT
VIEWPORT-AT VIEWPORT-MARK nip + constant VIEWPORT-END
PAGE-MARK VIEWPORT-END MARK constant PAGE-AT
PAGE-AT PAGE-MARK nip + constant PAGE-END
APP-MARK PAGE-END MARK constant APP-AT
APP-AT APP-MARK nip + constant APP-END
MODULE-MARK APP-END MARK constant MODULE-AT
MODULE-AT MODULE-MARK nip + constant MODULE-END

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


\ The module's bytes, the escaped title and the BUF.
: COMPOSE ( ptr u8 n ptr u8 n ptr a -- )
   {: m:ptr mu:n t:ptr tu:n buf:ptr :}
   0 TITLE-AT buf GAP+  t tu buf SPAN+
   TITLE-END TURN-AT buf GAP+  TURN TURN-U buf BASE64+
   TURN-END VIEWPORT-AT buf GAP+  VIEWPORT VIEWPORT-U buf BASE64+
   VIEWPORT-END PAGE-AT buf GAP+  PAGE PAGE-U buf BASE64+
   PAGE-END APP-AT buf GAP+  APP APP-U buf BASE64+
   APP-END MODULE-AT buf GAP+  m mu buf BASE64+
   MODULE-END HTML-U buf GAP+ ;


\ Escapes the title into the scratch bytes before the BUF is touched, so a
\ title ESCAPE-TEXT refuses leaves the BUF as it was.
: ESCAPED ( ptr u8 n ptr u8 n ptr a n ptr u8 NUM:alloc-byte-len -- )
   drop {: m:ptr mu:n t:ptr tu:n buf:ptr cap:n s:ptr :}
   t tu s cap XML:ESCAPE-TEXT {: n:n :}
   m mu s n buf COMPOSE ;

public

\ The module's bytes, the title and the BUF the page is appended to; the BUF
\ must not hold the module, and a title that is not XML text is refused.
: PAGE+ ( ptr u8 n ptr u8 n ptr a -- )
   {: m:ptr mu:n t:ptr tu:n buf:ptr :}
   \ An allocation is never empty, so an empty title takes one escape's room.
   tu 1 max WIDEST * {: cap:n :}
   m mu t tu buf cap  cap MEM:BYTES-ALLOC-LEN [: ESCAPED ;] MEM:WITH-BYTES ;

;package
