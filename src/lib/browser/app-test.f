\ app-test.f - MCP-APP:PAGE+ of a small module into a BUF holding a prefix,
\ with a title holding <&"'. The prefix is kept; the page after it is valid
\ UTF-8, holds the module's base64 once and the escaped title, and each of its
\ scripts decodes to the bytes of its file in src/lib/browser, read here from
\ the tree's root. A title that is not XML text, a module held in the BUF
\ itself or a negative module length is refused and the BUF kept.
\ test/browser/app-test.f's --chrome check runs a composed page in Chrome.
\ Run: bin/hb --load src/lib/browser/app-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/utf8-scalar.f
require src/lib/byte-buffer.f
require src/lib/base64.f
require src/lib/xml.f
require src/lib/browser/app.f

package MCP-APP-TEST
private

$20000 constant CAP                   \ room for any of the page's scripts
CAP SPAN-BUFFER: DECODED
CAP BUFFER: WANT
64 SPAN-BUFFER: CODED
BUF:HDR-BYTES BUFFER: PAGE

: PREFIX$ ( -- ptr u8 n )  s" HTTP/1.1 200 OK prefix|" ;
: MODULE$ ( -- ptr u8 n )  s\" \x00asm\x01\x00\x00\x00\xFF\xFE" ;
: TITLE$ ( -- ptr u8 n )  s\" Zürich <&\"'" ;
: ESCAPED$ ( -- ptr u8 n )  s\" <title>Zürich &lt;&amp;\"'</title>" ;

: PAGE$ ( -- ptr u8 n )  PAGE BUF:SPAN$ BUF:BLEN>N ;

: AFTER$ ( -- ptr u8 n )
   PAGE$ {: a:ptr u:n :}
   PREFIX$ nip {: pu:n :}
   a pu + u pu - ;


\ The text between the script element of this id and its end tag.
: SCRIPT$ ( ptr u8 n -- ptr u8 n )
   {: id:ptr iu:n :}
   SB-RESET  s\" id=\"" SB-APPEND  id iu SB-APPEND  s\" \">" SB-APPEND
   AFTER$ {: a:ptr u:n :}
   a u SB$ FIND-SUB MATCH option
      none OF a 0 ENDOF
      some OF IDX>N SB$ nip + {: at:n :}
         a at + u at - {: s:ptr su:n :}
         s su s" </script>" FIND-SUB MATCH option
            none OF s 0 ENDOF
            some OF IDX>N s swap ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;


: CARRIES ( ptr u8 n ptr u8 n -- )
   {: id:ptr iu:n path:ptr pu:n :}
   path pu WANT CAP FS:READ-ALL {: wu:n :}
   id iu SCRIPT$ DECODED BASE64:DECODE {: du:n :}
   DECODED SPAN:$ drop du WANT wu T$= ;


: COMPOSE-CASES ( -- )
   PAGE CAP BUF:N>BLEN BUF:INIT
   PREFIX$ BUF:N>BLEN PAGE BUF:APPEND-SPAN
   MODULE$ TITLE$ PAGE MCP-APP:PAGE+
   s" the prefix is kept" T-LABEL
   PAGE$ drop PREFIX$ nip PREFIX$ T$=
   s" the page is valid UTF-8" T-LABEL
   AFTER$ UTF8:VALID? TTRUE
   s" the page starts as an HTML document" T-LABEL
   AFTER$ s" <!doctype html>" STARTS-WITH? TTRUE
   s" the title is HTML-escaped into the page's title element" T-LABEL
   AFTER$ ESCAPED$ COUNT-SUB 1 T=
   s" the page holds the module's base64 exactly once" T-LABEL
   MODULE$ CODED BASE64:ENCODE {: cu:n :}
   AFTER$ CODED SPAN:$ drop cu COUNT-SUB 1 T=
   s" the module's script element holds its base64" T-LABEL
   s" module" SCRIPT$ CODED SPAN:$ drop cu T$=
   s" turn.js's base64 decodes to src/lib/browser/turn.js" T-LABEL
   s" turn.js" s" src/lib/browser/turn.js" CARRIES
   s" viewport.js's base64 decodes to src/lib/browser/viewport.js" T-LABEL
   s" viewport.js" s" src/lib/browser/viewport.js" CARRIES
   s" page.js's base64 decodes to src/lib/browser/page.js" T-LABEL
   s" page.js" s" src/lib/browser/page.js" CARRIES
   s" app.js's base64 decodes to src/lib/browser/app.js" T-LABEL
   s" app.js" s" src/lib/browser/app.js" CARRIES
   s" no marker is left in the page" T-LABEL
   AFTER$ s" <!--" COUNT-SUB 0 T=
   PAGE$ {: u:n :} drop
   s" a title that is not XML text is refused" T-LABEL
   [: MODULE$ s\" bad\z" PAGE MCP-APP:PAGE+ ;] XML:E-SCALAR TTHROWSQ
   s" a refused title leaves the BUF as it was" T-LABEL
   PAGE$ nip u T=
   s" a module held in the BUF is refused" T-LABEL
   [: PAGE$ TITLE$ PAGE MCP-APP:PAGE+ ;] E-BUF-BOUNDS TTHROWSQ
   s" a refused module leaves the BUF as it was" T-LABEL
   PAGE$ nip u T=
   s" a negative module length is refused" T-LABEL
   [: MODULE$ drop -1 TITLE$ PAGE MCP-APP:PAGE+ ;] E-STR-BOUNDS TTHROWSQ
   s" a refused length leaves the BUF as it was" T-LABEL
   PAGE$ nip u T=
   PAGE BUF:DISPOSE ;

T-RESET
COMPOSE-CASES
T-REPORT

;package
