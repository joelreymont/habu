split-str
\ app-test.f - MCP-APP:PAGE+ into a BUF holding a prefix, with a title holding
\ <&"', and MCP-APP:CODE+ of a small module. The prefix is kept; the page after
\ it is valid UTF-8, holds the escaped title, the page id and app.js's base64,
\ and no marker; MCP-APP:PAGE-ID$ is the page's id. CODE+ names the same page
\ id and carries view.html, view.js, turn.js, viewport.js, page.js and the
\ module as base64 that decodes to their bytes, the files read here from
\ src/lib/browser at the tree's root. MCP-APP:FILE+ of a.js with the bytes hi
\ appends "a.js":"aGk=" after a prefix. A title that is not XML text is
\ refused and the BUF kept; the cases of a
\ module held in the BUF and of a negative module length need the build's
\ --checks option and are off. test/browser/app-test.f's --chrome check runs a
\ composed page and its code in Chrome.
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
BUF:HDR-BYTES BUFFER: PAGE
BUF:HDR-BYTES BUFFER: CODE
BUF:HDR-BYTES BUFFER: FILE

: PREFIX$ ( -- str )  s" HTTP/1.1 200 OK prefix|" ;
: MODULE$ ( -- str )  s\" \x00asm\x01\x00\x00\x00\xFF\xFE" ;
: TITLE$ ( -- str )  s\" Zürich <&\"'" ;
: ESCAPED$ ( -- str )  s\" <title>Zürich &lt;&amp;\"'</title>" ;

: PAGE$ ( -- str )  PAGE BUF:SPAN$ BUF:BLEN>N ;
: CODE$ ( -- str )  CODE BUF:SPAN$ BUF:BLEN>N ;
: FILE$ ( -- str )  FILE BUF:SPAN$ BUF:BLEN>N ;

: AFTER$ ( -- str )
   PAGE$ {: a:^ u:n :}
   PREFIX$ nip {: pu:n :}
   a pu + u pu - ;


\ FILE+'s member, after the prefix.
: MEMBER-AFTER$ ( -- str )
   FILE$ {: a:^ u:n :}
   PREFIX$ nip {: pu:n :}
   a pu + u pu - ;


\ The text in the span after the first opener and before the next closer,
\ empty when either is missing.
: BETWEEN$ ( str str str -- str )
   {: a:^ u:n o:^ ou:n c:^ cu:n :}
   a u o ou FIND-SUB MATCH option
      none OF a 0 ENDOF
      some OF IDX>N ou + {: at:n :}
         a at + u at - {: s:^ su:n :}
         s su c cu FIND-SUB MATCH option
            none OF s 0 ENDOF
            some OF IDX>N s swap ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;


\ The text of the page's element of this id.
: SCRIPT$ ( str -- str )
   {: id:^ iu:n :}
   SB-RESET  s\" id=\"" SB-APPEND  id iu SB-APPEND  s\" \">" SB-APPEND
   AFTER$ SB$ s" </script>" BETWEEN$ ;


\ The string of CODE+'s member of this name.
: MEMBER$ ( str -- str )
   {: name:^ nu:n :}
   SB-RESET  s\" \"" SB-APPEND  name nu SB-APPEND  s\" \":\"" SB-APPEND
   CODE$ SB$ s\" \"" BETWEEN$ ;


\ The base64 decodes to the bytes.
: DECODES ( ^u8 n ^u8 n -- )
   {: c:^ cu:n w:^ wu:n :}
   c cu DECODED BASE64:DECODE {: du:n :}
   DECODED SPAN:$ drop du w wu T$= ;


\ The base64 decodes to the file's bytes.
: CARRIES ( ^u8 n ^u8 n -- )
   {: c:^ cu:n path:^ pu:n :}
   path pu WANT CAP FS:READ-ALL {: wu:n :}
   c cu WANT wu DECODES ;


: PAGE-CASES ( -- )
   s" the prefix is kept" T-LABEL
   PAGE$ drop PREFIX$ nip PREFIX$ T$=
   s" the page is valid UTF-8" T-LABEL
   AFTER$ UTF8:VALID? TTRUE
   s" the page starts as an HTML document" T-LABEL
   AFTER$ s" <!doctype html>" STARTS-WITH? TTRUE
   s" the title is HTML-escaped into the page's title element" T-LABEL
   AFTER$ ESCAPED$ COUNT-SUB 1 T=
   s" the page id is 64 hex digits" T-LABEL
   s" page-id" SCRIPT$ nip 64 T=
   s" PAGE-ID$ is the page's id" T-LABEL
   s" page-id" SCRIPT$ MCP-APP:PAGE-ID$ T$=
   s" app.js's base64 decodes to src/lib/browser/app.js" T-LABEL
   s" app.js" SCRIPT$ s" src/lib/browser/app.js" CARRIES
   s" no marker is left in the page" T-LABEL
   AFTER$ s" <!--" COUNT-SUB 0 T= ;


: CODE-CASES ( -- )
   s" CODE+ names the page's id" T-LABEL
   s" page" MEMBER$ s" page-id" SCRIPT$ T$=
   s" view.html's base64 decodes to src/lib/browser/view.html" T-LABEL
   s" view.html" MEMBER$ s" src/lib/browser/view.html" CARRIES
   s" view.js's base64 decodes to src/lib/browser/view.js" T-LABEL
   s" view.js" MEMBER$ s" src/lib/browser/view.js" CARRIES
   s" turn.js's base64 decodes to src/lib/browser/turn.js" T-LABEL
   s" turn.js" MEMBER$ s" src/lib/browser/turn.js" CARRIES
   s" viewport.js's base64 decodes to src/lib/browser/viewport.js" T-LABEL
   s" viewport.js" MEMBER$ s" src/lib/browser/viewport.js" CARRIES
   s" page.js's base64 decodes to src/lib/browser/page.js" T-LABEL
   s" page.js" MEMBER$ s" src/lib/browser/page.js" CARRIES
   s" the module's base64 decodes to its bytes" T-LABEL
   s" module" MEMBER$ MODULE$ DECODES ;


: FILE-CASES ( -- )
   FILE CAP BUF:N>BLEN BUF:INIT
   PREFIX$ BUF:N>BLEN FILE BUF:APPEND-SPAN
   s" a.js" s" hi" FILE MCP-APP:FILE+
   s" FILE+ appends the name and the bytes as base64" T-LABEL
   MEMBER-AFTER$ s\" \"a.js\":\"aGk=\"" T$=
   FILE BUF:DISPOSE ;


: COMPOSE-CASES ( -- )
   PAGE CAP BUF:N>BLEN BUF:INIT
   PREFIX$ BUF:N>BLEN PAGE BUF:APPEND-SPAN
   TITLE$ PAGE MCP-APP:PAGE+
   CODE CAP BUF:N>BLEN BUF:INIT
   MODULE$ CODE MCP-APP:CODE+
   PAGE-CASES
   CODE-CASES
   PAGE$ {: u:n :} drop
   s" a title that is not XML text is refused" T-LABEL
   [: s\" bad\z" PAGE MCP-APP:PAGE+ ;] XML:E-SCALAR TTHROWSQ
   s" a refused title leaves the BUF as it was" T-LABEL
   PAGE$ nip u T=
   CODE BUF:DISPOSE
   PAGE BUF:DISPOSE ;

T-RESET
COMPOSE-CASES
FILE-CASES
T-REPORT

false [if] \ off until dot habu-add-the-checks-a49d054d
: NEGATIVE-LENGTH-CASE ( -- )
   PAGE CAP BUF:N>BLEN BUF:INIT
   PREFIX$ BUF:N>BLEN PAGE BUF:APPEND-SPAN
   PAGE$ {: u:n :} drop
   s" a negative module length is refused" T-LABEL
   [: MODULE$ drop -1 PAGE MCP-APP:CODE+ ;] E-STR-BOUNDS TTHROWSQ
   s" a refused length leaves the BUF as it was" T-LABEL
   PAGE$ nip u T=
   PAGE BUF:DISPOSE ;

T-RESET
NEGATIVE-LENGTH-CASE
T-REPORT
[then]

false [if] \ off until dot habu-add-the-checks-a49d054d
\ Runs in COMPOSE-CASES after the refused title, the page built.
: MODULE-HELD ( -- )
   PAGE$ {: u:n :} drop
   s" a module held in the BUF is refused" T-LABEL
   [: PAGE$ PAGE MCP-APP:CODE+ ;] E-BUF-BOUNDS TTHROWSQ
   s" a refused module leaves the BUF as it was" T-LABEL
   PAGE$ nip u T= ;
[then]

;package
