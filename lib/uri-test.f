\ uri-test.f - file URIs decoded to paths, and paths encoded to file URIs,
\ through lib/uri.f.
\ Run: bin/hb --load lib/uri-test.f

require lib/errors.f
require lib/test.f
require lib/span.f
require lib/uri.f

package URI-TEST
using SPAN

64 SPAN-BUFFER: DST
64 SPAN-BUFFER: URI-DST
2 BUFFER: ONE                     \ `/` and the byte a round trip takes
$A5 constant CANARY

: PATH$ ( ptr u8 n -- ptr u8 n )   \ decode into DST, answer the bytes written
   DST URI:FILE>PATH {: u:n :}
   DST $ drop u ;

: PATH= ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   a u PATH$ b v T$= ;

\ An empty or `localhost` authority, the scheme and the host in any case.
: FORMS ( -- )
   s" file:///home/u/a.f" s" /home/u/a.f" PATH=
   s" file://localhost/home/u/a.f" s" /home/u/a.f" PATH=
   s" FILE://LocalHost/x" s" /x" PATH=
   s" File:///x" s" /x" PATH=
   s" file:///" s" /" PATH= ;

\ Each escape is one byte, in either hex case, decoded once: a decoded `%`
\ starts nothing. `%2F`, `%00`, `%3F` and `%23` are bytes like any other; a
\ byte a client left unescaped is kept as it is.
: ESCAPES ( -- )
   s" file:///a%20b/%C3%A9.f" s\" /a b/\xC3\xA9.f" PATH=
   s" file:///%c3%a9" s\" /\xC3\xA9" PATH=
   s" file:///a%2Fb" s" /a/b" PATH=
   s" file:///a%00b" s\" /a\x00b" PATH=
   s" file:///%2541" s" /%41" PATH=
   s" file:///a%3Fb%23c" s" /a?b#c" PATH=
   s\" file:///\xC3\xA9" s\" /\xC3\xA9" PATH= ;

: DECODE-DROP ( ptr u8 n -- )
   DST URI:FILE>PATH drop ;

: SCHEMES ( -- )
   [: s" http://localhost/x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" untitled:Untitled-1" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" /home/u/a.f" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" files:///x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" fil:///x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" " DECODE-DROP ;] E-URI-SCHEME TTHROWSQ ;

\ A remote host, a port, user information, a missing `//`, and an authority
\ with no path after it.
: AUTHORITIES ( -- )
   [: s" file://host/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://localhost:80/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://user@localhost/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://localhost" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ ;

\ A `%` at the end, with one digit, with a non-hex digit in either place, and a
\ bare `?` or `#`, which would open a query or fragment a file URI cannot have.
: ESCAPE-REFUSALS ( -- )
   [: s" file:///a%" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%4" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%G1" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%1G" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a?b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a#b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ ;

: UNTOUCHED ( n -- )
   {: n:n :}   \ DST's first n bytes still hold the canary
   n 0 ?do DST i U8@ CANARY T= loop ;

: FOUR-SHORT ( -- )
   s" file:///abcd" DST 4 TAKE URI:FILE>PATH drop ;

: LATE-ESCAPE ( -- )
   s" file:///abcd%G" DECODE-DROP ;

: NEGATIVE-LENGTH ( -- )
   s" file:///x" drop -1 DECODE-DROP ;

\ A refusal writes nothing: the whole URI is checked, and the path's length
\ measured against the span, before the first byte lands. A span of exactly the
\ path's length is enough.
: ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: FOUR-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED
   [: LATE-ESCAPE ;] E-URI-ESCAPE TTHROWSQ
   5 UNTOUCHED
   [: NEGATIVE-LENGTH ;] E-SPAN-LENGTH TTHROWSQ
   s" file:///abcd" DST 5 TAKE URI:FILE>PATH 5 T=
   DST $ drop 5 s" /abcd" T$= ;

: URI$ ( ptr u8 n -- ptr u8 n )   \ encode into URI-DST, answer the bytes written
   URI-DST URI:PATH>FILE {: u:n :}
   URI-DST $ drop u ;

: URI= ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   a u URI$ b v T$= ;

\ `file://` and the path, whose `/`, letters, digits and `-._~` stay as they
\ are.
: ENCODE-FORMS ( -- )
   s" /home/u/a.f" s" file:///home/u/a.f" URI=
   s" /" s" file:///" URI=
   s" /AZaz09-._~/x" s" file:///AZaz09-._~/x" URI= ;

\ Every other byte is `%XX` in upper-case hex: a space, each byte of a UTF-8
\ character, the delimiters a URI gives meaning to, and NUL.
: ENCODE-ESCAPES ( -- )
   s" /a b" s" file:///a%20b" URI=
   s\" /\xC3\xA9.f" s" file:///%C3%A9.f" URI=
   s" /%?#:@+()" s" file:///%25%3F%23%3A%40%2B%28%29" URI=
   s\" /a\x00b\xFF" s" file:///a%00b%FF" URI= ;

: ENCODE-DROP ( ptr u8 n -- )
   URI-DST URI:PATH>FILE drop ;

\ A path that is not absolute: relative, empty, or a URI already.
: RELATIVES ( -- )
   [: s" a/b" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" ./a" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" " ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" file:///a" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ ;

: ENCODE-SHORT ( -- )   \ `file:///a%20b` is 13 bytes
   s" /a b" DST 12 TAKE URI:PATH>FILE drop ;

: ENCODE-RELATIVE ( -- )
   s" a b" DST URI:PATH>FILE drop ;

: ENCODE-NEGATIVE ( -- )
   s" /x" drop -1 DST URI:PATH>FILE drop ;

\ An encoding refused writes nothing, and a span of exactly the URI's length
\ is enough.
: ENCODE-ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: ENCODE-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-RELATIVE ;] E-URI-RELATIVE TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   13 UNTOUCHED
   s" /a b" DST 13 TAKE URI:PATH>FILE 13 T=
   DST $ drop 13 s" file:///a%20b" T$= ;

: TRIP ( n -- )
   {: b:n :}   \ `/` and byte b, encoded and decoded back
   [char] / ONE c!
   b ONE 1+ c!
   ONE 2 URI$ PATH$ ONE 2 T$= ;

\ Every byte comes back from its URI as it went in.
: ROUND-TRIP ( -- )
   256 0 do i TRIP loop ;

: MAIN ( -- )
   T-RESET
   FORMS
   ESCAPES
   SCHEMES
   AUTHORITIES
   ESCAPE-REFUSALS
   ALL-OR-NOTHING
   ENCODE-FORMS
   ENCODE-ESCAPES
   RELATIVES
   ENCODE-ALL-OR-NOTHING
   ROUND-TRIP
   T-REPORT
   s" uri-test: ok" type cr ;

MAIN

;using
;package
